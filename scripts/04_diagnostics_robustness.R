# Step 4: Panel specification diagnostics, mechanism/placebo checks, and robustness.

library(ggplot2)

source("R/theme_config.R")
source("R/diagnostics.R")
source("R/extensions_robustness.R")

df   <- readRDS("data/processed/fatalities_panel.rds")
core <- readRDS("outputs/models/core_models.rds")
m4   <- core$main$baseline

message("Running formal panel diagnostics and joint Wald exclusion tests...")
diag_res <- run_panel_diagnostics(df, m4)

message("Running mechanism/placebo outcomes, alternative estimators, and 48-state LOSO jackknife...")
sub_res  <- fit_subgroup_outcomes(df)
rob_res  <- fit_robustness_specs(df)
loso_res <- run_leave_one_state_out(df)

saveRDS(
  list(diagnostics = diag_res, subgroups = sub_res, robustness = rob_res, loso = loso_res),
  "outputs/models/diagnostics_robustness.rds"
)

readr::write_csv(diag_res$panel_tests,   "outputs/tables/panel_diagnostics.csv")
readr::write_csv(diag_res$wald_tests,    "outputs/tables/wald_tests.csv")
readr::write_csv(diag_res$se_comparison, "outputs/tables/se_comparison.csv")
readr::write_csv(sub_res$summary,        "outputs/tables/subgroup_outcomes.csv")
readr::write_csv(rob_res$summary,        "outputs/tables/robustness_specs.csv")
readr::write_csv(loso_res,               "outputs/tables/loso_sensitivity.csv")

# ---------------------------------------------------------------------------
# Figure 5: Mechanism & Placebo Outcomes (% of Sample Mean Effect per +$1 Tax)
# ---------------------------------------------------------------------------
sub_plot_df <- sub_res$summary |>
  dplyr::mutate(
    outcome_ord = factor(outcome, levels = rev(sub_res$summary$outcome)),
    lbl = sprintf("%.1f%% (p = %.3f)", pct_effect, p_value)
  )

x_lo <- min(sub_plot_df$pct_low) - 10
x_hi <- max(sub_plot_df$pct_high) + 38

fig5 <- ggplot(sub_plot_df, aes(x = pct_effect, y = outcome_ord, color = category)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = PALETTE$slate, linewidth = 0.8) +
  geom_errorbar(
    aes(xmin = pct_low, xmax = pct_high),
    width = 0.24,
    linewidth = 0.95,
    orientation = "y"
  ) +
  geom_point(size = 3.4) +
  geom_text(
    aes(x = pct_high + 2.5, label = lbl),
    hjust = 0,
    size = 3.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Overall"                            = PALETTE$navy,
      "Alcohol-Sensitive Mechanism"        = PALETTE$teal,
      "Lower-Alcohol Comparison / Placebo" = PALETTE$amber
    ),
    name = "Mechanism Group"
  ) +
  scale_x_continuous(
    limits = c(x_lo, x_hi),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Mechanism & Placebo Validation: Beer Tax Effects Normalized by Sample Mean",
    subtitle = "TWFE estimates with state-clustered 95% CIs (controls: drinking age, DUI laws, miles, unemployment, log income)",
    x = "% Change Relative to Outcome Mean per +$1 Real Beer Tax",
    y = NULL,
    caption = "Source: Author's estimation on AER::Fatalities (N = 335, 48 state clusters). Youth 18-20 rate uses age-specific population."
  ) +
  theme_portfolio(base_size = 11.2) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", color = PALETTE$dark_text)
  )

save_figure(fig5, "05_outcomes_and_placebo.png", width = 11.2, height = 5.6)

# ---------------------------------------------------------------------------
# Figure 6: 48-State Leave-One-State-Out (LOSO) Jackknife Sensitivity
# ---------------------------------------------------------------------------
full_ct <- broom::tidy(m4, conf.int = TRUE) |> dplyr::filter(term == "beertax")

loso_plot_df <- loso_res |>
  dplyr::mutate(state_ord = factor(dropped_state, levels = dropped_state))

fig6 <- ggplot(loso_plot_df, aes(x = estimate, y = state_ord)) +
  geom_rect(
    xmin = full_ct$conf.low, xmax = full_ct$conf.high,
    ymin = -Inf, ymax = Inf,
    fill = PALETTE$light_grey, alpha = 0.55
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = PALETTE$crimson, linewidth = 0.8) +
  geom_vline(xintercept = full_ct$estimate, linetype = "solid", color = PALETTE$navy, linewidth = 0.9) +
  geom_errorbar(
    aes(xmin = conf_low, xmax = conf_high),
    width = 0,
    linewidth = 0.55,
    color = PALETTE$slate,
    orientation = "y"
  ) +
  geom_point(size = 2.0, color = PALETTE$navy) +
  annotate(
    "text",
    x = full_ct$estimate,
    y = 49.1,
    label = sprintf("Full-sample TWFE = %.3f", full_ct$estimate),
    hjust = 1.06, vjust = 0, size = 3.2, fontface = "bold", color = PALETTE$navy
  ) +
  annotate(
    "text",
    x = 0,
    y = 2.2,
    label = "Null (0.00)",
    hjust = -0.1, size = 3.1, fontface = "bold", color = PALETTE$crimson
  ) +
  scale_y_discrete(expand = expansion(add = c(1.2, 3.0))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Leave-One-State-Out (LOSO) Jackknife Sensitivity Check (48 Iterations)",
    subtitle = "Each row re-estimates Model (4) after dropping one state; shaded band shows full-sample 95% clustered CI",
    x = "Estimated Beer Tax Coefficient (with 95% State-Clustered CI)",
    y = "Omitted State",
    caption = "Source: Author's sensitivity analysis on AER::Fatalities (Model 4 TWFE). All 48 subsamples preserve the negative effect."
  ) +
  theme_portfolio(base_size = 10.5) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 7.5, face = "bold")
  )

save_figure(fig6, "06_jackknife_sensitivity.png", width = 9.5, height = 8.6)

message("Diagnostics and robustness checks completed.")
