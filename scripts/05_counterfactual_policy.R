# Step 5: Counterfactual policy impact simulation on the 1988 cross-section.

library(ggplot2)

source("R/theme_config.R")
source("R/counterfactual.R")

df   <- readRDS("data/processed/fatalities_panel.rds")
core <- readRDS("outputs/models/core_models.rds")
m4   <- core$main$baseline

message("Running counterfactual policy simulations for 1988 baseline...")
cf <- run_counterfactual_simulation(df, m4)

saveRDS(cf, "outputs/models/counterfactual.rds")
readr::write_csv(cf$national_summary, "outputs/tables/counterfactual_scenarios.csv")
readr::write_csv(cf$state_breakdown, "outputs/tables/counterfactual_states_1988.csv")

# ---------------------------------------------------------------------------
# Figure 7: Counterfactual Annual Traffic Lives Saved (1988 Baseline)
# ---------------------------------------------------------------------------
cf_plot_df <- cf$national_summary |>
  dplyr::arrange(lives_saved_est) |>
  dplyr::mutate(
    scenario_ord = factor(scenario_label, levels = rev(scenario_label)),
    label_x = lives_saved_high + max(lives_saved_high) * 0.03,
    label_txt = sprintf(
      "%s lives (%.1f%% of 1988 deaths)\n95%% CI: [%s to %s]",
      scales::comma(round(lives_saved_est)),
      pct_reduction_est,
      scales::comma(round(lives_saved_low)),
      scales::comma(round(lives_saved_high))
    )
  )

x_min <- min(cf_plot_df$lives_saved_low) * 1.18
x_max <- max(cf_plot_df$lives_saved_high) * 1.55

fig7 <- ggplot(cf_plot_df, aes(x = lives_saved_est, y = scenario_ord)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = PALETTE$slate, linewidth = 0.8) +
  geom_col(fill = PALETTE$navy, alpha = 0.16, width = 0.55) +
  geom_errorbar(
    aes(xmin = lives_saved_low, xmax = lives_saved_high),
    width = 0.2,
    linewidth = 1.0,
    color = PALETTE$teal,
    orientation = "y"
  ) +
  geom_point(size = 3.5, color = PALETTE$navy) +
  geom_text(
    aes(x = label_x, label = label_txt),
    hjust = 0,
    size = 3.05,
    lineheight = 0.95,
    fontface = "bold",
    color = PALETTE$dark_text
  ) +
  scale_x_continuous(
    limits = c(x_min, x_max),
    labels = scales::comma_format()
  ) +
  labs(
    title = "Projected Annual US Traffic Lives Saved Under Real Beer Tax Policy Scenarios",
    subtitle = sprintf(
      "Linear extrapolation from Model (4) TWFE (beta = %.3f, SE = %.3f) applied to 1988 baseline (%s fatalities)",
      cf$national_summary$beta_hat[1],
      cf$national_summary$se_hat[1],
      scales::comma(cf$national_summary$total_fatal_1988[1])
    ),
    x = "Estimated Annual Lives Saved (with 95% Clustered Confidence Interval)",
    y = NULL,
    caption = "Note: Counterfactual assumes linear marginal response and no cross-border bootlegging spillovers; 95% CI reflects clustered t(47) uncertainty."
  ) +
  theme_portfolio(base_size = 11.2) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", color = PALETTE$dark_text)
  )

save_figure(fig7, "07_counterfactual_policy.png", width = 12.2, height = 5.0)

message("Counterfactual simulation completed.")
