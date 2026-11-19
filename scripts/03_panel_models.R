# Step 3: Core panel model estimation and coefficient progression forest plot.

library(ggplot2)

source("R/theme_config.R")
source("R/models_core.R")

df <- readRDS("data/processed/fatalities_panel.rds")

message("Fitting preliminary and 7-column panel regression models...")
prelim <- fit_preliminary_models(df)
main   <- fit_main_panel_models(df)

dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)
saveRDS(list(prelim = prelim, main = main), "outputs/models/core_models.rds")

readr::write_csv(prelim$summary, "outputs/tables/preliminary_models_summary.csv")
readr::write_csv(main$summary, "outputs/tables/main_models_summary.csv")

# ---------------------------------------------------------------------------
# Figure 4: Model Progression Forest Plot (Models 1-7)
# ---------------------------------------------------------------------------
forest_df <- main$summary |>
  dplyr::mutate(
    spec_group = dplyr::if_else(state_fe == "No", "Pooled OLS (No Fixed Effects)", "Fixed Effects (State / Two-Way)"),
    model_ord  = factor(model, levels = rev(main$summary$model)),
    label_txt  = sprintf("%.3f [%.3f, %.3f]", estimate, conf_low, conf_high)
  )

x_min <- min(forest_df$conf_low) - 0.15
label_col_x <- max(forest_df$conf_high) + 0.14
x_max <- label_col_x + 0.95

fig4 <- ggplot(forest_df, aes(x = estimate, y = model_ord, color = spec_group)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = PALETTE$slate, linewidth = 0.8) +
  geom_errorbar(
    aes(xmin = conf_low, xmax = conf_high),
    width = 0.22,
    linewidth = 0.95,
    orientation = "y"
  ) +
  geom_point(size = 3.4) +
  geom_text(
    aes(x = label_col_x, label = label_txt),
    hjust = 0,
    size = 3.2,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Pooled OLS (No Fixed Effects)" = PALETTE$crimson,
      "Fixed Effects (State / Two-Way)" = PALETTE$navy
    ),
    name = "Estimator Class"
  ) +
  scale_x_continuous(limits = c(x_min, x_max), labels = scales::number_format(accuracy = 0.1)) +
  labs(
    title = "Estimated Effect of Real Beer Tax on Traffic Fatalities Across Specifications",
    subtitle = "Point estimates and 95% confidence intervals (Models 2-7 use state-clustered standard errors across 48 states)",
    x = "Estimated Change in Fatality Rate per +$1 Real Beer Tax (deaths per 10,000 pop.)",
    y = NULL,
    caption = "Source: Author's estimation on AER::Fatalities (1982-1988). Model (4) is the preferred baseline TWFE specification (N = 335)."
  ) +
  theme_portfolio(base_size = 11.2) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", color = PALETTE$dark_text)
  )

save_figure(fig4, "04_model_progression_forest.png", width = 10.8, height = 5.6)

message("Core models estimated and Figure 4 saved.")
