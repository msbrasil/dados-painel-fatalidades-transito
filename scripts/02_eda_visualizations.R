# Step 2: Exploratory data analysis and signature visualizations.

library(ggplot2)
library(patchwork)

source("R/theme_config.R")

df <- readRDS("data/processed/fatalities_panel.rds")

# ---------------------------------------------------------------------------
# Figure 1: Contiguous US Choropleth Maps (1988 Fatality Rate vs Beer Tax)
# ---------------------------------------------------------------------------
us_map <- ggplot2::map_data("state")
df_1988 <- df |>
  dplyr::filter(year_num == 1988) |>
  dplyr::mutate(region = tolower(state_name))

map_joined <- dplyr::left_join(us_map, df_1988, by = "region", relationship = "many-to-many")

p_map_frate <- ggplot(map_joined, aes(x = long, y = lat, group = group, fill = frate)) +
  geom_polygon(color = "white", linewidth = 0.25) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient(
    low = "#EDF2F7",
    high = PALETTE$crimson,
    na.value = "#D9DEE3",
    name = "Deaths / 10k"
  ) +
  labs(
    title = "A. Traffic Fatality Rate (1988)",
    subtitle = "Annual motor-vehicle deaths per 10,000 residents"
  ) +
  theme_portfolio(base_size = 10.5) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    legend.key.width = unit(1.1, "cm")
  )

p_map_tax <- ggplot(map_joined, aes(x = long, y = lat, group = group, fill = beertax)) +
  geom_polygon(color = "white", linewidth = 0.25) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_gradient(
    low = "#EDF7F6",
    high = PALETTE$navy,
    na.value = "#D9DEE3",
    labels = scales::dollar_format(),
    name = "Tax ($/case)"
  ) +
  labs(
    title = "B. Real Beer Excise Tax (1988)",
    subtitle = "State excise tax per case of beer (1988 USD)"
  ) +
  theme_portfolio(base_size = 10.5) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    legend.key.width = unit(1.1, "cm")
  )

fig1 <- (p_map_frate + p_map_tax) +
  plot_annotation(
    title = "Geographic Distribution of Traffic Fatalities and Alcohol Taxation (1988)",
    subtitle = "Cross-sectional variation across the 48 contiguous US states (Alaska, Hawaii, and DC excluded from FARS panel)",
    caption = "Source: AER::Fatalities (FARS & US Brewers' Association). District of Columbia shaded grey (not in sample).",
    theme = theme_portfolio(base_size = 11.5)
  )

save_figure(fig1, "01_us_choropleth_map.png", width = 12.5, height = 5.6)

# ---------------------------------------------------------------------------
# Figure 2: The Omitted Variable Bias Paradox (Pooled OLS vs Within-State FE)
# ---------------------------------------------------------------------------
m_pool <- fixest::feols(frate ~ beertax, data = df, vcov = "HC1")
m_sfe  <- fixest::feols(
  frate ~ beertax | state,
  data = df,
  vcov = ~state,
  ssc = fixest::ssc(K.adj = TRUE, K.fixef = "nested", G.adj = TRUE)
)

ct_pool <- broom::tidy(m_pool) |> dplyr::filter(term == "beertax")
ct_sfe  <- broom::tidy(m_sfe)  |> dplyr::filter(term == "beertax")

lbl_pool <- sprintf(
  "Pooled OLS slope: +%.3f\n(HC1 SE = %.3f, p = %.4f)",
  ct_pool$estimate, ct_pool$std.error, ct_pool$p.value
)
lbl_sfe <- sprintf(
  "State FE slope: %.3f\n(Clustered SE = %.3f, p = %.4f)",
  ct_sfe$estimate, ct_sfe$std.error, ct_sfe$p.value
)

p_ovb_left <- ggplot(df, aes(x = beertax, y = frate)) +
  geom_point(alpha = 0.42, size = 2.0, color = PALETTE$slate) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = PALETTE$crimson, fill = PALETTE$crimson, alpha = 0.16, linewidth = 1.15) +
  annotate(
    "label",
    x = max(df$beertax) * 0.98,
    y = max(df$frate) * 0.97,
    label = lbl_pool,
    hjust = 1, vjust = 1,
    size = 3.3, fontface = "bold",
    color = PALETTE$crimson, fill = "white", linewidth = 0.35
  ) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(
    title = "A. Naive Pooled OLS (Confounded)",
    subtitle = "Between-state differences create a spurious positive correlation",
    x = "Real Beer Tax ($ per case, 1988 USD)",
    y = "Fatality Rate (deaths per 10,000 pop.)"
  ) +
  theme_portfolio(base_size = 11)

p_ovb_right <- ggplot(df, aes(x = beertax_within, y = frate_within)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = PALETTE$light_grey, linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = PALETTE$light_grey, linewidth = 0.7) +
  geom_point(alpha = 0.45, size = 2.0, color = PALETTE$slate) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = PALETTE$teal, fill = PALETTE$teal, alpha = 0.18, linewidth = 1.15) +
  annotate(
    "label",
    x = max(df$beertax_within) * 0.98,
    y = max(df$frate_within) * 0.97,
    label = lbl_sfe,
    hjust = 1, vjust = 1,
    size = 3.3, fontface = "bold",
    color = PALETTE$navy, fill = "white", linewidth = 0.35
  ) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(
    title = "B. Within-State Fixed Effects (De-meaned)",
    subtitle = "Removing time-invariant state heterogeneity reverses the sign",
    x = "Within-State Deviation in Beer Tax ($ per case)",
    y = "Within-State Deviation in Fatality Rate"
  ) +
  theme_portfolio(base_size = 11)

fig2 <- (p_ovb_left + p_ovb_right) +
  plot_annotation(
    title = "The Omitted Variable Bias Paradox: Why Cross-Sectional OLS Misleads Policy Analysis",
    subtitle = "48 contiguous US states observed annually (1982-1988, N = 336 state-years)",
    caption = "Note: Panel B plots deviations from each state's 1982-1988 mean. Standard errors clustered by state (48 clusters) in Panel B.",
    theme = theme_portfolio(base_size = 11.5)
  )

save_figure(fig2, "02_ovb_paradox_comparison.png", width = 12, height = 5.6)

# ---------------------------------------------------------------------------
# Figure 3: Temporal Trajectories & Within-State Tax Variation (1982-1988)
# ---------------------------------------------------------------------------
us_mean <- df |>
  dplyr::group_by(year_num) |>
  dplyr::summarise(mean_frate = mean(frate), .groups = "drop")

state_extremes <- df |>
  dplyr::group_by(state, state_abb) |>
  dplyr::summarise(avg_frate = mean(frate), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(avg_frate))

top_states <- c(state_extremes$state_abb[1], state_extremes$state_abb[2],
                tail(state_extremes$state_abb, 2))

df_hl <- df |>
  dplyr::filter(state_abb %in% top_states)

df_hl_end <- df_hl |>
  dplyr::filter(year_num == 1988) |>
  dplyr::arrange(dplyr::desc(frate)) |>
  dplyr::mutate(
    y_lbl = frate + c(0.14, -0.14, 0.12, -0.12)
  )

p_traj <- ggplot() +
  geom_line(
    data = df,
    aes(x = year_num, y = frate, group = state),
    color = PALETTE$slate, alpha = 0.22, linewidth = 0.45
  ) +
  geom_line(
    data = df_hl,
    aes(x = year_num, y = frate, group = state_abb),
    color = PALETTE$amber, linewidth = 0.85, alpha = 0.85
  ) +
  geom_line(
    data = us_mean,
    aes(x = year_num, y = mean_frate),
    color = PALETTE$navy, linewidth = 1.4
  ) +
  geom_point(
    data = us_mean,
    aes(x = year_num, y = mean_frate),
    color = PALETTE$navy, size = 2.3
  ) +
  geom_segment(
    data = df_hl_end,
    aes(x = 1988, xend = 1988.16, y = frate, yend = y_lbl),
    color = PALETTE$amber, linewidth = 0.6
  ) +
  geom_text(
    data = df_hl_end,
    aes(x = 1988.20, y = y_lbl, label = state_abb),
    hjust = 0, size = 3.1, fontface = "bold", color = PALETTE$dark_text
  ) +
  annotate(
    "text", x = 1988.20, y = us_mean$mean_frate[us_mean$year_num == 1988],
    label = "48-State Mean", hjust = 0, size = 3.2, fontface = "bold", color = PALETTE$navy
  ) +
  scale_x_continuous(breaks = 1982:1988, limits = c(1982, 1989.5)) +
  labs(
    title = "A. State Fatality Rate Trajectories (1982-1988)",
    subtitle = "Persistent level gaps across states motivate fixed effects",
    x = "Year",
    y = "Fatality Rate (deaths per 10,000 pop.)"
  ) +
  theme_portfolio(base_size = 10.8)

tax_change <- df |>
  dplyr::filter(year_num %in% c(1982, 1988)) |>
  dplyr::select(state_abb, year_num, beertax) |>
  tidyr::pivot_wider(names_from = year_num, values_from = beertax, names_prefix = "y") |>
  dplyr::mutate(
    d_tax = y1988 - y1982,
    direction = dplyr::if_else(d_tax >= 0, "Real Tax Increase", "Real Tax Erosion (Inflation)")
  ) |>
  dplyr::arrange(d_tax)

# Show top 10 increases and top 10 decreases (most informative identifying variation)
tax_extremes <- dplyr::bind_rows(
  dplyr::slice_head(tax_change, n = 10),
  dplyr::slice_tail(tax_change, n = 10)
) |>
  dplyr::distinct(state_abb, .keep_all = TRUE) |>
  dplyr::mutate(state_abb = forcats::fct_reorder(state_abb, d_tax))

p_tax_var <- ggplot(tax_extremes, aes(x = d_tax, y = state_abb, color = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = PALETTE$slate, linewidth = 0.7) +
  geom_segment(aes(x = 0, xend = d_tax, y = state_abb, yend = state_abb), linewidth = 0.9) +
  geom_point(size = 2.6) +
  scale_color_manual(
    values = c("Real Tax Increase" = PALETTE$teal, "Real Tax Erosion (Inflation)" = PALETTE$crimson),
    name = NULL
  ) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(
    title = "B. Largest Real Beer Tax Changes (1982 to 1988)",
    subtitle = "Top 10 positive hikes vs. top 10 inflation-driven erosions",
    x = "Change in Real Beer Tax ($/case, 1988 USD)",
    y = NULL
  ) +
  theme_portfolio(base_size = 10.8) +
  theme(legend.position = "bottom")

fig3 <- (p_traj + p_tax_var) +
  plot_annotation(
    title = "Panel Dynamics: Persistent Cross-State Heterogeneity and Identifying Tax Variation",
    subtitle = "States exhibit large baseline fatality gaps alongside asymmetric real beer tax changes over 1982-1988",
    caption = "Source: AER::Fatalities. Panel A highlights the two highest- and two lowest-average fatality states.",
    theme = theme_portfolio(base_size = 11.5)
  )

save_figure(fig3, "03_panel_trajectories.png", width = 12.5, height = 5.8)

message("EDA visualizations saved successfully.")
