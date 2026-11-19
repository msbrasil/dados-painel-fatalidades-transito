# Counterfactual policy simulation using Model (4) Baseline TWFE estimates.

#' Simulate annual traffic lives saved in 1988 under real beer tax increase scenarios
#'
#' @param df Tibble returned by `prepare_fatalities_panel()$panel`.
#' @param m4_baseline Fitted `fixest` object for Model (4) Baseline TWFE.
#' @return A list with `national_summary` and `state_breakdown`.
run_counterfactual_simulation <- function(df, m4_baseline) {
  beta_hat <- stats::coef(m4_baseline)[["beertax"]]
  se_hat   <- sqrt(stats::vcov(m4_baseline)[["beertax", "beertax"]])
  df_t     <- fixest::degrees_freedom(m4_baseline, type = "t")
  crit     <- stats::qt(0.975, df = df_t)

  beta_ci_low  <- beta_hat - crit * se_hat
  beta_ci_high <- beta_hat + crit * se_hat

  df_1988 <- df |>
    dplyr::filter(year_num == 1988) |>
    dplyr::select(state, state_abb, state_name, pop, fatal, frate, beertax)

  total_fatal_1988 <- sum(df_1988$fatal)
  tax_p75_1988     <- stats::quantile(df_1988$beertax, 0.75, names = FALSE)

  scenarios <- list(
    list(
      id = "plus_025",
      label = "+$0.25/case uniform increase",
      delta_fn = function(tax) rep(0.25, length(tax))
    ),
    list(
      id = "plus_050",
      label = "+$0.50/case uniform increase",
      delta_fn = function(tax) rep(0.50, length(tax))
    ),
    list(
      id = "plus_100",
      label = "+$1.00/case uniform increase",
      delta_fn = function(tax) rep(1.00, length(tax))
    ),
    list(
      id = "floor_p75",
      label = sprintf("Raise states to 75th pct ($%.2f floor)", tax_p75_1988),
      delta_fn = function(tax) pmax(0, tax_p75_1988 - tax)
    )
  )

  state_breakdown <- purrr::map_dfr(scenarios, function(sc) {
    d_tax <- sc$delta_fn(df_1988$beertax)
    # Note: when beta_hat < 0 and d_tax > 0, delta_frate < 0, so lives_saved > 0.
    # For the 95% CI, more negative beta (beta_ci_low) yields upper bound of lives saved.
    df_1988 |>
      dplyr::mutate(
        scenario_id      = sc$id,
        scenario_label   = sc$label,
        delta_beertax    = d_tax,
        lives_saved_est  = -(beta_hat * delta_beertax) * (pop / 10000),
        lives_saved_low  = -(beta_ci_high * delta_beertax) * (pop / 10000),
        lives_saved_high = -(beta_ci_low * delta_beertax) * (pop / 10000)
      )
  })

  national_summary <- state_breakdown |>
    dplyr::group_by(scenario_id, scenario_label) |>
    dplyr::summarise(
      states_affected    = sum(delta_beertax > 0),
      avg_tax_increase   = mean(delta_beertax),
      total_fatal_1988   = total_fatal_1988,
      lives_saved_est    = sum(lives_saved_est),
      lives_saved_low    = sum(lives_saved_low),
      lives_saved_high   = sum(lives_saved_high),
      pct_reduction_est  = lives_saved_est / total_fatal_1988 * 100,
      pct_reduction_low  = lives_saved_low / total_fatal_1988 * 100,
      pct_reduction_high = lives_saved_high / total_fatal_1988 * 100,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      beta_hat     = beta_hat,
      se_hat       = se_hat,
      p75_tax_1988 = tax_p75_1988
    )

  list(
    national_summary = national_summary,
    state_breakdown  = state_breakdown
  )
}
