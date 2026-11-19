# Core econometric estimation: cross-sections, long first difference, and panel TWFE progression.

#' Fit preliminary cross-sectional and long first-difference benchmark models
#'
#' @param df Tibble returned by `prepare_fatalities_panel()$panel`.
#' @return Named list of `fixest` model objects and a tidy summary tibble.
fit_preliminary_models <- function(df) {
  cs_1982 <- fixest::feols(
    frate ~ beertax,
    data = dplyr::filter(df, year_num == 1982),
    vcov = "HC1"
  )

  cs_1988 <- fixest::feols(
    frate ~ beertax,
    data = dplyr::filter(df, year_num == 1988),
    vcov = "HC1"
  )

  df_diff <- df |>
    dplyr::filter(year_num %in% c(1982, 1988)) |>
    dplyr::select(state, state_name, year_num, frate, beertax) |>
    tidyr::pivot_wider(names_from = year_num, values_from = c(frate, beertax)) |>
    dplyr::mutate(
      d_frate   = frate_1988 - frate_1982,
      d_beertax = beertax_1988 - beertax_1982
    )

  fd_82_88 <- fixest::feols(
    d_frate ~ d_beertax,
    data = df_diff,
    vcov = "HC1"
  )

  extract_row <- function(mod, label, term_name) {
    ct <- broom::tidy(mod, conf.int = TRUE) |>
      dplyr::filter(term == term_name)
    tibble::tibble(
      model     = label,
      term      = ct$term,
      estimate  = ct$estimate,
      std_error = ct$std.error,
      statistic = ct$statistic,
      p_value   = ct$p.value,
      conf_low  = ct$conf.low,
      conf_high = ct$conf.high,
      n_obs     = stats::nobs(mod),
      r2        = unname(fixest::r2(mod, "r2"))
    )
  }

  summary_tbl <- dplyr::bind_rows(
    extract_row(cs_1982,  "Cross-Section OLS (1982)",     "beertax"),
    extract_row(cs_1988,  "Cross-Section OLS (1988)",     "beertax"),
    extract_row(fd_82_88, "Long First Diff (1988 - 1982)", "d_beertax")
  )

  list(
    models = list(cs_1982 = cs_1982, cs_1988 = cs_1988, fd_82_88 = fd_82_88),
    diff_data = df_diff,
    summary = summary_tbl
  )
}

#' Fit the 7-column panel progression (Pooled OLS -> State FE -> TWFE + Controls)
#'
#' @param df Tibble returned by `prepare_fatalities_panel()$panel`.
#' @return Named list with `models`, `ssc_spec`, and `summary` (for `beertax`).
fit_main_panel_models <- function(df) {
  # Small-sample correction aligning nested fixed effects with Stata xtreg cluster(state)
  ssc_cluster <- fixest::ssc(K.adj = TRUE, K.fixef = "nested", G.adj = TRUE)

  m1_pooled <- fixest::feols(
    frate ~ beertax,
    data = df,
    vcov = "HC1"
  )

  m2_sfe <- fixest::feols(
    frate ~ beertax | state,
    data = df,
    vcov = ~state,
    ssc = ssc_cluster
  )

  m3_twfe <- fixest::feols(
    frate ~ beertax | state + year_num,
    data = df,
    vcov = ~state,
    ssc = ssc_cluster
  )

  m4_baseline <- fixest::feols(
    frate ~ beertax + drinkagec + jail + service + miles_k + unemp + log_income | state + year_num,
    data = df,
    vcov = ~state,
    ssc = ssc_cluster
  )

  m5_nomacro <- fixest::feols(
    frate ~ beertax + drinkagec + jail + service + miles_k | state + year_num,
    data = df,
    vcov = ~state,
    ssc = ssc_cluster
  )

  m6_altform <- fixest::feols(
    frate ~ beertax + drinkage + punish + miles_k + unemp + log_income | state + year_num,
    data = df,
    vcov = ~state,
    ssc = ssc_cluster
  )

  m7_extended <- fixest::feols(
    frate ~ beertax + drinkagec + jail + service + miles_k + unemp + log_income +
      breath + dry + youngdrivers_pct + spirits | state + year_num,
    data = df,
    vcov = ~state,
    ssc = ssc_cluster
  )

  models <- list(
    "(1) Pooled OLS"       = m1_pooled,
    "(2) State FE"         = m2_sfe,
    "(3) Two-Way FE"       = m3_twfe,
    "(4) TWFE Baseline"    = m4_baseline,
    "(5) TWFE No Macro"    = m5_nomacro,
    "(6) TWFE Alt. Policy" = m6_altform,
    "(7) TWFE Extended"    = m7_extended
  )

  meta <- tibble::tribble(
    ~model,                 ~state_fe, ~time_fe, ~se_type,
    "(1) Pooled OLS",       "No",      "No",     "HC1 Robust",
    "(2) State FE",         "Yes",     "No",     "State-Clustered",
    "(3) Two-Way FE",       "Yes",     "Yes",    "State-Clustered",
    "(4) TWFE Baseline",    "Yes",     "Yes",    "State-Clustered",
    "(5) TWFE No Macro",    "Yes",     "Yes",    "State-Clustered",
    "(6) TWFE Alt. Policy", "Yes",     "Yes",    "State-Clustered",
    "(7) TWFE Extended",    "Yes",     "Yes",    "State-Clustered"
  )

  beertax_summary <- purrr::imap_dfr(models, function(mod, name) {
    ct <- broom::tidy(mod, conf.int = TRUE) |>
      dplyr::filter(term == "beertax")
    tibble::tibble(
      model     = name,
      estimate  = ct$estimate,
      std_error = ct$std.error,
      statistic = ct$statistic,
      p_value   = ct$p.value,
      conf_low  = ct$conf.low,
      conf_high = ct$conf.high,
      n_obs     = stats::nobs(mod),
      r2        = unname(fixest::r2(mod, "r2")),
      wr2       = unname(fixest::r2(mod, "wr2")),
      adj_r2    = unname(fixest::r2(mod, "ar2"))
    )
  }) |>
    dplyr::left_join(meta, by = "model")

  list(
    models   = models,
    baseline = m4_baseline,
    summary  = beertax_summary
  )
}
