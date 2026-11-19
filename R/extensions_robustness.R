# Analytical extensions: mechanism & placebo outcomes, alternative estimators, and LOSO jackknife.

#' Estimate baseline TWFE effect of beer tax across alcohol-sensitive and placebo outcomes
#'
#' @param df Tibble returned by `prepare_fatalities_panel()$panel`.
#' @return A list with `models` and `summary` tibble (raw and % of sample mean effects).
fit_subgroup_outcomes <- function(df) {
  ssc_cluster <- fixest::ssc(K.adj = TRUE, K.fixef = "nested", G.adj = TRUE)

  outcomes <- tibble::tribble(
    ~outcome_var, ~label,                         ~category,
    "frate",      "Total Fatalities",             "Overall",
    "afrate",     "Alcohol-Involved Fatalities",  "Alcohol-Sensitive Mechanism",
    "nfrate",     "Nighttime Fatalities (00-04h)","Alcohol-Sensitive Mechanism",
    "yfrate1820", "Youth 18-20 Fatalities",       "Alcohol-Sensitive Mechanism",
    "dfrate",     "Daytime Fatalities",           "Lower-Alcohol Comparison / Placebo",
    "nonafrate",  "Non-Alcohol Fatalities",       "Lower-Alcohol Comparison / Placebo"
  )

  rhs <- "beertax + drinkagec + jail + service + miles_k + unemp + log_income | state + year_num"

  models <- list()
  summary_tbl <- purrr::pmap_dfr(outcomes, function(outcome_var, label, category) {
    fml <- stats::as.formula(paste(outcome_var, "~", rhs))
    mod <- fixest::feols(fml, data = df, vcov = ~state, ssc = ssc_cluster)
    models[[label]] <<- mod

    ct <- broom::tidy(mod, conf.int = TRUE) |>
      dplyr::filter(term == "beertax")
    mean_y <- mean(df[[outcome_var]], na.rm = TRUE)

    tibble::tibble(
      outcome_var = outcome_var,
      outcome     = label,
      category    = category,
      mean_y      = mean_y,
      estimate    = ct$estimate,
      std_error   = ct$std.error,
      p_value     = ct$p.value,
      conf_low    = ct$conf.low,
      conf_high   = ct$conf.high,
      pct_effect  = ct$estimate / mean_y * 100,
      pct_low     = ct$conf.low / mean_y * 100,
      pct_high    = ct$conf.high / mean_y * 100,
      n_obs       = stats::nobs(mod)
    )
  })

  list(models = models, summary = summary_tbl)
}

#' Fit alternative estimator robustness checks (Baseline TWFE, Log-Linear, WLS, First-Differences)
#'
#' @param df Tibble returned by `prepare_fatalities_panel()$panel`.
#' @return A list with `models` and `summary` tibble.
fit_robustness_specs <- function(df) {
  ssc_cluster <- fixest::ssc(K.adj = TRUE, K.fixef = "nested", G.adj = TRUE)

  m_twfe <- fixest::feols(
    frate ~ beertax + drinkagec + jail + service + miles_k + unemp + log_income | state + year_num,
    data = df,
    vcov = ~state,
    ssc = ssc_cluster
  )

  m_log <- fixest::feols(
    log(frate) ~ beertax + drinkagec + jail + service + miles_k + unemp + log_income | state + year_num,
    data = df,
    vcov = ~state,
    ssc = ssc_cluster
  )

  m_wls <- fixest::feols(
    frate ~ beertax + drinkagec + jail + service + miles_k + unemp + log_income | state + year_num,
    data = df,
    weights = ~pop,
    vcov = ~state,
    ssc = ssc_cluster
  )

  # Adjacent-year First-Difference panel estimator via plm (with year dummies & state-clustered SE)
  df_cc <- df |> dplyr::filter(!is.na(jail), !is.na(service))
  pdf_cc <- plm::pdata.frame(as.data.frame(df_cc), index = c("state", "year_num"))
  m_fd <- plm::plm(
    frate ~ beertax + drinkagec + jail + service + miles_k + unemp + log_income + factor(year_num),
    data = pdf_cc,
    model = "fd"
  )
  vcov_fd <- plm::vcovHC(m_fd, type = "HC1", cluster = "group")
  ct_fd <- lmtest::coeftest(m_fd, vcov. = vcov_fd)
  fd_est <- ct_fd["beertax", "Estimate"]
  fd_se  <- ct_fd["beertax", "Std. Error"]
  fd_p   <- ct_fd["beertax", "Pr(>|t|)"]
  fd_crit <- stats::qt(0.975, df = 47)

  extract_fixest <- function(mod, label, outcome_scale) {
    ct <- broom::tidy(mod, conf.int = TRUE) |>
      dplyr::filter(term == "beertax")
    tibble::tibble(
      specification = label,
      outcome_scale = outcome_scale,
      estimate      = ct$estimate,
      std_error     = ct$std.error,
      p_value       = ct$p.value,
      conf_low      = ct$conf.low,
      conf_high     = ct$conf.high,
      n_obs         = stats::nobs(mod)
    )
  }

  summary_tbl <- dplyr::bind_rows(
    extract_fixest(m_twfe, "Baseline TWFE (Unweighted)",        "Deaths per 10,000 pop."),
    extract_fixest(m_wls,  "Population-Weighted TWFE (WLS)",    "Deaths per 10,000 pop."),
    tibble::tibble(
      specification = "Panel First-Differences (plm FD)",
      outcome_scale = "Deaths per 10,000 pop.",
      estimate      = fd_est,
      std_error     = fd_se,
      p_value       = fd_p,
      conf_low      = fd_est - fd_crit * fd_se,
      conf_high     = fd_est + fd_crit * fd_se,
      n_obs         = stats::nobs(m_fd)
    ),
    extract_fixest(m_log,  "Log-Linear TWFE (Semi-Elasticity)", "Log(frate): % change / 100")
  )

  list(
    models = list(twfe = m_twfe, log_linear = m_log, wls = m_wls, fd = m_fd),
    summary = summary_tbl
  )
}

#' 48-state Leave-One-State-Out (LOSO) jackknife sensitivity analysis on Baseline TWFE
#'
#' @param df Tibble returned by `prepare_fatalities_panel()$panel`.
#' @return A tibble with 48 rows sorted by point estimate.
run_leave_one_state_out <- function(df) {
  ssc_cluster <- fixest::ssc(K.adj = TRUE, K.fixef = "nested", G.adj = TRUE)
  full_mod <- fixest::feols(
    frate ~ beertax + drinkagec + jail + service + miles_k + unemp + log_income | state + year_num,
    data = df,
    vcov = ~state,
    ssc = ssc_cluster
  )
  full_est <- stats::coef(full_mod)[["beertax"]]

  states <- df |>
    dplyr::distinct(state, state_abb, state_name) |>
    dplyr::arrange(state_abb)

  purrr::pmap_dfr(states, function(state, state_abb, state_name) {
    sub_df <- df |> dplyr::filter(.data$state != .env$state)
    mod <- fixest::feols(
      frate ~ beertax + drinkagec + jail + service + miles_k + unemp + log_income | state + year_num,
      data = sub_df,
      vcov = ~state,
      ssc = ssc_cluster
    )
    ct <- broom::tidy(mod, conf.int = TRUE) |>
      dplyr::filter(term == "beertax")
    tibble::tibble(
      dropped_state = state_abb,
      state_name    = state_name,
      estimate      = ct$estimate,
      std_error     = ct$std.error,
      conf_low      = ct$conf.low,
      conf_high     = ct$conf.high,
      p_value       = ct$p.value,
      delta_from_full = ct$estimate - full_est
    )
  }) |>
    dplyr::arrange(estimate)
}
