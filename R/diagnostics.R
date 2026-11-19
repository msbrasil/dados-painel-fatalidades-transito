# Formal panel diagnostics, joint Wald exclusion tests, and standard error comparisons.

#' Run formal panel specification diagnostics, joint Wald tests, and SE sensitivity checks
#'
#' @param df Tibble returned by `prepare_fatalities_panel()$panel`.
#' @param m4_baseline Fitted `fixest` object for Model (4) Baseline TWFE.
#' @return A list with `panel_tests`, `wald_tests`, and `se_comparison`.
run_panel_diagnostics <- function(df, m4_baseline) {
  # Complete-case subset for controlled models (drops ca 1988 missing jail/service)
  df_cc <- df |>
    dplyr::filter(!is.na(jail), !is.na(service))

  pdf_all <- plm::pdata.frame(as.data.frame(df), index = c("state", "year_num"))
  pdf_cc  <- plm::pdata.frame(as.data.frame(df_cc), index = c("state", "year_num"))

  # Bivariate models for Poolability & Hausman
  plm_pool_biv  <- plm::plm(frate ~ beertax, data = pdf_all, model = "pooling")
  plm_fe_biv    <- plm::plm(frate ~ beertax, data = pdf_all, model = "within", effect = "individual")
  plm_twfe_biv  <- plm::plm(frate ~ beertax, data = pdf_all, model = "within", effect = "twoways")
  plm_re_biv    <- plm::plm(frate ~ beertax, data = pdf_all, model = "random", effect = "individual")

  # Controlled models without year dummies in between matrix (avoids Swamy-Arora singularity)
  ctrl_noyr_formula <- frate ~ beertax + drinkagec + jail + service + miles_k + unemp + log_income
  plm_sfe_ctrl  <- plm::plm(ctrl_noyr_formula, data = pdf_cc, model = "within", effect = "individual")
  plm_re_ctrl   <- plm::plm(ctrl_noyr_formula, data = pdf_cc, model = "random", effect = "individual")
  plm_twfe_ctrl <- plm::plm(ctrl_noyr_formula, data = pdf_cc, model = "within", effect = "twoways")

  # Controlled models with explicit year dummies for Poolability, Serial Correlation, and CD tests
  ctrl_formula  <- frate ~ beertax + drinkagec + jail + service + miles_k + unemp + log_income + factor(year_num)
  plm_pool_ctrl <- plm::plm(ctrl_formula, data = pdf_cc, model = "pooling")
  plm_fe_ctrl   <- plm::plm(ctrl_formula, data = pdf_cc, model = "within", effect = "individual")

  # 1. Poolability F-tests
  f_state_vs_pool <- plm::pFtest(plm_fe_ctrl, plm_pool_ctrl)
  f_twfe_vs_sfe   <- plm::pFtest(plm_twfe_ctrl, plm_sfe_ctrl)

  # 2. Breusch-Pagan LM test (Random Effects vs Pooled OLS)
  bp_lm_ctrl <- plm::plmtest(plm_pool_ctrl, type = "bp")

  # 3. Hausman FE vs RE tests (Classical Swamy-Arora & Cluster-Robust Aux Regression)
  hausman_biv  <- plm::phtest(plm_fe_biv, plm_re_biv)
  hausman_ctrl <- plm::phtest(plm_sfe_ctrl, plm_re_ctrl)
  hausman_rob  <- plm::phtest(
    ctrl_noyr_formula,
    data = pdf_cc,
    method = "aux",
    vcov = function(x) plm::vcovHC(x, type = "HC1", cluster = "group")
  )

  # 4. Serial correlation tests (Breusch-Godfrey/Wooldridge & Wooldridge AR(1) FE)
  bg_serial   <- plm::pbgtest(plm_fe_ctrl)
  woold_serial <- plm::pwartest(plm_fe_ctrl)

  # 5. Cross-sectional dependence test (Pesaran CD)
  pesaran_cd  <- plm::pcdtest(plm_fe_ctrl, test = "cd")

  extract_htest <- function(test_obj, test_name, null_hyp, conclusion) {
    df_str <- if (length(test_obj$parameter) > 0) {
      paste0(names(test_obj$parameter), "=", round(unname(test_obj$parameter), 1), collapse = ", ")
    } else {
      "-"
    }
    tibble::tibble(
      test          = test_name,
      null_hyp      = null_hyp,
      statistic_lbl = paste0(names(test_obj$statistic), " = ", sprintf("%.3f", unname(test_obj$statistic))),
      statistic     = unname(test_obj$statistic),
      df            = df_str,
      p_value       = test_obj$p.value,
      conclusion    = conclusion
    )
  }

  panel_tests <- dplyr::bind_rows(
    extract_htest(
      f_state_vs_pool,
      "F-test for State Fixed Effects (vs. Pooled OLS)",
      "H0: All state intercepts are equal (no state FE)",
      ifelse(f_state_vs_pool$p.value < 0.05, "Reject H0: State FE strictly required", "Fail to reject H0")
    ),
    extract_htest(
      f_twfe_vs_sfe,
      "F-test for Time Fixed Effects (TWFE vs. State FE)",
      "H0: All year effects equal zero",
      ifelse(f_twfe_vs_sfe$p.value < 0.05, "Reject H0: Two-way FE preferred", "Fail to reject H0: Time shocks weak")
    ),
    extract_htest(
      bp_lm_ctrl,
      "Breusch-Pagan Lagrange Multiplier (RE vs. Pooled)",
      "H0: Variance of unobserved state effect = 0",
      ifelse(bp_lm_ctrl$p.value < 0.05, "Reject H0: Unobserved heterogeneity present", "Fail to reject H0")
    ),
    extract_htest(
      hausman_biv,
      "Hausman Specification Test (Bivariate: FE vs. RE)",
      "H0: Random effects estimator is consistent",
      ifelse(hausman_biv$p.value < 0.05, "Reject H0: Use Fixed Effects", "Fail to reject H0")
    ),
    extract_htest(
      hausman_ctrl,
      "Hausman Specification Test (Controlled: FE vs. RE)",
      "H0: Random effects estimator is consistent",
      ifelse(hausman_ctrl$p.value < 0.05, "Reject H0: Use Fixed Effects", "Fail to reject H0")
    ),
    extract_htest(
      hausman_rob,
      "Cluster-Robust Hausman Test (Aux. Regression)",
      "H0: Random effects consistent under state clustering",
      ifelse(hausman_rob$p.value < 0.05, "Reject H0: Fixed Effects strictly required", "Fail to reject H0")
    ),
    extract_htest(
      bg_serial,
      "Breusch-Godfrey / Wooldridge Serial Correlation Test",
      "H0: No serial correlation in idiosyncratic errors",
      ifelse(bg_serial$p.value < 0.05, "Reject H0: Serial correlation -> Cluster SEs", "Fail to reject H0")
    ),
    extract_htest(
      woold_serial,
      "Wooldridge First-Difference AR(1) Test (FE)",
      "H0: No AR(1) autocorrelation in FE residuals",
      ifelse(woold_serial$p.value < 0.05, "Reject H0: Autocorrelation -> Cluster SEs", "Fail to reject H0")
    ),
    extract_htest(
      pesaran_cd,
      "Pesaran CD Cross-Sectional Dependence Test",
      "H0: Cross-sectional independence across states",
      ifelse(pesaran_cd$p.value < 0.05, "Reject H0: Compare Driscoll-Kraay SEs", "Fail to reject H0: Weak spatial correlation")
    )
  )

  # Joint Wald Exclusion Tests on Model (4) using State-Clustered VCOV
  ssc_cluster <- fixest::ssc(K.adj = TRUE, K.fixef = "nested", G.adj = TRUE)
  m4_explicit_yr <- fixest::feols(
    frate ~ beertax + drinkagec + jail + service + miles_k + unemp + log_income + factor(year_num) | state,
    data = df_cc,
    vcov = ~state,
    ssc = ssc_cluster
  )

  w_drinkage <- fixest::wald(m4_baseline, keep = "drinkagec", print = FALSE)
  w_punish   <- fixest::wald(m4_baseline, keep = "jail|service", print = FALSE)
  w_macro    <- fixest::wald(m4_baseline, keep = "unemp|log_income", print = FALSE)
  w_years    <- fixest::wald(m4_explicit_yr, keep = "year_num", print = FALSE)

  extract_wald <- function(w_obj, group_label, df1_val) {
    tibble::tibble(
      hypothesis_group = group_label,
      df1              = df1_val,
      df2              = w_obj$df2,
      f_stat           = w_obj$stat,
      p_value          = w_obj$p,
      significant_5pct = ifelse(w_obj$p < 0.05, "Yes (p < 0.05)", "No (p >= 0.05)")
    )
  }

  wald_tests <- dplyr::bind_rows(
    extract_wald(w_drinkage, "Minimum Legal Drinking Age dummies (18, 19, 20)", 3L),
    extract_wald(w_punish,   "Mandatory Punishment laws (jail, service)",        2L),
    extract_wald(w_macro,    "State Macroeconomic Controls (unemp, log_income)", 2L),
    extract_wald(w_years,    "Year Fixed Effects (1983-1988 dummies)",           6L)
  )

  # Standard Error Comparison across 5 covariance estimators on Model (4)
  m4_panel <- fixest::feols(
    frate ~ beertax + drinkagec + jail + service + miles_k + unemp + log_income | state + year_num,
    data = df_cc,
    panel.id = ~state + year_num
  )

  vcov_specs <- list(
    "IID (Homoskedastic)"           = stats::vcov(m4_panel, vcov = "iid"),
    "HC1 (Heteroskedasticity-Robust)" = stats::vcov(m4_panel, vcov = "HC1"),
    "State-Clustered (48 clusters)" = stats::vcov(m4_panel, vcov = ~state, ssc = ssc_cluster),
    "Two-Way Clustered (State + Year)" = stats::vcov(m4_panel, vcov = ~state + year_num, ssc = ssc_cluster),
    "Driscoll-Kraay (Spatial + AR)" = fixest::vcov_DK(m4_panel, time = ~year_num)
  )

  beta_bt <- stats::coef(m4_panel)[["beertax"]]
  se_comparison <- purrr::imap_dfr(vcov_specs, function(V, label) {
    se_bt <- sqrt(V["beertax", "beertax"])
    # Use cluster df (47) for clustered/DK, residual df for IID/HC1
    df_t <- if (grepl("IID|HC1", label)) {
      fixest::degrees_freedom(m4_panel, type = "resid")
    } else if (grepl("Two-Way", label)) {
      min(47, 6)
    } else {
      47
    }
    t_stat <- beta_bt / se_bt
    p_val  <- 2 * stats::pt(abs(t_stat), df = df_t, lower.tail = FALSE)
    crit   <- stats::qt(0.975, df = df_t)
    tibble::tibble(
      vcov_estimator = label,
      estimate       = beta_bt,
      std_error      = se_bt,
      df_t           = df_t,
      t_stat         = t_stat,
      p_value        = p_val,
      conf_low       = beta_bt - crit * se_bt,
      conf_high      = beta_bt + crit * se_bt
    )
  })

  list(
    panel_tests   = panel_tests,
    wald_tests    = wald_tests,
    se_comparison = se_comparison
  )
}
