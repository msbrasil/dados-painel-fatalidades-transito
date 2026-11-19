# Data preparation, quality audit, and variable dictionary for AER::Fatalities.

#' Prepare the US Traffic Fatalities panel dataset (1982-1988, 48 lower US states)
#'
#' @return A named list with elements `panel`, `audit`, `summary_stats`, and `dictionary`.
prepare_fatalities_panel <- function() {
  env <- new.env(parent = emptyenv())
  utils::data("Fatalities", package = "AER", envir = env)
  raw_df <- tibble::as_tibble(env$Fatalities)

  state_name_map <- stats::setNames(datasets::state.name, tolower(datasets::state.abb))
  state_region_map <- stats::setNames(as.character(datasets::state.region), tolower(datasets::state.abb))

  panel <- raw_df |>
    dplyr::mutate(
      state_chr    = as.character(state),
      state_abb    = toupper(state_chr),
      state_name   = unname(state_name_map[state_chr]),
      state_region = factor(unname(state_region_map[state_chr])),
      year_num     = as.integer(as.character(year)),
      # Fatality rates per 10,000 population (or age-specific population for 18-20)
      frate        = fatal / pop * 10000,
      afrate       = afatal / pop * 10000,
      nonafrate    = (fatal - afatal) / pop * 10000,
      nfrate       = nfatal / pop * 10000,
      dfrate       = (fatal - nfatal) / pop * 10000,
      yfrate1820   = fatal1820 / pop1820 * 10000,
      # Control transformations
      miles_k      = miles / 1000,
      log_income   = log(income),
      youngdrivers_pct = youngdrivers * 100,
      drinkagec    = cut(
        drinkage,
        breaks = c(18, 19, 20, 21, 22),
        include.lowest = TRUE,
        right = FALSE,
        labels = c("18", "19", "20", "21+")
      ),
      drinkagec    = stats::relevel(drinkagec, ref = "21+"),
      punish       = dplyr::if_else(
        is.na(jail) | is.na(service),
        NA_character_,
        dplyr::if_else(jail == "yes" | service == "yes", "yes", "no")
      ),
      punish       = factor(punish, levels = c("no", "yes"))
    ) |>
    dplyr::select(-state_chr) |>
    dplyr::group_by(state) |>
    dplyr::mutate(
      frate_within   = frate - mean(frate),
      beertax_within = beertax - mean(beertax)
    ) |>
    dplyr::ungroup()

  # Structural balance & missingness audit
  state_counts <- panel |>
    dplyr::count(state, name = "n_years")

  missing_summary <- panel |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.)))) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "column", values_to = "n_missing") |>
    dplyr::filter(n_missing > 0)

  missing_detail <- if (nrow(missing_summary) > 0) {
    missing_rows <- panel |>
      dplyr::filter(dplyr::if_any(dplyr::all_of(missing_summary$column), is.na)) |>
      dplyr::mutate(row_tag = paste0(state_abb, " (", year_num, ")")) |>
      dplyr::pull(row_tag) |>
      paste(collapse = ", ")
    paste0(
      paste0(missing_summary$column, "=", missing_summary$n_missing, collapse = "; "),
      " | Affected observation(s): ", missing_rows
    )
  } else {
    "None"
  }

  n_complete_ctrl <- panel |>
    dplyr::select(frate, beertax, drinkagec, jail, service, miles_k, unemp, log_income) |>
    stats::complete.cases() |>
    sum()

  audit <- tibble::tibble(
    metric = c(
      "Total panel observations",
      "Number of states (contiguous US, excl. AK/HI/DC)",
      "Time span (annual waves)",
      "Strictly balanced raw panel?",
      "Complete cases for baseline controlled TWFE (M4)",
      "Missingness profile"
    ),
    value = c(
      as.character(nrow(panel)),
      as.character(dplyr::n_distinct(panel$state)),
      paste0(min(panel$year_num), "-", max(panel$year_num), " (T = ", dplyr::n_distinct(panel$year_num), ")"),
      ifelse(all(state_counts$n_years == 7) && nrow(panel) == 336, "Yes (48 x 7 = 336)", "No"),
      paste0(n_complete_ctrl, " / ", nrow(panel)),
      missing_detail
    )
  )

  # Descriptive summary table for key analytical variables
  vars_to_summarize <- tibble::tribble(
    ~var,              ~label,
    "frate",           "Total fatality rate (per 10,000 pop.)",
    "afrate",          "Alcohol-involved fatality rate (per 10,000 pop.)",
    "nonafrate",       "Non-alcohol fatality rate (per 10,000 pop.)",
    "nfrate",          "Nighttime fatality rate (per 10,000 pop.)",
    "dfrate",          "Daytime fatality rate (per 10,000 pop.)",
    "yfrate1820",      "Youth 18-20 fatality rate (per 10,000 aged 18-20)",
    "beertax",         "Real beer tax ($/case, 1988 USD)",
    "drinkage",        "Minimum legal drinking age (years)",
    "miles_k",         "Average miles per driver (thousands)",
    "unemp",           "State unemployment rate (%)",
    "income",          "Real per capita personal income (1987 USD)",
    "dry",             "Population in dry counties (%)",
    "youngdrivers_pct","Drivers aged 15-24 (%)",
    "spirits",         "Per capita spirits consumption"
  )

  summary_stats <- purrr::pmap_dfr(vars_to_summarize, function(var, label) {
    x <- panel[[var]]
    tibble::tibble(
      variable = var,
      label    = label,
      n        = sum(!is.na(x)),
      mean     = mean(x, na.rm = TRUE),
      sd       = stats::sd(x, na.rm = TRUE),
      min      = min(x, na.rm = TRUE),
      p25      = stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE),
      median   = stats::median(x, na.rm = TRUE),
      p75      = stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE),
      max      = max(x, na.rm = TRUE)
    )
  })

  dictionary <- tibble::tribble(
    ~variable,      ~role,        ~unit_or_coding,                        ~description,
    "frate",        "Outcome",    "Deaths per 10,000 residents",          "Total motor-vehicle fatality rate (FARS / state population)",
    "afrate",       "Outcome",    "Deaths per 10,000 residents",          "Alcohol-involved traffic fatality rate",
    "nonafrate",    "Placebo",    "Deaths per 10,000 residents",          "Non-alcohol traffic fatality rate ((fatal - afatal) / pop * 10000)",
    "nfrate",       "Outcome",    "Deaths per 10,000 residents",          "Nighttime traffic fatality rate (midnight to 3:59 AM)",
    "dfrate",       "Comparison", "Deaths per 10,000 residents",          "Daytime / non-nighttime traffic fatality rate",
    "yfrate1820",   "Subgroup",   "Deaths per 10,000 aged 18-20",         "Traffic fatality rate among young adults aged 18-20",
    "beertax",      "Treatment",  "1988 USD per case of beer",            "State real excise tax on a case of beer (US Brewers' Assoc.)",
    "drinkagec",    "Policy",     "Factor: 21+ (ref), 18, 19, 20",        "Discretized minimum legal drinking age",
    "jail",         "Policy",     "Binary factor (no / yes)",             "Mandatory jail sentence for first-time DUI conviction",
    "service",      "Policy",     "Binary factor (no / yes)",             "Mandatory community service for first-time DUI conviction",
    "breath",       "Policy",     "Binary factor (no / yes)",             "Preliminary breath test law authorized",
    "miles_k",      "Exposure",   "Thousands of miles per driver",        "Average annual vehicle miles per licensed driver",
    "unemp",        "Macro",      "Percent (%)",                          "State unemployment rate (BLS)",
    "log_income",   "Macro",      "Log(1987 USD)",                        "Log real per capita personal income (BEA)",
    "dry",          "Cultural",   "Percent (%)",                          "Share of state population residing in dry counties",
    "youngdrivers", "Demographic","Proportion [0, 1]",                    "Proportion of licensed drivers aged 15-24",
    "spirits",      "Control",    "Gallons per capita",                   "Per capita distilled spirits consumption"
  )

  list(
    panel         = panel,
    audit         = audit,
    summary_stats = summary_stats,
    dictionary    = dictionary
  )
}
