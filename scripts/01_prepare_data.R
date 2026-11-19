# Step 1: Prepare and audit the AER::Fatalities panel dataset.

source("R/data_prep.R")

message("Preparing US Traffic Fatalities panel dataset (1982-1988)...")
prep <- prepare_fatalities_panel()

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

saveRDS(prep$panel, "data/processed/fatalities_panel.rds")
readr::write_csv(prep$panel, "data/processed/fatalities_panel.csv")
readr::write_csv(prep$audit, "outputs/tables/data_audit.csv")
readr::write_csv(prep$summary_stats, "outputs/tables/summary_stats.csv")
readr::write_csv(prep$dictionary, "outputs/tables/data_dictionary.csv")

message("Data preparation complete: ", nrow(prep$panel), " observations across ",
        dplyr::n_distinct(prep$panel$state), " states.")
