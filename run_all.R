# End-to-end orchestrator: prepares data, runs models/diagnostics/simulations,
# generates figures, and renders the self-contained Quarto report to docs/index.html.

user_lib <- Sys.getenv("R_LIBS_USER")
if (nzchar(user_lib) && dir.exists(user_lib)) {
  .libPaths(c(user_lib, .libPaths()))
}

required_pkgs <- c(
  "AER", "fixest", "plm", "dplyr", "tidyr", "ggplot2", "readr", "purrr",
  "stringr", "forcats", "tibble", "scales", "patchwork", "modelsummary",
  "gt", "broom", "maps", "mapproj", "knitr", "rmarkdown", "sandwich", "lmtest"
)
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing required R packages: ", paste(missing_pkgs, collapse = ", "))
}

pipeline_scripts <- c(
  "scripts/01_prepare_data.R",
  "scripts/02_eda_visualizations.R",
  "scripts/03_panel_models.R",
  "scripts/04_diagnostics_robustness.R",
  "scripts/05_counterfactual_policy.R"
)

for (script in pipeline_scripts) {
  message("=== Running ", script, " ===")
  source(script, local = new.env(parent = globalenv()))
}

find_quarto <- function() {
  q_path <- Sys.which("quarto")
  if (nzchar(q_path)) return(unname(q_path))
  candidates <- c(
    "C:/Program Files/Quarto/bin/quarto.exe",
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs/Quarto/bin/quarto.exe")
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) > 0) return(found[1])
  ""
}

quarto_bin <- find_quarto()
if (!nzchar(quarto_bin)) {
  stop("Quarto CLI not found on PATH or standard Windows install locations.")
}

rscript_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
Sys.setenv(QUARTO_R = rscript_bin)

message("=== Rendering Quarto technical report ===")
status <- system2(
  quarto_bin,
  args = c("render", shQuote("reports/traffic_fatalities_panel.qmd"))
)
if (!identical(status, 0L)) {
  stop("Quarto render failed with exit status: ", status)
}

dir.create("docs", recursive = TRUE, showWarnings = FALSE)
rendered_html <- "reports/traffic_fatalities_panel.html"
if (!file.exists(rendered_html)) {
  stop("Expected rendered report not found at: ", rendered_html)
}
ok <- file.copy(rendered_html, "docs/index.html", overwrite = TRUE)
if (!ok) {
  stop("Failed to copy rendered HTML to docs/index.html")
}
unlink(rendered_html)

message("=== Pipeline finished successfully. Output: docs/index.html ===")
