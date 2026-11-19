# Publication-grade theme and colorblind-safe palette for portfolio figures.

PALETTE <- list(
  navy       = "#1B365D",
  crimson    = "#C84B31",
  teal       = "#2A9D8F",
  amber      = "#D99B26",
  slate      = "#5C6B73",
  light_grey = "#E6E9ED",
  dark_text  = "#1F2933",
  muted_text = "#52606D"
)

#' Cohesive ggplot2 theme for the traffic fatalities portfolio project
#'
#' @param base_size Base font size in points.
#' @return A ggplot2 theme object.
theme_portfolio <- function(base_size = 11.5) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(color = PALETTE$dark_text),
      plot.title = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(1.22),
        color = PALETTE$dark_text,
        margin = ggplot2::margin(b = 4)
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(0.92),
        color = PALETTE$muted_text,
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        size = ggplot2::rel(0.78),
        color = PALETTE$slate,
        face = "italic",
        hjust = 0,
        margin = ggplot2::margin(t = 10)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      panel.grid.major = ggplot2::element_line(color = PALETTE$light_grey, linewidth = 0.45),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.9), color = PALETTE$dark_text),
      axis.text = ggplot2::element_text(color = PALETTE$muted_text, size = ggplot2::rel(0.85)),
      strip.text = ggplot2::element_text(
        face = "bold",
        size = ggplot2::rel(0.92),
        color = PALETTE$navy,
        margin = ggplot2::margin(b = 6, t = 4)
      ),
      strip.background = ggplot2::element_rect(fill = "#F4F6F8", color = NA),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.85)),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.82)),
      plot.margin = ggplot2::margin(14, 16, 12, 14)
    )
}

#' Save a high-resolution PNG figure to outputs/figures/
#'
#' @param plot A ggplot or patchwork object.
#' @param filename Output filename (e.g., "01_us_choropleth_map.png").
#' @param width Width in inches.
#' @param height Height in inches.
#' @param dpi Resolution in DPI.
save_figure <- function(plot, filename, width = 10, height = 6, dpi = 300) {
  out_dir <- "outputs/figures"
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  out_path <- file.path(out_dir, filename)
  ggplot2::ggsave(
    filename = out_path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  message(sprintf("Saved figure: %s", out_path))
  invisible(out_path)
}
