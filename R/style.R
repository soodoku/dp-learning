theme_evidence <- function() {
  ggplot2::theme_minimal(base_size = 11, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot"
    )
}

save_evidence <- function(plot, path, width, height) {
  ggplot2::ggsave(paste0(path, ".pdf"), plot, width = width, height = height)
  ggplot2::ggsave(paste0(path, ".png"), plot, width = width, height = height, dpi = 180)
}

# Point estimates and 95% intervals look the same in every figure.
geom_estimate <- function(...) {
  ggplot2::geom_pointrange(
    ggplot2::aes(xmin = lower, xmax = upper),
    shape = 16, size = 0.3, linewidth = 0.45, ...
  )
}

geom_zero <- function() ggplot2::geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3)
