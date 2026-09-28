purrr::walk(list.files("R", full.names = TRUE), source)
dir.create("figs", recursive = TRUE, showWarnings = FALSE)

read_output <- \(name) readr::read_csv(file.path("tabs", name), show_col_types = FALSE)

pooled_row <- function(x, panel, label = "Pooled estimate") {
  tibble::tibble(
    panel = panel, label = label, estimate = x$estimate,
    lower = x$lower, upper = x$upper, pooled = TRUE
  )
}

# Rows ordered by estimate within each panel; the pooled estimate sits at the
# bottom, set off by a rule.
forest <- function(data, x_label) {
  rules <- data |>
    dplyr::summarise(yintercept = sum(pooled) + 0.5, .by = panel)
  data <- dplyr::mutate(
    data,
    row = forcats::fct_reorder(paste(panel, label, sep = "::"), dplyr::if_else(pooled, -Inf, estimate))
  )
  ggplot2::ggplot(data, ggplot2::aes(estimate, row)) +
    geom_zero() +
    ggplot2::geom_hline(
      data = rules, ggplot2::aes(yintercept = yintercept),
      inherit.aes = FALSE, colour = "grey75", linewidth = 0.3
    ) +
    geom_estimate() +
    ggplot2::scale_y_discrete(labels = \(x) sub(".*::", "", x)) +
    ggplot2::facet_grid(panel ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = x_label, y = NULL) +
    theme_evidence() +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, hjust = 0))
}

# One scale and one hierarchical-bootstrap interval throughout Figure 1.
prepost_panel <- "A. Attendees before\nand after"
controlled_panel <- "B. Attendee gain minus\ncontrol gain"
prepost <- read_output("poll_gains_main.csv") |>
  dplyr::transmute(
    panel = prepost_panel, label = pollname,
    estimate = 100 * raw, lower = 100 * raw_lower,
    upper = 100 * raw_upper, pooled = FALSE
  )
controlled <- read_output("control_learning.csv") |>
  dplyr::transmute(
    panel = controlled_panel,
    label = paste(dplyr::if_else(poll_id == "a1r-climate-2021", "Climate 2021", "America in One Room 2019"),
      dplyr::if_else(term == "raw", "observed", "guessing adjusted"),
      sep = ": "
    ),
    estimate = 100 * estimate, lower = 100 * lower, upper = 100 * upper, pooled = FALSE
  )
meta <- read_output("meta.csv") |>
  dplyr::mutate(dplyr::across(c(estimate, lower, upper), ~ 100 * .x))
learning <- dplyr::bind_rows(
  prepost,
  pooled_row(
    dplyr::filter(meta, model == "raw, pooled", parameter == "mu"),
    prepost_panel, "Average observed gain"
  ),
  pooled_row(
    dplyr::filter(meta, model == "guessing adjusted, pooled", parameter == "mu"),
    prepost_panel, "Average guessing-adjusted learning"
  ),
  controlled
)
p <- forest(learning, "Knowledge difference (percentage points; 95% confidence interval)")
save_evidence(p, "figs/learning", width = 6.5, height = 7.5)

# Figure 2. Within-poll effect of groupmates' T1 knowledge.
peers <- read_output("peer_effects.csv") |>
  dplyr::filter(peer == "k1") |>
  dplyr::transmute(panel = "", label = poll, estimate, lower, upper, pooled = FALSE) |>
  dplyr::bind_rows(pooled_row(dplyr::filter(read_output("peer_effects_pooled.csv"), peer == "k1"), ""))
p <- forest(peers, "Association of groupmates' mean T1 knowledge (95% CI)") +
  ggplot2::theme(strip.text = ggplot2::element_blank())
save_evidence(p, "figs/peer_effects", width = 6.5, height = 7.5)
