purrr::walk(list.files("R", full.names = TRUE), source)
dir.create("figs", recursive = TRUE, showWarnings = FALSE)

read_output <- \(name) readr::read_csv(file.path("tabs", name), show_col_types = FALSE)

with_interval <- function(data, estimate, se) {
  dplyr::mutate(data, lower = {{ estimate }} - 1.96 * {{ se }}, upper = {{ estimate }} + 1.96 * {{ se }})
}

pooled_row <- function(x, panel, label = "Pooled estimate") {
  tibble::tibble(
    panel = panel, label = label, estimate = x$median,
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

# Figure 1. Standardize pre-post gains by the poll's T1 SD and control
# comparisons by the reference group's observed-wave SD.
prepost_panel <- "A. Attendees before\nand after"
controlled_panel <- "B. Attendees versus\ncontrols"
prepost <- read_output("poll_gains_main.csv") |>
  dplyr::transmute(
    panel = prepost_panel,
    label = paste0(pollname, dplyr::if_else(online == 1, " (online)", "")),
    estimate = raw_sd, se = raw_sd_se, pooled = FALSE
  ) |>
  with_interval(estimate, se)
controlled <- read_output("control_effects.csv") |>
  dplyr::filter(comparison %in% c(
    "Attended vs uninvited control", "Attended vs randomized control",
    "Attended vs control, one year later",
    "T3 attendee vs control, no baseline"
  )) |>
  dplyr::transmute(
    panel = controlled_panel,
    label = dplyr::case_when(
      grepl("later", comparison) ~ "Climate 2021 (online), one year later",
      study == "America in One Room: Climate 2021" ~ "Climate 2021 (online)",
      study == "Antimicrobial Resistance 2024" ~ "Antimicrobial Resistance 2024 (online)",
      study == "Northern Ireland 2007" ~ "Northern Ireland 2007, T3 only",
      .default = study
    ),
    estimate = estimate_sd,
    se = std_error / dplyr::coalesce(control_t1_sd, control_t3_sd),
    pooled = FALSE
  ) |>
  with_interval(estimate, se)
learning <- dplyr::bind_rows(
  prepost,
  pooled_row(
    dplyr::filter(read_output("meta.csv"), model == "raw SD, pooled", parameter == "mu"),
    prepost_panel, "Pooled observed gain"
  ),
  pooled_row(
    dplyr::filter(read_output("meta.csv"), model == "guessing adjusted SD, pooled", parameter == "mu"),
    prepost_panel, "Pooled guessing-adjusted learning"
  ),
  controlled,
  pooled_row(
    dplyr::filter(read_output("meta_control.csv"), parameter == "mu"),
    controlled_panel, "Pooled T1-adjusted (three polls)"
  )
)
p <- forest(learning, "Knowledge difference, in reference SDs (95% CI)")
save_evidence(p, "figs/learning", width = 6.5, height = 7.5)

# Figure 2. Within-poll effect of groupmates' T1 knowledge.
peers <- read_output("peer_effects.csv") |>
  dplyr::filter(peer == "k1") |>
  dplyr::transmute(panel = "", label = poll, estimate, se = std_error, pooled = FALSE) |>
  with_interval(estimate, se) |>
  dplyr::bind_rows(pooled_row(dplyr::filter(read_output("peer_effects_pooled.csv"), peer == "k1"), ""))
p <- forest(peers, "Association of groupmates' mean T1 knowledge (95% CI)") +
  ggplot2::theme(strip.text = ggplot2::element_blank())
save_evidence(p, "figs/peer_effects", width = 6.5, height = 7.5)
