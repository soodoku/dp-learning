purrr::walk(list.files("R", full.names = TRUE), source)
dir.create("figs", recursive = TRUE, showWarnings = FALSE)

read_output <- \(name) readr::read_csv(file.path("tabs", name), show_col_types = FALSE)

with_interval <- function(data, estimate, se) {
  dplyr::mutate(data, lower = {{ estimate }} - 1.96 * {{ se }}, upper = {{ estimate }} + 1.96 * {{ se }})
}

pooled_row <- function(x, panel) {
  tibble::tibble(
    panel = panel, label = "Pooled estimate", estimate = x$median, lower = x$lower, upper = x$upper, pooled = TRUE
  )
}

# Rows ordered by estimate within each panel; the pooled estimate sits at the
# bottom, set off by a rule.
forest <- function(data, x_label) {
  data <- dplyr::mutate(
    data,
    row = forcats::fct_reorder(paste(panel, label, sep = "::"), dplyr::if_else(pooled, -Inf, estimate))
  )
  ggplot2::ggplot(data, ggplot2::aes(estimate, row)) +
    geom_zero() +
    ggplot2::geom_hline(yintercept = 1.5, colour = "grey75", linewidth = 0.3) +
    geom_estimate() +
    ggplot2::scale_y_discrete(labels = \(x) sub(".*::", "", x)) +
    ggplot2::facet_grid(panel ~ ., scales = "free_y", space = "free_y") +
    ggplot2::labs(x = x_label, y = NULL) +
    theme_evidence() +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, hjust = 0))
}

# Figure 1. All learning evidence in standard deviations of T1 knowledge: the
# poll's own for pre-post gains, the control group's for effects.
prepost_panel <- "A. Before and after,\nno control group"
controlled_panel <- "B. Against a\ncontrol group"
prepost <- read_output("poll_gains.csv") |>
  dplyr::transmute(
    panel = prepost_panel,
    label = paste0(pollname, dplyr::if_else(online == 1, " (online)", "")),
    estimate = raw_sd, se = raw_sd_se, pooled = FALSE
  ) |>
  with_interval(estimate, se)
controlled <- read_output("control_effects.csv") |>
  dplyr::filter(comparison %in% c(
    "Attended vs uninvited control", "Deliberation vs control villages",
    "Attended vs randomized control", "Attended vs control, one year later"
  )) |>
  dplyr::transmute(
    panel = controlled_panel,
    label = dplyr::case_when(
      grepl("later", comparison) ~ "Climate 2021 (online), one year later",
      study == "America in One Room: Climate 2021" ~ "Climate 2021 (online)",
      study == "Antimicrobial Resistance 2024" ~ "Antimicrobial Resistance 2024 (online)",
      .default = study
    ),
    estimate = estimate_sd, se = std_error / control_t1_sd, pooled = FALSE
  ) |>
  with_interval(estimate, se)
learning <- dplyr::bind_rows(
  prepost,
  pooled_row(dplyr::filter(read_output("meta.csv"), model == "raw SD, pooled", parameter == "mu"), prepost_panel),
  controlled,
  pooled_row(dplyr::filter(read_output("meta_causal.csv"), parameter == "mu"), controlled_panel)
)
p <- forest(learning, "Learning, in standard deviations of T1 knowledge (95% CI)")
save_evidence(p, "figs/learning", width = 6.5, height = 7)

# Figure 2. Three learning estimates for attendees and untreated controls.
measures <- c(raw = "Raw", reset = "Reset correction", lca = "Latent class")
check <- read_output("correction_check.csv") |>
  dplyr::filter(!grepl("later", group)) |>
  tidyr::pivot_longer(
    dplyr::matches("_gain"),
    names_to = c("measure", ".value"),
    names_pattern = "(raw|reset|lca)_(gain_se|gain)$"
  ) |>
  with_interval(gain, gain_se) |>
  dplyr::mutate(
    measure = factor(measures[measure], measures),
    arm = factor(sub(",.*", "", group), c("Controls", "Attendees")),
    study = sub("America in One Room: ", "", study)
  )
p <- ggplot2::ggplot(check, ggplot2::aes(gain, arm)) +
  geom_zero() +
  geom_estimate() +
  ggplot2::facet_grid(study ~ measure) +
  ggplot2::scale_x_continuous(breaks = c(0, 0.1, 0.2)) +
  ggplot2::labs(x = "Learning, change in proportion correct (95% CI)", y = NULL) +
  theme_evidence() +
  ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, hjust = 0))
save_evidence(p, "figs/correction_check", width = 6.5, height = 3.4)

# Figure 3. Within-poll effect of groupmates' T1 knowledge.
peers <- read_output("peer_effects.csv") |>
  dplyr::filter(peer == "k1") |>
  dplyr::transmute(panel = "", label = poll, estimate, se = std_error, pooled = FALSE) |>
  with_interval(estimate, se) |>
  dplyr::bind_rows(pooled_row(dplyr::filter(read_output("peer_effects_pooled.csv"), peer == "k1"), ""))
p <- forest(peers, "Effect of groupmates' mean T1 knowledge (95% CI)") +
  ggplot2::theme(strip.text = ggplot2::element_blank())
save_evidence(p, "figs/peer_effects", width = 6.5, height = 5)
