read_out <- \(name) readr::read_csv(file.path("tabs", name), show_col_types = FALSE)

num <- \(x, digits = 2) formatC(x, format = "f", digits = digits)

# Drops the leading zero, as for coefficients and proportions in the text.
coef_text <- function(x, digits = 3) {
  out <- sub("^(-?)0\\.", "\\1.", num(x, digits))
  sub("^-(\\.0+)$", "\\1", out)
}

est_se <- \(estimate, se, digits = 3) paste0(coef_text(estimate, digits), " (", coef_text(se, digits), ")")

interval <- \(median, lower, upper, digits = 3) {
  paste0(coef_text(median, digits), " [", coef_text(lower, digits), ", ", coef_text(upper, digits), "]")
}

model_term <- function(models, model, term) {
  row <- models[models$model == model & models$term == term, ]
  est_se(row$estimate, row$std_error)
}

meta_row <- function(meta, model, parameter, digits = 3) {
  row <- meta[meta$model == model & meta$parameter == parameter, ]
  interval(row$median, row$lower, row$upper, digits)
}

control_row <- \(effects, study, comparison) effects[effects$study == study & effects$comparison == comparison, ]

term_labels <- c(
  "(Intercept)" = "Intercept",
  k1 = "T1 knowledge",
  "educationHigh school" = "High school",
  "educationBA or more" = "BA or more",
  "k1:educationHigh school" = "T1 knowledge x high school",
  "k1:educationBA or more" = "T1 knowledge x BA or more",
  age_decades = "Age (decades)",
  extremity = "Attitude extremity",
  group_size = "Group size",
  group_k1 = "Group knowledge: others' mean T1",
  group_k1_items = "Group knowledge: others' T1 on items missed",
  heterogeneity = "Opinion heterogeneity",
  female = "Female",
  p_female = "Proportion female in group",
  "female:p_female" = "Female x proportion female",
  minority = "Minority",
  p_minority = "Proportion minority in group",
  "minority:p_minority" = "Minority x proportion minority",
  online = "Online",
  poll_k1 = "Poll mean T1 knowledge",
  read_briefing = "Read briefing materials"
)

row_labels <- term_labels

model_table <- function(models, keep, headers) {
  est <- models |>
    dplyr::filter(model %in% keep, !grepl("pollid", term)) |>
    dplyr::mutate(
      label = factor(unname(row_labels[term]), levels = unique(unname(row_labels))),
      cell = est_se(estimate, std_error)
    ) |>
    dplyr::select(label, model, cell) |>
    tidyr::pivot_wider(names_from = model, values_from = cell, values_fill = "") |>
    dplyr::arrange(label)
  sizes <- models |>
    dplyr::filter(model %in% keep) |>
    dplyr::distinct(model, n, groups, polls) |>
    dplyr::mutate(dplyr::across(c(n, groups, polls), \(x) prettyNum(x, big.mark = ","))) |>
    tidyr::pivot_longer(c(n, groups, polls), names_to = "label") |>
    tidyr::pivot_wider(names_from = model) |>
    dplyr::mutate(label = c(n = "Participants", groups = "Small groups", polls = "Polls")[label])
  dplyr::bind_rows(dplyr::mutate(est, label = as.character(label)), sizes) |>
    dplyr::select(label, dplyr::all_of(keep)) |>
    purrr::set_names(c("", headers))
}
