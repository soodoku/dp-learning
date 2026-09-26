read_out <- \(name) readr::read_csv(file.path("tabs", name), show_col_types = FALSE)

read_item_catalog <- function(root = Sys.getenv("DP_DATA_ROOT", unset = "../dp-data")) {
  readr::read_csv(
    file.path(root, "metadata", "items.csv"),
    col_types = readr::cols(.default = readr::col_character())
  )
}

item_appendix_markdown <- function(root = Sys.getenv("DP_DATA_ROOT", unset = "../dp-data")) {
  items <- read_item_catalog(root)
  polls <- readr::read_csv(file.path(root, "metadata", "polls.csv"), show_col_types = FALSE)
  items <- dplyr::left_join(
    items, dplyr::select(polls, "poll_id", "title", "year"),
    by = "poll_id", relationship = "many-to-one"
  ) |>
    dplyr::arrange(.data$year, .data$title)
  stopifnot(!anyNA(items$title), !anyDuplicated(items[c("poll_id", "item_id")]))

  lines <- character()
  for (poll_id in unique(items$poll_id)) {
    group <- items[items$poll_id == poll_id, ]
    lines <- c(lines, sprintf("## %s (%s)\n", group$title[1], group$year[1]))
    for (i in seq_len(nrow(group))) {
      item <- group[i, ]
      question <- item$question
      type <- item$response_type
      source <- item$source_column_t1
      detail <- sprintf("**%s** (%s; source `%s`).", question, type, source)
      if (!is.na(item$answer_choices)) {
        detail <- c(detail, paste0("Choices: ", item$answer_choices, "."))
      }
      key <- paste0(item$correct_answer, " [", item$correct_codes, "].")
      detail <- c(detail, paste("Scored correct:", key))
      if (!is.na(item$coding_note)) detail <- c(detail, item$coding_note)
      lines <- c(lines, paste0("- ", paste(detail, collapse = " "), "\n"))
    }
  }
  paste(lines, collapse = "\n")
}

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
  "k1:educationHigh school" = "T1 x high school",
  "k1:educationBA or more" = "T1 x BA or more",
  age_decades = "Age (decades)",
  extremity = "Attitude extremity",
  group_size = "Group size",
  group_k1 = "Groupmates' mean T1",
  group_k1_items = "Groupmates' T1 on missed questions",
  heterogeneity = "Opinion heterogeneity",
  female = "Female",
  p_female = "Group share women",
  "female:p_female" = "Female x group share women",
  minority = "Minority",
  p_minority = "Group share minority",
  "minority:p_minority" = "Minority x group share minority",
  online = "Online",
  poll_k1 = "Poll mean T1",
  read_briefing = "Briefing reading"
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
    dplyr::distinct(model, n, groups, polls, r2_marginal, r2_conditional) |>
    dplyr::mutate(dplyr::across(c(n, groups, polls), \(x) prettyNum(x, big.mark = ","))) |>
    dplyr::mutate(dplyr::across(c(r2_marginal, r2_conditional), \(x) num(x, 3))) |>
    tidyr::pivot_longer(c(n, groups, polls, r2_marginal, r2_conditional), names_to = "label") |>
    tidyr::pivot_wider(names_from = model) |>
    dplyr::mutate(label = c(
      n = "Participants", groups = "Small groups", polls = "Polls",
      r2_marginal = "Marginal R-squared", r2_conditional = "Conditional R-squared"
    )[label])
  dplyr::bind_rows(dplyr::mutate(est, label = as.character(label)), sizes) |>
    dplyr::select(label, dplyr::all_of(keep)) |>
    purrr::set_names(c("", headers))
}
