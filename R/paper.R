read_out <- \(name) readr::read_csv(file.path("tabs", name), show_col_types = FALSE)

read_item_catalog <- function(root = Sys.getenv("DP_DATA_ROOT", unset = "../dp-data")) {
  arrow::read_parquet(file.path(root, "output", "analysis", "analysis_items.parquet"))
}

item_appendix_markdown <- function(root = Sys.getenv("DP_DATA_ROOT", unset = "../dp-data"),
                                   items = read_item_catalog(root)) {
  polls <- arrow::read_parquet(file.path(root, "output", "analysis", "analysis_polls.parquet"))
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
      question <- item$question_display
      type <- item$response_type
      detail <- sprintf("**%s** (%s).", question, type)
      if (!is.na(item$answer_choices_display)) {
        detail <- c(detail, paste0("Choices: ", item$answer_choices_display, "."))
      }
      key <- if (item$correct_answer_display == "Answer text not recovered") {
        paste0("Code ", item$correct_codes, " (answer text not recovered).")
      } else {
        paste0(item$correct_answer_display, " [", item$correct_codes, "].")
      }
      detail <- c(detail, paste("Scored correct:", key))
      if (!is.na(item$coding_note)) {
        note <- gsub(" See R/[^[:space:]]+\\.", "", item$coding_note)
        note <- gsub("correct_codes", "the answer key", note, fixed = TRUE)
        note <- gsub("explicit party spellings in metadata/knowledge_items.csv",
          "party-name variants listed in the cited dataset", note, fixed = TRUE
        )
        detail <- c(detail, note)
      }
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

interval <- \(estimate, lower, upper, digits = 3) {
  paste0(coef_text(estimate, digits), " [", coef_text(lower, digits), ", ", coef_text(upper, digits), "]")
}

model_term <- function(models, model, term) {
  row <- models[models$model == model & models$term == term, ]
  interval(row$estimate, row$lower, row$upper)
}

control_row <- \(effects, study, comparison) effects[effects$study == study & effects$comparison == comparison, ]

term_labels <- c(
  "(Intercept)" = "Intercept",
  k1 = "Initial knowledge",
  "educationHigh school" = "Secondary/some college",
  "educationBA or more" = "BA or more",
  "k1:educationHigh school" = "Initial x secondary/some college",
  "k1:educationBA or more" = "Initial x BA or more",
  age_decades = "Age (decades)",
  extremity = "Attitude extremity",
  group_size = "Group size",
  group_k1 = "Groupmates' mean initial",
  group_k1_items = "Groupmates' initial on missed questions",
  heterogeneity = "Opinion heterogeneity",
  disagreement = "Policy disagreement",
  attitude_sd = "Policy dispersion (SD)",
  female = "Female",
  p_female = "Group share women",
  "female:p_female" = "Female x group share women",
  minority = "Minority",
  p_minority = "Group share minority",
  "minority:p_minority" = "Minority x group share minority",
  online = "Online",
  read_briefing = "Briefing reading"
)

row_labels <- term_labels

model_table <- function(models, keep, headers) {
  est <- models |>
    dplyr::filter(model %in% keep, !grepl("pollid", term)) |>
    dplyr::mutate(
      label = factor(unname(row_labels[term]), levels = unique(unname(row_labels))),
      cell = interval(estimate, lower, upper)
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

stack_intervals <- function(table) {
  table[-1] <- lapply(table[-1], function(column) {
    vapply(column, function(cell) {
      if (!nzchar(cell)) return("")
      parts <- strsplit(cell, " [", fixed = TRUE)[[1]]
      if (length(parts) == 1L) return(cell)
      paste0(
        "\\begin{tabular}[t]{@{}r@{}}", parts[1], " \\\\ {[", parts[2],
        "}\\end{tabular}"
      )
    }, character(1))
  })
  table
}

phase_labels <- c(
  arrival_minus_pre_arrival = "Pre-arrival to arrival",
  post_minus_arrival = "Arrival to exit",
  post_minus_pre_arrival = "Pre-arrival to exit"
)

phase_interval_text <- function(estimate, lower, upper) {
  out <- rep("--", length(estimate))
  observed <- is.finite(estimate)
  supported <- observed & is.finite(lower) & is.finite(upper)
  out[observed] <- num(100 * estimate[observed], 1)
  out[supported] <- paste0(
    out[supported], " [", num(100 * lower[supported], 1), ", ",
    num(100 * upper[supported], 1), "]"
  )
  out
}

phase_comparison_table <- function(data) {
  data <- dplyr::filter(data, sample == "all_three_observed_attendees", n_people > 0)
  sizes <- data |>
    dplyr::summarise(
      comparisons = dplyr::n_distinct(contrast), sizes = dplyr::n_distinct(n_people),
      .by = c(poll_id, source_dataset, battery_id)
    )
  stopifnot(all(sizes$comparisons == 3L), all(sizes$sizes == 1L))
  data |>
    dplyr::mutate(cell = phase_interval_text(estimate, lower, upper)) |>
    dplyr::select(pollname, source_dataset, battery_id, n_people, contrast, cell) |>
    tidyr::pivot_wider(names_from = contrast, values_from = cell) |>
    dplyr::arrange(pollname) |>
    dplyr::transmute(
      Poll = pollname, N = prettyNum(n_people, big.mark = ","),
      `Pre-arrival to arrival` = arrival_minus_pre_arrival,
      `Arrival to exit` = post_minus_arrival,
      `Pre-arrival to exit` = post_minus_pre_arrival
    ) |>
    stack_intervals()
}

phase_selection_table <- function(data) {
  data |>
    dplyr::filter(
      analysis == "baseline_selection_difference", source_dataset == "control",
      n_scored > 0L, n_reference_scored > 0L, is.finite(estimate)
    ) |>
    dplyr::arrange(pollname, category) |>
    dplyr::transmute(
      Poll = pollname,
      Comparator = dplyr::recode(category,
        attended_minus_control = "Uninvited controls",
        attended_minus_invited_nonattender = "Invited nonattenders"
      ),
      `Attendee N` = prettyNum(n_scored, big.mark = ","),
      `Comparator N` = prettyNum(n_reference_scored, big.mark = ","),
      `Gap (pp)` = num(100 * estimate, 1)
    )
}

phase_pair_table <- function(data) {
  data |>
    dplyr::filter(sample == "available_paired_attendees", n_people > 0L) |>
    dplyr::arrange(pollname, match(contrast, names(phase_labels))) |>
    dplyr::transmute(
      Poll = pollname, Comparison = unname(phase_labels[contrast]),
      N = prettyNum(n_people, big.mark = ","), `Change (pp)` = num(100 * estimate, 1),
      `95% interval` = dplyr::if_else(is.finite(lower) & is.finite(upper),
        paste(num(100 * lower, 1), num(100 * upper, 1), sep = " to "), "--"
      )
    )
}

phase_attrition_table <- function(data) {
  studies <- c(
    "america-in-one-room-2019", "a1r-climate-2021", "marousi-2006", "tomorrows-europe-2007"
  )
  data |>
    dplyr::filter(analysis == "attendee_exit_attrition", poll_id %in% studies) |>
    dplyr::arrange(pollname, category) |>
    dplyr::transmute(
      Poll = pollname,
      Exit = dplyr::recode(category,
        exit_observed = "Observed", exit_absent = "Absent", exit_unknown = "Uncertain"
      ),
      `Attendee N` = prettyNum(n_people, big.mark = ","),
      `Baseline N` = prettyNum(n_scored, big.mark = ","),
      `Baseline (%)` = dplyr::if_else(is.finite(mean_t0), num(100 * mean_t0, 1), "--")
    )
}
