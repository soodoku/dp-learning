add_relative_education <- function(frame, polardata = read_polardata(), polls = read_respondent_sources()) {
  definitions <- polardata |>
    dplyr::left_join(dplyr::select(polls, poll_id, dpnum), by = "dpnum", relationship = "many-to-one") |>
    dplyr::transmute(
      poll_id, source_dataset = "historical", historical_respondent_id = as.character(caseid),
      education_relative = bettered
    )
  stopifnot(all(is.na(definitions$education_relative) | definitions$education_relative %in% c(0, 1)))
  dplyr::left_join(frame, definitions,
    by = c("poll_id", "source_dataset", "historical_respondent_id"),
    relationship = "many-to-one", na_matches = "never"
  )
}

knowledge_gap_frame <- function() {
  frame <- core_group_frame(attendee_panel())
  frame[model_complete_cases(frame, demographic_formula), ]
}

knowledge_gap_spec <- function() {
  tibble::tribble(
    ~comparison, ~variable, ~first, ~second,
    "gender", "female", "0", "1",
    "relative_education", "education_relative", "1", "0",
    "higher_vs_lower", "education", "Higher education", "Lower education",
    "higher_vs_middle", "education", "Higher education", "Middle education"
  )
}

knowledge_gaps <- function(frame = knowledge_gap_frame(), n_boot = bootstrap_replicates(), seed = 20260929L) {
  stopifnot(
    !anyDuplicated(frame[c("poll_id", "respondent_id")]),
    all(is.finite(frame$k1)), all(is.finite(frame$k2)), !anyNA(frame$online), n_boot >= 2L
  )
  if (!"education_relative" %in% names(frame)) frame <- add_relative_education(frame)
  specifications <- knowledge_gap_spec()
  results <- lapply(seq_len(nrow(specifications)), function(i) {
    spec <- specifications[i, ]
    means <- frame |>
      dplyr::mutate(category = as.character(.data[[spec$variable]])) |>
      dplyr::filter(category %in% c(spec$first, spec$second)) |>
      dplyr::summarise(
        pre = mean(k1), post = mean(k2), n = dplyr::n(),
        .by = c(poll_id, online, category)
      )
    by_poll <- dplyr::inner_join(
      dplyr::filter(means, category == spec$first), dplyr::filter(means, category == spec$second),
      by = c("poll_id", "online"), suffix = c("_first", "_second"), relationship = "one-to-one"
    ) |>
      dplyr::transmute(
        comparison = spec$comparison, poll_id, online, n_first, n_second,
        pre = pre_first - pre_second, post = post_first - post_second, change = post - pre
      )
    stopifnot(nrow(by_poll) >= 2L)
    values <- as.matrix(by_poll[c("pre", "post", "change")])
    # Whole-poll resampling preserves paired interviews and sparse education cells.
    set.seed(seed + i)
    draws <- replicate(n_boot, colMeans(values[sample_polls(by_poll$online), , drop = FALSE]))
    summary <- tibble::tibble(
      comparison = spec$comparison, term = colnames(values), estimate = unname(colMeans(values)),
      std_error = unname(apply(draws, 1, stats::sd)),
      lower = unname(apply(draws, 1, stats::quantile, probs = .025)),
      upper = unname(apply(draws, 1, stats::quantile, probs = .975)),
      polls = nrow(by_poll), n_people = sum(by_poll$n_first + by_poll$n_second),
      bootstrap_replicates = n_boot, inference = "poll_bootstrap_stratified_by_mode"
    )
    list(summary = summary, by_poll = by_poll)
  })
  list(
    summary = purrr::map(results, "summary") |> purrr::list_rbind(),
    by_poll = purrr::map(results, "by_poll") |> purrr::list_rbind()
  )
}
