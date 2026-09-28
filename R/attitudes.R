pairwise_disagreement <- function(x) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n < 2L) return(NA_real_)
  2 * sum((2 * seq_len(n) - n - 1) * x) / (n * (n - 1))
}

add_attitude_measures <- function(frame, attitudes = read_analysis_attitudes()) {
  keys <- c("poll_id", "source_dataset", "respondent_id")
  long <- attitudes |>
    dplyr::filter(wave == "t1") |>
    dplyr::inner_join(dplyr::select(frame, dplyr::all_of(keys), group),
      by = keys, relationship = "many-to-one"
    )
  individual <- long |>
    dplyr::summarise(
      extremity = mean(abs(value - .5), na.rm = TRUE),
      attitude_items = sum(!is.na(value)), .by = dplyr::all_of(keys)
    )
  group <- long |>
    dplyr::summarise(
      disagreement = pairwise_disagreement(value),
      attitude_sd = stats::sd(value, na.rm = TRUE),
      .by = c(group, attitude_id)
    ) |>
    dplyr::summarise(
      disagreement = mean(disagreement, na.rm = TRUE),
      attitude_sd = mean(attitude_sd, na.rm = TRUE), .by = group
    )
  frame |>
    dplyr::left_join(individual, by = keys, relationship = "one-to-one") |>
    dplyr::left_join(group, by = "group", relationship = "many-to-one") |>
    dplyr::mutate(dplyr::across(c(extremity, disagreement, attitude_sd),
      \(x) replace(x, is.nan(x), NA_real_)
    ))
}
