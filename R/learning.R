# Participant-level mean gain (T2 - T1 proportion correct) by poll.
poll_gains <- function(frame) {
  se <- \(x) stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  frame |>
    dplyr::mutate(raw_gain = k2 - k1) |>
    dplyr::summarise(
      respondents = dplyr::n(),
      online = dplyr::first(online),
      k1_mean = mean(k1, na.rm = TRUE),
      k2_mean = mean(k2, na.rm = TRUE),
      raw = mean(raw_gain, na.rm = TRUE),
      raw_se = se(raw_gain),
      k1_sd = stats::sd(k1, na.rm = TRUE),
      .by = c(dpnum, pollname)
    ) |>
    dplyr::mutate(raw_sd = raw / k1_sd, raw_sd_se = raw_se / k1_sd) |>
    dplyr::arrange(dplyr::desc(raw))
}

# Polls represented by source-survey item batteries but absent from the
# historical respondent panel still contribute to the pre/post comparison.
additional_poll_gains <- function(scores, polls, historical_ids) {
  se <- \(x) stats::sd(x) / sqrt(length(x))
  scores |>
    dplyr::filter(
      source_dataset == "cor_sood", !poll_id %in% historical_ids,
      wave %in% c("t1", "t2")
    ) |>
    dplyr::select(poll_id, respondent_id, wave, score) |>
    tidyr::pivot_wider(names_from = wave, values_from = score) |>
    dplyr::filter(!is.na(t1), !is.na(t2)) |>
    dplyr::left_join(
      dplyr::select(polls, poll_id, pollname = title, mode),
      by = "poll_id", relationship = "many-to-one"
    ) |>
    dplyr::mutate(raw_gain = t2 - t1, online = as.integer(mode == "online")) |>
    dplyr::summarise(
      respondents = dplyr::n(), online = dplyr::first(online),
      k1_mean = mean(t1), k2_mean = mean(t2), raw = mean(raw_gain),
      raw_se = se(raw_gain), k1_sd = stats::sd(t1),
      .by = c(poll_id, pollname)
    ) |>
    dplyr::mutate(
      dpnum = NA_real_, raw_sd = raw / k1_sd,
      raw_sd_se = raw_se / k1_sd
    )
}

control_poll_gains <- function(data, polls) {
  se <- \(x) stats::sd(x) / sqrt(length(x))
  data |>
    dplyr::filter(treated == 1, !is.na(k1), !is.na(k2)) |>
    dplyr::left_join(
      dplyr::select(polls, poll_id, pollname = title, mode),
      by = "poll_id", relationship = "many-to-one"
    ) |>
    dplyr::mutate(raw_gain = k2 - k1, online = as.integer(mode == "online")) |>
    dplyr::summarise(
      respondents = dplyr::n(), online = dplyr::first(online),
      k1_mean = mean(k1), k2_mean = mean(k2), raw = mean(raw_gain),
      raw_se = se(raw_gain), k1_sd = stats::sd(k1),
      .by = c(poll_id, pollname)
    ) |>
    dplyr::mutate(
      dpnum = NA_real_, raw_sd = raw / k1_sd,
      raw_sd_se = raw_se / k1_sd
    )
}
