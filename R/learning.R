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
