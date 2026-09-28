retention_frame <- function(
  participants = read_phase_participants(), scores = read_analysis_phase_scores()
) {
  sources <- tibble::tribble(
    ~poll_id, ~source_dataset,
    "nic-1996", "historical",
    "a1r-climate-2021", "control"
  )
  ids <- c("poll_id", "source_dataset", "respondent_id")
  selected <- scores |>
    dplyr::semi_join(sources, by = c("poll_id", "source_dataset")) |>
    dplyr::filter(wave %in% c("t0", "t2", "t3"), scale == "proportion_correct")
  roles <- c(t0 = "pre_arrival", t2 = "post_deliberation", t3 = "follow_up")
  stopifnot(
    !anyDuplicated(selected[c(ids, "wave")]),
    all(selected$wave_role == roles[selected$wave])
  )
  selected |>
    dplyr::filter(wave_observed %in% TRUE, is.finite(score)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(ids))) |>
    dplyr::filter(dplyr::n() == 3L, dplyr::n_distinct(n_items) == 1L,
      dplyr::n_distinct(battery_id) == 1L, all(n_items > 0L)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(ids), wave, score) |>
    tidyr::pivot_wider(names_from = wave, values_from = score, names_prefix = "score_") |>
    dplyr::inner_join(
      dplyr::filter(participants, attended %in% TRUE),
      by = ids, relationship = "one-to-one"
    ) |>
    dplyr::mutate(pollid = poll_id, group = as.character(small_group_id))
}

retention_estimates <- function(frame = retention_frame(), n_boot = bootstrap_replicates()) {
  comparisons <- tibble::tribble(
    ~contrast, ~from, ~to,
    "exit_minus_pre_arrival", "t0", "t2",
    "followup_minus_exit", "t2", "t3",
    "followup_minus_pre_arrival", "t0", "t3"
  )
  frame |>
    dplyr::group_split(poll_id) |>
    purrr::map(function(data) {
      phase_sample_statistics(data, comparisons, seed = 20260928L, n_boot = n_boot) |>
        dplyr::mutate(
          poll_id = dplyr::first(data$poll_id),
          mean_pre_arrival = mean(data$score_t0), mean_exit = mean(data$score_t2),
          mean_followup = mean(data$score_t3), .before = 1
        )
    }) |>
    purrr::list_rbind()
}
