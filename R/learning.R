# One paired attendee record per person, selected from the canonical dp-data
# participant and score tables. Overlapping historical and Cor--Sood source
# rows represent the same poll, so the historical linkage takes priority.
attendee_panel <- function(
  participants = read_analysis_participants(),
  scores = read_analysis_scores(),
  polls = read_poll_registry()
) {
  paired <- scores |>
    dplyr::filter(wave %in% c("t1", "t2"), scale == "proportion_correct") |>
    dplyr::select("poll_id", "source_dataset", "respondent_id", "wave", "score") |>
    tidyr::pivot_wider(names_from = wave, values_from = score) |>
    dplyr::filter(!is.na(t1), !is.na(t2))
  available <- participants |>
    dplyr::inner_join(
      paired,
      by = c("poll_id", "source_dataset", "respondent_id"),
      relationship = "one-to-one"
    )
  historical_ids <- unique(available$poll_id[
    available$source_dataset == "historical" & available$panel
  ])
  cor_ids <- unique(available$poll_id[
    available$source_dataset == "cor_sood" &
      !available$poll_id %in% historical_ids
  ])
  out <- available |>
    dplyr::filter(
      (source_dataset == "historical" & panel) |
        (source_dataset == "cor_sood" & poll_id %in% cor_ids) |
        (source_dataset == "control" & arm == "attended" &
           !poll_id %in% c(historical_ids, cor_ids))
    ) |>
    dplyr::left_join(
      dplyr::select(polls, "poll_id", pollname = "title", "mode"),
      by = "poll_id", relationship = "many-to-one"
    ) |>
    dplyr::transmute(
      poll_id, pollname, source_dataset, respondent_id, historical_respondent_id,
      k1 = t1, k2 = t2,
      online = as.integer(mode == "online"),
      female, ba, age, education, read_briefing,
      group = dplyr::if_else(
        is.na(small_group_id), NA_character_,
        paste(poll_id, small_group_id, sep = "_")
      )
    )
  stopifnot(
    dplyr::n_distinct(out$poll_id) == 31L,
    !anyDuplicated(out[c("poll_id", "respondent_id")]),
    !anyNA(out$pollname), !anyNA(out$online),
    all(out$k1 >= 0 & out$k1 <= 1),
    all(out$k2 >= 0 & out$k2 <= 1)
  )
  out
}
