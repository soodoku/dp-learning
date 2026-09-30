# One paired attendee record per person, selected from the canonical dp-data
# participant and score tables. Overlapping historical and Cor--Sood source
# rows represent the same poll, so the historical linkage takes priority except
# for the reviewed Primaries cohort, which includes every verified attendee.
attendee_panel <- function(
  participants = read_analysis_participants(),
  scores = read_analysis_scores(),
  polls = read_poll_registry(),
  studies = read_analysis_studies(),
  phase_participants = read_phase_participants(),
  phase_scores = read_analysis_phase_scores()
) {
  paired <- scores |>
    dplyr::filter(wave %in% c("t1", "t2"), scale == "proportion_correct") |>
    dplyr::select("poll_id", "source_dataset", "respondent_id", "wave", "score") |>
    tidyr::pivot_wider(names_from = wave, values_from = score) |>
    dplyr::filter(!is.na(t1), !is.na(t2))
  available <- participants |>
    dplyr::filter(attended %in% TRUE) |>
    dplyr::inner_join(
      paired,
      by = c("poll_id", "source_dataset", "respondent_id"),
      relationship = "one-to-one"
    )
  primaries <- "btp-online-primaries-2004"
  primaries_study <- studies$study_id[studies$poll_id == primaries]
  aliases <- studies$poll_id[studies$study_id == primaries_study & studies$poll_id != primaries]
  eligible <- phase_scores |>
    dplyr::filter(
      poll_id == primaries, source_dataset == "cor_sood",
      wave %in% c("t0", "t2"), wave_observed %in% TRUE, is.finite(score)
    ) |>
    dplyr::summarise(waves = dplyr::n_distinct(wave),
      .by = c(poll_id, source_dataset, respondent_id)
    ) |>
    dplyr::filter(waves == 2L) |>
    dplyr::inner_join(
      dplyr::filter(phase_participants, attended %in% TRUE),
      by = c("poll_id", "source_dataset", "respondent_id"), relationship = "one-to-one"
    )
  stopifnot(length(aliases) == 1L, nrow(eligible) == 239L)
  nic_eligible <- phase_scores |>
    dplyr::filter(
      poll_id == "nic-1996", source_dataset == "historical",
      wave %in% c("t0", "t2"), wave_observed %in% TRUE, is.finite(score)
    ) |>
    dplyr::summarise(waves = dplyr::n_distinct(wave), .by = respondent_id) |>
    dplyr::filter(waves == 2L)
  available <- available |>
    dplyr::filter(
      poll_id != "nic-1996" | (
        source_dataset == "historical" & respondent_id %in% nic_eligible$respondent_id
      ),
      !poll_id %in% aliases,
      poll_id != primaries | (source_dataset == "cor_sood" & respondent_id %in% eligible$respondent_id)
    ) |>
    dplyr::left_join(studies, by = "poll_id", relationship = "many-to-one")
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
        (source_dataset == "control" & arm %in% c("attended", "completed") &
           !poll_id %in% c(historical_ids, cor_ids))
    ) |>
    dplyr::left_join(
      dplyr::select(polls, "poll_id", pollname = "title", "mode"),
      by = "poll_id", relationship = "many-to-one"
    ) |>
    dplyr::transmute(
      poll_id, study_id, pollname, source_dataset, respondent_id, historical_respondent_id,
      k1 = t1, k2 = t2,
      online = as.integer(mode == "online"),
      female, ba, age, education, read_briefing,
      group = dplyr::if_else(
        is.na(small_group_id), NA_character_,
        paste(poll_id, small_group_id, sep = "_")
      )
    )
  stopifnot(
    dplyr::n_distinct(out$poll_id) == 30L,
    dplyr::n_distinct(out$poll_id) == dplyr::n_distinct(out$study_id),
    !anyNA(out$study_id),
    !anyDuplicated(out[c("poll_id", "respondent_id")]),
    !anyNA(out$pollname), !anyNA(out$online),
    all(out$k1 >= 0 & out$k1 <= 1),
    all(out$k2 >= 0 & out$k2 <= 1)
  )
  out
}

main_interview_timing <- function(panel, phase_scores = read_analysis_phase_scores()) {
  frames <- dplyr::distinct(panel, poll_id, source_dataset)
  timing <- phase_scores |>
    dplyr::semi_join(frames, by = c("poll_id", "source_dataset")) |>
    dplyr::filter(original_score_wave %in% c("t1", "t2")) |>
    dplyr::distinct(poll_id, source_dataset, original_score_wave, wave) |>
    tidyr::pivot_wider(names_from = original_score_wave, values_from = wave, names_prefix = "phase_")
  out <- dplyr::left_join(frames, timing, by = c("poll_id", "source_dataset"), relationship = "one-to-one")
  stopifnot(!anyNA(out), all(out$phase_t1 == "t0"), all(out$phase_t2 == "t2"))
  out
}
