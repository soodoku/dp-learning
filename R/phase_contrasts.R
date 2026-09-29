phase_contrast_spec <- function() {
  tibble::tibble(
    contrast = c("post_minus_arrival", "arrival_minus_pre_arrival", "post_minus_pre_arrival"),
    from = c("t1", "t0", "t0"), to = c("t2", "t1", "t2")
  )
}

phase_score_frame <- function(participants, scores, polls) {
  ids <- c("poll_id", "source_dataset", "respondent_id")
  required <- c(ids, "wave", "score", "scale", "n_items", "wave_observed", "battery_id")
  if (!all(required %in% names(scores))) stop("Phase scores lack required phase or battery metadata.")
  if (!all(c(ids, "study_id", "attended", "arm", "small_group_id") %in% names(participants))) {
    stop("Participants lack explicit attendance or identity metadata.")
  }
  if (anyNA(participants[ids]) || anyDuplicated(participants[ids])) {
    stop("Participant identities must be unique within poll and source dataset.")
  }
  if (!is.logical(participants$attended) || !is.logical(scores$wave_observed)) {
    stop("Attendance and wave presence must be logical, with NA for unknown.")
  }
  scores <- dplyr::filter(scores, wave %in% c("t0", "t1", "t2"), scale == "proportion_correct")
  if ("wave_role" %in% names(scores)) {
    expected_role <- c(t0 = "pre_arrival", t1 = "arrival", t2 = "post_deliberation")
    if (anyNA(scores$wave_role) || any(scores$wave_role != expected_role[scores$wave])) {
      stop("Event-phase labels do not match verified wave roles.")
    }
  }
  score_ids <- c(ids, "battery_id", "wave")
  if (anyNA(scores[score_ids]) || anyDuplicated(scores[score_ids])) {
    stop("Phase score identities must be unique within source, battery, and phase.")
  }
  if (any(!is.na(scores$score) & (!is.finite(scores$score) | scores$score < 0 | scores$score > 1))) {
    stop("Proportion-correct phase scores must be between zero and one.")
  }
  if (nrow(dplyr::anti_join(scores, participants, by = ids)) > 0L) {
    stop("Phase scores contain respondents absent from the participant table.")
  }
  if (!all(c("poll_id", "title") %in% names(polls)) || anyDuplicated(polls$poll_id)) {
    stop("Poll registry must provide one title per poll.")
  }
  batteries <- dplyr::distinct(scores, poll_id, source_dataset, battery_id)
  people <- dplyr::inner_join(participants, batteries, by = c("poll_id", "source_dataset"),
    relationship = "many-to-many"
  )
  wide <- scores |>
    dplyr::select(dplyr::all_of(c(ids, "battery_id", "wave", "score", "n_items", "wave_observed"))) |>
    dplyr::mutate(record = TRUE) |>
    tidyr::pivot_wider(
      names_from = wave, values_from = c(score, n_items, wave_observed, record), names_sep = "_"
    )
  out <- dplyr::left_join(people, wide, by = c(ids, "battery_id"), relationship = "one-to-one") |>
    dplyr::left_join(dplyr::select(polls, poll_id, pollname = title),
      by = "poll_id", relationship = "many-to-one"
    )
  if (anyNA(out$pollname)) stop("Phase scores contain polls absent from the poll registry.")
  for (phase in c("t0", "t1", "t2")) {
    for (field in c("score", "n_items", "wave_observed", "record")) {
      column <- paste(field, phase, sep = "_")
      if (!column %in% names(out)) out[[column]] <- NA
    }
  }
  out$pollid <- out$poll_id
  out$group <- as.character(out$small_group_id)
  out
}

phase_has_score <- function(data, phase) {
  data[[paste0("wave_observed_", phase)]] %in% TRUE &
    is.finite(data[[paste0("score_", phase)]])
}

phase_same_denominator <- function(data, phases) {
  denominator <- data[[paste0("n_items_", phases[1])]]
  valid <- !is.na(denominator) & denominator > 0
  for (phase in phases[-1]) {
    other <- data[[paste0("n_items_", phase)]]
    valid <- valid & !is.na(other) & denominator == other
  }
  valid
}

phase_sample_statistics <- function(data, specifications, seed, n_boot) {
  statistic <- function(x) {
    stats::setNames(vapply(seq_len(nrow(specifications)), function(i) {
      mean(x[[paste0("score_", specifications$to[i])]] - x[[paste0("score_", specifications$from[i])]])
    }, numeric(1)), specifications$contrast)
  }
  n_groups <- dplyr::n_distinct(data$group[!is.na(data$group)])
  inference <- if (nrow(data) == 0L) {
    "no_eligible_pairs"
  } else if (anyNA(data$group)) {
    "descriptive_unknown_groups"
  } else if (n_groups < 2L) {
    "descriptive_fewer_than_two_groups"
  } else {
    "group_bootstrap"
  }
  estimate <- if (nrow(data) > 0L) statistic(data) else rep(NA_real_, nrow(specifications))
  result <- tibble::tibble(
    contrast = specifications$contrast, estimate = unname(estimate),
    std_error = NA_real_, lower = NA_real_, upper = NA_real_, inference = inference,
    n_people = nrow(data), n_known_groups = n_groups, n_unknown_group = sum(is.na(data$group))
  )
  if (inference == "group_bootstrap") {
    fit <- bootstrap_stat(data, statistic, seed = seed, resample_polls = FALSE, n = n_boot)
    result$std_error <- unname(fit$se)
    result$lower <- unname(fit$lower)
    result$upper <- unname(fit$upper)
  }
  result
}

phase_coverage <- function(data) {
  dplyr::bind_rows(lapply(c("t0", "t1", "t2"), function(phase) {
    observed <- data[[paste0("wave_observed_", phase)]]
    tibble::tibble(
      phase = phase, n_people = nrow(data), n_attendees = sum(data$attended %in% TRUE),
      n_nonattendees = sum(data$attended %in% FALSE), n_unknown_attendance = sum(is.na(data$attended)),
      n_missing_phase = sum(!data[[paste0("record_", phase)]] %in% TRUE),
      n_unknown_presence = sum(is.na(observed)), n_absent_wave = sum(observed %in% FALSE),
      n_observed_wave = sum(observed %in% TRUE), n_scored = sum(phase_has_score(data, phase)),
      n_observed_missing_score = sum(observed %in% TRUE & !phase_has_score(data, phase)),
      n_known_groups = dplyr::n_distinct(data$group[!is.na(data$group)]),
      n_unknown_group = sum(is.na(data$group))
    )
  }))
}

phase_selection_summary <- function(data, analysis, category) {
  available <- phase_has_score(data, "t0")
  tibble::tibble(
    analysis = analysis, category = category, n_people = nrow(data), n_scored = sum(available),
    n_missing_phase = sum(!data$record_t0 %in% TRUE), n_unknown_presence = sum(is.na(data$wave_observed_t0)),
    n_absent_wave = sum(data$wave_observed_t0 %in% FALSE), n_unknown_attendance = sum(is.na(data$attended)),
    n_observed_missing_score = sum(data$wave_observed_t0 %in% TRUE & !available),
    mean_t0 = if (any(available)) mean(data$score_t0[available]) else NA_real_,
    estimate = NA_real_, std_error = NA_real_, lower = NA_real_, upper = NA_real_,
    inference = "descriptive_selection_not_causal"
  )
}

phase_selection <- function(data) {
  attendance <- ifelse(is.na(data$attended), "attendance_unknown", ifelse(data$attended, "attended", "not_attended"))
  arm <- ifelse(is.na(data$arm), "arm_unknown", data$arm)
  categories <- paste(attendance, arm, sep = ":")
  summaries <- dplyr::bind_rows(lapply(unique(categories), function(category) {
    phase_selection_summary(data[categories == category, ], "baseline_by_attendance_and_arm", category)
  }))
  attendees <- data[data$attended %in% TRUE, ]
  exit_status <- ifelse(is.na(attendees$wave_observed_t2), "exit_unknown",
    ifelse(attendees$wave_observed_t2, "exit_observed", "exit_absent")
  )
  attrition <- dplyr::bind_rows(lapply(unique(exit_status), function(category) {
    phase_selection_summary(attendees[exit_status == category, ], "attendee_exit_attrition", category)
  }))
  completion_cohort <- any(data$arm %in% "completed")
  selected <- if (completion_cohort) data[data$arm %in% "completed", ] else attendees
  reference_arms <- intersect(
    c("invited_nonattender", "invited_noncompleter", "recruitment_nonattender", "control"), data$arm
  )
  comparisons <- dplyr::bind_rows(lapply(reference_arms, function(arm) {
    reference_rows <- data$arm %in% arm &
      (data$attended %in% FALSE | (completion_cohort & arm == "invited_noncompleter"))
    reference <- data[reference_rows, ]
    category <- paste0(if (completion_cohort) "completed_minus_" else "attended_minus_", arm)
    first <- phase_selection_summary(selected, "baseline_selection_difference", category)
    second <- phase_selection_summary(reference, "baseline_selection_difference", arm)
    first$n_reference <- second$n_people
    first$n_reference_scored <- second$n_scored
    first$reference_mean_t0 <- second$mean_t0
    first$estimate <- first$mean_t0 - second$mean_t0
    first
  }))
  dplyr::bind_rows(summaries, attrition, comparisons)
}

# Phase differences are descriptive changes among explicit attendees. Source
# datasets remain separate because identical respondent IDs do not establish linkage.
phase_contrasts <- function(
  participants = read_phase_participants(), scores = read_analysis_phase_scores(),
  polls = read_poll_registry(), n_boot = bootstrap_replicates(), seed = 20260927L
) {
  frame <- phase_score_frame(participants, scores, polls)
  specifications <- phase_contrast_spec()
  strata <- dplyr::group_split(frame, poll_id, source_dataset, battery_id, .keep = TRUE)
  results <- lapply(seq_along(strata), function(index) {
    people <- strata[[index]]
    metadata <- dplyr::distinct(people, poll_id, study_id, pollname, source_dataset, battery_id)
    attendees <- people[people$attended %in% TRUE, ]
    balanced <- Reduce(`&`, lapply(c("t0", "t1", "t2"), function(phase) phase_has_score(attendees, phase))) &
      phase_same_denominator(attendees, c("t0", "t1", "t2"))
    all_three <- phase_sample_statistics(
      attendees[balanced & !is.na(attendees$group), ], specifications, seed + index * 1000L, n_boot
    ) |>
      dplyr::mutate(
        sample = "all_three_observed_attendees",
        n_excluded_unknown_group = sum(balanced & is.na(attendees$group)),
        n_incompatible_denominator = sum(
          Reduce(`&`, lapply(c("t0", "t1", "t2"), function(phase) phase_has_score(attendees, phase))) &
            !phase_same_denominator(attendees, c("t0", "t1", "t2"))
        )
      )
    pairs <- dplyr::bind_rows(lapply(seq_len(nrow(specifications)), function(i) {
      phases <- c(specifications$from[i], specifications$to[i])
      observed <- phase_has_score(attendees, phases[1]) & phase_has_score(attendees, phases[2])
      compatible <- phase_same_denominator(attendees, phases)
      phase_sample_statistics(attendees[observed & compatible, ], specifications[i, ],
        seed + index * 1000L + i, n_boot
      ) |>
        dplyr::mutate(sample = "available_paired_attendees", n_incompatible_denominator = sum(observed & !compatible))
    }))
    attach <- function(table) dplyr::bind_cols(metadata[rep(1L, nrow(table)), ], table)
    contrasts <- dplyr::bind_rows(pairs, all_three) |>
      dplyr::mutate(
        n_attendees_total = nrow(attendees),
        n_unknown_attendance = sum(is.na(people$attended))
      )
    list(
      contrasts = attach(contrasts),
      coverage = attach(phase_coverage(people)), selection = attach(phase_selection(people))
    )
  })
  list(
    contrasts = dplyr::bind_rows(lapply(results, `[[`, "contrasts")),
    coverage = dplyr::bind_rows(lapply(results, `[[`, "coverage")),
    selection = dplyr::bind_rows(lapply(results, `[[`, "selection"))
  )
}
