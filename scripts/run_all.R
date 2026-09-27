purrr::walk(list.files("R", full.names = TRUE), source)

verify_sources()
polardata <- read_polardata()
historical_items <- read_historical_items()
frame <- analysis_frame(
  polardata, item_scores_for_respondents(historical_items, polardata)
)
frame <- dplyr::left_join(
  frame, read_briefing_scores(), by = c("dpnum", "caseid"), relationship = "one-to-one"
)
stopifnot(dplyr::n_distinct(frame$dpnum[!is.na(frame$read_briefing)]) == 9L)

dir.create("tabs", showWarnings = FALSE)
write_output <- \(x, name) readr::write_csv(x, file.path("tabs", name), na = "")

historical_polls <- read_respondent_sources() |>
  dplyr::select(poll_id, dpnum) |>
  dplyr::filter(dpnum %in% polardata$dpnum)
stopifnot(nrow(historical_polls) == 21L)
group_items <- purrr::map2(
  historical_polls$poll_id, historical_polls$dpnum, t1_items_for_poll,
  polardata = polardata, knowledge = historical_items
) |>
  purrr::list_rbind() |>
  item_group_knowledge()
frame <- dplyr::left_join(
  frame,
  dplyr::select(
    group_items, pollid, caseid, group_k1_items = item_group_k1,
    no_missed_items
  ),
  by = c("pollid", "caseid"),
  relationship = "one-to-one"
)

control_data <- control_panel()
attendees <- attendee_panel()
core_frame <- core_group_frame(attendees)
core_ids <- unique(core_frame$poll_id)
dplyr::bind_rows(
  assignment_check(core_frame, c("k1", "female")),
  assignment_check(frame, c("age", "education_ba"))
) |>
  write_output("assignment_check.csv")
gains <- poll_gains(attendees) |>
  dplyr::mutate(group_model = poll_id %in% core_ids)
stopifnot(nrow(gains) == 31L, !anyDuplicated(gains$poll_id))
write_output(gains, "poll_gains.csv")
main_gains <- poll_gains(core_frame)
stopifnot(nrow(main_gains) == 28L, sum(main_gains$respondents) == nrow(core_frame))
write_output(main_gains, "poll_gains_main.csv")
responses <- read_analysis_responses()
guessing <- guessing_adjusted_gains(
  attendees, responses, gains
)
write_output(guessing, "guessing_gains.csv")
# The group-linked panel is a strict subset; reuse fits for polls whose
# respondent set is unchanged and refit polls with missing group assignments.
stopifnot(nrow(dplyr::anti_join(
  core_frame, attendees,
  by = c("poll_id", "source_dataset", "respondent_id")
)) == 0L)
main_guessing <- guessing_adjusted_gains(
  core_frame, responses, main_gains, reuse = guessing
)
stopifnot(all(main_guessing$respondents[
  match(main_gains$poll_id, main_guessing$poll_id)
] == main_gains$respondents))
write_output(main_guessing, "guessing_gains_main.csv")
write_output(appendix_polls(group_ids = core_ids), "polls.csv")

models <- list(
  historical = main_formula,
  minority = minority_formula,
  items = items_formula,
  briefing = briefing_formula
)
dplyr::bind_rows(
  tidy_fit(fit_knowledge(core_frame, core_formula), "core"),
  purrr::imap(models, \(formula, name) {
    tidy_fit(fit_knowledge(frame, formula), name)
  }) |>
    purrr::list_rbind()
) |>
  write_output("models.csv")

list(
  meta_gain(main_gains, "raw", "raw_se") |> tidy_meta("raw, pooled"),
  meta_mode(main_gains, "raw", "raw_se") |> tidy_meta("raw, by mode"),
  meta_gain(main_gains, "raw_sd", "raw_sd_se") |> tidy_meta("raw SD, pooled"),
  meta_mode(main_gains, "raw_sd", "raw_sd_se") |> tidy_meta("raw SD, by mode"),
  meta_gain(main_guessing, "adjusted", "adjusted_se") |>
    tidy_meta("guessing adjusted, pooled"),
  meta_gain(main_guessing, "adjusted_sd", "adjusted_sd_se") |>
    tidy_meta("guessing adjusted SD, pooled"),
  meta_gain(gains, "raw", "raw_se") |> tidy_meta("raw, all"),
  meta_gain(guessing, "adjusted", "adjusted_se") |>
    tidy_meta("guessing adjusted, all")
) |>
  purrr::list_rbind() |>
  write_output("meta.csv")

effects <- control_effects(control_data)
write_output(effects, "control_effects.csv")
write_output(meta_control(effects), "meta_control.csv")
write_output(control_heterogeneity(control_data), "control_heterogeneity.csv")
write_output(selection(control_data), "selection.csv")

peers <- peer_effects(core_frame)
write_output(peers, "peer_effects.csv")
write_output(pool_peer_effects(peers), "peer_effects_pooled.csv")
