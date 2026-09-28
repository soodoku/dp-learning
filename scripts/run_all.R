purrr::walk(list.files("R", full.names = TRUE), source)

verify_sources()
dir.create("tabs", showWarnings = FALSE)
write_output <- \(x, name) readr::write_csv(x, file.path("tabs", name), na = "")

control_data <- control_panel()
attendees <- attendee_panel()
core_frame <- core_group_frame(attendees)
core_ids <- unique(core_frame$poll_id)
write_output(main_interview_timing(core_frame), "main_interviews.csv")
assignment_check(core_frame) |> write_output("assignment_check.csv")
core_frame |>
  dplyr::summarise(
    participants = dplyr::n(), age = sum(!is.na(age)),
    education = sum(!is.na(education)), female = sum(!is.na(female)),
    reading = sum(!is.na(read_briefing)), .by = c(poll_id, pollname)
  ) |>
  write_output("covariate_coverage.csv")
responses <- read_analysis_responses()
phases <- phase_contrasts(participants = read_phase_participants())
write_output(phases$contrasts, "phase_contrasts.csv")
write_output(phases$coverage, "phase_coverage.csv")
write_output(phases$selection, "pre_arrival_selection.csv")
main_learning <- learning_estimates(core_frame, responses)
polls <- appendix_polls(group_ids = core_ids)
main_learning$summary |>
  dplyr::arrange(match(poll_id, polls$poll_id)) |>
  write_output("poll_gains.csv")
write_output(pooled_learning(main_learning, "pooled"), "meta.csv")
controlled_learning <- control_learning(control_data, core_frame, responses)
write_output(controlled_learning$summary, "control_learning.csv")
write_output(controlled_learning$pooled, "control_learning_pooled.csv")
write_output(polls, "polls.csv")

reading_frame <- dplyr::filter(core_frame, any(!is.na(read_briefing)), .by = poll_id)
attitude_frame <- add_attitude_measures(core_frame)
dplyr::bind_rows(
  bootstrap_model(core_frame, core_formula, "core"),
  bootstrap_model(core_frame, demographic_formula, "demographic"),
  bootstrap_model(core_frame, core_formula, "core_demographic_sample",
    included = model_complete_cases(core_frame, demographic_formula)
  ),
  bootstrap_model(reading_frame, expanded_briefing_formula, "briefing"),
  bootstrap_model(attitude_frame, attitude_formula, "attitudes"),
  bootstrap_model(attitude_frame, attitude_sd_formula, "attitude_sd"),
  bootstrap_model(attitude_frame, demographic_formula, "demographic_attitude_sample",
    included = model_complete_cases(attitude_frame, attitude_formula)
  ),
  bootstrap_model(add_item_peer_measure(core_frame, responses), items_formula, "items")
) |>
  write_output("models.csv")

effects <- control_effects(control_data)
write_output(effects, "control_effects.csv")
write_output(control_heterogeneity(control_data), "control_heterogeneity.csv")
write_output(selection(control_data), "selection.csv")

peers <- peer_effects(core_frame)
write_output(dplyr::select(peers, -draws), "peer_effects.csv")
write_output(pool_peer_effects(peers), "peer_effects_pooled.csv")

write_results_provenance()
