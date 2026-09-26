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

write_output(assignment_check(frame), "assignment_check.csv")

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

gains <- poll_gains(frame)
write_output(gains, "poll_gains.csv")
write_output(appendix_polls(), "polls.csv")

models <- list(
  main = main_formula,
  minority = minority_formula,
  items = items_formula,
  briefing = briefing_formula
)
purrr::imap(models, \(formula, name) tidy_fit(fit_knowledge(frame, formula), name)) |>
  purrr::list_rbind() |>
  write_output("models.csv")

list(
  meta_gain(gains, "raw", "raw_se") |> tidy_meta("raw, pooled"),
  meta_mode(gains, "raw", "raw_se") |> tidy_meta("raw, by mode"),
  meta_gain(gains, "raw_sd", "raw_sd_se") |> tidy_meta("raw SD, pooled"),
  meta_mode(gains, "raw_sd", "raw_sd_se") |> tidy_meta("raw SD, by mode")
) |>
  purrr::list_rbind() |>
  write_output("meta.csv")

control_data <- control_panel()
effects <- control_effects(control_data)
write_output(effects, "control_effects.csv")
write_output(meta_control(effects), "meta_control.csv")
write_output(control_heterogeneity(control_data), "control_heterogeneity.csv")
write_output(selection(control_data), "selection.csv")

peers <- peer_effects(frame, a1r_group_frame(control_data))
write_output(peers, "peer_effects.csv")
write_output(pool_peer_effects(peers), "peer_effects_pooled.csv")
