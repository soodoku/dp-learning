purrr::walk(list.files("R", full.names = TRUE), source)

verify_sources()
polardata <- read_polardata()
historical_items <- read_historical_items()
frame <- analysis_frame(
  polardata, item_scores_for_respondents(historical_items, polardata)
)
frame <- dplyr::left_join(
  frame, read_briefing_scores(),
  by = c("dpnum", "caseid"), relationship = "one-to-one"
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
    group_items, pollid, caseid,
    group_k1_items = item_group_k1,
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
responses <- read_analysis_responses()
main_learning <- learning_estimates(core_frame, responses)
# Reuse draws only for polls with identical respondent identities and groups.
unchanged <- vapply(split(attendees, attendees$poll_id), function(x) {
  main <- core_frame[core_frame$poll_id == x$poll_id[1], ]
  identical(sort(x$respondent_id), sort(main$respondent_id))
}, logical(1))
extra_learning <- learning_estimates(dplyr::filter(attendees, poll_id %in% names(unchanged)[!unchanged]), responses)
all_learning <- list(
  summary = dplyr::bind_rows(
    dplyr::filter(main_learning$summary, poll_id %in% names(unchanged)[unchanged]),
    extra_learning$summary
  ),
  fits = c(main_learning$fits[names(unchanged)[unchanged]], extra_learning$fits)
)
gains <- dplyr::mutate(all_learning$summary, group_model = poll_id %in% core_ids)
main_gains <- main_learning$summary
write_output(gains, "poll_gains.csv")
write_output(main_gains, "poll_gains_main.csv")
write_output(gains, "guessing_gains.csv")
write_output(main_gains, "guessing_gains_main.csv")
write_output(dplyr::bind_rows(
  pooled_learning(main_learning, "pooled"),
  pooled_learning(all_learning, "all")
), "meta.csv")
write_output(control_learning(control_data, core_frame, responses), "control_learning.csv")
write_output(appendix_polls(group_ids = core_ids), "polls.csv")

models <- list(
  historical = main_formula,
  minority = minority_formula,
  items = items_formula,
  briefing = briefing_formula
)
dplyr::bind_rows(
  bootstrap_model(core_frame, core_formula, "core"),
  purrr::imap(models, \(formula, name) {
    bootstrap_model(frame, formula, name)
  }) |>
    purrr::list_rbind()
) |>
  write_output("models.csv")

effects <- control_effects(control_data)
write_output(effects, "control_effects.csv")
write_output(control_heterogeneity(control_data), "control_heterogeneity.csv")
write_output(selection(control_data), "selection.csv")

peers <- peer_effects(core_frame)
write_output(dplyr::select(peers, -draws), "peer_effects.csv")
write_output(pool_peer_effects(peers), "peer_effects_pooled.csv")

provenance <- list(
  data_commit = system2("git", c("-C", shQuote(dp_data_root()), "rev-parse", "HEAD"), stdout = TRUE),
  guess_version = as.character(utils::packageVersion("guess")),
  bootstrap_replicates = bootstrap_replicates(),
  sources = upstream_source_manifest()
)
jsonlite::write_json(provenance, "tabs/provenance.json", pretty = TRUE, auto_unbox = TRUE)

writeLines(c(
  "@misc{dpdata,", "  author = {Sood, Gaurav},", "  title = {Deliberative Poll Data},",
  "  year = {2026},", paste0("  note = {Revision ", provenance$data_commit, "},"),
  paste0("  url = {https://github.com/soodoku/dp-data/tree/", provenance$data_commit, "}"), "}"
), "ms/data-version.bib")
