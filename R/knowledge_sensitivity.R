knowledge_sensitivity_panel <- function(
  panel = attendee_panel(), exclude = c("zero_score", "all_blank"), flags = read_knowledge_flags()
) {
  exclude <- match.arg(exclude)
  keys <- c("poll_id", "source_dataset", "respondent_id")
  selected <- flags |>
    dplyr::filter(
      battery_id == paste(poll_id, source_dataset, "knowledge", sep = ":"),
      wave %in% c("t0", "t2")
    ) |>
    dplyr::inner_join(dplyr::select(panel, dplyr::all_of(keys), k1, k2),
      by = keys, relationship = "many-to-one"
    )
  stopifnot(
    !anyDuplicated(selected[c(keys, "wave")]), nrow(selected) == 2L * nrow(panel),
    all(selected$wave_observed %in% TRUE),
    all(abs(selected$score - ifelse(selected$wave == "t0", selected$k1, selected$k2)) < 1e-7)
  )
  excluded <- selected |>
    dplyr::filter(.data[[exclude]] %in% TRUE) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(keys)))
  dplyr::anti_join(panel, excluded, by = keys)
}


knowledge_sensitivity_models <- function(panel, models) {
  main <- models |>
    dplyr::filter(model == "core") |>
    dplyr::mutate(sample = "main", paired_people = nrow(panel))
  alternatives <- purrr::map(c("zero_score", "all_blank"), function(exclude) {
    selected <- knowledge_sensitivity_panel(panel, exclude)
    frame <- core_group_frame(selected)
    bootstrap_model(frame, core_formula, paste0("without_", exclude)) |>
      dplyr::mutate(sample = exclude, paired_people = nrow(selected))
  }) |>
    purrr::list_rbind()
  dplyr::bind_rows(main, alternatives) |>
    dplyr::mutate(bootstrap_replicates = bootstrap_replicates())
}
