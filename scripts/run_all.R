purrr::walk(list.files("R", full.names = TRUE), source)

verify_sources()
frame <- analysis_frame(dplyr::bind_rows(read_polardata(), read_greece()))
cor_dir <- extract_cor_data()

dir.create("tabs", showWarnings = FALSE)
write_output <- \(x, name) readr::write_csv(x, file.path("tabs", name), na = "")

write_output(assignment_check(frame), "assignment_check.csv")

polardata <- read_polardata()
group_items <- purrr::map2(
  t1_linked_polls$file_key, t1_linked_polls$dpnum, t1_items_for_poll,
  polardata = polardata, data_dir = cor_dir
) |>
  purrr::list_rbind() |>
  item_group_knowledge()
frame <- dplyr::left_join(
  frame,
  dplyr::select(group_items, pollid, caseid, group_k1_items = item_group_k1),
  by = c("pollid", "caseid"),
  relationship = "one-to-one"
)

gains <- poll_gains(frame)
write_output(gains, "poll_gains.csv")

cor_learning <- poll_map$file_key |>
  purrr::map(poll_learning, data_dir = cor_dir) |>
  purrr::list_rbind() |>
  dplyr::left_join(
    purrr::list_rbind(purrr::map(poll_map$file_key, irt_learning, data_dir = cor_dir)),
    by = "file_key",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(dplyr::select(poll_map, file_key, cor_poll_name), by = "file_key")
write_output(cor_learning, "item_learning.csv")

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

control_paths <- fetch_control_files()
effects <- control_effects(control_paths)
write_output(effects, "control_effects.csv")
write_output(meta_causal(effects), "meta_causal.csv")
write_output(control_heterogeneity(control_paths), "control_heterogeneity.csv")
write_output(selection(control_paths), "selection.csv")

peers <- peer_effects(frame, a1r_group_frame(control_paths[["a1r"]]))
write_output(peers, "peer_effects.csv")
write_output(pool_peer_effects(peers), "peer_effects_pooled.csv")
