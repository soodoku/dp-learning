# The reset correction treats every item answered right at T1 and wrong at T2
# as a lucky T1 guess. Control groups receive no treatment, so whatever
# "learning" the correction finds among them is an artefact: response noise
# on repeated items and forgetting, not knowledge gained.
# Right, wrong, or don't know (NA), as the latent class model needs; skipped
# and refused are treated as don't know.
dk_items <- function(data, items, key, dk = NULL) {
  purrr::map2(items, key, \(item, answer) {
    x <- data[[item]]
    dplyr::case_when(is.na(x) | x %in% dk ~ NA_integer_, x == answer ~ 1L, .default = 0L)
  }) |>
    purrr::set_names(paste0("i", seq_along(items))) |>
    tibble::as_tibble()
}

# Standard errors: raw and reset gains from person-level gains; latent
# class learning by resampling respondents.
transitions <- function(t1, t2, study, group, n_boot = 200) {
  lca_mean <- \(rows) mean(fit_item_model(t1[rows, ], t2[rows, ])$learning)
  boot <- withr::with_seed(1, replicate(n_boot, lca_mean(sample(nrow(t1), replace = TRUE))))
  lca <- lca_mean(seq_len(nrow(t1)))
  t1 <- as.matrix(dplyr::mutate(t1, dplyr::across(dplyr::everything(), \(x) dplyr::coalesce(x, 0L))))
  t2 <- as.matrix(dplyr::mutate(t2, dplyr::across(dplyr::everything(), \(x) dplyr::coalesce(x, 0L))))
  se <- \(x) stats::sd(x) / sqrt(length(x))
  tibble::tibble(
    study = study,
    group = group,
    people = nrow(t1),
    raw_gain = mean(t2) - mean(t1),
    raw_gain_se = se(rowMeans(t2) - rowMeans(t1)),
    reset_gain = mean(t2) - mean(t1 * t2),
    reset_gain_se = se(rowMeans(t2) - rowMeans(t1 * t2)),
    lca_gain = lca,
    lca_gain_se = stats::sd(boot),
    right_to_wrong = sum(t1 == 1 & t2 == 0) / sum(t1 == 1),
    wrong_to_right = sum(t1 == 0 & t2 == 1) / sum(t1 == 0)
  )
}

correction_check <- function(paths) {
  a1r <- readr::read_tsv(paths[["a1r"]], show_col_types = FALSE) |> dplyr::filter(POST == 1)
  climate <- readr::read_tsv(paths[["climate"]], show_col_types = FALSE)
  amr <- readr::read_csv(paths[["amr"]], show_col_types = FALSE)
  amr_wide <- \(group) {
    rows <- dplyr::filter(amr, Group == group) |> dplyr::arrange(ID, Time)
    list(
      t1 = dk_items(dplyr::filter(rows, Time == 0), paste0("knowledge_", 1:6), amr_key),
      t2 = dk_items(dplyr::filter(rows, Time == 1), paste0("knowledge_", 1:6), amr_key)
    )
  }
  climate_rows <- list(
    attendees = dplyr::filter(climate, P_DELEGATE == 1),
    controls = dplyr::filter(climate, P_TREATMENT == 0, P_DELEGATE == 0)
  )
  climate_items <- \(rows, prefix) dk_items(rows, paste0(prefix, 17:24), climate_key, dk = c(77, 98, 99))
  climate_t3 <- dplyr::filter(climate_rows$attendees, !is.na(T3Q17))
  list(
    transitions(
      dk_items(dplyr::filter(a1r, CONDITION == 1), paste0("PK", 1:7), a1r_key, dk = c(77, 98, 99)),
      dk_items(dplyr::filter(a1r, CONDITION == 1), paste0("T2PK", 1:7), a1r_key, dk = c(77, 98, 99)),
      "America in One Room 2019", "Attendees, T1 to T2"
    ),
    transitions(
      dk_items(dplyr::filter(a1r, CONDITION == 0), paste0("PK", 1:7), a1r_key, dk = c(77, 98, 99)),
      dk_items(dplyr::filter(a1r, CONDITION == 0), paste0("T2PK", 1:7), a1r_key, dk = c(77, 98, 99)),
      "America in One Room 2019", "Controls, T1 to T2"
    ),
    transitions(
      climate_items(climate_rows$attendees, "Q"), climate_items(climate_rows$attendees, "T2Q"),
      "America in One Room: Climate 2021", "Attendees, T1 to T2"
    ),
    transitions(
      climate_items(climate_rows$controls, "Q"), climate_items(climate_rows$controls, "T2Q"),
      "America in One Room: Climate 2021", "Controls, T1 to T2"
    ),
    transitions(
      climate_items(climate_t3, "T2Q"), climate_items(climate_t3, "T3Q"),
      "America in One Room: Climate 2021", "Attendees, T2 to one year later"
    ),
    do.call(transitions, c(amr_wide(1), study = "Antimicrobial Resistance 2024", group = "Attendees, T1 to T2")),
    do.call(transitions, c(amr_wide(0), study = "Antimicrobial Resistance 2024", group = "Controls, T1 to T2"))
  ) |>
    purrr::list_rbind()
}
