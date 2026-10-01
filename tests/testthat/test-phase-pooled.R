phase_pooled_fixture <- function() {
  participants <- tibble::tibble(
    poll_id = c(rep("michigan-2009", 2L), rep("other", 6L)),
    source_dataset = "cor_sood", respondent_id = letters[1:8],
    study_id = poll_id, attended = TRUE, arm = "participant", small_group_id = letters[1:8]
  )
  scores <- tidyr::expand_grid(respondent_id = letters[1:8], wave = c("t0", "t1", "t2")) |>
    dplyr::left_join(dplyr::select(participants, respondent_id, poll_id, source_dataset),
      by = "respondent_id", relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      battery_id = paste(poll_id, source_dataset, "knowledge", sep = ":"),
      scale = "proportion_correct", n_items = 9L, wave_observed = TRUE,
      score = dplyr::case_when(
        wave == "t0" ~ .1,
        poll_id == "michigan-2009" & wave == "t1" ~ .2,
        poll_id == "michigan-2009" & wave == "t2" ~ .3,
        wave == "t1" ~ .6, TRUE ~ .9
      )
    )
  list(
    participants = participants, scores = scores,
    polls = tibble::tibble(poll_id = c("michigan-2009", "other"), title = c("Michigan", "Other"))
  )
}

test_that("phase means give each poll equal weight and preserve component additivity", {
  fixture <- phase_pooled_fixture()
  result <- do.call(phase_pooled, c(fixture, list(n_boot = 39L, seed = 101L)))
  estimate <- stats::setNames(result$estimate, result$contrast)
  expect_equal(unname(estimate[c("arrival_minus_pre_arrival", "post_minus_arrival", "post_minus_pre_arrival")]),
    c(.3, .2, .5), tolerance = 1e-12
  )
  expect_equal(estimate[["post_minus_pre_arrival"]],
    estimate[["arrival_minus_pre_arrival"]] + estimate[["post_minus_arrival"]]
  )
  expect_equal(result$n_polls, rep(2L, 3L))
  expect_equal(result$n_people, rep(8L, 3L))
  expect_equal(result$bootstrap_replicates, rep(39L, 3L))
  expect_true(all(result$inference == "poll_and_group_bootstrap"))
  expect_true(all(result$lower <= result$estimate & result$upper >= result$estimate))
  expect_equal(do.call(phase_pooled, c(fixture, list(n_boot = 39L, seed = 101L))), result)
})

test_that("Michigan's placement sensitivity cannot enter the primary mean twice", {
  fixture <- phase_pooled_fixture()
  baseline <- do.call(phase_pooled, c(fixture, list(n_boot = 19L)))
  sensitivity <- fixture$scores |>
    dplyr::filter(poll_id == "michigan-2009") |>
    dplyr::mutate(
      battery_id = paste(poll_id, source_dataset, "knowledge_placements_four", sep = ":"),
      n_items = 4L, score = dplyr::if_else(wave == "t0", 0, 1)
    )
  fixture$scores <- dplyr::bind_rows(fixture$scores, sensitivity)
  expect_identical(do.call(phase_pooled, c(fixture, list(n_boot = 19L))), baseline)
  sample <- do.call(phase_primary_sample, fixture)
  expect_equal(nrow(sample), 8L)
  expect_false(any(grepl("placements_four", sample$battery_id)))
})

test_that("all pooled components use the same complete three-wave people and denominator", {
  fixture <- phase_pooled_fixture()
  fixture$scores$wave_observed[fixture$scores$respondent_id == "c" & fixture$scores$wave == "t0"] <- FALSE
  fixture$scores$n_items[fixture$scores$respondent_id == "d" & fixture$scores$wave == "t2"] <- 8L
  fixture$participants$small_group_id[fixture$participants$respondent_id == "e"] <- NA_character_
  sample <- do.call(phase_primary_sample, fixture)
  expect_setequal(sample$respondent_id, c("a", "b", "f", "g", "h"))
  expect_true(all(vapply(c("t0", "t1", "t2"), function(phase) all(phase_has_score(sample, phase)), logical(1))))
  expect_true(all(phase_same_denominator(sample, c("t0", "t1", "t2"))))
  result <- do.call(phase_pooled, c(fixture, list(n_boot = 19L)))
  expect_equal(result$n_people, rep(5L, 3L))
})

test_that("the primary phase exhibit retains one canonical knowledge battery per poll", {
  source <- read_output("phase_contrasts.csv")
  exhibit <- phase_primary_exhibit(source)
  expect_equal(nrow(exhibit), 15L)
  expect_equal(dplyr::n_distinct(exhibit$poll_id), 5L)
  expect_true(all(dplyr::count(exhibit, poll_id)$n == 3L))
  michigan <- dplyr::filter(exhibit, poll_id == "michigan-2009")
  expect_true(all(michigan$battery_id == "michigan-2009:cor_sood:knowledge"))
  summary <- read_output("phase_summary.csv")
  expected <- dplyr::summarise(exhibit, estimate = mean(estimate), .by = contrast)
  joined <- dplyr::left_join(summary, expected, by = "contrast", suffix = c("", "_expected"))
  expect_equal(joined$estimate, joined$estimate_expected, tolerance = 1e-12)
  expect_equal(summary$n_people, rep(1504L, 3L))
  expect_equal(summary$n_known_groups, rep(99L, 3L))
})
