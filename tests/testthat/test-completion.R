test_that("climate completion labels retain the paired treatment cohort", {
  people <- read_analysis_participants()
  climate <- people$poll_id == "a1r-climate-2021" & people$source_dataset == "control"
  complete <- climate & people$panel & people$assignment == "invited"
  expected_ids <- people$respondent_id[complete]
  expect_length(expected_ids, 962L)
  other <- climate & !people$panel & people$assignment == "invited"
  people$arm[complete] <- "completed"
  people$arm[other] <- "invited_noncompleter"
  people$attended[other] <- NA
  data <- control_panel(participants = people)
  study <- dplyr::filter(data, poll_id == "a1r-climate-2021")
  expect_equal(sum(study$arm == "completed"), 962L)
  expect_equal(sum(study$arm == "invited_noncompleter"), 7018L)
  expect_equal(sum(study$treated == 1), 962L)
  expect_equal(sum(study$treated == 0 & !is.na(study$k2)), 671L)
  selected <- selection(data)
  study <- dplyr::filter(selected, study == "America in One Room: Climate 2021")
  expect_setequal(study$group, c("completed", "other invitees"))
  expect_equal(study$n[study$group == "completed"], 962L)
  expect_equal(study$n[study$group == "other invitees"], 7018L)
  expect_equal(study$k1[study$group == "completed"], .6855509356, tolerance = 1e-9)
  panel <- attendee_panel(participants = people)
  climate_panel <- dplyr::filter(panel, poll_id == "a1r-climate-2021")
  expect_equal(nrow(climate_panel), 962L)
  expect_setequal(climate_panel$respondent_id, expected_ids)
  expect_equal(dplyr::n_distinct(panel$poll_id), 30L)
  expect_false(any(people$respondent_id[other] %in% climate_panel$respondent_id))
})


test_that("the current upstream completion cohort reaches the main panel unchanged", {
  people <- read_analysis_participants()
  climate <- dplyr::filter(
    people, poll_id == "a1r-climate-2021", source_dataset == "control"
  )
  completed <- dplyr::filter(climate, arm == "completed")
  expect_equal(nrow(completed), 962L)
  expect_true(all(completed$attended & completed$panel))
  expect_true(all(completed$assignment == "invited"))
  expect_true(all(is.na(climate$attended[climate$arm == "invited_noncompleter"])))
  panel <- attendee_panel()
  actual <- dplyr::filter(panel, poll_id == "a1r-climate-2021")
  expect_setequal(actual$respondent_id, completed$respondent_id)
  expect_equal(dplyr::n_distinct(panel$poll_id), 30L)

  scores <- read_analysis_scores() |>
    dplyr::filter(
      poll_id == "a1r-climate-2021", source_dataset == "control",
      respondent_id %in% completed$respondent_id,
      wave %in% c("t1", "t2"), scale == "proportion_correct"
    ) |>
    dplyr::select(respondent_id, wave, score) |>
    tidyr::pivot_wider(names_from = wave, values_from = score)
  expected <- scores[match(actual$respondent_id, scores$respondent_id), ]
  expect_equal(actual$k1, expected$t1)
  expect_equal(actual$k2, expected$t2)
})
