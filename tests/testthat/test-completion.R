test_that("climate completion labels retain the paired treatment cohort", {
  people <- read_analysis_participants()
  climate <- people$poll_id == "a1r-climate-2021" & people$source_dataset == "control"
  complete <- climate & people$panel & people$assignment == "invited"
  expected_ids <- people$respondent_id[complete]
  expect_length(expected_ids, 962L)
  other <- climate & !people$panel & people$assignment == "invited"
  people$arm[complete] <- "completed"
  people$arm[other] <- "invited_noncompleter"
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
  other <- dplyr::filter(climate, arm == "invited_noncompleter")
  expect_equal(nrow(other), 7018L)
  expect_false(any(other$panel))
  expect_false(anyNA(other$attended))
  expect_equal(sum(other$attended), 184L)
  sessions <- dplyr::filter(other, attendance_basis == "source_session_records")
  expect_equal(sum(sessions$attended), 184L)
  expect_equal(sum(!sessions$attended), 426L)
  inferred <- dplyr::filter(other, attendance_basis == "inferred_absent_post_questionnaire")
  expect_equal(nrow(inferred), 6408L)
  expect_true(all(!inferred$attended))
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

test_that("an explicitly documented nonattendee is excluded from paired attendees", {
  people <- read_analysis_participants()
  original <- attendee_panel(participants = people)
  row <- which(people$poll_id == "btp-national-2003" &
                 people$source_dataset == "historical" &
                 people$historical_respondent_id == "930160")
  expect_length(row, 1L)
  people$attended[row] <- FALSE
  result <- attendee_panel(participants = people)
  expected <- original |>
    dplyr::filter(!(poll_id == "btp-national-2003" &
                      historical_respondent_id == "930160"))
  expect_identical(result, expected)
  expect_false(any(result$poll_id == "btp-national-2003" &
                     result$historical_respondent_id == "930160"))
})
