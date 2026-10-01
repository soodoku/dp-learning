test_that("retention follows the same people and battery at all three interviews", {
  people <- tibble::tibble(
    poll_id = "nic-1996", source_dataset = "historical",
    respondent_id = as.character(1:4), attended = c(TRUE, TRUE, TRUE, FALSE),
    small_group_id = "1"
  )
  scores <- tidyr::crossing(respondent_id = as.character(1:4), wave = c("t0", "t2", "t3")) |>
    dplyr::mutate(
      poll_id = "nic-1996", source_dataset = "historical", battery_id = "same",
      scale = "proportion_correct", score = .5, n_items = 11L,
      wave_observed = !(respondent_id == "2" & wave == "t3"),
      n_items = ifelse(respondent_id == "3" & wave == "t3", 8L, n_items),
      wave_role = unname(c(t0 = "pre_arrival", t2 = "post_deliberation", t3 = "follow_up")[wave])
    )
  result <- retention_frame(people, scores)
  expect_equal(result$respondent_id, "1")
  scores$wave_role[scores$wave == "t3"] <- "post_deliberation"
  expect_error(retention_frame(people, scores))
})

test_that("NIC main sample uses observed exit rather than delayed follow-up", {
  panel <- attendee_panel() |> dplyr::filter(poll_id == "nic-1996")
  scores <- read_analysis_phase_scores() |>
    dplyr::filter(poll_id == "nic-1996", source_dataset == "historical", wave == "t2")
  check <- dplyr::left_join(panel, scores,
    by = c("poll_id", "source_dataset", "respondent_id"), relationship = "one-to-one"
  )
  expect_equal(nrow(panel), 456L)
  expect_true(all(check$wave_observed))
  expect_equal(check$k2, check$score)
  timing <- main_interview_timing(core_group_frame(attendee_panel()))
  expect_true(all(timing$phase_t1 == "t0" & timing$phase_t2 == "t2"))
  returning <- retention_frame()
  expect_equal(sum(returning$poll_id == "nic-1996"), 383L)
  expect_equal(sum(returning$poll_id == "a1r-climate-2021"), 845L)
})
