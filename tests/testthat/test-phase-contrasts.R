phase_fixture <- function() {
  participants <- tibble::tibble(
    poll_id = "example", study_id = "example", source_dataset = "historical", respondent_id = letters[1:7],
    attended = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, NA),
    arm = c(rep("participant", 4), "invited_nonattender", "control", "surveyed"),
    small_group_id = c("1", "2", "3", "4", NA, NA, "5")
  )
  scores <- tidyr::expand_grid(respondent_id = letters[1:7], wave = c("t0", "t1", "t2")) |>
    dplyr::mutate(
      poll_id = "example", source_dataset = "historical", battery_id = "facts",
      scale = "proportion_correct", n_items = 5L, wave_observed = TRUE,
      score = c(.2, .4, .8, .4, .6, 0, .6, .8, 0, .2, .2, .4, 0, NA, NA, .2, NA, NA, .8, .8, .8)
    )
  scores$wave_observed[scores$respondent_id == "c" & scores$wave == "t2"] <- FALSE
  scores$wave_observed[scores$respondent_id == "d" & scores$wave == "t0"] <- NA
  scores$wave_observed[scores$respondent_id %in% c("e", "f") & scores$wave != "t0"] <- FALSE
  list(participants = participants, scores = scores, polls = tibble::tibble(poll_id = "example", title = "Example"))
}

run_phase_fixture <- function(fixture = phase_fixture()) {
  phase_contrasts(fixture$participants, fixture$scores, fixture$polls, n_boot = 19L)
}

test_that("balanced phase changes decompose on the same people and bootstrap draws", {
  output <- run_phase_fixture()
  balanced <- dplyr::filter(output$contrasts, sample == "all_three_observed_attendees")
  expect_equal(balanced$n_people, rep(2L, 3))
  expect_equal(balanced$estimate, c(-.1, .2, .1), tolerance = 1e-12)
  expect_equal(balanced$estimate[3], balanced$estimate[1] + balanced$estimate[2], tolerance = 1e-12)
  expect_true(all(balanced$inference == "group_bootstrap"))
  expect_equal(balanced, dplyr::filter(run_phase_fixture()$contrasts, sample == "all_three_observed_attendees"))
  data <- phase_score_frame(phase_fixture()$participants, phase_fixture()$scores, phase_fixture()$polls)[1:2, ]
  fit <- bootstrap_stat(data, function(x) {
    c(
      post_arrival = mean(x$score_t2 - x$score_t1), arrival_pre = mean(x$score_t1 - x$score_t0),
      post_pre = mean(x$score_t2 - x$score_t0)
    )
  }, n = 19L, resample_polls = FALSE)
  expect_equal(fit$draws[, 3], fit$draws[, 1] + fit$draws[, 2], tolerance = 1e-12)
  paired <- dplyr::filter(output$contrasts, sample == "available_paired_attendees")
  expect_equal(paired$n_people, c(3L, 3L, 2L))
  expect_equal(paired$estimate, c(0, .2, .1), tolerance = 1e-12)
})

test_that("observed zero differs from absent wave and unknown presence", {
  output <- run_phase_fixture()
  pre <- dplyr::filter(output$coverage, phase == "t0")
  post <- dplyr::filter(output$coverage, phase == "t2")
  expect_equal(pre$n_unknown_presence, 1L)
  expect_equal(post$n_absent_wave, 3L)
  expect_equal(post$n_scored, 4L)
  expect_equal(post$n_unknown_attendance, 1L)
  expect_equal(post$n_people, 7L)
  fixture <- phase_fixture()
  fixture$scores <- dplyr::filter(fixture$scores, !(respondent_id == "c" & wave == "t2"))
  changed <- run_phase_fixture(fixture)
  post <- dplyr::filter(changed$coverage, phase == "t2")
  expect_equal(post$n_missing_phase, 1L)
  expect_equal(post$n_unknown_presence, 1L)
  expect_equal(post$n_absent_wave, 2L)
  expect_equal(changed$contrasts$estimate, output$contrasts$estimate)
})

test_that("selection and attrition preserve arms and use explicit presence", {
  output <- run_phase_fixture()
  selected <- dplyr::filter(output$selection, analysis == "baseline_selection_difference")
  expect_equal(selected$mean_t0, c(.4, .4))
  expect_equal(selected$reference_mean_t0, c(0, .2))
  expect_equal(selected$estimate, c(.4, .2))
  expect_equal(selected$n_people, c(4L, 4L))
  expect_equal(selected$n_scored, c(3L, 3L))
  expect_equal(selected$n_unknown_presence, c(1L, 1L))
  attrition <- dplyr::filter(output$selection, analysis == "attendee_exit_attrition")
  expect_equal(attrition$mean_t0[attrition$category == "exit_absent"], .6)
  expect_equal(attrition$mean_t0[attrition$category == "exit_observed"], .3)
  expect_true(any(output$selection$category == "attendance_unknown:surveyed"))
  expect_true(all(is.na(output$selection$std_error)))
})

test_that("phase comparisons require the same battery and denominator", {
  fixture <- phase_fixture()
  fixture$scores$battery_id[fixture$scores$respondent_id == "a" & fixture$scores$wave == "t2"] <- "other"
  output <- run_phase_fixture(fixture)
  balanced <- dplyr::filter(output$contrasts, sample == "all_three_observed_attendees", battery_id == "facts")
  expect_equal(balanced$n_people, rep(1L, 3))
  expect_equal(balanced$estimate, c(-.6, .2, -.4), tolerance = 1e-12)
  fixture <- phase_fixture()
  fixture$scores$n_items[fixture$scores$respondent_id == "a" & fixture$scores$wave == "t2"] <- 10L
  output <- run_phase_fixture(fixture)
  pair <- dplyr::filter(output$contrasts, sample == "available_paired_attendees", contrast == "post_minus_arrival")
  expect_equal(pair$n_people, 2L)
  expect_equal(pair$n_incompatible_denominator, 1L)
})

test_that("unknown groups retain estimates without manufactured precision", {
  fixture <- phase_fixture()
  fixture$participants$small_group_id[1] <- NA
  output <- run_phase_fixture(fixture)
  balanced <- dplyr::filter(output$contrasts, sample == "all_three_observed_attendees")
  expect_equal(balanced$estimate, c(-.1, .2, .1), tolerance = 1e-12)
  expect_equal(balanced$n_unknown_group, rep(1L, 3))
  expect_true(all(balanced$inference == "descriptive_unknown_groups"))
  expect_true(all(is.na(balanced$std_error)))
})

test_that("source-local respondent IDs cannot create cross-source phase linkage", {
  fixture <- phase_fixture()
  other <- fixture$participants
  other$source_dataset <- "control"
  fixture$participants <- dplyr::bind_rows(fixture$participants, other)
  scores <- fixture$scores
  scores$source_dataset <- "control"
  scores$score[scores$wave == "t2" & scores$respondent_id == "a"] <- .4
  fixture$scores <- dplyr::bind_rows(fixture$scores, scores)
  output <- run_phase_fixture(fixture)
  expect_setequal(output$contrasts$source_dataset, c("historical", "control"))
  post_pre <- dplyr::filter(output$contrasts,
    sample == "all_three_observed_attendees", contrast == "post_minus_pre_arrival"
  )
  expect_equal(post_pre$estimate[post_pre$source_dataset == "historical"], .1, tolerance = 1e-12)
  expect_equal(post_pre$estimate[post_pre$source_dataset == "control"], -.1, tolerance = 1e-12)
  duplicate <- fixture
  duplicate$scores <- dplyr::bind_rows(duplicate$scores, duplicate$scores[1, ])
  expect_error(run_phase_fixture(duplicate), "Phase score identities must be unique")
  duplicate <- fixture
  duplicate$participants <- dplyr::bind_rows(duplicate$participants, duplicate$participants[1, ])
  expect_error(run_phase_fixture(duplicate), "Participant identities must be unique")
})

test_that("a missing arrival phase cannot substitute pre-arrival for arrival", {
  fixture <- phase_fixture()
  fixture$scores <- dplyr::filter(fixture$scores, wave != "t1")
  output <- run_phase_fixture(fixture)
  pairs <- dplyr::filter(output$contrasts, sample == "available_paired_attendees")
  expect_equal(pairs$n_people, c(0L, 0L, 2L))
  expect_true(all(is.na(pairs$estimate[1:2])))
  expect_equal(pairs$estimate[3], .1, tolerance = 1e-12)
  arrival <- dplyr::filter(output$coverage, phase == "t1")
  expect_equal(arrival$n_missing_phase, 7L)
  expect_equal(arrival$n_unknown_presence, 7L)
  fixture$scores$wave_observed[fixture$scores$respondent_id == "a" & fixture$scores$wave == "t2"] <- NA
  output <- run_phase_fixture(fixture)
  unknown <- dplyr::filter(output$selection, analysis == "attendee_exit_attrition", category == "exit_unknown")
  expect_equal(unknown$n_people, 1L)
  expect_equal(unknown$mean_t0, .2)
})

test_that("interim measurements and follow-ups are not treated as arrival or exit", {
  fixture <- phase_fixture()
  roles <- c(t0 = "pre_arrival", t1 = "arrival", t2 = "post_deliberation")
  fixture$scores$wave_role <- unname(roles[fixture$scores$wave])
  original <- run_phase_fixture(fixture)
  interim <- dplyr::filter(fixture$scores, wave == "t1")
  interim$wave <- "interim_1"
  interim$wave_role <- "interim_deliberation"
  followup <- dplyr::filter(fixture$scores, wave == "t2")
  followup$wave <- "t3"
  followup$wave_role <- "follow_up"
  fixture$scores <- dplyr::bind_rows(fixture$scores, interim, followup)
  expect_equal(run_phase_fixture(fixture), original)
  fixture$scores$wave_role[fixture$scores$wave == "t1"] <- "interim_deliberation"
  expect_error(run_phase_fixture(fixture), "Event-phase labels do not match verified wave roles")
})

test_that("published phase changes preserve a common three-wave sample", {
  output <- read_output("phase_contrasts.csv")
  balanced <- dplyr::filter(output, sample == "all_three_observed_attendees", n_people > 0)
  wide <- balanced |>
    dplyr::select(poll_id, source_dataset, battery_id, contrast, estimate, n_people) |>
    tidyr::pivot_wider(names_from = contrast, values_from = c(estimate, n_people))
  expect_equal(nrow(wide), 2L)
  expect_equal(
    wide$estimate_post_minus_pre_arrival,
    wide$estimate_post_minus_arrival + wide$estimate_arrival_minus_pre_arrival
  )
  expect_equal(wide$n_people_post_minus_arrival, wide$n_people_arrival_minus_pre_arrival)
  expect_equal(wide$n_people_post_minus_arrival, wide$n_people_post_minus_pre_arrival)
  expect_true(all(output$n_people <= output$n_attendees_total))
  expect_true(all(is.na(output$std_error[output$inference == "descriptive_unknown_groups"])))
  provenance <- jsonlite::read_json(project_file("tabs", "phase_provenance.json"))
  expect_equal(provenance$bootstrap_replicates, 999L)
})


test_that("recruitment nonattenders are not relabeled as verified invitees", {
  fixture <- phase_fixture()
  fixture$participants$arm[fixture$participants$respondent_id == "e"] <- "recruitment_nonattender"
  output <- run_phase_fixture(fixture)
  selected <- dplyr::filter(output$selection, analysis == "baseline_selection_difference")
  expect_setequal(selected$category, c("attended_minus_recruitment_nonattender", "attended_minus_control"))
  row <- dplyr::filter(selected, category == "attended_minus_recruitment_nonattender")
  expect_equal(row$n_reference_scored, 1L)
  expect_equal(row$estimate, .4)
  selected$source_dataset <- "control"
  expect_true("Recruitment nonattenders" %in% phase_selection_table(selected)$Comparator)
})


test_that("main interview labels follow stages, not input score numbers", {
  panel <- tibble::tibble(poll_id = c("first", "second"), source_dataset = "historical")
  scores <- tibble::tibble(
    poll_id = rep(c("first", "second"), each = 2L), source_dataset = "historical",
    original_score_wave = rep(c("t1", "t2"), 2L), wave = c("t0", "t2", "t0", "t3")
  )
  expect_error(main_interview_timing(panel, scores))
  scores$wave[4] <- "t2"
  timing <- main_interview_timing(panel, scores)
  expect_equal(timing$phase_t1, c("t0", "t0"))
  expect_equal(timing$phase_t2, c("t2", "t2"))
})
