test_that("a correct outcome model recovers the cohort mean under selective response", {
  x <- rep(c(-1, 0, 1), each = 100)
  y <- stats::plogis(-.2 + .8 * x)
  observed <- c(seq_len(100) <= 90, seq_len(100) <= 30, seq_len(100) <= 60)
  data <- tibble::tibble(poll_id = "example", x, score_t2 = y, wave_observed_t2 = observed)
  fit <- attrition_mean(data, "t2", "x")
  expect_equal(fit$aipw, mean(y), tolerance = 1e-8)
  expect_gt(abs(mean(y[observed]) - mean(y)), .01)
  data$score_t2[!observed] <- 0
  expect_equal(attrition_mean(data, "t2", "x"), fit)
  data$score_t2[!observed] <- NA_real_
  expect_equal(attrition_mean(data, "t2", "x"), fit)
  expect_lte(fit$effective_n, sum(observed))
})

test_that("fully observed means need no fitted response model", {
  data <- tibble::tibble(poll_id = "example", score_t0 = c(0, .5, 1), wave_observed_t0 = TRUE)
  fit <- attrition_mean(data, "t0", "score_t0")
  expect_equal(fit$aipw, .5)
  expect_equal(fit$ipw, .5)
  expect_equal(fit$effective_n, 3)
  data$wave_observed_t0 <- FALSE
  expect_error(attrition_mean(data, "t0", "score_t0"))
})

test_that("attrition targets preserve the original source cohort without changing main eligibility", {
  frame <- attrition_frame()
  marousi <- dplyr::filter(frame, poll_id == "marousi-2006")
  target <- attrition_target(marousi)
  expect_equal(nrow(target), 159L)
  expect_equal(sum(!target$attended), 21L)
  missing_exit <- target[!phase_has_score(target, "t2"), ]
  expect_equal(nrow(missing_exit), 21L)
  expect_true(all(missing_exit$attendance_before_post_rule))
  expect_true(all(!missing_exit$participant))
  expect_true(all(!missing_exit$attended))
  expect_equal(sum(!phase_has_score(target, "t1") & phase_has_score(target, "t2")), 5L)
  expect_equal(attrition_predictors(target), "score_t0")
  nic <- attrition_target(dplyr::filter(frame, poll_id == "nic-1996"))
  expect_equal(nrow(nic), 456L)
  expect_equal(sum(phase_has_score(nic, "t3")), 383L)
  expect_true(all(nic$attended))
  expect_equal(attrition_predictors(nic), c("score_t0", "score_t2"))
})

test_that("published adjustments preserve counts and additive contrasts", {
  result <- attrition_analysis()
  for (name in names(result)) {
    published <- read_output(paste0("attrition_", name, ".csv"))
    expect_equal(as.data.frame(published), as.data.frame(result[[name]]), ignore_attr = TRUE)
  }
  for (poll in unique(result$contrasts$poll_id)) {
    rows <- dplyr::filter(result$contrasts, poll_id == poll)
    expect_equal(nrow(rows), 3L)
    for (method in c("complete_case", "ipw", "aipw")) {
      expect_equal(rows[[method]][3], sum(rows[[method]][1:2]), tolerance = 1e-12)
    }
  }
  flow <- result$flow
  expect_equal(flow$n_attendee_observed + flow$n_attendee_absent + flow$n_attendee_unknown_presence, flow$n_attendees)
  expect_equal(flow$n_attendees + flow$n_nonattendees + flow$n_unknown_attendance, flow$n_source)
  expect_true(all(result$means$min_probability > .7))
  expect_true(all(result$means$max_weight < 1.5))
  expect_true(all(result$means$aipw >= 0 & result$means$aipw <= 1))
})
