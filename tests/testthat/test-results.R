expect_near <- \(actual, expected, tolerance) expect_lte(abs(actual - expected), tolerance)

test_that("A1R 2019 scoring reproduces the published 46% -> 60%", {
  effects <- read_output("control_effects.csv") |>
    dplyr::filter(study == "America in One Room 2019", comparison == "Attended vs uninvited control")
  expect_near(effects$treated_gain, 0.60 - 0.46, 0.005)
  expect_near(effects$control_gain, 0.01, 0.005)
})

test_that("control comparisons distinguish the T3-only poll", {
  effects <- read_output("control_effects.csv")
  expect_equal(dplyr::n_distinct(effects$study), 3L)
  ni <- dplyr::filter(effects, study == "Northern Ireland 2007")
  expect_equal(c(ni$n_treated, ni$n_control), c(93L, 150L))
  expect_true(is.na(ni$control_t1_sd))
  expect_true(is.finite(ni$control_t3_sd))
})

test_that("assignment checks use the main sample and report finite intervals", {
  check <- read_output("assignment_check.csv")
  expect_setequal(
    check$covariate,
    c("k1", "female", "age", "education_higher")
  )
  k1 <- dplyr::filter(check, covariate == "k1")
  expect_equal(k1$n, 8450L)
  expect_true(all(is.finite(check$lower) & is.finite(check$upper)))
  expect_true(all(check$lower <= check$upper))
})

test_that("item_group_knowledge() matches a hand calculation", {
  items <- tibble::tribble(
    ~pollid, ~caseid, ~group, ~item, ~correct,
    1, 1, "g", "a", 0, 1, 1, "g", "b", 1,
    1, 2, "g", "a", 1, 1, 2, "g", "b", 0,
    1, 3, "g", "a", 1, 1, 3, "g", "b", 1
  )
  out <- item_group_knowledge(items)
  # Person 1 missed a; others (2, 3) got a right: 1. Person 2 missed b; others: (1 + 1) / 2.
  expect_equal(out$item_group_k1, c(1, 1, 0))
  expect_equal(out$no_missed_items, c(0L, 0L, 1L))
})

test_that("missed-item peer scores ignore absent answers and empty groups", {
  items <- tibble::tribble(
    ~pollid, ~caseid, ~group, ~item, ~correct,
    1, 1, "g", "a", 0, 1, 2, "g", "a", NA,
    1, 3, "g", "a", 1, 1, 4, "solo", "a", 0
  )
  out <- item_group_knowledge(items)
  expect_equal(out$item_group_k1, c(1, NA, 0, NA))
  expect_equal(out$no_missed_items, c(0L, 0L, 1L, 0L))
})

test_that("gains use the same grouped attendees as the main model", {
  gains <- read_output("poll_gains.csv")
  expect_identical(gains$poll_id, read_output("polls.csv")$poll_id)
  expect_equal(nrow(gains), 27L)
  expect_equal(sum(gains$respondents), 8450L)
  expect_equal(gains$respondents[gains$poll_id == "btp-online-primaries-2004"], 238L)
  effects <- read_output("control_effects.csv") |>
    dplyr::filter(comparison %in% c("Attended vs uninvited control", "Completed vs uninvited control"))
  expect_equal(sum(gains$respondents[gains$poll_id %in% c(
    "america-in-one-room-2019", "a1r-climate-2021"
  )]), sum(effects$n_treated))
  expect_equal(anyDuplicated(gains$poll_id), 0L)
  expect_true(all(gains$raw_se > 0))
})

test_that("guessing-adjusted pooling uses the same poll samples", {
  gains <- read_output("poll_gains.csv")
  meta <- read_output("meta.csv")
  expect_true(all(is.finite(gains$adjusted_se) & gains$adjusted_se > 0))
  expect_true(all(gains$adjusted >= 0 & gains$adjusted <= 1))
  expect_true(all(meta$polls[meta$parameter == "mu"] == nrow(gains)))
  expect_equal(sum(meta$model == "guessing adjusted SD, pooled" &
                     meta$parameter == "mu"), 1L)
})

test_that("missed-item peer model uses the main item sample", {
  items <- read_output("models.csv") |> dplyr::filter(model == "items")
  main <- read_output("models.csv") |> dplyr::filter(model == "demographic")
  core <- read_output("models.csv") |> dplyr::filter(model == "core")
  expect_equal(unique(core$polls), 27L)
  expect_equal(unique(core$n), 8450L)
  expect_equal(unique(items$polls), 27L)
  expect_equal(unique(main$n) - unique(items$n), 0L)
  expect_true(all(c(
    "group_k1", "group_k1_items", "no_missed_items"
  ) %in% items$term))
  expect_true(all(items$r2_marginal >= 0 & items$r2_conditional <= 1))
})

test_that("briefing model uses all source-linked reading reports", {
  briefing <- read_output("models.csv") |> dplyr::filter(model == "briefing")
  expect_equal(unique(briefing$polls), 12L)
  expect_gt(unique(briefing$n), 0L)
})

test_that("conditional gain and posttest models are equivalent", {
  frame <- core_group_frame(attendee_panel())
  gain <- fit_knowledge(frame, core_formula, check = FALSE)
  posttest <- fit_knowledge(frame, stats::update(core_formula, k2 ~ .), check = FALSE)
  output <- read_output("models.csv") |> dplyr::filter(model == "core")
  expect_equal(output$estimate, unname(lme4::fixef(gain)[output$term]), tolerance = 1e-4)
  coefficients <- lme4::fixef(gain)
  coefficients[["k1"]] <- coefficients[["k1"]] + 1
  expect_equal(coefficients, lme4::fixef(posttest), tolerance = 1e-4)
  expect_equal(stats::fitted(gain) + frame$k1, stats::fitted(posttest), tolerance = 1e-4)
  expect_equal(stats::residuals(gain), stats::residuals(posttest), tolerance = 1e-4)
})


test_that("expanded models retain all polls and comparisons hold cases fixed", {
  models <- read_output("models.csv")
  samples <- dplyr::distinct(models, model, n, polls, groups)
  row <- function(name) samples[samples$model == name, ]
  expect_equal(row("demographic")$polls, 27L)
  expect_equal(row("demographic")$groups, 622L)
  expect_equal(row("demographic")$n, 8335L)
  expect_equal(row("core_demographic_sample")$n, row("demographic")$n)
  expect_equal(row("attitudes")$polls, 27L)
  expect_equal(row("attitude_sd")$n, row("attitudes")$n)
  expect_equal(row("demographic_attitude_sample")$n, row("attitudes")$n)
  expect_equal(row("demographic_attitude_sample")$polls, row("attitudes")$polls)
})
