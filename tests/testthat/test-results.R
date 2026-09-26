expect_near <- \(actual, expected, tolerance) expect_lte(abs(actual - expected), tolerance)

read_output <- \(name) readr::read_csv(file.path("../../tabs", name), show_col_types = FALSE)

test_that("A1R 2019 scoring reproduces the published 46% -> 60%", {
  effects <- read_output("control_effects.csv") |>
    dplyr::filter(study == "America in One Room 2019", comparison == "Attended vs uninvited control")
  expect_near(effects$treated_gain, 0.60 - 0.46, 0.005)
  expect_near(effects$control_gain, 0.01, 0.005)
})

test_that("control comparisons distinguish the T3-only poll", {
  effects <- read_output("control_effects.csv")
  pooled <- read_output("meta_control.csv")
  expect_equal(dplyr::n_distinct(effects$study), 4L)
  expect_true(all(pooled$studies == 3L))
  ni <- dplyr::filter(effects, study == "Northern Ireland 2007")
  expect_equal(c(ni$n_treated, ni$n_control), c(93L, 150L))
  expect_true(is.na(ni$control_t1_sd))
  expect_true(is.finite(ni$control_t3_sd))
})

test_that("small-group assignment shows no peer sorting", {
  check <- read_output("assignment_check.csv")
  expect_true(all(abs(check$estimate / check$std_error) < 2))
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

test_that("gains are computed for all item-linked respondent polls", {
  gains <- read_output("poll_gains.csv")
  expect_equal(nrow(gains), 31)
  expect_equal(sum(gains$respondents), 10598L)
  expect_equal(sum(gains$respondents[!is.na(gains$dpnum)]), 5869L)
  expect_equal(sum(is.na(gains$dpnum)), 10L)
  effects <- read_output("control_effects.csv") |>
    dplyr::filter(comparison %in% c(
      "Attended vs uninvited control", "Attended vs randomized control"
    ))
  expect_equal(sum(gains$respondents[gains$poll_id %in% c(
    "america-in-one-room-2019", "a1r-climate-2021", "amr-2024"
  )]), sum(effects$n_treated))
  expect_equal(anyDuplicated(gains$poll_id), 0L)
  expect_true(all(gains$raw_se > 0))
})

test_that("guessing-adjusted pooling uses the same poll samples", {
  gains <- read_output("poll_gains.csv")
  adjusted <- read_output("guessing_gains.csv")
  meta <- read_output("meta.csv")
  expect_setequal(adjusted$poll_id, gains$poll_id)
  expect_equal(adjusted$respondents[match(gains$poll_id, adjusted$poll_id)],
               gains$respondents)
  expect_true(all(is.finite(adjusted$adjusted_se) & adjusted$adjusted_se > 0))
  expect_true(all(adjusted$adjusted >= 0 & adjusted$adjusted <= 1))
  expect_equal(sum(meta$model == "guessing adjusted SD, pooled" &
                     meta$parameter == "mu"), 1L)
})

test_that("missed-item peer model uses the historical item sample", {
  items <- read_output("models.csv") |> dplyr::filter(model == "items")
  main <- read_output("models.csv") |> dplyr::filter(model == "main")
  expect_equal(unique(items$polls), 21L)
  expect_equal(unique(items$n), 5758L)
  expect_equal(unique(main$n) - unique(items$n), 0L)
  expect_true(all(c(
    "group_k1", "group_k1_items", "no_missed_items"
  ) %in% items$term))
  expect_true(all(items$r2_marginal >= 0 & items$r2_conditional <= 1))
})

test_that("briefing model uses all source-linked reading reports", {
  briefing <- read_output("models.csv") |> dplyr::filter(model == "briefing")
  expect_equal(unique(briefing$polls), 9L)
  expect_equal(unique(briefing$n), 2643L)
})

test_that("AMR 2024 counts match the published Extended Data Table 2", {
  effects <- read_output("control_effects.csv") |>
    dplyr::filter(study == "Antimicrobial Resistance 2024", grepl("control: ", comparison))
  counts <- stats::setNames(paste(effects$n_treated, effects$n_control), sub(".*: ", "", effects$comparison))
  expect_equal(
    counts[c("Brazil", "Colombia", "India", "Indonesia", "Nigeria", "Tanzania")],
    c(
      Brazil = "188 178", Colombia = "275 185", India = "231 160", Indonesia = "198 200",
      Nigeria = "203 210", Tanzania = "185 206"
    )
  )
})
