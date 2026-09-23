expect_near <- \(actual, expected, tolerance) expect_lte(abs(actual - expected), tolerance)

read_output <- \(name) readr::read_csv(file.path("../../tabs", name), show_col_types = FALSE)

test_that("item learning reproduces Cor and Sood (2016)", {
  learning <- read_output("item_learning.csv")
  expect_equal(nrow(learning), 23)
  expect_near(mean(learning$raw), 0.159, 0.001)
  expect_near(mean(learning$lca), 0.208, 0.001)
  expect_true(all(learning$converged))
})

test_that("A1R 2019 scoring reproduces the published 46% -> 60%", {
  effects <- read_output("control_effects.csv") |>
    dplyr::filter(study == "America in One Room 2019", comparison == "Attended vs uninvited control")
  expect_near(effects$treated_gain, 0.60 - 0.46, 0.005)
  expect_near(effects$control_gain, 0.01, 0.005)
})

test_that("Tanzania effects match Sandefur et al. appendix Table 6", {
  effects <- read_output("control_effects.csv") |> dplyr::filter(study == "Tanzania 2015")
  get <- \(label) effects$estimate[effects$comparison == label]
  expect_near(get("Deliberation vs control villages"), 0.330, 0.01)
  expect_near(get("Information vs control villages"), 0.126, 0.01)
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
  expect_equal(out$item_group_k1, c(1, 1, NA))
})

test_that("gains are computed for all 22 polls", {
  gains <- read_output("poll_gains.csv")
  expect_equal(nrow(gains), 22)
  expect_true(all(gains$raw_se > 0))
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


test_that("Greece enters with its T2 zeros treated as missing", {
  gains <- read_output("poll_gains.csv") |> dplyr::filter(pollname == "Marousi, Greece")
  expect_equal(gains$respondents, 146)
  expect_near(gains$raw, 0.076, 0.005)
})
