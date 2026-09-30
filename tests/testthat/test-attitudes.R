test_that("pairwise disagreement measures observed pairs without a covariance determinant", {
  expect_equal(pairwise_disagreement(c(0, 1)), 1)
  expect_equal(pairwise_disagreement(c(0, .5, 1)), 2 / 3)
  expect_equal(pairwise_disagreement(c(.2, .2, .2)), 0)
  expect_equal(pairwise_disagreement(c(NA, 0, 1)), 1)
  expect_true(is.na(pairwise_disagreement(c(NA, .5))))
  expect_equal(
    pairwise_disagreement(c(.1, .3, .8)),
    pairwise_disagreement(1 - c(.1, .3, .8))
  )
})

test_that("attitude measures use the selected group members and exclude missing responses", {
  frame <- tibble::tibble(
    poll_id = "p", source_dataset = "s",
    respondent_id = c("1", "2", "3"), group = c("a", "a", "b")
  )
  long <- tidyr::expand_grid(
    respondent_id = c("1", "2", "3", "4"), attitude_id = c("x", "y")
  ) |>
    dplyr::mutate(
      poll_id = "p", source_dataset = "s", wave = "t1",
      value = c(0, NA, 1, .5, 0, 1, .5, .5)
    )
  catalog <- tibble::tibble(poll_id = "p", attitude_id = c("x", "y"), is_primary = TRUE)
  out <- add_attitude_measures(frame, long, catalog)
  expect_equal(out$extremity, c(.5, .25, .5))
  expect_equal(out$disagreement[1:2], c(1, 1))
  expect_true(is.na(out$disagreement[3]))
  expect_equal(out$attitude_items, c(1L, 2L, 2L))
})

test_that("stacked regression cells retain empty cells", {
  table <- data.frame(label = c("a", "b"), baseline = c("", ".100 [.000, .200]"))
  out <- stack_intervals(table)
  expect_equal(out$baseline[1], "")
  expect_match(out$baseline[2], ".100", fixed = TRUE)
  expect_match(out$baseline[2], "[.000, .200]", fixed = TRUE)
  expect_false(any(grepl("NA", out$baseline, fixed = TRUE)))
})


test_that("midpoint-imputed alternatives do not duplicate primary attitudes", {
  frame <- tibble::tibble(
    poll_id = "p", source_dataset = "s", respondent_id = c("1", "2"), group = "g"
  )
  attitudes <- tidyr::expand_grid(respondent_id = c("1", "2"), attitude_id = c("x", "x_midpoint_imputed")) |>
    dplyr::mutate(poll_id = "p", source_dataset = "s", wave = "t1", value = c(0, 0, NA, .5))
  catalog <- tibble::tibble(
    poll_id = "p", attitude_id = c("x", "x_midpoint_imputed"), is_primary = c(TRUE, FALSE)
  )
  actual <- add_attitude_measures(frame, attitudes, catalog)
  expect_equal(actual$attitude_items, c(1L, 0L))
  expect_equal(actual$extremity, c(.5, NA))
  expect_true(all(is.na(actual$disagreement)))
  expect_identical(actual, add_attitude_measures(frame, dplyr::filter(attitudes, attitude_id == "x"), catalog))
})
