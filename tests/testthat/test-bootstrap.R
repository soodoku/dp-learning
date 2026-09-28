test_that("resampling preserves groups, waves, and independent control units", {
  data <- tibble::tibble(
    pollid = rep(c("p1", "p2"), each = 6),
    group = rep(c("g1", "g1", "g2", "g2", "c1", "c2"), 2),
    arm = rep(c(rep("attended", 4), rep("control", 2)), 2),
    person = seq_len(12), k1 = seq_len(12) / 20, k2 = seq_len(12) / 20 + .1
  )
  set.seed(1)
  sampled <- resample_hierarchy(data)
  expect_equal(nrow(sampled), nrow(data))
  expect_equal(sampled$k2 - sampled$k1, rep(.1, nrow(sampled)))
  expect_equal(sampled$k1, data$k1[match(sampled$person, data$person)])
  sizes <- dplyr::count(sampled, pollid, arm, group)
  expect_true(all(sizes$n[sizes$arm == "attended"] == 2L))
  expect_true(all(sizes$n[sizes$arm == "control"] == 1L))
  expect_equal(nrow(sizes), 8L)
})

test_that("hierarchical uncertainty reflects shared poll and group shocks", {
  set.seed(22)
  data <- expand.grid(person = seq_len(10), group_number = seq_len(5), pollid = seq_len(20))
  data$group <- interaction(data$pollid, data$group_number)
  data$group <- as.character(data$group)
  data$value <- rep(rnorm(20, sd = 2), each = 50) + rep(rnorm(100), each = 10) + rnorm(1000, sd = .1)
  result <- bootstrap_stat(data, function(x) c(mean = mean(x$value)), n = 199L)
  expect_equal(unname(result$estimate), mean(data$value))
  expect_gt(unname(result$se), 3 * stats::sd(data$value) / sqrt(nrow(data)))
  expect_lt(result$lower, result$upper)
})

test_that("pooling weights polls equally and is reproducible", {
  old <- options(dp.bootstrap.replicates = 99L)
  on.exit(options(old))
  x <- pool_bootstrap(c(.1, .3), list(rep(.1, 99), rep(.3, 99)))
  expect_equal(x[["estimate"]], .2)
  expect_identical(x, pool_bootstrap(c(.1, .3), list(rep(.1, 99), rep(.3, 99))))
})

test_that("absolute and relative summaries use the declared denominator", {
  x <- read_output("poll_gains_main.csv")
  expect_equal(x$raw, x$k2_mean - x$k1_mean)
  expect_equal(x$relative, x$raw / x$k1_mean)
  controls <- read_output("control_learning.csv")
  expect_setequal(controls$poll_id, c("america-in-one-room-2019", "a1r-climate-2021"))
  counts <- dplyr::distinct(controls, poll_id, n_treated, n_control)
  expect_equal(counts$n_treated, x$respondents[match(counts$poll_id, x$poll_id)])
  expect_equal(nrow(controls), 4L)
})


test_that("poll resampling preserves online and in-person composition", {
  data <- tibble::tibble(pollid = seq_len(6), group = letters[1:6], online = c(0, 0, 0, 0, 1, 1))
  for (seed in seq_len(20)) {
    set.seed(seed)
    out <- resample_hierarchy(data)
    expect_equal(sum(out$online), 2)
    expect_equal(nrow(out), 6)
  }
})
