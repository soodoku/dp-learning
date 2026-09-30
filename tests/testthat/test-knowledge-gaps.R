test_that("gap changes preserve paired interviews and equal poll weighting", {
  frame <- tidyr::expand_grid(poll_id = as.character(1:6), female = c(0, 1),
    education = c("Below high school", "High school", "BA or more")
  ) |>
    dplyr::mutate(
      respondent_id = as.character(dplyr::row_number()), online = as.integer(as.integer(poll_id) > 3),
      educ = match(education, c("Below high school", "High school", "BA or more")) - 1,
      education_relative = as.numeric(educ > 0),
      k1 = .1 + .02 * as.integer(poll_id) * (1 - female) + .1 * educ,
      k2 = k1 + .1 - .01 * (1 - female) + .02 * educ
    )
  output <- knowledge_gaps(frame, n_boot = 99L)
  changes <- dplyr::filter(output$summary, term == "change")
  expect_equal(changes$estimate, c(-.01, .03, .04, .02), tolerance = 1e-12)
  expect_equal(changes$lower, changes$estimate, tolerance = 1e-12)
  expect_equal(changes$upper, changes$estimate, tolerance = 1e-12)
  pre <- dplyr::filter(output$summary, term == "pre", comparison == "gender")
  expect_gt(pre$upper - pre$lower, 0)
  without <- dplyr::filter(frame, !(poll_id == "1" & education == "Below high school"))
  fewer <- knowledge_gaps(without, n_boot = 19L)$summary
  expect_equal(unique(fewer$polls[fewer$comparison == "degree_vs_below_secondary"]), 5L)
  duplicate <- dplyr::bind_rows(frame, frame[1, ])
  expect_error(knowledge_gaps(duplicate, n_boot = 19L))
})

test_that("published knowledge gaps reproduce and measure paired change", {
  output <- knowledge_gaps()
  expect_equal(as.data.frame(output$summary), as.data.frame(read_output("knowledge_gaps.csv")), ignore_attr = TRUE)
  expect_equal(
    as.data.frame(output$by_poll), as.data.frame(read_output("knowledge_gaps_by_poll.csv")), ignore_attr = TRUE
  )
  for (comparison in unique(output$summary$comparison)) {
    rows <- dplyr::filter(output$summary, comparison == .env$comparison)
    expect_equal(rows$estimate[rows$term == "change"],
      rows$estimate[rows$term == "post"] - rows$estimate[rows$term == "pre"], tolerance = 1e-12
    )
  }
  expect_equal(unique(output$summary$n_people[output$summary$comparison == "gender"]), 8353L)
  expect_equal(nrow(output$by_poll), 100L)
})


test_that("relative education reuses upstream definitions without changing identities", {
  frame <- knowledge_gap_frame()
  enriched <- add_relative_education(frame)
  expect_equal(nrow(enriched), nrow(frame))
  expect_identical(enriched[names(frame)], frame)
  expect_true(all(is.na(enriched$education_relative[enriched$source_dataset != "historical"])))
  expect_equal(dplyr::n_distinct(enriched$poll_id[!is.na(enriched$education_relative)]), 20L)
})
