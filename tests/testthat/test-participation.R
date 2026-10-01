test_that("main models require eligibility as well as attendance and paired scores", {
  people <- read_analysis_participants()
  before <- attendee_panel(participants = people)
  selected_id <- before$historical_respondent_id[before$poll_id == "btp-national-2003"][1]
  row <- which(people$poll_id == "btp-national-2003" & people$source_dataset == "historical" &
                 people$historical_respondent_id == selected_id)
  expect_length(row, 1L)
  expect_true(people$attended[row])
  expect_true(people$participant[row])
  for (eligible in c(FALSE, NA)) {
    changed <- people
    changed$participant[row] <- eligible
    after <- attendee_panel(participants = changed)
    expected <- dplyr::filter(before, !(poll_id == "btp-national-2003" & historical_respondent_id == selected_id))
    expect_identical(after, expected)
    expect_identical(changed$attended, people$attended)
  }
})

test_that("missing collected arrival forms exclude people even with two available scores", {
  people <- read_analysis_participants()
  panel <- attendee_panel()
  missing_arrival <- people |>
    dplyr::filter(
      poll_id %in% c("denmark-euro-2000", "michigan-2009", "tomorrows-europe-2007"),
      exclusion_reason == "missing_t1_questionnaire", attended %in% TRUE
    )
  expect_true(all(c("denmark-euro-2000", "michigan-2009", "tomorrows-europe-2007") %in% missing_arrival$poll_id))
  wrongly_included <- dplyr::inner_join(panel, missing_arrival,
    by = c("poll_id", "source_dataset", "respondent_id")
  )
  expect_equal(nrow(wrongly_included), 0L)
  selected <- dplyr::left_join(panel, people,
    by = c("poll_id", "source_dataset", "respondent_id"), relationship = "one-to-one"
  )
  expect_true(all(selected$participant))
  expect_equal(nrow(panel), 10239L)
  expect_equal(nrow(core_group_frame(panel)), 8450L)
})

test_that("zero sensitivity uses only the selected battery and preserves main zero scores", {
  panel <- tibble::tibble(
    poll_id = "p", source_dataset = "s", respondent_id = c("1", "2", "3", "4"),
    k1 = c(0, 0, .5, .5), k2 = .5
  )
  flags <- tidyr::expand_grid(respondent_id = panel$respondent_id, wave = c("t0", "t2")) |>
    dplyr::left_join(panel, by = "respondent_id") |>
    dplyr::mutate(
      battery_id = "p:s:knowledge", score = ifelse(wave == "t0", k1, k2),
      zero_score = score == 0, all_blank = respondent_id == "1" & wave == "t0", wave_observed = TRUE
    )
  flags <- dplyr::select(flags, -k1, -k2)
  flags$all_blank[flags$respondent_id == "4"] <- NA
  alternative <- dplyr::mutate(flags, battery_id = "p:s:knowledge_expanded", zero_score = TRUE, all_blank = TRUE)
  flags <- dplyr::bind_rows(flags, alternative)
  expect_equal(sum(panel$k1 == 0), 2L)
  expect_identical(knowledge_sensitivity_panel(panel, "zero_score", flags), panel[3:4, ])
  expect_identical(knowledge_sensitivity_panel(panel, "all_blank", flags), panel[2:4, ])
  flags$score[1] <- 1
  expect_error(knowledge_sensitivity_panel(panel, "zero_score", flags))
})

test_that("upstream zero flags match main scores without changing their sample", {
  panel <- attendee_panel()
  result <- knowledge_sensitivity_panel(panel)
  expect_identical(result, dplyr::filter(panel, k1 != 0, k2 != 0))
  expect_true(any(panel$k1 == 0 | panel$k2 == 0))
  blanks <- knowledge_sensitivity_panel(panel, "all_blank")
  expect_true(nrow(blanks) >= nrow(result))
  expect_true(nrow(blanks) < nrow(panel))
})


test_that("source-specific education labels preserve the ordered design matrix", {
  values <- c(0, .5, 1, .5, 0)
  old <- factor(c("lower", "middle", "higher", "middle", "lower"), levels = c("lower", "middle", "higher"))
  current <- factor(education_labels[as.character(values)], levels = education_labels)
  expect_equal(as.numeric(stats::model.matrix(~ current)), as.numeric(stats::model.matrix(~ old)))
  expect_identical(unname(education_labels), c("Lower education", "Middle education", "Higher education"))
  expect_false(any(grepl("degree|BA|school", unname(term_labels[grepl("education", names(term_labels))]))))
})


test_that("sensitivity results preserve full bootstrap inference and declared samples", {
  results <- read_output("knowledge_sensitivity.csv")
  main <- read_output("models.csv") |>
    dplyr::filter(model == "core")
  expect_setequal(results$sample, c("main", "zero_score", "all_blank"))
  expect_true(all(results$bootstrap_replicates == 999L))
  expect_true(all(is.finite(results$lower) & is.finite(results$upper)))
  expect_true(all(results$lower <= results$upper))
  expect_equal(dplyr::filter(results, sample == "main")$estimate, main$estimate)
  panel <- attendee_panel()
  for (exclude in c("zero_score", "all_blank")) {
    expected <- knowledge_sensitivity_panel(panel, exclude)
    actual <- dplyr::filter(results, sample == exclude)
    expect_equal(unique(actual$paired_people), nrow(expected))
    expect_equal(unique(actual$n), nrow(core_group_frame(expected)))
  }
})
