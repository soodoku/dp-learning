test_that("the peer reader preserves invalid-item missingness", {
  frame <- tibble::tibble(
    poll_id = "p", source_dataset = "historical",
    respondent_id = c("1", "2", "3"), group = "g"
  )
  responses <- dplyr::mutate(dplyr::select(frame, -group), wave = "t1", item_id = "a",
    correct = c(0L, NA_integer_, 1L)
  )
  result <- add_item_peer_measure(frame, responses)
  expect_equal(result$group_k1_items, c(1, NA, 0))
  expect_equal(result$no_missed_items, c(0L, 0L, 1L))
})

test_that("guessing-model inputs preserve invalid answers and score DK as zero", {
  responses <- tibble::tibble(
    poll_id = "p", respondent_id = "1", item_id = rep(c("invalid", "dk", "right"), 2),
    wave = rep(c("t1", "t2"), each = 3),
    correct = c(NA_integer_, 0L, 1L, 1L, NA_integer_, 0L)
  )
  answers <- paired_item_answers(responses, "p")
  expect_true(is.na(answers$pre$invalid))
  expect_equal(answers$pre$dk, 0L)
  expect_equal(answers$pre$right, 1L)
  expect_equal(answers$post$invalid, 1L)
  expect_true(is.na(answers$post$dk))
})
