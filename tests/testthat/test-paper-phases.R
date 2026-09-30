test_that("phase exhibits report estimates and intervals on the same sample", {
  balanced <- read_output("phase_contrasts.csv") |>
    dplyr::filter(sample == "all_three_observed_attendees", n_people > 0L)
  table <- phase_comparison_table(balanced)
  expect_equal(nrow(table), 6L)
  expect_setequal(balanced$poll_id, c(
    "california-whats-next-2011", "europolis-2009", "marousi-2006",
    "michigan-2009", "tomorrows-europe-2007"
  ))
  expect_equal(
    unique(balanced$n_people[balanced$poll_id == "california-whats-next-2011"]), 386L
  )
  expect_equal(unique(balanced$n_people[balanced$poll_id == "europolis-2009"]), 348L)
  expect_equal(unique(balanced$n_people[balanced$poll_id == "michigan-2009"]), 309L)
  expect_true(all(vapply(table, is.character, logical(1))))
  marousi <- table[grepl("Marousi", table$Poll), ]
  expect_equal(unname(marousi$N), "129")
  expect_match(unname(marousi$`Arrival to exit`), "2.5 ", fixed = TRUE)
  expect_match(unname(marousi$`Pre-arrival to arrival`), "4.9 ", fixed = TRUE)
  expect_match(unname(marousi$`Pre-arrival to exit`), "7.4 ", fixed = TRUE)
  for (column in names(marousi)[-(1:3)]) {
    expect_match(marousi[[column]], "\\[-?[0-9.]+, -?[0-9.]+\\]")
  }
  europe <- table[grepl("Europe", table$Poll), ]
  expect_equal(unname(europe$N), "332")
  expect_match(europe$`Arrival to exit`, "7.8", fixed = TRUE)
  expect_match(europe$`Arrival to exit`, "7.8 ", fixed = TRUE)
  expect_match(europe$`Arrival to exit`, "\\[[0-9.]+, [0-9.]+\\]")
  changed <- balanced
  changed$n_people[1] <- changed$n_people[1] - 1L
  expect_error(phase_comparison_table(changed))
})

test_that("selection and attrition exhibits retain distinct populations", {
  source <- read_output("pre_arrival_selection.csv")
  selection <- phase_selection_table(source)
  expect_equal(nrow(selection), 5L)
  amr <- selection[selection$Poll == "Antimicrobial Resistance", ]
  expect_equal(amr$`Participant N`, "1,280")
  expect_equal(amr$`Comparator N`, "1,139")
  expect_equal(amr$`Gap (pp)`, "4.7")
  a1r <- selection[selection$Poll == "America in One Room", ]
  expect_true(all(a1r$`Participant N` == "523"))
  invited <- a1r[a1r$Comparator == "Recruitment nonattenders", ]
  expect_equal(invited$`Comparator N`, "2,215")
  expect_equal(invited$`Gap (pp)`, "8.0")
  control <- a1r[a1r$Comparator == "Uninvited controls", ]
  expect_equal(control$`Comparator N`, "1,101")
  expect_equal(control$`Gap (pp)`, "5.3")
  attrition <- phase_attrition_table(source)
  a1r <- attrition[attrition$Poll == "America in One Room", ]
  expect_equal(sum(as.integer(a1r$`Attendee N`)), 523L)
  expect_false(any(a1r$Exit == "Absent"))
  original <- read_phase_participants() |>
    dplyr::filter(poll_id == "america-in-one-room-2019", source_dataset == "control")
  expect_equal(sum(original$attendance_before_post_rule %in% TRUE), 526L)
  expect_equal(sum(original$attendance_before_post_rule %in% TRUE & !original$attended), 3L)
  europe <- attrition[grepl("Europe", attrition$Poll), ]
  expect_false(any(europe$Exit == "Uncertain"))
  original <- read_phase_participants() |>
    dplyr::filter(poll_id == "tomorrows-europe-2007", source_dataset == "historical")
  expect_equal(sum(original$attendance_before_post_rule %in% TRUE & !original$attended), 9L)
  expect_false(any(attrition$Exit == "Unknown"))
})


test_that("phase exhibits distinguish question batteries within the same poll", {
  pairs <- phase_pair_table(read_output("phase_contrasts.csv"))
  california <- pairs[grepl("California", pairs$Poll) & pairs$Comparison == "Arrival to exit", ]
  expect_setequal(california$Items, c("5", "8"))
  europolis <- pairs[pairs$Poll == "Europolis" & pairs$Comparison == "Arrival to exit", ]
  expect_setequal(europolis$Items, c("6", "9"))
  michigan <- pairs[pairs$Poll == "Michigan" & pairs$Comparison == "Arrival to exit", ]
  expect_setequal(michigan$Items, c("4", "6", "9"))
})
