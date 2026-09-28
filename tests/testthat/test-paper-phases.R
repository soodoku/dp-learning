test_that("phase exhibits keep scores, sample sizes and unsupported intervals", {
  balanced <- read_output("phase_contrasts.csv") |>
    dplyr::filter(sample == "all_three_observed_attendees", n_people > 0L)
  table <- phase_comparison_table(balanced)
  expect_equal(nrow(table), 2L)
  expect_true(all(vapply(table, is.character, logical(1))))
  marousi <- table[grepl("Marousi", table$Poll), ]
  expect_equal(unname(marousi$N), "133")
  expect_equal(unname(marousi$`During event (pp)`), "2.3")
  expect_equal(unname(marousi$`Before arrival (pp)`), "5.0")
  expect_equal(unname(marousi$`Total (pp)`), "7.3")
  europe <- table[grepl("Europe", table$Poll), ]
  expect_equal(unname(europe$N), "332")
  expect_match(europe$`During event (pp)`, "7.8", fixed = TRUE)
  expect_match(europe$`During event (pp)`, "[5.6, 10.2]", fixed = TRUE)
  changed <- balanced
  changed$n_people[1] <- changed$n_people[1] - 1L
  expect_error(phase_comparison_table(changed))
})

test_that("selection and attrition exhibits retain distinct populations", {
  source <- read_output("pre_arrival_selection.csv")
  selection <- phase_selection_table(source)
  expect_equal(nrow(selection), 4L)
  a1r <- selection[selection$Poll == "America in One Room", ]
  expect_true(all(a1r$`Attendee N` == "526"))
  invited <- a1r[a1r$Comparator == "Invited nonattenders", ]
  expect_equal(invited$`Comparator N`, "2,215")
  expect_equal(invited$`Gap (pp)`, "8.0")
  control <- a1r[a1r$Comparator == "Uninvited controls", ]
  expect_equal(control$`Comparator N`, "1,101")
  expect_equal(control$`Gap (pp)`, "5.3")
  attrition <- phase_attrition_table(source)
  a1r <- attrition[attrition$Poll == "America in One Room", ]
  expect_equal(sum(as.integer(a1r$`Attendee N`)), 526L)
  expect_equal(a1r$`Attendee N`[a1r$Exit == "Absent"], "3")
  europe <- attrition[grepl("Europe", attrition$Poll), ]
  expect_equal(europe$`Attendee N`[europe$Exit == "Uncertain"], "9")
  expect_false(any(attrition$Exit == "Unknown"))
})
