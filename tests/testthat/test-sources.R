test_that("upstream manifests identify available and unchanged files", {
  manifest <- upstream_source_manifest()
  expect_equal(nrow(manifest), 10L)
  expect_false(anyDuplicated(manifest$source) > 0L)
  expect_true(verify_sources())
  expect_named(control_source_paths(), c("a1r", "tanzania", "climate", "amr"))
})

test_that("briefing reports link nine upstream polls to historical participants", {
  reading <- read_briefing_scores()
  expect_equal(dplyr::n_distinct(reading$dpnum[!is.na(reading$read_briefing)]), 9L)
  expect_false(anyDuplicated(reading[c("dpnum", "caseid")]) > 0L)
})

test_that("appendix poll coverage comes from upstream data", {
  frame <- analysis_frame(dplyr::bind_rows(read_polardata(), read_greece()))
  scores <- arrow::read_parquet(source_path("knowledge_scores"))
  polls <- appendix_polls(frame, scores, read_historical_items())
  expect_equal(nrow(polls), 29L)
  expect_equal(sum(grepl("P", polls$data, fixed = TRUE)), 22L)
  expect_equal(sum(grepl("I", polls$data, fixed = TRUE)), 28L)
  expect_equal(sum(polls$data == "P+I"), 21L)
  expect_equal(polls$data[polls$poll == "Marousi, Greece"], "P")
  expect_true(any(polls$poll == "Bulgarian National Crime Poll" & polls$year == 2002L))
})

test_that("item appendix renders every canonical upstream question", {
  catalog <- readr::read_csv(
    file.path(dp_data_root(), "metadata", "items.csv"),
    col_types = readr::cols(.default = readr::col_character())
  )
  appendix <- item_appendix_markdown()
  expect_equal(lengths(regmatches(appendix, gregexpr("\\n- \\*\\*", appendix))), nrow(catalog))
  headings <- gregexpr("## ", appendix, fixed = TRUE)
  expect_equal(lengths(regmatches(appendix, headings)), dplyr::n_distinct(catalog$poll_id))
  expect_match(appendix, "Open answer coded into source categories", fixed = TRUE)
  expect_match(appendix, "Archived variable/value labels are truncated", fixed = TRUE)
})

test_that("source verification rejects missing and altered files", {
  root <- tempfile("upstream data ")
  dir.create(root)
  fixture_path <- file.path(root, "input.csv")
  writeLines(c("id,value", "1,2"), fixture_path)
  manifest <- tibble::tibble(
    source = "example", path = "input.csv",
    sha256 = digest::digest(file = fixture_path, algo = "sha256")
  )
  expect_identical(source_path("example", manifest, root), fixture_path)
  expect_true(verify_sources(manifest, root))
  writeLines(c("id,value", "1,3"), fixture_path)
  expect_error(verify_sources(manifest, root), "Source checksum mismatch: example")
  unlink(fixture_path)
  expect_error(verify_sources(manifest, root), "Missing upstream source")
  expect_error(source_path("unknown", manifest, root), "Expected exactly one source entry")
  expect_error(
    source_path("example", dplyr::bind_rows(manifest, manifest), root),
    "Expected exactly one source entry"
  )
})

test_that("the upstream location can be overridden", {
  manifest <- upstream_source_manifest()
  original <- Sys.getenv("DP_DATA_ROOT", unset = NA_character_)
  on.exit({
    if (is.na(original)) Sys.unsetenv("DP_DATA_ROOT") else Sys.setenv(DP_DATA_ROOT = original)
  })
  Sys.setenv(DP_DATA_ROOT = "/alternative/dp-data")
  expect_identical(dp_data_root(), "/alternative/dp-data")
  expect_identical(
    source_path("greece", manifest = manifest),
    "/alternative/dp-data/data/marousi-2006/participants.csv"
  )
})

test_that("baseline items join by respondent ID regardless of input order", {
  poll <- tibble::tibble(
    dpnum = 17L, caseid = c(2, 1), t1know = c(1, .5),
    pollid = 96, pollgroup = c(2, 1)
  )
  items <- tibble::tibble(
    poll_id = "san-mateo-2008", wave = 1L,
    historical_respondent_id = c("1", "2", "1", "2"),
    item_id = c("a", "b", "b", "a"), correct = c(0L, 1L, 1L, 1L)
  )
  read <- function(p = poll, x = items) t1_items_for_poll("san-mateo-2008", 17L, p, x)
  expected <- read()
  expect_identical(read(poll[2:1, ], items[4:1, ]), expected)
  expect_equal(expected$group, c("96_1", "96_1", "96_2", "96_2"))
  expect_error(read(x = items[-1, ]))
  expect_error(read(x = dplyr::bind_rows(items, items[1, ])))
  changed <- items
  changed$historical_respondent_id[1] <- "3"
  expect_error(read(x = changed))
  changed <- items
  changed$correct[1] <- 1L
  expect_error(read(x = changed))
  expect_equal(nrow(read(poll[1, ], items[items$historical_respondent_id == "2", ])), 2L)
})

test_that("NIC age and mode are consumed from corrected upstream values", {
  source <- read_polardata()
  frame <- analysis_frame(dplyr::bind_rows(source, read_greece()))
  nic <- dplyr::filter(frame, pollname == "National Issues Convention")
  expect_equal(nrow(nic), 466L)
  expect_true(all(nic$online == 0))
  expect_equal(nic$age[nic$caseid %in% 10000080], 34)
  expect_equal(sum(!is.na(nic$age)), 454L)
})

test_that("participant ages come from upstream without reader recoding", {
  source <- read_polardata()
  frame <- analysis_frame(dplyr::bind_rows(source, read_greece()))
  zeguo <- frame[frame$dpnum == 9 & frame$caseid == 52125, ]
  europolis <- frame[frame$dpnum == 11 & frame$caseid == 71300005619, ]
  expect_equal(zeguo$age, 33)
  expect_true(is.na(europolis$age))
  expect_equal(frame$age[match(
    paste(source$dpnum, source$caseid), paste(frame$dpnum, frame$caseid)
  )], source$ppage)

  source$ppage[source$dpnum == 9 & source$caseid == 52125] <- 15
  expect_error(analysis_frame(dplyr::bind_rows(source, read_greece())))
})
