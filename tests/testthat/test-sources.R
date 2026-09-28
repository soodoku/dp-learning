test_that("upstream manifests identify available and unchanged files", {
  manifest <- upstream_source_manifest()
  expect_equal(nrow(manifest), 12L)
  expect_false(anyDuplicated(manifest$source) > 0L)
  expect_true(verify_sources())
  expect_setequal(
    manifest$source,
    c(
      "distortions_responses", "briefing_reading", "polls", "items",
      "participants", "item_responses", "scores", "attitudes",
      "phase_participants", "phase_scores", "studies", "survey_waves"
    )
  )
})

test_that("briefing reports link nine upstream polls to historical participants", {
  reading <- read_briefing_scores()
  expect_equal(dplyr::n_distinct(reading$dpnum[!is.na(reading$read_briefing)]), 9L)
  expect_false(anyDuplicated(reading[c("dpnum", "caseid")]) > 0L)
})

test_that("both score waves are rebuilt from respondent item answers", {
  source <- dplyr::filter(read_polardata(), pollname != "National Issues Convention")
  scores <- item_scores_for_respondents(read_historical_items(), source)
  expect_equal(nrow(scores), nrow(source))
  frame <- analysis_frame(source, scores)
  expect_equal(nrow(frame), nrow(source))
  changed <- scores
  changed$k2[1] <- changed$k2[1] + 0.1
  expect_error(analysis_frame(source, changed))
})

test_that("appendix poll coverage comes from upstream data", {
  polls <- appendix_polls()
  expect_equal(nrow(polls), 27L)
  expect_equal(sum(polls$control_group), 3L)
  expect_false("Vermont Energy" %in% polls$poll)
  expect_true("Michigan" %in% polls$poll)
  expect_false(anyDuplicated(polls$poll) > 0L)
  expect_false(any(polls$poll == "Marousi, Greece"))
  expect_false(any(polls$poll == "Tanzania"))
  expect_true(polls$control_group[polls$poll == "America in One Room"])
  expect_true(any(polls$poll == "Bulgarian National Crime Poll" & polls$year == 2002L))
})

test_that("one canonical attendee panel supplies gains and group models", {
  panel <- attendee_panel()
  expect_equal(nrow(panel), 10282L)
  expect_equal(dplyr::n_distinct(panel$poll_id), 30L)
  expect_equal(dplyr::n_distinct(panel$poll_id[!is.na(panel$group)]), 27L)
  expect_equal(nrow(core_group_frame(panel)), 8496L)
  primaries <- dplyr::filter(panel, study_id == "btp-primaries-2004")
  expect_identical(unique(primaries$poll_id), "btp-online-primaries-2004")
  expect_equal(nrow(primaries), 239L)
  evidence <- read_phase_participants() |>
    dplyr::filter(poll_id == "btp-online-primaries-2004", source_dataset == "cor_sood")
  evidence <- evidence[match(primaries$respondent_id, evidence$respondent_id), ]
  expect_true(all(evidence$attendance_status == "attended"))
  expect_true(all(evidence$sessions_attended >= 1L))
  expect_setequal(unique(core_group_frame(panel)$poll_id), read_output("poll_gains.csv")$poll_id)
})

test_that("control analyses include only polls with respondent item answers", {
  panel <- control_panel()
  expect_setequal(
    unique(panel$poll_id),
    c(
      "america-in-one-room-2019", "a1r-climate-2021",
      "northern-ireland-2007"
    )
  )
  expect_true(all(unique(panel$poll_id) %in% unique(read_analysis_responses()$poll_id)))
})

test_that("item appendix renders every question for the analyzed polls", {
  catalog <- read_item_catalog() |>
    dplyr::filter(poll_id %in% read_output("poll_gains.csv")$poll_id)
  appendix <- item_appendix_markdown(items = catalog)
  expect_equal(dplyr::n_distinct(catalog$poll_id), 27L)
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
    source_path("item_responses", manifest = manifest),
    "/alternative/dp-data/output/analysis/analysis_item_responses.parquet"
  )
})

test_that("baseline items join by respondent ID regardless of input order", {
  poll <- tibble::tibble(
    dpnum = 17L, caseid = c(2, 1), t1know = c(1, .5),
    pollid = 96, pollgroup = c(2, 1)
  )
  items <- tibble::tibble(
    poll_id = "san-mateo-2008", wave = "t1",
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
  nic <- dplyr::filter(attendee_panel(), poll_id == "nic-1996")
  upstream <- read_analysis_participants() |>
    dplyr::filter(poll_id == "nic-1996", source_dataset == "historical")
  expect_equal(nrow(nic), 456L)
  expect_true(all(nic$online == 0))
  expect_equal(nic$age[nic$historical_respondent_id %in% "10000080"], 34)
  expect_equal(nic$age, upstream$age[match(nic$respondent_id, upstream$respondent_id)])
})

test_that("participant ages come from upstream without reader recoding", {
  source <- dplyr::filter(read_polardata(), pollname != "National Issues Convention")
  scores <- item_scores_for_respondents(read_historical_items(), source)
  frame <- analysis_frame(source, scores)
  zeguo <- frame[frame$dpnum == 9 & frame$caseid == 52125, ]
  europolis <- frame[frame$dpnum == 11 & frame$caseid == 71300005619, ]
  expect_equal(zeguo$age, 33)
  expect_true(is.na(europolis$age))
  expect_equal(frame$age[match(
    paste(source$dpnum, source$caseid), paste(frame$dpnum, frame$caseid)
  )], source$ppage)

  source$ppage[source$dpnum == 9 & source$caseid == 52125] <- 15
  expect_error(analysis_frame(source, scores))
})

test_that("matched models use identical common regressors", {
  source <- dplyr::filter(read_polardata(), pollname != "National Issues Convention")
  historical <- analysis_frame(
    source, item_scores_for_respondents(read_historical_items(), source)
  ) |>
    dplyr::mutate(historical_respondent_id = as.character(caseid)) |>
    dplyr::left_join(
      dplyr::select(read_respondent_sources(), poll_id, dpnum),
      by = "dpnum", relationship = "many-to-one"
    ) |>
    dplyr::filter(!poll_id %in% c("btp-presidential-primaries-2004", "nic-1996"))
  joined <- dplyr::inner_join(
    core_group_frame(attendee_panel()), historical,
    by = c("poll_id", "historical_respondent_id"),
    relationship = "one-to-one", suffix = c(".core", ".historical")
  )
  expect_equal(nrow(joined), nrow(historical))
  for (variable in c("k1", "k2", "group_size", "group_k1", "poll_k1", "p_female")) {
    expect_equal(
      joined[[paste0(variable, ".core")]],
      joined[[paste0(variable, ".historical")]], tolerance = 1e-10
    )
  }
})
