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

test_that("historical canonical scores agree with polardata including absent waves", {
  people <- read_analysis_participants() |>
    dplyr::filter(source_dataset == "historical") |>
    dplyr::select(poll_id, source_dataset, respondent_id, historical_respondent_id)
  scores <- read_analysis_scores() |>
    dplyr::filter(source_dataset == "historical", wave %in% c("t1", "t2")) |>
    dplyr::inner_join(people,
      by = c("poll_id", "source_dataset", "respondent_id"), relationship = "many-to-one"
    ) |>
    dplyr::filter(!is.na(historical_respondent_id)) |>
    dplyr::select(poll_id, historical_respondent_id, wave, score) |>
    tidyr::pivot_wider(names_from = wave, values_from = score)
  legacy <- read_polardata() |>
    dplyr::filter(pollname != "National Issues Convention") |>
    dplyr::mutate(historical_respondent_id = as.character(caseid)) |>
    dplyr::left_join(dplyr::select(read_respondent_sources(), poll_id, dpnum),
      by = "dpnum", relationship = "many-to-one"
    )
  joined <- dplyr::left_join(legacy, scores,
    by = c("poll_id", "historical_respondent_id"), relationship = "one-to-one"
  )
  expect_equal(nrow(joined), nrow(legacy))
  expect_equal(is.na(joined$t1), is.na(joined$t1know))
  expect_equal(is.na(joined$t2), is.na(joined$t2know))
  expect_equal(joined$t1, joined$t1know, tolerance = 1e-6)
  expect_equal(joined$t2, joined$t2know, tolerance = 1e-6)
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
  expect_equal(nrow(panel), 10259L)
  expect_equal(dplyr::n_distinct(panel$poll_id), 30L)
  expect_equal(dplyr::n_distinct(panel$poll_id[!is.na(panel$group)]), 27L)
  expect_equal(nrow(core_group_frame(panel)), 8469L)
  nic <- dplyr::filter(panel, poll_id == "nic-1996")
  expect_equal(nrow(nic), 456L)
  historical_nic <- read_analysis_participants() |>
    dplyr::filter(poll_id == "nic-1996", source_dataset == "historical", attended %in% TRUE)
  expect_equal(nrow(historical_nic), 466L)
  expect_setequal(setdiff(historical_nic$respondent_id, nic$respondent_id), c(
    "cdd-nic-1996-survey:source-row-1", "10000400", "10000460", "10004670",
    "10007580", "10007590", "10011680", "10012790", "10014282", "10014650"
  ))
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

test_that("unavailable exit questionnaires are retained upstream and excluded from paired gains", {
  expected <- tibble::tibble(
    poll_id = c(rep("uk-health-1998", 2), rep("tomorrows-europe-2007", 9)),
    respondent_id = c("3809", "4307", "3522", "3495", "2824", "625", "516", "693", "374", "225", "148")
  )
  people <- read_analysis_participants() |>
    dplyr::filter(source_dataset == "historical") |>
    dplyr::inner_join(expected, by = c("poll_id", "respondent_id"), relationship = "one-to-one")
  expect_equal(nrow(people), 11L)
  exit <- read_analysis_scores() |>
    dplyr::filter(source_dataset == "historical", wave == "t2") |>
    dplyr::inner_join(expected, by = c("poll_id", "respondent_id"), relationship = "one-to-one")
  expect_equal(nrow(exit), 11L)
  expect_true(all(is.na(exit$score)))
  expect_true(all(is.na(exit$n_observed) | exit$n_observed == 0))
  health_exit <- dplyr::filter(exit, poll_id == "uk-health-1998")
  expect_true(all(health_exit$n_observed == 0))
  paired <- attendee_panel() |>
    dplyr::inner_join(expected, by = c("poll_id", "respondent_id"))
  expect_equal(nrow(paired), 0L)
  health <- dplyr::filter(people, poll_id == "uk-health-1998")
  expect_true(all(health$attended))
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
  panel <- attendee_panel()
  upstream <- dplyr::select(read_analysis_participants(), poll_id, source_dataset, respondent_id, age)
  joined <- dplyr::left_join(panel, upstream,
    by = c("poll_id", "source_dataset", "respondent_id"),
    relationship = "one-to-one", suffix = c(".reader", ".upstream")
  )
  expect_equal(nrow(joined), nrow(panel))
  expect_equal(joined$age.reader, joined$age.upstream)
  zeguo <- dplyr::filter(panel, poll_id == "zeguo-2005", historical_respondent_id == "52125")
  europolis <- dplyr::filter(panel, poll_id == "europolis-2009", historical_respondent_id == "71300005619")
  expect_equal(nrow(zeguo), 1L)
  expect_equal(zeguo$age, 33)
  expect_equal(nrow(europolis), 1L)
  expect_true(is.na(europolis$age))
})

test_that("group regressors use exactly the corrected eligible attendees", {
  panel <- attendee_panel()
  actual <- core_group_frame(panel)
  eligible <- panel[!is.na(panel$group), ]
  sizes <- table(eligible$group)
  eligible <- eligible[eligible$group %in% names(sizes)[sizes > 1L], ]
  key <- function(data) paste(data$poll_id, data$respondent_id, sep = ":")
  expect_setequal(key(actual), key(eligible))
  eligible <- eligible[match(key(actual), key(eligible)), ]
  expect_equal(actual$k1, eligible$k1)
  expect_equal(actual$k2, eligible$k2)
  expected <- vapply(seq_len(nrow(eligible)), function(index) {
    peers <- eligible$group == eligible$group[index]
    peers[index] <- FALSE
    women <- eligible$female[peers]
    c(
      group_size = sum(peers) + 1L,
      group_k1 = mean(eligible$k1[peers]),
      poll_k1 = mean(eligible$k1[eligible$poll_id == eligible$poll_id[index]]),
      p_female = if (all(is.na(women))) NA_real_ else mean(women, na.rm = TRUE)
    )
  }, numeric(4))
  for (variable in rownames(expected)) {
    expect_equal(actual[[variable]], unname(expected[variable, ]), tolerance = 1e-10)
  }
})


test_that("California's absent telephone forms do not become zero baselines", {
  ids <- as.character(c(599, 647, 711, 722, 724, 729, 872, 882, 887, 888))
  people <- read_analysis_participants() |>
    dplyr::filter(poll_id == "california-whats-next-2011", source_dataset == "cor_sood")
  expect_equal(nrow(people), 396L)
  expect_setequal(people$respondent_id[!people$panel], ids)
  scores <- read_analysis_phase_scores() |>
    dplyr::filter(
      poll_id == "california-whats-next-2011", source_dataset == "cor_sood",
      respondent_id %in% ids
    )
  baseline <- dplyr::filter(scores, wave == "t0")
  expect_equal(nrow(baseline), 10L)
  expect_true(all(!baseline$wave_observed))
  expect_true(all(is.na(baseline$score)))
  later <- dplyr::filter(scores, wave %in% c("t1", "t2"), n_items == 5L)
  expect_equal(nrow(later), 20L)
  expect_true(all(later$wave_observed))
  expect_true(all(is.finite(later$score)))
  panel <- attendee_panel() |>
    dplyr::filter(poll_id == "california-whats-next-2011")
  expect_equal(nrow(panel), 386L)
  expect_false(any(panel$respondent_id %in% ids))
})


test_that("approved attendance, exit absence and unknown groups define model eligibility", {
  people <- read_phase_participants()
  selected <- attendee_panel()
  grouped <- core_group_frame(selected)
  new_haven <- dplyr::filter(people,
    poll_id == "new-haven-2004", source_dataset == "historical", respondent_id == "3124"
  )
  expect_equal(nrow(new_haven), 1L)
  expect_true(new_haven$attended)
  expect_false(new_haven$panel)
  departure <- read_analysis_phase_scores() |>
    dplyr::filter(
      poll_id == "new-haven-2004", source_dataset == "historical",
      respondent_id == "3124", wave == "t2"
    )
  expect_equal(nrow(departure), 1L)
  expect_false(departure$wave_observed)
  expect_true(is.na(departure$score))
  expect_false(any(selected$poll_id == "new-haven-2004" & selected$respondent_id == "3124"))

  national <- dplyr::filter(people,
    poll_id == "btp-national-2003", source_dataset == "historical", respondent_id == "134"
  )
  expect_equal(nrow(national), 1L)
  expect_false(national$attended)
  expect_true(national$panel)
  expect_false(any(selected$poll_id == "btp-national-2003" & selected$respondent_id == "134"))

  unknown_ids <- c("1008", "3132", "4316", "5022")
  unknown_groups <- dplyr::filter(selected, poll_id == "uk-eu-1995", respondent_id %in% unknown_ids)
  expect_setequal(unknown_groups$respondent_id, unknown_ids)
  expect_true(all(is.na(unknown_groups$group)))
  expect_true(all(is.finite(unknown_groups$k1) & is.finite(unknown_groups$k2)))
  expect_false(any(grouped$poll_id == "uk-eu-1995" & grouped$respondent_id %in% unknown_ids))
  demographic <- grouped[model_complete_cases(grouped, demographic_formula), ]
  expect_equal(nrow(demographic), 8353L)
  expect_equal(dplyr::n_distinct(demographic$group), 622L)
})
