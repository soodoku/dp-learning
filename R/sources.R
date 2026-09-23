project_file <- function(...) {
  file.path(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")), ...)
}

source_manifest <- tibble::tribble(
  ~source, ~path, ~doi, ~md5, ~license,
  "distortions_responses", "data/raw/polardata.tab", "10.7910/DVN/D7G1LO",
  "8e2b8aa45f9ebb71d73d88e2ecf3dbdb", "CC0 1.0",
  "distortions_indices", "data/raw/poll_indices.tab", "10.7910/DVN/D7G1LO",
  "cb262ca053f88d0119757a5c1039ba18", "CC0 1.0",
  "cor_sood_replication", "data/raw/cor-sood-replication.zip", "10.7910/DVN/HZHVCU",
  "80b59acf267496b42f48756a1b3d9a5c", "CC0 1.0",
  "greece", "data/raw/greece.csv", NA,
  "adde068512852c8094af82f5920bf353", "CC0 1.0"
)

poll_map <- tibble::tibble(
  file_key = c(
    "aus", "btp04", "btp04GE", "btp05", "btp07", "bul", "ca", "cpl", "dk",
    "eu2007", "eu2009", "ire", "mi", "nic1", "sm", "swp", "ukbge", "ukcrime",
    "ukeu", "ukhealth", "ukmon", "vt", "wtu"
  ),
  cor_poll_name = c(
    "Australia Constitutional Referendum", "By the People 2004 Online Primaries",
    "By the People 2004 General Election", "By the People 2005", "By the People 2007",
    "Bulgaria", "California What's Next", "Central Power & Light", "Denmark: Euro",
    "Tomorrow's Europe (EU)", "Europolis", "Northern Ireland", "Michigan",
    "National Issues Convention", "San Mateo", "Southwestern Electric Power",
    "UK General Election", "UK Crime", "UK EU", "UK Health", "UK Monarchy",
    "Vermont Energy", "West Texas Utilities"
  ),
  dpnum = c(
    5L, NA, 15L, 18L, NA, 10L, NA, 8L, NA, 7L, 11L, NA, NA, 20L, 17L,
    21L, 4L, 6L, 1L, 2L, 3L, NA, 19L
  ),
  match_status = c(
    "poll_only_battery_mismatch", "cor_sood_only", "poll_only_count_mismatch",
    "validated_person_link", "cor_sood_only", "poll_only_row_order_mismatch",
    "cor_sood_only", "validated_person_link", "cor_sood_only",
    "poll_only_count_mismatch", "poll_only_row_order_mismatch", "cor_sood_only",
    "cor_sood_only", "poll_only_battery_mismatch", "poll_only_score_mismatch",
    "validated_person_link", "poll_only_score_mismatch", "validated_person_link",
    "poll_only_count_mismatch", "validated_person_link", "poll_only_battery_mismatch",
    "cor_sood_only", "validated_person_link"
  ),
  mismatch_reason = c(
    "Deposited battery has 10 items; polardata score uses a larger battery.",
    "Poll is absent from polardata.",
    "Item file has 250 rows; polardata has 246 retained rows.", "",
    "Poll is absent from polardata.",
    "Battery and score distribution agree, but public row order does not.",
    "Poll is absent from polardata.", "", "Poll is absent from polardata.",
    "Item file has 335 rows; polardata has 344 retained rows.",
    "Battery and score distribution agree, but public row order does not.",
    "Poll is absent from polardata.", "Poll is absent from polardata.",
    "Deposited battery has 8 items; polardata score uses 11 items.",
    "T1 reconstructs exactly; T2 scores differ for some rows.", "",
    "T1 reconstructs exactly; T2 scores differ slightly for some rows.", "",
    "Item file has 224 rows; polardata has 238 retained rows.", "",
    "Deposited battery has 8 items; polardata score uses 9 items.",
    "Poll is absent from polardata.", ""
  )
)

missing_dp_polls <- tibble::tibble(
  file_key = NA_character_,
  cor_poll_name = NA_character_,
  dpnum = c(9L, 12L, 13L, 14L, 16L),
  match_status = "distortions_only",
  mismatch_reason = "No matching public item matrix was found in the Cor-Sood deposit."
)

verify_sources <- function(manifest = source_manifest) {
  hashes <- manifest |>
    dplyr::mutate(
      observed_md5 = unname(tools::md5sum(project_file(manifest$path)))
    )
  assertr::verify(
    hashes,
    all(hashes$md5 == hashes$observed_md5),
    error_fun = assertr::error_stop
  )
  invisible(TRUE)
}

read_polardata <- function(path = project_file("data", "raw", "polardata.tab")) {
  readr::read_tsv(path, show_col_types = FALSE) |>
    dplyr::distinct(dplyr::across(-X), .keep_all = TRUE)
}

# Marousi, Greece (2006) was withheld from the public polardata release; these
# rows come from the authors' 2014 analysis file (see data/raw/README.md).
read_greece <- function(path = project_file("data", "raw", "greece.csv")) {
  readr::read_csv(path, show_col_types = FALSE)
}

extract_cor_data <- function(
  archive = project_file("data", "raw", "cor-sood-replication.zip"),
  target = project_file("build", "cor-sood")
) {
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  outer <- file.path(target, "outer")
  dir.create(outer, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(archive, files = "replication/data.zip", exdir = outer, overwrite = TRUE)
  data_zip <- file.path(outer, "replication", "data.zip")
  data_dir <- file.path(target, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(data_zip, exdir = data_dir, overwrite = TRUE)
  file.path(data_dir, "data")
}

read_battery <- function(path) {
  data <- readr::read_csv(path, na = c("", "NA"), show_col_types = FALSE)
  item_names <- setdiff(names(data), "female")
  tibble::tibble(item_count = length(item_names)) |>
    assertr::verify(item_count %% 2L == 0L)
  item_count <- length(item_names) / 2L
  t1_names <- item_names[seq_len(item_count)]
  t2_names <- item_names[item_count + seq_len(item_count)]
  score_items <- function(names) {
    data |>
      dplyr::select(dplyr::all_of(names)) |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), ~ tidyr::replace_na(as.numeric(.x), 0))
      )
  }
  list(
    t1 = score_items(t1_names),
    t2 = score_items(t2_names),
    t1_names = t1_names,
    t2_names = t2_names,
    female = as.numeric(data$female)
  )
}

battery_summary <- function(file_key, data_dir) {
  battery <- read_battery(file.path(data_dir, paste0(file_key, ".csv")))
  tibble::tibble(
    file_key = file_key,
    respondents = nrow(battery$t1),
    items = ncol(battery$t1),
    alpha_t1 = ltm::cronbach.alpha(battery$t1)$alpha,
    alpha_t2 = ltm::cronbach.alpha(battery$t2)$alpha,
    score_correlation = stats::cor(rowMeans(battery$t1), rowMeans(battery$t2))
  )
}

poll_inventory <- function(polardata) {
  polardata |>
    dplyr::distinct(dpnum, pollid, pollname, numitems) |>
    dplyr::left_join(
      dplyr::count(polardata, dpnum, name = "polardata_respondents"),
      by = "dpnum",
      relationship = "one-to-one"
    )
}

make_crosswalk <- function(polardata, reliability) {
  dplyr::bind_rows(poll_map, missing_dp_polls) |>
    dplyr::left_join(
      dplyr::select(reliability, -cor_poll_name, -dpnum),
      by = "file_key",
      relationship = "many-to-one",
      na_matches = "never"
    ) |>
    dplyr::left_join(
      poll_inventory(polardata),
      by = "dpnum",
      relationship = "many-to-one",
      na_matches = "never"
    ) |>
    dplyr::arrange(is.na(dpnum), dpnum, file_key)
}

validate_person_link <- function(polardata, battery, dpnum, tolerance = 1e-7) {
  poll <- dplyr::filter(polardata, .data$dpnum == .env$dpnum)
  t1_score <- rowMeans(battery$t1)
  t2_score <- rowMeans(battery$t2)
  observed_gender <- !is.na(battery$female) & !is.na(poll$female)
  checks <- tibble::tibble(
    same_rows = nrow(poll) == nrow(battery$t1),
    same_t1 = length(t1_score) == nrow(poll) &&
      all(abs(t1_score - poll$t1know) <= tolerance),
    same_t2 = length(t2_score) == nrow(poll) &&
      all(abs(t2_score - poll$t2know) <= tolerance),
    same_gender = all(battery$female[observed_gender] == poll$female[observed_gender])
  )
  assertr::verify(
    checks,
    all(as.matrix(checks)),
    error_fun = assertr::error_stop
  )
  poll
}

linked_items_for_poll <- function(file_key, dpnum, polardata, data_dir) {
  battery <- read_battery(file.path(data_dir, paste0(file_key, ".csv")))
  poll <- validate_person_link(polardata, battery, dpnum)
  make_wave <- function(items, wave) {
    items |>
      dplyr::mutate(
        source_row = dplyr::row_number(),
        caseid = poll$caseid,
        female = poll$female
      ) |>
      tidyr::pivot_longer(
        cols = -c(source_row, caseid, female),
        names_to = "source_variable",
        values_to = "correct"
      ) |>
      dplyr::mutate(
        dpnum = dpnum,
        poll_name = poll$pollname[[1]],
        item_id = paste0("item_", match(source_variable, names(items))),
        wave = wave,
        linkage_basis = "validated_public_row_position",
        .before = 1
      )
  }
  dplyr::bind_rows(make_wave(battery$t1, "t1"), make_wave(battery$t2, "t2"))
}
