project_file <- function(...) {
  file.path(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")), ...)
}

dp_data_root <- function() {
  Sys.getenv("DP_DATA_ROOT", unset = project_file("..", "dp-data"))
}

upstream_source_ids <- c(
  distortions_responses = "polardata_tab",
  historical_items = "historical_knowledge_items_parquet",
  briefing_reading = "briefing_reading_parquet",
  knowledge_scores = "knowledge_scores_parquet",
  cor_sood_replication = "cor-sood-replication",
  greece = "dp-learning-greece",
  a1r = "a1r-2019",
  tanzania = "tanzania-2015",
  climate = "a1r-climate-2021",
  amr = "amr-2024"
)

upstream_source_manifest <- function(root = dp_data_root()) {
  raw <- readr::read_csv(file.path(root, "metadata", "source_files.csv"), show_col_types = FALSE) |>
    dplyr::transmute(id = source_id, path, sha256)
  generated <- c("output/manifest.csv", "output/polardata/manifest.csv", "output/respondent/manifest.csv") |>
    purrr::map(\(path) readr::read_csv(file.path(root, path), show_col_types = FALSE)) |>
    purrr::list_rbind() |>
    dplyr::transmute(id = paste(table, tools::file_ext(path), sep = "_"), path, sha256)
  catalog <- dplyr::bind_rows(raw, generated)
  selected <- catalog[match(unname(upstream_source_ids), catalog$id), c("path", "sha256")]
  if (anyNA(selected$path) || anyDuplicated(catalog$id)) {
    stop("Missing or duplicate entries in dp-data source manifests.")
  }
  dplyr::mutate(selected, source = names(upstream_source_ids), .before = 1)
}

source_path <- function(source, manifest = upstream_source_manifest(), root = dp_data_root()) {
  entry <- manifest[manifest$source == source, ]
  if (nrow(entry) != 1L) stop("Expected exactly one source entry: ", source)
  file.path(root, entry$path)
}

read_poll_registry <- function(root = dp_data_root()) {
  readr::read_csv(file.path(root, "metadata", "polls.csv"), show_col_types = FALSE)
}

read_poll_aliases <- function(root = dp_data_root()) {
  readr::read_csv(file.path(root, "metadata", "poll_aliases.csv"), show_col_types = FALSE)
}

read_respondent_sources <- function(root = dp_data_root()) {
  readr::read_csv(file.path(root, "metadata", "respondent_sources.csv"), show_col_types = FALSE)
}

read_briefing_scores <- function(path = source_path("briefing_reading"), root = dp_data_root()) {
  out <- arrow::read_parquet(path) |>
    dplyr::left_join(
      dplyr::select(read_respondent_sources(root), "poll_id", "dpnum"),
      by = "poll_id", relationship = "many-to-one"
    ) |>
    dplyr::filter(!is.na(.data$historical_respondent_id)) |>
    dplyr::transmute(
      dpnum, caseid = as.numeric(.data$historical_respondent_id),
      read_briefing = reading_score
    )
  stopifnot(!anyNA(out$dpnum), !anyNA(out$caseid), !anyDuplicated(out[c("dpnum", "caseid")]))
  out
}

cor_poll_map <- function(root = dp_data_root()) {
  aliases <- read_poll_aliases(root) |>
    dplyr::filter(system == "cor-sood-file") |>
    dplyr::transmute(poll_id, file_key = alias)
  out <- dplyr::left_join(
    aliases, dplyr::select(read_poll_registry(root), poll_id, cor_poll_name = title),
    by = "poll_id", relationship = "one-to-one"
  )
  stopifnot(nrow(out) == 23L, !anyNA(out$cor_poll_name), !anyDuplicated(out$file_key))
  out
}

appendix_polls <- function(frame, item_scores, historical_items, root = dp_data_root()) {
  aliases <- read_poll_aliases(root) |>
    dplyr::filter(system == "legacy-pollid") |>
    dplyr::transmute(poll_id, pollid = as.numeric(alias))
  participant_ids <- frame |>
    dplyr::distinct(pollid) |>
    dplyr::left_join(aliases, by = "pollid", relationship = "one-to-one")
  stopifnot(!anyNA(participant_ids$poll_id))
  item_ids <- union(unique(item_scores$poll_id), unique(historical_items$poll_id))
  out <- read_poll_registry(root) |>
    dplyr::filter(poll_id %in% union(participant_ids$poll_id, item_ids)) |>
    dplyr::mutate(
      participants = poll_id %in% participant_ids$poll_id,
      items = poll_id %in% item_ids,
      data = dplyr::case_when(
        participants & items ~ "P+I", participants ~ "P", items ~ "I"
      ),
      mode = dplyr::recode(mode, "face-to-face" = "Face to face", online = "Online")
    ) |>
    dplyr::arrange(year, title) |>
    dplyr::transmute(poll = title, year, topic, mode, data)
  stopifnot(nrow(out) == length(union(participant_ids$poll_id, item_ids)))
  out
}

verify_sources <- function(manifest = upstream_source_manifest(), root = dp_data_root()) {
  paths <- file.path(root, manifest$path)
  missing <- !file.exists(paths)
  if (any(missing)) {
    stop(
      "Missing upstream source: ", paste(paths[missing], collapse = ", "),
      ". See README.md for dp-data setup."
    )
  }
  observed <- vapply(paths, function(path) {
    digest::digest(file = path, algo = "sha256")
  }, character(1L), USE.NAMES = FALSE)
  changed <- observed != manifest$sha256
  if (any(changed)) {
    stop(
      "Source checksum mismatch: ", paste(manifest$source[changed], collapse = ", "),
      ". Rebuild dp-data or investigate its source manifest."
    )
  }
  invisible(TRUE)
}

read_polardata <- function(path = source_path("distortions_responses")) {
  readr::read_tsv(path, show_col_types = FALSE) |>
    dplyr::distinct(dplyr::across(-X), .keep_all = TRUE)
}

# Marousi, Greece (2006) was withheld from the public polardata release; these
# rows come from the authors' 2014 analysis file (see README.md).
read_greece <- function(path = source_path("greece")) {
  readr::read_csv(path, show_col_types = FALSE)
}

extract_cor_data <- function(
  archive = source_path("cor_sood_replication"),
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
