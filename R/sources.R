project_file <- function(...) {
  file.path(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")), ...)
}

dp_data_root <- function() {
  Sys.getenv("DP_DATA_ROOT", unset = project_file("..", "dp-data"))
}

upstream_source_ids <- c(
  distortions_responses = "polardata_parquet",
  briefing_reading = "briefing_reading_parquet",
  polls = "analysis_polls_parquet",
  items = "analysis_items_parquet",
  participants = "analysis_participants_parquet",
  item_responses = "analysis_item_responses_parquet",
  scores = "analysis_scores_parquet",
  attitudes = "analysis_attitude_responses_parquet"
)

upstream_source_manifest <- function(root = dp_data_root()) {
  catalog <- c(
    "output/polardata/manifest.csv", "output/respondent/manifest.csv",
    "output/analysis/manifest.csv"
  ) |>
    purrr::map(\(path) readr::read_csv(file.path(root, path), show_col_types = FALSE)) |>
    purrr::list_rbind() |>
    dplyr::transmute(id = paste(table, tools::file_ext(path), sep = "_"), path, sha256)
  selected <- catalog[match(unname(upstream_source_ids), catalog$id), c("path", "sha256")]
  if (anyNA(selected$path) || anyDuplicated(catalog$id)) {
    stop("Missing or duplicate entries in dp-data source manifests.")
  }
  dplyr::mutate(selected, source = names(upstream_source_ids), .before = 1)
}

source_path <- function(source, manifest = upstream_source_manifest(root), root = dp_data_root()) {
  entry <- manifest[manifest$source == source, ]
  if (nrow(entry) != 1L) stop("Expected exactly one source entry: ", source)
  file.path(root, entry$path)
}

read_poll_registry <- function(root = dp_data_root()) {
  arrow::read_parquet(source_path("polls", root = root))
}

read_item_catalog <- function(root = dp_data_root()) {
  arrow::read_parquet(source_path("items", root = root))
}

read_analysis_participants <- function(root = dp_data_root()) {
  arrow::read_parquet(source_path("participants", root = root))
}

read_analysis_responses <- function(root = dp_data_root()) {
  arrow::read_parquet(source_path("item_responses", root = root))
}

read_analysis_scores <- function(root = dp_data_root()) {
  arrow::read_parquet(source_path("scores", root = root))
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
      dpnum,
      caseid = as.numeric(.data$historical_respondent_id),
      read_briefing = reading_score
    )
  stopifnot(!anyNA(out$dpnum), !anyNA(out$caseid), !anyDuplicated(out[c("dpnum", "caseid")]))
  out
}

appendix_polls <- function(
  root = dp_data_root(),
  group_ids = NULL
) {
  participants <- read_analysis_participants(root)
  item_ids <- unique(read_analysis_responses(root)$poll_id)
  registry <- read_poll_registry(root)
  if (is.null(group_ids)) {
    panel <- attendee_panel(
      participants, read_analysis_scores(root), registry
    )
    group_ids <- unique(core_group_frame(panel)$poll_id)
  }
  control_ids <- unique(participants$poll_id[participants$source_dataset == "control"])
  out <- registry |>
    dplyr::filter(poll_id %in% item_ids, poll_id %in% group_ids) |>
    dplyr::mutate(
      control_group = poll_id %in% control_ids,
      mode = dplyr::recode(mode, "face-to-face" = "Face to face", online = "Online")
    ) |>
    dplyr::arrange(year, title) |>
    dplyr::transmute(
      poll_id, poll = title, year, topic, mode, control_group
    )
  stopifnot(nrow(out) == length(intersect(item_ids, group_ids)))
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
  arrow::read_parquet(path)
}

read_analysis_attitudes <- function(root = dp_data_root()) {
  arrow::read_parquet(source_path("attitudes", root = root))
}
