read_historical_items <- function(path = source_path("item_responses")) {
  bridge <- read_analysis_participants() |>
    dplyr::filter(source_dataset == "historical") |>
    dplyr::select("poll_id", "respondent_id", "historical_respondent_id")
  arrow::read_parquet(path) |>
    dplyr::filter(source_dataset == "historical") |>
    dplyr::left_join(
      bridge,
      by = c("poll_id", "respondent_id"),
      relationship = "many-to-one"
    )
}

item_scores_for_respondents <- function(items, polardata, root = dp_data_root()) {
  poll_ids <- read_respondent_sources(root) |>
    dplyr::select(poll_id, dpnum) |>
    dplyr::filter(dpnum %in% polardata$dpnum)
  scores <- items |>
    dplyr::inner_join(poll_ids, by = "poll_id", relationship = "many-to-one") |>
    dplyr::transmute(
      dpnum,
      caseid = as.numeric(historical_respondent_id),
      wave = as.integer(sub("^t", "", wave)), correct
    ) |>
    dplyr::inner_join(
      dplyr::distinct(polardata, dpnum, caseid),
      by = c("dpnum", "caseid"), relationship = "many-to-one"
    ) |>
    dplyr::summarise(score = sum(correct, na.rm = TRUE) / dplyr::n(), .by = c(dpnum, caseid, wave)) |>
    tidyr::pivot_wider(names_from = wave, values_from = score, names_prefix = "k")
  stopifnot(
    nrow(scores) == nrow(polardata),
    !anyDuplicated(scores[c("dpnum", "caseid")]),
    all(is.finite(scores$k1)), all(is.finite(scores$k2))
  )
  scores
}

t1_items_for_poll <- function(poll_id, dpnum, polardata, knowledge) {
  poll <- dplyr::filter(polardata, .data$dpnum == .env$dpnum)
  items <- knowledge |>
    dplyr::filter(.data$poll_id == .env$poll_id, wave == "t1") |>
    dplyr::transmute(
      caseid = as.numeric(historical_respondent_id),
      item = item_id, correct
    ) |>
    dplyr::filter(!is.na(caseid), caseid %in% poll$caseid)
  stopifnot(
    nrow(poll) > 0L, nrow(items) > 0L,
    !anyNA(items$caseid),
    !anyDuplicated(poll$caseid), !anyDuplicated(items[c("caseid", "item")]),
    all(items$correct[!is.na(items$correct)] %in% 0:1)
  )
  scores <- items |>
    dplyr::summarise(score = sum(correct, na.rm = TRUE) / dplyr::n(), n_items = dplyr::n(), .by = caseid)
  stopifnot(
    dplyr::n_distinct(scores$n_items) == 1L,
    all(
      abs(scores$score - poll$t1know[match(scores$caseid, poll$caseid)]) < 1e-6,
      na.rm = TRUE
    )
  )
  items |>
    dplyr::left_join(
      dplyr::transmute(poll, caseid, pollid, group = paste(pollid, pollgroup, sep = "_")),
      by = "caseid", relationship = "many-to-one"
    ) |>
    dplyr::select(pollid, caseid, group, item, correct) |>
    dplyr::arrange(caseid, item)
}

# For each person, the other group members' mean T1 correctness on the items
# the person answered incorrectly at T1. A perfect T1 score has zero missed
# items; the indicator distinguishes that case from zero peer correctness.
item_group_knowledge <- function(items) {
  items |>
    dplyr::mutate(
      others_correct = (sum(correct, na.rm = TRUE) - dplyr::coalesce(correct, 0L)) /
        (sum(!is.na(correct)) - !is.na(correct)),
      .by = c(group, item)
    ) |>
    dplyr::summarise(
      item_group_k1 = {
        missed <- others_correct[correct %in% 0L]
        if (length(missed) == 0L) {
          if (all(!is.na(correct) & correct == 1L)) 0 else NA_real_
        } else {
          value <- mean(missed, na.rm = TRUE)
          if (is.nan(value)) NA_real_ else value
        }
      },
      no_missed_items = as.integer(all(!is.na(correct) & correct == 1L)),
      .by = c(pollid, caseid, group)
    )
}

add_item_peer_measure <- function(frame, responses = read_analysis_responses()) {
  keys <- c("poll_id", "source_dataset", "respondent_id")
  measures <- responses |>
    dplyr::filter(wave == "t1") |>
    dplyr::inner_join(dplyr::select(frame, dplyr::all_of(keys), group),
      by = keys, relationship = "many-to-one"
    ) |>
    dplyr::transmute(pollid = poll_id, caseid = respondent_id,
      group, item = item_id, correct
    ) |>
    item_group_knowledge() |>
    dplyr::rename(poll_id = pollid, respondent_id = caseid,
      group_k1_items = item_group_k1
    )
  dplyr::left_join(frame, dplyr::select(measures, -group),
    by = c("poll_id", "respondent_id"), relationship = "one-to-one"
  )
}
