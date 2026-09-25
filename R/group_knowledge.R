# Reviewed baseline batteries with identified upstream respondent links.
t1_linked_polls <- tibble::tribble(
  ~poll_id, ~dpnum,
  "uk-health-1998", 2L,
  "uk-crime-1994", 6L,
  "cpl-1996", 8L,
  "btp-health-education-2005", 18L,
  "wtu-1996", 19L,
  "swepco-1996", 21L,
  "uk-general-election-1997", 4L,
  "san-mateo-2008", 17L
)

read_respondent_knowledge <- function(path = source_path("respondent_knowledge")) {
  arrow::read_parquet(path)
}

t1_items_for_poll <- function(poll_id, dpnum, polardata, knowledge) {
  poll <- dplyr::filter(polardata, .data$dpnum == .env$dpnum)
  items <- knowledge |>
    dplyr::filter(.data$poll_id == .env$poll_id, wave == 1L) |>
    dplyr::transmute(
      caseid = as.numeric(historical_respondent_id),
      item = item_id, correct = correct_zero_filled
    )
  stopifnot(
    nrow(poll) > 0L, nrow(items) > 0L,
    !anyNA(items$caseid), !anyNA(items$correct),
    !anyDuplicated(poll$caseid), !anyDuplicated(items[c("caseid", "item")]),
    setequal(items$caseid, poll$caseid), all(items$correct %in% 0:1)
  )
  scores <- items |>
    dplyr::summarise(score = mean(correct), n_items = dplyr::n(), .by = caseid)
  stopifnot(
    dplyr::n_distinct(scores$n_items) == 1L,
    all(abs(scores$score - poll$t1know[match(scores$caseid, poll$caseid)]) < 1e-7)
  )
  items |>
    dplyr::left_join(
      dplyr::transmute(poll, caseid, pollid, group = paste(pollid, pollgroup, sep = "_")),
      by = "caseid", relationship = "many-to-one", unmatched = "error"
    ) |>
    dplyr::select(pollid, caseid, group, item, correct) |>
    dplyr::arrange(caseid, item)
}

a1r_t1_items <- function(path) {
  data <- readr::read_tsv(path, show_col_types = FALSE) |>
    dplyr::filter(CONDITION == 1, POST == 1)
  purrr::map2(paste0("PK", 1:7), a1r_key, \(item, answer) {
    tibble::tibble(
      pollid = "a1r2019",
      caseid = seq_len(nrow(data)),
      group = paste0("a1r_", data$GROUP),
      item = item,
      correct = dplyr::coalesce(as.numeric(data[[item]] == answer), 0)
    )
  }) |>
    purrr::list_rbind()
}

# For each person, the other group members' mean T1 correctness on the items
# the person answered incorrectly at T1. People
# who answered every item correctly have no such items and get NA.
item_group_knowledge <- function(items) {
  items |>
    dplyr::mutate(
      others_correct = (sum(correct) - correct) / (dplyr::n() - 1),
      .by = c(group, item)
    ) |>
    dplyr::summarise(
      item_group_k1 = if (any(correct == 0)) mean(others_correct[correct == 0]) else NA_real_,
      .by = c(pollid, caseid, group)
    )
}
