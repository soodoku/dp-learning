# Polls whose Cor-Sood item file links to polardata row for row at T1.
t1_linked_polls <- tibble::tribble(
  ~file_key, ~dpnum,
  "ukhealth", 2L, "ukcrime", 6L, "cpl", 8L, "btp05", 18L, "wtu", 19L, "swp", 21L,
  "ukbge", 4L, "sm", 17L
)

# Item-level T1 correctness linked to small groups.
t1_items_for_poll <- function(file_key, dpnum, polardata, data_dir) {
  battery <- read_battery(file.path(data_dir, paste0(file_key, ".csv")))
  poll <- dplyr::filter(polardata, .data$dpnum == .env$dpnum)
  observed <- !is.na(battery$female) & !is.na(poll$female)
  stopifnot(
    nrow(poll) == nrow(battery$t1),
    all(abs(rowMeans(battery$t1) - poll$t1know) < 1e-7),
    all(battery$female[observed] == poll$female[observed])
  )
  battery$t1 |>
    dplyr::mutate(pollid = poll$pollid, caseid = poll$caseid, group = paste(poll$pollid, poll$pollgroup, sep = "_")) |>
    tidyr::pivot_longer(-c(pollid, caseid, group), names_to = "item", values_to = "correct")
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
