education_labels <- c("0" = "Below high school", "0.5" = "High school", "1" = "BA or more")

# Share of the other members of i's group with attribute x, and the same share
# among the other members of i's poll. Conditioning on the second removes the
# mechanical negative correlation between own value and leave-one-out peer means
# under random assignment (Guryan, Kroft and Notowidigdo 2009).
leave_one_out <- function(x, by) {
  total <- stats::ave(x, by, FUN = \(v) sum(v, na.rm = TRUE))
  count <- stats::ave(!is.na(x), by, FUN = sum)
  (total - dplyr::coalesce(x, 0)) / (count - !is.na(x))
}

assignment_check <- function(frame, covariates = c("k1", "female", "age", "education_ba")) {
  if ("education" %in% names(frame)) {
    frame <- dplyr::mutate(
      frame,
      education_ba = as.numeric(education == "BA or more")
    )
  }
  purrr::map(covariates, \(covariate) {
    polls <- frame |>
      dplyr::filter(!is.na(.data[[covariate]])) |>
      dplyr::mutate(
        own = .data[[covariate]],
        peer = leave_one_out(own, group),
        poll_others = leave_one_out(own, pollid)
      ) |>
      dplyr::filter(is.finite(peer), is.finite(poll_others))
    statistic <- function(x) {
      x$peer <- leave_one_out(x$own, x$group)
      x$poll_others <- leave_one_out(x$own, x$pollid)
      c(peer = stats::coef(stats::lm(own ~ peer + poll_others + factor(pollid), data = x))[["peer"]])
    }
    result <- bootstrap_stat(polls, statistic)
    tibble::tibble(
      covariate = covariate, estimate = result$estimate[[1]], std_error = result$se[[1]],
      lower = result$lower[[1]], upper = result$upper[[1]],
      n = nrow(polls), groups = dplyr::n_distinct(polls$group)
    )
  }) |>
    purrr::list_rbind()
}
