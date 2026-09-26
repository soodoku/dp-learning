education_labels <- c("0" = "Below high school", "0.5" = "High school", "1" = "BA or more")

# One row per participant with the variables the models use. Knowledge scores
# come from respondent-level item responses; upstream corrections are in dp-data.
analysis_frame <- function(polardata, item_scores) {
  polardata <- dplyr::left_join(
    polardata,
    dplyr::rename(item_scores, item_k1 = k1, item_k2 = k2),
    by = c("dpnum", "caseid"), relationship = "one-to-one"
  )
  stopifnot(
    all(is.finite(polardata$item_k1)), all(is.finite(polardata$item_k2)),
    all(abs(polardata$item_k1 - polardata$t1know) < 1e-6),
    all(abs(polardata$item_k2 - polardata$t2know) < 1e-6)
  )
  polardata |>
    dplyr::mutate(
      ppage = dplyr::if_else(ppage < 16 | ppage > 100, NA_real_, ppage)
    ) |>
    dplyr::transmute(
      dpnum, pollid, pollname, caseid,
      group = paste(pollid, pollgroup, sep = "_"),
      online = mode,
      k1 = item_k1,
      k2 = item_k2,
      group_k1 = meant1know_ind,
      education = factor(
        education_labels[as.character(educ3)],
        levels = education_labels
      ),
      age = ppage,
      female,
      minority,
      p_female = pfemale,
      p_minority = pminority,
      extremity = attextreme,
      heterogeneity = genvar,
      group_size = groupsize
    ) |>
    # Salience: participants' mean T1 knowledge. The whole-sample mean in the
    # release is missing for Australia and equals this in half the polls.
    dplyr::mutate(poll_k1 = mean(k1, na.rm = TRUE), .by = pollid) |>
    assertr::assert(assertr::within_bounds(0, 1), k1, k2, group_k1, female, p_female) |>
    assertr::assert(assertr::in_set(0, 1), online) |>
    assertr::assert(assertr::within_bounds(16, 100), age) |>
    assertr::verify(dplyr::n_distinct(pollid) == dplyr::n_distinct(polardata$pollid))
}

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
  frame <- dplyr::mutate(frame, education_ba = as.numeric(education == "BA or more"))
  purrr::map(covariates, \(covariate) {
    polls <- frame |>
      dplyr::filter(!is.na(.data[[covariate]])) |>
      dplyr::mutate(
        own = .data[[covariate]],
        peer = leave_one_out(own, group),
        poll_others = leave_one_out(own, pollid)
      ) |>
      dplyr::filter(is.finite(peer), is.finite(poll_others))
    fit <- stats::lm(own ~ peer + poll_others + factor(pollid), data = polls)
    vc <- sandwich::vcovCL(fit, cluster = ~group)
    tibble::tibble(
      covariate = covariate,
      estimate = stats::coef(fit)[["peer"]],
      std_error = sqrt(vc["peer", "peer"]),
      n = stats::nobs(fit),
      groups = dplyr::n_distinct(polls$group)
    )
  }) |>
    purrr::list_rbind()
}
