education_labels <- c("0" = "Below high school", "0.5" = "High school", "1" = "BA or more")

# One row per participant with the variables the models use. Recodes are
# documented in docs/recode_ledger.csv.
analysis_frame <- function(polardata) {
  nic <- polardata$pollname == "National Issues Convention"
  greece <- polardata$pollname == "Marousi, Greece"
  polardata |>
    dplyr::mutate(
      t2know = dplyr::if_else(greece & t2know == 0 & t1know > 0, NA_real_, t2know),
      educ3 = dplyr::if_else(greece & educ4 %in% 7, NA_real_, educ3),
      dplyr::across(c(t1know, t2know), \(x) round(x, 10)),
      mode = dplyr::if_else(nic, 0, mode),
      ppage = dplyr::if_else(nic, 1996 - ppage, ppage),
      ppage = dplyr::if_else(ppage < 16 | ppage > 100, NA_real_, ppage)
    ) |>
    dplyr::transmute(
      dpnum, pollid, pollname, caseid,
      group = paste(pollid, pollgroup, sep = "_"),
      online = mode,
      k1 = t1know,
      k2 = t2know,
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
      group_size = groupsize,
      read_briefing = readbrief
    ) |>
    # Salience: participants' mean T1 knowledge. The whole-sample mean in the
    # release is missing for Australia and equals this in half the polls.
    dplyr::mutate(poll_k1 = mean(k1, na.rm = TRUE), .by = pollid) |>
    assertr::assert(assertr::within_bounds(0, 1), k1, k2, group_k1, female, p_female) |>
    assertr::assert(assertr::in_set(0, 1), online) |>
    assertr::assert(assertr::within_bounds(16, 100), age) |>
    assertr::verify(dplyr::n_distinct(pollid) == 22)
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
