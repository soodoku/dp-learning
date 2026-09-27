paired_item_answers <- function(responses, poll_id) {
  data <- dplyr::filter(responses, .data$poll_id == .env$poll_id) |>
    dplyr::mutate(correct = dplyr::coalesce(correct, 0L))
  wide <- function(wave_name) {
    data |>
      dplyr::filter(wave == wave_name) |>
      dplyr::select(respondent_id, item_id, correct) |>
      tidyr::pivot_wider(names_from = item_id, values_from = correct) |>
      dplyr::arrange(respondent_id)
  }
  pre <- wide("t1")
  post <- wide("t2")
  stopifnot(identical(pre$respondent_id, post$respondent_id))
  item_ids <- sort(setdiff(names(pre), "respondent_id"))
  stopifnot(identical(sort(setdiff(names(post), "respondent_id")), item_ids))
  list(
    pre = as.data.frame(pre[item_ids]),
    post = as.data.frame(post[item_ids])
  )
}

guessing_poll_gain <- function(poll_id, responses, respondents, k1_sd,
                               n_resamples = 200L, seed = 20260926L) {
  answers <- paired_item_answers(responses, poll_id)
  stopifnot(nrow(answers$pre) == respondents, ncol(answers$pre) >= 2L)
  fit_gain <- function(rows) {
    starts <- list(
      NULL,
      c(gg = 0.3, gk = 0.3, kk = 0.4, gamma = 0.25),
      c(gg = 0.5, gk = 0.2, kk = 0.3, gamma = 0.30),
      c(gg = 0.2, gk = 0.4, kk = 0.4, gamma = 0.20)
    )
    for (start in starts) {
      fit <- tryCatch(
        suppressWarnings(guess::fit_item_lca(
          answers$pre[rows, , drop = FALSE],
          answers$post[rows, , drop = FALSE],
          na_as = "missing", start = start
        )),
        error = \(e) NULL
      )
      if (!is.null(fit) && all(fit$diagnostics$convergence == 0L)) {
        return(mean(fit$learning))
      }
    }
    stop("Guessing model did not converge for ", poll_id)
  }
  set.seed(seed)
  adjusted <- fit_gain(seq_len(respondents))
  draws <- vapply(seq_len(n_resamples), \(i) {
    fit_gain(sample.int(respondents, replace = TRUE))
  }, numeric(1))
  adjusted_se <- stats::sd(draws)
  tibble::tibble(
    poll_id, respondents, items = ncol(answers$pre),
    adjusted, adjusted_se,
    adjusted_sd = adjusted / k1_sd,
    adjusted_sd_se = adjusted_se / k1_sd
  )
}

guessing_adjusted_gains <- function(panel, source_responses, gains,
                                    n_resamples = 200L, reuse = NULL) {
  responses <- source_responses |>
    dplyr::filter(wave %in% c("t1", "t2")) |>
    dplyr::inner_join(
      dplyr::select(panel, "poll_id", "source_dataset", "respondent_id"),
      by = c("poll_id", "source_dataset", "respondent_id"),
      relationship = "many-to-one"
    ) |>
    dplyr::select(poll_id, respondent_id, wave, item_id, correct)
  stopifnot(!anyDuplicated(responses[c("poll_id", "respondent_id", "wave", "item_id")]))
  retained <- tibble::tibble(poll_id = character())
  if (!is.null(reuse)) {
    retained <- reuse |>
      dplyr::select(-"pollname") |>
      dplyr::inner_join(
        dplyr::select(gains, "poll_id", new_n = "respondents", "k1_sd"),
        by = "poll_id", relationship = "one-to-one"
      ) |>
      dplyr::filter(
        respondents == new_n,
        abs(adjusted / adjusted_sd - k1_sd) < 1e-10
      ) |>
      dplyr::select(-"new_n", -"k1_sd")
  }
  fit_gains <- dplyr::anti_join(gains, retained, by = "poll_id")
  computed <- purrr::pmap(
    dplyr::arrange(dplyr::select(fit_gains, poll_id, respondents, k1_sd), poll_id),
    \(poll_id, respondents, k1_sd) {
      guessing_poll_gain(
        poll_id, responses, respondents, k1_sd,
        n_resamples = n_resamples,
        seed = 20260926L + match(poll_id, sort(unique(source_responses$poll_id)))
      )
    }
  ) |>
    purrr::list_rbind()
  dplyr::bind_rows(retained, computed) |>
    dplyr::left_join(
      dplyr::select(gains, poll_id, pollname),
      by = "poll_id", relationship = "one-to-one"
    )
}
