paired_item_answers <- function(responses, poll_id) {
  data <- dplyr::filter(responses, .data$poll_id == .env$poll_id)
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
    ids = pre$respondent_id,
    pre = as.data.frame(pre[item_ids]),
    post = as.data.frame(post[item_ids])
  )
}

fit_learning <- function(pre, post) {
  starts <- list(
    NULL, c(gg = .3, gk = .3, kk = .4, gamma = .25),
    c(gg = .5, gk = .2, kk = .3, gamma = .30),
    c(gg = .2, gk = .4, kk = .4, gamma = .20)
  )
  for (start in starts) {
    fit <- tryCatch(suppressWarnings(guess::fit_item_lca(
      pre, post,
      na_as = "missing", start = start
    )), error = function(e) NULL)
    if (!is.null(fit) && all(fit$diagnostics$convergence == 0L)) {
      return(mean(fit$learning))
    }
  }
  stop("Item learning model failed to converge")
}

learning_bootstrap <- function(panel, responses, seed = 20260927L) {
  responses <- responses |>
    dplyr::inner_join(dplyr::select(panel, poll_id, source_dataset, respondent_id),
      by = c("poll_id", "source_dataset", "respondent_id"),
      relationship = "many-to-one"
    )
  answers <- paired_item_answers(responses, unique(panel$poll_id))
  panel <- panel[match(answers$ids, panel$respondent_id), ]
  panel$.boot_row <- seq_len(nrow(panel))
  panel$pollid <- panel$poll_id
  stopifnot(!anyNA(panel$group))
  statistic <- function(data) {
    rows <- data$.boot_row
    baseline <- mean(data$k1)
    gain <- mean(data$k2 - data$k1)
    adjusted <- fit_learning(answers$pre[rows, , drop = FALSE], answers$post[rows, , drop = FALSE])
    c(
      raw = gain, relative = gain / baseline, adjusted = adjusted,
      raw_sd = gain / stats::sd(data$k1), adjusted_sd = adjusted / stats::sd(data$k1)
    )
  }
  bootstrap_stat(panel, statistic, seed, resample_polls = FALSE)
}

learning_estimates <- function(panel, responses) {
  polls <- split(panel, panel$poll_id)
  results <- lapply(seq_along(polls), function(i) {
    message("Learning bootstrap: ", names(polls)[i])
    learning_bootstrap(polls[[i]], responses, 20260927L + i * 10000L)
  })
  names(results) <- names(polls)
  summary <- dplyr::bind_rows(lapply(names(polls), function(id) {
    data <- polls[[id]]
    out <- tibble::tibble(
      poll_id = id, pollname = data$pollname[1],
      respondents = nrow(data), online = data$online[1],
      k1_mean = mean(data$k1), k2_mean = mean(data$k2),
      k1_sd = stats::sd(data$k1)
    )
    result <- results[[id]]
    for (term in names(result$estimate)) {
      out[[term]] <- result$estimate[[term]]
      out[[paste0(term, "_se")]] <- result$se[[term]]
      out[[paste0(term, "_lower")]] <- result$lower[[term]]
      out[[paste0(term, "_upper")]] <- result$upper[[term]]
    }
    out
  }))
  list(summary = summary, fits = results)
}
