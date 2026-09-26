# Item matrices keep don't-know as NA so the latent class model can treat it
# as its own response; the percent-correct scores count it as wrong.
read_items <- function(path) {
  data <- readr::read_csv(path, na = c("", "NA"), show_col_types = FALSE)
  items <- setdiff(names(data), "female")
  half <- length(items) / 2L
  as_int <- \(names) dplyr::mutate(data[names], dplyr::across(dplyr::everything(), as.integer))
  pre <- as_int(items[seq_len(half)])
  post <- as_int(items[half + seq_len(half)])
  names(post) <- names(pre)
  list(pre = pre, post = post)
}

# Starting values from guess-replication, where the default start fails to
# converge for one NIC item.
fit_item_model <- function(pre, post) {
  starts <- list(
    NULL,
    c(gg = .2, gk = .2, gd = .1, kk = .1, dg = .1, dk = .1, dd = .2, gamma = .2),
    c(gg = .3, gk = .1, gd = .1, kk = .1, dg = .1, dk = .1, dd = .2, gamma = .25)
  )
  attempt <- function(start) {
    tryCatch(
      guess::fit_item_lca(pre, post, na_as = "dk", start = start),
      error = \(e) NULL
    )
  }
  fit <- purrr::detect(purrr::map(starts, attempt), Negate(is.null))
  if (is.null(fit)) stop("fit_item_lca failed for every starting value.")
  fit
}

poll_learning <- function(file_key, data_dir) {
  poll <- read_items(file.path(data_dir, paste0(file_key, ".csv")))
  fit <- fit_item_model(poll$pre, poll$post)
  score <- \(x) as.matrix(dplyr::mutate(x, dplyr::across(dplyr::everything(), \(v) dplyr::coalesce(v, 0L))))
  raw_gain <- rowMeans(score(poll$post) - score(poll$pre))
  tibble::tibble(
    file_key = file_key,
    respondents = nrow(poll$pre),
    items = ncol(poll$pre),
    k1 = mean(score(poll$pre)),
    k2 = mean(score(poll$post)),
    raw = mean(raw_gain),
    raw_se = stats::sd(raw_gain) / sqrt(length(raw_gain)),
    lca = mean(fit$learning),
    lca_converged = all(fit$diagnostics$convergence == 0)
  )
}

# Participant-level mean gain (T2 - T1 proportion correct) by poll.
poll_gains <- function(frame) {
  se <- \(x) stats::sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
  frame |>
    dplyr::mutate(raw_gain = k2 - k1) |>
    dplyr::summarise(
      respondents = dplyr::n(),
      online = dplyr::first(online),
      k1_mean = mean(k1, na.rm = TRUE),
      k2_mean = mean(k2, na.rm = TRUE),
      raw = mean(raw_gain, na.rm = TRUE),
      raw_se = se(raw_gain),
      k1_sd = stats::sd(k1, na.rm = TRUE),
      .by = c(dpnum, pollname)
    ) |>
    dplyr::mutate(raw_sd = raw / k1_sd, raw_sd_se = raw_se / k1_sd) |>
    dplyr::arrange(dplyr::desc(raw))
}
