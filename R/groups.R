# Within each poll we regress T2 knowledge on the group's
# leave-one-out mean, own value and the poll's leave-one-out mean (Guryan, Kroft
# and Notowidigdo 2009), resampling groups, and pool with the same hierarchical bootstrap.
# Assignment balance is checked separately; peer coefficients are associations.
peer_effect <- function(poll, peer) {
  poll <- poll |>
    dplyr::filter(!is.na(k1), !is.na(k2), !is.na(.data[[peer]])) |>
    dplyr::mutate(
      own = .data[[peer]],
      peer_mean = leave_one_out(own, group),
      poll_others = leave_one_out(own, pollid)
    ) |>
    dplyr::filter(is.finite(peer_mean))
  covariates <- if (peer == "k1") "" else " + k1"
  statistic <- function(x) {
    x$peer_mean <- leave_one_out(x$own, x$group)
    x$poll_others <- leave_one_out(x$own, x$pollid)
    fit <- stats::lm(stats::as.formula(paste0("k2 ~ peer_mean + own + poll_others", covariates)), data = x)
    c(peer = stats::coef(fit)[["peer_mean"]])
  }
  fit <- bootstrap_stat(poll, statistic, resample_polls = FALSE)
  tibble::tibble(
    estimate = fit$estimate[[1]], std_error = fit$se[[1]],
    lower = fit$lower[[1]], upper = fit$upper[[1]],
    n = nrow(poll), groups = dplyr::n_distinct(poll$group), draws = list(fit$draws[, 1])
  )
}

peer_effects <- function(frame) {
  polls <- split(frame, frame$pollname)
  tidyr::expand_grid(poll = names(polls), peer = c("k1", "female")) |>
    purrr::pmap(\(poll, peer) {
      data <- polls[[poll]]
      if (sum(!is.na(data[[peer]])) < 50 || dplyr::n_distinct(data$group) < 5) {
        return(NULL)
      }
      dplyr::mutate(peer_effect(data, peer), poll = poll, peer = peer, online = data$online[1], .before = 1)
    }) |>
    purrr::list_rbind()
}

pool_peer_effects <- function(effects) {
  effects |>
    dplyr::filter(is.finite(std_error), std_error > 0) |>
    dplyr::group_split(peer) |>
    purrr::map(\(x) {
      fit <- pool_bootstrap(x$estimate, x$draws, x$online)
      tibble::tibble(
        peer = x$peer[[1]], polls = nrow(x),
        estimate = fit[["estimate"]], lower = fit[["lower"]], upper = fit[["upper"]]
      )
    }) |>
    purrr::list_rbind()
}
