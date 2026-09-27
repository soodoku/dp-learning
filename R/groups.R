# Within each poll we regress T2 knowledge on the group's
# leave-one-out mean, own value and the poll's leave-one-out mean (Guryan, Kroft
# and Notowidigdo 2009), clustering by group, and pool the poll estimates.
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
  fit <- stats::lm(stats::as.formula(paste0("k2 ~ peer_mean + own + poll_others", covariates)), data = poll)
  vc <- sandwich::vcovCL(fit, cluster = ~group, type = "HC1")
  tibble::tibble(
    estimate = stats::coef(fit)[["peer_mean"]],
    std_error = sqrt(vc["peer_mean", "peer_mean"]),
    n = nrow(poll),
    groups = dplyr::n_distinct(poll$group)
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
      dplyr::mutate(peer_effect(data, peer), poll = poll, peer = peer, .before = 1)
    }) |>
    purrr::list_rbind()
}

pool_peer_effects <- function(effects) {
  effects |>
    dplyr::filter(is.finite(std_error), std_error > 0) |>
    dplyr::group_split(peer) |>
    purrr::map(\(x) {
      fit <- bayesmeta::bayesmeta(y = x$estimate, sigma = x$std_error, labels = x$poll, tau.prior = tau_prior)
      tibble::tibble(
        peer = x$peer[[1]],
        polls = nrow(x),
        median = fit$summary["median", "mu"],
        lower = fit$summary["95% lower", "mu"],
        upper = fit$summary["95% upper", "mu"],
        tau = fit$summary["median", "tau"]
      )
    }) |>
    purrr::list_rbind()
}
