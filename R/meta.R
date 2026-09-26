# Normal-normal random-effects model for poll-level gains. bayesmeta computes the posterior by direct numerical
# integration rather than MCMC, so results are exact and deterministic.
# Priors: flat on the mean (and on regression coefficients); half-normal with
# scale 0.1 on between-poll SD, i.e. poll gains spread about +/- 10 points.
tau_prior <- \(t) bayesmeta::dhalfnormal(t, scale = 0.1)

meta_gain <- function(gains, estimate = "raw", se = "raw_se") {
  gains <- dplyr::filter(gains, is.finite(.data[[estimate]]))
  bayesmeta::bayesmeta(
    y = gains[[estimate]],
    sigma = gains[[se]],
    labels = gains$pollname,
    tau.prior = tau_prior
  )
}

meta_mode <- function(gains, estimate = "raw", se = "raw_se") {
  gains <- dplyr::filter(gains, is.finite(.data[[estimate]]))
  bayesmeta::bmr(
    y = gains[[estimate]],
    sigma = gains[[se]],
    labels = gains$pollname,
    X = cbind(face_to_face = 1 - gains$online, online = gains$online),
    tau.prior = tau_prior
  )
}

tidy_meta <- function(fit, model) {
  if (inherits(fit, "bmr")) {
    s <- fit$summary
    betas <- setdiff(colnames(s), "tau")
  } else {
    s <- fit$summary
    betas <- "mu"
  }
  tibble::tibble(
    model = model,
    parameter = c(betas, "tau"),
    median = s["median", c(betas, "tau")],
    lower = s["95% lower", c(betas, "tau")],
    upper = s["95% upper", c(betas, "tau")]
  )
}

# Pools the adjusted attendee-control comparisons in control-group SD units.
meta_control <- function(effects) {
  primary <- effects |>
    dplyr::filter(comparison %in% c(
      "Attended vs uninvited control", "Attended vs randomized control"
    )) |>
    dplyr::mutate(
      y = estimate / control_t1_sd,
      sigma = std_error / control_t1_sd
    )
  pooled <- bayesmeta::bayesmeta(y = primary$y, sigma = primary$sigma, labels = primary$study, tau.prior = tau_prior)
  tibble::tibble(
    parameter = c("mu", "tau"),
    median = pooled$summary["median", c("mu", "tau")],
    lower = pooled$summary["95% lower", c("mu", "tau")],
    upper = pooled$summary["95% upper", c("mu", "tau")],
    studies = nrow(primary)
  )
}
