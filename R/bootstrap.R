sample_polls <- function(strata) {
  unlist(lapply(split(seq_along(strata), strata), function(ids) {
    ids[sample.int(length(ids), length(ids), replace = TRUE)]
  }), use.names = FALSE)
}

bootstrap_replicates <- function() getOption("dp.bootstrap.replicates", 999L)

resample_hierarchy <- function(data, resample_polls = TRUE) {
  stopifnot(all(c("pollid", "group") %in% names(data)), !anyNA(data$group))
  polls <- unique(data$pollid)
  selected <- if (resample_polls && length(polls) > 1L) {
    mode <- if ("online" %in% names(data)) data$online[match(polls, data$pollid)] else rep(0, length(polls))
    stopifnot(!anyNA(mode))
    polls[sample_polls(mode)]
  } else {
    polls
  }
  result <- lapply(seq_along(selected), function(i) {
    poll <- data[data$pollid == selected[i], , drop = FALSE]
    strata <- if ("arm" %in% names(poll)) split(poll, poll$arm) else list(poll)
    sampled <- lapply(seq_along(strata), function(a) {
      arm <- strata[[a]]
      groups <- split(seq_len(nrow(arm)), arm$group)
      selected_groups <- sample.int(length(groups), length(groups), replace = TRUE)
      rows <- groups[selected_groups]
      out <- arm[unlist(rows, use.names = FALSE), , drop = FALSE]
      out$group <- rep(paste(i, a, seq_along(rows), sep = "_"), lengths(rows))
      out$pollid <- as.character(i)
      out
    })
    dplyr::bind_rows(sampled)
  }) |>
    dplyr::bind_rows()
  if ("poll_k1" %in% names(result)) {
    result <- dplyr::mutate(result, poll_k1 = mean(k1), .by = pollid)
  }
  result
}

bootstrap_stat <- function(data, statistic, seed = 20260927L,
                           resample_polls = TRUE, n = bootstrap_replicates()) {
  estimate <- statistic(data)
  stopifnot(length(estimate) > 0L, all(is.finite(estimate)), n >= 2L)
  one <- function(i) {
    set.seed(seed + i)
    value <- statistic(resample_hierarchy(data, resample_polls))
    if (!identical(names(value), names(estimate)) || any(!is.finite(value))) {
      stop("Invalid bootstrap fit in draw ", i)
    }
    value
  }
  workers <- max(1L, as.integer(Sys.getenv("DP_BOOT_WORKERS", "2")))
  values <- if (.Platform$OS.type == "unix" && workers > 1L) {
    parallel::mclapply(seq_len(n), one, mc.cores = workers, mc.set.seed = FALSE)
  } else {
    lapply(seq_len(n), one)
  }
  failed <- vapply(values, inherits, logical(1), "try-error")
  if (any(failed)) stop("Bootstrap fit failed: ", values[[which(failed)[1]]])
  draws <- do.call(rbind, values)
  list(
    estimate = estimate, draws = draws,
    se = apply(draws, 2, stats::sd),
    lower = apply(draws, 2, stats::quantile, probs = 0.025),
    upper = apply(draws, 2, stats::quantile, probs = 0.975)
  )
}

bootstrap_rows <- function(result) {
  tibble::tibble(
    term = names(result$estimate), estimate = unname(result$estimate),
    std_error = unname(result$se), lower = unname(result$lower),
    upper = unname(result$upper)
  )
}

# Independent polls define the population of the pooled, equally weighted mean.
# Each selected poll supplies an independently selected within-poll bootstrap draw.
pool_bootstrap <- function(estimates, draws, strata = rep(0, length(estimates)), seed = 20260928L,
                           weights = rep(1, length(estimates))) {
  stopifnot(
    length(estimates) == length(draws), length(strata) == length(estimates),
    !anyNA(strata), all(is.finite(estimates)),
    length(weights) == length(estimates), all(is.finite(weights)), all(weights > 0)
  )
  set.seed(seed)
  pooled <- replicate(bootstrap_replicates(), {
    polls <- sample_polls(strata)
    stats::weighted.mean(vapply(polls, function(p) {
      draws[[p]][sample.int(length(draws[[p]]), 1L)]
    }, numeric(1)), weights[polls])
  })
  c(
    estimate = stats::weighted.mean(estimates, weights), lower = unname(stats::quantile(pooled, .025)),
    upper = unname(stats::quantile(pooled, .975))
  )
}
