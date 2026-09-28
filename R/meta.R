pooled_learning <- function(results, label) {
  summary <- results$summary
  fits <- results$fits
  summarize <- function(ids, metric, model, parameter = "mu") {
    points <- vapply(fits[ids], function(x) x$estimate[[metric]], numeric(1))
    draws <- lapply(fits[ids], function(x) x$draws[, metric])
    pooled <- pool_bootstrap(points, draws, summary$online[match(ids, summary$poll_id)])
    tibble::tibble(
      model = model, parameter = parameter,
      estimate = pooled[["estimate"]], lower = pooled[["lower"]],
      upper = pooled[["upper"]], polls = length(ids)
    )
  }
  metrics <- c(
    raw = "raw", relative = "relative", adjusted = "guessing adjusted",
    raw_sd = "raw SD", adjusted_sd = "guessing adjusted SD"
  )
  pooled <- lapply(names(metrics), function(metric) {
    summarize(summary$poll_id, metric, paste(metrics[[metric]], label, sep = ", "))
  })
  if (label == "pooled") {
    pooled <- c(pooled, lapply(c(0, 1), function(mode) {
      summarize(
        summary$poll_id[summary$online == mode], "raw", "raw, by mode",
        if (mode == 0) "face_to_face" else "online"
      )
    }))
  }
  dplyr::bind_rows(pooled)
}
