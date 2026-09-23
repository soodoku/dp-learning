# Learning on the latent knowledge scale: a 2PL model with item parameters held
# equal across waves and the T2 mean and variance free, so the T2 mean is the
# gain in T1 standard deviations. Waves are treated as independent groups,
# which ignores the pairing of respondents and so understates precision. Items
# with no variation in either wave cannot be estimated and are dropped.
# Don't-know is scored as incorrect, as in the percent-correct measures.
irt_learning <- function(file_key, data_dir) {
  poll <- read_items(file.path(data_dir, paste0(file_key, ".csv")))
  score <- \(x) dplyr::mutate(x, dplyr::across(dplyr::everything(), \(v) dplyr::coalesce(v, 0L)))
  items <- rbind(score(poll$pre), score(poll$post))
  varies <- purrr::map2_lgl(score(poll$pre), score(poll$post), \(a, b) {
    length(unique(a)) > 1 && length(unique(b)) > 1
  })
  items <- items[varies]
  wave <- rep(c("t1", "t2"), each = nrow(poll$pre))
  fit <- mirt::multipleGroup(
    as.data.frame(items),
    model = 1,
    group = wave,
    itemtype = "2PL",
    invariance = c(names(items), "free_means", "free_var"),
    SE = TRUE,
    verbose = FALSE
  )
  t2 <- mirt::coef(fit, printSE = TRUE)$t2$GroupPars
  tibble::tibble(
    file_key = file_key,
    items_used = ncol(items),
    theta_gain = t2["par", "MEAN_1"],
    theta_gain_se = t2["SE", "MEAN_1"],
    t2_sd = sqrt(t2["par", "COV_11"]),
    converged = mirt::extract.mirt(fit, "converged")
  )
}
