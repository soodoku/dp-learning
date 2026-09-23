# Post-deliberation knowledge on pre-deliberation knowledge and covariates,
# with random intercepts for small group and poll. Every regressor is measured
# before deliberation. Minority status is missing for whole polls (EU, China,
# Greece), so it enters only in a robustness model.
main_formula <- k2 ~ k1 * education + age_decades + extremity + group_size + group_k1 +
  heterogeneity + female * p_female + online + poll_k1 + (1 | group) + (1 | pollid)

minority_formula <- stats::update(main_formula, . ~ . + minority * p_minority)

# Other members' T1 knowledge of the items i missed at T1, for the polls with
# linked item-level data.
items_formula <- stats::update(main_formula, . ~ . - group_k1 + group_k1_items)

# Only four face-to-face polls asked about the briefing materials, so poll-level
# terms are replaced by poll intercepts.
briefing_formula <- stats::update(
  main_formula,
  . ~ . - online - poll_k1 - (1 | pollid) + factor(pollid) + read_briefing
)

fit_knowledge <- function(frame, formula = main_formula) {
  frame <- dplyr::mutate(frame, age_decades = age / 10)
  lme4::lmer(formula, data = frame, REML = FALSE)
}

tidy_fit <- function(fit, model) {
  est <- stats::coef(summary(fit))
  tibble::tibble(
    model = model,
    term = rownames(est),
    estimate = est[, "Estimate"],
    std_error = est[, "Std. Error"],
    n = stats::nobs(fit),
    polls = dplyr::n_distinct(lme4::getME(fit, "flist")$group |> sub(pattern = "_.*", replacement = "")),
    groups = lme4::ngrps(fit)[["group"]]
  )
}
