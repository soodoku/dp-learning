# T2 knowledge on pre-deliberation knowledge and covariates,
# with random intercepts for small group and poll. Every regressor is measured
# before deliberation. Minority status is missing for whole polls (EU, China,
# Greece), so it enters only in a robustness model.
core_formula <- k2 ~ k1 + group_k1 + group_size + online + poll_k1 +
  (1 | group) + (1 | pollid)

core_group_frame <- function(attendees) {
  attendees |>
    dplyr::filter(!is.na(group)) |>
    dplyr::mutate(
      group_size = dplyr::n(),
      group_k1 = (sum(k1) - k1) / (group_size - 1),
      .by = group
    ) |>
    dplyr::filter(group_size > 1L) |>
    dplyr::mutate(poll_k1 = mean(k1), .by = poll_id) |>
    dplyr::mutate(pollid = poll_id)
}

main_formula <- k2 ~ k1 * education + age_decades + extremity + group_size + group_k1 +
  heterogeneity + female * p_female + online + poll_k1 + (1 | group) + (1 | pollid)

minority_formula <- stats::update(main_formula, . ~ . + minority * p_minority)

# Other members' T1 knowledge of items the participant missed at T1, added
# alongside their overall T1 knowledge. A separate indicator marks respondents
# with no missed T1 items, whose targeted opportunity score is zero.
items_formula <- stats::update(main_formula, . ~ . + group_k1_items + no_missed_items)

# Source-linked respondent reports of briefing-material reading are available.
# Poll-level terms are replaced by poll intercepts.
briefing_formula <- stats::update(
  main_formula,
  . ~ . - online - poll_k1 - (1 | pollid) + factor(pollid) + read_briefing
)

fit_knowledge <- function(frame, formula = main_formula, check = TRUE) {
  if ("age" %in% names(frame)) {
    frame <- dplyr::mutate(frame, age_decades = age / 10)
  }
  fit <- lme4::lmer(
    formula,
    data = frame, REML = FALSE,
    control = lme4::lmerControl(
      optimizer = "bobyqa", optCtrl = list(maxfun = 200000, rhoend = 1e-9)
    )
  )
  if (check) stopifnot(is.null(fit@optinfo$conv$lme4$messages))
  stopifnot(fit@optinfo$conv$opt == 0)
  fit
}

tidy_fit <- function(fit, model) {
  est <- stats::coef(summary(fit))
  random <- lme4::VarCorr(fit)
  stopifnot(all(vapply(random, nrow, integer(1L)) == 1L))
  fixed_var <- stats::var(as.vector(lme4::getME(fit, "X") %*% lme4::fixef(fit)))
  random_var <- sum(vapply(random, \(x) as.numeric(x[1, 1]), numeric(1L)))
  total_var <- fixed_var + random_var + stats::sigma(fit)^2
  tibble::tibble(
    model = model,
    term = rownames(est),
    estimate = est[, "Estimate"],
    n = stats::nobs(fit),
    polls = dplyr::n_distinct(lme4::getME(fit, "flist")$group |> sub(pattern = "_.*", replacement = "")),
    groups = lme4::ngrps(fit)[["group"]],
    r2_marginal = fixed_var / total_var,
    r2_conditional = (fixed_var + random_var) / total_var
  )
}


bootstrap_model <- function(frame, formula, model) {
  if ("age" %in% names(frame)) frame$age_decades <- frame$age / 10
  fit <- fit_knowledge(frame, formula, check = FALSE)
  terms <- names(lme4::fixef(fit))
  terms <- terms[!grepl("factor\\(pollid\\)", terms)]
  statistic <- function(x) {
    suppressMessages(lme4::fixef(fit_knowledge(x, formula, check = FALSE)))[terms]
  }
  message("Regression bootstrap: ", model)
  result <- bootstrap_stat(frame, statistic)
  tidy_fit(fit, model) |>
    dplyr::inner_join(dplyr::select(bootstrap_rows(result), -estimate), by = "term")
}
