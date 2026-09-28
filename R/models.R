# Observed knowledge gain conditional on baseline knowledge and covariates,
# with random intercepts for small group and poll. Core regressors refer to
# baseline; briefing reading is recalled afterward. Minority status is missing
# for whole polls, so it enters only in a robustness model.
core_formula <- I(k2 - k1) ~ k1 + group_k1 + group_size + online + poll_k1 +
  (1 | group) + (1 | pollid)

core_group_frame <- function(attendees) {
  attendees |>
    dplyr::filter(!is.na(group)) |>
    dplyr::mutate(
      group_size = dplyr::n(),
      group_k1 = (sum(k1) - k1) / (group_size - 1),
      p_female = leave_one_out(female, group),
      .by = group
    ) |>
    dplyr::filter(group_size > 1L) |>
    dplyr::mutate(poll_k1 = mean(k1), .by = poll_id) |>
    dplyr::mutate(
      pollid = poll_id,
      education = factor(education_labels[as.character(education)], levels = education_labels)
    )
}

demographic_formula <- I(k2 - k1) ~ k1 * education + age_decades +
  female * p_female + group_size + group_k1 + online + poll_k1 +
  (1 | group) + (1 | pollid)

main_formula <- I(k2 - k1) ~ k1 * education + age_decades + extremity + group_size + group_k1 +
  heterogeneity + female * p_female + online + poll_k1 + (1 | group) + (1 | pollid)

minority_formula <- stats::update(main_formula, . ~ . + minority * p_minority)

# Other members' T1 knowledge of items the participant missed at T1, added
# alongside their overall T1 knowledge. A separate indicator marks respondents
# with no missed T1 items, whose targeted opportunity score is zero.
items_formula <- stats::update(main_formula, . ~ . + group_k1_items + no_missed_items)

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
  if (fit@optinfo$conv$opt != 0) {
    fit <- lme4::lmer(
      formula, data = frame, REML = FALSE,
      control = lme4::lmerControl(
        optimizer = "nloptwrap",
        optCtrl = list(maxeval = 200000, xtol_abs = 1e-9, ftol_abs = 1e-9)
      )
    )
  }
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


bootstrap_model <- function(frame, formula, model, included = rep(TRUE, nrow(frame))) {
  if ("age" %in% names(frame)) frame$age_decades <- frame$age / 10
  stopifnot(length(included) == nrow(frame), !anyNA(included))
  frame$.model_included <- included
  fit <- fit_knowledge(frame[frame$.model_included, ], formula, check = FALSE)
  terms <- names(lme4::fixef(fit))
  terms <- terms[!grepl("factor\\(pollid\\)", terms)]
  statistic <- function(x) {
    suppressMessages(lme4::fixef(fit_knowledge(x[x$.model_included, ], formula, check = FALSE)))[terms]
  }
  message("Regression bootstrap: ", model)
  result <- bootstrap_stat(frame, statistic)
  tidy_fit(fit, model) |>
    dplyr::inner_join(dplyr::select(bootstrap_rows(result), -estimate), by = "term")
}


model_complete_cases <- function(frame, formula) {
  frame$age_decades <- frame$age / 10
  stats::complete.cases(frame[all.vars(formula)])
}

historical_model_sample <- function(core, historical) {
  keys <- historical |>
    dplyr::mutate(
      historical_respondent_id = as.character(caseid),
      included = model_complete_cases(historical, main_formula)
    ) |>
    dplyr::left_join(
      dplyr::select(read_respondent_sources(), poll_id, dpnum),
      by = "dpnum", relationship = "many-to-one"
    ) |>
    dplyr::select(poll_id, historical_respondent_id, included)
  core |>
    dplyr::inner_join(keys, by = c("poll_id", "historical_respondent_id"),
                      relationship = "one-to-one")
}

expanded_briefing_formula <- stats::update(
  demographic_formula,
  . ~ . - online - poll_k1 - (1 | pollid) + factor(pollid) + read_briefing
)
