attrition_sources <- function() {
  tibble::tribble(
    ~poll_id, ~source_dataset,
    "marousi-2006", "score_only",
    "nic-1996", "historical",
    "a1r-climate-2021", "control"
  )
}

attrition_frame <- function(
  participants = read_phase_participants(), scores = read_analysis_phase_scores(),
  polls = read_poll_registry()
) {
  ids <- c("poll_id", "source_dataset", "respondent_id")
  people <- dplyr::semi_join(participants, attrition_sources(), by = c("poll_id", "source_dataset"))
  selected <- dplyr::semi_join(scores, people, by = ids)
  frame <- phase_score_frame(people, selected, polls)
  later <- selected |>
    dplyr::filter(wave == "t3", scale == "proportion_correct")
  stopifnot(
    !anyDuplicated(later[ids]), all(later$wave_role == "follow_up"),
    all(is.na(later$score) | (is.finite(later$score) & later$score >= 0 & later$score <= 1))
  )
  later <- later |>
    dplyr::transmute(
      poll_id, source_dataset, respondent_id, battery_id,
      score_t3 = score, wave_observed_t3 = wave_observed, n_items_t3 = n_items, record_t3 = TRUE
    )
  dplyr::left_join(frame, later, by = c(ids, "battery_id"), relationship = "one-to-one")
}

attrition_phases <- function(poll_id) {
  if (poll_id == "marousi-2006") c("t0", "t1", "t2") else c("t0", "t2", "t3")
}

attrition_target <- function(data) {
  phases <- attrition_phases(unique(data$poll_id))
  source_cohort <- data$attended %in% TRUE | data$attendance_before_post_rule %in% TRUE
  eligible <- source_cohort & phase_has_score(data, "t0") &
    phase_same_denominator(data, phases)
  if ("t3" %in% phases) eligible <- eligible & phase_has_score(data, "t2")
  data[eligible, ]
}

# Complete predictors avoid dropping attendees because a demographic is unmeasured.
# Each poll has its own models; covariates are never harmonized by pooling polls.
attrition_predictors <- function(data) {
  candidates <- c("score_t0", if (unique(data$poll_id) != "marousi-2006") "score_t2", "age", "ba", "female")
  candidates[vapply(data[candidates], function(x) all(is.finite(x)) && dplyr::n_distinct(x) > 1L, logical(1))]
}

attrition_mean <- function(data, phase, predictors) {
  observed <- phase_has_score(data, phase)
  y <- data[[paste0("score_", phase)]]
  stopifnot(any(observed), all(is.finite(as.matrix(data[predictors]))))
  if (all(observed)) {
    return(list(
      aipw = mean(y), ipw = mean(y), min_probability = 1, max_weight = 1,
      effective_n = nrow(data), predictors = "none: fully observed"
    ))
  }
  model <- data
  model$response <- as.integer(observed)
  model$outcome <- ifelse(observed, y, NA_real_)
  response_fit <- stats::glm(stats::reformulate(predictors, "response"), data = model,
    family = stats::binomial(), na.action = stats::na.fail
  )
  outcome_fit <- stats::glm(stats::reformulate(predictors, "outcome"), data = model[observed, ],
    family = stats::quasibinomial(), na.action = stats::na.fail
  )
  probability <- stats::predict(response_fit, newdata = model, type = "response")
  prediction <- stats::predict(outcome_fit, newdata = model, type = "response")
  if (!response_fit$converged || !outcome_fit$converged ||
        any(!is.finite(probability) | probability <= 0 | probability >= 1) ||
        any(!is.finite(prediction)) || anyNA(stats::coef(response_fit)) || anyNA(stats::coef(outcome_fit))) {
    stop("Unsupported attrition fit for ", unique(data$poll_id), " at ", phase)
  }
  correction <- numeric(nrow(data))
  correction[observed] <- (y[observed] - prediction[observed]) / probability[observed]
  weights <- 1 / probability[observed]
  list(
    aipw = mean(prediction + correction), ipw = stats::weighted.mean(y[observed], weights),
    min_probability = min(probability), max_weight = max(weights),
    effective_n = sum(weights)^2 / sum(weights^2), predictors = paste(predictors, collapse = " + ")
  )
}

attrition_analysis <- function(frame = attrition_frame()) {
  results <- frame |>
    dplyr::group_split(poll_id, source_dataset, battery_id) |>
    purrr::map(function(data) {
      target <- attrition_target(data)
      phases <- attrition_phases(unique(data$poll_id))
      predictors <- attrition_predictors(target)
      complete <- Reduce(`&`, lapply(phases, function(phase) phase_has_score(target, phase)))
      stopifnot(nrow(target) > 0, any(complete))
      metadata <- dplyr::distinct(data, poll_id, pollname, source_dataset, battery_id)
      attach <- function(x) dplyr::bind_cols(metadata[rep(1L, nrow(x)), ], x)
      flow <- purrr::map(phases, function(phase) {
        observed <- data[[paste0("wave_observed_", phase)]]
        attended <- data$attended %in% TRUE
        tibble::tibble(
          phase, n_source = nrow(data), n_attendees = sum(attended),
          n_nonattendees = sum(data$attended %in% FALSE), n_unknown_attendance = sum(is.na(data$attended)),
          n_attendee_observed = sum(attended & observed %in% TRUE),
          n_attendee_absent = sum(attended & observed %in% FALSE),
          n_attendee_unknown_presence = sum(attended & is.na(observed)),
          n_target = nrow(target), n_target_observed = sum(phase_has_score(target, phase)),
          n_target_unknown_group = sum(is.na(target$group))
        )
      }) |> purrr::list_rbind()
      means <- purrr::map(phases, function(phase) {
        fit <- attrition_mean(target, phase, predictors)
        observed <- phase_has_score(target, phase)
        tibble::tibble(
          phase, n_target = nrow(target), n_observed = sum(observed), n_complete = sum(complete),
          complete_case = mean(target[[paste0("score_", phase)]][complete]),
          ipw = fit$ipw, aipw = fit$aipw,
          min_probability = fit$min_probability, max_weight = fit$max_weight,
          effective_n = fit$effective_n, predictors = fit$predictors,
          baseline_observed = mean(target$score_t0[observed]),
          baseline_missing = if (any(!observed)) mean(target$score_t0[!observed]) else NA_real_
        )
      }) |> purrr::list_rbind()
      pairs <- tibble::tibble(from = phases[c(1, 2, 1)], to = phases[c(2, 3, 3)])
      contrasts <- pairs |>
        dplyr::mutate(
          n_target = nrow(target), n_complete = sum(complete),
          complete_case = means$complete_case[match(to, means$phase)] - means$complete_case[match(from, means$phase)],
          ipw = means$ipw[match(to, means$phase)] - means$ipw[match(from, means$phase)],
          aipw = means$aipw[match(to, means$phase)] - means$aipw[match(from, means$phase)]
        )
      recruitment <- data |>
        dplyr::count(arm, assignment, attendance_status, name = "n_people")
      patterns <- target |>
        dplyr::mutate(pattern = do.call(paste, c(lapply(phases, function(phase) {
          ifelse(phase_has_score(target, phase), paste0(phase, ":observed"), paste0(phase, ":unmeasured"))
        }), sep = "; "))) |>
        dplyr::count(pattern, name = "n_people")
      list(
        flow = attach(flow), means = attach(means), contrasts = attach(contrasts),
        recruitment = attach(recruitment), patterns = attach(patterns)
      )
    })
  tables <- c("flow", "means", "contrasts", "recruitment", "patterns")
  stats::setNames(lapply(tables, function(name) {
    purrr::map(results, name) |> purrr::list_rbind()
  }), tables)
}
