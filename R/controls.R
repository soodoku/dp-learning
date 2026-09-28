control_panel <- function(
  participants = read_analysis_participants(),
  scores = read_analysis_scores(),
  responses = read_analysis_responses()
) {
  item_polls <- unique(responses$poll_id)
  people <- participants |>
    dplyr::filter(source_dataset == "control", poll_id %in% item_polls)
  outcomes <- scores |>
    dplyr::filter(source_dataset == "control", poll_id %in% item_polls) |>
    dplyr::select("poll_id", "respondent_id", "wave", "score") |>
    tidyr::pivot_wider(
      names_from = wave, values_from = score,
      names_prefix = "k"
    )
  labels <- c(
    "america-in-one-room-2019" = "America in One Room 2019",
    "a1r-climate-2021" = "America in One Room: Climate 2021",
    "amr-2024" = "Antimicrobial Resistance 2024",
    "northern-ireland-2007" = "Northern Ireland 2007"
  )
  out <- people |>
    dplyr::left_join(outcomes,
      by = c("poll_id", "respondent_id"),
      relationship = "one-to-one"
    ) |>
    dplyr::transmute(
      study = unname(labels[poll_id]), poll_id, id = respondent_id,
      source_dataset, respondent_id, arm, treated = as.numeric(arm == "attended"), panel,
      k1 = kt1, k2 = kt2, k3 = kt3,
      weight, ba, female, group = small_group_id,
      cluster = cluster_id, country
    )
  stopifnot(!anyNA(out$study), !anyDuplicated(out[c("poll_id", "id")]))
  out$pollid <- out$poll_id
  out$group <- ifelse(!is.na(out$group), paste0("discussion_", out$group),
    paste0(out$arm, "_", out$cluster)
  )
  out
}

read_a1r <- function(data) dplyr::filter(data, poll_id == "america-in-one-room-2019")
read_climate <- function(data) dplyr::filter(data, poll_id == "a1r-climate-2021")
read_amr <- function(data) dplyr::filter(data, poll_id == "amr-2024")

amr_countries <- c("Brazil", "Colombia", "India", "Indonesia", "Nigeria", "Tanzania")

# ANCOVA adjusts attendee-control differences in T2 for T1 knowledge.
# The gain-score comparison uses a different baseline restriction. Both
# comparisons can reflect selection into attendance.
effect <- function(data, contrast, treated_value, control_value, weights = NULL) {
  data <- data |>
    dplyr::filter(.data[[contrast]] %in% c(treated_value, control_value), !is.na(k1), !is.na(k2)) |>
    dplyr::mutate(treat = as.numeric(.data[[contrast]] == treated_value))
  statistic <- function(x) {
    w <- if (is.null(weights)) NULL else x[[weights]]
    fit <- stats::lm(k2 ~ treat + k1, data = x, weights = w)
    gains <- stats::lm(I(k2 - k1) ~ treat, data = x, weights = w)
    c(
      estimate = stats::coef(fit)[["treat"]],
      gain_difference = stats::coef(gains)[["treat"]]
    )
  }
  result <- bootstrap_stat(data, statistic, resample_polls = FALSE)
  tibble::tibble(
    estimate = result$estimate[["estimate"]], std_error = result$se[["estimate"]],
    lower = result$lower[["estimate"]], upper = result$upper[["estimate"]],
    gain_difference = result$estimate[["gain_difference"]],
    gain_difference_se = result$se[["gain_difference"]],
    gain_lower = result$lower[["gain_difference"]], gain_upper = result$upper[["gain_difference"]],
    t1_difference = mean(data$k1[data$treat == 1]) - mean(data$k1[data$treat == 0]),
    control_t1_sd = stats::sd(data$k1[data$treat == 0]),
    n_treated = sum(data$treat == 1), n_control = sum(data$treat == 0),
    treated_gain = mean(data$k2[data$treat == 1] - data$k1[data$treat == 1]),
    control_gain = mean(data$k2[data$treat == 0] - data$k1[data$treat == 0])
  )
}

ni_t3_effect <- function(data) {
  data <- data |>
    dplyr::filter(poll_id == "northern-ireland-2007", !is.na(k3))
  stopifnot(
    sum(data$treated == 1) == 93L,
    sum(data$treated == 0) == 150L
  )
  result <- bootstrap_stat(data, function(x) {
    c(estimate = stats::coef(stats::lm(k3 ~ treated, data = x))[["treated"]])
  }, resample_polls = FALSE)
  control_sd <- stats::sd(data$k3[data$treated == 0])
  tibble::tibble(
    study = "Northern Ireland 2007",
    comparison = "T3 attendee vs control, no baseline",
    scale = "percent",
    estimate = result$estimate[["estimate"]],
    std_error = result$se[["estimate"]],
    lower = result$lower[["estimate"]], upper = result$upper[["estimate"]],
    control_t1_sd = NA_real_, control_t3_sd = control_sd,
    n_treated = sum(data$treated == 1),
    n_control = sum(data$treated == 0),
    estimate_sd = estimate / control_sd
  )
}

control_effects <- function(data) {
  a1r <- read_a1r(data)
  climate <- read_climate(data)
  climate_t3 <- dplyr::mutate(climate, k2 = k3)
  amr <- read_amr(data)
  amr_countries_list <- purrr::map(amr_countries, \(country) {
    list(
      dplyr::filter(amr, .data$country == .env$country), "treated", 1, 0, NULL,
      paste0("Attended vs randomized control: ", country), "percent"
    )
  })
  adjusted <- c(list(
    list(a1r, "treated", 1, 0, NULL, "Attended vs uninvited control", "percent"),
    list(a1r, "treated", 1, 0, "weight", "Attended vs uninvited control, weighted", "percent"),
    list(climate, "treated", 1, 0, NULL, "Attended vs uninvited control", "percent"),
    list(climate, "treated", 1, 0, "weight", "Attended vs uninvited control, weighted", "percent"),
    list(climate_t3, "treated", 1, 0, NULL, "Attended vs control, one year later", "percent"),
    list(amr, "treated", 1, 0, NULL, "Attended vs randomized control", "percent"),
    list(amr, "treated", 1, 0, "weight", "Attended vs randomized control, weighted", "percent")
  ), amr_countries_list) |>
    purrr::map(\(x) {
      effect(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]]) |>
        dplyr::mutate(study = x[[1]]$study[[1]], comparison = x[[6]], scale = x[[7]], .before = 1)
    }) |>
    purrr::list_rbind() |>
    dplyr::mutate(estimate_sd = estimate / control_t1_sd)
  dplyr::bind_rows(adjusted, ni_t3_effect(data))
}

# Who shows up: T1 knowledge of attendees vs invitees who did not attend.
selection <- function(data) {
  data |>
    dplyr::filter(
      poll_id %in% c("america-in-one-room-2019", "a1r-climate-2021"),
      arm %in% c("attended", "invited_nonattender")
    ) |>
    dplyr::summarise(
      k1 = mean(k1), n = dplyr::n(),
      .by = c(study, arm)
    ) |>
    dplyr::mutate(group = dplyr::recode(
      arm,
      attended = "attended",
      invited_nonattender = "invited, did not attend"
    )) |>
    dplyr::mutate(arm = factor(arm, levels = c("attended", "invited_nonattender"))) |>
    dplyr::arrange(study, arm) |>
    dplyr::select(study, group, k1, n)
}

# Does participation help those who start out knowing less, or those with less
# schooling, more? The interaction of treatment with T1 knowledge (centred) and
# with a bachelor's degree, in the same ANCOVA.
heterogeneity <- function(data, contrast, treated_value, control_value, moderator) {
  data <- data |>
    dplyr::filter(
      .data[[contrast]] %in% c(treated_value, control_value),
      !is.na(k1), !is.na(k2), !is.na(.data[[moderator]])
    ) |>
    dplyr::mutate(
      treat = as.numeric(.data[[contrast]] == treated_value),
      k1_centred = k1 - mean(k1),
      moderator = if (moderator == "k1") k1_centred else .data[[moderator]]
    )
  result <- bootstrap_stat(data, function(x) {
    fit <- stats::lm(k2 ~ treat * moderator + k1_centred, data = x)
    c(interaction = stats::coef(fit)[["treat:moderator"]])
  }, resample_polls = FALSE)
  tibble::tibble(
    moderator = moderator, interaction = result$estimate[[1]],
    interaction_se = result$se[[1]], lower = result$lower[[1]], upper = result$upper[[1]],
    n = nrow(data)
  )
}

control_heterogeneity <- function(data) {
  a1r <- read_a1r(data)
  climate <- read_climate(data)
  amr <- read_amr(data)
  list(
    list(a1r, "treated", 1, 0, "k1"), list(a1r, "treated", 1, 0, "ba"),
    list(climate, "treated", 1, 0, "k1"), list(climate, "treated", 1, 0, "ba"),
    list(amr, "treated", 1, 0, "k1"), list(amr, "treated", 1, 0, "ba")
  ) |>
    purrr::map(\(x) {
      heterogeneity(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]]) |>
        dplyr::mutate(study = x[[1]]$study[[1]], .before = 1)
    }) |>
    purrr::list_rbind()
}


control_learning <- function(data, core, responses) {
  ids <- c("america-in-one-room-2019", "a1r-climate-2021")
  dplyr::bind_rows(lapply(seq_along(ids), function(i) {
    id <- ids[i]
    people <- dplyr::filter(
      data, poll_id == .env$id, arm %in% c("attended", "control"),
      !is.na(k1), !is.na(k2)
    )
    people <- dplyr::filter(
      people, arm == "control" |
        respondent_id %in% core$respondent_id[core$poll_id == .env$id]
    )
    paired <- responses |>
      dplyr::filter(
        poll_id == .env$id, source_dataset == "control",
        respondent_id %in% people$respondent_id
      )
    answers <- paired_item_answers(paired, id)
    people <- people[match(answers$ids, people$respondent_id), ]
    people$.boot_row <- seq_len(nrow(people))
    statistic <- function(x) {
      learning <- vapply(c("attended", "control"), function(arm) {
        rows <- x$.boot_row[x$arm == arm]
        fit_learning(answers$pre[rows, , drop = FALSE], answers$post[rows, , drop = FALSE])
      }, numeric(1))
      gain <- x$k2 - x$k1
      c(
        raw = mean(gain[x$arm == "attended"]) - mean(gain[x$arm == "control"]),
        adjusted = unname(learning[1] - learning[2])
      )
    }
    message("Control learning bootstrap: ", id)
    fit <- bootstrap_stat(people, statistic, 20260927L + i * 20000L, resample_polls = FALSE)
    bootstrap_rows(fit) |>
      dplyr::mutate(
        poll_id = id, study = people$study[1],
        n_treated = sum(people$arm == "attended"),
        n_control = sum(people$arm == "control"), .before = 1
      )
  }))
}
