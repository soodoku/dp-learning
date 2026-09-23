fetch_control_files <- function(manifest = project_file("data", "control_files.csv"),
                                cache = project_file("data", "cache")) {
  files <- readr::read_csv(manifest, show_col_types = FALSE)
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(cache, files$file)
  purrr::walk2(files$url, paths, \(url, path) {
    if (!file.exists(path)) utils::download.file(url, path, mode = "wb", quiet = TRUE)
  })
  observed <- purrr::map_chr(paths, \(path) digest::digest(file = path, algo = "sha256"))
  if (!identical(observed, files$sha256)) {
    stop("Checksum mismatch: ", paste(files$file[observed != files$sha256], collapse = ", "))
  }
  rlang::set_names(paths, tools::file_path_sans_ext(files$file))
}

# Proportion correct; don't know, skipped and refused count as not knowing,
# as in the Deliberative Poll scores.
score_battery <- function(data, items, key) {
  purrr::map2(items, key, \(item, answer) as.numeric(data[[item]] == answer)) |>
    purrr::map(\(x) dplyr::coalesce(x, 0)) |>
    as.data.frame() |>
    rowMeans()
}

# Key checked against the published results: attendees 46% -> 60%, controls +1
# point (Fishkin et al. 2021). PK4 (Paris Agreement) counts "All of the above":
# Russia had not ratified when fieldwork began.
a1r_key <- c(2, 1, 1, 4, 4, 2, 2)

read_a1r <- function(path) {
  data <- readr::read_tsv(path, show_col_types = FALSE)
  tibble::tibble(
    study = "America in One Room 2019",
    id = seq_len(nrow(data)),
    treated = data$CONDITION,
    panel = data$POST == 1,
    k1 = score_battery(data, paste0("PK", 1:7), a1r_key),
    k2 = dplyr::if_else(
      data$POST == 1, score_battery(data, paste0("T2PK", 1:7), a1r_key), NA_real_
    ),
    weight = dplyr::if_else(data$CONDITION == 1, data$WEIGHT_DELEGATE, data$WEIGHT_CONTROL),
    ba = dplyr::if_else(data$EDUC4 %in% 1:4, as.numeric(data$EDUC4 == 4), NA_real_),
    cluster = seq_len(nrow(data))
  )
}

# Key reproduces the weighted percent correct in the published results for
# every item at T1 and T2.
climate_key <- c(1, 1, 3, 1, 5, 1, 1, 1)

read_climate <- function(path) {
  data <- readr::read_tsv(path, show_col_types = FALSE)
  attended <- data$P_DELEGATE == 1
  control_panel <- data$P_TREATMENT == 0 & data$P_DELEGATE == 0
  tibble::tibble(
    study = "America in One Room: Climate 2021",
    id = seq_len(nrow(data)),
    treated = as.numeric(data$P_TREATMENT == 1),
    panel = attended | control_panel,
    k1 = score_battery(data, paste0("Q", 17:24), climate_key),
    k2 = dplyr::if_else(
      attended | control_panel, score_battery(data, paste0("T2Q", 17:24), climate_key), NA_real_
    ),
    k3 = dplyr::if_else(
      !is.na(data$T3Q17), score_battery(data, paste0("T3Q", 17:24), climate_key), NA_real_
    ),
    weight = data$WEIGHT1,
    ba = dplyr::if_else(data$EDUC5 %in% 1:5, as.numeric(data$EDUC5 >= 4), NA_real_),
    cluster = seq_len(nrow(data))
  )
}

# The authors' standardized knowledge index; item-level missing codes are
# undocumented. Village is the unit of randomization.
read_tanzania <- function(path) {
  data <- haven::read_dta(path) |>
    dplyr::filter(sample == "Citizens" | haven::as_factor(sample) == "Citizens")
  arm <- dplyr::case_when(
    data$zdelib == 1 ~ "deliberation",
    data$zoinfo == 1 ~ "information",
    data$zspill == 1 ~ "spillover",
    data$z == 0 ~ "control"
  )
  tibble::tibble(
    study = "Tanzania 2015",
    id = seq_len(nrow(data)),
    arm = arm,
    k1 = as.numeric(data$H600),
    k2 = as.numeric(data$H601),
    cluster = as.character(data$VillageID)
  )
}

# Key from the study's expert panel (Mendelson et al. 2025, Table 14). The
# release stores "don't know" as blank, scored as not knowing.
amr_key <- c(4, 4, 5, 5, 4, 5)
amr_countries <- c("Brazil", "Colombia", "India", "Indonesia", "Nigeria", "Tanzania")

read_amr <- function(path) {
  data <- readr::read_csv(path, show_col_types = FALSE)
  data$k <- score_battery(data, paste0("knowledge_", 1:6), amr_key)
  data |>
    dplyr::select(ID, Group, Country, Weight, Time, k, education_ISCE) |>
    tidyr::pivot_wider(names_from = Time, values_from = k, names_prefix = "wave") |>
    dplyr::transmute(
      study = "Antimicrobial Resistance 2024",
      id = ID,
      treated = Group,
      country = amr_countries[Country],
      k1 = wave0,
      k2 = wave1,
      weight = Weight,
      ba = as.numeric(education_ISCE >= 6),
      cluster = ID
    )
}

# ANCOVA: T2 on treatment and T1, which is more precise than the gain-score
# difference-in-differences and unbiased under randomization. For the A1R
# studies, whose controls are a separate uninvited sample, it assumes that
# attendees and controls with the same T1 score would have changed alike.
effect <- function(data, contrast, treated_value, control_value, weights = NULL) {
  data <- data |>
    dplyr::filter(.data[[contrast]] %in% c(treated_value, control_value), !is.na(k1), !is.na(k2)) |>
    dplyr::mutate(treat = as.numeric(.data[[contrast]] == treated_value))
  w <- if (is.null(weights)) NULL else data[[weights]]
  fit <- stats::lm(k2 ~ treat + k1, data = data, weights = w)
  vc <- sandwich::vcovCL(fit, cluster = ~cluster, type = "HC1")
  gain_fit <- stats::lm(I(k2 - k1) ~ treat, data = data, weights = w)
  gain_vc <- sandwich::vcovCL(gain_fit, cluster = ~cluster, type = "HC1")
  control_sd <- stats::sd(data$k1[data$treat == 0])
  tibble::tibble(
    estimate = stats::coef(fit)[["treat"]],
    std_error = sqrt(vc["treat", "treat"]),
    gain_difference = stats::coef(gain_fit)[["treat"]],
    gain_difference_se = sqrt(gain_vc["treat", "treat"]),
    t1_difference = mean(data$k1[data$treat == 1]) - mean(data$k1[data$treat == 0]),
    control_t1_sd = control_sd,
    n_treated = sum(data$treat == 1),
    n_control = sum(data$treat == 0),
    treated_gain = mean(data$k2[data$treat == 1] - data$k1[data$treat == 1]),
    control_gain = mean(data$k2[data$treat == 0] - data$k1[data$treat == 0])
  )
}

control_effects <- function(paths) {
  a1r <- read_a1r(paths[["a1r"]])
  climate <- read_climate(paths[["climate"]])
  tanzania <- read_tanzania(paths[["tanzania"]])
  climate_t3 <- dplyr::mutate(climate, k2 = k3)
  amr <- read_amr(paths[["amr"]])
  amr_countries_list <- purrr::map(amr_countries, \(country) {
    list(
      dplyr::filter(amr, .data$country == .env$country), "treated", 1, 0, NULL,
      paste0("Attended vs randomized control: ", country), "percent"
    )
  })
  c(list(
    list(a1r, "treated", 1, 0, NULL, "Attended vs uninvited control", "percent"),
    list(a1r, "treated", 1, 0, "weight", "Attended vs uninvited control, weighted", "percent"),
    list(climate, "treated", 1, 0, NULL, "Attended vs uninvited control", "percent"),
    list(climate, "treated", 1, 0, "weight", "Attended vs uninvited control, weighted", "percent"),
    list(climate_t3, "treated", 1, 0, NULL, "Attended vs control, one year later", "percent"),
    list(tanzania, "arm", "deliberation", "control", NULL, "Deliberation vs control villages", "index"),
    list(tanzania, "arm", "information", "control", NULL, "Information vs control villages", "index"),
    list(tanzania, "arm", "deliberation", "information", NULL, "Deliberation vs information", "index"),
    list(amr, "treated", 1, 0, NULL, "Attended vs randomized control", "percent"),
    list(amr, "treated", 1, 0, "weight", "Attended vs randomized control, weighted", "percent")
  ), amr_countries_list) |>
    purrr::map(\(x) {
      effect(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]]) |>
        dplyr::mutate(study = x[[1]]$study[[1]], comparison = x[[6]], scale = x[[7]], .before = 1)
    }) |>
    purrr::list_rbind() |>
    dplyr::mutate(estimate_sd = estimate / control_t1_sd)
}

# Who shows up: T1 knowledge of attendees vs invitees who did not attend.
selection <- function(paths) {
  a1r <- readr::read_tsv(paths[["a1r"]], show_col_types = FALSE)
  climate <- readr::read_tsv(paths[["climate"]], show_col_types = FALSE)
  tibble::tibble(
    study = c(rep("America in One Room 2019", 2), rep("America in One Room: Climate 2021", 2)),
    group = rep(c("attended", "invited, did not attend"), 2),
    k1 = c(
      mean(score_battery(a1r, paste0("PK", 1:7), a1r_key)[a1r$CONDITION == 1 & a1r$POST == 1]),
      mean(score_battery(a1r, paste0("PK", 1:7), a1r_key)[a1r$CONDITION == 1 & a1r$POST == 0]),
      mean(score_battery(climate, paste0("Q", 17:24), climate_key)[climate$P_DELEGATE == 1]),
      mean(score_battery(climate, paste0("Q", 17:24), climate_key)[climate$P_DELEGATE == -99])
    ),
    n = c(
      sum(a1r$CONDITION == 1 & a1r$POST == 1), sum(a1r$CONDITION == 1 & a1r$POST == 0),
      sum(climate$P_DELEGATE == 1), sum(climate$P_DELEGATE == -99)
    )
  )
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
  fit <- stats::lm(k2 ~ treat * moderator + k1_centred, data = data)
  vc <- sandwich::vcovCL(fit, cluster = ~cluster, type = "HC1")
  term <- "treat:moderator"
  tibble::tibble(
    moderator = moderator,
    interaction = stats::coef(fit)[[term]],
    interaction_se = sqrt(vc[term, term]),
    n = stats::nobs(fit)
  )
}

control_heterogeneity <- function(paths) {
  a1r <- read_a1r(paths[["a1r"]])
  climate <- read_climate(paths[["climate"]])
  amr <- read_amr(paths[["amr"]])
  tanzania <- read_tanzania(paths[["tanzania"]])
  list(
    list(a1r, "treated", 1, 0, "k1"), list(a1r, "treated", 1, 0, "ba"),
    list(climate, "treated", 1, 0, "k1"), list(climate, "treated", 1, 0, "ba"),
    list(amr, "treated", 1, 0, "k1"), list(amr, "treated", 1, 0, "ba"),
    list(tanzania, "arm", "deliberation", "control", "k1")
  ) |>
    purrr::map(\(x) {
      heterogeneity(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]]) |>
        dplyr::mutate(study = x[[1]]$study[[1]], .before = 1)
    }) |>
    purrr::list_rbind()
}
