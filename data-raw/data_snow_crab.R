# Male-only eastern Bering Sea snow crab data and parameters for the
# catch-at-length model, based on the 2025 GMACS assessment model. The GMACS
# .dat, .ctl and .par files are copied unchanged into data-raw/snow_crab from
# Models/26_gmacs_male_only in https://github.com/szuwalski/snow_sept at
# commit b49b3f1ea78fa100c47de223dd560a71ac1dbd1f.
#
# data-raw/snow_crab also has an R version of GMACS (gmacs_male_only.R),
# which reproduces the GMACS objective at gmacs.par. It gives the reference
# values in data_snow_crab_results:
# * "gmacs": the R version of GMACS in its GMACS form at gmacs.par.
# * "gmacs_par": the FIMS form at gmacs.par.
# * "fit": the FIMS form, fit with nlminb.
# The FIMS form keeps the GMACS robust normal approximation to the
# multinomial and the smoothness penalties on survey selectivity and initial
# numbers. It differs from GMACS in these ways:
# * The index likelihood has its full constant and no additional survey CV.
# * There is no recruitment bias correction and no first-difference penalty
#   on the recruitment deviations.
# * There is no penalty on the mean log F deviation, because FIMS has 1 log F
#   per year and no mean F. The R version of GMACS fixes log mean F at its
#   GMACS value instead.
# * There are no constants for uniform priors.
# * Logistic selectivity is not divided by its value in the last class.
# * Catch compositions come from Baranov catch with F and the Z of each
#   maturity group, and there are none in years with no catch.
# * Survey selectivity is smoothed on the logit scale.
# * The base immature M offset is fixed at its lower bound of -1, and its
#   2019 block is still estimated.
# The R version of GMACS in its FIMS form still gives the first recruitment
# deviation sd sigma R, where FIMS gives it the stationary AR1 sd,
# sigma R / sqrt(1 - rho^2). With rho = 0.01 the difference is negligible.
#
# Units: GMACS numbers are in thousands of crabs, weight at length in metric
# tons per crab, and catch and biomass in thousands of metric tons. FIMS
# numbers are in crabs and catch and biomass in metric tons. So catch and
# index values are multiplied by 1000, weight at length keeps its values, and
# log recruitment and log initial numbers increase by log(1000).
#
# Run from the package root with FIMS installed or loaded with
# devtools::load_all(). It takes about 10 seconds. It stops if the R version
# of GMACS no longer gives the objective in gmacs.par, or if the refit does
# not converge.

# The data workflow runs this script after every push. Saving only when the
# values change keeps it from committing .rda files that differ only by
# optimizer noise between runners, as in data_big.R. all.equal() compares the
# mean relative difference, so a small real change can stay under the
# tolerance. Delete the .rda file to force a save.
saved_data_changed <- function(object, name,
                               tolerance = sqrt(.Machine[["double.eps"]])) {
  file <- file.path("data", paste0(name, ".rda"))
  if (!file.exists(file)) {
    return(TRUE)
  }
  saved <- new.env(parent = emptyenv())
  load(file, envir = saved)
  !isTRUE(all.equal(object, saved[[name]], tolerance = tolerance))
}

source(file.path("data-raw", "snow_crab", "read_gmacs.R"))
source(file.path("data-raw", "snow_crab", "gmacs_male_only.R"))
source(file.path("data-raw", "snow_crab", "fit_gmacs.R"))

gmacs_dir <- file.path("data-raw", "snow_crab")
gmacs_data <- read_gmacs_dat(file.path(gmacs_dir, "26_snow_male_only.dat"))
gmacs_control <- read_gmacs_ctl(file.path(gmacs_dir, "snow.ctl"), gmacs_data)
gmacs_par <- read_gmacs_par(file.path(gmacs_dir, "gmacs.par"))
model_data <- gmacs_model_data(gmacs_data, gmacs_control)
gmacs_values <- gmacs_parameters(gmacs_par[["values"]], model_data)
gmacs_fixed_values <- gmacs_constants(gmacs_par[["values"]])
gmacs_fit <- gmacs_objective(
  gmacs_values, model_data, gmacs_fixed_values, gmacs_options()
)
# gmacs.par gives the objective to 10 decimal places.
objective_difference <- gmacs_fit[["total"]] - gmacs_par[["objective"]]
if (abs(objective_difference) > 1e-6) {
  cli::cli_abort(c(
    "x" = "The R version of GMACS differs from the gmacs.par objective by
      {.val {objective_difference}}.",
    "i" = "It must match to 1e-6.",
    "i" = "Check the files in {.file data-raw/snow_crab} against the source
      commit."
  ))
}
# The GMACS estimate of the immature M offset sits on its lower bound of -1,
# and the FIMS form fixes it there.
gmacs_values[["m_immature_offset"]] <- -1

years <- model_data[["years"]]
n_lengths <- model_data[["n_lengths"]]
lower_edges <- model_data[["length_edges"]][seq_len(n_lengths)]
log_number_scale <- log(1000)

# 1 data row per value, with the columns FIMSFrame() needs plus `maturity`.
data_rows <- function(type, fleet, timing, length, maturity, observed, unit,
                      uncertainty) {
  tibble::tibble(
    type = type,
    fleet = fleet,
    timing = timing,
    age = NA_real_,
    length = length,
    maturity = maturity,
    observed = observed,
    unit = unit,
    uncertainty = uncertainty
  )
}

# Data ---------------------------------------------------------------------

# Weight at length: row 1 of the GMACS weight matrix is mature, row 2
# immature.
weight_rows <- dplyr::bind_rows(
  data_rows(
    "weight_at_length", NA_character_, NA_real_, lower_edges, "immature",
    as.numeric(model_data[["weight_at_length"]][2, ]), "mt", NA_character_
  ),
  data_rows(
    "weight_at_length", NA_character_, NA_real_, lower_edges, "mature",
    as.numeric(model_data[["weight_at_length"]][1, ]), "mt", NA_character_
  )
)

# Probability of the terminal molt to maturity at post-molt length, for each
# model year and the year after.
molt_years <- c(years, max(years) + 1)
terminal_molt_rows <- data_rows(
  "terminal_molt_probability_at_length", NA_character_,
  rep(molt_years, each = n_lengths),
  rep(lower_edges, length(molt_years)), "immature",
  as.vector(t(model_data[["terminal_molt_probability"]])), "proportion",
  NA_character_
)

# Retained and discarded catch. GMACS gives a CV, and sdlog is
# sqrt(log(1 + CV^2)). Years with no catch get a 0, so
# setup_default_parameters() holds log_Fmort constant in those years.
catch_rows <- purrr::map(model_data[["catch"]], function(rows) {
  catch_type <- switch(as.character(unique(rows[["type"]])),
    "0" = "catch",
    "1" = "catch_retained",
    "2" = "catch_discarded"
  )
  uncertainty <- paste0(
    "~ dlnorm(meanlog = log_", catch_type, "_expected, sdlog = ",
    format(sqrt(log(1 + rows[["cv"]]^2)), digits = 16, trim = TRUE), ")"
  )
  fleet_name <- gmacs_data[["fleet_names"]][unique(rows[["fleet"]])]
  closed_years <- setdiff(years, rows[["year"]])
  dplyr::bind_rows(
    data_rows(
      catch_type, fleet_name, rows[["year"]], NA_real_, NA_character_,
      rows[["observed"]] * 1000, "mt", uncertainty
    ),
    data_rows(
      catch_type, fleet_name, closed_years, NA_real_, NA_character_,
      rep(0, length(closed_years)), "mt",
      rep(uncertainty[1], length(closed_years))
    )
  ) |>
    dplyr::arrange(.data[["timing"]])
}) |>
  dplyr::bind_rows()

# Survey biomass of mature males.
index <- model_data[["index"]]
index_rows <- data_rows(
  "index", gmacs_data[["fleet_names"]][index[["fleet"]]], index[["year"]],
  NA_real_,
  dplyr::case_when(
    index[["maturity"]] == 1 ~ "mature",
    index[["maturity"]] == 2 ~ "immature",
    .default = NA_character_
  ),
  index[["observed"]] * 1000, "mt",
  paste0(
    "~ dlnorm(meanlog = log_index_expected, sdlog = ",
    format(sqrt(log(1 + index[["cv"]]^2)), digits = 16, trim = TRUE), ")"
  )
)

# Length compositions. Catch compositions in years with no catch (pot
# fishery 2022 and 2023) are left out. Each puts all its weight in 1 or 2
# length bins but has a sample size of 100, and FIMS predicts no catch in
# those years.
composition_rows <- model_data[["compositions"]] |>
  purrr::map(function(composition) {
    info <- composition[["info"]]
    observed <- composition[["observed"]]
    fleet_number <- unique(info[["fleet"]])
    if (composition[["catch_like"]]) {
      fleet_index <- match(fleet_number, model_data[["fishing_fleets"]])
      keep <- info[["year"]] %in% model_data[["f_years"]][[fleet_index]]
      info <- info[keep, , drop = FALSE]
      observed <- observed[keep, , drop = FALSE]
    }
    catch_type <- unique(info[["type"]])
    expected_name <- dplyr::case_when(
      !composition[["catch_like"]] || catch_type == 0 ~ "lengthcomp_proportion",
      catch_type == 1 ~ "lengthcomp_retained_proportion",
      .default = "lengthcomp_discarded_proportion"
    )
    data_rows(
      "length_comp", gmacs_data[["fleet_names"]][fleet_number],
      rep(info[["year"]], each = n_lengths),
      rep(lower_edges, nrow(info)),
      switch(as.character(unique(info[["maturity"]])),
        "0" = NA_character_,
        "1" = "mature",
        "2" = "immature"
      ),
      as.vector(t(observed)), "proportion",
      rep(
        paste0(
          "~ robust_normal_multinomial(prob = ", expected_name, ", size = ",
          format(info[["sample_size"]], digits = 16, trim = TRUE), ")"
        ),
        each = n_lengths
      )
    )
  }) |>
  dplyr::bind_rows()

# Molt increments by premolt length. GMACS uses the CV as sdlog.
growth_observations <- model_data[["growth"]]
growth_rows <- data_rows(
  "growth_increment", NA_character_, NA_real_,
  growth_observations[["premolt_length"]], "immature",
  growth_observations[["increment"]], "mm",
  paste0(
    "~ dlnorm(meanlog = log_growth_increment_expected, sdlog = ",
    format(growth_observations[["cv"]], digits = 16, trim = TRUE), ")"
  )
)

data_snow_crab <- dplyr::bind_rows(
  weight_rows, terminal_molt_rows, catch_rows, index_rows, composition_rows,
  growth_rows
)

if (saved_data_changed(data_snow_crab, "data_snow_crab")) {
  usethis::use_data(data_snow_crab, overwrite = TRUE)
}

# Parameters at gmacs.par --------------------------------------------------

frame <- FIMS::FIMSFrame(data_snow_crab)

# GMACS logistic selectivity is (log inflection point, log scale), where the
# scale is 1 / slope.
logistic_values <- function(gmacs_pair) {
  c(exp(gmacs_pair[1]), 1 / exp(gmacs_pair[2]))
}
# GMACS survey selectivity is log selectivity, and FIMS uses its logit.
logit_from_log <- function(log_selectivity) {
  log_selectivity - log(1 - exp(log_selectivity))
}

selectivity_rows <- dplyr::bind_rows(
  FIMS::setup_default_Selectivity(frame, "Pot_Fishery", "Logistic") |>
    dplyr::mutate(value = logistic_values(gmacs_values[["selectivity_pot"]])),
  FIMS::setup_default_Selectivity(frame, "Trawl_Bycatch", "Logistic") |>
    dplyr::mutate(value = logistic_values(gmacs_values[["selectivity_trawl"]])),
  FIMS::setup_default_Selectivity(frame, "NMFS_Trawl_1982", "LengthSpecific") |>
    dplyr::mutate(
      value = logit_from_log(gmacs_values[["log_selectivity_nmfs_1982"]])
    ),
  FIMS::setup_default_Selectivity(frame, "NMFS_Trawl_1989", "LengthSpecific") |>
    dplyr::mutate(
      value = logit_from_log(gmacs_values[["log_selectivity_nmfs_1989"]])
    )
) |>
  dplyr::mutate(estimation_type = "fixed_effects")

retention_rows <- frame |>
  FIMS::setup_default_Retention("Pot_Fishery", "Logistic") |>
  dplyr::mutate(
    value = logistic_values(gmacs_values[["retention_pot"]]),
    estimation_type = "fixed_effects"
  )

# GMACS log F is log mean F plus a deviation in each year with catch. In years
# with no catch log_Fmort stays at its constant default of -200.
fleet_log_f <- function(fleet_index, log_f_deviations) {
  log_f <- rep(-200, length(years))
  catch_years <- match(model_data[["f_years"]][[fleet_index]], years)
  log_f[catch_years] <- gmacs_values[["log_mean_f"]][fleet_index] +
    log_f_deviations
  log_f
}
fishing_fleet_rows <- function(fleet, log_f, discard_mortality) {
  FIMS::setup_default_Fleet(frame, fleet) |>
    dplyr::mutate(
      value = dplyr::case_when(
        .data[["label"]] == "log_Fmort" ~
          log_f[match(.data[["timing"]], years)],
        .data[["label"]] == "discard_mortality" ~ discard_mortality,
        .default = .data[["value"]]
      )
    )
}
fleet_rows <- dplyr::bind_rows(
  fishing_fleet_rows(
    "Pot_Fishery", fleet_log_f(1, gmacs_values[["log_f_deviations_pot"]]),
    model_data[["discard_mortality"]][1]
  ),
  fishing_fleet_rows(
    "Trawl_Bycatch", fleet_log_f(2, gmacs_values[["log_f_deviations_trawl"]]),
    model_data[["discard_mortality"]][2]
  ),
  # GMACS fixes survey catchability at 1.
  FIMS::setup_default_Fleet(frame, "NMFS_Trawl_1982") |>
    dplyr::mutate(estimation_type = "constant"),
  FIMS::setup_default_Fleet(frame, "NMFS_Trawl_1989") |>
    dplyr::mutate(estimation_type = "constant")
)

# Recruits enter the 3 smallest length bins with a gamma size distribution,
# and log_r has an AR1 process with sd and rho fixed at their GMACS values.
# GMACS log mean recruitment is the process mean.
recruitment_rows <- dplyr::bind_rows(
  FIMS:::setup_default_parameters_template(n_parameters = 2) |>
    dplyr::mutate(
      module_name = "Recruitment",
      module_type = "GammaLength",
      label = c("log_recruit_alpha", "log_recruit_beta"),
      value = log(c(
        gmacs_fixed_values[["recruitment_length_mean"]],
        gmacs_fixed_values[["recruitment_length_scale"]]
      )),
      estimation_type = "constant"
    ),
  FIMS:::setup_default_parameters_template(n_parameters = length(years)) |>
    dplyr::mutate(
      module_name = "Recruitment",
      module_type = "GammaLength",
      label = "log_r",
      timing = years,
      value = gmacs_values[["log_mean_recruitment"]] +
        gmacs_values[["recruitment_deviations"]] +
        log_number_scale,
      estimation_type = "fixed_effects"
    )
) |>
  FIMS::add_process(
    log_r ~ 1 + ar1(1 | year),
    module_name = "Recruitment",
    estimation_type = "fixed_effects",
    sd = list(
      value = exp(gmacs_fixed_values[["log_recruitment_sd"]]),
      estimation_type = "constant"
    ),
    rho = list(
      value = gmacs_fixed_values[["recruitment_rho"]],
      estimation_type = "constant"
    )
  ) |>
  dplyr::mutate(
    value = dplyr::if_else(
      .data[["label"]] == "expected_mean" &
        .data[["target_label"]] %in% "log_r",
      gmacs_values[["log_mean_recruitment"]] + log_number_scale,
      .data[["value"]]
    )
  )

# GMACS writes the mean molt increment as alpha - beta * premolt length, and
# FIMS as alpha + beta * premolt length.
growth_rows <- FIMS::setup_default_Growth(frame, "LinearGammaIncrement") |>
  dplyr::mutate(
    value = c(
      gmacs_values[["growth_alpha"]],
      -gmacs_values[["growth_beta"]],
      gmacs_fixed_values[["growth_scale"]]
    )
  )

# Mature M has its own value in 2018 and in 2019 (map_group). The immature M
# offset is log(immature M / first-year mature M).
log_m <- rep(log(gmacs_values[["m_mature"]]), length(years))
log_m[years == 2018] <- log_m[years == 2018] +
  gmacs_values[["m_mature_block"]][1]
log_m[years == 2019] <- log_m[years == 2019] +
  gmacs_values[["m_mature_block"]][2]
immature_offset <- rep(gmacs_values[["m_immature_offset"]], length(years))
immature_offset[years == 2019] <- immature_offset[years == 2019] +
  gmacs_values[["m_immature_block"]][2]
population_rows <- dplyr::bind_rows(
  FIMS:::setup_default_parameters_template(n_parameters = length(years)) |>
    dplyr::mutate(
      module_name = "Population",
      label = "log_M",
      timing = years,
      value = log_m,
      estimation_type = "fixed_effects",
      map_group = dplyr::case_when(
        years == 2018 ~ 2L,
        years == 2019 ~ 3L,
        .default = 1L
      )
    ),
  FIMS:::setup_default_parameters_template(n_parameters = length(years)) |>
    dplyr::mutate(
      module_name = "Population",
      label = "log_M_immature_offset",
      timing = years,
      value = immature_offset,
      estimation_type = dplyr::if_else(
        years == 2019, "fixed_effects", "constant"
      )
    ),
  FIMS:::setup_default_parameters_template(n_parameters = n_lengths) |>
    dplyr::mutate(
      module_name = "Population",
      label = "log_init_nal_immature",
      length = lower_edges,
      value = gmacs_values[["log_initial_numbers_immature"]] + log_number_scale,
      estimation_type = "fixed_effects"
    ),
  FIMS:::setup_default_parameters_template(n_parameters = n_lengths) |>
    dplyr::mutate(
      module_name = "Population",
      label = "log_init_nal_mature",
      length = lower_edges,
      value = gmacs_values[["log_initial_numbers_mature"]] + log_number_scale,
      estimation_type = "fixed_effects"
    )
)

# GMACS smoothness penalties with weight w are random walks by length with
# sd = 1 / sqrt(w).
penalty_weights <- model_data[["penalty_emphasis"]]
selectivity_sd <- 1 / sqrt(penalty_weights[["free_selectivity"]])
initial_numbers_sd <- 1 / sqrt(penalty_weights[["initial_numbers_at_length"]])

# GMACS normal priors, on the natural scale of each parameter. Up to a
# constant, they are the GMACS priors on the parameters FIMS estimates.
selectivity_parameters <- model_data[["selectivity_parameters"]]
prior_1982 <- selectivity_parameters[4 + seq_len(n_lengths), ]
prior_1989 <- selectivity_parameters[4 + n_lengths + seq_len(n_lengths), ]
prior_retention <- selectivity_parameters[4 + 2 * n_lengths + 1, ]
prior_m <- model_data[["m_parameters"]][1, ]

parameters_snow_crab <- dplyr::bind_rows(
  selectivity_rows, retention_rows, fleet_rows, recruitment_rows,
  growth_rows, population_rows
) |>
  FIMS::add_process(
    logit_sel_at_length ~ rw(1 | length),
    module_name = "Selectivity",
    fleet = "NMFS_Trawl_1982",
    estimation_type = "fixed_effects",
    sd = list(value = selectivity_sd, estimation_type = "constant")
  ) |>
  FIMS::add_process(
    logit_sel_at_length ~ rw(1 | length),
    module_name = "Selectivity",
    fleet = "NMFS_Trawl_1989",
    estimation_type = "fixed_effects",
    sd = list(value = selectivity_sd, estimation_type = "constant")
  ) |>
  FIMS::add_process(
    log_init_nal_immature ~ rw(1 | length),
    module_name = "Population",
    estimation_type = "fixed_effects",
    sd = list(value = initial_numbers_sd, estimation_type = "constant")
  ) |>
  FIMS::add_process(
    log_init_nal_mature ~ rw(1 | length),
    module_name = "Population",
    estimation_type = "fixed_effects",
    sd = list(value = initial_numbers_sd, estimation_type = "constant")
  ) |>
  FIMS::add_priors(
    log_M ~ normal(
      mean = dplyr::if_else(year == years[1], prior_m[["prior_1"]], NA_real_),
      sd = dplyr::if_else(year == years[1], prior_m[["prior_2"]], NA_real_)
    ),
    module_name = "Population",
    transform = "exp"
  ) |>
  FIMS::add_priors(
    logit_sel_at_length ~ normal(
      mean = prior_1982[["prior_1"]],
      sd = prior_1982[["prior_2"]]
    ),
    module_name = "Selectivity",
    fleet = "NMFS_Trawl_1982",
    transform = "inv_logit"
  ) |>
  FIMS::add_priors(
    logit_sel_at_length ~ normal(
      mean = prior_1989[["prior_1"]],
      sd = prior_1989[["prior_2"]]
    ),
    module_name = "Selectivity",
    fleet = "NMFS_Trawl_1989",
    transform = "inv_logit"
  ) |>
  FIMS::add_priors(
    inflection_point ~ normal(
      mean = prior_retention[["prior_1"]],
      sd = prior_retention[["prior_2"]]
    ),
    module_name = "Retention",
    fleet = "Pot_Fishery"
  )

if (saved_data_changed(parameters_snow_crab, "parameters_snow_crab")) {
  usethis::use_data(parameters_snow_crab, overwrite = TRUE)
}

# Reference values ---------------------------------------------------------

fims_form <- gmacs_options(
  index_constant = TRUE,
  index_additional_cv = FALSE,
  recruitment_bias_correction = FALSE,
  uniform_priors = FALSE,
  normalize_logistic = FALSE,
  gmacs_catch_compositions = FALSE,
  mean_f_deviation_penalty = FALSE,
  recruitment_first_difference = FALSE,
  smoothing_scale = "logit"
)
fims_form_at_gmacs_par <- gmacs_objective(
  gmacs_values, model_data, gmacs_fixed_values, fims_form
)
# FIMS has no mean log F, so log mean F is fixed at its GMACS value and the
# deviations carry log F. Without that, log mean F and the deviations have a
# flat direction and the Hessian is singular.
fims_form_fit <- fit_gmacs(
  gmacs_values, model_data, gmacs_fixed_values, fims_form,
  extra_map = list(
    m_immature_offset = factor(NA),
    log_mean_f = factor(c(NA, NA))
  )
)
if (fims_form_fit[["max_gradient"]] > 1e-5) {
  cli::cli_abort(c(
    "x" = "The refit of the FIMS form did not converge.",
    "i" = "Its largest gradient is {.val {fims_form_fit[['max_gradient']]}}.",
    "i" = "It must be below 1e-5."
  ))
}

# Objective, spawning biomass (mature male biomass, metric tons),
# recruitment (crabs), and expected catch and survey biomass (metric tons)
# for 1 model case.
reference_values <- function(model, objective, quantities) {
  catch_values <- purrr::map2(
    model_data[["catch"]], quantities[["catch_expected"]],
    function(rows, predicted) {
      catch_type <- switch(as.character(unique(rows[["type"]])),
        "0" = "catch",
        "1" = "catch_retained",
        "2" = "catch_discarded"
      )
      tibble::tibble(
        label = paste0(catch_type, "_expected"),
        fleet = gmacs_data[["fleet_names"]][rows[["fleet"]]],
        year = rows[["year"]],
        value = predicted * 1000
      )
    }
  )
  dplyr::bind_rows(
    tibble::tibble(
      label = c(
        "objective", rep("spawning_biomass", length(years)),
        rep("expected_recruitment", length(years))
      ),
      year = c(NA_real_, years, years),
      value = c(
        objective, quantities[["mature_male_biomass"]] * 1000,
        quantities[["recruits"]] * 1000
      )
    ),
    catch_values,
    tibble::tibble(
      label = "index_expected",
      fleet = gmacs_data[["fleet_names"]][index[["fleet"]]],
      year = index[["year"]],
      value = quantities[["index_expected"]] * 1000
    )
  ) |>
    dplyr::mutate(model = model, .before = 1)
}
data_snow_crab_results <- dplyr::bind_rows(
  reference_values("gmacs", gmacs_fit[["total"]], gmacs_fit[["quantities"]]),
  reference_values(
    "gmacs_par", fims_form_at_gmacs_par[["total"]],
    fims_form_at_gmacs_par[["quantities"]]
  ),
  reference_values(
    "fit", fims_form_fit[["objective"]],
    fims_form_fit[["report"]][["quantities"]]
  )
)

if (saved_data_changed(data_snow_crab_results, "data_snow_crab_results")) {
  usethis::use_data(data_snow_crab_results, overwrite = TRUE)
}
