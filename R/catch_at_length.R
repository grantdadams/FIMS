# Catch-at-length model: predictions
#
# catch_at_length() predicts biology, mortality, population numbers at length
# by maturity stage, and each fleet's catch, index, and length compositions.
# fims_model() (R/model.R) calls it, then adds the likelihood and reports.
#
# Within-year order (3 seasons with natural mortality fractions m_fractions):
# 1. Season 1 (start of year): survey index and survey length compositions,
#    followed by natural mortality M * m_fractions[1].
# 2. Season 2 (fishery): continuous fishing mortality F and natural mortality
#    M * m_fractions[2]. Baranov catch (total, retained, and discarded) and
#    fishery length compositions come from season 2 numbers and mortality.
# 3. Season 3 (spawning, molting, and recruitment): mature male biomass
#    (spawning_biomass) at the start of season 3, followed by natural
#    mortality M * m_fractions[3], immature molting (LinearGammaIncrement
#    growth transition), terminal molt to mature, and recruitment into the
#    immature stage.

#' Predictions of the catch-at-length model
#'
#' @param module_parameters Parameters grouped by module (group_parameters()).
#' @param model_data The `model_data` element of initialize_fims().
#' @return A list with `biology`, `population`, and `fleets` (a list by fleet).
#' @noRd
catch_at_length <- function(module_parameters, model_data) {
  population_data <- model_data[["populations"]][[1]]
  biology <- catch_at_length_biology(
    module_parameters, model_data, population_data
  )
  population <- catch_at_length_dynamics(
    module_parameters, model_data, population_data, biology
  )
  list(
    biology = biology,
    population = population,
    fleets = catch_at_length_fleet_predictions(
      module_parameters, model_data, biology, population
    )
  )
}

# Biology and mortality ------------------------------------------------------

#' Natural mortality, growth transition, selectivity, retention, and fishing
#' mortality for the catch-at-length model
#' @noRd
catch_at_length_biology <- function(module_parameters, model_data,
                                    population_data) {
  "c" <- RTMB::ADoverload("c")
  n_years <- model_data[["n_years"]]
  n_lengths <- model_data[["n_lengths"]]
  length_midpoints <- population_data[["length_midpoints"]]
  module_types <- population_data[["module_types"]]
  population_parameters <- module_parameters[["Population"]]

  log_m_mature <- per_year(c(population_parameters[["log_M"]]), n_years)
  mortality_m_mature <- exp(log_m_mature)
  mortality_m_immature <- if (!is.null(population_parameters[["log_M_immature_offset"]])) {
    exp(
      c(population_parameters[["log_M"]])[1] +
        per_year(c(population_parameters[["log_M_immature_offset"]]), n_years)
    )
  } else if (!is.null(population_parameters[["log_M_immature"]])) {
    exp(per_year(c(population_parameters[["log_M_immature"]]), n_years))
  } else {
    mortality_m_mature
  }
  mortality_M <- list(
    immature = RTMB::matrix(mortality_m_immature, n_years, n_lengths),
    mature = RTMB::matrix(mortality_m_mature, n_years, n_lengths)
  )

  growth <- do.call(
    growth_modules[[module_types[["growth"]]]][["evaluate"]],
    c(population_data[["growth_inputs"]], module_parameters[["Growth"]])
  )
  weight_at_length <- population_data[["weight_at_length"]]
  terminal_molt_probability <- population_data[["terminal_molt_probability"]]

  selectivity <- list()
  selectivity_extended <- list()
  retention <- list()
  discard_mortality <- list()
  annual_f <- list()
  catchability_extended <- list()
  fishing_mortality_at_length <- list()

  for (fleet in names(model_data[["fleets"]])) {
    fleet_data <- model_data[["fleets"]][[fleet]]
    fleet_module_types <- fleet_data[["module_types"]]
    selectivity_fleet <- evaluate_by_year(
      selectivity_functions, fleet_module_types[["selectivity"]],
      length_midpoints, module_parameters[["Selectivity"]][[fleet]], n_years
    )
    selectivity[[fleet]] <- selectivity_fleet
    selectivity_extended[[fleet]] <- rbind(
      selectivity_fleet, selectivity_fleet[n_years, ]
    )

    retention_type <- fleet_module_types[["retention"]]
    retention[[fleet]] <- if (!is.null(retention_type) && length(retention_type) > 0) {
      evaluate_by_year(
        retention_functions, retention_type, length_midpoints,
        module_parameters[["Retention"]][[fleet]], n_years
      )
    } else if (isTRUE(fleet_data[["has_catch_discarded"]]) &&
      !isTRUE(fleet_data[["has_catch_retained"]])) {
      matrix(0, n_years, n_lengths)
    } else {
      matrix(1, n_years, n_lengths)
    }

    fleet_parameters <- module_parameters[["Fleet"]][[fleet]]
    fleet_discard_mortality <- if (is.null(fleet_parameters[["discard_mortality"]])) {
      1
    } else {
      c(fleet_parameters[["discard_mortality"]])[1]
    }
    discard_mortality[[fleet]] <- fleet_discard_mortality

    fleet_annual_f <- per_year(exp(c(fleet_parameters[["log_Fmort"]])), n_years)
    annual_f[[fleet]] <- fleet_annual_f
    vulnerability <- selectivity_fleet * (
      retention[[fleet]] +
        (1 - retention[[fleet]]) * fleet_discard_mortality
    )
    fishing_mortality_at_length[[fleet]] <- fleet_annual_f * vulnerability

    fleet_catchability <- per_year(exp(c(fleet_parameters[["log_q"]])), n_years)
    catchability_extended[[fleet]] <- c(
      fleet_catchability, fleet_catchability[n_years]
    )
  }

  mortality_F <- Reduce(`+`, fishing_mortality_at_length)
  m_fractions <- population_data[["m_fractions"]]
  mortality_Z_seasons <- lapply(mortality_M, function(stage_m) {
    list(
      m_fractions[1] * stage_m,
      m_fractions[2] * stage_m + mortality_F,
      m_fractions[3] * stage_m
    )
  })
  mortality_Z <- list(
    immature = mortality_M[["immature"]] + mortality_F,
    mature = mortality_M[["mature"]] + mortality_F
  )

  list(
    mortality_M = mortality_M,
    growth = growth,
    weight_at_length = weight_at_length,
    terminal_molt_probability = terminal_molt_probability,
    selectivity = selectivity,
    selectivity_extended = selectivity_extended,
    retention = retention,
    discard_mortality = discard_mortality,
    annual_f = annual_f,
    catchability_extended = catchability_extended,
    fishing_mortality_at_length = fishing_mortality_at_length,
    mortality_F = mortality_F,
    mortality_Z = mortality_Z,
    mortality_Z_seasons = mortality_Z_seasons,
    sum_selectivity = Reduce(`+`, selectivity)
  )
}

# Population dynamics --------------------------------------------------------

#' Numbers at length by maturity stage, recruitment, and mature biomass
#' @noRd
catch_at_length_dynamics <- function(module_parameters, model_data,
                                     population_data, biology) {
  "[<-" <- RTMB::ADoverload("[<-")
  "c" <- RTMB::ADoverload("c")
  n_years <- model_data[["n_years"]]
  n_lengths <- model_data[["n_lengths"]]
  population_parameters <- module_parameters[["Population"]]
  recruitment_parameters <- module_parameters[["Recruitment"]]

  mortality_Z_seasons <- biology[["mortality_Z_seasons"]]
  growth_transition <- biology[["growth"]][["growth_transition"]]
  weight_at_length <- biology[["weight_at_length"]]
  terminal_molt_probability <- biology[["terminal_molt_probability"]]

  recruit_length_distribution <- gamma_length_recruitment_distribution(
    length_edges = population_data[["length_edges"]],
    recruit_gamma_shape = exp(recruitment_parameters[["log_recruit_alpha"]]),
    recruit_gamma_scale = exp(recruitment_parameters[["log_recruit_beta"]]),
    max_recruit_length_bin = population_data[["max_recruit_length_bin"]]
  )
  expected_recruitment <- per_year(exp(c(recruitment_parameters[["log_r"]])), n_years)

  numbers_at_length <- list(
    immature = matrix(0, n_years + 1, n_lengths),
    mature = matrix(0, n_years + 1, n_lengths)
  )
  numbers_season2 <- list(
    immature = matrix(0, n_years, n_lengths),
    mature = matrix(0, n_years, n_lengths)
  )
  spawning_biomass <- numeric(n_years)

  current_immature <- exp(c(population_parameters[["log_init_nal_immature"]]))
  current_mature <- exp(c(population_parameters[["log_init_nal_mature"]]))

  for (year in seq_len(n_years)) {
    # Season 1: survey at the start of the year, then pre-fishery M.
    numbers_at_length[["immature"]][year, ] <- current_immature
    numbers_at_length[["mature"]][year, ] <- current_mature
    immature_after_season1 <- current_immature *
      exp(-mortality_Z_seasons[["immature"]][[1]][year, ])
    mature_after_season1 <- current_mature *
      exp(-mortality_Z_seasons[["mature"]][[1]][year, ])

    # Season 2: fishery and concurrent natural mortality.
    numbers_season2[["immature"]][year, ] <- immature_after_season1
    numbers_season2[["mature"]][year, ] <- mature_after_season1
    immature_after_season2 <- immature_after_season1 *
      exp(-mortality_Z_seasons[["immature"]][[2]][year, ])
    mature_after_season2 <- mature_after_season1 *
      exp(-mortality_Z_seasons[["mature"]][[2]][year, ])

    # Season 3: mature biomass at the start of season 3, then post-fishery M,
    # molting, terminal molt, and recruitment.
    spawning_biomass[year] <- sum(
      mature_after_season2 * weight_at_length[year, ]
    )
    immature_survived <- immature_after_season2 *
      exp(-mortality_Z_seasons[["immature"]][[3]][year, ])
    mature_survived <- mature_after_season2 *
      exp(-mortality_Z_seasons[["mature"]][[3]][year, ])

    molted <- c(immature_survived %*% growth_transition)
    maturing <- molted * terminal_molt_probability[year, ]
    current_immature <- molted * (1 - terminal_molt_probability[year, ]) +
      expected_recruitment[year] * recruit_length_distribution
    current_mature <- mature_survived + maturing
  }
  numbers_at_length[["immature"]][n_years + 1, ] <- current_immature
  numbers_at_length[["mature"]][n_years + 1, ] <- current_mature

  total_numbers_at_length <- numbers_at_length[["immature"]] +
    numbers_at_length[["mature"]]
  population <- list(
    numbers_at_length = numbers_at_length,
    numbers_at_length_immature = numbers_at_length[["immature"]],
    numbers_at_length_mature = numbers_at_length[["mature"]],
    total_numbers_at_length = total_numbers_at_length,
    numbers_season2 = numbers_season2,
    expected_recruitment = expected_recruitment,
    recruit_length_distribution = recruit_length_distribution,
    spawning_biomass = spawning_biomass,
    biomass = RTMB::rowSums(total_numbers_at_length * weight_at_length)
  )
  if (!is.null(biology[["growth"]][["growth_increment_expected"]])) {
    population[["growth_increment_expected"]] <-
      biology[["growth"]][["growth_increment_expected"]]
    population[["log_growth_increment_expected"]] <-
      biology[["growth"]][["log_growth_increment_expected"]]
  }
  population
}

# Fleet predictions ----------------------------------------------------------

#' Catch, index, and length compositions by fleet for the catch-at-length model
#' @noRd
catch_at_length_fleet_predictions <- function(module_parameters, model_data,
                                              biology, population) {
  n_years <- model_data[["n_years"]]
  weight_model_years <- biology[["weight_at_length"]][seq_len(n_years), , drop = FALSE]
  numbers_season2_immature <- population[["numbers_season2"]][["immature"]]
  numbers_season2_mature <- population[["numbers_season2"]][["mature"]]
  mortality_Z_season2_immature <- biology[["mortality_Z_seasons"]][["immature"]][[2]]
  mortality_Z_season2_mature <- biology[["mortality_Z_seasons"]][["mature"]][[2]]

  baranov_catch <- function(annual_f, capture_selectivity) {
    fishing_mortality_at_length <- annual_f * capture_selectivity
    numbers_season2_immature * fishing_mortality_at_length *
      (1 - exp(-mortality_Z_season2_immature)) / mortality_Z_season2_immature +
      numbers_season2_mature * fishing_mortality_at_length *
        (1 - exp(-mortality_Z_season2_mature)) / mortality_Z_season2_mature
  }

  predictions <- list()
  for (fleet in names(model_data[["fleets"]])) {
    fleet_data <- model_data[["fleets"]][[fleet]]
    annual_f <- biology[["annual_f"]][[fleet]]
    selectivity_fleet <- biology[["selectivity"]][[fleet]]
    retention_fleet <- biology[["retention"]][[fleet]]

    catch_numbers_at_length <- baranov_catch(annual_f, selectivity_fleet)
    catch_retained_numbers_at_length <- baranov_catch(
      annual_f, selectivity_fleet * retention_fleet
    )
    catch_discarded_numbers_at_length <- baranov_catch(
      annual_f, selectivity_fleet * (1 - retention_fleet)
    )

    catch_weight_at_length <- catch_numbers_at_length * weight_model_years
    catch_retained_weight_at_length <-
      catch_retained_numbers_at_length * weight_model_years
    catch_discarded_weight_at_length <-
      catch_discarded_numbers_at_length * weight_model_years

    catch_numbers <- RTMB::rowSums(catch_numbers_at_length)
    catch_weight <- RTMB::rowSums(catch_weight_at_length)
    catch_retained_numbers <- RTMB::rowSums(catch_retained_numbers_at_length)
    catch_retained_weight <- RTMB::rowSums(catch_retained_weight_at_length)
    catch_discarded_numbers <- RTMB::rowSums(catch_discarded_numbers_at_length)
    catch_discarded_weight <- RTMB::rowSums(catch_discarded_weight_at_length)

    catch_expected <- if (fleet_data[["observed_catch_units"]] == "number") {
      catch_numbers
    } else {
      catch_weight
    }
    catch_retained_expected <- if (fleet_data[["observed_catch_retained_units"]] == "number") {
      catch_retained_numbers
    } else {
      catch_retained_weight
    }
    catch_discarded_expected <- if (fleet_data[["observed_catch_discarded_units"]] == "number") {
      catch_discarded_numbers
    } else {
      catch_discarded_weight
    }

    n_index_years <- fleet_data[["n_index_years"]]
    index_years <- seq_len(n_index_years)
    selectivity_index <-
      biology[["selectivity_extended"]][[fleet]][index_years, , drop = FALSE]
    catchability <- biology[["catchability_extended"]][[fleet]][index_years]
    weight_index <- biology[["weight_at_length"]][index_years, , drop = FALSE]

    index_numbers_immature <- catchability * selectivity_index *
      population[["numbers_at_length"]][["immature"]][index_years, , drop = FALSE]
    index_numbers_mature <- catchability * selectivity_index *
      population[["numbers_at_length"]][["mature"]][index_years, , drop = FALSE]
    index_maturity <- fleet_data[["index_maturity"]]
    index_numbers_at_length <- if (identical(index_maturity, "mature")) {
      index_numbers_mature
    } else if (identical(index_maturity, "immature")) {
      index_numbers_immature
    } else {
      index_numbers_immature + index_numbers_mature
    }
    index_weight_at_length <- index_numbers_at_length * weight_index
    index_numbers <- RTMB::rowSums(index_numbers_at_length)
    index_weight <- RTMB::rowSums(index_weight_at_length)
    index_expected <- if (fleet_data[["observed_index_units"]] == "number") {
      index_numbers
    } else {
      index_weight
    }

    observed_compositions <- fleet_data[["length_comp"]]
    total_numbers_for_composition <- if (fleet_data[["has_catch"]]) {
      catch_numbers_at_length
    } else {
      index_numbers_immature + index_numbers_mature
    }
    # evaluate_age_comp() normalizes each year's numbers across bins and scales
    # by the observed sample size, which applies directly to length bins.
    composition_total <- evaluate_age_comp(
      total_numbers_for_composition,
      observed_compositions[["lengthcomp_proportion"]]
    )
    composition_retained <- evaluate_age_comp(
      catch_retained_numbers_at_length,
      observed_compositions[["lengthcomp_retained_proportion"]]
    )
    composition_discarded <- evaluate_age_comp(
      catch_discarded_numbers_at_length,
      observed_compositions[["lengthcomp_discarded_proportion"]]
    )
    composition_immature <- evaluate_age_comp(
      index_numbers_immature,
      observed_compositions[["lengthcomp_immature_proportion"]]
    )
    composition_mature <- evaluate_age_comp(
      index_numbers_mature,
      observed_compositions[["lengthcomp_mature_proportion"]]
    )

    predictions[[fleet]] <- list(
      catch_numbers_at_length = catch_numbers_at_length,
      catch_weight_at_length = catch_weight_at_length,
      catch_retained_numbers_at_length = catch_retained_numbers_at_length,
      catch_retained_weight_at_length = catch_retained_weight_at_length,
      catch_discarded_numbers_at_length = catch_discarded_numbers_at_length,
      catch_discarded_weight_at_length = catch_discarded_weight_at_length,
      index_numbers_at_length = index_numbers_at_length,
      index_weight_at_length = index_weight_at_length,
      catch_numbers = catch_numbers,
      catch_weight = catch_weight,
      catch_retained_numbers = catch_retained_numbers,
      catch_retained_weight = catch_retained_weight,
      catch_discarded_numbers = catch_discarded_numbers,
      catch_discarded_weight = catch_discarded_weight,
      index_numbers = index_numbers,
      index_weight = index_weight,
      catch_expected = catch_expected,
      catch_retained_expected = catch_retained_expected,
      catch_discarded_expected = catch_discarded_expected,
      index_expected = index_expected,
      log_catch_expected = log(catch_expected),
      log_catch_retained_expected = log(catch_retained_expected),
      log_catch_discarded_expected = log(catch_discarded_expected),
      log_index_expected = log(index_expected),
      lengthcomp_expected = composition_total[["expected"]],
      lengthcomp_proportion = composition_total[["proportion"]],
      lengthcomp_retained_expected = composition_retained[["expected"]],
      lengthcomp_retained_proportion = composition_retained[["proportion"]],
      lengthcomp_discarded_expected = composition_discarded[["expected"]],
      lengthcomp_discarded_proportion = composition_discarded[["proportion"]],
      lengthcomp_immature_expected = composition_immature[["expected"]],
      lengthcomp_immature_proportion = composition_immature[["proportion"]],
      lengthcomp_mature_expected = composition_mature[["expected"]],
      lengthcomp_mature_proportion = composition_mature[["proportion"]]
    )
  }
  predictions
}
