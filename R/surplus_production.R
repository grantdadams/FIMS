# Surplus production model: predictions
#
# surplus_production() predicts depletion and biomass, then the index of each
# fleet with index data. fims_model() (R/model.R) calls it, then adds the
# likelihood and reports. As in R/catch_at_age.R, branch only on data, never
# on an estimated value.
#
# Inputs, both from initialize_fims():
# - `module_parameters`: the Depletion parameters, e.g.,
#   module_parameters[["Depletion"]][["log_growth_rate"]].
# - `model_data`: n_years, the depletion module type and the total catch by
#   year (`populations`), and 1 index likelihood per fleet with index data
#   (`likelihoods`).
#
# Conventions:
# - The state is depletion, P = B / K, at the start of the year. Depletion and
#   biomass have 1 more value than the model years, the start of the year
#   after the last model year.
# - P_1 is exp(log_init_depletion), with no process error. From year 2 to the
#   last model year, log P_t = log_depletion_t, a random effect with
#   log_depletion_t ~ Normal(log_expected_depletion_t, sd), where
#   log_expected_depletion_t is the log of the depletion module's prediction
#   from year t - 1. The year after the last model year has no process error.
# - Catch is the total over fleets, known without error, and is taken at the
#   end of the year, after production.
# - The index is at the start of the year, I_t = q B_t. q is not a parameter:
#   log q is the mean of log(I_t / B_t) over the years with an index,
#   weighted by 1 / sd_t^2, which is its maximum likelihood estimate given
#   biomass (Walters and Ludwig 1994).

#' Predictions of the surplus production model
#'
#' The model has 1 population, the first element of
#' `model_data[["populations"]]`.
#'
#' @param module_parameters Parameters grouped by module (group_parameters()).
#' @param model_data The `model_data` element of initialize_fims().
#' @param parameters The flat parameter list, for the index log_sd, which can
#'   be estimated.
#' @return A list with `population` (depletion, biomass, harvest rate and MSY
#'   quantities) and `fleets` (the index predictions, by fleet).
#' @noRd
surplus_production <- function(module_parameters, model_data, parameters) {
  population <- depletion_dynamics(
    module_parameters[["Depletion"]], model_data
  )
  list(
    population = population,
    fleets = index_predictions(model_data, parameters, population[["biomass"]])
  )
}

#' Depletion, biomass, harvest rate and MSY quantities, 1 year at a time
#'
#' @param depletion_parameters The Depletion parameters.
#' @inheritParams surplus_production
#' @noRd
depletion_dynamics <- function(depletion_parameters, model_data) {
  # RTMB: lets `x[i] <- value` keep AD values that depend on parameters when
  # `x` starts as a plain vector of zeros.
  "[<-" <- RTMB::ADoverload("[<-")
  n_years <- model_data[["n_years"]]
  population_data <- model_data[["populations"]][[1]]
  catch <- population_data[["catch"]]
  # The module function takes the Depletion parameters other than
  # log_init_depletion and log_depletion. The process sd is read by the
  # process density.
  module_parameters <- depletion_parameters[setdiff(
    names(depletion_parameters),
    c("log_init_depletion", names(depletion_process_means))
  )]

  depletion <- numeric(n_years + 1)
  log_expected_depletion <- numeric(n_years - 1)
  depletion[1] <- exp(depletion_parameters[["log_init_depletion"]])
  for (year in seq_len(n_years)) {
    expected_depletion <- call_module(
      depletion_functions, population_data[["module_types"]][["depletion"]],
      depletion = depletion[year], catch = catch[year],
      parameters = module_parameters
    )
    # A catch above the biomass plus production would give a depletion below
    # 0, which has no log. The smooth floor at 0.001 keeps the log and its
    # derivatives defined. C = 1e-8 smooths it over about 1e-4, so it adds
    # about 2.5e-9 / P to an expected depletion P well above the floor. The
    # default C = 1e-5 would bias a collapsed stock low, by 2.7% at P = 0.01.
    expected_depletion <- ad_max(expected_depletion, 0.001, C = 1e-8)
    if (year < n_years) {
      log_expected_depletion[year] <- log(expected_depletion)
      depletion[year + 1] <- exp(depletion_parameters[["log_depletion"]][year])
    } else {
      depletion[year + 1] <- expected_depletion
    }
  }

  # MSY quantities of the Pella-Tomlinson curve. Depletion at MSY is
  # m^(-1 / (m - 1)), and the harvest rate at MSY (catch / biomass) is r / m.
  growth_rate <- exp(depletion_parameters[["log_growth_rate"]])
  carrying_capacity <- exp(depletion_parameters[["log_carrying_capacity"]])
  shape <- exp(depletion_parameters[["log_shape"]])
  biomass <- carrying_capacity * depletion
  harvest_rate_at_msy <- growth_rate / shape
  biomass_at_msy <- carrying_capacity * shape^(-1 / (shape - 1))
  list(
    biomass = biomass,
    depletion = depletion,
    log_expected_depletion = log_expected_depletion,
    # Catch over biomass at the start of the year.
    harvest_rate = catch / biomass[seq_len(n_years)],
    harvest_rate_at_msy = harvest_rate_at_msy,
    biomass_at_msy = biomass_at_msy,
    maximum_sustainable_yield = harvest_rate_at_msy * biomass_at_msy
  )
}

#' Index predictions of each fleet with index data (model years only)
#'
#' @inheritParams surplus_production
#' @param biomass Biomass at the start of each year.
#' @return A list by fleet, empty for a fleet without index data, with
#'   `catchability` (q), `index_expected` and `log_index_expected`.
#' @noRd
index_predictions <- function(model_data, parameters, biomass) {
  log_biomass <- log(biomass[seq_len(model_data[["n_years"]])])
  predictions <- lapply(model_data[["fleets"]], function(fleet_data) list())
  # In the surplus production model, the only likelihoods are the indices,
  # all lognormal (initialize_surplus_production_data()).
  for (component in model_data[["likelihoods"]]) {
    is_observed <- !is.na(component[["observed"]])
    log_sd <- rep_len(
      family_parameters(component, parameters)[["log_sd"]],
      length(component[["observed"]])
    )
    weight <- exp(-2 * log_sd[is_observed])
    log_q <- sum(weight * (log(component[["observed"]][is_observed]) -
      log_biomass[is_observed])) / sum(weight)
    log_index_expected <- log_q + log_biomass
    predictions[[component[["fleet"]]]] <- list(
      catchability = exp(log_q),
      index_expected = exp(log_index_expected),
      log_index_expected = log_index_expected
    )
  }
  predictions
}
