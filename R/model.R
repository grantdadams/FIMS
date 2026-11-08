# The objective function
#
# fims_model() is what RTMB tapes. It puts the parameters into module groups,
# calls the model's predictions (catch_at_age() for a catch-at-age model,
# looked up in model_functions), adds the negative log-likelihood of priors,
# processes and data, and reports.

#' Joint negative log-likelihood of a FIMS model
#'
#' @param parameters The flat RTMB parameter list ("module.fleet.label").
#' @param model_data The `model_data` element of initialize_fims().
#' @return The joint negative log-likelihood.
#' @noRd
fims_model <- function(parameters, model_data) {
  model <- model_predictions(parameters, model_data)
  likelihood <- model_likelihood(model[["parameters"]], model_data, model)
  report_model(model_data, model, likelihood)
  likelihood[["jnll"]]
}

#' Everything the model predicts from a set of parameter values
#'
#' Used by fims_model() (with RTMB AD values), and by get_estimates() and
#' simulate_fims() (with ordinary numbers).
#'
#' @inheritParams fims_model
#' @return A list with the flat `parameters` (split parameters put back
#'   together) and the model's predictions (`biology`, `population` and
#'   `fleets` for catch at age).
#' @noRd
model_predictions <- function(parameters, model_data) {
  parameters <- assemble_split_parameters(
    parameters, model_data[["split_parameters"]]
  )
  # Prior, process and likelihood parameters are read by their densities,
  # by flat name, so they are not passed to the modules.
  module_parameters <- group_parameters(
    parameters,
    skip = model_data[["not_module_parameter_names"]]
  )
  predictions <- model_functions[[model_data[["model_type"]]]](
    module_parameters, model_data
  )
  c(list(parameters = parameters), predictions)
}

# Model types, by `model_type` in the model data.
model_functions <- list(
  CatchAtAge = catch_at_age
)

# Likelihood -----------------------------------------------------------------

#' The parts of a prior or process density: its target values, mean and
#' hyperparameters (log_sd, logit_rho), looked up by flat name
#'
#' The mean is 0, a parameter (expected_mean), or a population quantity
#' (log_expected_recruitment, the log of the stock-recruit curve, for log_r).
#'
#' @param density_entry 1 entry of `model_data$densities`.
#' @param parameters The flat parameter list.
#' @param population The population predictions, or NULL.
#' @noRd
density_inputs <- function(density_entry, parameters, population) {
  x <- parameters[[density_entry[["target"]]]]
  density_mean <- switch(density_entry[["mean"]][["kind"]],
    zero = 0,
    parameter = parameters[[density_entry[["mean"]][["name"]]]],
    derived = population[[density_entry[["mean"]][["name"]]]]
  )
  # A prior scores the values in `positions` (prior_densities()): values
  # that share 1 estimate once, and each constant value. A mean with 1 value
  # per target value is subset the same way, so each mean stays with its
  # value.
  positions <- density_entry[["positions"]]
  if (!is.null(positions)) {
    if (length(density_mean) == length(x)) {
      density_mean <- density_mean[positions]
    }
    x <- x[positions]
  }
  list(
    x = x,
    mean = density_mean,
    hyperparameters = lapply(density_entry[["hyperparameters"]], function(name) {
      parameters[[name]]
    })
  )
}

#' Negative log-likelihood: priors, then processes (random effects), then
#' data
#'
#' The order is the order of `nll_components`, which label_nll_components()
#' names.
#'
#' @param parameters The flat parameter list. Densities name their parameters
#'   by flat name ("Fleet.survey1.log_q").
#' @param model_data The `model_data` element of initialize_fims().
#' @param model Output of model_predictions().
#' @return A list with `jnll`, `nll_components` (1 per density, then 1 per
#'   data set) and `observation_lpdf` (the log density of each observation).
#' @noRd
model_likelihood <- function(parameters, model_data, model) {
  # RTMB: lets c(x, value) keep AD values that depend on parameters.
  "c" <- RTMB::ADoverload("c")
  # Priors and processes. initialize_densities() puts priors first.
  nll_components <- numeric(0)
  for (density_entry in model_data[["densities"]]) {
    inputs <- density_inputs(density_entry, parameters, model[["population"]])
    log_density <- do.call(
      process_structures[[density_entry[["structure"]]]][["lpdf"]],
      c(list(inputs[["x"]], inputs[["mean"]]), inputs[["hyperparameters"]])
    )
    nll_components <- c(nll_components, -log_density)
  }

  # Each data set is compared with the predicted quantity named in the data's
  # `uncertainty` column. For example "~ dlnorm(meanlog = log_catch_expected,
  # sdlog = 0.01)" compares catch with log_catch_expected (the `link`).
  observation_lpdf <- numeric(0)
  for (component in model_data[["likelihoods"]]) {
    # Log scale for dlnorm, proportions for dmultinom.
    predicted <- model[["fleets"]][[component[["fleet"]]]][[component[["link"]]]]
    log_density <- do.call(
      likelihood_families[[component[["family"]]]][["lpdf"]],
      c(
        list(component[["observed"]], predicted),
        family_parameters(component, parameters)
      )
    )
    nll_components <- c(nll_components, -sum(log_density))
    observation_lpdf <- c(
      observation_lpdf,
      if (is.matrix(log_density)) flatten_by_year(log_density) else log_density
    )
  }
  list(
    jnll = sum(nll_components),
    nll_components = nll_components,
    observation_lpdf = observation_lpdf
  )
}

#' A data set's family parameters
#'
#' They come from the data's uncertainty column (e.g., log_sd), unless the
#' parameter tibble has a row for them, which lets them be estimated.
#'
#' @param component 1 entry of `model_data$likelihoods`.
#' @param parameters The flat parameter list.
#' @noRd
family_parameters <- function(component, parameters) {
  values <- component[["parameters"]]
  for (name in names(component[["estimated_parameters"]])) {
    values[[name]] <- parameters[[component[["estimated_parameters"]][[name]]]]
  }
  values
}
