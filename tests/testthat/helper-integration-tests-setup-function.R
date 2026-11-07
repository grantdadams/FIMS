# Helper file for FIMS R tests----
# This file contains multiple functions that are used to set up and run
# FIMS models with wrapper functions. The functions are sourced by
# devtools::load_all().

# FIMS dmultinorm function ----
#' FIMS dmultinom()
#' This function matches the dmultinom() function in TMB and differs from R
#' by NOT rounding obs to the nearest integer. The function is evaluated in
#' log space and returns the log probability mass function.
#'
#' @param x A vector of length K of numeric values.
#' @param p A numeric non-negative vector of length K, specifying the probability
#' for the K classes; must sum 1.
#'
#' @return The log of the probability mass function for the multinomial.
FIMS_dmultinom <- function(x, p) {
  xp1 <- x + 1
  log_pmf <- lgamma(sum(x) + 1) - sum(lgamma(xp1)) + sum(x * log(p))
  return(log_pmf)
}


# FIMS helper function to run FIMS model with wrappers ----
#' Set Up and Run FIMS Model using wrapper functions
#'
#' This function sets up and runs the FIMS for a given iteration.
#' It configures the model with the OM inputs and outputs (see simulated data from
#' tests/testthat/fixtures/simulate-integration-test-data.R),
#' and runs the optimization process.
#' It then generates and returns the results including parameter estimates, model
#' reports, and standard deviation reports.
#'
#' @param iter_id An integer specifying the iteration ID to use for loading
#' the OM data.
#' @param om_input_list A list of OM inputs, where each element
#' corresponds to a different iteration.
#' @param om_output_list A list of OM outputs, where each element
#' corresponds to a different iteration.
#' @param em_input_list A list of EM inputs, where each element
#' corresponds to a different iteration.
#' @param estimation_mode A logical value indicating whether to perform
#' optimization (`TRUE`) or skip it (`FALSE`). If `TRUE`, the model parameters
#' will be optimized using `nlminb`. If `FALSE`, the input values will be used
#' for the report.
#' @param random_effects A logical value indicating whether to include random
#' effects in the model (`TRUE`) or skip it (`FALSE`). If `TRUE`, random effects
#' will be included on recruitment in the model.
#' @param map Not used. Values are held constant or shared through the
#' `estimation_type` and `map_group` columns of `modified_parameters`.
#'
#' @return The `FIMSFit` object returned by `fit_fims()`.
#' @examples
#' results <- setup_and_run_FIMS_with_wrappers(
#'   iter_id = 1,
#'   om_input_list = om_input_list,
#'   om_output_list = om_output_list,
#'   em_input_list = em_input_list,
#'   estimation_mode = TRUE
#' )
setup_and_run_FIMS_with_wrappers <- function(iter_id,
                                             om_input_list,
                                             om_output_list,
                                             em_input_list,
                                             estimation_mode = TRUE,
                                             random_effects = FALSE,
                                             modified_parameters,
                                             map = list()) {
  # Load operating model data for the current iteration
  om_input <- om_input_list[[iter_id]]
  om_output <- om_output_list[[iter_id]]
  em_input <- em_input_list[[iter_id]]

  data <- FIMS::FIMSFrame(data_big)
  if (tibble::is_tibble(modified_parameters)) {
    parameters <- modified_parameters
  } else {
    parameters <- modified_parameters[[iter_id]]
  }

  # The model will not always run when log_q is very small.
  # We will need to make sure log_q is the true value for deterministic runs but
  # then reset to log(1.0) for estimation runs.
  if (estimation_mode == TRUE) {
    parameters <- parameters |>
      dplyr::mutate(
        value = dplyr::if_else(
          fleet == "survey1" & label == "log_q",
          log(1.0),
          value
        )
      )
  }

  parameter_list <- initialize_fims(
    parameters = parameters,
    data = data
  )

  fit <- fit_fims(input = parameter_list, optimize = estimation_mode)

  return(fit)
}

# FIMS helper function to run a model at its initial values ----
#' Run a FIMS model at its initial values
#'
#' Sets up a model with the default parameters, builds it without optimizing,
#' and returns the objective function and the model report. The report keeps
#' full precision, unlike `get_estimates()`. It is evaluated at the initial
#' random effects, because evaluating the objective re-optimizes them.
#'
#' @param data A data frame of FIMS input data.
#'
#' @return A list with `objective`, the objective function value, and
#'   `report`, the model report.
run_at_initial_values <- function(data) {
  data_frame <- FIMSFrame(data)
  fit <- suppressMessages(setup_default_parameters(data_frame)) |>
    initialize_fims(data = data_frame) |>
    fit_fims(optimize = FALSE)
  obj <- get_obj(fit)
  list(
    objective = obj[["fn"]](obj[["par"]]),
    report = obj[["report"]](obj[["env"]][["par"]])
  )
}
