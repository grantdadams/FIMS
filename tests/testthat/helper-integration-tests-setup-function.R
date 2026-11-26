# Helper file for FIMS R tests----
# This file contains functions that set up and run FIMS models, and that make
# the data rows and growth parameter rows tests add to them. The functions are
# sourced by devtools::load_all().

# FIMS dmultinorm function ----
#' FIMS dmultinom()
#' This function matches multinomial_lpmf(). Both differ from
#' stats::dmultinom() by not rounding the counts. The function is evaluated in
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
                                             modified_parameters) {
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

# Population length bins ----
#' Make `length_bin` rows that set the population length bins
#'
#' @param lengths A numeric vector of the lower edges of the population length
#'   bins.
#' @param timing A numeric timing for the rows. Population length bins need
#'   `NA`, so other values are only for testing errors.
#'
#' @return A tibble with the columns of `data_big`.
population_length_bin_rows <- function(lengths, timing = NA_real_) {
  tibble::tibble(
    type = "length_bin",
    fleet = NA_character_,
    age = NA_real_,
    length = lengths,
    timing = timing,
    observed = NA_real_,
    unit = NA_character_,
    uncertainty = NA_character_
  )
}

# von Bertalanffy-Schnute growth forms ----
#' Hold L2 constant in von Bertalanffy-Schnute growth rows
#'
#' With L2 constant and L1 estimated, L1 is estimated on a logit scale bounded
#' by L2.
#'
#' @param rows Parameter rows with von Bertalanffy-Schnute growth rows, such
#'   as from setup_default_Growth() or setup_default_parameters().
#'
#' @return `rows` with `mean_length_old` constant.
with_constant_mean_length_old <- function(rows) {
  dplyr::mutate(
    rows,
    estimation_type = dplyr::if_else(
      label == "mean_length_old", "constant", estimation_type
    )
  )
}

#' Use the delta-method sd of length at age in von Bertalanffy-Schnute growth
#' rows
#'
#' The delta-method sd replaces the 2 interpolation sds with 3 sds on the log
#' scale and 3 correlations on a logit scale bounded by -1 and 1. Each new row
#' copies the first interpolation sd row, so it keeps that row's estimation
#' type.
#'
#' @inheritParams with_constant_mean_length_old
#' @param correlations The correlations of L1 with L2, L1 with k, and L2 with
#'   k.
#'
#' @return `rows` without the 2 interpolation sd rows, and with the 6
#'   delta-method rows at the end.
with_delta_method_sd <- function(rows, correlations = c(0.5, 0.2, 0.1)) {
  is_interpolation_sd <- rows[["label"]] %in%
    c("length_at_age_sd_young", "length_at_age_sd_old")
  delta_method_rows <- rows |>
    dplyr::slice(rep(which(is_interpolation_sd)[1], 6)) |>
    dplyr::mutate(
      label = c(
        "log_sd_mean_length_young", "log_sd_mean_length_old",
        "log_sd_growth_coefficient",
        "mean_length_young_mean_length_old_logit_corr",
        "mean_length_young_growth_coefficient_logit_corr",
        "mean_length_old_growth_coefficient_logit_corr"
      ),
      value = c(log(c(0.05, 0.05, 0.05)), logit(-1, 1, correlations))
    )
  rows |>
    dplyr::filter(!is_interpolation_sd) |>
    dplyr::bind_rows(delta_method_rows)
}
