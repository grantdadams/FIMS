# Simulation from a fitted model, and a simulation self-test
#
# simulate_fims() draws new data from the model's predictions at the fitted
# values, in plain R. Each likelihood family has a matching draw function
# (likelihood_families in R/distributions.R), and the predictions come from
# model_predictions(), the same code the likelihood uses. Random effects are
# either kept at their fitted values (simulation conditional on them) or
# redrawn from their process densities first. The simulated data come back in
# the FIMSFrame input format, ready for FIMSFrame() and initialize_fims().
#
# Why not RTMB's obj$simulate(): it records a simulated observation only when
# every element is drawn and a variable with the observation's name is on the
# call stack, so subsets (missing years), helper functions and row-by-row
# compositions are silently dropped. Writing the likelihoods around those
# rules would cost readability.
#
# run_fims_self_test() simulates data, refits with the same parameter tibble,
# and reports convergence and relative errors.

#' Simulate data sets from a fitted model
#'
#' @param fit A FIMSFit from fit_fims().
#' @param number_of_simulations Number of data sets.
#' @param random_effects "keep" to hold random effects at their fitted values,
#'   or "redraw" to draw new ones from their densities.
#' @return A list with 1 element per data set. Each has `data` (a tibble for
#'   FIMSFrame()), `parameters` (the flat parameter list used) and `truth`
#'   (the model's predictions at those values).
#' @keywords diagnostics
#' @export
simulate_fims <- function(fit, number_of_simulations = 1,
                          random_effects = c("keep", "redraw")) {
  random_effects <- match.arg(random_effects)
  input <- get_input(fit)
  obj <- get_obj(fit)
  fitted_parameters <- obj[["env"]][["parList"]](par = obj[["env"]][["last.par.best"]])
  lapply(seq_len(number_of_simulations), function(i) {
    parameters <- fitted_parameters
    if (random_effects == "redraw") {
      parameters <- redraw_random_effects(parameters, input)
    }
    truth <- model_predictions(parameters, input[["model_data"]])
    list(
      data = write_simulated_data(input, draw_observations(truth, input[["model_data"]])),
      parameters = parameters,
      truth = truth
    )
  })
}

#' New random effects from their process densities
#'
#' Only processes whose values are random effects are redrawn. A penalized
#' process (fixed effects) keeps its fitted values, as do all other
#' parameters.
#' @noRd
redraw_random_effects <- function(parameters, input) {
  splits <- input[["model_data"]][["split_parameters"]]
  whole <- assemble_split_parameters(parameters, splits)
  for (density_entry in input[["model_data"]][["densities"]]) {
    target <- density_entry[["target"]]
    is_random <- target %in% input[["random"]] ||
      paste0(target, "|random") %in% input[["random"]]
    if (density_entry[["distribution_type"]] != "process" || !is_random) {
      next
    }
    if (density_entry[["mean"]][["kind"]] == "derived") {
      cli::cli_abort(c(
        "x" = "Cannot redraw {.val {target}}.",
        "i" = "Its mean depends on the population it drives. Use
        {.code random_effects = \"keep\"}."
      ))
    }
    inputs <- density_inputs(density_entry, whole, population = NULL)
    drawn <- do.call(
      process_structures[[density_entry[["structure"]]]][["draw"]],
      c(list(inputs[["x"]], inputs[["mean"]]), inputs[["hyperparameters"]])
    )
    if (is.null(splits[[target]])) {
      parameters[[target]] <- drawn
    } else {
      parameters[[paste0(target, "|random")]] <-
        as.vector(drawn)[splits[[target]][["random"]]]
    }
  }
  parameters
}

#' New observations for every data set, from the model's predictions
#'
#' @return A list by data set name (e.g., "fleet1_catch"), each the shape of
#'   that data set's observations.
#' @noRd
draw_observations <- function(model, model_data) {
  draws <- list()
  for (component in model_data[["likelihoods"]]) {
    predicted <- model[["fleets"]][[component[["fleet"]]]][[component[["link"]]]]
    draws[[component[["name"]]]] <- do.call(
      likelihood_families[[component[["family"]]]][["draw"]],
      c(
        list(component[["observed"]], predicted),
        family_parameters(component, model[["parameters"]])
      )
    )
  }
  draws
}

#' Put simulated observations into a copy of the model's data tibble
#'
#' Each data set's simulated values are matched to its rows by fleet, type,
#' year, and age, length bin or both. Compositions given as proportions are
#' returned as proportions of the simulated row total. Values that were
#' missing stay as they were.
#' @noRd
write_simulated_data <- function(input, simulated) {
  frame <- input[["frame"]]
  data <- get_data(frame)
  start_year <- get_start_year(frame)
  ages <- get_ages(frame)
  for (component in input[["model_data"]][["likelihoods"]]) {
    values <- simulated[[component[["name"]]]]
    fleet <- component[["fleet"]]
    data_type <- component[["data_type"]]
    is_row <- which(data[["fleet"]] %in% fleet & data[["type"]] == data_type)
    rows <- dplyr::slice(data, is_row)
    year <- rows[["timing"]] - start_year + 1
    fleet_data <- input[["model_data"]][["fleets"]][[fleet]]
    length_bins <- fleet_data[["length_bins"]]
    if (data_type %in% c("catch", "index")) {
      new_values <- as.vector(values)[year]
    } else {
      row_index <- if (data_type == "conditional_age_at_length") {
        conditional_rows <- fleet_data[["conditional_age_at_length"]]
        match(
          paste(year, match(rows[["length"]], length_bins)),
          paste(conditional_rows[["year"]], conditional_rows[["length_bin"]])
        )
      } else {
        year
      }
      column_index <- if (data_type == "length_comp") {
        match(rows[["length"]], length_bins)
      } else {
        match(rows[["age"]], ages)
      }
      counts <- values[cbind(row_index, column_index)]
      new_values <- dplyr::if_else(
        rows[["unit"]] == "proportion",
        counts / rowSums(values)[row_index], counts
      )
    }
    data[["observed"]][is_row] <- dplyr::coalesce(new_values, rows[["observed"]])
  }
  data
}

#' Simulation self-test: simulate, refit, and compare with the truth
#'
#' Each data set is drawn after a call to set.seed(), so the self-test
#' changes the state of the random number generator for the rest of the
#' session.
#'
#' @param fit A FIMSFit from fit_fims().
#' @param number_of_simulations Number of simulated data sets.
#' @param random_effects "keep" or "redraw", as in simulate_fims().
#' @param seed Seed for the data sets: data set i uses seed + i.
#' @return A tibble with 1 row per data set: convergence (nlminb code 0
#'   and maximum gradient at most 0.01), and relative errors in
#'   terminal spawning biomass, terminal depletion, mean recruitment and the
#'   largest relative error in spawning biomass over all years.
#' @keywords diagnostics
#' @export
run_fims_self_test <- function(fit, number_of_simulations = 10,
                               random_effects = c("keep", "redraw"), seed = 20261106) {
  random_effects <- match.arg(random_effects)
  input <- get_input(fit)
  rows <- lapply(seq_len(number_of_simulations), function(i) {
    set.seed(seed + i)
    simulation <- simulate_fims(fit, 1, random_effects)[[1]]
    truth <- simulation[["truth"]][["population"]]
    refit <- tryCatch(
      suppressWarnings(FIMSFrame(simulation[["data"]])) |>
        initialize_fims(parameters = input[["tibble"]]) |>
        fit_fims(get_sd = FALSE, number_of_loops = 1),
      error = function(e) e
    )
    if (inherits(refit, "error")) {
      return(tibble::tibble(
        simulation = i, converged = FALSE, note = conditionMessage(refit)
      ))
    }
    obj <- get_obj(refit)
    estimate <- model_predictions(
      obj[["env"]][["parList"]](par = obj[["env"]][["last.par.best"]]),
      get_input(refit)[["model_data"]]
    )[["population"]]
    terminal <- length(truth[["spawning_biomass"]])
    relative_error <- function(name, index = seq_along(truth[[name]])) {
      estimate[[name]][index] / truth[[name]][index] - 1
    }
    tibble::tibble(
      simulation = i,
      converged = get_opt(refit)[["convergence"]] == 0 &&
        get_max_gradient(refit) <= 0.01,
      max_gradient = get_max_gradient(refit),
      relative_error_terminal_spawning_biomass =
        relative_error("spawning_biomass", terminal),
      relative_error_terminal_depletion =
        relative_error("spawning_biomass_ratio", terminal),
      relative_error_mean_recruitment = mean(estimate[["expected_recruitment"]]) /
        mean(truth[["expected_recruitment"]]) - 1,
      max_abs_relative_error_spawning_biomass =
        max(abs(relative_error("spawning_biomass"))),
      note = ""
    )
  })
  dplyr::bind_rows(rows)
}
