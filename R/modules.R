# Calling modules
#
# Each module family keeps a lookup table at the bottom of its own file
# (selectivity_functions in R/selectivity.R, and so on). A table maps the
# `module_type` in the parameter tibble to the R function for it. The
# function's arguments are named after the parameter labels, so the model can
# call it with that module's parameters. The helpers here call a module, and
# evaluate an at-age module in every year.

#' Call a module function with its parameters
#'
#' @param functions A module lookup table, e.g., `selectivity_functions`.
#' @param module_type The module type, e.g., "Logistic".
#' @param ... Inputs that are not parameters, e.g., `age`.
#' @param parameters Named list of the module's parameters.
#' @return What the module function returns: 1 value per element of its
#'   first input in `...`.
#' @noRd
call_module <- function(functions, module_type, ..., parameters = list()) {
  module_function <- functions[[module_type]]
  if (is.null(module_function)) {
    cli::cli_abort(c(
      "x" = "There is no function for module type {.val {module_type}}.",
      "i" = "The module types in this family are {.val {names(functions)}}.",
      "i" = "Check the module types with
      {.code dplyr::distinct(parameters, module_name, fleet, module_type)}."
    ))
  }
  # List the expected and given labels, where R would say only "argument is
  # missing" or "unused argument".
  parameter_names <- setdiff(names(formals(module_function)), ...names())
  if (!setequal(parameter_names, names(parameters))) {
    given <- if (length(parameters) == 0) {
      "no labels"
    } else {
      cli::format_inline("{.val {names(parameters)}}")
    }
    cli::cli_abort(c(
      "x" = "Module type {.val {module_type}} has the parameters
      {.val {parameter_names}}, but the parameter tibble gives {given}.",
      "i" = "Give rows for each of these labels and no others.",
      "i" = "Check the rows with {.code dplyr::filter(parameters,
      module_type == \"{module_type}\")}."
    ))
  }
  inputs <- list(...)
  output <- do.call(module_function, c(inputs, parameters))
  # A module returns 1 value per element of its first input: 1 per age for
  # an at-age module, and 1 for a module whose first input is a single
  # number, such as recruitment. R would recycle a wrong number of values
  # into the model without an error.
  if (length(inputs) > 0 && length(output) != length(inputs[[1]])) {
    n_output <- length(output)
    n_needed <- length(inputs[[1]])
    first_input <- names(inputs)[1]
    cli::cli_abort(c(
      "x" = "Module type {.val {module_type}} returned {.val {n_output}}
      value{?s}.",
      "i" = "It needs 1 value per element of {.arg {first_input}}
      ({.val {n_needed}}).",
      "i" = "Check what its function in the family's lookup table returns,
      such as {.code selectivity_functions} in {.file R/selectivity.R}."
    ))
  }
  output
}

#' Evaluate an at-age module in every year
#'
#' Parameters that vary by year are matrices with 1 row per year
#' (initialize_fims() stores them that way). Others have the same value in
#' every year. If no parameter varies by year, the module is evaluated once.
#'
#' @inheritParams call_module
#' @param ages Ages in years.
#' @param n_years Number of model years.
#' @return Matrix, years by ages.
#' @noRd
evaluate_by_year <- function(functions, module_type, ages, parameters,
                             n_years) {
  # RTMB: lets `x[i, ] <- value` store AD values in a plain matrix.
  "[<-" <- RTMB::ADoverload("[<-")
  is_by_year <- vapply(parameters, is.matrix, logical(1))
  if (!any(is_by_year)) {
    at_age <- call_module(functions, module_type,
      age = ages, parameters = parameters
    )
    return(RTMB::matrix(at_age, n_years, length(ages), byrow = TRUE))
  }
  by_year <- matrix(0, n_years, length(ages))
  for (year in seq_len(n_years)) {
    year_parameters <- lapply(parameters, function(value) {
      if (is.matrix(value)) value[year, ] else value
    })
    by_year[year, ] <- call_module(functions, module_type,
      age = ages, parameters = year_parameters
    )
  }
  by_year
}

#' A value that is either the same in every year or 1 per year, as a vector
#' with 1 value per year
#'
#' initialize_fims() checks the number of values, with
#' check_fleet_parameter_sizes().
#'
#' @param value The parameter values.
#' @param n_years Number of model years.
#' @noRd
per_year <- function(value, n_years) {
  rep_len(as.vector(value), n_years)
}

#' Can the model take 1 value of this parameter per year?
#'
#' Module parameters evaluated with evaluate_by_year() or per_year() can.
#' Growth, the stock-recruit parameters, and the depletion model's growth
#' rate, carrying capacity, shape and initial depletion cannot, because the
#' model reads them as 1 value. Initial numbers at age and proportion female
#' cannot, because the model reads them by age only.
#'
#' @param label The parameter labels.
#' @param module_name The modules they belong to.
#' @return TRUE or FALSE for each label.
#' @noRd
can_vary_by_year <- function(label, module_name) {
  module_name != "Growth" &
    !(module_name == "Depletion" & !label %in% c("log_depletion", "log_sd")) &
    !label %in% c(
      "log_rzero", "logit_steep", "log_init_naa", "proportion_female"
    )
}
