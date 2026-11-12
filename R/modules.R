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
#' @return What the module function returns.
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
  do.call(module_function, c(list(...), parameters))
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
  module_function <- functions[[module_type]]
  grid_name <- if (!is.null(module_function) &&
    "length" %in% names(formals(module_function))) {
    "length"
  } else {
    "age"
  }
  call_at_grid <- function(year_parameters) {
    do.call(
      call_module,
      c(
        list(functions, module_type),
        stats::setNames(list(ages), grid_name),
        list(parameters = year_parameters)
      )
    )
  }
  is_by_year <- vapply(parameters, is.matrix, logical(1))
  if (!any(is_by_year)) {
    at_age <- call_at_grid(parameters)
    return(RTMB::matrix(at_age, n_years, length(ages), byrow = TRUE))
  }
  by_year <- matrix(0, n_years, length(ages))
  for (year in seq_len(n_years)) {
    year_parameters <- lapply(parameters, function(value) {
      if (is.matrix(value)) value[year, ] else value
    })
    by_year[year, ] <- call_at_grid(year_parameters)
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
#' Growth, the stock-recruit parameters and initial numbers at age or length
#' cannot.
#'
#' @param label The parameter label.
#' @param module_name The module it belongs to.
#' @noRd
can_vary_by_year <- function(label, module_name) {
  module_name != "Growth" &&
    !label %in% c(
      "log_rzero", "logit_steep", "log_init_naa",
      "log_init_nal_immature", "log_init_nal_mature",
      "log_recruit_alpha", "log_recruit_beta", "max_recruit_length_bin",
      "discard_mortality"
    )
}
