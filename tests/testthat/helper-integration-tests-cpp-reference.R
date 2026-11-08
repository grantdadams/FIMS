# Helpers for test-integration-cpp-reference.R and test-integration-retape.R,
# which compare the RTMB model with values from the C++ build of FIMS
# (tests/testthat/fixtures/integration_cpp_reference.RDS, made by
# data-raw/integration_cpp_reference.R).

#' Build the RTMB objective for a parameter tibble
#'
#' Uses fit_fims(optimize = FALSE), so the objective is the one fit_fims()
#' optimizes.
#'
#' @param parameters A parameter tibble.
#' @param data A FIMSFrame.
#' @param laplace If `FALSE`, every parameter is treated as fixed, so the
#'   gradient covers the random effects too. If `TRUE`, random effects are
#'   integrated out with the Laplace approximation.
#' @param get_sd Whether the objective reports derived quantities for
#'   RTMB::sdreport().
#' @return A list with the objective (`obj`) and the inputs from
#'   initialize_fims() (`input`).
rtmb_objective <- function(parameters, data, laplace = FALSE, get_sd = FALSE) {
  input <- initialize_fims(parameters = parameters, data = data)
  if (!laplace) {
    input[["random"]] <- character(0)
  }
  fit <- fit_fims(input, optimize = FALSE, get_sd = get_sd)
  list(obj = get_obj(fit), input = input)
}

#' Key each value of an RTMB objective by the columns of the parameter tibble
#'
#' Gives the same keys as cpp_parameter_keys() in
#' data-raw/integration_cpp_reference.R: `module_name`, `fleet`, the tibble
#' `label` (growth parameters keep their natural-scale label, as in C++), and
#' the position among the estimated values of that parameter in tibble row
#' order.
#'
#' @param par_names Names of the estimated values, e.g., `names(obj[["par"]])`.
#' @param parameter_table The `parameter_table` from initialize_fims().
#' @return A tibble with 1 row per value.
rtmb_parameter_keys <- function(par_names, parameter_table) {
  # RTMB repeats a parameter's name for each estimated value, so the position
  # within the name is the map level stored in rtmb_level (as in
  # parameter_labels()).
  level <- stats::ave(seq_along(par_names), par_names, FUN = seq_along)
  row <- match(
    paste(par_names, level),
    paste(parameter_table[["rtmb_name"]], parameter_table[["rtmb_level"]])
  )
  tibble::tibble(
    module_name = parameter_table[["module_name"]][row],
    fleet = parameter_table[["fleet"]][row],
    label = parameter_table[["original_label"]][row],
    parameter_id = parameter_table[["parameter_id"]][row]
  ) |>
    dplyr::mutate(
      position = rank(parameter_id),
      .by = c("module_name", "fleet", "label")
    )
}

#' Index that puts RTMB values in the order of the C++ values
#'
#' @param rtmb_keys Keys from rtmb_parameter_keys().
#' @param cpp_values A `values` tibble from the C++ reference fixture.
#' @return An integer vector, `NA` where a C++ value has no RTMB value.
order_like_cpp <- function(rtmb_keys, cpp_values) {
  key <- function(x) {
    paste(x[["module_name"]], x[["fleet"]], x[["label"]], x[["position"]])
  }
  match(key(cpp_values), key(rtmb_keys))
}

#' Largest relative difference between 2 numeric vectors
#'
#' Each difference is relative to the larger of `abs(expected)` and `floor`.
#' Where that is 0, the difference is absolute. Vectors of different lengths
#' differ by `Inf`. This is the largest difference, so it is stricter than
#' `all.equal()`, which checks the mean relative difference.
#'
#' @param object,expected Numeric vectors.
#' @param floor The smallest scale a difference is relative to.
#' @return A number.
max_relative_difference <- function(object, expected, floor = 0) {
  object <- as.numeric(object)
  expected <- as.numeric(expected)
  if (length(object) != length(expected)) {
    return(Inf)
  }
  if (length(expected) == 0) {
    return(0)
  }
  scale <- pmax(abs(expected), floor)
  scale[scale == 0] <- 1
  max(abs(object - expected) / scale)
}

#' Differences between the RTMB model and C++ FIMS at the C++ parameter values
#'
#' Builds the RTMB objective from the parameter tibble that C++ FIMS was given
#' and evaluates it at the C++ values of the estimated parameters.
#'
#' @param cpp A configuration from the C++ reference fixture, with
#'   `parameters`, `values`, `jnll`, `nll_components` and `reports`.
#' @param data The FIMSFrame that C++ FIMS was given.
#' @return A list with the number of C++ values that have no RTMB value
#'   (`n_unmatched`), and the largest relative difference in the starting
#'   values (`start`), `jnll`, `nll_components`, `gradient` and each report
#'   quantity (`reports`, named).
compare_with_cpp <- function(cpp, data) {
  rtmb <- rtmb_objective(cpp[["parameters"]], data)
  obj <- rtmb[["obj"]]
  rtmb_keys <- rtmb_parameter_keys(
    names(obj[["par"]]), rtmb[["input"]][["parameter_table"]]
  )
  cpp_order <- order_like_cpp(rtmb_keys, cpp[["values"]])
  n_unmatched <- sum(is.na(cpp_order)) +
    abs(length(obj[["par"]]) - length(cpp_order))
  if (n_unmatched > 0) {
    return(list(n_unmatched = n_unmatched))
  }
  x <- numeric(length(cpp_order))
  x[cpp_order] <- cpp[["values"]][["value"]]
  jnll <- obj[["fn"]](x)
  gradient <- as.vector(obj[["gr"]](x))[cpp_order]
  report <- obj[["report"]]()
  list(
    n_unmatched = n_unmatched,
    start = max_relative_difference(
      obj[["par"]][cpp_order], cpp[["values"]][["value"]]
    ),
    jnll = max_relative_difference(jnll, cpp[["jnll"]]),
    nll_components = max_relative_difference(
      report[["nll_components"]], cpp[["nll_components"]]
    ),
    gradient = max_relative_difference(
      gradient, cpp[["values"]][["gradient"]]
    ),
    reports = purrr::map_dbl(
      purrr::set_names(names(cpp[["reports"]])),
      \(name) {
        max_relative_difference(
          unlist(report[[name]], use.names = FALSE), cpp[["reports"]][[name]]
        )
      }
    )
  )
}
