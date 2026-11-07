# Maturity modules
#
# Each function returns the proportion mature at age. `age` is the age in
# years, not an index.

#' Logistic maturity
#'
#' m(a) = 1 / (1 + exp(-slope * (a - inflection_point)))
#'
#' @param age Ages in years.
#' @param inflection_point Age at 50% maturity.
#' @param slope Steepness of the curve at the inflection point.
#' @noRd
logistic_maturity <- function(age, inflection_point, slope) {
  1 / (1 + exp(-slope * (age - inflection_point)))
}

# Module types, by the `module_type` in the parameter tibble.
maturity_functions <- list(
  Logistic = logistic_maturity
)
