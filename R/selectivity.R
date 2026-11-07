# Selectivity modules
#
# Each function returns selectivity at age, a value between 0 and 1, for 1
# fleet. `age` is the age in years (e.g., 1:12), not an index. Parameters are
# named as in the `label` column of the parameter tibble and are on the scale
# the label says (natural unless it starts with log_ or logit_). Selectivity
# is not rescaled to a maximum of 1, so a dome may peak below 1.
# The functions work on ordinary numbers, so they can be called and plotted at
# the console, and on RTMB AD values inside the model.

#' Logistic selectivity
#'
#' S(a) = 1 / (1 + exp(-slope * (a - inflection_point)))
#'
#' @param age Ages in years.
#' @param inflection_point Age at which selectivity is 0.5.
#' @param slope Steepness of the curve at the inflection point.
#' @noRd
logistic_selectivity <- function(age, inflection_point, slope) {
  logistic(inflection_point, slope, age)
}

#' Double logistic (dome-shaped) selectivity
#'
#' An ascending logistic times a descending one:
#' S(a) = 1 / (1 + exp(-slope_asc (a - inflection_point_asc))) *
#'        1 / (1 + exp(slope_desc (a - inflection_point_desc)))
#'
#' @param age Ages in years.
#' @param inflection_point_asc,slope_asc Ascending limb.
#' @param inflection_point_desc,slope_desc Descending limb.
#' @noRd
double_logistic_selectivity <- function(age, inflection_point_asc, slope_asc,
                                        inflection_point_desc, slope_desc) {
  ascending <- logistic(inflection_point_asc, slope_asc, age)
  descending <- logistic(inflection_point_desc, -slope_desc, age)
  ascending * descending
}

#' Age-specific selectivity
#'
#' 1 value per age, estimated on the logit scale: S(a) = 1 / (1 + exp(-x_a)).
#' 1 value can also be given for all ages.
#'
#' @param age Ages in years.
#' @param logit_sel_at_age Selectivity on the logit scale: 1 value per age,
#'   or 1 value for all ages.
#' @noRd
age_specific_selectivity <- function(age, logit_sel_at_age) {
  if (length(logit_sel_at_age) == 1) {
    logit_sel_at_age <- logit_sel_at_age[rep(1, length(age))]
  }
  if (length(logit_sel_at_age) != length(age)) {
    cli::cli_abort(c(
      "x" = "Age-specific selectivity has {.val {length(logit_sel_at_age)}}
      value{?s} of {.var logit_sel_at_age}.",
      "i" = "It needs 1 value per age ({.val {length(age)}}), or 1 for all
      ages.",
      "i" = "Count the values with {.code dplyr::count(parameters, fleet,
      label, timing)}."
    ))
  }
  inv_logit(0, 1, logit_sel_at_age)
}

# Module types, by the `module_type` in the parameter tibble. To add a type,
# write its function above and add 1 line here.
selectivity_functions <- list(
  Logistic = logistic_selectivity,
  DoubleLogistic = double_logistic_selectivity,
  AgeSpecific = age_specific_selectivity
)
