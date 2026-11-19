# Growth modules
#
# A growth module returns a list with weight at age: a vector by age when
# growth is the same every year, or a matrix with 1 row per year (n_years + 1
# rows, including the start of the year after the last model year) when it is
# data that change by year. Weight is in the units of the weight-at-age data
# (mt per fish in data_big). Modules that model length also return the
# distribution of length at age, which the age-to-length key uses.
#
# Growth modules need inputs that are not parameters (weight data, ages, a
# length grid), so each type also has an inputs function. Both are listed in
# growth_modules at the bottom of this file.

#' Empirical weight at age (EWAA)
#'
#' Weight at age is data, so this module only returns it.
#'
#' @param weight_at_age Matrix of weight at age, years by ages.
#' @noRd
ewaa_growth <- function(weight_at_age) {
  list(weight_at_age = weight_at_age)
}

#' von Bertalanffy growth, Schnute parameterization, with length variability
#'
#' Mean length at age follows von Bertalanffy growth through 2 reference
#' lengths, L1 at age a1 and L2 at age a2 (Schnute 1981):
#'   L(a) = L1 + (L2 - L1) (1 - exp(-k (a - a1))) / (1 - exp(-k (a2 - a1)))
#' and rises linearly from 0 to L1 below a1. Length at age is normal. Its SD
#' comes from 1 of 2 options:
#' - interpolation: the SD changes linearly with mean length from sd1 at a1
#'   to sd2 at a2. It is sd1 below a1 and sd2 above a2.
#' - delta method: SD = L(a) * sd(log L(a)), where the variance of log L(a)
#'   comes from a covariance matrix of log L1, log L2 and log k
#'   (delta_method_length_sd()).
#' Weight at length is W = alpha L^beta. Weight at age is W averaged over
#' the length distribution on the population length bins, or, with no length
#' bins, W at the mean length.
#'
#' These parameters are estimated on a log, logit or log-difference scale,
#' which keeps each in its valid range and L2 above L1, while the parameter
#' tibble shows natural-scale values. initialize_fims() transforms them
#' (growth_modules), so the arguments here are named log_*, logit_* or
#' log_difference_*.
#'
#' @param age Ages in years.
#' @param log_mean_length_old,log_difference_mean_length_old L2, either on the
#'   log scale (when L2 is constant), or (when L2 is estimated) as the log of
#'   the difference L2 - L1, so L2 stays above L1: L2 = L1 + exp(x). 1 of the
#'   2 is given.
#' @param log_mean_length_young,logit_mean_length_young L1, either on the log
#'   scale, or (when L1 is estimated and L2 is constant) on a logit scale
#'   bounded by 0 and L2, so L1 stays below L2: L1 = L2 / (1 + exp(-x)). 1 of
#'   the 2 is given.
#' @param log_growth_coefficient Log of k.
#' @param log_length_weight_a,log_length_weight_b Log of alpha and beta.
#' @param log_length_at_age_sd_young,log_length_at_age_sd_old Interpolation
#'   option: log of sd1 and sd2.
#' @param log_sd_mean_length_young,log_sd_mean_length_old,log_sd_growth_coefficient
#'   Delta-method option: log of the SDs of log L1, log L2 and log k.
#' @param mean_length_young_mean_length_old_logit_corr,mean_length_young_growth_coefficient_logit_corr,mean_length_old_growth_coefficient_logit_corr
#'   Delta-method option: their correlations, on a logit scale bounded by -1
#'   and 1.
#' @param reference_ages Data: c(a1, a2). Fixed because the formula switches
#'   form at a1, and the switch cannot depend on an estimated value.
#' @param length_edges Data: edges of the population length bins, or NULL
#'   when neither the fleets nor the population have length bins.
#' @return A list with weight at age, mean and SD of length at age, and, with
#'   length bins, the probability of each population length bin at each age.
#' @noRd
von_bertalanffy_schnute_growth <- function(age,
                                           log_growth_coefficient,
                                           log_length_weight_a,
                                           log_length_weight_b,
                                           reference_ages,
                                           length_edges,
                                           log_mean_length_old = NULL,
                                           log_difference_mean_length_old = NULL,
                                           log_mean_length_young = NULL,
                                           logit_mean_length_young = NULL,
                                           log_length_at_age_sd_young = NULL,
                                           log_length_at_age_sd_old = NULL,
                                           log_sd_mean_length_young = NULL,
                                           log_sd_mean_length_old = NULL,
                                           log_sd_growth_coefficient = NULL,
                                           mean_length_young_mean_length_old_logit_corr = NULL,
                                           mean_length_young_growth_coefficient_logit_corr = NULL,
                                           mean_length_old_growth_coefficient_logit_corr = NULL) {
  # Which arguments are given is set by the tibble (data), not by an
  # estimated value, so these `if`s are safe on the tape.
  if (is.null(log_difference_mean_length_old)) {
    mean_length_old <- exp(log_mean_length_old)
    mean_length_young <- if (is.null(logit_mean_length_young)) {
      exp(log_mean_length_young)
    } else {
      inv_logit(0, mean_length_old, logit_mean_length_young)
    }
  } else {
    mean_length_young <- exp(log_mean_length_young)
    mean_length_old <- mean_length_young + exp(log_difference_mean_length_old)
  }
  growth_coefficient <- exp(log_growth_coefficient)
  length_weight_a <- exp(log_length_weight_a)
  length_weight_b <- exp(log_length_weight_b)

  mean_length <- von_bertalanffy_length_at_age(
    age, mean_length_young, mean_length_old, growth_coefficient,
    reference_ages[1], reference_ages[2]
  )

  sd_length <- if (is.null(log_sd_mean_length_young)) {
    # Between the reference ages, (L(a) - L1) / (L2 - L1) is the growth
    # fraction, so this is linear in mean length. Holding the age inside the
    # reference ages holds the SD at sd1 below a1 and at sd2 above a2, rather
    # than extrapolating it. Ages and reference ages are data.
    sd_young <- exp(log_length_at_age_sd_young)
    sd_old <- exp(log_length_at_age_sd_old)
    interpolation_age <- pmin(pmax(age, reference_ages[1]), reference_ages[2])
    ad_max(
      sd_young + (sd_old - sd_young) *
        growth_fraction(interpolation_age, growth_coefficient, reference_ages),
      1e-8
    )
  } else {
    correlation <- function(logit_corr) inv_logit(-1, 1, logit_corr)
    delta_method_length_sd(
      age, mean_length, mean_length_young, mean_length_old,
      growth_coefficient, reference_ages,
      sd_log_parameters = exp(c(
        log_sd_mean_length_young, log_sd_mean_length_old,
        log_sd_growth_coefficient
      )),
      correlation = correlation(c(
        mean_length_young_mean_length_old_logit_corr,
        mean_length_young_growth_coefficient_logit_corr,
        mean_length_old_growth_coefficient_logit_corr
      ))
    )
  }

  # With no length bins there is no length distribution, so weight at age is
  # the weight at mean length. Whether there are bins is data, so this `if`
  # is safe on the tape.
  if (is.null(length_edges)) {
    return(list(
      weight_at_age = weight_at_length(
        mean_length, length_weight_a, length_weight_b
      ),
      mean_length_at_age = mean_length,
      sd_length_at_age = sd_length
    ))
  }

  # Probability of each population length bin at each age, and mean weight
  # at age averaged over that distribution.
  length_probability <- length_distribution(
    mean_length, sd_length, length_edges
  )
  length_bin_midpoints <- (length_edges[-1] +
    length_edges[-length(length_edges)]) / 2
  weight_at_bin <- weight_at_length(
    length_bin_midpoints, length_weight_a, length_weight_b
  )
  mean_weight <- as.vector(length_probability %*% weight_at_bin)

  list(
    weight_at_age = mean_weight,
    # For the weight at the midpoint of each fleet length bin, reported as
    # growth_derived_mean_WAA.
    length_weight_a = length_weight_a,
    length_weight_b = length_weight_b,
    mean_length_at_age = mean_length,
    sd_length_at_age = sd_length,
    length_probability = length_probability
  )
}

#' Mean length at age, von Bertalanffy with Schnute reference lengths
#'
#' Ages below the young reference age grow linearly from 0 to L1. `age` and
#' the reference ages are data, so the switch does not depend on an estimated
#' value.
#'
#' @inheritParams delta_method_length_sd
#' @param reference_age_young,reference_age_old The reference ages a1 and a2,
#'   where mean length is L1 and L2.
#' @return Mean length at each age, in the units of L1 and L2.
#' @noRd
von_bertalanffy_length_at_age <- function(age, mean_length_young,
                                          mean_length_old, growth_coefficient,
                                          reference_age_young,
                                          reference_age_old) {
  von_bertalanffy <- mean_length_young + (mean_length_old - mean_length_young) *
    growth_fraction(
      age, growth_coefficient, c(reference_age_young, reference_age_old)
    )
  is_younger <- reference_age_young > 0 & age < reference_age_young
  linear <- mean_length_young * pmax(age, 0) / reference_age_young
  if (any(is_younger)) {
    # Combine without branching on AD values: is_younger is data.
    von_bertalanffy * (!is_younger) + linear * is_younger
  } else {
    von_bertalanffy
  }
}

#' Fraction of the growth from L1 to L2 reached at each age
#'
#' (1 - exp(-k (a - a1))) / (1 - exp(-k (a2 - a1))), which is 0 at a1 and 1
#' at a2. It is written as expm1(-k (a - a1)) / expm1(-k (a2 - a1)). When
#' k (a2 - a1) is small, 1 - exp() subtracts 2 numbers near 1 and loses
#' digits, but expm1() does not. So the fraction approaches the linear
#' fraction (a - a1) / (a2 - a1) as k approaches 0, without a guard or a
#' branch on k. k must be above 0, which exp(log k) ensures.
#'
#' @param age Ages in years.
#' @param growth_coefficient k.
#' @param reference_ages c(a1, a2).
#' @return The fraction at each age.
#' @noRd
growth_fraction <- function(age, growth_coefficient, reference_ages) {
  expm1(-growth_coefficient * (age - reference_ages[1])) /
    expm1(-growth_coefficient * (reference_ages[2] - reference_ages[1]))
}

#' SD of length at age by the delta method
#'
#' Var(log L(a)) = g' S g, where g is the gradient of log L(a) with respect to
#' (log L1, log L2, log k) and S their covariance matrix, built from their SDs
#' and correlations (L1 with L2, L1 with k, L2 with k). These are the log
#' lengths themselves, also when L2 is estimated as log(L2 - L1). Then
#' SD = L(a) sqrt(Var(log L(a))). Below the young reference age, log L(a)
#' depends on L1 only. The smooth guards (ad_max()) keep the derivatives
#' defined where a length or the variance is near 0.
#'
#' @param age Ages in years.
#' @param mean_length Mean length at each age.
#' @param mean_length_young,mean_length_old,growth_coefficient L1, L2 and k.
#' @param reference_ages c(a1, a2).
#' @param sd_log_parameters SDs of log L1, log L2 and log k.
#' @param correlation Correlations of (L1, L2), (L1, k) and (L2, k).
#' @noRd
delta_method_length_sd <- function(age, mean_length, mean_length_young,
                                   mean_length_old, growth_coefficient,
                                   reference_ages, sd_log_parameters,
                                   correlation) {
  reference_age_young <- reference_ages[1]
  is_younger <- reference_age_young > 0 & age < reference_age_young

  # Ages at or above the young reference age. The ratio is growth_fraction(),
  # written out so its derivative with respect to k uses the same expm1()
  # terms. That derivative loses relative precision as k approaches 0, but it
  # is used only times k (the gradient with respect to log k), which keeps
  # the error near machine precision.
  age_from_young <- age - reference_age_young
  reference_span <- reference_ages[2] - reference_age_young
  numerator <- expm1(-growth_coefficient * age_from_young)
  denominator <- expm1(-growth_coefficient * reference_span)
  ratio <- numerator / denominator
  length_difference <- mean_length_old - mean_length_young
  length_safe <- ad_max(
    mean_length_young + length_difference * ratio, 1e-8
  )
  d_ratio_d_k <- (reference_span * exp(-growth_coefficient * reference_span) *
    numerator - age_from_young * exp(-growth_coefficient * age_from_young) *
    denominator) / (denominator * denominator)
  gradient_young <- (1 - ratio) / length_safe
  gradient_old <- ratio / length_safe
  gradient_k <- length_difference * d_ratio_d_k / length_safe

  # Younger ages: L(a) = L1 a / a1.
  younger_ratio <- pmax(age, 0) / reference_age_young
  younger_safe <- ad_max(mean_length_young * younger_ratio, 1e-8)
  if (any(is_younger)) {
    # Combine without branching on AD values: is_younger is data.
    gradient_young <- gradient_young * (!is_younger) +
      (younger_ratio / younger_safe) * is_younger
    gradient_old <- gradient_old * (!is_younger)
    gradient_k <- gradient_k * (!is_younger)
  }

  # Gradient with respect to the log-scale parameters.
  gradient_young <- gradient_young * mean_length_young
  gradient_old <- gradient_old * mean_length_old
  gradient_k <- gradient_k * growth_coefficient
  sd_young <- sd_log_parameters[1]
  sd_old <- sd_log_parameters[2]
  sd_k <- sd_log_parameters[3]
  variance_log_length <- gradient_young^2 * sd_young^2 +
    2 * gradient_young * gradient_old * correlation[1] * sd_young * sd_old +
    2 * gradient_young * gradient_k * correlation[2] * sd_young * sd_k +
    gradient_old^2 * sd_old^2 +
    2 * gradient_old * gradient_k * correlation[3] * sd_old * sd_k +
    gradient_k^2 * sd_k^2
  ad_max(mean_length * sqrt(ad_max(variance_log_length, 0)), 1e-8)
}

#' Weight at length, W = alpha L^beta
#'
#' @param fish_length Lengths, in the units of the length data.
#' @param length_weight_a,length_weight_b alpha and beta, on the natural
#'   scale.
#' @return Weight at each length.
#' @noRd
weight_at_length <- function(fish_length, length_weight_a, length_weight_b) {
  length_weight_a * ad_max(fish_length, 1e-8)^length_weight_b
}

# Inputs ----------------------------------------------------------------------
#
# Each takes the FIMSFrame, the parameter tibble and the population length
# bin edges (NULL unless growth models length) and returns the module's
# arguments that are not parameters.

#' Weight at age from the data, years (n_years + 1) by ages
#' @noRd
ewaa_growth_inputs <- function(data, parameters, population_edges) {
  # The model years and the year after the last model year.
  n_rows <- get_n_years(data) + 1
  weight_at_age <- model_weight_at_age(data)
  check_weight_at_age_dimensions(weight_at_age, n_rows, get_n_ages(data))
  list(weight_at_age = matrix(
    weight_at_age, n_rows, get_n_ages(data),
    byrow = TRUE
  ))
}

#' Ages, the 2 reference ages and the population length grid
#' @noRd
von_bertalanffy_schnute_growth_inputs <- function(data, parameters,
                                                  population_edges) {
  reference_rows <- parameters |>
    dplyr::filter(
      .data[["module_name"]] == "Growth",
      .data[["label"]] %in% c(
        "reference_age_for_length_young", "reference_age_for_length_old"
      )
    )
  is_estimated <- reference_rows[["estimation_type"]] %in%
    c("fixed_effects", "random_effects")
  if (any(is_estimated)) {
    cli::cli_abort(c(
      "x" = "Reference ages must be constant, but
      {.val {reference_rows[['label']][is_estimated]}} {?is/are} estimated.",
      "i" = "Length at age switches form at the young reference age, and the
      switch cannot depend on an estimated value.",
      "i" = "Check them with {.code dplyr::filter(parameters, module_name ==
      \"Growth\", grepl(\"reference_age\", label))}."
    ))
  }
  reference_age <- function(label) {
    reference_rows[["value"]][reference_rows[["label"]] == label]
  }
  list(
    age = get_ages(data),
    reference_ages = c(
      reference_age("reference_age_for_length_young"),
      reference_age("reference_age_for_length_old")
    ),
    length_edges = population_edges
  )
}

#' The scale each von Bertalanffy parameter is estimated on
#'
#' The tibble shows natural values, but 7 of them are estimated on the log
#' scale, which keeps them positive. 2 settings keep L2 above L1 instead:
#' - When L2 is estimated, it is estimated as log(L2 - L1) (log_difference),
#'   so L2 = L1 + exp(x).
#' - When L1 is estimated and L2 is constant, L1 is on a logit scale bounded
#'   by 0 and L2.
#' The delta-method SDs and correlations are given on their working scales
#' already (log_sd_*, *_logit_corr).
#'
#' @param rows The Growth rows of the parameter tibble.
#' @return "log", "logit", "log_difference" or NA (no transform) for each
#'   row.
#' @noRd
von_bertalanffy_schnute_working_scale <- function(rows) {
  is_estimated <- function(label) {
    any(rows[["estimation_type"]][rows[["label"]] == label] %in%
      c("fixed_effects", "random_effects"))
  }
  log_scale_labels <- c(
    "mean_length_young", "mean_length_old", "growth_coefficient",
    "length_weight_a", "length_weight_b",
    "length_at_age_sd_young", "length_at_age_sd_old"
  )
  working_scale <- dplyr::if_else(rows[["label"]] %in% log_scale_labels, "log", NA_character_)
  if (is_estimated("mean_length_young") && !is_estimated("mean_length_old")) {
    working_scale[rows[["label"]] == "mean_length_young"] <- "logit"
  }
  if (is_estimated("mean_length_old")) {
    working_scale[rows[["label"]] == "mean_length_old"] <- "log_difference"
  }
  working_scale
}

# Module types ----------------------------------------------------------------
#
# 1 entry per growth type:
# - evaluate: the module function.
# - inputs: its non-parameter inputs, from the data.
# - models_length: TRUE if it gives length at age, so fleets' age-to-length
#   keys come from growth.
# - working_scale: the scale each tibble row is estimated on ("log", "logit",
#   "log_difference" or NA). initialize_fims() transforms them and names them
#   log_*, logit_* or log_difference_*. Rows labeled reference_age_* are data
#   (see `inputs`).
growth_modules <- list(
  EWAA = list(
    evaluate = ewaa_growth,
    inputs = ewaa_growth_inputs,
    models_length = FALSE,
    working_scale = function(rows) rep(NA_character_, nrow(rows))
  ),
  VonBertalanffySchnute = list(
    evaluate = von_bertalanffy_schnute_growth,
    inputs = von_bertalanffy_schnute_growth_inputs,
    models_length = TRUE,
    working_scale = von_bertalanffy_schnute_working_scale
  )
)
