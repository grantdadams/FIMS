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
#'   to sd2 at a2.
#' - delta method: SD = L(a) * sd(log L(a)), where the variance of log L(a)
#'   comes from a covariance matrix of log L1, log L2 and log k
#'   (delta_method_length_sd()).
#' Weight at length is W = alpha L^beta.
#'
#' These parameters are estimated on a log or logit scale, which keeps each
#' in its valid range, while the parameter tibble shows natural-scale values.
#' initialize_fims() transforms them (growth_modules), so the arguments here
#' are named log_* or logit_*.
#'
#' @param age Ages in years.
#' @param log_mean_length_old Log of L2.
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
#' @param length_edges Data: edges of the population length bins.
#' @return A list with weight at age, mean and SD of length at age, and the
#'   probability of each population length bin at each age.
#' @noRd
von_bertalanffy_schnute_growth <- function(age,
                                           log_mean_length_old,
                                           log_growth_coefficient,
                                           log_length_weight_a,
                                           log_length_weight_b,
                                           reference_ages,
                                           length_edges,
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
  mean_length_old <- exp(log_mean_length_old)
  # Which arguments are given is set by the tibble (data), not by an
  # estimated value, so this `if` is safe on the tape.
  mean_length_young <- if (is.null(logit_mean_length_young)) {
    exp(log_mean_length_young)
  } else {
    mean_length_old / (1 + exp(-logit_mean_length_young))
  }
  growth_coefficient <- exp(log_growth_coefficient)
  length_weight_a <- exp(log_length_weight_a)
  length_weight_b <- exp(log_length_weight_b)

  length_at <- function(at_age) {
    von_bertalanffy_length_at_age(
      at_age, mean_length_young, mean_length_old, growth_coefficient,
      reference_ages[1], reference_ages[2]
    )
  }
  mean_length <- length_at(age)

  sd_length <- if (is.null(log_sd_mean_length_young)) {
    # SD changes linearly with mean length between the reference ages.
    length_young <- length_at(reference_ages[1])
    length_old <- length_at(reference_ages[2])
    sd_young <- exp(log_length_at_age_sd_young)
    sd_old <- exp(log_length_at_age_sd_old)
    slope <- (sd_old - sd_young) /
      ad_max(ad_fabs(length_old - length_young), 1e-8)
    ad_max(sd_young + slope * (mean_length - length_young), 1e-8)
  } else {
    correlation <- function(logit_corr) -1 + 2 / (1 + exp(-logit_corr))
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

  # Probability of each population length bin at each age, and mean weight
  # at age averaged over that distribution.
  length_probability <- length_distribution(
    mean_length, sd_length, length_edges
  )
  bin_centers <- (length_edges[-1] + length_edges[-length(length_edges)]) / 2
  weight_at_bin <- weight_at_length(
    bin_centers, length_weight_a, length_weight_b
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
  denominator <- 1 - exp(-growth_coefficient *
    (reference_age_old - reference_age_young))
  numerator <- 1 - exp(-growth_coefficient * (age - reference_age_young))
  von_bertalanffy <- mean_length_young + (mean_length_old - mean_length_young) *
    numerator / ad_max(ad_fabs(denominator), 1e-8)
  is_younger <- reference_age_young > 0 & age < reference_age_young
  linear <- mean_length_young * pmax(age, 0) / reference_age_young
  if (any(is_younger)) {
    # Combine without branching on AD values: is_younger is data.
    von_bertalanffy * (!is_younger) + linear * is_younger
  } else {
    von_bertalanffy
  }
}

#' SD of length at age by the delta method
#'
#' Var(log L(a)) = g' S g, where g is the gradient of log L(a) with respect to
#' (log L1, log L2, log k) and S their covariance matrix, built from their SDs
#' and correlations (L1 with L2, L1 with k, L2 with k). Then
#' SD = L(a) sqrt(Var(log L(a))). Below the young reference age, log L(a)
#' depends on L1 only. The smooth guards (ad_fabs(), ad_max()) keep the
#' derivatives defined where a denominator or a length is near 0.
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

  # Ages at or above the young reference age.
  age_from_young <- age - reference_age_young
  reference_span <- reference_ages[2] - reference_age_young
  exp_numerator <- exp(-growth_coefficient * age_from_young)
  exp_denominator <- exp(-growth_coefficient * reference_span)
  numerator <- 1 - exp_numerator
  denominator <- ad_max(ad_fabs(1 - exp_denominator), 1e-8)
  ratio <- numerator / denominator
  length_difference <- mean_length_old - mean_length_young
  length_safe <- ad_max(
    mean_length_young + length_difference * ratio, 1e-8
  )
  d_ratio_d_k <- (age_from_young * exp_numerator * denominator -
    numerator * reference_span * exp_denominator) / (denominator * denominator)
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

# Smooth versions of abs() and max(), used as numerical guards. Unlike abs()
# and max(), they have derivatives everywhere. The cost is a small shift even
# where no guard is needed: ad_fabs(x) = sqrt(x^2 + C), about
# |x| + C / (2 |x|), with C = 1e-5.
ad_fabs <- function(x, C = 1e-5) {
  sqrt(x * x + C)
}

ad_max <- function(a, b, C = 1e-5) {
  (a + b + ad_fabs(a - b, C)) * 0.5
}

# Inputs ----------------------------------------------------------------------
#
# Each takes the FIMSFrame, the parameter tibble and the population length
# bin edges (NULL unless growth models length) and returns the module's
# arguments that are not parameters.

#' Weight at age from the data, years (n_years + 1) by ages
#' @noRd
ewaa_growth_inputs <- function(data, parameters, population_edges) {
  n_years <- get_n_years(data) + 1
  weight_at_age <- model_weight_at_age(data)
  check_weight_at_age_dimensions(weight_at_age, n_years, get_n_ages(data))
  list(weight_at_age = matrix(
    weight_at_age, n_years, get_n_ages(data),
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
  if (any(reference_rows[["estimation_type"]] %in% c("fixed_effects", "random_effects"))) {
    cli::cli_abort(c(
      "x" = "Reference ages must be constant.",
      "i" = "Length at age switches form at the young reference age, and the
      switch cannot depend on an estimated value."
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
#' scale, which keeps them positive. When L1 is estimated and L2 is constant,
#' L1 is instead on a logit scale bounded by 0 and L2, which keeps L1 below
#' L2. The delta-method SDs and correlations are given on their working
#' scales already (log_sd_*, *_logit_corr).
#'
#' @param rows The Growth rows of the parameter tibble.
#' @return "log", "logit" or NA (no transform) for each row.
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
  working_scale
}

# Module types ----------------------------------------------------------------
#
# 1 entry per growth type:
# - evaluate: the module function.
# - inputs: its non-parameter inputs, from the data.
# - models_length: TRUE if it gives length at age, so fleets' age-to-length
#   keys come from growth.
# - working_scale: the scale each tibble row is estimated on ("log", "logit"
#   or NA). initialize_fims() transforms them and names them log_* or
#   logit_*. Rows labeled reference_age_* are data (see `inputs`).
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
