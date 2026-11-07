# Distributions: log densities for data, processes and priors
#
# These return log densities, and model_likelihood() negates their sum.
#
# Data log densities take (observed, predicted, ...): the observations, the
# model's prediction named in the data's uncertainty formula, then the
# family's own parameters (e.g., log_sd) by name. They return 1 value per
# observation, so each can be reported, and the total is sum() of the result.
# Missing observations are NA and contribute 0. Which observations are missing
# is data, so skipping them does not branch on an estimated value.
#
# Process and prior log densities take (x, mean, ...): the parameter values,
# their mean, then the density's parameters (log_sd, logit_rho) by name, and
# return 1 total.
#
# RTMB::dnorm() and the other RTMB:: densities are written out in full: they
# work on both AD values and ordinary numbers, while stats::dnorm() fails on
# AD values.
#
# To add a family or structure, write its functions here and add 1 entry to
# likelihood_families or process_structures at the bottom of this file.

#' Normal log density for data
#'
#' @param observed Observed values.
#' @param expected Expected values, same length as `observed` or length 1.
#' @param log_sd Log standard deviation, same length as `observed` or length 1.
#' @noRd
normal_lpdf <- function(observed, expected, log_sd) {
  # RTMB: lets `x[i] <- value` store AD values in a plain vector.
  "[<-" <- RTMB::ADoverload("[<-")
  n_observations <- length(observed)
  expected <- rep_len(expected, n_observations)
  standard_deviation <- rep_len(exp(log_sd), n_observations)
  is_observed <- !is.na(observed)
  log_density <- numeric(n_observations)
  log_density[is_observed] <- RTMB::dnorm(
    observed[is_observed], expected[is_observed],
    standard_deviation[is_observed],
    log = TRUE
  )
  log_density
}

#' Lognormal log density for data
#'
#' log(observed) ~ Normal(log_expected, sd), as the density of the observation
#' itself: dlnorm(x) = dnorm(log(x)) - log(x). So it includes the -log(x)
#' Jacobian term. The term does not depend on the parameters, so it changes
#' the objective but not the estimates.
#'
#' @param observed Observed values on the natural scale.
#' @param log_expected Expected values on the log scale.
#' @param log_sd Log of the standard deviation of log(observed).
#' @noRd
lognormal_lpdf <- function(observed, log_expected, log_sd) {
  # RTMB: lets `x[i] <- value` store AD values in a plain vector.
  "[<-" <- RTMB::ADoverload("[<-")
  n_observations <- length(observed)
  log_expected <- rep_len(log_expected, n_observations)
  standard_deviation <- rep_len(exp(log_sd), n_observations)
  is_observed <- !is.na(observed)
  log_density <- numeric(n_observations)
  log_density[is_observed] <- RTMB::dlnorm(
    observed[is_observed], log_expected[is_observed],
    standard_deviation[is_observed],
    log = TRUE
  )
  log_density
}

#' Multinomial log probability for data
#'
#' log p(x) = lgamma(sum(x) + 1) - sum(lgamma(x + 1)) + sum(x log(prob))
#'
#' Counts are not rounded (stats::dmultinom() rounds them) and `prob` is used
#' as given, so each row should already sum to 1. A row with any missing value
#' is skipped. Each row's value is shared equally among its bins, so the
#' per-bin values in get_estimates() sum to the row's value.
#'
#' @param observed Matrix of observed counts, 1 row per year (or length bin).
#' @param prob Matrix of expected proportions, same shape as `observed`.
#' @return Matrix the same shape as `observed`.
#' @noRd
multinomial_lpmf <- function(observed, prob) {
  # RTMB: lets `x[i, ] <- value` store AD values in a plain matrix.
  "[<-" <- RTMB::ADoverload("[<-")
  log_probability <- matrix(0, nrow(observed), ncol(observed))
  for (row_index in seq_len(nrow(observed))) {
    counts <- observed[row_index, ]
    if (anyNA(counts)) {
      next
    }
    row_value <- lgamma(sum(counts) + 1) - sum(lgamma(counts + 1)) +
      sum(counts * log(prob[row_index, ]))
    log_probability[row_index, ] <- row_value / ncol(observed)
  }
  log_probability
}

# Draws for simulation -----------------------------------------------------
#
# 1 per data family, with the same arguments, returning new observations the
# same shape as `observed` (NA where `observed` is NA). Used by
# simulate_fims(), with ordinary numbers.

lognormal_draw <- function(observed, log_expected, log_sd) {
  n_observations <- length(observed)
  draws <- exp(stats::rnorm(
    n_observations, rep_len(log_expected, n_observations),
    rep_len(exp(log_sd), n_observations)
  ))
  replace(draws, is.na(observed), NA)
}

normal_draw <- function(observed, expected, log_sd) {
  n_observations <- length(observed)
  draws <- stats::rnorm(
    n_observations, rep_len(expected, n_observations),
    rep_len(exp(log_sd), n_observations)
  )
  replace(draws, is.na(observed), NA)
}

#' Multinomial draws, 1 row at a time
#'
#' The sample size is the observed row total, rounded. Observed counts are
#' proportions times a sample size, so they need not be whole numbers.
#' @noRd
multinomial_draw <- function(observed, prob) {
  draws <- observed
  for (row_index in seq_len(nrow(observed))) {
    if (anyNA(observed[row_index, ])) {
      next
    }
    draws[row_index, ] <- stats::rmultinom(1, round(sum(observed[row_index, ])), prob[row_index, ])
  }
  draws
}

# Process and prior log densities --------------------------------------------
#
# x is a parameter that varies by year (a vector, or a years-by-ages matrix
# for iid), and mean is its expected value with the same shape (or 0). The SD
# is the year-to-year (innovation) SD in every case. Correlation rho is
# estimated as logit_rho, with rho = 2 / (1 + exp(-logit_rho)) - 1. So rho is
# between -1 and 1, and logit_rho = log(3) gives rho = 0.5.

#' Independent normal deviations around the mean (iid)
#'
#' x_t ~ Normal(m_t, sd), independently for every value (each year, or each
#' age and year).
#' @noRd
iid_process_lpdf <- function(x, mean, log_sd) {
  mean <- rep_len(as.vector(mean), length(x))
  standard_deviation <- rep_len(exp(log_sd), length(x))
  sum(RTMB::dnorm(x, mean, standard_deviation, log = TRUE))
}

#' First-order autoregressive process around the mean (AR1)
#'
#' x_t - m_t = rho (x_(t-1) - m_(t-1)) + e_t, e_t ~ Normal(0, sd). The first
#' year comes from the stationary distribution, x_1 ~ Normal(m_1,
#' sd / sqrt(1 - rho^2)). With rho = 0 this is the iid process.
#' @noRd
ar1_process_lpdf <- function(x, mean, log_sd, logit_rho) {
  if (is.matrix(x) && ncol(x) > 1) {
    cli::cli_abort(c(
      "x" = "An AR1 process is for parameters that vary by year only.",
      "i" = "This parameter varies by age and year. Use an iid process,
      {.code (1 | age:year)}."
    ))
  }
  deviation <- x - rep_len(as.vector(mean), length(x))
  standard_deviation <- exp(log_sd)
  rho <- 2 / (1 + exp(-logit_rho)) - 1
  n_years <- length(deviation)
  RTMB::dnorm(deviation[1], 0, standard_deviation / sqrt(1 - rho^2), log = TRUE) +
    sum(RTMB::dnorm(
      deviation[-1], rho * deviation[-n_years], standard_deviation,
      log = TRUE
    ))
}

#' Random walk
#'
#' x_t = x_(t-1) + e_t, e_t ~ Normal(0, sd). A random walk has no mean, so
#' `mean` is not used. The first year has no density term because it is a
#' fixed effect (add_process() makes it one).
#' @noRd
random_walk_process_lpdf <- function(x, mean, log_sd) {
  if (is.matrix(x) && ncol(x) > 1) {
    cli::cli_abort(c(
      "x" = "A random walk is for parameters that vary by year only.",
      "i" = "This parameter varies by age and year."
    ))
  }
  n_years <- length(x)
  sum(RTMB::dnorm(x[-1], x[-n_years], exp(log_sd), log = TRUE))
}

# Draws from the process densities, for simulation. Each returns new values
# the same shape as x.

iid_process_draw <- function(x, mean, log_sd) {
  x[] <- stats::rnorm(length(x), rep_len(as.vector(mean), length(x)), exp(log_sd))
  x
}

ar1_process_draw <- function(x, mean, log_sd, logit_rho) {
  n_years <- length(x)
  standard_deviation <- exp(log_sd)
  rho <- 2 / (1 + exp(-logit_rho)) - 1
  deviation <- numeric(n_years)
  deviation[1] <- stats::rnorm(1, 0, standard_deviation / sqrt(1 - rho^2))
  for (year in seq_len(n_years)[-1]) {
    deviation[year] <- rho * deviation[year - 1] +
      stats::rnorm(1, 0, standard_deviation)
  }
  x[] <- rep_len(as.vector(mean), n_years) + deviation
  x
}

#' The first year is a fixed effect and keeps its value.
#' @noRd
random_walk_process_draw <- function(x, mean, log_sd) {
  n_years <- length(x)
  x[] <- x[1] + c(0, cumsum(stats::rnorm(n_years - 1, 0, exp(log_sd))))
  x
}

# Families and structures -----------------------------------------------------
#
# Data families, by the family in the data's uncertainty formula, e.g.,
# "~ dlnorm(meanlog = log_catch_expected, sdlog = 0.01)". Each has:
# - lpdf: the log density.
# - draw: the matching draw for simulate_fims().
# - distribution: its name in the get_estimates() table.
# - parameters: its own parameters from the parsed uncertainty column
#   (parse_data_distribution()), by the names lpdf uses.
likelihood_families <- list(
  dlnorm = list(
    lpdf = lognormal_lpdf,
    draw = lognormal_draw,
    distribution = "log_normal",
    parameters = function(uncertainty) {
      list(log_sd = log(unlist(uncertainty[["sdlog"]])))
    }
  ),
  dnorm = list(
    lpdf = normal_lpdf,
    draw = normal_draw,
    distribution = "normal",
    parameters = function(uncertainty) {
      list(log_sd = log(unlist(uncertainty[["sd"]])))
    }
  ),
  dmultinom = list(
    lpdf = multinomial_lpmf,
    draw = multinomial_draw,
    distribution = "multinomial",
    parameters = function(uncertainty) list()
  )
)

# Process and prior structures, by `re_structure` in the parameter tibble.
process_structures <- list(
  iid = list(lpdf = iid_process_lpdf, draw = iid_process_draw),
  ar1 = list(lpdf = ar1_process_lpdf, draw = ar1_process_draw),
  rw = list(lpdf = random_walk_process_lpdf, draw = random_walk_process_draw)
)
