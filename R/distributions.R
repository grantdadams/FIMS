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
# Process and prior log densities take (values, expected_mean, ...): the
# parameter values, their mean, then the density's parameters (log_sd,
# logit_rho) by name, and return 1 total.
#
# RTMB::dnorm() and the other RTMB:: densities are written out in full: they
# work on both AD values and ordinary numbers, while stats::dnorm() fails on
# AD values.
#
# To add a family or structure, write its functions here and add 1 entry to
# likelihood_families or process_structures at the bottom of this file.

#' Stop unless expected values and sds have 1 value or 1 per observation
#'
#' Recycling a vector of another length would compare observations with the
#' wrong expected values, with no error.
#' @noRd
check_density_lengths <- function(n_observations, ...) {
  lengths <- vapply(list(...), length, integer(1))
  is_wrong <- !lengths %in% c(1L, n_observations)
  if (any(is_wrong)) {
    cli::cli_abort(c(
      "x" = "{.val {names(lengths)[is_wrong]}} {?has/have}
      {.val {lengths[is_wrong]}} value{?s} for {.val {n_observations}}
      observation{?s}.",
      "i" = "Expected values and sds need 1 value, or 1 per observation.",
      "i" = "Count the data rows by fleet, type and year with
      {.code dplyr::count(get_data(data), fleet, type, timing)}."
    ))
  }
  invisible(n_observations)
}

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
  check_density_lengths(n_observations, expected = expected, log_sd = log_sd)
  expected <- rep_len(expected, n_observations)
  standard_deviation <- rep_len(exp(log_sd), n_observations)
  is_observed <- !is.na(observed)
  log_density <- numeric(n_observations)
  log_density[is_observed] <- RTMB::dnorm(
    observed[is_observed], expected[is_observed],
    standard_deviation[is_observed],
    log = TRUE
  )
  # Compositions keep their years-by-ages (or bins) shape, so each cell's log
  # density stays with its year in the report.
  if (is.matrix(observed)) {
    log_density <- RTMB::matrix(log_density, nrow(observed), ncol(observed))
  }
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
  check_density_lengths(n_observations, log_expected = log_expected, log_sd = log_sd)
  log_expected <- rep_len(log_expected, n_observations)
  standard_deviation <- rep_len(exp(log_sd), n_observations)
  is_observed <- !is.na(observed)
  log_density <- numeric(n_observations)
  log_density[is_observed] <- RTMB::dlnorm(
    observed[is_observed], log_expected[is_observed],
    standard_deviation[is_observed],
    log = TRUE
  )
  # Compositions keep their years-by-ages (or bins) shape, so each cell's log
  # density stays with its year in the report.
  if (is.matrix(observed)) {
    log_density <- RTMB::matrix(log_density, nrow(observed), ncol(observed))
  }
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

#' Robust normal approximation to the multinomial log density for compositions
#'
#' Fournier et al. (1990) normal approximation to the multinomial with fat
#' tails (`acl::robust_multi::pdf` in GMACS, with offset 1e-8). This is not a
#' normalized probability density over compositions. `observed` holds counts
#' (proportions times sample size), so `sum(counts)` is the sample size `n`
#' and `counts / sum(counts)` is the observed proportion. Each bin returns its
#' own term:
#'   log(exp(-n * l_b) + 0.01) - 0.5 * log(v_b / n),
#' where `v_b = 0.1 / B + o_b (1 - o_b)` and `l_b = 0.5 (q_b - o_b)^2 / v_b`.
#'
#' @param observed Matrix of observed counts (proportions times sample size),
#'   1 row per year.
#' @param prob Matrix of expected proportions, same shape as `observed`.
#' @return Matrix of per-bin log density values the same shape as `observed`.
#' @noRd
robust_normal_multinomial_lpdf <- function(observed, prob) {
  # RTMB: lets `x[i, ] <- value` store AD values in a plain matrix.
  "[<-" <- RTMB::ADoverload("[<-")
  n_bins <- ncol(observed)
  offset <- 1e-8
  log_density <- matrix(0, nrow(observed), n_bins)
  for (row_index in seq_len(nrow(observed))) {
    counts <- observed[row_index, ]
    if (anyNA(counts)) {
      next
    }
    sample_size <- sum(counts)
    if (sample_size <= 0) {
      next
    }
    observed_proportion <- counts / sample_size + offset
    observed_proportion <- observed_proportion / sum(observed_proportion)
    expected_proportion <- prob[row_index, ] + offset
    expected_proportion <- expected_proportion / sum(expected_proportion)
    variance <- 0.1 / n_bins + observed_proportion * (1 - observed_proportion)
    scaled_squared_error <- 0.5 *
      (expected_proportion - observed_proportion)^2 / variance
    log_density[row_index, ] <- log(
      exp(-sample_size * scaled_squared_error) + 0.01
    ) - 0.5 * log(variance / sample_size)
  }
  log_density
}

#' Formula stub for `parse_data_distribution()`
#'
#' `parse_one_data_distribution()` checks argument names with `formals()`.
#' Unlike `dnorm`, `dlnorm`, and `dmultinom`, `dmultinom_robust` is not in
#' `stats`, so this provides the same formal arguments as `stats::dmultinom`.
#' @noRd
dmultinom_robust <- function(x, size = NULL, prob, log = FALSE) {
  invisible(NULL)
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
  # rnorm() returns a vector, so rebuild a composition matrix.
  if (is.matrix(observed)) {
    draws <- matrix(draws, nrow(observed), ncol(observed))
  }
  replace(draws, is.na(observed), NA)
}

normal_draw <- function(observed, expected, log_sd) {
  n_observations <- length(observed)
  draws <- stats::rnorm(
    n_observations, rep_len(expected, n_observations),
    rep_len(exp(log_sd), n_observations)
  )
  # rnorm() returns a vector, so rebuild a composition matrix.
  if (is.matrix(observed)) {
    draws <- matrix(draws, nrow(observed), ncol(observed))
  }
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
# `values` is a parameter that varies by year or by age (a vector), or a
# years-by-ages matrix for iid. `expected_mean` is its mean with the same
# shape (or 0). AR1 and random walks go from each value to the next in the
# vector's order, which is year or age order (initialize_parameters()).
# For iid the SD is the SD around the mean. For AR1 and random walks it is
# the SD of 1 step (the innovation SD). Correlation rho is estimated as
# logit_rho, with rho = 2 / (1 + exp(-logit_rho)) - 1. So rho is between -1
# and 1, and logit_rho = log(3) gives rho = 0.5.
#
# `steps` is the number of steps from each value to the next, more than 1
# across constant values (process_densities(), process_steps()). NULL means
# 1 step each and keeps the plain form of each density.

#' Independent normal deviations around the mean (iid)
#'
#' x_t ~ Normal(m_t, sd), independently for every value (each year, each age,
#' or each age and year).
#'
#' @param values The process values.
#' @param expected_mean Their mean, the same shape or 1 value.
#' @param log_sd Log of the SD.
#' @param steps Not used. Each value is independent of the others.
#' @noRd
iid_process_lpdf <- function(values, expected_mean, log_sd, steps = NULL) {
  expected_mean <- rep_len(as.vector(expected_mean), length(values))
  standard_deviation <- rep_len(exp(log_sd), length(values))
  sum(RTMB::dnorm(values, expected_mean, standard_deviation, log = TRUE))
}

#' First-order autoregressive process around the mean (AR1)
#'
#' x_t - m_t = rho (x_(t-1) - m_(t-1)) + e_t, e_t ~ Normal(0, sd). The first
#' value comes from the stationary distribution, x_1 ~ Normal(m_1,
#' sd / sqrt(1 - rho^2)). With rho = 0 this is the iid process. Across k
#' steps the correlation is rho^k and the SD is
#' sd * sqrt((1 - rho^(2k)) / (1 - rho^2)), so the density of values with
#' gaps is the AR1 density of those values alone.
#'
#' @inheritParams iid_process_lpdf
#' @param logit_rho The correlation on the logit scale.
#'   rho = inv_logit(-1, 1, logit_rho) is between -1 and 1.
#' @param steps Steps from each value to the next, or NULL for 1 each.
#' @noRd
ar1_process_lpdf <- function(values, expected_mean, log_sd, logit_rho,
                             steps = NULL) {
  if (is.matrix(values) && ncol(values) > 1) {
    cli::cli_abort(c(
      "x" = "This parameter has {.val {ncol(values)}} columns (ages), so it
      varies by year and by age.",
      "i" = "An AR1 process varies by year or by age, not both. Use an iid
      process, {.code (1 | age:year)}, for a parameter that varies by age and
      year.",
      "i" = "Check the target rows with {.code dplyr::filter(parameters,
      re_structure == \"ar1\")}."
    ))
  }
  deviation <- values - rep_len(as.vector(expected_mean), length(values))
  standard_deviation <- exp(log_sd)
  rho <- inv_logit(-1, 1, logit_rho)
  n_values <- length(deviation)
  step_rho <- rho
  step_sd <- standard_deviation
  if (!is.null(steps)) {
    step_rho <- rho^steps
    step_sd <- standard_deviation * sqrt((1 - rho^(2 * steps)) / (1 - rho^2))
  }
  RTMB::dnorm(
    deviation[1], 0, standard_deviation / sqrt(1 - rho^2),
    log = TRUE
  ) +
    sum(RTMB::dnorm(
      deviation[-1], step_rho * deviation[-n_values], step_sd,
      log = TRUE
    ))
}

#' Random walk
#'
#' x_t = x_(t-1) + e_t, e_t ~ Normal(0, sd). Across k steps the SD is
#' sd * sqrt(k). A random walk has no mean, so `expected_mean` is ignored. It
#' is an argument only so every process density is called the same way. The
#' first value has no density term because it is a fixed effect
#' (add_process() makes it one).
#'
#' @inheritParams ar1_process_lpdf
#' @noRd
random_walk_process_lpdf <- function(values, expected_mean, log_sd,
                                     steps = NULL) {
  if (is.matrix(values) && ncol(values) > 1) {
    cli::cli_abort(c(
      "x" = "This parameter has {.val {ncol(values)}} columns (ages), so it
      varies by year and by age.",
      "i" = "A random walk varies by year or by age, not both. Use an iid
      process, {.code (1 | age:year)}, for a parameter that varies by age and
      year.",
      "i" = "Check the target rows with {.code dplyr::filter(parameters,
      re_structure == \"rw\")}."
    ))
  }
  n_values <- length(values)
  step_sd <- exp(log_sd)
  if (!is.null(steps)) {
    step_sd <- step_sd * sqrt(steps)
  }
  sum(RTMB::dnorm(values[-1], values[-n_values], step_sd, log = TRUE))
}

# Draws from the process densities, for simulation. Each takes the arguments
# of its log density and returns new values the same shape as `values`.

iid_process_draw <- function(values, expected_mean, log_sd, steps = NULL) {
  values[] <- stats::rnorm(
    length(values), rep_len(as.vector(expected_mean), length(values)),
    exp(log_sd)
  )
  values
}

ar1_process_draw <- function(values, expected_mean, log_sd, logit_rho,
                             steps = NULL) {
  n_values <- length(values)
  standard_deviation <- exp(log_sd)
  rho <- inv_logit(-1, 1, logit_rho)
  step_rho <- rep(rho, n_values - 1)
  step_sd <- rep(standard_deviation, n_values - 1)
  if (!is.null(steps)) {
    step_rho <- rho^steps
    step_sd <- standard_deviation * sqrt((1 - rho^(2 * steps)) / (1 - rho^2))
  }
  deviation <- numeric(n_values)
  deviation[1] <- stats::rnorm(1, 0, standard_deviation / sqrt(1 - rho^2))
  for (value in seq_len(n_values)[-1]) {
    deviation[value] <- step_rho[value - 1] * deviation[value - 1] +
      stats::rnorm(1, 0, step_sd[value - 1])
  }
  values[] <- rep_len(as.vector(expected_mean), n_values) + deviation
  values
}

#' The first value is a fixed effect and keeps its value.
#' @noRd
random_walk_process_draw <- function(values, expected_mean, log_sd,
                                     steps = NULL) {
  n_values <- length(values)
  step_sd <- exp(log_sd)
  if (!is.null(steps)) {
    step_sd <- step_sd * sqrt(steps)
  }
  values[] <- values[1] +
    c(0, cumsum(stats::rnorm(n_values - 1, 0, step_sd)))
  values
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
  ),
  # Fournier's robust normal approximation is not a normalized density, so
  # simulation uses a multinomial stand-in draw with the same sample size and
  # expected proportions.
  dmultinom_robust = list(
    lpdf = robust_normal_multinomial_lpdf,
    draw = multinomial_draw,
    distribution = "robust_multinomial",
    parameters = function(uncertainty) list()
  )
)

# Process and prior structures, by `re_structure` in the parameter tibble.
process_structures <- list(
  iid = list(lpdf = iid_process_lpdf, draw = iid_process_draw),
  ar1 = list(lpdf = ar1_process_lpdf, draw = ar1_process_draw),
  rw = list(lpdf = random_walk_process_lpdf, draw = random_walk_process_draw)
)
