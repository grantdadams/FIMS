# Recruitment modules
#
# A recruitment module has 2 parts:
# - a stock-recruitment function giving mean (expected) recruitment from
#   spawning biomass, such as beverton_holt_recruitment().
# - a recruitment process giving log recruitment in each year from the log of
#   expected recruitment, such as log_devs_recruitment_process().

#' Beverton-Holt stock-recruitment, steepness form
#'
#' R = 0.8 R0 h S / (0.2 phi0 R0 (1 - h) + S (h - 0.2))
#'
#' Steepness h is estimated on a logit scale bounded to (0.2, 1):
#' h = 0.2 + 0.8 / (1 + exp(-logit_steep)).
#'
#' @param spawning_biomass Spawning biomass S in the previous year.
#' @param log_rzero Log of unfished recruitment R0.
#' @param logit_steep Steepness on the logit scale described above.
#' @param phi_0 Unfished spawning biomass per recruit, from
#'   spawning_biomass_per_recruit().
#' @noRd
beverton_holt_recruitment <- function(spawning_biomass, log_rzero,
                                      logit_steep, phi_0) {
  rzero <- exp(log_rzero)
  steepness <- 0.2 + (1 - 0.2) / (1 + exp(-logit_steep))
  (0.8 * rzero * steepness * spawning_biomass) /
    (0.2 * phi_0 * rzero * (1 - steepness) +
      spawning_biomass * (steepness - 0.2))
}

#' Unfished spawning biomass per recruit (phi0)
#'
#' Survival of 1 recruit through the ages with natural mortality only, with
#' the oldest age as a plus group:
#'   n_1 = 1, n_a = n_(a-1) exp(-M_(a-1)),
#'   n_A = n_(A-1) exp(-M_(A-1)) / (1 - exp(-M_A)).
#' phi0 = sum over ages of n_a * proportion female * proportion mature * weight.
#'
#' @param mortality_M Natural mortality at age (first model year).
#' @param proportion_female Proportion female at age, or 1 value for all ages.
#' @param proportion_mature Proportion mature at age (first model year).
#' @param weight_at_age Weight at age (first model year).
#' @noRd
spawning_biomass_per_recruit <- function(mortality_M, proportion_female,
                                         proportion_mature, weight_at_age) {
  # RTMB: lets c(1, value) keep AD values that depend on parameters.
  "c" <- RTMB::ADoverload("c")
  n_ages <- length(mortality_M)
  survival_to_age <- c(1, exp(-cumsum(mortality_M[-n_ages])))
  # The plus group holds all older ages, the sum of a geometric series.
  plus_group_scale <- c(
    rep(1, n_ages - 1),
    1 / (1 - exp(-mortality_M[n_ages]))
  )
  numbers_per_recruit <- survival_to_age * plus_group_scale
  sum(numbers_per_recruit * proportion_female * proportion_mature *
    weight_at_age)
}

# Recruitment processes
#
# A process gives log recruitment in a year from that year's value of its
# parameter (`process_value`) and the log of expected recruitment. The mean
# of the process density is 0 for deviations and log E[R] for log
# recruitment (see recruitment_process_means below).

#' Recruitment process: log deviations from the stock-recruitment curve
#'
#' `log R_y = log E[R_y] + log_devs_y`, with `log_devs_y ~ Normal(0, sd)`.
#'
#' @param log_expected_recruitment Log of mean recruitment from the
#'   stock-recruitment function.
#' @param process_value The log deviation for the same year.
#' @noRd
log_devs_recruitment_process <- function(log_expected_recruitment,
                                         process_value) {
  log_expected_recruitment + process_value
}

#' Recruitment process: log recruitment as the parameter
#'
#' `log R_y = log_r_y`, with `log_r_y ~ Normal(log E[R_y], sd)`.
#' @noRd
log_r_recruitment_process <- function(log_expected_recruitment,
                                      process_value) {
  process_value
}

# Module types, by the `module_type` in the parameter tibble.
recruitment_functions <- list(
  BevertonHolt = beverton_holt_recruitment
)

# Recruitment processes, by the label of the parameter that holds 1 value
# per year.
recruitment_process_functions <- list(
  log_devs = log_devs_recruitment_process,
  log_r = log_r_recruitment_process
)

# The mean of each process's density: "zero" for deviations, or the name of
# the population quantity it varies around.
recruitment_process_means <- list(
  log_devs = "zero",
  log_r = "log_expected_recruitment"
)
