# Depletion modules
#
# A depletion module gives the expected depletion (biomass divided by
# carrying capacity K) at the start of next year from depletion at the start
# of this year and this year's catch.

#' Pella-Tomlinson depletion
#'
#' P_(t+1) = P_t + r / (m - 1) * P_t * (1 - P_t^(m - 1)) - C_t / K
#'
#' Surplus production, r / (m - 1) * P * (1 - P^(m - 1)), is in units of K.
#' Catch is taken after production, at the end of the year. The shape m sets
#' depletion at MSY to m^(-1 / (m - 1)), e.g., 0.5 for the Schaefer model
#' (m = 2). This is the generalized logistic N + r_max * N * (1 - (N / K)^z) -
#' C with N = P * K, r_max = r / (m - 1) and z = m - 1.
#'
#' @param depletion Depletion at the start of the year, P_t = B_t / K.
#' @param catch Catch during the year, C_t, in the units of K.
#' @param log_growth_rate Log of the intrinsic growth rate r. The harvest rate
#'   at MSY is r / m.
#' @param log_carrying_capacity Log of the carrying capacity K.
#' @param log_shape Log of the shape m, which cannot be 1.
#' @return Expected depletion at the start of the next year. It is below 0
#'   when the catch is more than the biomass plus production.
#' @noRd
pella_tomlinson_depletion <- function(depletion, catch, log_growth_rate,
                                      log_carrying_capacity, log_shape) {
  growth_rate <- exp(log_growth_rate)
  shape <- exp(log_shape)
  depletion +
    growth_rate / (shape - 1) * depletion * (1 - depletion^(shape - 1)) -
    catch / exp(log_carrying_capacity)
}

# Module types, by the `module_type` in the parameter tibble.
depletion_functions <- list(
  PellaTomlinson = pella_tomlinson_depletion
)

# The mean of the depletion process density: log_depletion varies around the
# log of the expected depletion from the module function.
depletion_process_means <- list(
  log_depletion = "log_expected_depletion"
)
