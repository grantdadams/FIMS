# Helper data for tests of the parameter tibble ----
# A small parameter tibble in the FIMS format, used by the tests of
# add_process(), add_priors() and initialize_parameters(). It has a survey
# log_q with 1 value and a fishery log_Fmort by year, which sets the years 1
# to 6.
small_parameters <- tibble::tibble(
  module_name = "Fleet",
  fleet = c("survey1", rep("fleet1", 6)),
  module_type = NA_character_,
  label = c("log_q", rep("log_Fmort", 6)),
  age = NA_real_,
  length = NA_real_,
  timing = c(NA, 1:6),
  value = c(-2, rep(-1, 6)),
  estimation_type = "fixed_effects",
  distribution_type = NA_character_,
  distribution = NA_character_
)
