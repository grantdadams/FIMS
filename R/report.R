# Reporting
#
# report_model() reports each quantity as a list with 1 element per
# population (population quantities) or per fleet (fleet quantities).
# Matrices are flattened by year, then age or length, the layout
# get_estimates() reads. A fleet without a quantity (for example no length
# data) has an empty element. Standard errors (ADREPORT) are on the same
# quantities, with the fleets' values stacked into 1 vector per quantity.
#
# RTMB's REPORT() and ADREPORT() name each quantity after the variable passed
# to them, so report_list() creates a variable with the right name for each
# list element.

# The dimensions of each quantity in the get_estimates() table, which labels
# every value with its year, age and length bin. Matrices are flattened by
# year (flatten_by_year()).
# - "year": 1 value per year.
# - "year_age": years by ages.
# - "year_length": years by the fleet's length bins.
# - "conditional_age_at_length": 1 row per year and length bin with
#   conditional age-at-length data, by ages.
# - "growth_increment": 1 value per observed molt increment row, by pre-molt
#   length.
# - "na": 1 value, not by year.
# Population quantities cover the model years, some with 1 more year for the
# start of the year after the last model year. Fleet quantities cover the
# model years. A reported quantity left out of the table is listed in `skip`
# in derived_quantity_rows() (R/output.R).
quantity_dimensions <- c(
  # Population quantities
  biomass = "year",
  expected_recruitment = "year",
  mortality_F = "year_age",
  mortality_M = "year_age",
  mortality_Z = "year_age",
  numbers_at_age = "year_age",
  proportion_mature_at_age = "year_age",
  spawning_biomass = "year",
  sum_selectivity = "year_age",
  total_catch_numbers = "year",
  total_catch_weight = "year",
  unfished_biomass = "year",
  unfished_numbers_at_age = "year_age",
  unfished_spawning_biomass = "year",
  spawning_biomass_ratio = "year",
  depletion = "year",
  harvest_rate = "year",
  harvest_rate_at_msy = "na",
  biomass_at_msy = "na",
  maximum_sustainable_yield = "na",
  mortality_M_immature = "year_length",
  mortality_M_mature = "year_length",
  mortality_Z_immature = "year_length",
  mortality_Z_mature = "year_length",
  numbers_at_length_immature = "year_length",
  numbers_at_length_mature = "year_length",
  total_numbers_at_length = "year_length",
  growth_increment_expected = "growth_increment",
  log_growth_increment_expected = "growth_increment",
  # Fleet quantities
  catch_numbers_at_age = "year_age",
  catch_weight_at_age = "year_age",
  index_numbers_at_age = "year_age",
  index_weight_at_age = "year_age",
  catch_numbers = "year",
  catch_weight = "year",
  catch_retained_numbers = "year",
  catch_retained_weight = "year",
  catch_discarded_numbers = "year",
  catch_discarded_weight = "year",
  index_numbers = "year",
  index_weight = "year",
  catch_expected = "year",
  catch_retained_expected = "year",
  catch_discarded_expected = "year",
  index_expected = "year",
  log_catch_expected = "year",
  log_catch_retained_expected = "year",
  log_catch_discarded_expected = "year",
  log_index_expected = "year",
  agecomp_expected = "year_age",
  agecomp_proportion = "year_age",
  lengthcomp_expected = "year_length",
  lengthcomp_proportion = "year_length",
  lengthcomp_retained_expected = "year_length",
  lengthcomp_retained_proportion = "year_length",
  lengthcomp_discarded_expected = "year_length",
  lengthcomp_discarded_proportion = "year_length",
  lengthcomp_immature_expected = "year_length",
  lengthcomp_immature_proportion = "year_length",
  lengthcomp_mature_expected = "year_length",
  lengthcomp_mature_proportion = "year_length",
  catch_numbers_at_length = "year_length",
  catch_weight_at_length = "year_length",
  catch_retained_numbers_at_length = "year_length",
  catch_retained_weight_at_length = "year_length",
  catch_discarded_numbers_at_length = "year_length",
  catch_discarded_weight_at_length = "year_length",
  index_numbers_at_length = "year_length",
  index_weight_at_length = "year_length",
  conditional_age_at_length_proportion = "conditional_age_at_length",
  catchability = "na"
)

#' Report the model's quantities
#'
#' Nothing here changes the likelihood. The dimensions of each quantity in
#' the get_estimates() table are in `quantity_dimensions` above.
#'
#' @param model_data The `model_data` element of initialize_fims().
#' @param model Output of model_predictions().
#' @param likelihood Output of model_likelihood().
#' @noRd
report_model <- function(model_data, model, likelihood) {
  # RTMB: lets do.call(c, by_fleet) keep AD values when the first fleet has
  # none, e.g., a catch fleet with no index in a surplus production model.
  "c" <- RTMB::ADoverload("c")
  n_years <- model_data[["n_years"]]
  model_type <- model_data[["model_type"]]
  fleets <- model[["fleets"]]

  population_values <- population_quantities(model, model_type)
  fleet_quantity_names <- unique(unlist(lapply(fleets, names)))
  fleet_quantities <- lapply(
    stats::setNames(fleet_quantity_names, fleet_quantity_names),
    function(name) {
      lapply(unname(fleets), function(quantities) {
        if (is.null(quantities[[name]])) numeric(0) else flatten_by_year(quantities[[name]])
      })
    }
  )

  # Growth and the age-to-length keys are catch-at-age quantities, reported
  # without standard errors.
  growth_quantities <- list()
  no_standard_error <- character(0)
  if (model_type == "CatchAtAge") {
    # Growth by age, empty unless growth models length. Growth does not
    # change by year, but it is reported for every year, like the other
    # at-age quantities.
    growth <- model[["biology"]][["growth"]]
    by_year <- function(name) {
      if (is.null(growth[["mean_length_at_age"]])) numeric(0) else rep(growth[[name]], n_years)
    }
    growth_quantities <- list(
      growth_mean_LAA = by_year("mean_length_at_age"),
      growth_sd_LAA = by_year("sd_length_at_age"),
      growth_mean_WAA = by_year("weight_at_age")
    )
    # The age-to-length keys. The full derived key, 1 copy per year, is
    # large, so it is reported only when asked for with fit_fims()'s
    # report_age_to_length_conversion_derived argument.
    fleet_data <- unname(model_data[["fleets"]])
    derived_key <- fleet_quantities[["age_to_length_conversion_derived"]]
    if (is.null(derived_key)) {
      derived_key <- lapply(fleet_data, function(x) numeric(0))
    }
    key_quantities <- list(
      age_to_length_conversion = lapply(fleet_data, function(fleet) {
        key <- fleet[["age_to_length_conversion"]]
        if (is.matrix(key)) key <- list(key)
        unlist(lapply(key, flatten_by_year))
      }),
      age_to_length_conversion_derived = lapply(derived_key, function(key) {
        if (isTRUE(model_data[["report_age_to_length_conversion_derived_tensor"]])) {
          rep(key, n_years)
        } else {
          numeric(0)
        }
      }),
      age_to_length_conversion_derived_used = lapply(fleet_data, function(fleet) {
        as.numeric(!is.null(fleet[["rebin_weights"]]))
      })
    )
    if (is.null(fleet_quantities[["growth_derived_mean_WAA"]])) {
      key_quantities[["growth_derived_mean_WAA"]] <- lapply(fleet_data, function(x) numeric(0))
    }
    fleet_quantities[names(key_quantities)] <- key_quantities
    no_standard_error <- c(names(key_quantities), "growth_derived_mean_WAA")
  }

  report_list(lapply(c(population_values, growth_quantities), list))
  report_list(fleet_quantities)
  report_list(likelihood)
  # The mean of the recruitment or depletion process (log_r or log_depletion)
  # is reported, but has no rows in get_estimates().
  process_means <- unlist(c(recruitment_process_means, depletion_process_means))
  report_list(model[["population"]][
    intersect(process_means, names(model[["population"]]))
  ])
  if (isTRUE(model_data[["adreport"]])) {
    # Standard errors for the population and fleet quantities.
    report_list(population_values, reporter = "ADREPORT")
    report_list(
      lapply(
        fleet_quantities[setdiff(names(fleet_quantities), no_standard_error)],
        function(by_fleet) do.call(c, by_fleet)
      ),
      reporter = "ADREPORT"
    )
  }
}

#' The population quantities in the report
#'
#' 1 value per year, per year and age flattened by year, or 1 value (the MSY
#' quantities of the surplus production model). get_estimates() reads the
#' names to tell population quantities from fleet quantities.
#'
#' @param model Output of model_predictions().
#' @param model_type The `model_type` of the model data.
#' @return A named list of the population quantities.
#' @noRd
population_quantities <- function(model, model_type) {
  if (model_type == "SurplusProduction") {
    return(model[["population"]][c(
      "biomass", "depletion", "harvest_rate", "harvest_rate_at_msy",
      "biomass_at_msy", "maximum_sustainable_yield"
    )])
  }
  if (model_type == "CatchAtLength") {
    biology <- model[["biology"]]
    population <- model[["population"]]
    fleets <- model[["fleets"]]
    quantities <- list(
      biomass = population[["biomass"]],
      expected_recruitment = population[["expected_recruitment"]],
      mortality_F = flatten_by_year(biology[["mortality_F"]]),
      mortality_M_immature = flatten_by_year(biology[["mortality_M"]][["immature"]]),
      mortality_M_mature = flatten_by_year(biology[["mortality_M"]][["mature"]]),
      mortality_Z_immature = flatten_by_year(biology[["mortality_Z"]][["immature"]]),
      mortality_Z_mature = flatten_by_year(biology[["mortality_Z"]][["mature"]]),
      numbers_at_length_immature = flatten_by_year(
        population[["numbers_at_length_immature"]]
      ),
      numbers_at_length_mature = flatten_by_year(
        population[["numbers_at_length_mature"]]
      ),
      total_numbers_at_length = flatten_by_year(
        population[["total_numbers_at_length"]]
      ),
      spawning_biomass = population[["spawning_biomass"]],
      sum_selectivity = flatten_by_year(biology[["sum_selectivity"]]),
      total_catch_numbers = Reduce(`+`, lapply(fleets, `[[`, "catch_numbers")),
      total_catch_weight = Reduce(`+`, lapply(fleets, `[[`, "catch_weight"))
    )
    if (!is.null(population[["growth_increment_expected"]])) {
      quantities[["growth_increment_expected"]] <-
        population[["growth_increment_expected"]]
      quantities[["log_growth_increment_expected"]] <-
        population[["log_growth_increment_expected"]]
    }
    return(quantities)
  }
  biology <- model[["biology"]]
  population <- model[["population"]]
  fleets <- model[["fleets"]]
  list(
    biomass = population[["biomass"]],
    expected_recruitment = population[["expected_recruitment"]],
    mortality_F = flatten_by_year(biology[["mortality_F"]]),
    mortality_M = flatten_by_year(biology[["mortality_M"]]),
    mortality_Z = flatten_by_year(biology[["mortality_Z"]]),
    numbers_at_age = flatten_by_year(population[["numbers_at_age"]]),
    proportion_mature_at_age =
      flatten_by_year(biology[["proportion_mature_at_age"]]),
    spawning_biomass = population[["spawning_biomass"]],
    sum_selectivity = flatten_by_year(biology[["sum_selectivity"]]),
    total_catch_numbers = Reduce(`+`, lapply(fleets, `[[`, "catch_numbers")),
    total_catch_weight = Reduce(`+`, lapply(fleets, `[[`, "catch_weight")),
    unfished_biomass = population[["unfished_biomass"]],
    unfished_numbers_at_age =
      flatten_by_year(population[["unfished_numbers_at_age"]]),
    unfished_spawning_biomass = population[["unfished_spawning_biomass"]],
    spawning_biomass_ratio = population[["spawning_biomass_ratio"]]
  )
}

#' REPORT() or ADREPORT() each element of a named list under its name
#'
#' @param quantities A named list.
#' @param reporter "REPORT", or "ADREPORT" for quantities that get standard
#'   errors.
#' @noRd
report_list <- function(quantities, reporter = "REPORT") {
  for (name in names(quantities)) {
    assign(name, quantities[[name]])
    eval(call(reporter, as.name(name)))
  }
}

#' Flatten a years-by-ages (or years-by-lengths) matrix by year
#'
#' The result is 1 vector ordered by year, then age. A vector is returned as
#' it is.
#' @noRd
flatten_by_year <- function(x) {
  if (is.matrix(x)) c(t(x)) else x
}
