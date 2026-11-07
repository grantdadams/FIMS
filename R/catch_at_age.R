# Catch-at-age model: predictions
#
# catch_at_age() predicts everything the likelihood compares with data:
# biology and mortality, then the population year by year, then each fleet's
# catch, index and compositions. fims_model() (R/model.R) calls it, then adds
# the likelihood and reports.
#
# RTMB records the arithmetic the first time it runs (the "tape") and then
# evaluates the tape and its derivatives in C++, so this R code runs only when
# the model is built. Because of that, an `if` on an estimated value would be
# decided once, at the starting values, and never revisited. Branch only on
# data.
#
# Inputs, both from initialize_fims():
# - `module_parameters`: parameters grouped by module and fleet, e.g.,
#   module_parameters$Selectivity$fleet1$slope. A parameter that varies by
#   year is a matrix with 1 row per year.
# - `model_data`: dimensions (n_years, ages), 1 entry per population
#   (`populations`: module types and growth inputs) and 1 per fleet
#   (`fleets`: selectivity type, units, observed compositions, length bins,
#   age-to-length key or rebin weights, ageing error).
#
# Conventions:
# - Rows are model years 1 to n_years. Numbers at age and biomass have 1 more
#   row, the start of the year after the last model year.
# - The index and spawning biomass are at the start of the year (index =
#   q * S * N, with no mortality within the year). Catch is over the whole
#   year (Baranov).
# - Recruitment in year y comes from spawning biomass in year y - 1.
# - Catch and index are computed for model years only.
# - Selectivity is not rescaled to a maximum of 1, so annual F is the fully
#   selected F only when selectivity reaches 1.

#' Predictions of the catch-at-age model
#'
#' The model has 1 population, the first element of
#' `model_data[["populations"]]`.
#'
#' @param module_parameters Parameters grouped by module (group_parameters()).
#' @param model_data The `model_data` element of initialize_fims().
#' @return A list with `biology`, `population` and `fleets` (a list by fleet).
#' @noRd
catch_at_age <- function(module_parameters, model_data) {
  population_data <- model_data[["populations"]][[1]]
  biology <- population_biology(module_parameters, model_data, population_data)
  population <- population_dynamics(
    module_parameters, model_data, population_data, biology
  )
  list(
    biology = biology,
    population = population,
    fleets = fleet_predictions(module_parameters, model_data, biology, population)
  )
}

# Biology and mortality ------------------------------------------------------

#' Natural mortality, maturity, growth, selectivity and fishing mortality
#' @noRd
population_biology <- function(module_parameters, model_data, population_data) {
  n_years <- model_data[["n_years"]]
  ages <- model_data[["ages"]]
  module_types <- population_data[["module_types"]]

  # log_M is a years-by-ages matrix of log natural mortality.
  mortality_M <- exp(module_parameters[["Population"]][["log_M"]])

  # Maturity in each model year. The year after the last model year repeats
  # the last year, because maturity parameters are given for model years
  # only.
  maturity <- evaluate_by_year(
    maturity_functions, module_types[["maturity"]], ages,
    module_parameters[["Maturity"]], n_years
  )
  proportion_mature_at_age <- rbind(maturity, maturity[n_years, ])

  # Growth gives weight at age and, for length-based growth, the
  # distribution of length at age. Its non-parameter inputs (weight data, or
  # ages and a length grid) come from initialize_fims().
  growth <- do.call(
    growth_modules[[module_types[["growth"]]]][["evaluate"]],
    c(population_data[["growth_inputs"]], module_parameters[["Growth"]])
  )
  weight_at_age <- growth[["weight_at_age"]]
  if (!is.matrix(weight_at_age)) {
    # The same in every year, including the year after the last model year.
    weight_at_age <- RTMB::matrix(weight_at_age, n_years + 1, length(ages), byrow = TRUE)
  }

  # F at age = annual fishing mortality * selectivity at age, by fleet. Every
  # fleet adds to Z. A survey has log_Fmort fixed at -200, so its F is about 0.
  selectivity <- list()
  fishing_mortality_at_age <- list()
  for (fleet in names(model_data[["fleets"]])) {
    selectivity[[fleet]] <- evaluate_by_year(
      selectivity_functions,
      model_data[["fleets"]][[fleet]][["module_types"]][["selectivity"]], ages,
      module_parameters[["Selectivity"]][[fleet]], n_years
    )
    annual_f <- per_year(
      exp(module_parameters[["Fleet"]][[fleet]][["log_Fmort"]]), n_years,
      name = paste0("Fleet.", fleet, ".log_Fmort")
    )
    fishing_mortality_at_age[[fleet]] <- annual_f * selectivity[[fleet]]
  }
  mortality_F <- Reduce(`+`, fishing_mortality_at_age)

  list(
    mortality_M = mortality_M,
    proportion_female = module_parameters[["Population"]][["proportion_female"]],
    proportion_mature_at_age = proportion_mature_at_age,
    growth = growth,
    weight_at_age = weight_at_age,
    selectivity = selectivity,
    fishing_mortality_at_age = fishing_mortality_at_age,
    mortality_F = mortality_F,
    mortality_Z = mortality_M + mortality_F,
    sum_selectivity = Reduce(`+`, selectivity)
  )
}

# Population dynamics --------------------------------------------------------

#' Numbers at age, recruitment and spawning biomass, 1 year at a time
#' @noRd
population_dynamics <- function(module_parameters, model_data, population_data,
                                biology) {
  # RTMB: lets `x[i] <- value` and c(1, value) keep AD values that depend on
  # parameters when `x` starts as a plain matrix of zeros.
  "[<-" <- RTMB::ADoverload("[<-")
  "c" <- RTMB::ADoverload("c")
  n_years <- model_data[["n_years"]]
  n_ages <- model_data[["n_ages"]]
  module_types <- population_data[["module_types"]]
  mortality_M <- biology[["mortality_M"]]
  mortality_Z <- biology[["mortality_Z"]]
  proportion_female <- biology[["proportion_female"]]
  proportion_mature_at_age <- biology[["proportion_mature_at_age"]]
  weight_at_age <- biology[["weight_at_age"]]

  recruitment_parameters <- module_parameters[["Recruitment"]]
  # The process ("log_devs" or "log_r") names the parameter that holds 1
  # value per year from year 2 (n_years - 1 values).
  recruitment_process <- module_types[["recruitment_process"]]
  rzero <- exp(recruitment_parameters[["log_rzero"]])
  # Every other Recruitment parameter belongs to the stock-recruit function.
  # An unused log_r (when log_devs is the process) is left out too.
  stock_recruit_parameters <- recruitment_parameters[setdiff(
    names(recruitment_parameters),
    c(names(recruitment_process_functions), "log_sd")
  )]
  # Unfished spawning biomass per recruit, from first-year biology.
  phi_0 <- spawning_biomass_per_recruit(
    mortality_M[1, ], proportion_female,
    proportion_mature_at_age[1, ], weight_at_age[1, ]
  )

  numbers_at_age <- matrix(0, n_years + 1, n_ages)
  unfished_numbers_at_age <- matrix(0, n_years + 1, n_ages)
  spawning_biomass <- numeric(n_years + 1)
  unfished_spawning_biomass <- numeric(n_years + 1)
  # `log_expected_recruitment` is the log of median recruitment from the
  # stock-recruit curve (no bias correction). `expected_recruitment` is the
  # realized number of recruits, including the deviation.
  expected_recruitment <- numeric(n_years + 1)
  log_expected_recruitment <- numeric(n_years - 1)

  for (year in seq_len(n_years + 1)) {
    if (year == 1) {
      # First year: numbers at age are parameters. Unfished numbers are the
      # equilibrium age structure from R0 and first-year M.
      numbers_at_age[1, ] <- exp(module_parameters[["Population"]][["log_init_naa"]])
      unfished_numbers_at_age[1, ] <- rzero *
        numbers_per_recruit(mortality_M[1, ])
    } else {
      # Recruits (youngest age) from last year's spawning biomass.
      recruitment_from_curve <- call_module(
        recruitment_functions, module_types[["recruitment"]],
        spawning_biomass = spawning_biomass[year - 1], phi_0 = phi_0,
        parameters = stock_recruit_parameters
      )
      if (year <= n_years) {
        log_expected_recruitment[year - 1] <- log(recruitment_from_curve)
        numbers_at_age[year, 1] <- exp(call_module(
          recruitment_process_functions, recruitment_process,
          log_expected_recruitment = log_expected_recruitment[year - 1],
          process_value = recruitment_parameters[[recruitment_process]][year - 1]
        ))
      } else {
        # The year after the last model year has no deviation.
        numbers_at_age[year, 1] <- recruitment_from_curve
      }
      unfished_numbers_at_age[year, 1] <- rzero

      # Older ages: survivors of last year's previous age. The oldest age is
      # a plus group that also keeps its own survivors.
      survivors <- numbers_at_age[year - 1, ] * exp(-mortality_Z[year - 1, ])
      numbers_at_age[year, -1] <- survivors[-n_ages]
      numbers_at_age[year, n_ages] <- numbers_at_age[year, n_ages] +
        survivors[n_ages]

      unfished_survivors <- unfished_numbers_at_age[year - 1, ] *
        exp(-mortality_M[year - 1, ])
      unfished_numbers_at_age[year, -1] <- unfished_survivors[-n_ages]
      unfished_numbers_at_age[year, n_ages] <-
        unfished_numbers_at_age[year, n_ages] + unfished_survivors[n_ages]
    }
    expected_recruitment[year] <- numbers_at_age[year, 1]
    # 1 year at a time, because proportion_female can be a vector by age.
    spawning_biomass[year] <- sum(
      proportion_female * numbers_at_age[year, ] *
        proportion_mature_at_age[year, ] * weight_at_age[year, ]
    )
    unfished_spawning_biomass[year] <- sum(
      proportion_female * unfished_numbers_at_age[year, ] *
        proportion_mature_at_age[year, ] * weight_at_age[year, ]
    )
  }

  list(
    numbers_at_age = numbers_at_age,
    unfished_numbers_at_age = unfished_numbers_at_age,
    expected_recruitment = expected_recruitment,
    log_expected_recruitment = log_expected_recruitment,
    spawning_biomass = spawning_biomass,
    biomass = RTMB::rowSums(numbers_at_age * weight_at_age),
    unfished_biomass = RTMB::rowSums(unfished_numbers_at_age * weight_at_age),
    unfished_spawning_biomass = unfished_spawning_biomass,
    # Depletion is relative to unfished spawning biomass in the first year.
    spawning_biomass_ratio = spawning_biomass / unfished_spawning_biomass[1]
  )
}

# Fleet predictions ----------------------------------------------------------

#' Catch, index and compositions by fleet (model years only)
#'
#' @return A list by fleet of predicted quantities, named as in the report.
#' @noRd
fleet_predictions <- function(module_parameters, model_data, biology,
                              population) {
  n_years <- model_data[["n_years"]]
  model_years <- seq_len(n_years)
  numbers <- population[["numbers_at_age"]][model_years, ]
  mortality_Z <- biology[["mortality_Z"]]
  predictions <- list()
  for (fleet in names(model_data[["fleets"]])) {
    fleet_data <- model_data[["fleets"]][[fleet]]
    # The fleet's own weight at age if it has one, otherwise the population's.
    fleet_weight <- if (is.null(fleet_data[["weight_at_age"]])) {
      biology[["weight_at_age"]][model_years, ]
    } else {
      fleet_data[["weight_at_age"]]
    }

    # Baranov catch equation.
    catch_numbers_at_age <- biology[["fishing_mortality_at_age"]][[fleet]] /
      mortality_Z * numbers * (1 - exp(-mortality_Z))

    # Index at the start of the year. q can vary by year.
    catchability <- per_year(
      exp(module_parameters[["Fleet"]][[fleet]][["log_q"]]), n_years,
      name = paste0("Fleet.", fleet, ".log_q")
    )
    index_numbers_at_age <- catchability * biology[["selectivity"]][[fleet]] *
      numbers

    quantities <- list(
      catch_numbers_at_age = catch_numbers_at_age,
      catch_weight_at_age = catch_numbers_at_age * fleet_weight,
      index_numbers_at_age = index_numbers_at_age,
      index_weight_at_age = index_numbers_at_age * fleet_weight
    )
    quantities[["catch_numbers"]] <- RTMB::rowSums(catch_numbers_at_age)
    quantities[["catch_weight"]] <- RTMB::rowSums(quantities[["catch_weight_at_age"]])
    quantities[["index_numbers"]] <- RTMB::rowSums(index_numbers_at_age)
    quantities[["index_weight"]] <- RTMB::rowSums(quantities[["index_weight_at_age"]])
    quantities[["catch_expected"]] <- if (fleet_data[["observed_catch_units"]] == "number") {
      quantities[["catch_numbers"]]
    } else {
      quantities[["catch_weight"]]
    }
    quantities[["index_expected"]] <- if (fleet_data[["observed_index_units"]] == "number") {
      quantities[["index_numbers"]]
    } else {
      quantities[["index_weight"]]
    }
    quantities[["log_catch_expected"]] <- log(quantities[["catch_expected"]])
    quantities[["log_index_expected"]] <- log(quantities[["index_expected"]])

    # Compositions: the age composition comes from the catch if the fleet has
    # catch data, otherwise from the index.
    numbers_for_composition <- if (fleet_data[["has_catch"]]) {
      catch_numbers_at_age
    } else {
      index_numbers_at_age
    }
    age_composition <- evaluate_age_comp(
      numbers_for_composition, fleet_data[["age_comp"]]
    )
    quantities[["agecomp_expected"]] <- age_composition[["expected"]]
    quantities[["agecomp_proportion"]] <- age_composition[["proportion"]]

    # The age-to-length key, P(length bin | age). It is either a fixed key
    # from the data (1, or 1 per year), or, when initialize_fims() stored
    # rebin weights for the fleet, the growth length distribution moved onto
    # the fleet's length bins.
    age_to_length <- fleet_data[["age_to_length_conversion"]]
    if (!is.null(fleet_data[["rebin_weights"]])) {
      age_to_length <- age_to_length_conversion_derived(
        biology[["growth"]][["length_probability"]],
        fleet_data[["rebin_weights"]]
      )
      quantities[["age_to_length_conversion_derived"]] <- age_to_length
      # Mean weight at age implied by the fleet's key: weight at each bin's
      # midpoint, averaged over P(length bin | age). Reported only.
      growth <- biology[["growth"]]
      mean_weight_at_age <- age_to_length %*% weight_at_length(
        fleet_data[["length_bin_midpoints"]],
        growth[["length_weight_a"]], growth[["length_weight_b"]]
      )
      quantities[["growth_derived_mean_WAA"]] <- RTMB::matrix(
        as.vector(mean_weight_at_age), n_years, length(mean_weight_at_age),
        byrow = TRUE
      )
    }
    if (!is.null(age_to_length)) {
      length_composition <- evaluate_length_comp(
        age_composition[["proportion"]], age_to_length,
        fleet_data[["length_comp"]]
      )
      quantities[["lengthcomp_expected"]] <- length_composition[["expected"]]
      quantities[["lengthcomp_proportion"]] <-
        length_composition[["proportion"]]
      quantities[["catch_numbers_at_length"]] <-
        apply_age_to_length_conversion(catch_numbers_at_age, age_to_length)
      quantities[["index_numbers_at_length"]] <-
        apply_age_to_length_conversion(index_numbers_at_age, age_to_length)
    }

    conditional_age_at_length <- fleet_data[["conditional_age_at_length"]]
    if (!is.null(conditional_age_at_length)) {
      quantities[["conditional_age_at_length_proportion"]] <-
        expected_conditional_age_at_length(
          numbers_for_composition, age_to_length,
          conditional_age_at_length[["year"]],
          conditional_age_at_length[["length_bin"]],
          ageing_error = fleet_data[["ageing_error"]]
        )
    }

    # Ageing error moves the expected age composition from true to read ages.
    # This comes after the length composition, which needs true ages.
    ageing_error <- fleet_data[["ageing_error"]]
    if (!is.null(ageing_error) && !is.null(fleet_data[["age_comp"]])) {
      quantities[["agecomp_proportion"]] <- apply_ageing_error(
        quantities[["agecomp_proportion"]], ageing_error
      )
      quantities[["agecomp_expected"]] <- apply_ageing_error(
        quantities[["agecomp_expected"]], ageing_error
      )
    }
    predictions[[fleet]] <- quantities
  }
  predictions
}

# Model types, by `model_type` in the model data.
model_functions <- list(
  CatchAtAge = catch_at_age
)
