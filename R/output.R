# The get_estimates() table
#
# The 24-column table is built from the parameter tibble, obj$report() and
# the sdreport. It has 1 row per parameter value (including constants) and 1
# per derived quantity value. Each data set's observations, expected values
# and likelihood values are added to the derived-quantity rows of its
# expected values. parameter_id numbers the parameter values. Modules have no
# object id of their own, so type_id is NA on every row.

estimate_columns <- c(
  "module_name", "module_id", "module_type", "label", "type", "type_id",
  "parameter_id", "fleet", "year_i", "age", "length", "input", "estimated",
  "expected", "observed", "estimation_type", "uncertainty", "distribution",
  "input_type", "lpdf", "likelihood", "log_sd", "log_like_cv", "gradient"
)

#' Build the get_estimates() table
#'
#' @param input The list returned by initialize_fims().
#' @param obj The object from RTMB::MakeADFun().
#' @param report The model report at the best parameters.
#' @param gradient The gradient of the fixed effects at the optimum, or NA
#'   values if the model was not optimized.
#' @param sdreport The sdreport, or an empty list.
#' @return A tibble with the 24 columns of get_estimates().
#' @noRd
estimates_table <- function(input, obj, report, gradient, sdreport) {
  parameter_list <- obj[["env"]][["parList"]](
    par = obj[["env"]][["last.par.best"]]
  )
  values <- assemble_split_parameters(
    parameter_list, input[["model_data"]][["split_parameters"]]
  )
  population <- model_predictions(
    parameter_list, input[["model_data"]]
  )[["population"]]
  fleets <- names(input[["model_data"]][["fleets"]])
  dplyr::bind_rows(
    parameter_rows(input, obj, gradient, sdreport, values),
    derived_quantity_rows(input, report, sdreport),
    weight_at_age_rows(input)
  ) |>
    add_data_rows(input, report, values) |>
    add_process_rows(input, values, population) |>
    dplyr::mutate(
      # Fleet rows, and the rows of each fleet's selectivity and data
      # modules, are numbered by fleet order. The model has 1 population, so
      # each other population module is 1. The Population module's own rows
      # have no id.
      module_id = dplyr::case_when(
        .data[["module_name"]] == "Population" ~ NA_integer_,
        is.na(.data[["fleet"]]) ~ 1L,
        .default = match(.data[["fleet"]], fleets)
      ),
      type_id = NA_integer_
    ) |>
    dplyr::select(dplyr::all_of(estimate_columns))
}

#' 1 row per parameter value, with its estimate, standard error and gradient
#' @noRd
parameter_rows <- function(input, obj, gradient, sdreport, values) {
  parameter_table <- input[["parameter_table"]]
  # Each parameter's values in the order of its rows in the table.
  parameter_table[["estimated"]] <- unlist(lapply(
    unique(parameter_table[["name"]]),
    function(name) flatten_by_year(values[[name]])
  ))
  parameter_table[["uncertainty"]] <- NA_real_
  parameter_table[["gradient"]] <- NA_real_
  if (length(sdreport) > 0) {
    # Each row's estimate is element `rtmb_level` of RTMB parameter
    # `rtmb_name` (rows that share a map_group share an element).
    by_name <- function(x, parameter_names) split(unname(x), parameter_names)
    fixed_standard_errors <- by_name(
      sqrt(diag(sdreport[["cov.fixed"]])), names(sdreport[["par.fixed"]])
    )
    gradient <- by_name(gradient, names(obj[["par"]]))
    random_standard_errors <- by_name(
      sqrt(as.numeric(sdreport[["diag.cov.random"]])),
      names(sdreport[["par.random"]])
    )
    # Rows that share 1 estimate get its standard error. Its gradient goes on
    # the first of them only, so gradients are not counted more than once.
    is_first_of_level <- !duplicated(paste(
      parameter_table[["rtmb_name"]], parameter_table[["rtmb_level"]]
    ))
    for (table_row in which(!is.na(parameter_table[["rtmb_level"]]))) {
      name <- parameter_table[["rtmb_name"]][table_row]
      level <- parameter_table[["rtmb_level"]][table_row]
      if (name %in% names(fixed_standard_errors)) {
        parameter_table[["uncertainty"]][table_row] <- fixed_standard_errors[[name]][level]
        if (is_first_of_level[table_row]) {
          parameter_table[["gradient"]][table_row] <- gradient[[name]][level]
        }
      } else if (name %in% names(random_standard_errors)) {
        parameter_table[["uncertainty"]][table_row] <- random_standard_errors[[name]][level]
      }
    }
  }
  # Growth parameters estimated on a log or logit scale are shown on the
  # natural scale under their tibble labels. The logit for L1 is bounded by 0
  # and L2 (mean_length_old), in length units.
  estimated <- parameter_table[["estimated"]]
  is_log <- parameter_table[["working_scale"]] %in% "log"
  estimated[is_log] <- exp(estimated[is_log])
  is_logit <- parameter_table[["working_scale"]] %in% "logit"
  if (any(is_logit)) {
    mean_length_old <- exp(values[["Growth.log_mean_length_old"]])
    estimated[is_logit] <- mean_length_old / (1 + exp(-estimated[is_logit]))
  }
  parameter_table[["estimated"]] <- estimated
  # A prior's or process's own parameters are named after their target
  # (log_q_prior_log_sd, log_q_log_sd), so each row says what it belongs to.
  parameter_table[["label"]] <- dplyr::if_else(
    is.na(parameter_table[["target_label"]]),
    parameter_table[["original_label"]], parameter_table[["flat_label"]]
  )
  parameter_table[["value"]] <- parameter_table[["original_value"]]
  # A recruitment log_sd with no recruitment process to belong to is not a
  # model parameter, so it is not listed.
  is_unused_sd <- parameter_table[["module_name"]] == "Recruitment" &
    parameter_table[["label"]] == "log_sd" &
    !parameter_table[["name"]] %in% input[["model_data"]][["density_parameter_names"]]
  parameter_table <- dplyr::filter(parameter_table, !is_unused_sd)
  # Reference ages are data in the model. They are listed as constants, with
  # the other growth inputs.
  reference_ages <- input[["tibble"]] |>
    dplyr::filter(
      .data[["module_name"]] == "Growth",
      grepl("^reference_age", .data[["label"]])
    ) |>
    dplyr::mutate(estimated = .data[["value"]], estimation_type = "constant")
  dplyr::bind_rows(parameter_table, reference_ages) |>
    dplyr::mutate(
      type = "vector",
      # A position from 1 (the first model year), not the calendar year in
      # `timing`.
      year_i = .data[["timing"]] - input[["model_data"]][["start_year"]] + 1,
      input = .data[["value"]],
      distribution = dplyr::if_else(
        .data[["label"]] == "log_sd" & .data[["module_name"]] == "Recruitment",
        "normal", NA_character_
      )
    )
}

#' Add each iid process value's expected value, log_sd and log density
#'
#' `likelihood` is each value's log density and `lpdf` the process total.
#' Only random effects get them. A fixed-effect (penalized) process is in the
#' objective, but its rows are left NA. Other process structures have no
#' density for 1 value on its own, so their rows are left NA too.
#' @noRd
add_process_rows <- function(estimates, input, values, population) {
  for (density_entry in input[["model_data"]][["densities"]]) {
    target <- which(estimates[["name"]] %in% density_entry[["target"]])
    if (density_entry[["distribution_type"]] != "process" ||
      density_entry[["structure"]] != "iid" ||
      !all(estimates[["estimation_type"]][target] == "random_effects")) {
      next
    }
    inputs <- density_inputs(density_entry, values, population)
    process_values <- as.vector(inputs[["x"]])
    process_mean <- rep_len(as.vector(inputs[["mean"]]), length(process_values))
    log_sd <- rep_len(
      inputs[["hyperparameters"]][["log_sd"]], length(process_values)
    )
    # Each value's log density, from the same function the model uses.
    log_density <- purrr::map_dbl(seq_along(process_values), function(i) {
      iid_process_lpdf(process_values[i], process_mean[i], log_sd[i])
    })
    # Each row's position in the parameter's own order (by column).
    position <- value_position(estimates[target, ], inputs[["x"]])
    estimates[["expected"]][target] <- process_mean[position]
    estimates[["log_sd"]][target] <- log_sd[position]
    estimates[["likelihood"]][target] <- log_density[position]
    # lpdf is the process total, on every row of the process.
    estimates[["lpdf"]][target] <- sum(log_density)
  }
  estimates
}

#' 1 row per derived quantity value, with its standard error
#'
#' Each reported quantity is a list with 1 element per population or fleet
#' (report_model()).
#' @noRd
derived_quantity_rows <- function(input, report, sdreport) {
  model_data <- input[["model_data"]]
  fleets <- names(model_data[["fleets"]])
  has_se <- length(sdreport) > 0
  # Not derived quantities, or the growth and age-to-length diagnostics,
  # which are in the report but not in the table.
  skip <- c(
    "jnll", "nll_components", "observation_lpdf", "log_expected_recruitment",
    "age_to_length_conversion", "age_to_length_conversion_derived",
    "age_to_length_conversion_derived_used", "growth_derived_mean_WAA",
    "growth_mean_WAA", "growth_mean_LAA", "growth_sd_LAA"
  )
  rows <- list()
  for (name in setdiff(names(report), skip)) {
    is_fleet <- !name %in% population_quantity_names
    groups <- if (is_fleet) fleets else NA_character_
    standard_errors <- if (has_se && name %in% names(sdreport[["value"]])) {
      sdreport[["sd"]][names(sdreport[["value"]]) == name]
    }
    n_earlier_values <- 0
    for (i in seq_along(groups)) {
      value <- report[[name]][[i]]
      if (length(value) == 0) {
        next
      }
      dimensions <- if (is_fleet) {
        fleet_dimensions(name, length(value), model_data, groups[i])
      } else {
        population_dimensions(length(value), model_data[["n_years"]], model_data[["ages"]])
      }
      index <- n_earlier_values + seq_along(value)
      n_earlier_values <- n_earlier_values + length(value)
      rows[[length(rows) + 1]] <- tibble::tibble(
        module_name = if (is_fleet) "Fleet" else "Population",
        label = name, fleet = groups[i],
        year_i = dimensions[["year_i"]], age = dimensions[["age"]],
        length = dimensions[["length"]],
        # A value that is not finite (for example the log of a catch of 0) is
        # shown as -999, the code for a missing value.
        estimated = dplyr::if_else(is.finite(value), value, -999),
        uncertainty = if (is.null(standard_errors)) NA_real_ else standard_errors[index],
        estimation_type = "derived_quantity"
      )
    }
  }
  dplyr::bind_rows(rows)
}

# Population quantities in the report, 1 element per population.
population_quantity_names <- c(
  "biomass", "expected_recruitment", "mortality_F", "mortality_M",
  "mortality_Z", "numbers_at_age", "proportion_mature_at_age",
  "spawning_biomass", "sum_selectivity", "total_catch_numbers",
  "total_catch_weight", "unfished_biomass", "unfished_numbers_at_age",
  "unfished_spawning_biomass", "spawning_biomass_ratio", "growth_mean_LAA",
  "growth_sd_LAA"
)

#' 1 row per empirical weight at age
#'
#' With EWAA growth, the weights from the data are listed as constant Growth
#' rows, by year (including the year after the last model year) and age.
#' @noRd
weight_at_age_rows <- function(input) {
  model_data <- input[["model_data"]]
  # The model has 1 population.
  population <- model_data[["populations"]][[1]]
  if (population[["module_types"]][["growth"]] != "EWAA") {
    return(NULL)
  }
  weight_at_age <- population[["growth_inputs"]][["weight_at_age"]]
  dimensions <- population_dimensions(
    length(weight_at_age), model_data[["n_years"]], model_data[["ages"]]
  )
  weights <- flatten_by_year(weight_at_age)
  tibble::tibble(
    module_name = "Growth", module_type = "EWAA", label = "weight_at_age",
    type = "vector", year_i = dimensions[["year_i"]], age = dimensions[["age"]],
    input = weights,
    estimated = weights,
    estimation_type = "constant"
  )
}

#' Year and age of each value of a population quantity, from its length: by
#' year, or by year and age, for the model years or 1 more
#' @noRd
population_dimensions <- function(n_values, n_years, ages) {
  for (years in c(n_years, n_years + 1)) {
    if (n_values == years) {
      return(list(year_i = seq_len(years), age = NA, length = NA))
    }
    if (n_values == years * length(ages)) {
      return(list(
        year_i = rep(seq_len(years), each = length(ages)),
        age = rep(ages, years), length = NA
      ))
    }
  }
  cli::cli_abort(c(
    "x" = "A population quantity has {n_values} values, which match no dimensions.",
    "i" = "Population quantities are by year, or by year and age, for
    {n_years} or {n_years + 1} years."
  ))
}

#' Year, age and length of each value of a fleet quantity
#'
#' Fleet quantities are by year, or by year (rows) and age or length bin
#' (columns), flattened by year. Conditional age-at-length has 1 row per year
#' and length bin with data.
#' @noRd
fleet_dimensions <- function(name, n_values, model_data, fleet) {
  ages <- model_data[["ages"]]
  length_bins <- model_data[["fleets"]][[fleet]][["length_bins"]]
  if (name == "conditional_age_at_length_proportion") {
    rows <- model_data[["fleets"]][[fleet]][["conditional_age_at_length"]]
    return(list(
      year_i = rep(rows[["year"]], each = length(ages)),
      age = rep(ages, length(rows[["year"]])),
      length = rep(length_bins[rows[["length_bin"]]], each = length(ages))
    ))
  }
  if (grepl("length", name)) {
    n_rows <- n_values / length(length_bins)
    return(list(
      year_i = rep(seq_len(n_rows), each = length(length_bins)), age = NA,
      length = rep(length_bins, n_rows)
    ))
  }
  if (n_values %in% c(model_data[["n_years"]], model_data[["n_years"]] + 1)) {
    return(list(year_i = seq_len(n_values), age = NA, length = NA))
  }
  n_rows <- n_values / length(ages)
  list(
    year_i = rep(seq_len(n_rows), each = length(ages)),
    age = rep(ages, n_rows), length = NA
  )
}

# The derived-quantity rows that hold each data set's values: the
# natural-scale expected values, not the quantity the likelihood uses.
estimate_row_for_link <- c(
  log_catch_expected = "catch_expected",
  log_index_expected = "index_expected",
  agecomp_proportion = "agecomp_expected",
  lengthcomp_proportion = "lengthcomp_expected"
)

#' Add observations and likelihood values to the derived-quantity rows
#'
#' `expected` holds the value the likelihood uses (log scale for lognormal
#' data, proportions for compositions). `likelihood` is the log-likelihood of
#' each observation and `lpdf` the total for the data set. Missing
#' observations are shown as -999.
#' @noRd
add_data_rows <- function(estimates, input, report, values) {
  estimates <- dplyr::mutate(
    estimates,
    expected = NA_real_, observed = NA_real_, lpdf = NA_real_,
    likelihood = NA_real_, log_sd = NA_real_, input_type = NA_character_,
    log_like_cv = NA_real_
  )
  observation_lpdf <- report[["observation_lpdf"]]
  n_earlier_observations <- 0
  for (component in input[["model_data"]][["likelihoods"]]) {
    observed <- flatten_by_year(component[["observed"]])
    n_observations <- length(observed)
    log_density <- observation_lpdf[
      n_earlier_observations + seq_len(n_observations)
    ]
    n_earlier_observations <- n_earlier_observations + n_observations
    link <- component[["link"]]
    row_label <- if (link %in% names(estimate_row_for_link)) {
      estimate_row_for_link[[link]]
    } else {
      link
    }
    is_fleet <- estimates[["fleet"]] %in% component[["fleet"]]
    target <- which(estimates[["label"]] == row_label & is_fleet)
    link_rows <- which(estimates[["label"]] == link & is_fleet)
    if (length(target) != n_observations || length(link_rows) != n_observations) {
      cli::cli_abort(c(
        "x" = "There are {length(target)} {.val {row_label}} rows for
        {.val {component[['fleet']]}}, but {n_observations} observations.",
        "i" = "Compare the fleet's data with its {.val {row_label}} report."
      ))
    }
    estimates[["expected"]][target] <- estimates[["estimated"]][link_rows]
    estimates[["observed"]][target] <- dplyr::coalesce(observed, -999)
    estimates[["likelihood"]][target] <- log_density
    estimates[["lpdf"]][target] <- sum(log_density)
    log_sd <- family_parameters(component, values)[["log_sd"]]
    if (!is.null(log_sd)) {
      estimates[["log_sd"]][target] <- rep_len(log_sd, n_observations)
    }
    estimates[["distribution"]][target] <-
      likelihood_families[[component[["family"]]]][["distribution"]]
    estimates[["input_type"]][target] <- "data"
  }
  estimates
}
