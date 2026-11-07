# Model data: what the model reads besides parameters
#
# The data are read with the FIMSFrame helpers (model_catch(),
# model_age_comp(), resolve_*(), parse_data_distribution()), so the rules for
# missing values, units and length bins are kept in 1 place. They are stored
# by population and by fleet:
# - `populations`: module types and growth inputs. The model has 1 population.
# - `fleets`: per fleet, its selectivity type, data units, observed
#   compositions, length bins, age-to-length key or rebin weights, and ageing
#   error.
# - `likelihoods`: 1 entry per data set, in the order of the data components
#   of nll_components.

#' Read the data and module types the model needs
#'
#' @param parameters The parameter tibble.
#' @param data A FIMSFrame.
#' @return The model data list (the `model_data` element of initialize_fims()).
#' @noRd
initialize_model_data <- function(parameters, data) {
  fleet_names <- parameters |>
    dplyr::pull(.data[["fleet"]]) |>
    unique() |>
    stats::na.omit() |>
    as.character()
  growth_type <- module_type(parameters, "Growth")
  if (is.null(growth_modules[[growth_type]])) {
    cli::cli_abort(c(
      "x" = "There is no growth module {.val {growth_type}}.",
      "i" = "The growth modules are {.val {names(growth_modules)}}.",
      "i" = "Check the module types with {.code dplyr::distinct(parameters,
      module_name, module_type)}."
    ))
  }
  # When growth models length, fleets' age-to-length keys come from growth,
  # otherwise from the fixed key in the data.
  growth_models_length <- growth_modules[[growth_type]][["models_length"]]
  length_bins <- resolve_fleet_length_bins(
    get_data(data),
    allow_global_conversion_fallback = !growth_models_length
  )
  # Only fleets with length bins set the range of the population length grid.
  fleets_with_bins <- Filter(function(bins) length(bins) > 0, length_bins)
  fleet_edges <- Map(
    edges_from_lower_edges, fleets_with_bins, names(fleets_with_bins)
  )
  population_edges <- if (growth_models_length) {
    default_population_edges(fleet_edges)
  }
  recruitment_labels <- parameters |>
    dplyr::filter(.data[["module_name"]] == "Recruitment") |>
    dplyr::pull(.data[["label"]])

  populations <- list(population = list(
    module_types = list(
      maturity = module_type(parameters, "Maturity"),
      growth = growth_type,
      recruitment = module_type(parameters, "Recruitment"),
      recruitment_process = recruitment_process_type(recruitment_labels)
    ),
    growth_inputs = growth_modules[[growth_type]][["inputs"]](
      data, parameters, population_edges
    ),
    fleets = fleet_names
  ))
  fleets <- lapply(stats::setNames(fleet_names, fleet_names), function(fleet) {
    edges <- fleet_edges[[fleet]]
    initialize_fleet(
      data, parameters, fleet, length_bins[[fleet]], edges,
      rebin = if (growth_models_length && !is.null(edges)) {
        rebin_weights(population_edges, edges)
      }
    )
  })
  likelihoods <- do.call(c, lapply(fleet_names, function(fleet) {
    initialize_likelihoods(data, fleet, fleets[[fleet]])
  }))

  list(
    model_type = "CatchAtAge",
    n_years = get_n_years(data),
    start_year = get_start_year(data),
    ages = get_ages(data),
    n_ages = get_n_ages(data),
    populations = populations,
    fleets = fleets,
    likelihoods = likelihoods,
    adreport = FALSE
  )
}

#' The module type of a module family (and fleet) in the parameter tibble
#'
#' @param parameters The parameter tibble.
#' @param module_name The module family, e.g., "Selectivity".
#' @param fleet The fleet, or NA for population modules.
#' @noRd
module_type <- function(parameters, module_name, fleet = NA) {
  rows <- dplyr::filter(parameters, .data[["module_name"]] == .env[["module_name"]])
  if (!is.na(fleet)) {
    rows <- dplyr::filter(rows, .data[["fleet"]] %in% .env[["fleet"]])
  }
  unique(stats::na.omit(rows[["module_type"]]))
}

#' The recruitment process: log_devs or log_r
#'
#' When the tibble has both, the model uses log_devs and says so.
#'
#' @param recruitment_labels The labels of the Recruitment rows.
#' @noRd
recruitment_process_type <- function(recruitment_labels) {
  process <- intersect(names(recruitment_process_functions), recruitment_labels)
  if (all(c("log_devs", "log_r") %in% process)) {
    cli::cli_warn(c(
      "!" = "Both {.val log_devs} and {.val log_r} are in the Recruitment
      rows, but there can be only 1 recruitment process. The model uses
      {.val log_devs}.",
      "i" = "When fitting an AR1 to recruitment, use {.val log_r} for
      improved performance.",
      "i" = "Remove the unused rows. Find them with
      {.code dplyr::filter(parameters, label %in% c(\"log_devs\",
      \"log_r\"))}."
    ))
    process <- "log_devs"
  }
  process
}

# Fleets ------------------------------------------------------------------------

#' 1 fleet's data and settings
#'
#' @param data A FIMSFrame.
#' @param parameters The parameter tibble.
#' @param fleet The fleet name.
#' @param length_bins The fleet's length bins (lower edges), or NULL.
#' @param edges The fleet's length bin edges from edges_from_lower_edges(), or
#'   NULL when it has no length bins.
#' @param rebin Rebin weights from the population length grid to the fleet's
#'   bins when its key comes from growth, otherwise NULL.
#' @return A list of the fleet's module types, units, observations and key.
#' @noRd
initialize_fleet <- function(data, parameters, fleet, length_bins, edges,
                             rebin) {
  n_years <- get_n_years(data)
  n_ages <- get_n_ages(data)
  fleet_rows <- dplyr::filter(get_data(data), .data[["fleet"]] %in% .env[["fleet"]])
  data_types <- unique(fleet_rows[["type"]])
  fleet_data <- list(
    module_types = list(
      selectivity = module_type(parameters, "Selectivity", fleet)
    ),
    has_catch = "catch" %in% data_types,
    observed_catch_units = fleet_units(fleet_rows, "catch"),
    observed_index_units = fleet_units(fleet_rows, "index"),
    length_bins = length_bins
  )
  if (!is.null(edges)) {
    fleet_data[["length_bin_midpoints"]] <- (edges[-1] + edges[-length(edges)]) / 2
  }
  if ("weight_at_age" %in% data_types) {
    weight_at_age <- model_weight_at_age(data, fleet = fleet)
    check_weight_at_age_dimensions(
      weight_at_age, c(n_years, n_years + 1), n_ages, fleet
    )
    # Fleet predictions are for the model years only, so a row for the year
    # after the last model year is not used.
    fleet_data[["weight_at_age"]] <- matrix(
      weight_at_age,
      ncol = n_ages,
      byrow = TRUE
    )[seq_len(n_years), ]
  }
  if ("age_comp" %in% data_types) {
    age_comp <- model_age_comp(data, fleet)
    check_comp_dimensions(
      age_comp, n_years, n_ages, "age_comp", fleet,
      dplyr::filter(fleet_rows, .data[["type"]] == "age_comp")
    )
    fleet_data[["age_comp"]] <- matrix(
      missing_to_na(age_comp), n_years, n_ages,
      byrow = TRUE
    )
  }
  # Age compositions and conditional age-at-length give read ages, so these
  # fleets use ageing error.
  if (any(c("age_comp", "conditional_age_at_length") %in% data_types)) {
    fleet_data[["ageing_error"]] <- ageing_error_matrices(data, fleet)
  }
  if ("length_comp" %in% data_types) {
    fleet_data[["length_comp"]] <- matrix(
      missing_to_na(read_length_comp(data, fleet, length_bins)), n_years,
      length(length_bins),
      byrow = TRUE
    )
  }
  if ("conditional_age_at_length" %in% data_types) {
    if (length(length_bins) == 0) {
      cli::cli_abort(c(
        "x" = "Fleet {.val {fleet}} has conditional age-at-length data but no
        length bins.",
        "i" = "Conditional age-at-length needs length bins, from an
        age-to-length conversion or a growth module that models length.",
        "i" = "Check the data types of each fleet with
        {.code dplyr::count(get_data(data), fleet, type)}."
      ))
    }
    fleet_data[["conditional_age_at_length"]] <- read_conditional_age_at_length(
      fleet_rows, get_ages(data), length_bins, get_start_year(data)
    )
  }
  # A fleet with length compositions or length_bin rows needs an
  # age-to-length key, and so does conditional age-at-length. The key comes
  # from growth when growth models length, otherwise from the fixed key in
  # the data.
  has_length_bins <- any(c("length_comp", "length_bin") %in% data_types)
  has_fixed_key <- any(get_data(data)[["type"]] == "age_to_length_conversion")
  needs_key <- has_length_bins || "conditional_age_at_length" %in% data_types
  if (needs_key && !is.null(rebin)) {
    fleet_data[["rebin_weights"]] <- rebin
  } else if (needs_key && has_fixed_key) {
    fleet_data[["age_to_length_conversion"]] <- age_to_length_conversion_fixed(
      data, fleet, length_bins
    )
  } else if (needs_key) {
    cli::cli_abort(c(
      "x" = "Fleet {.val {fleet}} has length data but no age-to-length
      conversion.",
      "i" = "Length data need an age-to-length key. It comes from
      {.val age_to_length_conversion} rows in the data, or from a growth
      module that models length, such as {.val VonBertalanffySchnute}.",
      "i" = "Check the data types of each fleet with
      {.code dplyr::count(get_data(data), fleet, type)}."
    ))
  }
  fleet_data
}

#' 1 likelihood entry per data set of a fleet
#'
#' Data sets are in the order index, catch, age composition, length
#' composition, then conditional age-at-length. The data components of
#' nll_components follow this order. Each entry has the observations, the
#' likelihood family
#' and its parameters from the data's uncertainty column, and the name of the
#' predicted quantity it is compared with (`link`).
#'
#' @param data A FIMSFrame.
#' @param fleet The fleet name.
#' @param fleet_data The fleet's entry from initialize_fleet().
#' @noRd
initialize_likelihoods <- function(data, fleet, fleet_data) {
  fleet_rows <- dplyr::filter(get_data(data), .data[["fleet"]] %in% .env[["fleet"]])
  data_types <- intersect(
    c("index", "catch", "age_comp", "length_comp", "conditional_age_at_length"),
    unique(fleet_rows[["type"]])
  )
  lapply(data_types, function(data_type) {
    observed <- switch(data_type,
      index = missing_to_na(model_index(data, fleet)),
      catch = initialize_catch(data, fleet),
      age_comp = fleet_data[["age_comp"]],
      length_comp = fleet_data[["length_comp"]],
      conditional_age_at_length =
        fleet_data[["conditional_age_at_length"]][["observed"]]
    )
    uncertainty <- fleet_rows |>
      dplyr::filter(.data[["type"]] == data_type) |>
      dplyr::pull(.data[["uncertainty"]]) |>
      parse_data_distribution()
    likelihood_family <- unique(stats::na.omit(uncertainty[["family"]]))
    if (is.null(likelihood_families[[likelihood_family]])) {
      cli::cli_abort(c(
        "x" = "There is no likelihood for family {.val {likelihood_family}}
        ({.val {fleet}} {data_type}).",
        "i" = "The families are {.val {names(likelihood_families)}}.",
        "i" = "Check the {.var uncertainty} column with
        {.code dplyr::distinct(get_data(data), fleet, type, uncertainty)}."
      ))
    }
    list(
      # The data set's name, used to match simulated values to their rows.
      name = make.names(paste(fleet, data_type, sep = "_")),
      fleet = fleet,
      data_type = data_type,
      family = likelihood_family,
      link = unique(stats::na.omit(uncertainty[["link"]])),
      observed = observed,
      # The family's own parameters, by the names its log density uses.
      parameters = likelihood_families[[likelihood_family]][["parameters"]](uncertainty)
    )
  })
}

# Readers -----------------------------------------------------------------------

#' Observed catch for a fleet, NA where missing
#'
#' A catch of 0 is left out of the likelihood, because the lognormal is not
#' defined at 0. setup_default_parameters() fixes F near 0 in that year.
#' @noRd
initialize_catch <- function(data, fleet) {
  catch <- missing_to_na(model_catch(data, fleet))
  catch[catch %in% 0] <- NA
  catch
}

#' Read 1 fleet's length compositions in its length bins
#'
#' Rows outside the fleet's bins are dropped with a warning, and the rest are
#' ordered by year and bin. Proportions are turned into counts with the
#' multinomial `size` in the uncertainty column.
#'
#' @param data A FIMSFrame.
#' @param fleet The fleet name.
#' @param length_bins The fleet's length bins (lower edges).
#' @return The counts by year, then length bin, with -999 where missing.
#' @noRd
read_length_comp <- function(data, fleet, length_bins) {
  rows <- dplyr::filter(
    get_data(data),
    .data[["fleet"]] == .env[["fleet"]],
    .data[["type"]] == "length_comp"
  )
  out_of_bin <- dplyr::filter(
    rows,
    !is.na(.data[["length"]]),
    !(.data[["length"]] %in% length_bins),
    !is.na(.data[["observed"]]),
    .data[["observed"]] != -999
  )
  if (nrow(out_of_bin) > 0) {
    out_of_bin_lengths <- sort(unique(out_of_bin[["length"]]))
    out_of_bin_timings <- sort(unique(out_of_bin[["timing"]]))
    cli::cli_warn(c(
      "!" = "Fleet {.val {fleet}} has {.val {nrow(out_of_bin)}}
      {.val length_comp} row{?s} outside its resolved length bins, at lengths
      {.val {out_of_bin_lengths}} in years {.val {out_of_bin_timings}}.",
      "i" = "Only rows in the fleet's length bins are used.",
      "i" = "Find the rows with {.code dplyr::filter(get_data(data),
      type == \"length_comp\", fleet == \"{fleet}\")}."
    ))
  }
  rows <- rows |>
    dplyr::filter(.data[["length"]] %in% length_bins) |>
    dplyr::arrange(.data[["timing"]], match(.data[["length"]], length_bins))
  counts <- rows |>
    dplyr::mutate(
      sample_size = ifelse(
        .data[["observed"]] == -999, 1,
        unlist(parse_data_distribution(.data[["uncertainty"]])[["size"]])
      ),
      count = ifelse(
        .data[["unit"]] == "number", .data[["observed"]],
        .data[["observed"]] * .data[["sample_size"]]
      )
    ) |>
    dplyr::pull(.data[["count"]])
  check_comp_dimensions(
    counts, get_n_years(data), length(length_bins), "length_comp", fleet,
    rows
  )
  counts
}

#' Stop when compositions do not have 1 value per year and age or bin
#'
#' Without this check, R would recycle the values to fill the years-by-ages
#' matrix, moving them to the wrong years and ages.
#'
#' @param values The composition values, by year then age or bin.
#' @param n_years,n_columns The number of model years, and of ages or bins.
#' @param data_type "age_comp" or "length_comp".
#' @param fleet The fleet name.
#' @param rows The data rows the values came from, to name the years with
#'   too few or too many.
#' @noRd
check_comp_dimensions <- function(values, n_years, n_columns, data_type,
                                  fleet, rows) {
  expected_length <- n_years * n_columns
  if (length(values) == expected_length) {
    return(invisible(values))
  }
  bad_data_years <- rows |>
    dplyr::count(.data[["timing"]]) |>
    dplyr::filter(.data[["n"]] != n_columns) |>
    dplyr::pull(.data[["timing"]])
  cli::cli_abort(c(
    "x" = "The length of the {.val {data_type}} data for fleet
    {.val {fleet}} does not match the expected dimensions:
    {.val {length(values)}} values, {.val {sum(values == -999)}} of them
    -999, in years {.val {bad_data_years}}.",
    "i" = "The data need 1 value per year and age or length bin
    ({.val {expected_length}}). Use -999 for a missing value.",
    "i" = "Count the rows in each year with
    {.code dplyr::count(get_data(data), fleet, type, timing)}."
  ))
}

#' Stop when weight at age does not have 1 value per year and age
#'
#' Without this check, R would recycle the values to fill the years-by-ages
#' matrix.
#'
#' @param values Weight at age, by year then age.
#' @param n_years The numbers of years allowed: the model years, with or
#'   without 1 more, for a fleet, and the model years and 1 more for the
#'   population.
#' @param n_ages The number of ages.
#' @param fleet The fleet name, or NA for the population.
#' @noRd
check_weight_at_age_dimensions <- function(values, n_years, n_ages,
                                           fleet = NA) {
  if (length(values) %in% (n_years * n_ages)) {
    return(invisible(values))
  }
  owner <- if (is.na(fleet)) {
    "the population"
  } else {
    cli::format_inline("fleet {.val {fleet}}")
  }
  cli::cli_abort(c(
    "x" = "Weight at age for {owner} has {.val {length(values)}} values,
    which is not 1 per year and age.",
    "i" = "Give 1 weight for each of the {.val {n_ages}} ages in each year.",
    "i" = "Count the weight-at-age rows in each year with
    {.code dplyr::count(get_data(data), type, fleet, timing)}."
  ))
}

#' Read 1 fleet's conditional age-at-length rows
#'
#' Each (year, length bin) with data becomes 1 row of an observed matrix
#' with 1 column per age. Values given as proportions are turned into counts
#' with the multinomial `size` in the uncertainty column, as for age and
#' length compositions. Missing values (-999) become NA.
#'
#' @return A list with `observed` (rows by ages) and the `year` and
#'   `length_bin` index of each row.
#' @noRd
read_conditional_age_at_length <- function(fleet_rows, ages, length_bins,
                                           start_year) {
  rows <- dplyr::filter(
    fleet_rows, .data[["type"]] == "conditional_age_at_length"
  )
  if (!all(rows[["length"]] %in% length_bins)) {
    cli::cli_abort(c(
      "x" = "Conditional age-at-length lengths
      {.val {setdiff(rows[['length']], length_bins)}} are not the fleet's
      length bins.",
      "i" = "Use the fleet's length bins: {.val {length_bins}}.",
      "i" = "Find the rows with {.code dplyr::filter(get_data(data),
      type == \"conditional_age_at_length\")}."
    ))
  }
  size <- unlist(parse_data_distribution(rows[["uncertainty"]])[["size"]])
  rows[["count"]] <- dplyr::case_when(
    rows[["observed"]] == -999 ~ NA_real_,
    rows[["unit"]] == "proportion" ~ rows[["observed"]] * size,
    .default = rows[["observed"]]
  )
  groups <- dplyr::distinct(rows, .data[["timing"]], .data[["length"]]) |>
    dplyr::arrange(.data[["timing"]], .data[["length"]])
  observed <- matrix(0, nrow(groups), length(ages))
  row_index <- match(
    paste(rows[["timing"]], rows[["length"]]),
    paste(groups[["timing"]], groups[["length"]])
  )
  observed[cbind(row_index, match(rows[["age"]], ages))] <- rows[["count"]]
  list(
    observed = observed,
    year = groups[["timing"]] - start_year + 1,
    length_bin = match(groups[["length"]], length_bins)
  )
}

#' FIMSFrame units "mt" and "number" become the model's "weight" and "number"
#' @noRd
fleet_units <- function(fleet_rows, data_type) {
  unit <- unique(fleet_rows[["unit"]][fleet_rows[["type"]] == data_type])
  if (length(unit) == 0) {
    return("weight")
  }
  model_unit <- c(mt = "weight", number = "number")[unit]
  # A unit with no rule here would otherwise be fit as the wrong quantity.
  if (anyNA(model_unit)) {
    cli::cli_abort(c(
      "x" = "Fleet {.val {unique(fleet_rows[['fleet']])}} has the catch or
      index unit {.val {unit[is.na(model_unit)]}}, which the model cannot
      use.",
      "i" = "Catch and index units must be {.val mt} (weight) or
      {.val number}.",
      "i" = "Check the units with
      {.code dplyr::count(get_data(data), fleet, type, unit)}."
    ))
  }
  unname(model_unit)
}

#' Missing data are marked with -999
#' @noRd
missing_to_na <- function(x) {
  x[x == -999] <- NA
  x
}

#' Fixed age-to-length key for 1 fleet, ages by length bins
#'
#' Read with resolve_age_to_length_conversion(): 1 matrix if the key has no
#' timing, otherwise 1 per model year.
#' @noRd
age_to_length_conversion_fixed <- function(data, fleet, length_bins) {
  ages <- get_ages(data)
  years <- get_start_year(data):get_end_year(data)
  key <- get_data(data) |>
    resolve_age_to_length_conversion(
      years = years, fleet = fleet, lengths = length_bins
    ) |>
    dplyr::mutate(
      age_order = match(.data[["age"]], ages),
      length_order = match(.data[["length"]], length_bins)
    ) |>
    dplyr::arrange(.data[["timing"]], .data[["age_order"]], .data[["length_order"]])
  one_matrix <- function(values) {
    matrix(values, length(ages), length(length_bins), byrow = TRUE)
  }
  if (all(is.na(key[["timing"]]))) {
    return(one_matrix(key[["value"]]))
  }
  lapply(years, function(year) one_matrix(key[["value"]][key[["timing"]] == year]))
}

#' A fleet's ageing-error matrices, true age (rows) by read age (columns)
#'
#' Read with resolve_ageing_error(): 1 matrix if the same in every year,
#' otherwise 1 per model year. NULL if the fleet has no ageing error.
#' @noRd
ageing_error_matrices <- function(data, fleet) {
  ages <- get_ages(data)
  years <- get_start_year(data):get_end_year(data)
  rows <- resolve_ageing_error(
    get_data(data),
    fleets = fleet, ages = ages, years = years
  )
  if (NROW(rows) == 0) {
    return(NULL)
  }
  # Rows are sorted by timing, true age, then read age.
  one_matrix <- function(values) {
    matrix(values, length(ages), length(ages), byrow = TRUE)
  }
  if (all(is.na(rows[["timing"]]))) {
    return(one_matrix(rows[["observed"]]))
  }
  lapply(years, function(year) one_matrix(rows[["observed"]][rows[["timing"]] == year]))
}
