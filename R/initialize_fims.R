# initialize_fims(): from the parameter tibble and the data to the inputs of
# RTMB::MakeADFun().

#' Initialize a FIMS model
#'
#' Takes the parameter tibble from setup_default_parameters() and the data,
#' and returns everything fit_fims() needs. The input checks run first. Then
#' the tibble becomes the RTMB parameters, map and random effects
#' (initialize_parameters()), the data are read by population and fleet
#' (initialize_model_data()), and priors and processes become densities
#' (initialize_densities()).
#'
#' @param parameters A tibble of parameters, as from setup_default_parameters()
#'   and add_process().
#' @param data A FIMSFrame, or a data frame that FIMSFrame() accepts.
#' @return A list with `parameters`, `map` and `random` for
#'   RTMB::MakeADFun(), the model data (`model_data`), the parameter table
#'   with the RTMB name of each row (`parameter_table`), the tibble as given
#'   (`tibble`), the FIMSFrame (`frame`), and `model`, which holds the model
#'   settings (model_settings()).
#' @keywords fit_fims
#' @export
initialize_fims <- function(parameters, data) {
  if (missing(parameters) || !tibble::is_tibble(parameters)) {
    cli::cli_abort("The {.var parameters} argument must be a tibble.")
  }
  if (!methods::is(data, "FIMSFrame")) {
    data <- FIMSFrame(data)
  }
  check_initialize_inputs(parameters, data)
  years <- get_start_year(data):get_end_year(data)
  parameters <- add_year_timing(parameters, years)
  check_fleet_parameter_sizes(parameters, length(years))
  model_parameters <- initialize_parameters(parameters)
  parameter_table <- model_parameters[["parameter_table"]]
  model_data <- initialize_model_data(parameters, data)
  model_data[["split_parameters"]] <- model_parameters[["splits"]]
  model_data[["densities"]] <- initialize_densities(parameter_table)
  model_data[["likelihoods"]] <- lapply(
    model_data[["likelihoods"]], link_estimated_family_parameters,
    parameter_names = unique(parameter_table[["name"]])
  )
  # Density and likelihood parameters are read by flat name, not by module.
  model_data[["density_parameter_names"]] <- unique(c(
    unlist(lapply(model_data[["densities"]], function(density) {
      c(density[["hyperparameters"]], density[["mean"]][["name"]])
    })),
    unlist(lapply(model_data[["likelihoods"]], `[[`, "estimated_parameters"))
  ))
  list(
    parameters = model_parameters[["parameters"]],
    map = model_parameters[["map"]],
    random = model_parameters[["random"]],
    model_data = model_data,
    parameter_table = parameter_table,
    tibble = parameters,
    frame = data,
    model = model_settings()
  )
}

#' Model settings, set by methods on `input[["model"]]`
#'
#' Some reporting is switched on with a method, for example
#' `input$model$ReportAgeToLengthConversionDerivedTensor(TRUE)`. The setting
#' is kept in an environment, so calling the method changes it, and
#' fit_fims() reads it.
#' @noRd
model_settings <- function() {
  settings <- new.env()
  settings[["report_age_to_length_conversion_derived_tensor"]] <- FALSE
  settings[["ReportAgeToLengthConversionDerivedTensor"]] <- function(report) {
    settings[["report_age_to_length_conversion_derived_tensor"]] <- isTRUE(report)
    invisible(NULL)
  }
  settings
}

#' Give year-major rows their years
#'
#' A parameter can be given as 1 row per age for every year, with no
#' `timing`, in year-major order (for example time-varying age-specific
#' selectivity).
#' This writes those years into `timing`, so the rest of the code sees an
#' ordinary year-by-age parameter.
#'
#' @inheritParams initialize_fims
#' @param years The model years.
#' @noRd
add_year_timing <- function(parameters, years) {
  key <- paste(parameters[["module_name"]], parameters[["fleet"]], parameters[["label"]])
  for (parameter in unique(key)) {
    row <- which(key == parameter)
    ages <- unique(parameters[["age"]][row])
    if (all(is.na(parameters[["timing"]][row])) && !anyNA(ages) &&
      length(row) == length(years) * length(ages) && length(years) > 1) {
      parameters[["timing"]][row] <- rep(years, each = length(ages))
    }
  }
  parameters
}

#' Check the number of log_q and log_Fmort values of each fleet
#'
#' log_q has 1 value or 1 per year. log_Fmort has 1 per year.
#'
#' @inheritParams initialize_fims
#' @param n_years Number of model years.
#' @noRd
check_fleet_parameter_sizes <- function(parameters, n_years) {
  for (fleet in stats::na.omit(unique(parameters[["fleet"]]))) {
    count <- function(label) {
      sum(parameters[["fleet"]] %in% fleet & parameters[["label"]] == label &
        parameters[["module_name"]] == "Fleet")
    }
    n_log_q <- count("log_q")
    if (!n_log_q %in% c(1, n_years)) {
      cli::cli_abort(c(
        "x" = "Fleet log_q size mismatch. Fleet log_q has {n_log_q} values,
        but it needs 1 value or 1 value per year ({n_years}).",
        "i" = "Fleet {.val {fleet}}."
      ))
    }
    n_log_fmort <- count("log_Fmort")
    if (n_log_fmort != n_years) {
      cli::cli_abort(c(
        "x" = "Fleet log_Fmort size mismatch. Fleet log_Fmort is of size
        {n_log_fmort} and the number of years is {n_years}.",
        "i" = "Fleet {.val {fleet}}."
      ))
    }
  }
  invisible(TRUE)
}

#' Check the parameter tibble and data before the model is built
#'
#' @inheritParams initialize_fims
#' @noRd
check_initialize_inputs <- function(parameters, data) {
  valid_estimation_types <- c("constant", "fixed_effects", "random_effects")
  invalid_estimation_types <- parameters |>
    dplyr::filter(!.data[["estimation_type"]] %in% valid_estimation_types) |>
    dplyr::pull(.data[["estimation_type"]]) |>
    unique() |>
    stats::na.omit()
  if (length(invalid_estimation_types) > 0) {
    cli::cli_abort(c(
      "The `estimation_type` must be one of: {valid_estimation_types}.",
      i = "Invalid values found: {invalid_estimation_types}."
    ))
  }

  # A catch of 0 is left out of the likelihood (initialize_catch()), so an
  # estimated F in that year has nothing in the data to inform it.
  zero_catch <- get_data(data) |>
    dplyr::filter(.data[["type"]] == "catch", .data[["observed"]] == 0) |>
    dplyr::select(dplyr::all_of(c("fleet", "timing")))
  estimated_zero_catch_fmort <- parameters |>
    dplyr::filter(
      .data[["label"]] == "log_Fmort",
      .data[["estimation_type"]] != "constant"
    ) |>
    dplyr::semi_join(zero_catch, by = c("fleet", "timing"))
  if (NROW(estimated_zero_catch_fmort) > 0) {
    zero_catch_labels <- paste(
      estimated_zero_catch_fmort[["fleet"]],
      estimated_zero_catch_fmort[["timing"]]
    )
    cli::cli_warn(c(
      "!" = "{.var log_Fmort} is estimated for the following {.var fleet} and
      {.var timing} combinations with a catch of 0: {.val {zero_catch_labels}}.",
      "i" = "A catch of 0 is left out of the likelihood, so nothing in the
      data informs fishing mortality in those years.
      {.fn setup_default_parameters} fixes {.var log_Fmort} at -200 for them.",
      "i" = "Use {.code dplyr::filter(parameters, label == 'log_Fmort',
      estimation_type != 'constant')} to find the rows."
    ))
  }

  fleets <- stats::na.omit(unique(parameters[["fleet"]]))
  if (length(fleets) == 0) {
    cli::cli_abort(c(
      "x" = "No fleets found in the provided {.var parameters}.",
      "i" = "The {.var fleet} column is empty or all {.code NA}.",
      "i" = "Create {.var parameters} with {.fn setup_default_parameters},
      which fills in {.var fleet} for each fleet."
    ))
  }

  check_recruitment_process(parameters)

  growth <- dplyr::filter(parameters, .data[["module_name"]] == "Growth")
  if ("VonBertalanffySchnute" %in% growth[["module_type"]]) {
    validate_von_bertalanffy_schnute_growth_inputs(growth)
  }
  invisible(TRUE)
}

#' Check the von Bertalanffy-Schnute growth rows
#'
#' Both reference ages must be given, and length variability must come from
#' exactly 1 of the 2 options: the interpolation SDs or the full delta-method
#' block. L1, L2, k and the length-weight parameters must be positive, and
#' the old reference age and L2 must be above the young ones.
#'
#' @param growth The Growth rows of the parameter tibble.
#' @noRd
validate_von_bertalanffy_schnute_growth_inputs <- function(growth) {
  value <- function(label) growth[["value"]][growth[["label"]] == label]
  missing_reference_labels <- setdiff(
    c("reference_age_for_length_young", "reference_age_for_length_old"),
    growth[["label"]]
  )
  if (length(missing_reference_labels) > 0) {
    cli::cli_abort(c(
      "VonBertalanffySchnute growth requires reference-age inputs.",
      "i" = "Missing labels: {toString(missing_reference_labels)}",
      "i" = "Use {.fn setup_default_Growth} or supply both reference ages explicitly."
    ))
  }
  delta_method_labels <- c(
    "log_sd_mean_length_young", "log_sd_mean_length_old",
    "log_sd_growth_coefficient",
    "mean_length_young_mean_length_old_logit_corr",
    "mean_length_young_growth_coefficient_logit_corr",
    "mean_length_old_growth_coefficient_logit_corr"
  )
  interpolation_labels <- c("length_at_age_sd_young", "length_at_age_sd_old")
  sd_rows <- dplyr::filter(growth, .data[["label"]] %in% interpolation_labels)
  has_interpolation <- nrow(sd_rows) > 0
  present_delta <- intersect(delta_method_labels, growth[["label"]])
  if (length(present_delta) > 0 && length(present_delta) < length(delta_method_labels)) {
    cli::cli_abort(c(
      "VonBertalanffy delta-method variability inputs must be supplied as a complete set.",
      "i" = "Provide all of: {toString(delta_method_labels)}"
    ))
  }
  if (has_interpolation && length(present_delta) == length(delta_method_labels)) {
    cli::cli_abort(c(
      "VonBertalanffy growth requires exactly one variability path.",
      "i" = "The default setup uses interpolation via {.var length_at_age_sd_young} and {.var length_at_age_sd_old}.",
      "i" = "If you use the delta-method path, supply the full delta block and omit interpolation rows."
    ))
  }
  if (!has_interpolation && length(present_delta) == 0) {
    cli::cli_abort(c(
      "VonBertalanffy growth requires exactly one variability path.",
      "i" = "Supply both {.var length_at_age_sd_young} and {.var length_at_age_sd_old}, or the full delta-method block."
    ))
  }
  if (has_interpolation &&
    (nrow(sd_rows) != 2 || !setequal(sd_rows[["label"]], interpolation_labels) ||
      any(!is.na(sd_rows[["age"]])))) {
    cli::cli_abort(c(
      "VonBertalanffySchnute interpolation-based variability inputs are malformed.",
      "i" = "Supply exactly one {.var length_at_age_sd_young} row and one {.var length_at_age_sd_old} row.",
      "i" = "Set their {.var age} entries to NA; their ages come from the Growth reference-age parameters."
    ))
  }
  if (value("reference_age_for_length_old") <= value("reference_age_for_length_young")) {
    cli::cli_abort(
      "VonBertalanffySchnuteGrowth reference_age_for_length_old must be > reference_age_for_length_young"
    )
  }
  for (label in c(
    "mean_length_young", "mean_length_old", "growth_coefficient",
    "length_weight_a", "length_weight_b"
  )) {
    if (!all(is.finite(value(label))) || any(value(label) <= 0)) {
      cli::cli_abort(c(
        "x" = "{label} must be > 0",
        "i" = "Its value is {.val {value(label)}}."
      ))
    }
  }
  if (value("mean_length_old") <= value("mean_length_young")) {
    cli::cli_abort(
      "VonBertalanffySchnuteGrowth mean_length_old must be > mean_length_young"
    )
  }
  invisible(TRUE)
}

#' Check that the recruitment process and its parameters agree
#'
#' @inheritParams initialize_fims
#' @noRd
check_recruitment_process <- function(parameters) {
  process_rows <- dplyr::filter(
    parameters,
    .data[["module_name"]] == "Recruitment",
    .data[["distribution_type"]] %in% "process",
    !is.na(.data[["distribution"]])
  )
  if (nrow(process_rows) == 0) {
    process_parameter <- dplyr::filter(
      parameters,
      .data[["module_name"]] == "Recruitment",
      .data[["label"]] %in% c("log_devs", "log_r")
    )
    process_parameter_name <- unique(process_parameter[["label"]])
    if (any(process_parameter[["estimation_type"]] %in% c("fixed_effects", "random_effects"))) {
      cli::cli_abort(c(
        x = "Missing required inputs for recruitment process random or
        fixed effects.",
        i = "There is no distribution process specified for the
        {.var {process_parameter_name}} variable in the recruitment module.",
        i = "Implement either one of the following options to resolve this
        error:",
        i = "1. Set a distribution and distribution_type for the Recruitment
        {.var module_name} in configurations tibble.",
        i = "2. Set the estimation_type for the recruitment
        {.var {process_parameter_name}} variable in the parameter tibble to
        {.var constant}."
      ))
    }
    return(invisible(TRUE))
  }
  process_label <- unique(setdiff(process_rows[["label"]], "log_sd"))
  if (length(process_label) == 0) {
    cli::cli_abort(c(
      x = "Missing required inputs for recruitment process random or
      fixed effects.",
      i = "There is a distribution specified for the Recruitment
      {.var module_name} in the configurations tibble, but no parameters are
      specified for the recruitment process in the parameters tibble.",
      i = "Implement either one of the following options to resolve this
      error:",
      i = "1. Add parameter, {.var log_devs} or {.var log_r}, for the
      recruitment process in the parameters tibble with an estimation_type of
      random_effects or fixed_effects.",
      i = "2. Set the distribution for the Recruitment distribution and
      distribution_type to {.var NA} in the configurations tibble."
    ))
  }
  if (any(process_rows[["estimation_type"]][process_rows[["label"]] != "log_sd"] %in% "constant")) {
    cli::cli_abort(c(
      x = "Missing required inputs for recruitment process random or
      fixed effects.",
      i = "The estimation type for {.var {process_label}} is constant, but
      there is a distribution specified for the Recruitment
      {.var module_name} in the configurations tibble.",
      i = "Implement either one of the following options to resolve this
      error:",
      i = "1. Set the distribution for the Recruitment distribution and
      distribution_type to {.var NA} in the configurations tibble.",
      i = "2. Set the estimation_type for the recruitment
      {.var {process_label}} in the parameter tibble to
      {.var random_effects} or {.var fixed_effects}."
    ))
  }
  # The process density covers every value of the parameter, including
  # values set to constant (with no distribution).
  process_estimation_types <- parameters[["estimation_type"]][
    parameters[["module_name"]] %in% "Recruitment" &
      parameters[["label"]] %in% process_label
  ]
  n_constant <- sum(
    process_estimation_types == "constant" | is.na(process_estimation_types)
  )
  if (n_constant > 0) {
    cli::cli_warn(c(
      "!" = "{n_constant} of the {length(process_estimation_types)}
      {.var {process_label}} values in the recruitment module are constant,
      but the recruitment process distribution includes all of them.",
      i = "The constant values still add to the negative log-likelihood and
      affect the estimate of {.var log_sd} (biased low when they are fixed at
      0) and, for {.var log_r}, the stock-recruit parameters."
    ))
  }
  invisible(TRUE)
}
