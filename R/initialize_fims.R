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
#' @param parameters A tibble of parameters, as from setup_default_parameters(),
#'   add_process() and add_priors().
#' @param data A FIMSFrame, or a data frame that FIMSFrame() accepts.
#' @return A list with `parameters`, `map` and `random` for
#'   RTMB::MakeADFun(), the model data (`model_data`), the parameter table
#'   with the RTMB name of each row (`parameter_table`), the tibble as given
#'   (`tibble`), and the FIMSFrame (`frame`).
#' @keywords fit_fims
#' @export
#' @seealso
#' * [setup_default_parameters()]
#' * [FIMSFrame()]
#' * [fit_fims()]
#' @examples
#' \dontrun{
#' # Prepare data for FIMS model
#' data("data_big", package = "FIMS")
#' data_4_model <- FIMSFrame(data_big)
#' # Build the model inputs from the default parameters
#' parameters_list <- setup_default_parameters(data = data_4_model) |>
#'   initialize_fims(data = data_4_model)
#' }
initialize_fims <- function(parameters, data) {
  if (missing(parameters) || !tibble::is_tibble(parameters)) {
    cli::cli_abort(c(
      "x" = "The {.var parameters} argument must be a tibble.",
      "i" = "{.fn setup_default_parameters} returns the parameter tibble.",
      "i" = "Check what was given with {.code class(parameters)}."
    ))
  }
  if (!methods::is(data, "FIMSFrame")) {
    data <- FIMSFrame(data)
  }
  check_initialize_inputs(parameters, data)
  years <- get_start_year(data):get_end_year(data)
  parameters <- add_year_timing(parameters, years)
  # A surplus production model has no log_q or log_Fmort.
  if (parameters_model_type(parameters) == "CatchAtAge") {
    check_fleet_parameter_sizes(parameters, length(years))
  }
  check_parameter_years(parameters, years)
  model_parameters <- initialize_parameters(parameters)
  parameter_table <- model_parameters[["parameter_table"]]
  model_data <- initialize_model_data(parameters, data)
  model_data[["split_parameters"]] <- model_parameters[["splits"]]
  model_data[["densities"]] <- initialize_densities(parameter_table)
  check_random_effect_densities(
    model_parameters[["random"]], model_data[["densities"]]
  )
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
  # A parameter is used if a density reads it, or if a prior scores it.
  model_data[["used_parameter_names"]] <- unique(c(
    model_data[["density_parameter_names"]],
    unlist(lapply(model_data[["densities"]], function(density) {
      if (density[["distribution_type"]] == "prior") density[["target"]]
    }))
  ))
  # Rows with a target_label belong to a prior or process, not a module. This
  # includes a process removed from its target, whose rows
  # hold_unused_process_parameters() holds constant.
  model_data[["not_module_parameter_names"]] <- unique(c(
    model_data[["density_parameter_names"]],
    parameter_table[["name"]][!is.na(parameter_table[["target_label"]])]
  ))
  held <- hold_unused_process_parameters(
    model_parameters[["map"]], parameter_table,
    model_data[["used_parameter_names"]]
  )
  model_parameters[["map"]] <- held[["map"]]
  parameter_table <- held[["parameter_table"]]
  list(
    parameters = model_parameters[["parameters"]],
    map = model_parameters[["map"]],
    random = model_parameters[["random"]],
    model_data = model_data,
    parameter_table = parameter_table,
    tibble = parameters,
    frame = data
  )
}

#' Hold a process's own parameters constant when no process uses them
#'
#' A process mean, log_sd or logit_rho row whose target has no process, such
#' as the recruitment log_sd in a model without a recruitment process, does
#' not affect the model. Estimating it would leave a gradient of 0 and a
#' singular Hessian, so it is held at its starting value, with a message,
#' and is not listed in get_estimates().
#'
#' @param map The map from initialize_parameters().
#' @param parameter_table The parameter table from initialize_parameters().
#' @param used_names The flat names of the parameters a density uses.
#' @return A list with the updated `map` and `parameter_table`.
#' @noRd
hold_unused_process_parameters <- function(map, parameter_table, used_names) {
  is_unused <- parameter_table[["original_label"]] %in%
    c("expected_mean", "log_sd", "logit_rho") &
    !parameter_table[["distribution_type"]] %in% "prior" &
    !parameter_table[["name"]] %in% used_names &
    parameter_table[["estimation_type"]] != "constant"
  if (!any(is_unused)) {
    return(list(map = map, parameter_table = parameter_table))
  }
  unused_names <- unique(parameter_table[["name"]][is_unused])
  cli::cli_inform(c(
    "i" = "{.val {unused_names}} {?is/are} not used by any process, so
    {?it is/they are} held at {?its/their} starting value{?s}.",
    "i" = "Add the process back with {.fn add_process}, or set
    {.var estimation_type} to {.val constant}."
  ))
  for (rtmb_name in unique(parameter_table[["rtmb_name"]][is_unused])) {
    map[[rtmb_name]] <- factor(rep(NA, length(map[[rtmb_name]])))
  }
  parameter_table[["estimation_type"]][is_unused] <- "constant"
  parameter_table[["rtmb_level"]][is_unused] <- NA_integer_
  list(map = map, parameter_table = parameter_table)
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
        "x" = "Fleet {.val {fleet}} has a log_q size mismatch:
        {.val {n_log_q}} log_q value{?s}.",
        "i" = "log_q needs 1 value, or 1 value per year ({.val {n_years}}).",
        "i" = "Count the rows with {.code dplyr::count(parameters, module_name,
        fleet, label)}."
      ))
    }
    n_log_fmort <- count("log_Fmort")
    if (n_log_fmort != n_years) {
      cli::cli_abort(c(
        "x" = "Fleet {.val {fleet}} has a log_Fmort size mismatch:
        {.val {n_log_fmort}} log_Fmort value{?s}.",
        "i" = "log_Fmort needs 1 value per year ({.val {n_years}}).",
        "i" = "Count the rows with {.code dplyr::count(parameters, module_name,
        fleet, label)}."
      ))
    }
  }
  invisible(TRUE)
}

#' Check that a parameter given by year has a row for each model year
#'
#' The model reads a parameter's values by year in order, so a missing year
#' would move every later value to the year before. A parameter with 1 row
#' has the same value in every year. Recruitment deviations
#' (log_devs or log_r) start in the 2nd model year, because recruitment in
#' the first year comes from the initial numbers at age. log_depletion also
#' starts in the 2nd model year, because depletion in the first year is the
#' initial depletion. They need their years even when no other parameter is
#' given by year.
#'
#' @inheritParams initialize_fims
#' @param years The model years.
#' @noRd
check_parameter_years <- function(parameters, years) {
  parameters <- add_process_columns(parameters)
  parameters[["name"]] <- dplyr::if_else(
    is.na(parameters[["fleet"]]),
    paste(parameters[["module_name"]], flat_label(parameters), sep = "."),
    paste(parameters[["module_name"]], parameters[["fleet"]],
      flat_label(parameters),
      sep = "."
    )
  )
  for (name in unique(parameters[["name"]])) {
    rows <- parameters[parameters[["name"]] == name, ]
    # The values that start in the 2nd model year and the rows of their
    # process or prior, such as a process mean by year.
    module_name <- rows[["module_name"]][1]
    target <- dplyr::coalesce(rows[["target_label"]][1], rows[["label"]][1])
    from_second_year <- (module_name == "Recruitment" &&
      target %in% c("log_devs", "log_r")) ||
      (module_name == "Depletion" && target == "log_depletion")
    is_second_year_value <- from_second_year && is.na(rows[["target_label"]][1])
    # 1 value, with or without a timing, is the same in every year.
    if (!is_second_year_value &&
      (all(is.na(rows[["timing"]])) || nrow(rows) == 1)) {
      next
    }
    expected_years <- if (from_second_year) years[-1] else years
    given_years <- sort(unique(stats::na.omit(rows[["timing"]])))
    if (anyNA(rows[["timing"]]) ||
      !identical(as.numeric(given_years), as.numeric(expected_years))) {
      no_timing <- if (anyNA(rows[["timing"]])) ", some with no timing" else ""
      first_year <- if (from_second_year) "the 2nd model year" else "model year"
      first_year_reason <- if (!from_second_year) {
        ""
      } else if (module_name == "Recruitment") {
        " Recruitment in the first year comes from the initial numbers at age."
      } else {
        " Depletion in the first year is the initial depletion."
      }
      cli::cli_abort(c(
        "x" = "{.val {name}} has {.val {nrow(rows)}} row{?s} for the
        year{?s} {.val {given_years}}{no_timing}.",
        "i" = "It needs rows for each year from {first_year}
        {.val {expected_years[1]}} to
        {.val {expected_years[length(expected_years)]}}, with the year in
        {.var timing}.{first_year_reason}",
        "i" = "Count the rows by year with {.code dplyr::filter(parameters,
        module_name == \"{module_name}\",
        label == \"{rows[['label']][1]}\") |> dplyr::count(fleet,
        timing)}."
      ))
    }
  }
  invisible(TRUE)
}

#' Stop when a random effect has no process or prior
#'
#' The Laplace approximation integrates the joint likelihood over the random
#' effects. With no process or prior, their density is flat, and the
#' integral adds a term that depends on the data's curvature alone.
#'
#' @param random The names of the random effects from initialize_parameters().
#' @param densities The densities from initialize_densities().
#' @noRd
check_random_effect_densities <- function(random, densities) {
  targets <- vapply(densities, `[[`, character(1), "target")
  # A parameter with random and other values is split, and its random part
  # is named "<name>|random".
  no_density <- random[!sub("|random", "", random, fixed = TRUE) %in% targets]
  if (length(no_density) > 0) {
    cli::cli_abort(c(
      "x" = "{.val {no_density}} {?is a random effect/are random effects}
      with no process or prior.",
      "i" = "A random effect needs a density to integrate over. Add a
      process with {.fn add_process}, or set {.var estimation_type} to
      {.val fixed_effects} or {.val constant}.",
      "i" = "Find the rows with {.code dplyr::filter(parameters,
      estimation_type == \"random_effects\")}."
    ))
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
      "x" = "The `estimation_type` must be one of: {valid_estimation_types}.",
      "i" = "Invalid values found: {.val {invalid_estimation_types}}.",
      "i" = "Count the values with
      {.code dplyr::count(parameters, estimation_type)}."
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

  if (!is.numeric(parameters[["value"]])) {
    cli::cli_abort(c(
      "x" = "The {.var value} column of {.var parameters} is
      {.cls {class(parameters[['value']])}}.",
      "i" = "Parameter values must be numbers.",
      "i" = "Convert it with {.code dplyr::mutate(parameters, value =
      as.numeric(value))} and check the rows that become {.code NA}."
    ))
  }
  # The surplus production model has no fleet parameters. Its fleets come
  # from the data.
  is_catch_at_age <- parameters_model_type(parameters) == "CatchAtAge"
  fleets <- stats::na.omit(unique(parameters[["fleet"]]))
  if (length(fleets) == 0 && is_catch_at_age) {
    cli::cli_abort(c(
      "x" = "No fleets found in the provided {.var parameters}.",
      "i" = "The {.var fleet} column is empty or all {.code NA}.",
      "i" = "Create {.var parameters} with {.fn setup_default_parameters},
      which fills in {.var fleet} for each fleet."
    ))
  }
  # Only fleets in the parameter tibble are in the model, so data for any
  # other fleet would be left out of the likelihood.
  data_fleets_missing <- setdiff(get_fleets(data), fleets)
  if (length(data_fleets_missing) > 0 && is_catch_at_age) {
    cli::cli_abort(c(
      "x" = "The data have fleet{?s} {.val {data_fleets_missing}}, which
      {?is/are} not in {.var parameters}.",
      "i" = "Each fleet in the data needs its rows in the parameter tibble.
      The fleets in {.var parameters} are {.val {fleets}}.",
      "i" = "Check the names with {.code get_fleets(data)} and
      {.code unique(parameters[[\"fleet\"]])}."
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
  check_growth_rows <- "Check the Growth rows with
    {.code dplyr::filter(parameters, module_name == \"Growth\")}."
  missing_reference_labels <- setdiff(
    c("reference_age_for_length_young", "reference_age_for_length_old"),
    growth[["label"]]
  )
  if (length(missing_reference_labels) > 0) {
    cli::cli_abort(c(
      "x" = "VonBertalanffySchnute growth requires reference-age inputs, but
      {.val {missing_reference_labels}} {?is/are} missing.",
      "i" = "{.fn setup_default_parameters} writes both reference ages. Give
      both when you write the Growth rows yourself.",
      "i" = check_growth_rows
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
      "x" = "VonBertalanffySchnute delta-method variability inputs are
      incomplete: {.val {setdiff(delta_method_labels, present_delta)}}
      {?is/are} missing.",
      "i" = "The delta-method option needs all 6 of
      {.val {delta_method_labels}}.",
      "i" = check_growth_rows
    ))
  }
  if (length(present_delta) == length(delta_method_labels)) {
    # The 3 correlations are a valid correlation matrix only if its
    # determinant is at least 0. The model would otherwise floor a negative
    # variance at 0 and give a length distribution with no meaning.
    correlation_value <- function(label) {
      inv_logit(-1, 1, growth[["value"]][growth[["label"]] == label][1])
    }
    young_old <- correlation_value(delta_method_labels[4])
    young_coefficient <- correlation_value(delta_method_labels[5])
    old_coefficient <- correlation_value(delta_method_labels[6])
    determinant <- 1 - young_old^2 - young_coefficient^2 -
      old_coefficient^2 + 2 * young_old * young_coefficient * old_coefficient
    if (determinant < 0) {
      cli::cli_abort(c(
        "x" = "VonBertalanffySchnute growth: the correlations of
        mean_length_young and mean_length_old
        ({.val {signif(young_old, 3)}}), mean_length_young and
        growth_coefficient ({.val {signif(young_coefficient, 3)}}), and
        mean_length_old and growth_coefficient
        ({.val {signif(old_coefficient, 3)}}) are not a valid correlation
        matrix.",
        "i" = "The correlation matrix must be positive semi-definite, so its
        determinant must be at least 0. Here it is
        {.val {signif(determinant, 3)}}.",
        "i" = check_growth_rows
      ))
    }
  }
  if (has_interpolation && length(present_delta) == length(delta_method_labels)) {
    cli::cli_abort(c(
      "x" = "VonBertalanffySchnute growth has rows for both length
      variability options: interpolation and the delta method.",
      "i" = "Use exactly 1 option. The default setup uses interpolation, with
      {.val length_at_age_sd_young} and {.val length_at_age_sd_old}. To use
      the delta method, remove those 2 rows.",
      "i" = check_growth_rows
    ))
  }
  if (!has_interpolation && length(present_delta) == 0) {
    cli::cli_abort(c(
      "x" = "VonBertalanffySchnute growth has no length variability rows.",
      "i" = "Use exactly 1 option: {.val length_at_age_sd_young} and
      {.val length_at_age_sd_old} (interpolation), or all 6 delta-method
      rows.",
      "i" = check_growth_rows
    ))
  }
  if (has_interpolation &&
    (nrow(sd_rows) != 2 || !setequal(sd_rows[["label"]], interpolation_labels) ||
      any(!is.na(sd_rows[["age"]])))) {
    cli::cli_abort(c(
      "x" = "VonBertalanffySchnute interpolation-based variability inputs are
      malformed: {.val {nrow(sd_rows)}} row{?s} with labels
      {.val {sd_rows[['label']]}} and ages {.val {sd_rows[['age']]}}.",
      "i" = "Give exactly 1 {.val length_at_age_sd_young} row and 1
      {.val length_at_age_sd_old} row, each with an {.var age} of NA. Their
      ages are the reference ages.",
      "i" = check_growth_rows
    ))
  }
  if (value("reference_age_for_length_old") <= value("reference_age_for_length_young")) {
    cli::cli_abort(c(
      "x" = "The VonBertalanffySchnute reference ages are
      {.val {value('reference_age_for_length_young')}} (young) and
      {.val {value('reference_age_for_length_old')}} (old).",
      "i" = "reference_age_for_length_old must be >
      reference_age_for_length_young.",
      "i" = check_growth_rows
    ))
  }
  for (label in c(
    "mean_length_young", "mean_length_old", "growth_coefficient",
    "length_weight_a", "length_weight_b"
  )) {
    if (!all(is.finite(value(label))) || any(value(label) <= 0)) {
      cli::cli_abort(c(
        "x" = "The VonBertalanffySchnute {.val {label}} is
        {.val {value(label)}}.",
        "i" = "{label} must be > 0.",
        "i" = check_growth_rows
      ))
    }
  }
  if (value("mean_length_old") <= value("mean_length_young")) {
    cli::cli_abort(c(
      "x" = "The VonBertalanffySchnute mean lengths are
      {.val {value('mean_length_young')}} (young) and
      {.val {value('mean_length_old')}} (old).",
      "i" = "mean_length_old must be > mean_length_young.",
      "i" = check_growth_rows
    ))
  }
  invisible(TRUE)
}

#' Check that the recruitment process and its parameters agree
#'
#' @inheritParams initialize_fims
#' @noRd
check_recruitment_process <- function(parameters) {
  check_recruitment_rows <- "Check the Recruitment rows with
    {.code dplyr::filter(parameters, module_name == \"Recruitment\")}."
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
        "x" = "Missing required inputs for recruitment process random or
        fixed effects. {.val {process_parameter_name}} is estimated, but no
        Recruitment row has a process.",
        "i" = "An estimated {.val {process_parameter_name}} needs a process.
        Set {.var distribution_type} to {.val process} and
        {.var distribution} to {.val Dnorm} on its rows and on the
        Recruitment {.val log_sd} row of the parameter tibble, or add a
        process with {.fn add_process}. Otherwise set its
        {.var estimation_type} to {.val constant}.",
        "i" = check_recruitment_rows
      ))
    }
    return(invisible(TRUE))
  }
  process_label <- unique(setdiff(process_rows[["label"]], "log_sd"))
  if (length(process_label) == 0) {
    cli::cli_abort(c(
      "x" = "Missing required inputs for recruitment process random or
      fixed effects. The Recruitment rows with a process are
      {.val {process_rows[['label']]}} only.",
      "i" = "A recruitment process needs {.val log_devs} or {.val log_r} rows
      with an {.var estimation_type} of {.val random_effects} or
      {.val fixed_effects}. Otherwise set {.var distribution_type} and
      {.var distribution} to {.code NA} on the Recruitment rows of the
      parameter tibble.",
      "i" = check_recruitment_rows
    ))
  }
  if (any(process_rows[["estimation_type"]][process_rows[["label"]] != "log_sd"] %in% "constant")) {
    cli::cli_abort(c(
      "x" = "Missing required inputs for recruitment process random or
      fixed effects. {.val {process_label}} has a process, but its
      {.var estimation_type} is {.val constant}.",
      "i" = "A process needs estimated values. Set the
      {.var estimation_type} of {.val {process_label}} to
      {.val random_effects} or {.val fixed_effects}, or set
      {.var distribution_type} and {.var distribution} to {.code NA} on the
      Recruitment rows of the parameter tibble.",
      "i" = check_recruitment_rows
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
      "i" = "The constant values still add to the negative log-likelihood
      and affect the estimate of {.var log_sd} (biased low when they are
      fixed at 0) and, for {.var log_r}, the stock-recruit parameters.",
      "i" = check_recruitment_rows
    ))
  }
  invisible(TRUE)
}
