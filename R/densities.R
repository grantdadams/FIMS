# Densities on parameters: priors and processes

#' 1 density entry per parameter with a prior or a process
#'
#' Priors come first, as they are first in nll_components.
#'
#' @param parameter_table The `parameter_table` from initialize_parameters().
#' @return A list of densities, each with its type, target, structure, mean
#'   and hyperparameters, by flat name.
#' @noRd
initialize_densities <- function(parameter_table) {
  c(prior_densities(parameter_table), process_densities(parameter_table))
}

#' 1 normal density per prior
#'
#' A prior is its 2 rows, expected_mean and log_sd, with distribution_type
#' "prior" and the target's name in the model in `target_label`
#' (add_priors()). The target's own rows are not changed, so a prior can be
#' put on any parameter, including a process's own sd. A prior on a constant
#' value is still scored, so a likelihood profile, which holds the profiled
#' parameter constant, includes the prior in its objective.
#' @noRd
prior_densities <- function(parameter_table) {
  prior_rows <- dplyr::filter(
    parameter_table,
    .data[["distribution_type"]] %in% "prior",
    .data[["label"]] %in% c("expected_mean", "log_sd"),
    !is.na(.data[["target_label"]])
  )
  groups <- dplyr::distinct(
    prior_rows, .data[["module_name"]], .data[["fleet"]], .data[["target_label"]]
  )
  lapply(seq_len(nrow(groups)), function(group) {
    module_name <- groups[["module_name"]][group]
    fleet <- groups[["fleet"]][group]
    target_label <- groups[["target_label"]][group]
    target <- if (is.na(fleet)) {
      paste(module_name, target_label, sep = ".")
    } else {
      paste(module_name, fleet, target_label, sep = ".")
    }
    target_rows <- dplyr::filter(parameter_table, .data[["name"]] == target)
    if (nrow(target_rows) == 0) {
      cli::cli_abort(c(
        "x" = "The prior on {.val {target}} has no target in the model.",
        "i" = "A prior's {.var target_label} names a parameter of the same
        module and fleet. {.fn add_priors} writes priors and checks the name.",
        "i" = "Find the prior rows with {.code dplyr::filter(parameters,
        distribution_type == \"prior\")}."
      ))
    }
    # Values that share 1 estimate (time blocks, a process mean) are scored
    # once, and each constant value is scored too. Positions are in the
    # parameter's own order (by column).
    shared_key <- dplyr::if_else(
      target_rows[["estimation_type"]] == "constant",
      paste0("row_", seq_len(nrow(target_rows))),
      paste(target_rows[["rtmb_name"]], target_rows[["rtmb_level"]])
    )
    positions <- value_position(target_rows, shape_parameter(target_rows))
    rows <- dplyr::filter(
      prior_rows,
      .data[["module_name"]] == .env[["module_name"]],
      .data[["fleet"]] %in% .env[["fleet"]],
      .data[["target_label"]] == .env[["target_label"]]
    )
    name_of <- function(label) rows[["name"]][rows[["label"]] == label][1]
    check_density_parameter_rows(rows, target_rows, target)
    list(
      distribution_type = "prior",
      target = target,
      positions = positions[!duplicated(shared_key)],
      structure = "iid",
      mean = list(kind = "parameter", name = name_of("expected_mean")),
      hyperparameters = list(log_sd = name_of("log_sd"))
    )
  })
}

#' 1 density per process
#'
#' Target rows have `distribution_type` "process". Their hyperparameters are
#' rows labeled expected_mean, log_sd and logit_rho with the target's label
#' in `target_label`. The recruitment process rows from
#' setup_default_parameters() have no target_label: their log_sd belongs to
#' log_devs or log_r.
#'
#' The mean of the density is the expected_mean parameter if there is one.
#' Otherwise it comes from the process mean tables for log_devs and log_r
#' (recruitment_process_means) and log_depletion (depletion_process_means),
#' or is 0 (deviations, random walks).
#' @noRd
process_densities <- function(parameter_table) {
  hyperparameter_labels <- c("expected_mean", "log_sd", "logit_rho")
  is_process <- parameter_table[["distribution_type"]] %in% "process"
  targets <- dplyr::filter(
    parameter_table,
    is_process & !.data[["label"]] %in% hyperparameter_labels
  )
  densities <- list()
  for (target in unique(targets[["name"]])) {
    rows <- dplyr::filter(targets, .data[["name"]] == target)
    label <- rows[["label"]][1]
    check_estimated_process(rows, target)
    distribution <- unique(stats::na.omit(rows[["distribution"]]))
    if (length(distribution) > 0 && !distribution %in% "Dnorm") {
      cli::cli_abort(c(
        "x" = "The process on {.val {target}} is {.val {distribution}}.",
        "i" = "Only {.val Dnorm} processes and priors are implemented.",
        "i" = "Check the {.var distribution} column with
        {.code dplyr::filter(parameters, distribution_type == \"process\")}."
      ))
    }
    process_structure <- unique(stats::na.omit(rows[["re_structure"]]))
    if (length(process_structure) == 0) {
      process_structure <- "iid"
    }
    # A process's own rows name their target in target_label. The 1
    # exception is the default recruitment process from
    # setup_default_parameters(), whose log_sd row has no target_label and
    # belongs to log_devs or log_r.
    is_hyperparameter <- is_process &
      parameter_table[["label"]] %in% hyperparameter_labels &
      parameter_table[["module_name"]] == rows[["module_name"]][1] &
      parameter_table[["fleet"]] %in% rows[["fleet"]][1] &
      (parameter_table[["target_label"]] %in% label |
        (is.na(parameter_table[["target_label"]]) &
          parameter_table[["module_name"]] == "Recruitment" &
          label %in% names(recruitment_process_means)))
    check_density_parameter_rows(
      dplyr::filter(parameter_table, is_hyperparameter),
      dplyr::filter(parameter_table, .data[["name"]] == target),
      target
    )
    hyperparameters <- parameter_table |>
      dplyr::filter(is_hyperparameter) |>
      dplyr::distinct(.data[["label"]], .data[["name"]])
    hyperparameter_name <- stats::setNames(
      hyperparameters[["name"]], hyperparameters[["label"]]
    )
    if (is.na(hyperparameter_name["log_sd"])) {
      cli::cli_abort(c(
        "x" = "The process on {.val {target}} has no {.val log_sd} row.",
        "i" = "Every process needs a {.val log_sd} row whose
        {.var target_label} is the target. {.fn add_process} adds it.",
        "i" = "Find the process rows with {.code dplyr::filter(parameters,
        distribution_type == \"process\")}."
      ))
    }
    if (process_structure == "ar1" && is.na(hyperparameter_name["logit_rho"])) {
      cli::cli_abort(c(
        "x" = "The AR1 process on {.val {target}} has no {.val logit_rho}
        row.",
        "i" = "An AR1 process needs a {.val logit_rho} row whose
        {.var target_label} is the target. {.fn add_process} adds it.",
        "i" = "Find the process rows with {.code dplyr::filter(parameters,
        distribution_type == \"process\")}."
      ))
    }
    process_means <- c(recruitment_process_means, depletion_process_means)
    density_mean <- if (!is.na(hyperparameter_name["expected_mean"])) {
      list(kind = "parameter", name = hyperparameter_name[["expected_mean"]])
    } else if (!is.null(process_means[[label]]) &&
      process_means[[label]] != "zero") {
      list(kind = "derived", name = process_means[[label]])
    } else {
      list(kind = "zero")
    }
    densities[[length(densities) + 1]] <- list(
      distribution_type = "process",
      target = target,
      structure = process_structure,
      mean = density_mean,
      hyperparameters = as.list(hyperparameter_name[intersect(
        c("log_sd", "logit_rho"), names(hyperparameter_name)
      )])
    )
  }
  densities
}

#' Stop when a process has no estimated values
#'
#' With every value constant, the process informs only its own
#' hyperparameters. When the values equal the mean, e.g., deviations fixed at
#' 0, an estimated sd goes to 0 and the objective is unbounded.
#' @noRd
check_estimated_process <- function(rows, target) {
  if (all(rows[["estimation_type"]] == "constant")) {
    cli::cli_abort(c(
      "x" = "The process on {.val {target}} has no estimated values.",
      "i" = "A process needs estimated values ({.val random_effects} or
      {.val fixed_effects}). On constant values it cannot inform the
      estimates, and an estimated process sd can go to 0. Estimate the
      values or remove the process.",
      "i" = "Check the estimation types with
      {.code dplyr::distinct(parameters, module_name, fleet, label,
      estimation_type)}."
    ))
  }
}

#' Stop unless a prior's or process's own parameter has 1 value or 1 per
#' target value
#'
#' A mean or sd with more than 1 value is read value by value with the
#' target's values, so it needs the target's rows: the same timing and, for a
#' target by age and year, the same age. Rows of any other length would be
#' recycled and read with the value of another year or age.
#'
#' @param rows The density's own rows (expected_mean, log_sd, logit_rho), in
#'   the parameter table's order (by timing, then age).
#' @param target_rows Every row of the target, in the same order.
#' @param target The target's flat name.
#' @noRd
check_density_parameter_rows <- function(rows, target_rows, target) {
  for (name in unique(rows[["name"]])) {
    parameter_rows <- dplyr::filter(rows, .data[["name"]] == .env[["name"]])
    is_matched <- nrow(parameter_rows) == 1 ||
      (identical(
        as.numeric(parameter_rows[["timing"]]),
        as.numeric(target_rows[["timing"]])
      ) &&
        identical(
          as.numeric(parameter_rows[["age"]]),
          as.numeric(target_rows[["age"]])
        ))
    if (is_matched) {
      next
    }
    target_years <- unique(stats::na.omit(target_rows[["timing"]]))
    target_timing <- if (length(target_years) == 0) {
      "It has no values by year."
    } else {
      cli::format_inline(
        "Its values are for the years {.val {min(target_years)}} to
        {.val {max(target_years)}}."
      )
    }
    cli::cli_abort(c(
      "x" = "{.val {name}} has {.val {nrow(parameter_rows)}} rows that do not
      match the {.val {nrow(target_rows)}} value{?s} of {.val {target}} by
      year and age.",
      "i" = "A process or prior mean, sd or rho needs 1 row, or 1 row for
      each value of its target with the same {.var timing} and {.var age}.
      {target_timing}",
      "i" = "Compare the rows by year with {.code dplyr::filter(parameters,
      module_name == \"{parameter_rows[['module_name']][1]}\") |>
      dplyr::count(fleet, label, timing)}."
    ))
  }
  invisible(TRUE)
}

#' Use an estimated family parameter when the tibble has a row for it
#'
#' A data likelihood's parameters (e.g., log_sd) come from the data's
#' uncertainty column. A tibble row with module_name "Data", the fleet, and
#' label "<data type>_<parameter>" (e.g., "index_log_sd") replaces it, so it
#' can be estimated.
#'
#' @param component 1 entry of `model_data$likelihoods`.
#' @param parameter_names The flat parameter names.
#' @noRd
link_estimated_family_parameters <- function(component, parameter_names) {
  for (name in names(component[["parameters"]])) {
    flat_name <- paste(
      "Data", component[["fleet"]],
      paste(component[["data_type"]], name, sep = "_"),
      sep = "."
    )
    if (flat_name %in% parameter_names) {
      component[["estimated_parameters"]][[name]] <- flat_name
    }
  }
  component
}
