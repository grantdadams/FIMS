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
#' put on any parameter, including a process's own sd.
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
        "i" = "Write priors with {.fn add_priors}, which checks the name."
      ))
    }
    check_estimated_target(target_rows, "prior", target)
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
#' rows labelled expected_mean, log_sd and logit_rho with the target's label
#' in `target_label`. The recruitment process rows from
#' setup_default_parameters() have no target_label: their log_sd belongs to
#' log_devs or log_r.
#'
#' The mean of the density is the expected_mean parameter if there is one.
#' Otherwise it comes from the recruitment process table for log_devs and log_r
#' (recruitment_process_means), or is 0 (deviations, random walks).
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
    check_estimated_target(rows, "process", target)
    distribution <- unique(stats::na.omit(rows[["distribution"]]))
    if (length(distribution) > 0 && !distribution %in% "Dnorm") {
      cli::cli_abort(c(
        "x" = "The process on {.val {target}} is {.val {distribution}}.",
        "i" = "Only Dnorm processes and priors are implemented."
      ))
    }
    process_structure <- unique(stats::na.omit(rows[["re_structure"]]))
    if (length(process_structure) == 0) {
      process_structure <- "iid"
    }
    is_hyperparameter <- is_process &
      parameter_table[["label"]] %in% hyperparameter_labels &
      parameter_table[["module_name"]] == rows[["module_name"]][1] &
      parameter_table[["fleet"]] %in% rows[["fleet"]][1] &
      (parameter_table[["target_label"]] %in% label |
        (is.na(parameter_table[["target_label"]]) &
          parameter_table[["module_name"]] == "Recruitment" &
          label %in% names(recruitment_process_means)))
    hyperparameters <- parameter_table |>
      dplyr::filter(is_hyperparameter) |>
      dplyr::distinct(.data[["label"]], .data[["name"]])
    hyperparameter_name <- stats::setNames(
      hyperparameters[["name"]], hyperparameters[["label"]]
    )
    if (is.na(hyperparameter_name["log_sd"])) {
      cli::cli_abort(c(
        "x" = "The process on {.val {target}} has no {.val log_sd} row.",
        "i" = "Write processes with {.fn add_process}, which adds it."
      ))
    }
    if (process_structure == "ar1" && is.na(hyperparameter_name["logit_rho"])) {
      cli::cli_abort(c(
        "x" = "The AR1 process on {.val {target}} has no {.val logit_rho} row.",
        "i" = "Write processes with {.fn add_process}, which adds it."
      ))
    }
    density_mean <- if (!is.na(hyperparameter_name["expected_mean"])) {
      list(kind = "parameter", name = hyperparameter_name[["expected_mean"]])
    } else if (!is.null(recruitment_process_means[[label]]) &&
      recruitment_process_means[[label]] != "zero") {
      list(kind = "derived", name = recruitment_process_means[[label]])
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

#' Stop when a prior or process has no estimated values
#'
#' A density on values that are all fixed would only move its own sd, to 0
#' for an estimated sd, where the objective is unbounded.
#' @noRd
check_estimated_target <- function(rows, density_type, target) {
  if (all(rows[["estimation_type"]] == "constant")) {
    cli::cli_abort(c(
      "x" = "The {density_type} on {.val {target}} has no estimated values.",
      "i" = "Estimate them ({.val random_effects} or {.val fixed_effects}),
      or remove the {density_type}."
    ))
  }
}

#' Use an estimated family parameter when the tibble has 1
#'
#' A data likelihood's parameters (e.g. log_sd) come from the data's
#' uncertainty column. A tibble row with module_name "Data", the fleet, and
#' label "<data type>_<parameter>" (e.g. "index_log_sd") replaces it, so it
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
