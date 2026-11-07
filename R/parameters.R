# Parameters: from the parameter tibble to RTMB and back
#
# RTMB needs a flat list of parameters. initialize_parameters() names each
# parameter "module.fleet.label" (or "module.label" when there is no fleet),
# builds the map (which values are fixed or shared) and the list of random
# effects. Inside the model, group_parameters() undoes the naming, so module
# code reads module_parameters$Selectivity$fleet1$slope.
# assemble_split_parameters() rejoins parameters that initialize_parameters()
# split into random and fixed parts.

#' Turn the parameter tibble into a flat RTMB parameter list
#'
#' Every row with a value becomes a parameter value. A process or prior
#' hyperparameter row (expected_mean, log_sd, logit_rho) with a
#' `target_label` is named after its target, e.g.,
#' "Fleet.survey1.log_q_log_sd". Constant values are parameters too, but
#' fixed with `map`, so the model reads all values the same way.
#'
#' Shapes: a parameter with rows by year (timing) is a matrix with 1 row per
#' year (1 column, or 1 column per age). By age only, it is a vector by age.
#' Otherwise it is a vector in row order.
#'
#' `map`: constant rows are fixed (NA). Rows with the same `map_group` share
#' 1 estimate. Every other estimated row has its own.
#'
#' @param parameters The parameter tibble.
#' @return A list with `parameters`, `map`, `random`, `splits` and
#'   `parameter_table` (the tibble with the RTMB name and map level of each
#'   row).
#' @noRd
initialize_parameters <- function(parameters) {
  # Parameter names are split on "." to group them by module and fleet.
  dotted_fleets <- unique(stats::na.omit(
    parameters[["fleet"]][grepl(".", parameters[["fleet"]], fixed = TRUE)]
  ))
  if (length(dotted_fleets) > 0) {
    cli::cli_abort(c(
      "x" = "Fleet names cannot contain {.val .}: {.val {dotted_fleets}}.",
      "i" = "Parameter names are split on {.val .} to find the module and
      fleet."
    ))
  }
  parameter_table <- parameters |>
    # The EWAA weight_at_age row is a placeholder: weights come from the
    # data, whatever its estimation type. Any other row with no estimation
    # type is a constant.
    dplyr::filter(
      !(.data[["module_name"]] %in% "Growth" & .data[["label"]] == "weight_at_age"),
      !(is.na(.data[["estimation_type"]]) & is.na(.data[["value"]]))
    ) |>
    dplyr::mutate(estimation_type = dplyr::coalesce(
      .data[["estimation_type"]], "constant"
    )) |>
    growth_to_working_scale() |>
    add_process_columns()
  parameter_table[["flat_label"]] <- flat_label(parameter_table)
  parameter_table <- parameter_table |>
    dplyr::mutate(
      name = dplyr::if_else(
        is.na(.data[["fleet"]]),
        paste(.data[["module_name"]], .data[["flat_label"]], sep = "."),
        paste(.data[["module_name"]], .data[["fleet"]], .data[["flat_label"]],
          sep = "."
        )
      )
    )
  check_row_order(parameter_table)
  parameter_table <- parameter_table |>
    dplyr::arrange(
      factor(.data[["name"]], levels = unique(.data[["name"]])),
      .data[["timing"]], .data[["age"]]
    )

  model_parameters <- list()
  map <- list()
  random <- character(0)
  splits <- list()
  # An id for each value, used in get_estimates() and in the sdreport's
  # names.
  parameter_table[["parameter_id"]] <- seq_len(nrow(parameter_table))
  parameter_table[["rtmb_name"]] <- NA_character_
  parameter_table[["rtmb_level"]] <- NA_integer_
  for (name in unique(parameter_table[["name"]])) {
    row_index <- which(parameter_table[["name"]] == name)
    rows <- dplyr::slice(parameter_table, row_index)
    value <- shape_parameter(rows)
    # Position of each row in as.vector(value), which is how RTMB reads a
    # parameter and its map (by column).
    position <- value_position(rows, value)
    estimation_type <- rows[["estimation_type"]]
    is_random <- estimation_type == "random_effects"
    if (any(is_random) && any(!is.na(rows[["map_group"]][is_random]))) {
      cli::cli_abort(c(
        "x" = "Random effects in {.val {name}} cannot share values through
        {.var map_group}.",
        "i" = "A shared random effect would be scored once per row by its
        process density."
      ))
    }
    # RTMB marks random effects by parameter name, so a parameter with both
    # random and other values is split into 2 RTMB parameters. The model puts
    # them back together (assemble_split_parameters()).
    parts <- if (any(is_random) && !all(is_random)) {
      splits[[name]] <- list(
        length = length(value),
        random = position[is_random], fixed = position[!is_random],
        dim = dim(value)
      )
      list(
        list(name = paste0(name, "|random"), rows = which(is_random)),
        list(name = paste0(name, "|fixed"), rows = which(!is_random))
      )
    } else {
      list(list(name = name, rows = seq_along(estimation_type)))
    }
    for (part in parts) {
      part_rows <- part[["rows"]]
      map_level <- map_levels(
        estimation_type[part_rows] != "constant", rows[["map_group"]][part_rows]
      )
      if (length(parts) == 1) {
        # Values and map in the parameter's own order (by column).
        model_parameters[[part[["name"]]]] <- value
        map_in_value_order <- integer(length(map_level))
        map_in_value_order[position] <- map_level
        map[[part[["name"]]]] <- factor(map_in_value_order)
      } else {
        # Split parts are plain vectors in row order.
        model_parameters[[part[["name"]]]] <- rows[["value"]][part_rows]
        map[[part[["name"]]]] <- factor(map_level)
      }
      if (all(estimation_type[part_rows] == "random_effects")) {
        random <- c(random, part[["name"]])
      }
      check_shared_values(part[["name"]], rows[["value"]][part_rows], map_level)
      parameter_table[["rtmb_name"]][row_index[part_rows]] <- part[["name"]]
      parameter_table[["rtmb_level"]][row_index[part_rows]] <- map_level
    }
  }
  list(
    parameters = model_parameters, map = map, random = random,
    splits = splits, parameter_table = parameter_table
  )
}

#' Each row's label in the model
#'
#' A row's label is its tibble label, except for a process's or prior's own
#' parameters. A process's own parameters are named after its target
#' (log_q_log_sd), and a prior's after its target and "prior"
#' (log_q_prior_log_sd). So a prior on a process's own parameter, such as the
#' recruitment log_sd, does not share its name (log_sd_prior_log_sd).
#'
#' @param parameters The parameter tibble, with a `target_label` column.
#' @return A character vector with 1 label per row.
#' @noRd
flat_label <- function(parameters) {
  dplyr::case_when(
    is.na(parameters[["target_label"]]) ~ parameters[["label"]],
    parameters[["distribution_type"]] %in% "prior" ~ paste(
      parameters[["target_label"]], "prior", parameters[["label"]],
      sep = "_"
    ),
    .default = paste(
      parameters[["target_label"]], parameters[["label"]],
      sep = "_"
    )
  )
}

#' Add the columns processes and priors use, when the tibble does not have
#' them
#'
#' setup_default_parameters() does not write these columns.
#' @noRd
add_process_columns <- function(parameters) {
  defaults <- list(
    map_group = NA_integer_, target_label = NA_character_,
    re_structure = NA_character_
  )
  for (column in names(defaults)) {
    if (!column %in% names(parameters)) {
      parameters[[column]] <- defaults[[column]]
    }
  }
  parameters
}

#' A parameter's value in its model shape (see initialize_parameters())
#' @noRd
shape_parameter <- function(rows) {
  value <- rows[["value"]]
  has_timing <- all(!is.na(rows[["timing"]]))
  has_age <- all(!is.na(rows[["age"]]))
  if (!has_timing) {
    return(value)
  }
  n_years <- dplyr::n_distinct(rows[["timing"]])
  n_ages <- if (has_age) dplyr::n_distinct(rows[["age"]]) else 1
  if (n_years * n_ages != length(value)) {
    cli::cli_abort(c(
      "x" = "Parameter {.val {rows[['name']][1]}} has {length(value)} row{?s}.",
      "i" = "It needs 1 row per year{if (has_age) ' and age' else ''}
      ({n_years * n_ages})."
    ))
  }
  # Rows are sorted by timing, then age, so fill by row (year).
  matrix(value, n_years, n_ages, byrow = TRUE)
}

#' Position of each row's value in as.vector(value)
#' @noRd
value_position <- function(rows, value) {
  if (!is.matrix(value)) {
    return(seq_len(nrow(rows)))
  }
  row_number_in_matrix <- matrix(
    seq_len(nrow(rows)), nrow(value), ncol(value),
    byrow = TRUE
  )
  match(seq_len(nrow(rows)), as.vector(row_number_in_matrix))
}

#' Map levels: NA for fixed values, 1 shared level per map_group, and 1 level
#' for every other estimated value
#' @noRd
map_levels <- function(is_estimated, map_group) {
  key <- dplyr::if_else(
    is.na(map_group), paste0("own_", seq_along(map_group)),
    paste0("group_", map_group)
  )
  key[!is_estimated] <- NA
  as.integer(factor(key, levels = unique(stats::na.omit(key))))
}

#' Refuse a parameter whose rows are not in year and age order
#'
#' A parameter's values are read by year, then age. Rows out of that order
#' would put values in the wrong years if read as given, so they are refused
#' rather than sorted without the user knowing.
#' @noRd
check_row_order <- function(parameter_table) {
  unsorted <- parameter_table |>
    dplyr::group_by(.data[["name"]]) |>
    dplyr::filter(
      is.unsorted(order(.data[["timing"]], .data[["age"]], na.last = TRUE))
    ) |>
    dplyr::pull(.data[["name"]]) |>
    unique()
  if (length(unsorted) > 0) {
    cli::cli_abort(c(
      "x" = "The rows of {.val {unsorted}} are not in year and age order.",
      "i" = "Values are read by year, then age. Sort the rows, for example
      with {.code dplyr::arrange(parameters, module_name, fleet, label,
      timing, age)}."
    ))
  }
  invisible(parameter_table)
}

#' Refuse shared values with different starting values
#'
#' Values that share 1 estimate (the same map level) must start from the
#' same value. Otherwise the model would start from, and report at
#' optimize = FALSE, a value other than the tibble's.
#' @noRd
check_shared_values <- function(name, value, map_level) {
  differs <- tapply(value, map_level, function(x) any(x != x[[1]]))
  if (any(differs)) {
    differing_levels <- as.integer(names(differs)[differs])
    first_level <- differing_levels[1]
    cli::cli_abort(c(
      "x" = "Values of {.val {name}} that share 1 estimate have different
      starting values.",
      "i" = "Map levels with different starting values:
      {.val {differing_levels}}. Level {.val {first_level}} has
      {.val {value[map_level %in% first_level]}}.",
      "i" = "Use identical starting values to share a parameter."
    ))
  }
  invisible(value)
}

#' Growth parameters on the scale they are estimated on
#'
#' The tibble gives natural-scale values, but some growth parameters are
#' estimated on a log or bounded logit scale, which keeps them in their valid
#' range (each growth type's `working_scale` in growth_modules). This
#' transforms them and renames them log_* or logit_*, so the transform is
#' visible. The original label and value are kept for get_estimates(), which
#' reports them on the natural scale. von Bertalanffy reference ages are data
#' (see von_bertalanffy_schnute_growth_inputs()), so they are dropped here.
#' @noRd
growth_to_working_scale <- function(parameters) {
  parameters[["original_label"]] <- parameters[["label"]]
  parameters[["original_value"]] <- parameters[["value"]]
  parameters[["working_scale"]] <- NA_character_
  is_growth <- parameters[["module_name"]] %in% "Growth"
  growth_type <- unique(stats::na.omit(parameters[["module_type"]][is_growth]))
  if (length(growth_type) == 0) {
    return(parameters)
  }
  growth_rows <- dplyr::filter(parameters, is_growth)
  parameters[["working_scale"]][is_growth] <-
    growth_modules[[growth_type]][["working_scale"]](growth_rows)
  is_log <- parameters[["working_scale"]] %in% "log"
  parameters[["value"]][is_log] <- log(parameters[["value"]][is_log])
  is_logit <- parameters[["working_scale"]] %in% "logit"
  if (any(is_logit)) {
    # Logit bounded by 0 and L2, so L1 stays below L2: log(L1 / (L2 - L1)).
    mean_length_old <- growth_rows[["value"]][growth_rows[["label"]] == "mean_length_old"]
    parameters[["value"]][is_logit] <-
      log(parameters[["value"]][is_logit]) -
      log(mean_length_old - parameters[["value"]][is_logit])
  }
  is_transformed <- !is.na(parameters[["working_scale"]])
  parameters[["label"]][is_transformed] <- paste0(
    parameters[["working_scale"]][is_transformed], "_",
    parameters[["label"]][is_transformed]
  )
  is_reference_age <- is_growth & grepl("^reference_age", parameters[["label"]])
  dplyr::filter(parameters, !is_reference_age)
}

#' Regroup flat parameter names into a nested list
#'
#' Turns "Selectivity.fleet1.slope" into module_parameters$Selectivity$
#' fleet1$slope.
#'
#' @param parameters The flat parameter list.
#' @param skip Flat names to leave out.
#' @noRd
group_parameters <- function(parameters, skip = character(0)) {
  grouped <- list()
  for (name in setdiff(names(parameters), skip)) {
    path <- strsplit(name, ".", fixed = TRUE)[[1]]
    module <- path[1]
    if (is.null(grouped[[module]])) {
      grouped[[module]] <- list()
    }
    if (length(path) == 2) {
      grouped[[module]][[path[2]]] <- parameters[[name]]
    } else {
      fleet <- path[2]
      if (is.null(grouped[[module]][[fleet]])) {
        grouped[[module]][[fleet]] <- list()
      }
      grouped[[module]][[fleet]][[path[3]]] <- parameters[[name]]
    }
  }
  grouped
}

#' Put back together parameters that mix random and fixed effects
#'
#' RTMB marks random effects by parameter name, so initialize_parameters()
#' splits a parameter whose values mix the 2 (e.g., a random walk, whose first
#' year is a fixed effect) into "name|random" and "name|fixed". This rebuilds
#' the original parameter, in its original shape, before the model uses it.
#'
#' @param parameters The flat parameter list.
#' @param splits The `splits` element of initialize_parameters().
#' @noRd
assemble_split_parameters <- function(parameters, splits) {
  # RTMB: lets `x[i] <- value` store AD values in a plain vector.
  "[<-" <- RTMB::ADoverload("[<-")
  for (name in names(splits)) {
    split_info <- splits[[name]]
    whole <- numeric(split_info[["length"]])
    whole[split_info[["random"]]] <- parameters[[paste0(name, "|random")]]
    whole[split_info[["fixed"]]] <- parameters[[paste0(name, "|fixed")]]
    if (!is.null(split_info[["dim"]])) {
      whole <- RTMB::matrix(whole, split_info[["dim"]][1], split_info[["dim"]][2])
    }
    parameters[[paste0(name, "|random")]] <- NULL
    parameters[[paste0(name, "|fixed")]] <- NULL
    parameters[[name]] <- whole
  }
  parameters
}
