# Readers for the male-only GMACS snow crab model (26_gmacs_male_only).
#
# GMACS (ADMB) reads its .dat and .ctl files as a stream of whitespace-
# separated values with everything after "#" ignored. These readers do the
# same: they take the values in order, following the read order in
# gmacsbase.TPL (DATA_SECTION), and stop if the end-of-file marker 9999 is
# not where it should be. They cover only the options this model uses
# (old-format catch, index and size data, 1 sex, 1 shell condition and 2
# maturity states) and stop on anything else.

# A stream of values from a GMACS input file, with comments removed.
gmacs_stream <- function(path) {
  values <- readLines(path, warn = FALSE) |>
    sub(pattern = "#.*$", replacement = "") |>
    trimws() |>
    strsplit("[[:space:]]+") |>
    unlist()
  values <- values[nzchar(values)]
  position <- 0
  take <- function(n = 1, numeric = TRUE) {
    if (position + n > length(values)) {
      cli::cli_abort("Read past the end of {.file {path}}.")
    }
    out <- values[position + seq_len(n)]
    position <<- position + n
    if (numeric) {
      out <- as.numeric(out)
      if (anyNA(out)) {
        cli::cli_abort(
          "Non-numeric value in {.file {path}} near value {position}, counted
          with comments removed."
        )
      }
    }
    out
  }
  take_matrix <- function(n_rows, n_columns) {
    matrix(
      take(n_rows * n_columns),
      nrow = n_rows, ncol = n_columns, byrow = TRUE
    )
  }
  list(take = take, take_matrix = take_matrix)
}

# Stops with the name of each check that fails. Each name says what the
# readers or the R version of GMACS need.
check_gmacs_input <- function(checks) {
  failed <- names(checks)[!checks]
  if (length(failed) > 0) {
    cli::cli_abort(c(
      "x" = "The GMACS input uses an option that the readers or the R version
        of GMACS do not cover.",
      "i" = "They need:",
      purrr::set_names(failed, rep("*", length(failed)))
    ))
  }
}

read_gmacs_dat <- function(path) {
  stream <- gmacs_stream(path)
  gmacs_data <- list()
  gmacs_data[["start_year"]] <- stream[["take"]]()
  gmacs_data[["end_year"]] <- stream[["take"]]()
  gmacs_data[["n_seasons"]] <- stream[["take"]]()
  gmacs_data[["n_fleets"]] <- stream[["take"]]()
  gmacs_data[["n_sexes"]] <- stream[["take"]]()
  gmacs_data[["n_shell_conditions"]] <- stream[["take"]]()
  gmacs_data[["n_maturity_states"]] <- stream[["take"]]()
  gmacs_data[["n_lengths"]] <- stream[["take"]]()
  gmacs_data[["season_recruitment"]] <- stream[["take"]]()
  gmacs_data[["season_growth"]] <- stream[["take"]]()
  gmacs_data[["season_spawning_biomass"]] <- stream[["take"]]()
  gmacs_data[["season_numbers"]] <- stream[["take"]]()
  check_gmacs_input(c(
    "1 sex, 1 shell condition and 2 maturity states" =
      gmacs_data[["n_sexes"]] == 1 &&
        gmacs_data[["n_shell_conditions"]] == 1 &&
        gmacs_data[["n_maturity_states"]] == 2
  ))
  n_lengths <- gmacs_data[["n_lengths"]]
  gmacs_data[["n_lengths_by_sex"]] <- stream[["take"]](gmacs_data[["n_sexes"]])
  length_edges <- stream[["take"]](n_lengths + 1)
  gmacs_data[["length_edges"]] <- length_edges
  gmacs_data[["length_midpoints"]] <-
    (length_edges[-1] + length_edges[-(n_lengths + 1)]) / 2
  check_gmacs_input(c(
    "M by season as a vector (type 1)" = stream[["take"]]() == 1
  ))
  # Fraction of the annual M in each season.
  gmacs_data[["m_season_fraction"]] <- stream[["take"]](
    gmacs_data[["n_seasons"]]
  )
  gmacs_data[["fleet_names"]] <- stream[["take"]](
    gmacs_data[["n_fleets"]],
    numeric = FALSE
  )
  gmacs_data[["season_type"]] <- stream[["take"]](gmacs_data[["n_seasons"]])

  check_gmacs_input(c("Old-format catch data" = stream[["take"]]() == 0))
  n_catch_series <- stream[["take"]]()
  n_catch_rows <- stream[["take"]](n_catch_series)
  catch_columns <- c(
    "year", "season", "fleet", "sex", "observed", "cv", "type",
    "units", "multiplier", "effort", "discard_mortality"
  )
  gmacs_data[["catch"]] <- purrr::map(n_catch_rows, function(n_rows) {
    stream[["take_matrix"]](n_rows, length(catch_columns)) |>
      as.data.frame() |>
      purrr::set_names(catch_columns)
  })

  check_gmacs_input(c("Old-format index data" = stream[["take"]]() == 0))
  n_surveys <- stream[["take"]]()
  gmacs_data[["survey_type"]] <- stream[["take"]](n_surveys)
  n_index_rows <- stream[["take"]]()
  index_columns <- c(
    "catchability_index", "year", "season", "fleet", "sex", "maturity",
    "observed", "cv", "units", "timing", "index_series"
  )
  gmacs_data[["index"]] <- stream[["take_matrix"]](
    n_index_rows, length(index_columns)
  ) |>
    as.data.frame() |>
    purrr::set_names(index_columns)
  check_gmacs_input(c("Index check value 999" = stream[["take"]]() == 999))

  check_gmacs_input(c("Old-format size data" = stream[["take"]]() == 0))
  n_compositions <- stream[["take"]]()
  n_composition_rows <- stream[["take"]](n_compositions)
  n_composition_lengths <- stream[["take"]](n_compositions)
  composition_columns <- c(
    "year", "season", "fleet", "sex", "type", "shell_condition",
    "maturity", "sample_size"
  )
  n_info_columns <- length(composition_columns)
  gmacs_data[["compositions"]] <- purrr::map2(
    n_composition_rows, n_composition_lengths,
    function(n_rows, n_composition_length) {
      values <- stream[["take_matrix"]](
        n_rows, n_info_columns + n_composition_length
      )
      list(
        info = values[, seq_len(n_info_columns), drop = FALSE] |>
          as.data.frame() |>
          purrr::set_names(composition_columns),
        observed = values[, -seq_len(n_info_columns), drop = FALSE]
      )
    }
  )

  check_gmacs_input(c(
    "Growth increment data type 1" = stream[["take"]]() == 1
  ))
  n_growth_rows <- stream[["take"]]()
  gmacs_data[["growth"]] <- stream[["take_matrix"]](n_growth_rows, 4) |>
    as.data.frame() |>
    purrr::set_names(c("premolt_length", "sex", "increment", "cv"))

  check_gmacs_input(c(
    "Old-format environmental data" = stream[["take"]]() == 0,
    "0 environmental series" = stream[["take"]]() == 0,
    "End-of-file marker 9999 in the .dat" = stream[["take"]]() == 9999
  ))
  gmacs_data
}

# Rows of a parameter table: initial, lower, upper, prior type, prior 1,
# prior 2, phase, then the time-variation columns when present (not used
# here).
parameter_rows <- function(values) {
  data.frame(
    initial = values[, 1],
    lower = values[, 2],
    upper = values[, 3],
    prior_type = values[, 4],
    prior_1 = values[, 5],
    prior_2 = values[, 6],
    phase = values[, 7]
  )
}

read_gmacs_ctl <- function(path, gmacs_data) {
  stream <- gmacs_stream(path)
  n_lengths <- gmacs_data[["n_lengths"]]
  n_fleets <- gmacs_data[["n_fleets"]]
  gmacs_control <- list()

  n_block_groups <- stream[["take"]]()
  blocks_per_group <- stream[["take"]](n_block_groups)
  # Each block group is a matrix of first and last years.
  gmacs_control[["time_blocks"]] <- purrr::map(
    blocks_per_group,
    function(n_blocks) stream[["take_matrix"]](n_blocks, 2)
  )
  check_gmacs_input(c(
    "0 environmental treatments" = stream[["take"]]() == 0
  ))

  general <- stream[["take"]](19)
  names(general) <- c(
    "recruitment_deviation_start_year", "recruitment_deviation_end_year",
    "terminal_molt", "recruitment_deviation_phase",
    "recruitment_proportion_phase", "initial_sex_ratio", "initial_conditions",
    "reference_size_class", "spr_lambda", "stock_recruitment",
    "average_sex_ratio", "years_equilibrium", "deviation_parameter_phase",
    "bias_first_year", "bias_first_full_year", "bias_last_full_year",
    "bias_last_year", "recruitment_size_option", "copy_growth"
  )
  gmacs_control[["general"]] <- general
  check_gmacs_input(c(
    "Initial conditions 2 (free parameters)" =
      general[["initial_conditions"]] == 2,
    "No stock-recruit curve" = general[["stock_recruitment"]] == 0
  ))

  # GMACS calls this table theta: recruitment and initial numbers.
  n_theta <- 8 + gmacs_data[["n_maturity_states"]] * n_lengths
  gmacs_control[["theta"]] <- parameter_rows(
    stream[["take_matrix"]](n_theta, 14)
  )

  check_gmacs_input(c(
    "Weight at length as vectors (type 2)" = stream[["take"]]() == 2
  ))
  gmacs_control[["weight_at_length"]] <- stream[["take_matrix"]](
    gmacs_data[["n_maturity_states"]], n_lengths
  )
  gmacs_control[["proportion_mature"]] <- stream[["take"]](n_lengths)
  gmacs_control[["proportion_legal"]] <- stream[["take"]](n_lengths)

  gmacs_control[["recruitment_max_length_class"]] <- stream[["take"]](
    gmacs_data[["n_sexes"]]
  )
  gmacs_control[["functional_maturity"]] <- stream[["take"]]()
  gmacs_control[["growth_type"]] <- stream[["take"]](3)
  gmacs_control[["molt_type"]] <- stream[["take"]](2)
  gmacs_control[["maturity_type"]] <- stream[["take"]](2)
  check_gmacs_input(c(
    "A gamma post-molt size with a linear increment" =
      gmacs_control[["growth_type"]][1] == 4 &&
        gmacs_control[["growth_type"]][2] == 1,
    "Molt probability 1" = gmacs_control[["molt_type"]][1] == 1,
    "A pre-specified terminal molt probability" =
      gmacs_control[["maturity_type"]][1] == 0
  ))
  gmacs_control[["growth_parameters"]] <- parameter_rows(
    stream[["take_matrix"]](3, 14)
  )
  # 1 row of terminal molt probability for the years outside every block,
  # then 1 row per block.
  maturity_block_group <- gmacs_control[["maturity_type"]][2]
  n_maturity_rows <- blocks_per_group[maturity_block_group] + 1
  gmacs_control[["terminal_molt_probability"]] <- stream[["take_matrix"]](
    n_maturity_rows, n_lengths
  )

  gmacs_control[["m_time_variation"]] <- stream[["take_matrix"]](
    gmacs_data[["n_maturity_states"]], 13
  )
  gmacs_control[["m_parameters"]] <- parameter_rows(
    stream[["take_matrix"]](8, 7)
  )

  gmacs_control[["selectivity_sex_specific"]] <- stream[["take"]](n_fleets)
  gmacs_control[["selectivity_type"]] <- stream[["take"]](n_fleets)
  gmacs_control[["selectivity_within_another"]] <- stream[["take"]](n_fleets)
  gmacs_control[["selectivity_extra"]] <- stream[["take"]](n_fleets)
  gmacs_control[["selectivity_max_at_1"]] <- stream[["take"]](n_fleets)
  gmacs_control[["selectivity_class_at_1"]] <- stream[["take"]](n_fleets)
  gmacs_control[["retention_sex_specific"]] <- stream[["take"]](n_fleets)
  gmacs_control[["retention_type"]] <- stream[["take"]](n_fleets)
  gmacs_control[["retention_flag"]] <- stream[["take"]](n_fleets)
  gmacs_control[["retention_extra"]] <- stream[["take"]](n_fleets)
  gmacs_control[["retention_max"]] <- stream[["take"]](n_fleets)
  # Logistic (type 2) has 2 parameters and per-class selectivity (type 0)
  # has 1 per length class. Retention type 5 has none.
  selectivity_type <- gmacs_control[["selectivity_type"]]
  retention_type <- gmacs_control[["retention_type"]]
  n_selectivity <- dplyr::case_when(
    selectivity_type == 2 ~ 2,
    selectivity_type == 0 ~ n_lengths
  ) |>
    sum()
  n_retention <- dplyr::case_when(
    retention_type == 2 ~ 2,
    retention_type == 5 ~ 0
  ) |>
    sum()
  check_gmacs_input(c(
    "Logistic and per-class selectivity only" =
      !is.na(n_selectivity) && !is.na(n_retention)
  ))
  gmacs_control[["selectivity_parameters"]] <- parameter_rows(
    stream[["take_matrix"]](n_selectivity + n_retention, 14)
  )

  n_indices <- length(unique(gmacs_data[["index"]][["catchability_index"]]))
  gmacs_control[["catchability_controls"]] <- stream[["take_matrix"]](
    n_indices, 10
  )
  gmacs_control[["catchability_parameters"]] <- parameter_rows(
    stream[["take_matrix"]](n_indices, 7)
  )
  gmacs_control[["additional_cv_controls"]] <- stream[["take_matrix"]](
    n_indices, 7
  )
  gmacs_control[["additional_cv_parameters"]] <- parameter_rows(
    stream[["take_matrix"]](n_indices, 7)
  )
  gmacs_control[["f_controls"]] <- stream[["take_matrix"]](n_fleets, 12)

  n_compositions <- length(gmacs_data[["compositions"]])
  gmacs_control[["composition_likelihood"]] <- stream[["take"]](n_compositions)
  gmacs_control[["composition_tail"]] <- stream[["take"]](n_compositions)
  gmacs_control[["composition_tail_minimum"]] <-
    stream[["take"]](n_compositions)
  gmacs_control[["composition_series"]] <- stream[["take"]](n_compositions)
  gmacs_control[["composition_catch_like"]] <- stream[["take"]](n_compositions)
  gmacs_control[["composition_lambda"]] <- stream[["take"]](n_compositions)
  gmacs_control[["composition_emphasis"]] <- stream[["take"]](n_compositions)
  gmacs_control[["composition_catchability"]] <-
    stream[["take"]](n_compositions)
  gmacs_control[["log_sample_size_multiplier"]] <- parameter_rows(
    stream[["take_matrix"]](max(gmacs_control[["composition_series"]]), 7)
  )

  gmacs_control[["tag_emphasis"]] <- stream[["take"]]()
  gmacs_control[["catch_emphasis"]] <- stream[["take"]](
    length(gmacs_data[["catch"]])
  )
  gmacs_control[["f_emphasis"]] <- stream[["take_matrix"]](n_fleets, 4)
  penalty_emphasis <- stream[["take"]](13)
  names(penalty_emphasis) <- c(
    "log_f_deviations", "mean_f", "m_deviations", "recruitment_deviations",
    "initial_deviations", "first_difference_deviations",
    "mean_sex_ratio", "molt_probability", "free_selectivity",
    "initial_numbers_at_length", "f_deviations", "f_deviations_other",
    "selectivity_deviations"
  )
  gmacs_control[["penalty_emphasis"]] <- penalty_emphasis
  check_gmacs_input(c(
    "End-of-file marker 9999 in the .ctl" = stream[["take"]]() == 9999
  ))
  gmacs_control
}

# gmacs.par: parameter names and values, in file order. The first line holds
# the objective function value.
read_gmacs_par <- function(path) {
  lines <- readLines(path, warn = FALSE)
  objective <- lines[1] |>
    sub(
      pattern = ".*Objective function value = *([^ ]+).*",
      replacement = "\\1"
    ) |>
    as.numeric()
  lines <- lines[-1]
  header <- grep("^#", lines)
  last <- c(header[-1] - 1, length(lines))
  values <- purrr::map2(header, last, function(first, last) {
    paste(lines[(first + 1):last], collapse = " ") |>
      trimws() |>
      strsplit("\\s+") |>
      unlist() |>
      as.numeric()
  })
  names(values) <- lines[header] |>
    sub(pattern = "^#\\s*", replacement = "") |>
    sub(pattern = ":$", replacement = "")
  list(objective = objective, values = values)
}

# Collapse "name[1]", "name[2]", ... into a list of 1 vector per index, in
# index order. A name with no index returns its vector.
par_vector <- function(par_values, name) {
  key_pattern <- paste0("^", name, "\\[[0-9]+\\]$")
  keys <- grep(key_pattern, names(par_values), value = TRUE)
  if (length(keys) == 0) {
    return(par_values[[name]])
  }
  key_order <- order(as.integer(sub(".*\\[([0-9]+)\\]$", "\\1", keys)))
  par_values[keys[key_order]] |>
    unname()
}
