# Male-only GMACS snow crab model (26_gmacs_male_only) in R, written so RTMB
# can tape it.
#
# Every formula follows gmacsbase.TPL (GMACS 2.20.34) for the options this
# model uses. The TPL function each piece copies is named in its comment.
# GMACS-specific choices are kept on purpose, so that the objective matches
# GMACS at gmacs.par. Each choice that the FIMS form changes has a switch in
# gmacs_options(), so this R version of GMACS can also give the FIMS form.
#
# Units follow GMACS: numbers in thousands of crabs, weight in metric tons
# per crab, and catch and biomass in thousands of metric tons.
#
# Maturity groups follow the GMACS order (pntr_hmo): 1 = mature,
# 2 = immature.
#
# Needs read_gmacs.R.

# TINY in include/nloglike.h
gmacs_tiny <- 1e-8

# ADMB dnorm(vector, sd): the full negative log density, with 0.5 log(2 pi).
negative_log_dnorm <- function(x, sd) {
  sum(0.5 * log(2 * pi) + log(sd) + 0.5 * (x / sd)^2)
}

# gsm::plogis on the log scale (include/selex.hpp). `sd` is 1 / slope.
log_logistic <- function(x, mean, sd) {
  -log(1 + exp(-(x - mean) / sd))
}

# The index of the first block that holds each year, or 0 for a year outside
# every block. `blocks` is a matrix of first and last years.
block_index <- function(years, blocks) {
  purrr::map_int(years, function(year) {
    in_block <- which(blocks[, 1] <= year & year <= blocks[, 2])
    if (length(in_block) == 0) 0L else in_block[1]
  })
}

# Data for the model from the GMACS .dat and .ctl.
gmacs_model_data <- function(gmacs_data, gmacs_control) {
  years <- gmacs_data[["start_year"]]:gmacs_data[["end_year"]]
  n_lengths <- gmacs_data[["n_lengths"]]
  theta <- gmacs_control[["theta"]]
  # Rows of theta that this model estimates: log mean recruitment and the
  # initial log numbers.
  theta_rows <- c(3, 8 + seq_len(2 * n_lengths))

  # Terminal molt probability at post-molt length: block 0 is row 1 and
  # block b is row b + 1 (calc_mature_probability, iYrsMatureChanges).
  maturity_blocks <- gmacs_control[["time_blocks"]][[
    gmacs_control[["maturity_type"]][2]
  ]]
  terminal_molt_row <- block_index(
    c(years, gmacs_data[["end_year"]] + 1), maturity_blocks
  ) + 1L

  # M blocks: the block group of the mature row of the M controls.
  m_blocks <- gmacs_control[["time_blocks"]][[
    gmacs_control[["m_time_variation"]][1, 6]
  ]]

  # Fishing fleets and the years with F (fhit): years with a catch row.
  catch_rows <- dplyr::bind_rows(gmacs_data[["catch"]])
  fishing_fleets <- sort(unique(catch_rows[["fleet"]]))
  f_years <- purrr::map(fishing_fleets, function(fleet) {
    catch_rows |>
      dplyr::filter(.data[["fleet"]] == .env[["fleet"]]) |>
      dplyr::pull("year") |>
      unique() |>
      sort()
  })
  discard_mortality <- purrr::map(fishing_fleets, function(fleet) {
    catch_rows |>
      dplyr::filter(.data[["fleet"]] == .env[["fleet"]]) |>
      dplyr::pull("discard_mortality") |>
      unique()
  })
  index <- gmacs_data[["index"]]
  catchability <- gmacs_control[["catchability_parameters"]]
  sample_size_multiplier <- gmacs_control[["log_sample_size_multiplier"]]
  check_gmacs_input(c(
    "1 discard mortality per fleet" = all(lengths(discard_mortality) == 1),
    "Catch is in season 2 only" = all(catch_rows[["season"]] == 2),
    "Catch is biomass" = all(catch_rows[["units"]] == 1),
    "Catch multiplier is 1" = all(catch_rows[["multiplier"]] == 1),
    "No effort data" = all(catch_rows[["effort"]] == 0),
    "Survey timing is 0" = all(index[["timing"]] == 0),
    "Survey catchability is fixed at 1" =
      all(catchability[["phase"]] < 0 & catchability[["initial"]] == 1),
    "Index is mature biomass" =
      all(index[["maturity"]] == 1 & index[["units"]] == 1),
    "Length compositions use the robust multinomial (type 2)" =
      all(gmacs_control[["composition_likelihood"]] == 2),
    "No tail compression" = all(gmacs_control[["composition_tail"]] == 0),
    "Each length composition matrix is its own series" = identical(
      as.numeric(gmacs_control[["composition_series"]]),
      as.numeric(seq_along(gmacs_data[["compositions"]]))
    ),
    "Effective sample size multipliers are fixed at 1" = all(
      sample_size_multiplier[["phase"]] < 0 &
        sample_size_multiplier[["initial"]] == 1
    ),
    "Lambdas and emphases are 1" = all(c(
      gmacs_control[["composition_lambda"]],
      gmacs_control[["composition_emphasis"]],
      gmacs_control[["catch_emphasis"]],
      gmacs_control[["catchability_controls"]][, 2:3]
    ) == 1),
    "Only log mean recruitment and initial numbers are estimated in theta" =
      all(theta[theta_rows, "phase"] > 0) &&
        all(theta[-theta_rows, "phase"] <= 0)
  ))

  # Observed length compositions, each row scaled to sum to 1.
  compositions <- purrr::map2(
    gmacs_data[["compositions"]], gmacs_control[["composition_catch_like"]],
    function(composition, catch_like) {
      observed <- composition[["observed"]]
      list(
        info = composition[["info"]],
        observed = observed / rowSums(observed),
        catch_like = catch_like == 1
      )
    }
  )

  list(
    years = years,
    n_years = length(years),
    n_lengths = n_lengths,
    length_edges = gmacs_data[["length_edges"]],
    length_midpoints = gmacs_data[["length_midpoints"]],
    m_season_fraction = gmacs_data[["m_season_fraction"]],
    # Rows: mature, immature.
    weight_at_length = gmacs_control[["weight_at_length"]],
    terminal_molt_probability =
      gmacs_control[["terminal_molt_probability"]][terminal_molt_row, ,
        drop = FALSE
      ],
    m_block = block_index(years, m_blocks),
    recruitment_max_length_class =
      gmacs_control[["recruitment_max_length_class"]],
    fishing_fleets = fishing_fleets,
    f_years = f_years,
    discard_mortality = unlist(discard_mortality),
    retention_flag = gmacs_control[["retention_flag"]],
    catch = gmacs_data[["catch"]],
    index = index,
    compositions = compositions,
    growth = gmacs_data[["growth"]],
    theta = theta,
    theta_rows = theta_rows,
    growth_parameters = gmacs_control[["growth_parameters"]],
    m_parameters = gmacs_control[["m_parameters"]],
    selectivity_parameters = gmacs_control[["selectivity_parameters"]],
    penalty_emphasis = gmacs_control[["penalty_emphasis"]],
    f_emphasis = gmacs_control[["f_emphasis"]],
    f_controls = gmacs_control[["f_controls"]]
  )
}

# Estimated parameters (GMACS parameterization) from gmacs.par values. Each
# comment gives the GMACS name or the meaning.
gmacs_parameters <- function(par_values, model_data) {
  theta <- par_vector(par_values, "T_pars_est") |> unlist()
  growth <- par_vector(par_values, "G_pars_est") |> unlist()
  m_values <- par_vector(par_values, "M_pars_est") |> unlist()
  selectivity <- par_vector(par_values, "S_pars_est") |> unlist()
  n_lengths <- model_data[["n_lengths"]]
  log_f_deviations <- par_vector(par_values, "log_fdev")
  log_mean_f <- par_vector(par_values, "log_fbar") |> unlist()
  list(
    # logRbar
    log_mean_recruitment = theta[3],
    # logN0 by maturity group
    log_initial_numbers_mature = theta[8 + seq_len(n_lengths)],
    log_initial_numbers_immature = theta[8 + n_lengths + seq_len(n_lengths)],
    # Mean molt increment alpha - beta * premolt length
    growth_alpha = growth[1],
    growth_beta = growth[2],
    # M0, its block offsets, and the immature offset and its block offsets
    m_mature = m_values[1],
    m_mature_block = m_values[2:4],
    m_immature_offset = m_values[5],
    m_immature_block = m_values[6:8],
    # Logistic: log inflection point and log scale (1 / slope)
    selectivity_pot = selectivity[1:2],
    selectivity_trawl = selectivity[3:4],
    # Per-class log selectivity of the 2 NMFS survey eras
    log_selectivity_nmfs_1982 = selectivity[4 + seq_len(n_lengths)],
    log_selectivity_nmfs_1989 = selectivity[4 + n_lengths + seq_len(n_lengths)],
    # Logistic: log inflection point and log scale (1 / slope)
    retention_pot = selectivity[4 + 2 * n_lengths + 1:2],
    # log_fbar and log_fdev
    log_mean_f = log_mean_f[seq_along(model_data[["fishing_fleets"]])],
    log_f_deviations_pot = log_f_deviations[[1]],
    log_f_deviations_trawl = log_f_deviations[[2]],
    # rec_dev_est
    recruitment_deviations = par_values[["rec_dev_est"]]
  )
}

# Values that GMACS holds constant in this model.
gmacs_constants <- function(par_values) {
  theta <- par_vector(par_values, "T_pars_est") |> unlist()
  growth <- par_vector(par_values, "G_pars_est") |> unlist()
  list(
    # ra and rb: the recruitment length distribution is a gamma with mean ra
    # and scale rb.
    recruitment_length_mean = theta[4],
    recruitment_length_scale = theta[5],
    # log(sigmaR)
    log_recruitment_sd = theta[6],
    recruitment_rho = theta[8],
    # Scale of the gamma postmolt length distribution
    growth_scale = growth[3],
    log_additional_cv = par_vector(par_values, "log_add_cv") |> unlist()
  )
}

# Switches for the GMACS-specific choices. The defaults are the GMACS forms.
# Each other value is the FIMS form of that choice.
gmacs_options <- function(...) {
  defaults <- list(
    # TRUE adds 0.5 log(2 pi) per index point
    index_constant = FALSE,
    # FALSE drops the additional survey CV from the index sd
    index_additional_cv = TRUE,
    # FALSE drops + sigma_R^2 / 2
    recruitment_bias_correction = TRUE,
    # FALSE drops the log(upper - lower) constants
    uniform_priors = TRUE,
    # FALSE leaves logistic selectivity unscaled
    normalize_logistic = TRUE,
    # FALSE: Baranov catch with F and the Z of each maturity group
    gmacs_catch_compositions = TRUE,
    # FALSE drops the penalty on mean(log F deviations)^2
    mean_f_deviation_penalty = TRUE,
    # FALSE drops dnorm(diff(recruitment deviations), 1)
    recruitment_first_difference = TRUE,
    # Or "logit": smooth logit selectivity
    smoothing_scale = "log"
  )
  changes <- list(...)
  unknown <- setdiff(names(changes), names(defaults))
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown option{?s} {.val {unknown}}.")
  }
  utils::modifyList(defaults, changes)
}

# The objective function and its parts.
gmacs_objective <- function(parameters, model_data, constants,
                            options = gmacs_options()) {
  # RTMB needs these overloads to assign AD values into plain vectors.
  "[<-" <- RTMB::ADoverload("[<-")
  "c" <- RTMB::ADoverload("c")
  n_lengths <- model_data[["n_lengths"]]
  n_years <- model_data[["n_years"]]
  years <- model_data[["years"]]
  length_midpoints <- model_data[["length_midpoints"]]
  length_edges <- model_data[["length_edges"]]
  m_season_fraction <- model_data[["m_season_fraction"]]
  weight_at_length <- model_data[["weight_at_length"]]
  terminal_molt_probability <- model_data[["terminal_molt_probability"]]
  fishing_fleets <- model_data[["fishing_fleets"]]
  f_years <- model_data[["f_years"]]
  # Survey years can include the year after the last model year.
  survey_years <- c(years, max(years) + 1)

  # Selectivity (calc_selectivities) ----------------------------------------
  # Logistic: log(plogis(L, exp(p1), exp(p2))), forced to 1 in the last
  # class by subtracting its log value. Per-class (NMFS): the parameter is
  # log selectivity, with no rescaling.
  log_selectivity_pot <- log_logistic(
    length_midpoints,
    exp(parameters[["selectivity_pot"]][1]),
    exp(parameters[["selectivity_pot"]][2])
  )
  log_selectivity_trawl <- log_logistic(
    length_midpoints,
    exp(parameters[["selectivity_trawl"]][1]),
    exp(parameters[["selectivity_trawl"]][2])
  )
  if (options[["normalize_logistic"]]) {
    log_selectivity_pot <- log_selectivity_pot - log_selectivity_pot[n_lengths]
    log_selectivity_trawl <- log_selectivity_trawl -
      log_selectivity_trawl[n_lengths]
  }
  log_selectivity_survey <- list(
    parameters[["log_selectivity_nmfs_1982"]],
    parameters[["log_selectivity_nmfs_1989"]]
  )
  # Retention: logistic, not rescaled. The trawl fleet has "uniform 0"
  # retention: log retention -100 and discard fraction 1 - TINY.
  log_retention_pot <- log_logistic(
    length_midpoints,
    exp(parameters[["retention_pot"]][1]),
    exp(parameters[["retention_pot"]][2])
  )
  log_selectivity <- list(log_selectivity_pot, log_selectivity_trawl)
  log_retention <- list(log_retention_pot, rep(-100, n_lengths))
  discard_selectivity <- list(
    1 - exp(log_retention_pot) + gmacs_tiny,
    rep(1 - gmacs_tiny, n_lengths)
  )
  # GMACS smooths log selectivity. On the logit scale (FIMS LengthSpecific)
  # the same penalty is on log(s / (1 - s)) = log s - log(1 - s).
  smoothed_value <- function(log_selectivity_at_length) {
    if (options[["smoothing_scale"]] == "logit") {
      log_selectivity_at_length - log(1 - exp(log_selectivity_at_length))
    } else {
      log_selectivity_at_length
    }
  }
  selectivity_smoothness <-
    negative_log_dnorm(diff(smoothed_value(log_selectivity_survey[[1]])), 1) +
    negative_log_dnorm(diff(smoothed_value(log_selectivity_survey[[2]])), 1)

  # Fishing mortality (calc_fishing_mortality) ------------------------------
  # F by year and class in season 2, summed over fleets. Vulnerability adds
  # 1e-10 to capture selectivity as GMACS does.
  log_f_deviations <- list(
    parameters[["log_f_deviations_pot"]],
    parameters[["log_f_deviations_trawl"]]
  )
  f_by_fleet <- list()
  fishing_mortality <- matrix(0, n_years, n_lengths)
  for (fleet_index in seq_along(fishing_fleets)) {
    f_by_year <- rep(0, n_years)
    f_rows <- match(f_years[[fleet_index]], years)
    f_by_year[f_rows] <- exp(
      parameters[["log_mean_f"]][fleet_index] + log_f_deviations[[fleet_index]]
    )
    f_by_fleet[[fleet_index]] <- f_by_year
    capture <- exp(log_selectivity[[fleet_index]]) + 1e-10
    retention <- exp(log_retention[[fleet_index]]) *
      model_data[["retention_flag"]][fleet_index]
    vulnerability <- capture * (retention + (1 - retention) *
      model_data[["discard_mortality"]][fleet_index])
    # Row by row: outer() drops the RTMB AD class.
    for (year_index in seq_len(n_years)) {
      fishing_mortality[year_index, ] <- fishing_mortality[year_index, ] +
        f_by_year[year_index] * vulnerability
    }
  }

  # Natural mortality (timevarparM, block_fn = 1) ---------------------------
  # Immature M is mature base M times exp(offset + immature block), not the
  # mature block value.
  block_value <- function(block_offsets) {
    out <- rep(0, n_years)
    in_block <- which(model_data[["m_block"]] > 0)
    out[in_block] <- block_offsets[model_data[["m_block"]][in_block]]
    out
  }
  m_mature <- parameters[["m_mature"]] *
    exp(block_value(parameters[["m_mature_block"]]))
  m_immature <- parameters[["m_mature"]] * exp(
    parameters[["m_immature_offset"]] +
      block_value(parameters[["m_immature_block"]])
  )
  m_by_group <- list(m_mature, m_immature)

  # Total mortality by group, season, year and class (calc_total_mortality).
  total_mortality <- purrr::map(m_by_group, function(m_by_year) {
    purrr::map(seq_along(m_season_fraction), function(season) {
      out <- matrix(0, n_years, n_lengths)
      for (year_index in seq_len(n_years)) {
        out[year_index, ] <- m_season_fraction[season] * m_by_year[year_index]
      }
      if (season == 2) {
        out <- out + fishing_mortality
      }
      out
    })
  })

  # Growth transition (calc_growth_transition, GROWTH_SIZEGAMMA) ------------
  # Row l: gamma postmolt length with shape (L_l + increment_l) / scale over
  # the edges at and above l. Mass below edge l and above the last edge is
  # dropped, and the row is rescaled to sum to 1.
  increment <- parameters[["growth_alpha"]] -
    parameters[["growth_beta"]] * length_midpoints
  growth_shape <- (length_midpoints + increment) / constants[["growth_scale"]]
  scaled_edges <- length_edges / constants[["growth_scale"]]
  growth_matrix <- matrix(0, n_lengths, n_lengths)
  for (premolt in seq_len(n_lengths)) {
    cumulative <- RTMB::pgamma(
      scaled_edges[premolt:(n_lengths + 1)],
      shape = growth_shape[premolt]
    )
    probability <- diff(cumulative)
    growth_matrix[premolt, premolt:n_lengths] <- probability / sum(probability)
  }

  # Recruitment (calc_recruitment_size_distribution) ------------------------
  recruitment_shape <- constants[["recruitment_length_mean"]] /
    constants[["recruitment_length_scale"]]
  recruitment_size <- diff(RTMB::pgamma(
    length_edges / constants[["recruitment_length_scale"]],
    shape = recruitment_shape
  ))
  recruitment_size[
    (model_data[["recruitment_max_length_class"]] + 1):n_lengths
  ] <- 0
  recruitment_size <- recruitment_size / sum(recruitment_size)
  recruits <- exp(
    parameters[["log_mean_recruitment"]] +
      parameters[["recruitment_deviations"]]
  )

  # Numbers at length by group, season and year -----------------------------
  # (update_population_numbers_at_length_season). Survival first. In season
  # 3 immature crabs molt (probability 1), the share given by the terminal
  # molt probability at postmolt length becomes mature, and recruits join
  # the immature group. Mature crabs only survive.
  numbers <- purrr::map(1:2, function(group) {
    purrr::map(
      seq_along(m_season_fraction),
      function(season) matrix(0, n_years + 1, n_lengths)
    )
  })
  current <- list(
    exp(parameters[["log_initial_numbers_mature"]]),
    exp(parameters[["log_initial_numbers_immature"]])
  )
  for (year_index in seq_len(n_years)) {
    for (season in seq_along(m_season_fraction)) {
      for (group in 1:2) {
        numbers[[group]][[season]][year_index, ] <- current[[group]]
      }
      survived <- purrr::map(1:2, function(group) {
        current[[group]] *
          exp(-total_mortality[[group]][[season]][year_index, ])
      })
      if (season == 3) {
        molted <- as.vector(survived[[2]] %*% growth_matrix)
        molt_probability <- terminal_molt_probability[year_index, ]
        maturing <- molted * molt_probability
        survived[[2]] <- molted * (1 - molt_probability) +
          recruits[year_index] * recruitment_size
        survived[[1]] <- survived[[1]] + maturing
      }
      current <- survived
    }
  }
  for (group in 1:2) {
    numbers[[group]][[1]][n_years + 1, ] <- current[[group]]
  }

  # Expected catch (calc_predicted_catch) -----------------------------------
  # Baranov per group with that group's Z, times weight. Retained, discarded
  # (all discards, alive or dead) or total by catch type.
  expected_catch <- function(fleet, year, catch_type) {
    fleet_index <- match(fleet, fishing_fleets)
    year_index <- match(year, years)
    capture <- exp(log_selectivity[[fleet_index]])
    capture <- switch(as.character(catch_type),
      "1" = exp(log_selectivity[[fleet_index]] + log_retention[[fleet_index]]),
      "2" = capture * (1 - exp(log_retention[[fleet_index]])),
      "0" = capture
    )
    total <- 0
    for (group in 1:2) {
      z_at_length <- total_mortality[[group]][[2]][year_index, ]
      total <- total + sum(
        numbers[[group]][[2]][year_index, ] * weight_at_length[group, ] *
          f_by_fleet[[fleet_index]][year_index] * capture *
          (1 - exp(-z_at_length)) / z_at_length
      )
    }
    total
  }
  catch_negative_log_likelihood <- numeric(length(model_data[["catch"]]))
  catch_expected <- list()
  for (series in seq_along(model_data[["catch"]])) {
    catch_rows <- model_data[["catch"]][[series]]
    expected <- numeric(nrow(catch_rows))
    for (row in seq_len(nrow(catch_rows))) {
      expected[row] <- expected_catch(
        catch_rows[["fleet"]][row], catch_rows[["year"]][row],
        catch_rows[["type"]][row]
      )
    }
    catch_expected[[series]] <- expected
    catch_sd <- sqrt(log(1 + catch_rows[["cv"]]^2))
    catch_negative_log_likelihood[series] <- negative_log_dnorm(
      log(catch_rows[["observed"]]) - log(expected), catch_sd
    )
  }

  # Survey index (calc_relative_abundance, index_likelihood) ----------------
  # Mature biomass at the start of season 1, q = 1. The likelihood leaves out
  # 0.5 log(2 pi) (index_constant adds it).
  index <- model_data[["index"]]
  survey_fleets <- sort(unique(index[["fleet"]]))
  index_expected <- numeric(nrow(index))
  for (row in seq_len(nrow(index))) {
    year_index <- match(index[["year"]][row], survey_years)
    survey <- match(index[["fleet"]][row], survey_fleets)
    index_expected[row] <- sum(
      numbers[[1]][[1]][year_index, ] * weight_at_length[1, ] *
        exp(log_selectivity_survey[[survey]])
    )
  }
  additional_cv <- exp(constants[["log_additional_cv"]][
    index[["catchability_index"]]
  ])
  index_sd <- sqrt(log(1 + index[["cv"]]^2) +
    options[["index_additional_cv"]] * log(1 + additional_cv^2))
  index_residual <- log(index[["observed"]]) - log(index_expected)
  index_terms <- log(index_sd) + 0.5 * (index_residual / index_sd)^2 +
    options[["index_constant"]] * 0.5 * log(2 * pi)
  index_series <- sort(unique(index[["index_series"]]))
  index_negative_log_likelihood <- numeric(length(index_series))
  for (series in index_series) {
    index_negative_log_likelihood[series] <- sum(
      index_terms[index[["index_series"]] == series]
    )
  }

  # Length compositions (calc_predicted_composition, robust_multi) ----------
  # Catch-like compositions use (1 - exp(-Z)) / Z of the mature group for
  # both groups and leave out F. Survey-like compositions are N times
  # selectivity. The likelihood is the robust normal approximation to the
  # multinomial, with TINY = 1e-8.
  composition_negative_log_likelihood <- numeric(
    length(model_data[["compositions"]])
  )
  composition_expected <- list()
  for (series in seq_along(model_data[["compositions"]])) {
    composition <- model_data[["compositions"]][[series]]
    info <- composition[["info"]]
    negative_log_likelihood <- 0
    expected_rows <- list()
    for (row in seq_len(nrow(info))) {
      year_index <- match(info[["year"]][row], survey_years)
      season <- info[["season"]][row]
      fleet <- info[["fleet"]][row]
      if (fleet %in% fishing_fleets) {
        fleet_index <- match(fleet, fishing_fleets)
        capture <- exp(log_selectivity[[fleet_index]])
        capture <- switch(as.character(info[["type"]][row]),
          "1" = capture * exp(log_retention[[fleet_index]]),
          "2" = capture * discard_selectivity[[fleet_index]],
          "0" = capture
        )
      } else {
        capture <- exp(log_selectivity_survey[[match(fleet, survey_fleets)]])
      }
      # With F in the prediction, a catch composition from a year with no
      # fishing has no expected catch. GMACS keeps those rows (retained
      # compositions in 2022 and 2023) because it leaves F out.
      if (composition[["catch_like"]] &&
        !options[["gmacs_catch_compositions"]] &&
        !(info[["year"]][row] %in% f_years[[fleet_index]])) {
        next
      }
      maturity <- info[["maturity"]][row]
      groups <- if (maturity == 0) 1:2 else maturity
      expected <- rep(0, n_lengths)
      for (group in groups) {
        if (!composition[["catch_like"]]) {
          timing <- 1
        } else if (options[["gmacs_catch_compositions"]]) {
          z_mature <- total_mortality[[1]][[season]][year_index, ] + 1e-10
          timing <- (1 - exp(-z_mature)) / z_mature
        } else {
          # Baranov catch at length: each group's own Z, times F.
          z_at_length <- total_mortality[[group]][[season]][year_index, ]
          timing <- f_by_fleet[[fleet_index]][year_index] *
            (1 - exp(-z_at_length)) / z_at_length
        }
        expected <- expected +
          numbers[[group]][[season]][year_index, ] * capture * timing
      }
      expected <- expected / sum(expected)
      expected_rows[[row]] <- expected
      sample_size <- info[["sample_size"]][row]
      # acl::robust_multi::pdf
      observed <- composition[["observed"]][row, ] + gmacs_tiny
      observed <- observed / sum(observed)
      expected_tiny <- expected + gmacs_tiny
      expected_tiny <- expected_tiny / sum(expected_tiny)
      variance <- 0.1 / n_lengths + observed * (1 - observed)
      residual_term <- 0.5 * (expected_tiny - observed)^2 / variance
      negative_log_likelihood <- negative_log_likelihood -
        sum(log(exp(-sample_size * residual_term) + 0.01)) +
        0.5 * sum(log(variance / sample_size))
    }
    composition_expected[[series]] <- expected_rows
    composition_negative_log_likelihood[series] <- negative_log_likelihood
  }

  # Recruitment (calc_stock_recruitment_relationship, recruitment_likelihood)
  # Residual: dev_y - rho dev_(y-1) + sigma_R^2 / 2, with full bias
  # correction in every model year.
  recruitment_deviations <- parameters[["recruitment_deviations"]]
  recruitment_sd <- exp(constants[["log_recruitment_sd"]])
  recruitment_residual <- recruitment_deviations
  recruitment_residual[-1] <- recruitment_deviations[-1] -
    constants[["recruitment_rho"]] * recruitment_deviations[-n_years]
  recruitment_residual <- recruitment_residual +
    options[["recruitment_bias_correction"]] * 0.5 * recruitment_sd^2
  recruitment_negative_log_likelihood <- negative_log_dnorm(
    recruitment_residual, recruitment_sd
  )

  # Growth increments (growth_likelihood) -----------------------------------
  growth <- model_data[["growth"]]
  expected_increment <- parameters[["growth_alpha"]] -
    parameters[["growth_beta"]] * growth[["premolt_length"]]
  growth_negative_log_likelihood <- negative_log_dnorm(
    log(growth[["increment"]]) - log(expected_increment),
    growth[["cv"]]
  )

  # Penalties (calc_objective_function) -------------------------------------
  # 1: mean log F deviation, 6: first differences of recruitment deviations,
  # 9: selectivity smoothness, 10: first differences of initial log N. The
  # product with an AD parameter makes the vector an AD vector.
  penalty <- rep(0, 13) * parameters[["log_mean_recruitment"]]
  for (fleet_index in seq_along(fishing_fleets)) {
    penalty[1] <- penalty[1] + model_data[["f_emphasis"]][fleet_index, 1] *
      mean(log_f_deviations[[fleet_index]])^2
  }
  penalty[6] <- negative_log_dnorm(diff(recruitment_deviations), 1)
  penalty[9] <- selectivity_smoothness
  penalty[10] <- negative_log_dnorm(
    diff(parameters[["log_initial_numbers_mature"]]), 1
  ) + negative_log_dnorm(diff(parameters[["log_initial_numbers_immature"]]), 1)
  if (!options[["mean_f_deviation_penalty"]]) {
    penalty[1] <- 0
  }
  if (!options[["recruitment_first_difference"]]) {
    penalty[6] <- 0
  }
  penalty_weighted <- penalty * model_data[["penalty_emphasis"]]

  # Priors (calc_prior_densities) -------------------------------------------
  # Uniform: log(upper - lower). Normal: full dnorm. Selectivity priors are on
  # exp(parameter).
  prior <- function(value, controls) {
    out <- 0
    for (row in seq_len(nrow(controls))) {
      prior_type <- controls[["prior_type"]][row]
      prior_1 <- controls[["prior_1"]][row]
      prior_2 <- controls[["prior_2"]][row]
      if (prior_type == 0) {
        if (options[["uniform_priors"]]) {
          out <- out + log(controls[["upper"]][row] - controls[["lower"]][row])
        }
      } else if (prior_type == 1) {
        out <- out + negative_log_dnorm(value[row] - prior_1, prior_2)
      } else {
        cli::cli_abort(
          "Prior type {prior_type} is not covered by the R version of GMACS."
        )
      }
    }
    out
  }
  theta_value <- c(
    parameters[["log_mean_recruitment"]],
    parameters[["log_initial_numbers_mature"]],
    parameters[["log_initial_numbers_immature"]]
  )
  m_value <- c(
    parameters[["m_mature"]], parameters[["m_mature_block"]],
    parameters[["m_immature_offset"]], parameters[["m_immature_block"]]
  )
  m_active <- model_data[["m_parameters"]][["phase"]] > 0
  selectivity_value <- exp(c(
    parameters[["selectivity_pot"]], parameters[["selectivity_trawl"]],
    parameters[["log_selectivity_nmfs_1982"]],
    parameters[["log_selectivity_nmfs_1989"]], parameters[["retention_pot"]]
  ))
  growth_active <- model_data[["growth_parameters"]][["phase"]] > 0
  theta_controls <- model_data[["theta"]][model_data[["theta_rows"]], ]
  priors <- prior(theta_value, theta_controls) +
    prior(
      c(parameters[["growth_alpha"]], parameters[["growth_beta"]])[
        growth_active[1:2]
      ],
      model_data[["growth_parameters"]][growth_active, ]
    ) +
    prior(m_value[m_active], model_data[["m_parameters"]][m_active, ]) +
    prior(selectivity_value, model_data[["selectivity_parameters"]])

  total <- sum(catch_negative_log_likelihood) +
    sum(index_negative_log_likelihood) +
    sum(composition_negative_log_likelihood) +
    recruitment_negative_log_likelihood + growth_negative_log_likelihood +
    sum(penalty_weighted) + priors

  list(
    total = total,
    components = list(
      catch = catch_negative_log_likelihood,
      index = index_negative_log_likelihood,
      compositions = composition_negative_log_likelihood,
      recruitment = recruitment_negative_log_likelihood,
      growth = growth_negative_log_likelihood,
      penalties = penalty,
      penalties_weighted = penalty_weighted,
      priors = priors
    ),
    quantities = list(
      # Mature male biomass at the start of season 3 (season_ssb = 3).
      mature_male_biomass = as.vector(
        numbers[[1]][[3]][seq_len(n_years), ] %*% weight_at_length[1, ]
      ),
      numbers = numbers,
      growth_matrix = growth_matrix,
      recruitment_size = recruitment_size,
      recruits = recruits,
      m_mature = m_mature,
      m_immature = m_immature,
      f_by_fleet = f_by_fleet,
      log_selectivity = log_selectivity,
      log_retention = log_retention,
      log_selectivity_survey = log_selectivity_survey,
      catch_expected = catch_expected,
      index_expected = index_expected,
      composition_expected = composition_expected
    )
  )
}
