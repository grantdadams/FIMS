# Fit the R version of GMACS with nlminb inside GMACS's parameter bounds.
#
# GMACS bounds every parameter (ADMB init_bounded_*). Here the bounds are box
# constraints in nlminb, so the fitted values match GMACS when a parameter
# sits on a bound. Parameters GMACS does not estimate (negative phase) are
# fixed with the RTMB map.
#
# Needs read_gmacs.R and gmacs_male_only.R.

# Lower and upper bounds of each parameter in gmacs_parameters(), on the
# scale the parameter is estimated on.
gmacs_bounds <- function(parameters, model_data) {
  n_lengths <- model_data[["n_lengths"]]
  theta <- model_data[["theta"]]
  growth <- model_data[["growth_parameters"]]
  m_parameters <- model_data[["m_parameters"]]
  selectivity <- model_data[["selectivity_parameters"]]
  f_controls <- model_data[["f_controls"]]
  bound_rows <- function(controls, rows) {
    list(
      lower = controls[["lower"]][rows],
      upper = controls[["upper"]][rows]
    )
  }
  # GMACS bounds selectivity and retention parameters on the natural scale
  # and estimates their log.
  log_bound_rows <- function(controls, rows) {
    list(
      lower = log(controls[["lower"]][rows]),
      upper = log(controls[["upper"]][rows])
    )
  }
  repeated_bounds <- function(lower, upper, n) {
    list(lower = rep(lower, n), upper = rep(upper, n))
  }
  fishing_fleets <- seq_along(model_data[["fishing_fleets"]])
  bounds <- list(
    log_mean_recruitment = bound_rows(theta, 3),
    log_initial_numbers_mature = bound_rows(theta, 8 + seq_len(n_lengths)),
    log_initial_numbers_immature = bound_rows(
      theta, 8 + n_lengths + seq_len(n_lengths)
    ),
    growth_alpha = bound_rows(growth, 1),
    growth_beta = bound_rows(growth, 2),
    m_mature = bound_rows(m_parameters, 1),
    m_mature_block = bound_rows(m_parameters, 2:4),
    m_immature_offset = bound_rows(m_parameters, 5),
    m_immature_block = bound_rows(m_parameters, 6:8),
    selectivity_pot = log_bound_rows(selectivity, 1:2),
    selectivity_trawl = log_bound_rows(selectivity, 3:4),
    log_selectivity_nmfs_1982 = log_bound_rows(
      selectivity, 4 + seq_len(n_lengths)
    ),
    log_selectivity_nmfs_1989 = log_bound_rows(
      selectivity, 4 + n_lengths + seq_len(n_lengths)
    ),
    retention_pot = log_bound_rows(selectivity, 4 + 2 * n_lengths + 1:2),
    # Columns 7 to 10 of the F controls are the bounds of log mean F and of
    # the log F deviations.
    log_mean_f = list(
      lower = f_controls[fishing_fleets, 7],
      upper = f_controls[fishing_fleets, 8]
    ),
    log_f_deviations_pot = repeated_bounds(
      f_controls[1, 9], f_controls[1, 10],
      length(parameters[["log_f_deviations_pot"]])
    ),
    log_f_deviations_trawl = repeated_bounds(
      f_controls[2, 9], f_controls[2, 10],
      length(parameters[["log_f_deviations_trawl"]])
    ),
    # GMACS bounds recruitment deviations at -12 and 12.
    recruitment_deviations = repeated_bounds(
      -12, 12, length(parameters[["recruitment_deviations"]])
    )
  )
  if (!identical(names(bounds), names(parameters))) {
    cli::cli_abort("Every parameter needs bounds, in the same order.")
  }
  bounds
}

# Map that fixes the M block values GMACS does not estimate.
gmacs_map <- function(model_data) {
  m_phase <- model_data[["m_parameters"]][["phase"]]
  fixed_factor <- function(estimated) {
    out <- seq_along(estimated)
    out[!estimated] <- NA
    factor(out)
  }
  list(
    m_mature_block = fixed_factor(m_phase[2:4] > 0),
    m_immature_block = fixed_factor(m_phase[6:8] > 0)
  )
}

# Returns the objective, the report of gmacs_objective() at the estimates,
# and the largest gradient among parameters not at a bound.
fit_gmacs <- function(parameters, model_data, constants,
                      options = gmacs_options(),
                      map = gmacs_map(model_data), extra_map = list()) {
  map <- utils::modifyList(map, extra_map)
  objective <- function(parameters) {
    gmacs_objective(parameters, model_data, constants, options)[["total"]]
  }
  obj <- RTMB::MakeADFun(objective, parameters, map = map, silent = TRUE)
  bounds <- gmacs_bounds(parameters, model_data)
  # Keep the bounds of the free values only, in the tape's order.
  free_bounds <- function(name, side) {
    value <- bounds[[name]][[side]]
    if (!is.null(map[[name]])) {
      value <- value[!is.na(map[[name]])]
    }
    value
  }
  free <- purrr::keep(names(parameters), function(name) {
    is.null(map[[name]]) || any(!is.na(map[[name]]))
  })
  lower <- purrr::map(free, free_bounds, side = "lower") |> unlist()
  upper <- purrr::map(free, free_bounds, side = "upper") |> unlist()
  start <- pmin(pmax(obj[["par"]], lower), upper)
  control <- list(eval.max = 10000, iter.max = 10000)
  opt <- stats::nlminb(start, obj[["fn"]], obj[["gr"]],
    lower = lower, upper = upper, control = control
  )
  # A second run from the first solution, like number_of_loops in fit_fims().
  opt <- stats::nlminb(opt[["par"]], obj[["fn"]], obj[["gr"]],
    lower = lower, upper = upper, control = control
  )
  # Newton steps on the parameters that are not at a bound, with the exact
  # AD Hessian, to bring the gradient down to the 1e-6 range.
  inside_bounds <- function(values) {
    values > lower + 1e-6 & values < upper - 1e-6
  }
  for (step in 1:20) {
    inside <- inside_bounds(opt[["par"]])
    gradient <- as.numeric(obj[["gr"]](opt[["par"]]))[inside]
    if (max(abs(gradient)) < 1e-6) break
    hessian <- obj[["he"]](opt[["par"]])[inside, inside, drop = FALSE]
    newton_step <- try(solve(hessian, gradient), silent = TRUE)
    if (inherits(newton_step, "try-error")) break
    candidate <- opt[["par"]]
    candidate[inside] <- candidate[inside] - newton_step
    candidate <- pmin(pmax(candidate, lower), upper)
    value <- obj[["fn"]](candidate)
    if (!is.finite(value) || value > opt[["objective"]] + 1e-8) break
    opt[["par"]] <- candidate
    opt[["objective"]] <- value
  }
  fitted_parameters <- obj[["env"]][["parList"]](opt[["par"]])
  inside <- inside_bounds(opt[["par"]])
  gradient <- as.numeric(obj[["gr"]](opt[["par"]]))
  list(
    objective = opt[["objective"]],
    report = gmacs_objective(fitted_parameters, model_data, constants, options),
    max_gradient = max(abs(gradient[inside]))
  )
}
