# add_process() and add_priors(): time-varying parameters, time blocks,
# processes and priors, written into the parameter tibble. The formula forms
# are listed in the add_process() help page.
#
# The helpers only write rows. initialize_fims() builds the map and the
# densities from the tibble, so a hand-edited tibble works the same way.
# Columns used: map_group (rows with the same integer share 1 estimate),
# re_structure (iid, ar1, rw on the target rows) and target_label (on the
# hyperparameter rows expected_mean, log_sd and logit_rho).
#
# Estimation status: "not_estimated" is accepted as another name for
# "constant", and `estimation_status` for `estimation_type`. Both are
# written as estimation_type.

#' Add blocks or a process to 1 parameter
#'
#' Rules:
#' 1. The left side of the formula is the target `label`, keyed by
#'    `module_name` and `fleet`.
#' 2. 1 call per target. A second call is an error.
#' 3. Formula variables are the target's index columns: `year` (from
#'    `timing`) and `age`.
#' 4. With no random term, the fixed term says which rows share 1 estimate.
#'    With a random term, it defines the `expected_mean` the process values
#'    vary around, and each non-constant row is its own process value.
#' 5. `estimation_type` of the target's rows, or of its non-constant rows
#'    when some are constant: "random_effects" (integrated, the default with
#'    a random term) or "fixed_effects" (penalized).
#' 6. `sd` and `rho` are lists with `value` and `estimation_type`.
#' 7. For `rw()`, the first estimated row (usually the first year) is a fixed
#'    effect and there is no `expected_mean`.
#' 8. Rows that share 1 estimate start from the mean of their values. With a
#'    random term, each `expected_mean` starts from the mean of the estimated
#'    values in its block, and the process values keep their own starting
#'    values.
#' 9. A constant row is not part of the process. The process keeps it
#'    constant and leaves it out of its density, whatever its
#'    `distribution_type`, e.g., `log_Fmort` at -200 in a year with no catch.
#'    An AR1 process or random walk steps over it. Across k years the random
#'    walk SD is sd * sqrt(k), and the AR1 correlation is rho^k with
#'    conditional SD sd * sqrt((1 - rho^(2k)) / (1 - rho^2)). k is the
#'    difference of the years, so years that are not consecutive also count
#'    as k steps. Blocks with no random term cannot mix constant and
#'    estimated rows. A prior ([add_priors()]) is different: it still scores
#'    a constant value. A known value with a prior is still informative, and
#'    a likelihood profile, which holds the profiled value constant, keeps the
#'    prior. A constant is not a process value, so a process steps over it.
#'    Holding a process value constant to profile it therefore leaves it out
#'    of the process.
#'
#' @details
#' The formula describes 1 parameter, the target. Its right side has a fixed
#' term, a random term, or both:
#' - `log_q ~ cut(year, c(-Inf, 10, 20, Inf))`: 3 time blocks.
#' - `inflection_point ~ cut(year, c(-Inf, 15, Inf)) + ar1(1 | year)`: an AR1
#'   process around 2 block means.
#' - `log_q ~ 1 + (1 | year)`: an iid process around 1 mean.
#' - `log_M ~ cut(age, c(-Inf, 1, Inf)) + (1 | age:year)`: an iid process by
#'   age and year.
#' - `log_q ~ rw(1 | year)`: a random walk.
#'
#' These breaks fit data with years 1 to 30, as in `data_big`. When the data
#' use calendar years, `year` is a calendar year, e.g.,
#' `cut(year, c(-Inf, 2000, Inf))`.
#'
#' @param parameters The parameter tibble.
#' @param formula `target ~ fixed term + random term`. See Details for the
#'   forms.
#' @param module_name,fleet Which module the target belongs to.
#' @param estimation_type,estimation_status Estimation type of the target's
#'   rows when there is a random term, or of its non-constant rows when some
#'   are constant.
#' @param sd,rho Starting value and estimation type of the process SD (on the
#'   natural scale, > 0) and AR1 correlation (between -1 and 1).
#' @return The parameter tibble with the target's rows replaced and the
#'   process's hyperparameter rows added.
#' @keywords setup_default_parameters
#' @export
#' @examples
#' \dontrun{
#' parameters <- setup_default_parameters(FIMSFrame(data_big))
#' # An AR1 process on survey catchability
#' parameters |>
#'   add_process(log_q ~ 1 + ar1(1 | year),
#'     module_name = "Fleet", fleet = "survey1"
#'   )
#' # Fishing mortality in 2 time blocks
#' parameters |>
#'   add_process(log_Fmort ~ cut(year, c(-Inf, 15, Inf)),
#'     module_name = "Fleet", fleet = "fleet1"
#'   )
#' }
add_process <- function(parameters,
                        formula,
                        module_name,
                        fleet = NA_character_,
                        estimation_type = "random_effects",
                        estimation_status = NULL,
                        sd = list(value = 1, estimation_type = "fixed_effects"),
                        rho = list(value = 0, estimation_type = "fixed_effects")) {
  if (!is.null(estimation_status)) {
    estimation_type <- estimation_status
  }
  estimation_type <- standard_estimation_type(estimation_type)
  sd[["estimation_type"]] <- hyperparameter_estimation_type(sd)
  rho[["estimation_type"]] <- hyperparameter_estimation_type(rho)
  parameters <- add_process_columns(parameters)
  process <- check_process(
    parameters, formula, module_name, fleet, estimation_type, sd, rho
  )
  write_process(parameters, process, estimation_type, sd, rho)
}

#' Check a process request and find the rows it writes
#'
#' Stops with a message for any request the model cannot fit. The checks
#' need the target's rows by year and the time block of each row, so this
#' also returns them for write_process().
#'
#' @inheritParams add_process
#' @return A list with the `target` label, `is_target` (its rows in
#'   `parameters`), the parsed `formula_terms`, the target's `rows` (1 per
#'   year when the formula uses year) and the `group` (time block) of each
#'   row.
#' @noRd
check_process <- function(parameters, formula, module_name, fleet,
                          estimation_type, sd, rho) {
  formula_terms <- parse_process_formula(formula)
  # The sd is written as log(sd) and rho as logit_rho, which are finite only
  # for an sd above 0 and a rho between -1 and 1. They are checked only when
  # the formula uses them.
  random_term <- formula_terms[["random"]]
  if (!is.null(random_term) &&
    (!is_single_number(sd[["value"]]) || sd[["value"]] <= 0)) {
    cli::cli_abort(c(
      "x" = "The process sd is {.val {sd[['value']]}}.",
      "i" = "The sd must be 1 number > 0, because it is written as log_sd.",
      "i" = "Give it as {.code sd = list(value = 0.5)}."
    ))
  }
  if (identical(random_term[["structure"]], "ar1") &&
    (!is_single_number(rho[["value"]]) || abs(rho[["value"]]) >= 1)) {
    cli::cli_abort(c(
      "x" = "The process rho is {.val {rho[['value']]}}.",
      "i" = "rho must be 1 number between -1 and 1, because it is written
      as logit_rho.",
      "i" = "Give it as {.code rho = list(value = 0.5)}."
    ))
  }
  target <- formula_terms[["target"]]
  is_target <- target_rows(parameters, target, module_name, fleet)
  old_rows <- dplyr::filter(parameters, is_target)
  check_new_target(old_rows, target)

  # Some values constant and some estimated (e.g., log_Fmort in years with no
  # catch). Blocks would estimate the constant ones. A process keeps them
  # constant (see process_densities()). When every value is constant (e.g.,
  # the default log_M), the call is a request to estimate them.
  if (is.null(random_term) &&
    any(old_rows[["estimation_type"]] == "constant") &&
    any(old_rows[["estimation_type"]] != "constant")) {
    cli::cli_abort(c(
      "x" = "{.val {target}} has some constant values and some estimated.",
      "i" = "Blocks would estimate the constant values. A process (a random
      term) keeps them constant.",
      "i" = "Count the values by estimation type with
      {.code dplyr::filter(parameters, label == \"{target}\") |>
      dplyr::count(fleet, estimation_type)}."
    ))
  }
  if (has_prior(parameters, target, module_name, unique(old_rows[["fleet"]]))) {
    cli::cli_abort(c(
      "x" = "{.val {target}} has a prior, so it cannot also take a process.",
      "i" = "Add the process first, then put priors on its parameters, e.g.,
      {.code {target}_log_sd ~ normal(...)}."
    ))
  }
  variables <- c(formula_terms[["fixed_variables"]], formula_terms[["random_variables"]])
  uses_age <- "age" %in% variables
  # A formula in age only (e.g., age blocks on log_init_naa) keeps the rows
  # as they are. A formula in year needs 1 row per year.
  rows <- old_rows
  if ("year" %in% variables) {
    if (!can_vary_by_year(target, module_name)) {
      cli::cli_abort(c(
        "x" = "{.val {target}} is not indexed by year in the model.",
        "i" = "It can take blocks or a process by age only, e.g.,
        {.code {target} ~ cut(age, c(-Inf, 5, Inf))}."
      ))
    }
    rows <- expand_by_year(old_rows, all_years(parameters), keep_age = uses_age)
  }
  group <- evaluate_fixed_term(
    formula_terms[["fixed"]], rows, environment(formula)
  )
  if (!uses_age && all(!is.na(rows[["age"]])) && length(unique(group)) > 1) {
    cli::cli_inform(c(
      "i" = "{.val {target}} varies by age. The formula has no age term, so
      each block shares 1 value across ages."
    ))
  }

  process <- list(
    target = target, is_target = is_target, formula_terms = formula_terms,
    rows = rows, group = group
  )
  if (is.null(formula_terms[["random"]])) {
    return(process)
  }
  process_structure <- formula_terms[["random"]][["structure"]]
  random_grouping <- formula_terms[["random"]][["grouping"]]
  has_age <- all(!is.na(rows[["age"]]))
  if (random_grouping == "year" && has_age) {
    cli::cli_abort(c(
      "x" = "{.code (1 | year)} on {.val {target}}, which varies by age and
      year, would share 1 random effect across ages.",
      "i" = "Use {.code (1 | age:year)}."
    ))
  }
  if (random_grouping == "age:year" && !has_age) {
    cli::cli_abort(c(
      "x" = "{.val {target}} does not vary by age.",
      "i" = "Use {.code (1 | year)}."
    ))
  }
  if (process_structure == "rw" && length(unique(group)) > 1) {
    cli::cli_abort(c(
      "x" = "A random walk has no mean, so it cannot have fixed blocks.",
      "i" = "Use {.code {target} ~ rw(1 | year)} or a mean with an AR1 or iid process."
    ))
  }
  # The density leaves constant values out, so a process needs at least 2
  # estimated values, and a block of only constant values would have a mean
  # that nothing informs.
  is_constant <- rows[["estimation_type"]] == "constant"
  n_estimated <- if (all(is_constant)) nrow(rows) else sum(!is_constant)
  if (n_estimated < 2) {
    cli::cli_abort(c(
      "x" = "{.val {target}} has {.val {n_estimated}} estimated
      {cli::qty(n_estimated)}value{?s}.",
      "i" = "A process leaves constant values out of its density, so it needs
      at least 2 estimated values.",
      "i" = "Count the values by estimation type with
      {.code dplyr::filter(parameters, label == \"{target}\") |>
      dplyr::count(fleet, estimation_type)}."
    ))
  }
  if (process_structure != "rw" && any(!is_constant) &&
    !all(group %in% group[!is_constant])) {
    cli::cli_abort(c(
      "x" = "A block of {.val {target}} has only constant values.",
      "i" = "A process leaves constant values out of its density, so the mean
      of that block would not be estimable. Every block needs an estimated
      value.",
      "i" = "Compare the blocks with the estimation types with
      {.code dplyr::filter(parameters, label == \"{target}\") |>
      dplyr::count(timing, age, estimation_type)}."
    ))
  }
  if (estimation_type == "fixed_effects" && sd[["estimation_type"]] != "constant") {
    cli::cli_warn(c(
      "!" = "The process on {.val {target}} is penalized (fixed effects) and
      its sd is estimated.",
      "i" = "The penalized objective is unbounded as the sd goes to 0."
    ))
  }
  process
}

#' Write the rows of a checked process request into the parameter tibble
#'
#' Time blocks alone become rows that share 1 estimate through `map_group`.
#' A random term makes the target's rows the process values and adds the
#' process's own rows (log_sd, expected_mean, logit_rho).
#'
#' @inheritParams add_process
#' @param process The output of check_process().
#' @return The parameter tibble with the target's rows replaced and the
#'   process's own rows added.
#' @noRd
write_process <- function(parameters, process, estimation_type, sd, rho) {
  target <- process[["target"]]
  rows <- process[["rows"]]
  group <- process[["group"]]
  # Each block starts from the mean of its rows' values, so the rows that
  # share 1 estimate start from the same value.
  block_mean <- stats::ave(rows[["value"]], group)

  if (is.null(process[["formula_terms"]][["random"]])) {
    # Fixed only: rows in the same group share 1 estimate.
    rows[["estimation_type"]] <- "fixed_effects"
    rows[["map_group"]] <- shared_groups(group)
    rows[["value"]] <- block_mean
    return(replace_rows(parameters, process[["is_target"]], rows))
  }

  process_structure <- process[["formula_terms"]][["random"]][["structure"]]
  # Constant rows stay constant when some rows are estimated (see
  # process_densities()). When every row is constant, the call is a request
  # to estimate them all.
  is_constant <- rows[["estimation_type"]] == "constant"
  if (all(is_constant)) {
    is_constant[] <- FALSE
  }
  rows[["estimation_type"]][!is_constant] <- estimation_type
  rows[["map_group"]] <- NA_integer_
  rows[["distribution_type"]] <- "process"
  rows[["distribution"]] <- "Dnorm"
  rows[["re_structure"]] <- process_structure
  if (process_structure == "rw") {
    # A random walk has no mean, so its first estimated value has no density
    # term. As a random effect it would be improper, so it is a fixed effect.
    # A penalized walk (all fixed effects) is then a penalty on the
    # differences only, and the data determine the level.
    years <- rows[["timing"]]
    is_first <- !is_constant & years == min(years[!is_constant])
    rows[["estimation_type"]][is_first] <- "fixed_effects"
  }
  # Each expected_mean starts from the mean of the estimated values in its
  # block, as the density uses only those.
  estimated_value <- dplyr::if_else(is_constant, NA_real_, rows[["value"]])
  process_mean <- stats::ave(
    estimated_value, group,
    FUN = function(block_values) mean(block_values, na.rm = TRUE)
  )

  template <- dplyr::slice(rows, 1) |>
    dplyr::mutate(
      age = NA, timing = NA, estimation_type = "fixed_effects",
      map_group = NA_integer_, re_structure = NA_character_,
      target_label = target, distribution_type = "process",
      distribution = "Dnorm"
    )
  hyperparameters <- list(
    dplyr::mutate(template,
      label = "log_sd", value = log(sd[["value"]]),
      estimation_type = sd[["estimation_type"]]
    )
  )
  if (process_structure != "rw") {
    hyperparameters[[2]] <- rows |>
      dplyr::mutate(
        label = "expected_mean", value = process_mean,
        estimation_type = "fixed_effects",
        map_group = shared_groups(group),
        re_structure = NA_character_, target_label = target
      )
  }
  if (process_structure == "ar1") {
    hyperparameters[[3]] <- dplyr::mutate(template,
      label = "logit_rho",
      value = logit(-1, 1, rho[["value"]]),
      estimation_type = rho[["estimation_type"]]
    )
  }
  replace_rows(
    parameters, process[["is_target"]],
    dplyr::bind_rows(rows, hyperparameters)
  )
}

#' Add a normal prior on 1 parameter
#'
#' `log_q ~ normal(mean = log(0.5), sd = 0.2)`. The mean and sd are written as
#' constant expected_mean and log_sd rows whose `target_label` is the
#' parameter's name in the model. A prior on a parameter with 1 value per year
#' or age applies to each value. Values that share 1 estimate, such as a time
#' block, are scored once, so the prior's weight does not depend on how many
#' years share the estimate. Constant values are scored too, which adds a
#' constant to the objective but does not change the estimates.
#'
#' Any parameter can take a prior, named as the model names it:
#' - A module parameter by its tibble label, such as `logit_steep` or
#'   `log_q`.
#' - A growth parameter estimated on a log or logit scale by that label, such
#'   as `log_mean_length_young ~ normal(mean = log(280), sd = 0.1)`, so the
#'   prior's scale is the one the label says.
#' - A process's own parameter by its flat name, such as `log_q_log_sd` for
#'   the sd of a process on `log_q`, and the recruitment `log_sd` as `log_sd`.
#' - An estimated likelihood parameter by its Data label, such as
#'   `index_log_sd`.
#'
#' @inheritParams add_process
#' @param formula `target ~ normal(mean = , sd = )`, with the mean and sd on
#'   the scale of the target.
#' @return The parameter tibble with the prior's 2 rows added.
#' @keywords setup_default_parameters
#' @export
#' @examples
#' \dontrun{
#' parameters <- setup_default_parameters(FIMSFrame(data_big)) |>
#'   dplyr::mutate(estimation_type = dplyr::if_else(
#'     label == "logit_steep", "fixed_effects", estimation_type
#'   ))
#' # A normal prior on steepness, on the logit scale it is estimated on
#' parameters |>
#'   add_priors(logit_steep ~ normal(mean = logit(0.2, 1, 0.75), sd = 0.5),
#'     module_name = "Recruitment"
#'   )
#' }
add_priors <- function(parameters, formula, module_name, fleet = NA_character_) {
  parameters <- add_process_columns(parameters)
  target <- as.character(formula[[2]])
  prior <- formula[[3]]
  if (!is.call(prior) || !identical(prior[[1]], as.name("normal"))) {
    cli::cli_abort(c(
      "x" = "The prior is {.code {deparse(prior)}}.",
      "i" = "Only {.code normal(mean = , sd = )} priors are implemented."
    ))
  }
  # Match the arguments as R would, so normal(log(0.5), 0.2) works too.
  prior_arguments <- as.list(match.call(function(mean, sd) NULL, prior))[-1]
  # The mean and sd may name variables from where the formula was written.
  prior_mean <- eval(prior_arguments[["mean"]], environment(formula))
  prior_sd <- eval(prior_arguments[["sd"]], environment(formula))
  if (!is_single_number(prior_mean)) {
    cli::cli_abort(c(
      "x" = "The prior mean of {.val {target}} is
      {.code {deparse(prior_arguments[['mean']])}}.",
      "i" = "The prior mean must be 1 number, on the scale of
      {.val {target}}.",
      "i" = "Give it as {.code {target} ~ normal(mean = 0, sd = 0.5)}."
    ))
  }
  # The prior sd is written as its log, which is finite only above 0.
  if (!is_single_number(prior_sd) || prior_sd <= 0) {
    cli::cli_abort(c(
      "x" = "The prior sd of {.val {target}} is
      {.code {deparse(prior_arguments[['sd']])}}.",
      "i" = "The prior sd must be 1 number > 0.",
      "i" = "Give it as {.code {target} ~ normal(mean = 0, sd = 0.5)}."
    ))
  }
  is_target <- prior_target_rows(parameters, target, module_name, fleet)
  target_fleets <- unique(parameters[["fleet"]][is_target])
  if (has_prior(parameters, target, module_name, target_fleets)) {
    cli::cli_abort(c(
      "x" = "{.val {target}} already has a prior.",
      "i" = "Use 1 call per target."
    ))
  }
  template <- dplyr::slice(parameters, which(is_target)[1]) |>
    dplyr::mutate(
      age = NA, timing = NA, estimation_type = "constant",
      map_group = NA_integer_, re_structure = NA_character_,
      target_label = target, distribution_type = "prior",
      distribution = "Dnorm"
    )
  dplyr::bind_rows(
    parameters,
    dplyr::mutate(template, label = "expected_mean", value = prior_mean),
    dplyr::mutate(template, label = "log_sd", value = log(prior_sd))
  )
}

#' The tibble rows a prior's target names
#'
#' See add_priors() for how targets are named. A target that has its own
#' process (e.g., log_devs) cannot also take a prior.
#' @noRd
prior_target_rows <- function(parameters, target, module_name, fleet) {
  in_module <- parameters[["module_name"]] == module_name &
    (is.na(fleet) | parameters[["fleet"]] %in% fleet)
  is_prior_row <- parameters[["distribution_type"]] %in% "prior" &
    !is.na(parameters[["target_label"]])
  is_target <- in_module & flat_label(parameters) == target & !is_prior_row
  if (module_name == "Growth") {
    is_target <- growth_prior_target(parameters, target, in_module)
  }
  check_target_rows(parameters, is_target, target, module_name, fleet)
  hyperparameter_labels <- c("expected_mean", "log_sd", "logit_rho")
  if (any(parameters[["distribution_type"]][is_target] %in% "process") &&
    !any(parameters[["label"]][is_target] %in% hyperparameter_labels)) {
    cli::cli_abort(c(
      "x" = "{.val {target}} has a process, so it cannot also take a prior.",
      "i" = "Put a prior on the process's parameters instead, e.g.,
      {.code {target}_log_sd ~ normal(...)}."
    ))
  }
  is_target
}

#' The Growth rows a prior's target names, on the scale they are estimated on
#'
#' A growth parameter estimated on a log or logit scale is named with that
#' prefix (log_mean_length_young). The tibble label alone is an error, so the
#' scale of the prior's mean and sd is never guessed.
#' @noRd
growth_prior_target <- function(parameters, target, in_module) {
  growth <- dplyr::filter(parameters, in_module, is.na(.data[["target_label"]]))
  growth_type <- unique(stats::na.omit(growth[["module_type"]]))
  working_scale <- growth_modules[[growth_type]][["working_scale"]](growth)
  model_label <- dplyr::if_else(
    is.na(working_scale), growth[["label"]],
    paste(working_scale, growth[["label"]], sep = "_")
  )
  is_transformed_label <- growth[["label"]] == target & !is.na(working_scale)
  if (any(is_transformed_label)) {
    target_scale <- working_scale[is_transformed_label][1]
    cli::cli_abort(c(
      "x" = "{.val {target}} is estimated on the {target_scale} scale.",
      "i" = "Give the prior on that scale, e.g.,
      {.code {target_scale}_{target} ~ normal(mean = , sd = )}."
    ))
  }
  in_module & is.na(parameters[["target_label"]]) &
    parameters[["label"]] %in% growth[["label"]][model_label == target]
}

# Helpers ----------------------------------------------------------------------

#' Is a value 1 finite number?
#' @noRd
is_single_number <- function(value) {
  is.numeric(value) && length(value) == 1 && is.finite(value)
}

#' "not_estimated" is another name for "constant"
#' @noRd
standard_estimation_type <- function(estimation_type) {
  dplyr::if_else(estimation_type == "not_estimated", "constant", estimation_type)
}

#' The estimation type of a process hyperparameter (sd or rho), from its
#' list's estimation_status or estimation_type, with fixed effects by default
#' @noRd
hyperparameter_estimation_type <- function(settings) {
  estimation_type <- if (!is.null(settings[["estimation_status"]])) {
    settings[["estimation_status"]]
  } else if (!is.null(settings[["estimation_type"]])) {
    settings[["estimation_type"]]
  } else {
    "fixed_effects"
  }
  standard_estimation_type(estimation_type)
}

#' Split a process formula into its target, fixed term and random term
#' @noRd
parse_process_formula <- function(formula) {
  if (length(formula) != 3) {
    cli::cli_abort(c(
      "x" = "The formula has no target on the left.",
      "i" = "Write it as {.code log_q ~ cut(year, c(-Inf, 10, Inf))}."
    ))
  }
  right_terms <- split_plus(formula[[3]])
  is_random <- vapply(right_terms, is_random_term, logical(1))
  if (sum(is_random) > 1) {
    cli::cli_abort(c(
      "x" = "The formula has {sum(is_random)} random terms.",
      "i" = "Use 1 random term per parameter."
    ))
  }
  fixed <- right_terms[!is_random]
  if (length(fixed) > 1) {
    cli::cli_abort(c(
      "x" = "Additive fixed terms (e.g., {.code cut(age) + cut(year)}) need a
      design matrix, not a map, and are not supported.",
      "i" = "Use an interaction ({.code :})."
    ))
  }
  fixed <- if (length(fixed) == 0) 1 else fixed[[1]]
  random <- if (any(is_random)) parse_random_term(right_terms[[which(is_random)]])
  list(
    target = as.character(formula[[2]]),
    fixed = fixed,
    fixed_variables = all.vars(fixed),
    random = random,
    random_variables = if (!is.null(random)) strsplit(random[["grouping"]], ":")[[1]]
  )
}

#' The terms of a formula side joined by `+`, as a list
#' @noRd
split_plus <- function(formula_part) {
  if (is.call(formula_part) && identical(formula_part[[1]], as.name("+"))) {
    return(c(split_plus(formula_part[[2]]), split_plus(formula_part[[3]])))
  }
  list(formula_part)
}

#' Is a formula term a random term: `(1 | g)`, `ar1(1 | g)` or `rw(1 | g)`?
#' @noRd
is_random_term <- function(formula_term) {
  if (!is.call(formula_term)) {
    return(FALSE)
  }
  as.character(formula_term[[1]]) %in% c("(", "ar1", "rw")
}

#' (1 | year) gives iid, ar1(1 | year) AR1, and rw(1 | year) a random walk
#' @noRd
parse_random_term <- function(formula_term) {
  process_structure <- switch(as.character(formula_term[[1]]),
    "(" = "iid",
    ar1 = "ar1",
    rw = "rw"
  )
  bar <- formula_term[[2]]
  if (!is.call(bar) || !identical(bar[[1]], as.name("|")) ||
    !identical(bar[[2]], 1)) {
    cli::cli_abort(c(
      "x" = "The random term {.code {deparse(formula_term)}} is not understood.",
      "i" = "Write random terms as {.code (1 | year)}, {.code ar1(1 | year)} or
      {.code rw(1 | year)}."
    ))
  }
  random_grouping <- paste(deparse(bar[[3]]), collapse = "")
  if (!random_grouping %in% c("year", "age:year")) {
    cli::cli_abort(c(
      "x" = "The random term groups by {.val {random_grouping}}.",
      "i" = "Random terms group by {.val year} or {.val age:year}."
    ))
  }
  if (process_structure != "iid" && random_grouping != "year") {
    cli::cli_abort(c(
      "x" = "{.code {process_structure}()} is by year only.",
      "i" = "A 2D AR1 is not implemented. Use {.code (1 | age:year)} for an
      iid process by age and year."
    ))
  }
  list(structure = process_structure, grouping = random_grouping)
}

#' The tibble rows of a process's target, by label, module and fleet
#' @noRd
target_rows <- function(parameters, target, module_name, fleet) {
  is_target <- parameters[["label"]] == target &
    parameters[["module_name"]] == module_name &
    (is.na(fleet) | parameters[["fleet"]] %in% fleet) &
    is.na(parameters[["target_label"]])
  check_target_rows(parameters, is_target, target, module_name, fleet)
  is_target
}

#' Stop when a target names no rows, or rows in more than 1 fleet when no
#' fleet is given
#' @noRd
check_target_rows <- function(parameters, is_target, target, module_name,
                              fleet) {
  if (!any(is_target)) {
    where <- if (is.na(fleet)) module_name else c(module_name, fleet)
    cli::cli_abort(c(
      "x" = "There are no {.val {target}} rows for {.val {where}}.",
      "i" = "Check the label, {.arg module_name} and {.arg fleet}, e.g., with
      {.code dplyr::distinct(parameters, module_name, fleet, label)}."
    ))
  }
  target_fleets <- unique(stats::na.omit(parameters[["fleet"]][is_target]))
  if (is.na(fleet) && length(target_fleets) > 1) {
    cli::cli_abort(c(
      "x" = "{.val {target}} is in more than 1 fleet: {.val {target_fleets}}.",
      "i" = "Give {.arg fleet}."
    ))
  }
  invisible(is_target)
}

#' Does a target already have a prior?
#'
#' @param fleets The fleets of the target's rows (NA for a population
#'   module).
#' @noRd
has_prior <- function(parameters, target, module_name, fleets) {
  any(
    parameters[["distribution_type"]] %in% "prior" &
      parameters[["target_label"]] %in% target &
      parameters[["module_name"]] == module_name &
      parameters[["fleet"]] %in% fleets
  )
}

#' Stop when a target already has a process, prior or blocks
#' @noRd
check_new_target <- function(rows, target) {
  has_structure <- !is.na(rows[["distribution_type"]]) |
    !is.na(rows[["map_group"]]) | !is.na(rows[["re_structure"]])
  if (any(has_structure)) {
    cli::cli_abort(c(
      "x" = "{.val {target}} already has a process, prior or blocks.",
      "i" = "Use 1 call per target."
    ))
  }
}

#' Every year from the first to the last `timing` in the tibble
#' @noRd
all_years <- function(parameters) {
  timing <- stats::na.omit(parameters[["timing"]])
  seq(min(timing), max(timing))
}

#' 1 row per year (and per age, when the formula uses age), keeping values
#' @noRd
expand_by_year <- function(rows, years, keep_age) {
  if (all(!is.na(rows[["timing"]]))) {
    return(rows)
  }
  if (!keep_age || all(is.na(rows[["age"]]))) {
    rows <- dplyr::slice(rows, 1)
  }
  # Rows vary fastest, so the result is in year order.
  expanded <- expand.grid(row = seq_len(nrow(rows)), timing = years)
  dplyr::slice(rows, expanded[["row"]]) |>
    dplyr::mutate(timing = expanded[["timing"]]) |>
    dplyr::arrange(.data[["timing"]], .data[["age"]])
}

#' Evaluate the fixed term on each row: a group number per row
#' @noRd
evaluate_fixed_term <- function(fixed, rows, formula_environment) {
  if (identical(fixed, 1)) {
    return(rep(1L, nrow(rows)))
  }
  # `year` and `age` come from the rows. Other names, such as a vector of
  # breaks, come from where the formula was written.
  groups <- eval(
    fixed, list(year = rows[["timing"]], age = rows[["age"]]),
    formula_environment
  )
  if (anyNA(groups)) {
    cli::cli_abort(c(
      "x" = "The fixed term gives NA for {sum(is.na(groups))} row{?s}.",
      "i" = "Check the {.code cut()} breaks cover every year and age in the
      target's rows."
    ))
  }
  # This usually means the breaks are outside the years, e.g., breaks 10 and
  # 20 with calendar years. A fixed term meant to give 1 group is written 1.
  if (length(unique(groups)) == 1) {
    cli::cli_abort(c(
      "x" = "The fixed term gives the same group for every row.",
      "i" = "Check the {.code cut()} breaks against the target's years (the
      {.var timing} column) and ages."
    ))
  }
  as.integer(droplevels(as.factor(groups)))
}

#' map_group from group numbers: NA when every row has its own value
#' @noRd
shared_groups <- function(group) {
  if (length(unique(group)) == length(group)) {
    return(rep(NA_integer_, length(group)))
  }
  as.integer(group)
}

#' Replace the target's rows, keeping the target's place in the tibble
#' @noRd
replace_rows <- function(parameters, is_target, new_rows) {
  first <- which(is_target)[1]
  before <- seq_len(first - 1)
  after <- setdiff(seq(first, nrow(parameters)), which(is_target))
  dplyr::bind_rows(
    dplyr::slice(parameters, before), new_rows, dplyr::slice(parameters, after)
  )
}
