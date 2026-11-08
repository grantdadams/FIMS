# Developers: ----

# This file defines the parent class of FIMSFit and its potential children. The
# class is an S4 class with accessors and validators but no setters. For more
# details on how to create an S4 class in FIMS please see R/fimsframe.R

# TODO: ----

# TODO: Fix "no metadata object found to revise superClass" in sdreportOrList
# TODO: Write more validity checks for FIMSFit
# TODO: Better document the return of [get_estimates()], i.e., columns
# TODO: Make a helper function to add lower and upper CI for users in estimates

# methods::setClass: ----

# Need to use an S3 class for the following S4 class
methods::setOldClass(Classes = "package_version")
methods::setOldClass(Classes = "difftime")
methods::setOldClass(Classes = "sdreport")
# Join sdreport and list into a class in case the sdreport is not created
methods::setClassUnion("sdreportOrList", members = c("sdreport", "list"))

methods::setClass(
  Class = "FIMSFit",
  slots = c(
    input = "list",
    obj = "list",
    opt = "list",
    max_gradient = "numeric",
    gradient = "numeric",
    report = "list",
    sdreport = "sdreportOrList",
    number_of_parameters = "integer",
    run_time = "difftime",
    version = "package_version",
    model_output = "character",
    convergence = "data.frame"
  )
)

# methods::setMethod: printers ----
# TODO: add `get_report`, `get_opt`, etc. to the list of available slots in show()?
methods::setMethod(
  f = "show",
  signature = "FIMSFit",
  definition = function(object) {
    cli::cli_inform(c(
      "i" = "The object is of the class FIMSFit v.{get_version(object)}",
      "i" = "The slots can be accessed using {.fn get_*} functions, e.g.,",
      "*" = "{.fn get_model_output}",
      "*" = "{.fn get_obj}",
      "*" = "{.fn get_version}",
      "i" = "The following slots are available: {methods::slotNames(object)}.",
      "i" = "Use {.fn print} to see a summary of the fit."
    ))
  }
)

methods::setMethod(
  f = "print",
  signature = "FIMSFit",
  definition = function(x) {
    rt <- as.numeric(x@run_time[["time_total"]], units = "secs")
    ru <- "seconds"
    if (rt > 60 * 60 * 24) {
      rt <- rt / (60 * 60 * 24)
      ru <- "days"
    } else if (rt > 60 * 60) {
      rt <- rt / (60 * 60)
      ru <- "hours"
    } else if (rt > 60) {
      rt <- rt / 60
      ru <- "minutes"
    }

    number_of_parameters <- paste(
      names(x@number_of_parameters),
      x@number_of_parameters,
      sep = "="
    )
    total_parameters <- sum(x@number_of_parameters)
    all_parameters_info <- c(number_of_parameters, paste(
      "total",
      total_parameters,
      sep = "="
    ))
    div_digit <- cli::cli_div(theme = list(.val = list(digits = 5)))
    terminal_sb <- sapply(
      x@report[["spawning_biomass"]],
      function(y) utils::tail(y, 1)
    )
    # The overall status is the most severe check, so a failed check cannot be
    # missed at the top of the summary.
    convergence <- get_convergence(x)
    severity_levels <- c("OK", "NOTE", "WARN", "FAIL")
    convergence_status <- if (NROW(convergence) == 0) {
      "not checked"
    } else {
      severity_levels[max(match(convergence[["severity"]], severity_levels))]
    }
    flagged_checks <- convergence[["check"]][convergence[["severity"]] != "OK"]
    convergence_bullet <- stats::setNames(
      if (length(flagged_checks) > 0) {
        "Convergence: {.val {convergence_status}} from
        {.val {flagged_checks}}. Use {.code sanity_fims(fit)} to see each
        check."
      } else {
        "Convergence: {.val {convergence_status}}"
      },
      switch(convergence_status,
        "FAIL" = "x",
        "WARN" = "!",
        "OK" = "v",
        "i"
      )
    )
    cli::cli_inform(c(
      convergence_bullet,
      "i" = "FIMS model version: {.val {x@version}}",
      "i" = "Total run time was {.val {rt}} {ru}",
      "i" = "Number of parameters: {all_parameters_info}",
      "i" = "Maximum gradient= {.val {x@max_gradient}}",
      "i" = "Negative log likelihood (NLL):",
      "*" = "Marginal NLL= {.val {x@opt$objective}}",
      "*" = "Total NLL= {.val {x@report$jnll}}",
      "i" = "Terminal SB= {.val {terminal_sb}}"
    ))
    cli::cli_end(div_digit)
  }
)

# methods::setMethod: accessors ----

# Accessor functions for a FIMSFit object
# 1 methods::setGeneric() per slot but potentially >1 methods::setMethod() per methods::setGeneric()

#' Get a slot in a FIMSFit object
#'
#' There is an accessor function for each slot in the S4 class `FIMSFit`, where
#' the function is named `get_*()` and the star can be replaced with the slot
#' name, e.g., [get_input()]. These accessor functions are the preferred way
#' to access objects stored in the available slots.
#'
#' @param x Output returned from [fit_fims()].
#' @name get_FIMSFit
#' @seealso
#' * [fit_fims()]
#' * [setup_default_parameters()]
NULL

#' @return
#' [get_input()] returns the list that was used to fit the FIMS model, which
#' is the returned object from [setup_default_parameters()].
#' @export
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setGeneric("get_input", function(x) standardGeneric("get_input"))
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setMethod("get_input", "FIMSFit", function(x) x@input)

#' @return
#' [get_report()] returns the model report, with every quantity the model
#' passes to `REPORT()`.
#' @export
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setGeneric("get_report", function(x) standardGeneric("get_report"))
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setMethod("get_report", "FIMSFit", function(x) x@report)

#' @return
#' [get_obj()] returns the output from [RTMB::MakeADFun()].
#' @export
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setGeneric("get_obj", function(x) standardGeneric("get_obj"))
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setMethod("get_obj", "FIMSFit", function(x) x@obj)

#' @return
#' [get_opt()] returns the output from [nlminb()], which is the minimizer used
#' in [fit_fims()].
#' @export
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setGeneric("get_opt", function(x) standardGeneric("get_opt"))
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setMethod("get_opt", "FIMSFit", function(x) x@opt)

#' @return
#' [get_max_gradient()] returns the maximum gradient found when optimizing the
#' model.
#' @export
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setGeneric("get_max_gradient", function(x) standardGeneric("get_max_gradient"))
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setMethod("get_max_gradient", "FIMSFit", function(x) x@max_gradient)

#' @return
#' [get_gradient()] returns the per-parameter gradient vector at the MLE as
#' stored during fitting, so the model is not evaluated again.
#' @export
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setGeneric("get_gradient", function(x) standardGeneric("get_gradient"))
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setMethod("get_gradient", "FIMSFit", function(x) x@gradient)

#' @return
#' [get_sdreport()] returns the list from [RTMB::sdreport()].
#' @export
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setGeneric("get_sdreport", function(x) standardGeneric("get_sdreport"))
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setMethod("get_sdreport", "FIMSFit", function(x) x@sdreport)

#' @return
#' [get_convergence()] returns a tibble with one row per convergence check
#' run by [fit_fims()]: `check`, `severity` (`"OK"`, `"NOTE"` for a check that
#' did not run, `"WARN"`, or `"FAIL"`), `value`, `threshold`, `parameter`,
#' and `message`. It has no rows when the model was not optimized.
#' @export
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setGeneric("get_convergence", function(x) standardGeneric("get_convergence"))
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setMethod("get_convergence", "FIMSFit", function(x) {
  # A FIMSFit without a convergence slot, e.g., read from an older saved file,
  # has no checks to return
  if (!methods::.hasSlot(x, "convergence")) {
    return(convergence_tibble(character(0), character(0)))
  }
  x@convergence
})

#' @return
#' [get_estimates()] returns a tibble of parameter values and their
#' uncertainties from a fitted model. The `year_i` column is a position that
#' starts at 1, and the `age` and `length` columns are the ages and length bins
#' from the data. `age` is `NA` if populations have different ages.
#' @export
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setGeneric("get_estimates", function(x) standardGeneric("get_estimates"))
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setMethod(
  "get_estimates",
  "FIMSFit",
  function(x) {
    # fit_fims() builds the table once, at the end of the fit, and stores it
    # as JSON in the model_output slot.
    estimates_from_json(get_model_output(x))
  }
)

#' @return
#' [get_number_of_parameters()] returns a vector of integers specifying the
#' number of fixed-effect parameters and the number of random-effect parameters
#' in the model.
#' @export
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setGeneric(
  "get_number_of_parameters",
  function(x) standardGeneric("get_number_of_parameters")
)
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setMethod(
  "get_number_of_parameters",
  "FIMSFit",
  function(x) x@number_of_parameters
)

#' @return
#' [get_run_time()] returns the amount of time it took to run the model in
#' seconds as a `difftime` object.
#' @export
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setGeneric("get_run_time", function(x) standardGeneric("get_run_time"))
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setMethod("get_run_time", "FIMSFit", function(x) x@run_time)

#' @return
#' [get_version()] returns the `package_version` of FIMS that was used to fit
#' the model.
#' @export
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setGeneric("get_version", function(x) standardGeneric("get_version"))
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setMethod("get_version", "FIMSFit", function(x) x@version)

#' @return
#' [get_model_output()] returns the [get_estimates()] table as a JSON string.
#' @export
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setGeneric("get_model_output", function(x) standardGeneric("get_model_output"))
#' @rdname get_FIMSFit
#' @keywords fit_fims
methods::setMethod("get_model_output", "FIMSFit", function(x) x@model_output)

# methods::setValidity ----

methods::setValidity(
  Class = "FIMSFit",
  method = function(object) {
    errors <- character()

    # Check that obj is from RTMB::MakeADFun(). %in% rather than setequal()
    # because RTMB adds elements, e.g., force.update, to TMB's list.
    TMB_MakeADFun_names <- c(
      "par", "fn", "gr", "he", "hessian", "method", "retape", "env", "report",
      "simulate"
    )
    if (!all(TMB_MakeADFun_names %in% names(object@obj))) {
      errors <- c(
        errors,
        "obj must be a list returned from RTMB::MakeADFun() but it does not
        appear to be so because it does not have the standard names."
      )
    }

    # Return
    if (length(errors) == 0) {
      return(TRUE)
    } else {
      return(errors)
    }
  }
)

# methods::setMethod: is.FIMSFit ----

#' Check if an object is of class FIMSFit
#'
#' @param x Returned list from [fit_fims()].
#' @keywords fit_fims
#' @export
is.FIMSFit <- function(x) {
  inherits(x, "FIMSFit")
}

# Constructors ----

#' Class constructors for class `FIMSFit` and associated child classes
#'
#' Create an object with the class of `FIMSFit` after running a FIMS model. This
#' is typically done within [fit_fims()] but it can be create manually by the
#' user if they have used their own bespoke code to fit a FIMS model.
#'
#' @inheritParams fit_fims
#' @param obj An object returned from [RTMB::MakeADFun()].
#' @param opt An object returned from an optimizer, typically from
#'   [stats::nlminb()], used to fit the model.
#' @param sdreport An object of the `sdreport` class as returned from
#'   [RTMB::sdreport()].
#' @param run_time A vector of at least length one, where all entries are of the
#'   `timediff` class and at least one is named "time_total". This information
#'   is available in [fit_fims()] and added to this argument internally but if
#'   you are a power user you can calculate the time it took to run your model
#'   by subtracting two [Sys.time()] objects.
#' @param version The version of FIMS that was used to optimize the model. If
#'   [fit_fims()] was not used to optimize the model, then the default is to
#'   use the current version of the package that is loaded.
#' @param convergence A data frame with one row per convergence check, as
#'   returned by [get_convergence()]. [fit_fims()] fills it in. The default has
#'   no rows, which means no checks were run.
#'
#' @return
#' An object with an S4 class of `FIMSFit` is returned. The object will have the
#' following slots:
#'   \describe{
#'     \item{\code{input}:}{
#'       A list containing the model setup in the same form it was passed.
#'     }
#'     \item{\code{obj}:}{
#'       A list returned from [RTMB::MakeADFun()].
#'     }
#'     \item{\code{opt}:}{
#'       A list containing the optimized model in the same form it was passed.
#'     }
#'     \item{\code{max_gradient}:}{
#'       The maximum gradient found when optimizing the model. The default is
#'       `NA`, which means that the model was not optimized.
#'     }
#'     \item{\code{report}:}{
#'       A list containing the model report from `obj[["report"]]()`.
#'     }
#'     \item{\code{sdreport}:}{
#'       An object with the `sdreport` class containing the output from
#'       `RTMB::sdreport(obj)`.
#'     }
#'     \item{\code{run_time}:}{
#'       The length of time it took to run the model if it was optimized.
#'     }
#'     \item{\code{version}:}{
#'       The package version of FIMS used to fit the model or at least the
#'       version used to create this output, which will not always be the same
#'       if you are running this function yourself.
#'     }
#'     \item{\code{model_output}:}{
#'       The FIMS model output as a JSON string.
#'     }
#'     \item{\code{convergence}:}{
#'       A tibble with the result of each convergence check. See
#'       [get_convergence()].
#'     }
#'   }
#' @keywords fit_fims
#' @export
FIMSFit <- function(
  input,
  obj,
  opt = list(),
  sdreport = list(),
  run_time = c("time_total" = as.difftime(0, units = "secs")),
  version = utils::packageVersion("FIMS"),
  convergence = convergence_tibble(character(0), character(0))
) {
  # Determine the number of parameters
  n_total <- length(obj[["env"]][["last.par.best"]])
  n_fixed_effects <- length(obj[["par"]])
  n_random_effects <- n_total - n_fixed_effects
  number_of_parameters <- c(
    fixed_effects = n_fixed_effects,
    random_effects = n_random_effects
  )
  rm(n_total, n_fixed_effects, n_random_effects)

  # Compute the full per-parameter gradient vector once at construction time.
  # max_gradient is derived from it so obj$gr() is only ever called here.
  # Storing it in the FIMSFit slot means get_estimates() and the broom
  # methods (tidy, augment, glance) read x@gradient instead of evaluating the
  # model again.
  # as.numeric() is required because obj$gr() returns a matrix (1xn) in some
  # call patterns, which would fail the "numeric" slot type check.
  gradient_vector <- if (length(opt) > 0) {
    as.numeric(obj[["gr"]](opt[["par"]]))
  } else {
    rep(NA_real_, length(obj[["par"]]))
  }
  max_gradient <- if (length(opt) == 0) {
    NA_real_
  } else if (length(gradient_vector) == 0) {
    0
  } else {
    max(abs(gradient_vector))
  }

  # Get the report
  report <- obj[["report"]](obj[["env"]][["last.par.best"]])

  # The get_estimates() table, built once and stored as JSON
  model_output <- estimates_to_json(
    estimates_table(input, obj, report, gradient_vector, sdreport)
  )

  # Name the sdreport's parameters by their parameter ids
  sdreport <- name_by_parameter_id(sdreport, input[["parameter_table"]])

  fit <- methods::new(
    "FIMSFit",
    input = input,
    obj = obj,
    opt = opt,
    max_gradient = max_gradient,
    gradient = gradient_vector,
    report = report,
    sdreport = sdreport,
    number_of_parameters = number_of_parameters,
    run_time = run_time,
    version = version,
    model_output = model_output,
    convergence = convergence
  )
  fit
}

#' Fit a FIMS model (BETA)
#'
#' @param input Input list as returned by [initialize_fims()]. Which values
#'   are estimated, held constant or shared comes from the `estimation_type`
#'   and `map_group` columns of the parameter tibble, which [initialize_fims()]
#'   turns into `input[["map"]]`, 1 factor per model parameter. Rows of the
#'   same parameter (module, fleet and label) with the same `map_group` share
#'   1 estimate, so they must have identical starting values. The same `map_group` in 2 parameters does not
#'   share a value between them.
#' @param get_sd A boolean specifying if the [RTMB::sdreport()] should be
#'   calculated? It is skipped when the maximum gradient is above 0.01 or not
#'   finite, because standard errors far from the optimum are not meaningful.
#' @param save_sd A logical, with the default `TRUE`, indicating whether the
#'   sdreport is returned in the output. If `FALSE`, the slot for the report
#'   will be empty.
#' @param number_of_loops A positive integer specifying the number of
#'   iterations of the optimizer that will be performed to improve the
#'   gradient. The default is three, leading to four total optimization steps.
#' @param optimize Optimize (TRUE, default) or (FALSE) build and return
#'   a list containing the obj and report slot.
#' @param number_of_newton_steps The number of Newton steps using the inverse
#'   Hessian to do after optimization. Not yet implemented.
#' @param control A list of optimizer settings passed to [stats::nlminb()].
#'   The default is a list of 3: `eval.max = 10000`,
#'   `iter.max = 10000`, and `trace = 0`.
#' @param filename Character string giving a file name to save the fitted
#'   object as an RDS object. Defaults to NULL. Not yet implemented: any
#'   other value gives a warning and nothing is saved.
#' @param getReportCovariance A logical passed to [RTMB::sdreport()], with the
#'   default `FALSE`. Standard errors of all parameters and derived quantities
#'   are calculated either way. If `TRUE`, the full covariance matrix of the
#'   derived quantities is also calculated and stored as `cov` in the
#'   `sdreport`; it grows with the square of the number of derived quantities
#'   and can use several GB of memory. If `FALSE`, `cov` is `NA`.
#' @param report_age_to_length_conversion_derived A logical, with the default
#'   `FALSE`. If `TRUE`, the report includes the growth-derived
#'   age-to-length conversion for every fleet and year in
#'   `age_to_length_conversion_derived`. It is large, so it is left out by
#'   default.
#' @return
#' An object of class `FIMSFit` is returned, where the structure is the same
#' regardless if `optimize = TRUE` or not. Uncertainty information is only
#' included in the `estimates` slot if `get_sd = TRUE`. The result of each
#' convergence check is stored on the object and returned by
#' [get_convergence()].
#' @seealso
#' * [FIMSFit()]
#' @details This function is a beta version still and subject to change
#'   without warning.
#'
#' [get_estimates()] keeps every parameter row and gives shared values the
#' same estimate and standard error. Constant values keep their starting
#' values and have `NA` standard errors. Gradients are reported only on the
#' first row of each shared value. Parameter counts refer to the values that
#' are estimated. If all fixed effects are held constant, the random-effects
#' model is evaluated without an outer optimizer. If every value is held
#' constant, use `optimize = FALSE`.
#' @keywords fit_fims
#' @export
fit_fims <- function(input,
                     get_sd = TRUE,
                     save_sd = TRUE,
                     number_of_loops = 3,
                     optimize = TRUE,
                     number_of_newton_steps = 0,
                     control = list(
                       eval.max = 10000,
                       iter.max = 10000,
                       trace = 0
                     ),
                     filename = NULL,
                     getReportCovariance = FALSE,
                     report_age_to_length_conversion_derived = FALSE) {
  # See issue 455 of sdmTMB to see what should be used.
  # https://github.com/pbs-assess/sdmTMB/issues/455
  # NOTE: When we add implementation for newton step we need to
  # review the above github issue to make sure we maintain continuity
  # between outputs as last.par may not equal last.par.best due to
  # the smallest newton gradient solution not matching the smallest
  # likelihood value. This can cause sanity issues in output reporting.
  if (number_of_newton_steps > 0) {
    cli::cli_abort(c(
      "x" = "Newton steps are not implemented yet.",
      "i" = "Use {.code number_of_newton_steps = 0}. To improve the gradient,
      increase {.arg number_of_loops} instead."
    ))
  }
  if (!is.list(input) || is.null(input[["model_data"]]) ||
    is.null(input[["parameter_table"]])) {
    cli::cli_abort(c(
      "x" = "{.arg input} is not a model from {.fn initialize_fims}.",
      "i" = "Build it with {.code initialize_fims(parameters, data)}."
    ))
  }
  # Checked before the fit, which can take minutes, rather than after it.
  if (!rlang::is_bool(get_sd) || !rlang::is_bool(save_sd) ||
    !rlang::is_bool(optimize)) {
    cli::cli_abort(c(
      "x" = "{.arg get_sd}, {.arg save_sd} and {.arg optimize} must each be
      {.code TRUE} or {.code FALSE}.",
      "i" = "They are {.code {deparse(get_sd)}}, {.code {deparse(save_sd)}}
      and {.code {deparse(optimize)}}."
    ))
  }
  if (!is_single_number(number_of_loops) || number_of_loops < 0) {
    cli::cli_abort(c(
      "x" = "{.arg number_of_loops} is {.code {deparse(number_of_loops)}}.",
      "i" = "{.arg number_of_loops} must be 1 number >= 0."
    ))
  }
  not_parameters <- setdiff(names(input[["map"]]), names(input[["parameters"]]))
  if (length(not_parameters) > 0) {
    cli::cli_abort(c(
      "x" = "{.code input[[\"map\"]]} has {.val {not_parameters}}, which
      {?is not a model parameter/are not model parameters}.",
      "i" = "Hold values constant with {.var estimation_type} {.val constant}
      in the parameter tibble, and share 1 estimate with the {.var map_group}
      column, e.g., {.fn add_process} with {.code cut(year, ...)} for time
      blocks.",
      "i" = "Compare {.code names(input[[\"map\"]])} with
      {.code names(input[[\"parameters\"]])} to see which names do not match."
    ))
  }
  # If the estimation_type of all parameters is constant, FIMS will abort if
  # optimize is set to TRUE
  n_estimated <- sum(!is.na(unlist(input[["map"]])))
  if (optimize && n_estimated == 0) {
    cli::cli_abort("FIMS must have at least one parameter to optimize.")
  }

  model_data <- input[["model_data"]]
  # Standard errors of derived quantities are only needed for an sdreport.
  model_data[["adreport"]] <- get_sd
  model_data[["report_age_to_length_conversion_derived_tensor"]] <-
    isTRUE(report_age_to_length_conversion_derived)
  obj <- RTMB::MakeADFun(
    function(parameters) fims_model(parameters, model_data),
    input[["parameters"]],
    map = input[["map"]],
    random = if (length(input[["random"]]) > 0) input[["random"]],
    silent = TRUE
  )
  parameter_names <- parameter_labels(
    names(obj[["par"]]),
    input[["parameter_table"]]
  )
  random_effects_names <- parameter_labels(
    names(obj[["env"]][["last.par.best"]])[obj[["env"]][["random"]]],
    input[["parameter_table"]]
  )
  if (!optimize) {
    initial_fit <- FIMSFit(
      input = input,
      obj = obj,
      run_time = c("time_total" = as.difftime(0, units = "secs"))
    )
    return(initial_fit)
  }
  if (!is_fims_verbose()) {
    control$trace <- 0
  }
  ## optimize and compare
  cli::cli_inform(c("v" = "Starting optimization ..."))
  t0 <- Sys.time()
  opt <- try_nlminb(
    object = obj,
    control_list = control,
    starting_values = obj[["par"]]
  )

  if (is.null(opt)) {
    return(failed_fims_fit(input, obj))
  }

  gradient <- as.numeric(obj[["gr"]](opt[["par"]]))
  maxgrad0 <- maxgrad <- if (length(gradient) > 0) max(abs(gradient)) else 0
  if (number_of_loops > 0 && length(obj[["par"]]) > 0) {
    cli::cli_inform(c(
      "i" = "Restarting optimizer {number_of_loops} time{?s} to improve
            gradient."
    ))
    # control$trace is reset to zero regardless of verbosity because the
    # differences in values printed out using control$trace will be
    # negligible between these different runs and is not worth printing
    control$trace <- 0
    for (ii in 1:number_of_loops) {
      opt <- try_nlminb(
        object = obj,
        control_list = control,
        starting_values = opt[["par"]]
      )
      # Handle hard failures where try_nlminb did not return a list
      if (is.null(opt)) {
        cli::cli_inform(message = c(
          "!" = "{.fn nlminb} failed during loop {ii}.",
          "i" = "The model successfully converged before this loop.",
          "i" = "The failed results are being returned."
        ))
        return(failed_fims_fit(input, obj))
      }
      gradient <- as.numeric(obj[["gr"]](opt[["par"]]))
      maxgrad <- if (length(gradient) > 0) max(abs(gradient)) else 0
    }
    div_digit <- cli::cli_div(theme = list(.val = list(digits = 5)))
    cli::cli_inform(c(
      "i" = "Maximum gradient went from {.val {maxgrad0}} to
            {.val {maxgrad}} after {number_of_loops} steps."
    ))
    cli::cli_end(div_digit)
  }
  time_optimization <- Sys.time() - t0
  cli::cli_inform(c("v" = "Finished optimization"))
  opt[["par"]] <- obj[["env"]][["last.par.best"]][obj[["env"]][["lfixed"]]()]
  opt[["objective"]] <- obj[["fn"]](opt[["par"]])
  gradient <- as.numeric(obj[["gr"]](opt[["par"]]))
  maxgrad <- if (length(gradient) > 0) max(abs(gradient)) else 0

  mle_convergence <- check_mle_convergence(
    opt,
    maxgrad,
    gradient = gradient,
    parameter_names = parameter_names
  )
  # Standard errors at a point far from the optimum are not meaningful, so
  # sdreport is skipped when the gradient fails. A failed convergence code with
  # a small gradient, e.g., a false convergence, still gets standard errors.
  gradient_failed <- any(
    mle_convergence[["check"]] == "max_gradient" &
      mle_convergence[["severity"]] == "FAIL"
  )

  time_sdreport <- NA
  if (get_sd && !gradient_failed) {
    t2 <- Sys.time()
    sdreport <- RTMB::sdreport(
      obj,
      par.fixed = opt[["par"]],
      getReportCovariance = getReportCovariance
    )
    cli::cli_inform(c("v" = "Finished sdreport"))
    time_sdreport <- Sys.time() - t2
    sdreport_convergence <- check_sdreport_convergence(
      obj,
      opt,
      sdreport,
      parameter_names = parameter_names,
      random_effects_names = random_effects_names
    )
  } else {
    sdreport <- list()
    time_sdreport <- as.difftime(0, units = "secs")
    # Without this row a fit with no sdreport would look like it passed the
    # Hessian checks rather than skipped them.
    sdreport_convergence <- convergence_tibble(
      check = "sdreport",
      severity = "NOTE",
      message = if (gradient_failed) {
        "Hessian not checked because the maximum gradient failed."
      } else {
        "Hessian not checked because get_sd = FALSE."
      }
    )
  }

  run_time <- c(
    time_optimization = time_optimization,
    time_sdreport = time_sdreport,
    time_total = Sys.time() - t0
  )
  fit <- FIMSFit(
    input = input,
    obj = obj,
    opt = opt,
    sdreport = sdreport,
    run_time = run_time
  )
  # The NLL check reads the report that FIMSFit() builds and the model data
  fit@convergence <- dplyr::bind_rows(
    mle_convergence,
    check_nll_convergence(opt, get_report(fit), input[["model_data"]]),
    sdreport_convergence
  )
  # The sdreport is dropped only after FIMSFit() has used it, so
  # get_estimates() still has the standard errors.
  if (!save_sd) {
    fit@sdreport <- list()
  }
  print(fit)
  if (!is.null(filename)) {
    cli::cli_warn(c(
      "i" = "Saving output to file is not yet implemented."
    ))
    # saveRDS(fit, file=file.path(input[["path"]], filename))
  }
  return(fit)
}

# we create an as.list method for this new FIMSFit
methods::setMethod("as.list", signature(x = "FIMSFit"), function(x) {
  mapply(
    function(y) {
      # apply as.list if the slot is again an user-defined object
      # therefore, as.list gets applied recursively
      if (inherits(slot(x, y), "FIMSFit")) {
        as.list(slot(x, y))
      } else {
        # otherwise just return the slot
        slot(x, y)
      }
    },
    slotNames(class(x)),
    SIMPLIFY = FALSE
  )
})

# Helper function used within fit_fims
try_nlminb <- function(object, control_list, starting_values) {
  optimized <- tryCatch(
    {
      if (length(starting_values) == 0) {
        list(
          par = starting_values,
          objective = object[["fn"]](starting_values),
          convergence = 0L,
          message = "No fixed effects to optimize"
        )
      } else {
        with(
          object,
          nlminb(
            start = starting_values,
            objective = fn,
            gradient = gr,
            control = control_list
          )
        )
      }
    },
    error = function(e) {
      cli::cli_rule(left = "{.fun stats::nlminb} failure details")
      NULL
    }
  )

  # Handle soft failures where nlminb returns an object but the objective
  # value is not finite (e.g., Inf or NaN). In such cases, convergence and
  # message fields may be unreliable, so treat this as a failure and trigger
  # the fallback logic by setting opt to NULL.
  if (!is.null(optimized) && !is.finite(optimized[["objective"]])) {
    cli::cli_warn(c(
      "x" = "Optimization with {.fun stats::nlminb} returned a non-finite
      objective value."
    ))
    return(NULL)
  }

  return(optimized)
}

return_failed_nlminb <- function(object) {
  failed_nlminb_message <- c(
    "x" = "{.fun fit_fims} did not lead to a converged model.",
    "i" = "The resulting coefficients, probability values, or predictions are
    not accurate or stable and should not be used for management.",
    "i" = "A {.cls FIMSFit} object containing partial results is returned.",
    "i" = "Type the name of the variable you assigned it (e.g.,
           {.code fit <- ...}) to inspect the model inputs and configuration.",
    "i" = "Run {.code sanity_fims(fit)} to see each convergence check, or
           {.code get_convergence(fit)} for the same checks as a tibble.",
    "i" = "To check the starting values, run
           {.code fit_fims(optimize = FALSE)} and look at
           {.code get_report(fit)}."
  )
  cli::cli_warn(message = failed_nlminb_message)
  # Construct a fallback optimizer result with consistent structure, i.e.,
  # convergence = 1L indicates non-convergence
  return(list(
    run_time = c(
      time_optimization = as.difftime(0, units = "secs"),
      time_sdreport = as.difftime(0, units = "secs"),
      time_total = as.difftime(0, units = "secs")
    ),
    opt = list(
      par = object[["env"]][["last.par.best"]][object[["env"]][["lfixed"]]()],
      objective = NA_real_,
      convergence = 1L,
      message = "Optimization failed"
    )
  ))
}

#' Build the FIMSFit returned when nlminb fails
#'
#' @param input Input list as returned by [initialize_fims()].
#' @param obj The RTMB object used for fitting.
#' @return
#' A FIMSFit object with the partial results and the convergence checks. The
#' report is from the last parameters nlminb tried, so the `"nll"` check names
#' the negative log-likelihood components that were not finite there.
#' @noRd
failed_fims_fit <- function(input, obj) {
  failed_nlminb_object <- return_failed_nlminb(obj)
  fit <- FIMSFit(
    input = input,
    obj = obj,
    opt = failed_nlminb_object[["opt"]],
    sdreport = list(),
    run_time = failed_nlminb_object[["run_time"]]
  )
  fit@convergence <- dplyr::bind_rows(
    convergence_tibble(
      check = "optimizer",
      severity = "FAIL",
      value = 1,
      threshold = 0,
      message = "nlminb failed or returned an objective that is not finite."
    ),
    check_nll_convergence(get_opt(fit), get_report(fit), input[["model_data"]]),
    convergence_tibble(
      check = "sdreport",
      severity = "NOTE",
      message = "Hessian not checked because the optimization failed."
    )
  )
  fit
}

#' Name the sdreport's parameters, ending in the parameter id
#'
#' Each estimated value is named "<name>.<parameter id>", so the sdreport's
#' rows can be matched to get_estimates(). Values that share 1 estimate take
#' the id of the first.
#'
#' @param sdreport The sdreport, or an empty list.
#' @param parameter_table The `parameter_table` from initialize_fims().
#' @noRd
name_by_parameter_id <- function(sdreport, parameter_table) {
  if (length(sdreport) == 0) {
    return(sdreport)
  }
  fixed_names <- parameter_labels(
    names(sdreport[["par.fixed"]]),
    parameter_table
  )
  names(sdreport[["par.fixed"]]) <- fixed_names
  dimnames(sdreport[["cov.fixed"]]) <- list(fixed_names, fixed_names)
  if (length(sdreport[["par.random"]]) > 0) {
    names(sdreport[["par.random"]]) <- parameter_labels(
      names(sdreport[["par.random"]]),
      parameter_table
    )
  }
  sdreport
}

#' Label each estimated value by its parameter id
#'
#' The label is "<name>.<parameter id>", e.g., `"Fleet.fleet1.log_Fmort.6"`,
#' the same in the sdreport and the convergence checks. Values that share 1
#' estimate take the id of the first.
#'
#' @param rtmb_names RTMB parameter names, 1 per estimated value, in RTMB's
#'   order.
#' @param parameter_table The `parameter_table` from initialize_fims().
#' @noRd
parameter_labels <- function(rtmb_names, parameter_table) {
  # RTMB repeats a parameter's name for each estimated value, so the position
  # within the name is the map level stored in rtmb_level.
  level <- stats::ave(seq_along(rtmb_names), rtmb_names, FUN = seq_along)
  row <- match(
    paste(rtmb_names, level),
    paste(parameter_table[["rtmb_name"]], parameter_table[["rtmb_level"]])
  )
  paste(rtmb_names, parameter_table[["parameter_id"]][row], sep = ".")
}

#' The get_estimates() table as JSON, for the `model_output` slot
#'
#' @param estimates The table from estimates_table().
#' @noRd
estimates_to_json <- function(estimates) {
  # na = "string" writes Inf, -Inf and NaN as strings, so they survive the
  # round trip. A failed likelihood (-Inf) would otherwise read back as NA.
  as.character(jsonlite::toJSON(
    estimates,
    dataframe = "columns", digits = NA, na = "string"
  ))
}

#' The get_estimates() table from the `model_output` slot, with each column's
#' type
#'
#' @param json The `model_output` slot.
#' @noRd
estimates_from_json <- function(json) {
  columns <- jsonlite::fromJSON(json, simplifyVector = TRUE)
  # A fit saved by an earlier version of FIMS holds JSON of another form.
  if (!all(c("label", "estimated") %in% names(columns))) {
    cli::cli_abort(c(
      "x" = "The fit's model output is not a table of estimates.",
      "i" = "Fits made with an earlier version of FIMS cannot be read by this
      version. Refit the model."
    ))
  }
  integer_columns <- c("module_id", "type_id", "parameter_id", "year_i")
  character_columns <- c(
    "module_name", "module_type", "label", "type", "fleet",
    "estimation_type", "distribution", "input_type"
  )
  for (name in names(columns)) {
    columns[[name]] <- if (name %in% integer_columns) {
      as.integer(columns[[name]])
    } else if (name %in% character_columns) {
      # na = "string" writes a missing string as "NA".
      dplyr::na_if(as.character(columns[[name]]), "NA")
    } else {
      as.numeric(columns[[name]])
    }
  }
  tibble::as_tibble(columns)
}
