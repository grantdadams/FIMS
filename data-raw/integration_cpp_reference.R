#' Reference values from the C++ build of FIMS
#'
#' Saves the objective, its gradient, reports and a fit from the C++ build of
#' FIMS (NOAA-FIMS main at the commit the RTMB model starts from) to
#' tests/testthat/fixtures/integration_cpp_reference.RDS. The tests in
#' test-integration-cpp-reference.R check that the RTMB model gives the same
#' numbers.
#'
#' @details
#' * Run from the package root in a new R session:
#'   `Rscript --vanilla data-raw/integration_cpp_reference.R`.
#'   Both builds are called FIMS, so the C++ build cannot be loaded in a
#'   session that has loaded this package.
#' * The C++ build is installed from `cpp_sha` in this repository's history
#'   into a temporary library, which takes a few minutes. To skip that, set
#'   the environment variable FIMS_CPP_LIBRARY to a library that already has
#'   FIMS built from `cpp_sha`.
#' * Configurations, all on `data_big`:
#'   * `default`: setup_default_parameters(), with recruitment deviations as
#'     random effects, at the starting values.
#'   * `von_bertalanffy`: the defaults without weight-at-age data, so growth
#'     is von Bertalanffy-Schnute with a growth-derived age-to-length key, at
#'     the starting values.
#'   * `fixed_effects_fit`: the operating-model values with recruitment
#'     deviations as fixed effects (fit_age_length_comp_fixed_effects in
#'     tests/testthat/helper-integration-tests-setup-run.R), fit with
#'     fit_fims().
#' * Parameters are keyed by `module_name`, `fleet`, `label` and their
#'   position among the estimated values of that parameter, the vocabulary of
#'   the parameter tibble, because C++ and RTMB order and name them
#'   differently.
#'

# The C++ build: NOAA-FIMS main at the commit the RTMB model starts from.
cpp_sha <- "a739d031bb62d04f9366231182682b9ee389c1e2"

###############################################################################
# Load the C++ build
###############################################################################
if ("FIMS" %in% loadedNamespaces()) {
  cli::cli_abort("Run this script in a new R session, with FIMS not loaded.")
}
cpp_library <- Sys.getenv("FIMS_CPP_LIBRARY")
if (!nzchar(cpp_library)) {
  build_dir <- tempfile("fims-cpp-")
  source_dir <- file.path(build_dir, "FIMS")
  cpp_library <- file.path(build_dir, "library")
  dir.create(source_dir, recursive = TRUE)
  dir.create(cpp_library)
  archive <- file.path(build_dir, "FIMS.tar")
  status <- system2(
    "git",
    c("archive", "--format=tar", paste0("--output=", archive), cpp_sha)
  )
  if (status != 0) {
    cli::cli_abort(c(
      "x" = "{.code git archive} failed for {.val {cpp_sha}}.",
      "i" = "Fetch NOAA-FIMS main so the commit exists in this repository."
    ))
  }
  utils::untar(archive, exdir = source_dir)
  utils::install.packages(
    source_dir,
    lib = cpp_library, repos = NULL, type = "source"
  )
}
.libPaths(c(cpp_library, .libPaths()))
library(FIMS)
library(dplyr)
# CreateTMBModel() exists only in the C++ build.
if (!exists("CreateTMBModel", envir = asNamespace("FIMS"))) {
  cli::cli_abort(
    "The FIMS in {.path {find.package('FIMS')}} is not the C++ build."
  )
}

###############################################################################
# Helper functions
###############################################################################
# Key each C++ parameter by the columns of the parameter tibble.
#
# C++ FIMS names each estimated value
# "<module>.<module id>.<label>.<parameter id>". Selectivity and Fleet module
# ids follow the fleet order of the tibble, because initialize_fims() creates 1
# of each per fleet, in that order. The recruitment log_sd belongs to the
# recruitment process's dnorm module. Growth parameters are estimated on the
# log scale under their natural-scale labels, as in the tibble.
cpp_parameter_keys <- function(cpp_names, fleets) {
  parts <- strsplit(cpp_names, ".", fixed = TRUE)
  module <- purrr::map_chr(parts, 1)
  module_id <- as.integer(purrr::map_chr(parts, 2))
  label <- purrr::map_chr(parts, 3)
  known_modules <- c(
    "Selectivity", "Fleet", "Recruitment", "Population", "Growth",
    "Maturity", "dnorm"
  )
  # Only the recruitment process has an estimated dnorm parameter in these
  # configurations.
  has_key <- module %in% known_modules &
    (module != "dnorm" | (label == "log_sd" & sum(module == "dnorm") == 1))
  if (!all(has_key)) {
    cli::cli_abort("No key for C++ parameters {.val {cpp_names[!has_key]}}.")
  }
  tibble::tibble(
    module_name = if_else(module == "dnorm", "Recruitment", module),
    fleet = if_else(
      module %in% c("Selectivity", "Fleet"), fleets[module_id], NA_character_
    ),
    label = label
  ) |>
    # The C++ vector lists the values of a parameter in tibble row order
    # (year, then age), so position counts them in that order.
    mutate(position = seq_len(n()), .by = c(module_name, fleet, label))
}

# Every estimated value of a C++ objective, keyed and labeled fixed or random.
cpp_parameters <- function(obj, parameters) {
  par <- obj[["par"]]
  is_random <- names(par) == "re"
  cpp_names <- character(length(par))
  cpp_names[!is_random] <- names(FIMS::get_parameter_names(par[!is_random]))
  if (any(is_random)) {
    cpp_names[is_random] <- names(FIMS::get_random_names(par[is_random]))
  }
  fleets <- parameters[["fleet"]] |>
    unique() |>
    stats::na.omit()
  cpp_parameter_keys(cpp_names, fleets) |>
    mutate(
      estimation = if_else(is_random, "random_effects", "fixed_effects"),
      value = unname(par)
    )
}

# Report quantities compared with the RTMB model. They cover population
# dynamics, growth and every prediction a likelihood reads. Each is a list
# with 1 element per population or fleet, stored as 1 vector.
report_names <- c(
  "numbers_at_age", "biomass", "spawning_biomass", "expected_recruitment",
  "mortality_Z", "unfished_spawning_biomass", "growth_mean_WAA",
  "growth_mean_LAA", "growth_sd_LAA", "catch_numbers_at_age",
  "catch_expected", "index_expected", "agecomp_proportion",
  "lengthcomp_proportion"
)

# The objective with every parameter treated as fixed (random = NULL), so the
# gradient covers the random effects, and its reports at the starting values.
cpp_at_start <- function(parameters, data) {
  input <- initialize_fims(parameters = parameters, data = data)
  obj <- TMB::MakeADFun(
    data = list(), parameters = input[["parameters"]], random = NULL,
    DLL = "FIMS", silent = TRUE
  )
  values <- cpp_parameters(obj, parameters)
  jnll <- obj[["fn"]](obj[["par"]])
  values[["gradient"]] <- as.vector(obj[["gr"]](obj[["par"]]))
  report <- obj[["report"]]()
  clear()
  list(
    parameters = parameters,
    values = values,
    jnll = jnll,
    nll_components = report[["nll_components"]],
    reports = purrr::map(
      report[report_names], \(x) unlist(x, use.names = FALSE)
    )
  )
}

# The Laplace approximation of the marginal likelihood (recruitment deviations
# integrated out) and its gradient at the starting values.
cpp_laplace_at_start <- function(parameters, data) {
  input <- initialize_fims(parameters = parameters, data = data)
  obj <- TMB::MakeADFun(
    data = list(), parameters = input[["parameters"]], random = "re",
    DLL = "FIMS", silent = TRUE
  )
  objective <- obj[["fn"]](obj[["par"]])
  values <- cpp_parameters(obj, parameters) |>
    mutate(gradient = as.vector(obj[["gr"]](obj[["par"]])))
  clear()
  list(objective = objective, values = values)
}

###############################################################################
# Configurations
###############################################################################
data("data_big", package = "FIMS")
data_4_model <- FIMSFrame(data_big)

## Defaults ----
parameters_default <- setup_default_parameters(data = data_4_model)
default <- cpp_at_start(parameters_default, data_4_model)
default[["laplace"]] <- cpp_laplace_at_start(parameters_default, data_4_model)

## von Bertalanffy-Schnute growth ----
# Without weight-at-age data, setup_default_parameters() chooses
# von Bertalanffy-Schnute growth (choose_default_Growth_module_type()).
data_von_bertalanffy <- data_big |>
  filter(type != "weight_at_age") |>
  FIMSFrame()
von_bertalanffy <- cpp_at_start(
  setup_default_parameters(data = data_von_bertalanffy),
  data_von_bertalanffy
)

## Fixed-effect fit ----
# The operating-model values from the Model Comparison Project, as in
# prepare_test_data() in tests/testthat/helper-integration-tests-setup-run.R.
load(file.path("tests", "testthat", "fixtures", "integration_test_data.RData"))
om_input <- om_input_list[[1]]
om_output <- om_output_list[[1]]
n_years <- get_n_years(data_4_model)
parameters_fixed_effects <- parameters_default |>
  rows_update(
    tibble::tibble(
      fleet = "fleet1",
      label = "log_Fmort",
      timing = seq_len(n_years),
      value = log(om_output[["f"]])
    ),
    by = c("fleet", "label", "timing")
  ) |>
  rows_update(
    tibble::tibble(
      fleet = "survey1",
      label = c("inflection_point", "slope", "log_q"),
      value = c(1.5, 2, log(om_output[["survey_q"]][["survey1"]]))
    ),
    by = c("fleet", "label")
  ) |>
  rows_update(
    tibble::tibble(
      label = "log_devs",
      timing = 2:n_years,
      value = om_input[["logR.resid"]][-1]
    ),
    by = c("label", "timing")
  ) |>
  rows_update(
    tibble::tibble(
      module_name = "Maturity",
      label = c("inflection_point", "slope"),
      value = c(om_input[["A50.mat"]], om_input[["slope.mat"]])
    ),
    by = c("module_name", "label")
  ) |>
  rows_update(
    tibble::tibble(
      label = "log_init_naa",
      age = seq_len(get_n_ages(data_4_model)),
      value = log(om_output[["N.age"]][1, ])
    ),
    by = c("label", "age")
  ) |>
  # Recruitment deviations are fixed effects, and their log_sd is held
  # constant at the operating-model value.
  mutate(
    estimation_type = if_else(
      label == "log_devs" & module_type == "BevertonHolt",
      "fixed_effects",
      estimation_type
    )
  ) |>
  rows_update(
    tibble::tibble(
      module_name = "Recruitment",
      label = "log_sd",
      estimation_type = "constant",
      value = log(om_input[["logR_sd"]])
    ),
    by = c("module_name", "label")
  )

fit <- parameters_fixed_effects |>
  initialize_fims(data = data_4_model) |>
  fit_fims(optimize = TRUE)
obj <- get_obj(fit)
opt <- get_opt(fit)
sdreport <- get_sdreport(fit)
fit_values <- cpp_parameters(obj, parameters_fixed_effects) |>
  mutate(
    estimate = unname(opt[["par"]]),
    standard_error = unname(sqrt(diag(sdreport[["cov.fixed"]])))
  )
# Derived quantities whose standard errors support management advice.
derived_names <- c(
  "spawning_biomass", "spawning_biomass_ratio", "biomass",
  "expected_recruitment"
)
derived <- tibble::tibble(
  name = names(sdreport[["value"]]),
  value = unname(sdreport[["value"]]),
  standard_error = unname(sdreport[["sd"]])
) |>
  filter(name %in% derived_names) |>
  mutate(position = seq_len(n()), .by = name)
fixed_effects_fit <- list(
  parameters = parameters_fixed_effects,
  values = fit_values,
  objective = opt[["objective"]],
  max_gradient = max(abs(obj[["gr"]](opt[["par"]]))),
  derived = derived
)
clear()

###############################################################################
# Save
###############################################################################
integration_cpp_reference <- list(
  cpp_sha = cpp_sha,
  cpp_version = as.character(utils::packageVersion("FIMS")),
  default = default,
  von_bertalanffy = von_bertalanffy,
  fixed_effects_fit = fixed_effects_fit
)

# Saving only when a value changes keeps pull requests from carrying a fixture
# that differs from the last one only by floating-point noise.
fixture_file <- file.path(
  "tests", "testthat", "fixtures", "integration_cpp_reference.RDS"
)
if (!file.exists(fixture_file) ||
  !isTRUE(all.equal(
    integration_cpp_reference,
    readRDS(fixture_file),
    tolerance = sqrt(.Machine[["double.eps"]])
  ))) {
  saveRDS(integration_cpp_reference, file = fixture_file)
}
