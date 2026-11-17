#' FIMS input data frame
#'
#' A data frame containing the information needed to run an age-structured stock
#' assessment model in FIMS. This data was generated using
#' the `ASSAMC` package written for the [model comparison project](
#' https://github.com/NOAA-FIMS/Age_Structured_Stock_Assessment_Model_Comparison).
#' The source code to make this dataset is in `data-raw/data_big.R` which is on
#' GitHub but will not be in your local installation because it is in the
#' .Rbuildignore file.
#'
#' @format
#' A data frame with `r NROW(data_big)` observations of `r NCOL(data_big)`
#' variables:
#' \describe{
#'   \item{type}{The type of input the row contains. Allowed types include
#'   `r glue::glue_collapse(fims_input_types, sep = ", ", last = ", and ")`.}
#'   \item{fleet}{A character string providing the name of the information
#'     source that the input was collected from, e.g., `"Trawl fishery"` or
#'     `"age_zero_survey"`. Entries can be `NA` for information that is not
#'     fleet/vessel specific.}
#'   \item{age}{An integer age. Entry can be `NA` if information pertains to
#'     multiple ages, e.g., total catch rather than catch of age-4 fish.
#'     In this dataset, ages start at one, therefore recruitment happens at age
#'     one not age zero.}
#'   \item{length}{A numeric length. Entry can be `NA` if information doesn't
#'     pertain to length. For `length_comp`, `length_bin`, and
#'     `age_to_length_conversion` rows, this is the lower edge of the length
#'     bin. A bin holds lengths from its lower edge up to the next lower edge.
#'     When growth is estimated in FIMS, the first bin also holds all smaller
#'     fish and the last bin holds all larger fish. With fixed
#'     `age_to_length_conversion` rows, those rows set what each bin holds. In
#'     this dataset, the bins are 50 mm wide, so `length = 250` is the bin from
#'     250 to 300 mm, and the conversion puts all fish under 50 mm in the first
#'     bin and all fish of 1100 mm or more in the last. `length_bin` rows with
#'     `fleet = NA` and `timing = NA` set the population length bins (see
#'     `vignette("fims-input-data", package = "FIMS")`).}
#'   \item{timing}{The timing, i.e., year the data was collected.}
#'   \item{observed}{The observed measurement of interest.}
#'   \item{unit}{A character string specifying the units of `observed`. Allowed
#'     units for each data type are as follows. `mt` or `number` are used for
#'     `index` and `catch` data, with 1 unit per fleet for each type. `mt` is
#'     used for `weight_at_age` data. `number` or `proportion` are each viable
#'     units for the composition data, where the former is the preferred unit
#'     of measurement. `proportion` is also used for `ageing_error` data, the
#'     probability that a fish of a given true age is read as the age in
#'     `age`.}
#'   \item{uncertainty}{A character string with a right-handed formula
#'     specifying the distributional assumptions for the entry in `observed`.
#'     See [FIMSFrame()] for more information about this column. For
#'     `ageing_error` rows, the string is the true age, e.g., `"5"`.
#' }
#' }
#' @source \url{https://github.com/NOAA-FIMS/Age_Structured_Stock_Assessment_Model_Comparison}
#' @seealso
#' * [FIMSFrame()]
"data_big"

#' FIMS parameter tibble
#'
#' A tibble of default FIMS parameters updated with operating-model values for
#' the example data set. The parameter table uses the same columns and
#' structure returned by [setup_default_parameters()].
#'
#' @source \url{https://github.com/NOAA-FIMS/Age_Structured_Stock_Assessment_Model_Comparison}
#' @seealso [setup_default_parameters()]
"parameters_big"

#' FIMS fit using the example data
#'
#' Precomputed [FIMSFit] objects for the example data, with and without
#' optimization. Both objects have the same slots; the optimized fit includes
#' uncertainty information when available.
#'
#' @format An object of class `FIMSFit`.
#' @seealso [fit_fims()]
"fit_big_optimized"

#' FIMS estimates for the example data
#'
#' Precomputed estimate tibbles returned by [get_estimates()] for the example
#' data, with and without optimization.
#'
#' @format A tibble returned by [get_estimates()].
#' @seealso [get_estimates()]
"estimates_big_optimized"

#' Surplus production input data frame
#'
#' Catch and catch per unit effort (CPUE) of South Atlantic albacore tuna from
#' 1967 to 1989, as used by Meyer and Millar (1999) and Best and Punt (2020)
#' to demonstrate a state-space surplus production model. It has the same
#' columns as [data_big], with `NA` ages and lengths. Catch (`fleet1`) is in
#' metric tons. Catch has no `uncertainty`, because the surplus production
#' model treats catch as known. The index (`survey1`) is CPUE in kg per 100
#' hooks, with a lognormal `uncertainty` of `sdlog = 0.1`. Its `unit` is
#' `"mt"` because [FIMSFrame()] accepts only `"mt"` or `"number"` for an
#' index. Catchability scales the index to biomass, so the unit of the index
#' does not change the fit. The source code to make this dataset is in
#' `data-raw/data_limited_tuna.R`.
#'
#' @format
#' A data frame with `r NROW(data_limited_tuna)` observations of
#' `r NCOL(data_limited_tuna)` variables, described in [data_big].
#' @source Meyer, R., and Millar, R.B. 1999. BUGS in Bayesian stock
#'   assessments. Canadian Journal of Fisheries and Aquatic Sciences 56:
#'   1078-1086.
#'
#'   Best, J.K., and Punt, A.E. 2020. Parameterizations for Bayesian
#'   state-space surplus production models. Fisheries Research 222: 105411.
#' @seealso
#' * [data_limited_tuna_results]
#' * [setup_default_parameters()]
"data_limited_tuna"

#' Posterior medians of a Stan fit to the tuna data
#'
#' Posterior medians from a Stan fit of the Meyer and Millar (1999) Schaefer
#' surplus production model (shape of 2) to [data_limited_tuna], with their
#' priors. The labels are `growth_rate`, `carrying_capacity` (metric tons),
#' `sigma2_obs` (the variance of the log index), `sigma2_depletion` (the
#' variance of the log depletion process), `depletion` (1 per year and the
#' year after the last year) and `q` (catchability, in kg per 100 hooks per
#' metric ton of biomass). The source code is in
#' `data-raw/data_limited_tuna.R`.
#'
#' @format A data frame with columns `label` and `median`.
#' @seealso
#' * [data_limited_tuna]
"data_limited_tuna_results"

#' Male-only snow crab input data frame
#'
#' Data based on the 2025 GMACS assessment model of male eastern Bering Sea
#' snow crab (*Chionoecetes opilio*), 1982 to 2025, for the catch-at-length
#' model (`model_type = "CatchAtLength"`). It has the columns of [data_big]
#' and a `maturity` column (`"immature"`, `"mature"`, or `NA` for both).
#' Length is the lower edge of 22 carapace width bins from 25 to 135 mm. The
#' fleets are the directed pot fishery (`Pot_Fishery`, retained and discarded
#' catch), groundfish trawl bycatch (`Trawl_Bycatch`), and the NMFS bottom
#' trawl survey before and after its 1989 change (`NMFS_Trawl_1982`,
#' `NMFS_Trawl_1989`). Catch and the survey index are in metric tons, and
#' weight at length in metric tons per crab. It also has molt increments (mm)
#' by premolt length (`growth_increment`) and the terminal molt probability
#' at postmolt length by year (`terminal_molt_probability_at_length`). Length
#' compositions use the robust normal approximation to the multinomial, as
#' in GMACS. Catch compositions from the 2 years with no pot fishery (2022
#' and 2023) are left out. The source code to make this dataset, and the
#' GMACS files it reads, are in `data-raw/data_snow_crab.R` and
#' `data-raw/snow_crab`.
#'
#' @format
#' A data frame with `r NROW(data_snow_crab)` observations of
#' `r NCOL(data_snow_crab)` variables.
#' @source `Models/26_gmacs_male_only` in
#'   \url{https://github.com/szuwalski/snow_sept} at commit
#'   `b49b3f1ea78fa100c47de223dd560a71ac1dbd1f`.
#' @seealso
#' * [parameters_snow_crab]
#' * [data_snow_crab_results]
"data_snow_crab"

#' Male-only snow crab parameters at the GMACS estimates
#'
#' The parameter tibble for [data_snow_crab], with every value set to its
#' GMACS estimate. Numbers are in crabs, so log recruitment and log initial
#' numbers at length are `log(1000)` larger than in GMACS, which counts
#' thousands of crabs. Survey selectivity at length is on the logit scale.
#' The GMACS smoothness penalties are random walks by length with fixed sd.
#' Log recruitment has an AR1 process with fixed sd and rho. The GMACS
#' normal priors are on the natural scale of each parameter.
#'
#' @format A tibble with the structure returned by
#'   [setup_default_parameters()].
#' @seealso
#' * [data_snow_crab]
#' * [data_snow_crab_results]
"parameters_snow_crab"

#' Reference values for the male-only snow crab model
#'
#' Values from an R version of the GMACS male-only snow crab model, which
#' reproduces the GMACS objective at the GMACS estimates. `model` is
#' `"gmacs"` for the R version in its GMACS form at the GMACS estimates,
#' `"gmacs_par"` for the FIMS form at the GMACS estimates
#' ([parameters_snow_crab]), and `"fit"` for the FIMS form fit with nlminb to
#' the same data as [data_snow_crab]. The FIMS form differs from GMACS in the
#' ways listed in `data-raw/data_snow_crab.R`. `label` is `objective` (the
#' negative log likelihood plus penalties and priors, as the R version of
#' GMACS computes it, not the FIMS objective), `spawning_biomass` (mature
#' male biomass in metric tons at the start of the third season),
#' `expected_recruitment` (crabs), `catch_retained_expected` and
#' `catch_discarded_expected` (metric tons), or `index_expected` (survey
#' biomass of mature males in metric tons).
#'
#' @format A tibble with columns `model`, `label`, `year`, `value`, and
#'   `fleet`. `fleet` is `NA` for the objective, spawning biomass and
#'   recruitment.
#' @seealso
#' * [data_snow_crab]
#' * [parameters_snow_crab]
"data_snow_crab_results"
