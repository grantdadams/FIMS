# :test_tube: Testing cheat sheet

FIMS uses [{testthat}](https://testthat.r-lib.org/) for its tests, following the guidance in the [testing chapter of R packages](https://r-pkgs.org/tests.html).

## Adding a new test

### :scroll: Create a test file using `FIMS:::use_testthat_template()`

- Calling [`FIMS:::use_testthat_template()`](https://noaa-fims.github.io/FIMS/reference/use_testthat_template.html) will create a new R test file, e.g., `tests/testthat/test-function_name.R`, for {testthat} following the [tidyverse test conventions](https://style.tidyverse.org/tests.html).

### :floppy_disk: Prepare test data for the new test

Prepare the test data in the new file or in a separate file if you plan on reusing the data in multiple test files.

- In-line test data: make the data directly in the new test file within the `setup` section.
- Data used by several test files: define it once in a `tests/testthat/helper-*.R` file, e.g., `small_parameters` in `helper-small-parameters.R`.
- R reusable test data: add code to the `prepare_test_data()` function in [`tests/testthat/helper-integration-tests-setup-run.R`](https://github.com/NOAA-FIMS/FIMS/blob/main/tests/testthat/helper-integration-tests-setup-run.R), including code to save the new data object, e.g., `base::saveRDS(object, file = testthat::test_path("fixtures", "data_name.RDS"))`, and call `prepare_test_data()` in the `setup` section of the new test file to load the data using
  ```r
  if (!file.exists(testthat::test_path("fixtures", "data_name.RDS"))) {
    prepare_test_data()
  }
  ```
- Use pre-existing integration data, e.g., `tests/testthat/fixtures/integration_test_data_components.RData` and `tests/testthat/fixtures/integration_test_data.RData`, by loading them within the `setup` section, e.g., `load(testthat::test_path("fixtures", "integration_test_data.RData"))` or within `prepare_test_data()`, where these data objects can be updated by running `data-raw/data_big.R`.
- Use pre-existing package data, e.g., `FIMS::data_big`, `FIMS::parameters_big`, `FIMS::fit_big_optimized`, and `FIMS::estimates_big_optimized`, by referring to them directly in tests. Update these objects by running `source(file.path("data-raw", "data_big.R"))` after changing the model or estimate-generation code. The fits are only saved again when their version or estimates change, so delete `data/fit_big_optimized.rda` to force a new save.

### :pencil: Edit the code in the new test file

Follow the structure of the new test file to write appropriate correctness, edge-handling, and error-handling tests.

#### :hammer: Helper functions to set up tests

The following :hammer: helper functions are available to assist with writing integration tests in R.

- `setup_and_run_FIMS_with_wrappers()`: Set up and fit a FIMS model with the wrapper functions (in [`tests/testthat/helper-integration-tests-setup-function.R`](https://github.com/NOAA-FIMS/FIMS/blob/main/tests/testthat/helper-integration-tests-setup-function.R)).
- `verify_fims_deterministic()`: Compare the model output from a FIMS deterministic run against the expected "truth" from an operating model (in [`tests/testthat/helper-integration-tests-validation.R`](https://github.com/NOAA-FIMS/FIMS/blob/main/tests/testthat/helper-integration-tests-validation.R)).
- `verify_fims_nll()`: Compare the negative log likelihood (NLL) from a FIMS model against the expected NLL calculated from an operating model (in [`tests/testthat/helper-integration-tests-validation.R`](https://github.com/NOAA-FIMS/FIMS/blob/main/tests/testthat/helper-integration-tests-validation.R)).
- `validate_fims()`: Compare the output from a FIMS model where parameters are estimated against the expected "truth" from an operating model (in [`tests/testthat/helper-integration-tests-validation.R`](https://github.com/NOAA-FIMS/FIMS/blob/main/tests/testthat/helper-integration-tests-validation.R)).

#### :shushing_face: Skipping tests

The following {testthat} functions can be used at the beginning of a test file to skip tests under certain conditions:

- Add [`testthat::skip_on_ci()`](https://testthat.r-lib.org/reference/skip.html) at the beginning of a test file to skip it during continuous integration runs.
- Add [`testthat::skip_on_covr()`](https://testthat.r-lib.org/reference/skip.html) at the beginning of a test file to skip it during coverage calculation.
- Add `testthat::skip_if(Sys.getenv("RUN_SLOW_TESTS") != "true", "Skipping: RUN_SLOW_TESTS is not set to true.")` at the beginning of a test file to run expensive integration tests only when `RUN_SLOW_TESTS=true`. The slow-test GitHub Actions workflow sets this variable; locally, set it before calling `devtools::test()` to run those tests.

#### :mute: Suppressing messages

- Messages for FIMS are suppressed globally in `tests/testthat.R` to keep test reports clean in the console and on GitHub Actions. If a specific function within a test still generates messages, wrap the call in `base::suppressMessages()` to silence its output.

## :computer: Run tests

### R

- Run [`devtools::load_all()`](https://devtools.r-lib.org/reference/load_all.html) and [`devtools::test()`](https://devtools.r-lib.org/reference/test.html) to execute all tests in the package.
- Run [`devtools::test(filter = "test_file_name")`](https://devtools.r-lib.org/reference/test.html) to execute a specific test file.
- Run [`devtools::install()`](https://devtools.r-lib.org/reference/install.html) before running `test-parallel*-.R`.
- [`FIMS:::run_r_unit_tests()`](https://noaa-fims.github.io/FIMS/reference/run_r_unit_tests.html): Run all R unit tests in the package.
- [`FIMS:::run_r_integration_tests()`](https://noaa-fims.github.io/FIMS/reference/run_r_integration_tests.html): Run all R integration tests in the package.
- [`FIMS:::remove_test_data()`](https://noaa-fims.github.io/FIMS/reference/remove_test_data.html): Remove test data from `tests/testthat/fixtures` to run the tests from fresh if changes in the core code affect FIMS input or output.

## :bug: Debug tests

### R

- A call to `base::browser()` can be included in the body of a function to pause execution of a test and allow for interactive debugging.
- Use `?debug` to learn how to set, unset, or query the debugging flag on a function.
- Visit the [Debugging Guide](https://support.posit.co/hc/en-us/articles/205612627-Debugging-with-the-RStudio-IDE) for instructions on debugging with the RStudio IDE.

### Long-running or crashing tests

If a test crashes or hangs, it helps to save the output so you can see which test was running last. See the help file for [`LocationReporter`](https://testthat.r-lib.org/reference/LocationReporter.html) from {testthat} to understand how the following line helps:

```bash
Rscript -e "devtools::test(reporter = 'location')" > test_log.txt 2>&1
```

You can also set the following environment variables to for better diagnostics:

```bash
R_KEEP_PKG_SOURCE=yes R_BACKTRACE_ON_ERROR=full TESTTHAT_PROGRESS=plain \
  Rscript -e "devtools::test(reporter = 'location')" > test_log.txt 2>&1
```

### Random numbers

Failed tests might arise because different random numbers are used or the order of the simulation changes through model development (see [FIMS-planning discussion 25](https://github.com/NOAA-FIMS/FIMS-planning/issues/25)). One can try the following solutions:
  - Add a TRUE/FALSE parameter in each FIMS simulation module for setting up testing seed. When testing the module, set the parameter to TRUE to fix the seed number in R and conduct tests.
  - Use `set.seed()` and investigate using [{rstream}](https://www.iro.umontreal.ca/~lecuyer/myftp/papers/rstream.pdf) to generate multiple streams of random numbers to associate distinct streams of random numbers with different sources of randomness. [{rstream}](http://www-labs.iro.umontreal.ca/~lecuyer/myftp/streams00/) was specifically designed to address the issue of needing very long streams of pseudo-random numbers for parallel computations.

## :raising_hand: Need help?

If any of the instructions above do not work as expected, please [report an issue on GitHub](https://github.com/NOAA-FIMS/FIMS/issues).

### :book: Definitions

- Unit test are for individual functions to test each of the smallest components.
- Functional tests are created for individual modules to ensure that the application of small components function within the requirements.
- Integration tests verify that modules work well together.
- Run-time tests include checks that are integrated into the code to catch errors related to user input.
- Platform compatibility tests are executed before releases to ensure that previously tested pieces are still performing after the code has been changed on all platforms of interest.
- Beta-testing is used to gather feedback from users during the pre-release stage.
- One-off testing is used for replicating and fixing user-reported bugs. Some of these one-off tests may eventually be integrated into the permanent testing framework.