#' Run R integration tests
#'
#' Intended for developers to run the R integration tests.
#' @keywords developer
#' @examples
#' \dontrun{
#' run_r_integration_tests()
#' }
run_r_integration_tests <- function() {
  devtools::test(filter = "integration")
  devtools::test(filter = "parallel")
}

#' Run R unit tests
#'
#' Intended for developers to run the R unit tests.
#' @keywords developer
#' @examples
#' \dontrun{
#' run_r_unit_tests()
#' }
run_r_unit_tests <- function() {
  # Exclude the files returned from the second set because we do not want
  # integration tests which includes integration and parallel tests
  unit_tests <- setdiff(
    list.files(
      path = testthat::test_path(),
      pattern = "^test-.*\\.R$"
    ),
    list.files(
      path = testthat::test_path(),
      pattern = "^test-integration.*|^test-parallel.*"
    )
  )
  # Remove "test-" and ".R" from the file names
  test_files <- gsub("^test-|\\.R$", "", unit_tests)
  # Run unit tests
  # TODO: use purrr::pwalk() to run the tests in parallel.
  purrr::walk(
    test_files,
    \(x) {
      devtools::test(filter = x)
    }
  )
}

#' Remove test data
#'
#' Intended for developers to remove test data to run the tests from fresh.
#' Developers should run this function before testing if changes affect FIMS
#' input or output.
#'
#' @keywords developer
#' @examples \dontrun{
#' remove_test_data()
#' }
remove_test_data <- function() {
  # List the data files starting with "integration-"
  data_to_keep <- list.files(
    path = testthat::test_path("fixtures"),
    pattern = "^integration[-_].*\\.RD",
    full.names = TRUE
  )

  # List all data files
  all_files <- list.files(
    path = testthat::test_path("fixtures"),
    pattern = "\\.RDS$|\\.RData$",
    full.names = TRUE
  )

  # Remove the files that are not data_to_keep
  unlink(
    setdiff(all_files, data_to_keep),
    recursive = TRUE,
    force = TRUE
  )
}
