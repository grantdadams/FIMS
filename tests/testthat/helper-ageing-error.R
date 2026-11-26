# Helper data for tests of ageing error ----
# Used by test-validate_ageing_error.R, test-integration-ageing-error.R and
# test-integration-retape.R.

# The ages of data_big, which are the true ages of the ageing error matrix
# below.
data_big_ages <- sort(unique(stats::na.omit(FIMS::data_big[["age"]])))

# An ageing error matrix for data_big. Each age is read correctly with
# probability 0.7 and 1 year too old with probability 0.3, and the plus group
# is always read correctly. The matrix is not symmetric, so using it
# transposed would give different compositions. Rows are true ages and
# columns are read ages, so reading 1 year too old is the cell to the right
# of the diagonal.
reads_older <- diag(0.7, length(data_big_ages))
reads_older[row(reads_older) + 1 == col(reads_older)] <- 0.3
reads_older[nrow(reads_older), ncol(reads_older)] <- 1

#' Make `ageing_error` rows from an ageing error matrix
#'
#' @param matrix An ageing error matrix with true ages of `data_big` as rows
#'   and read ages as columns.
#' @param fleet The fleet the matrix is for, or `NA` for every fleet.
#' @param timing The year the matrix is for, or `NA` for every year.
#' @param observed_ages The read ages of the columns. A matrix can have read
#'   ages outside the ages of `data_big`.
#'
#' @return A tibble with the columns of `data_big`.
ageing_error_rows <- function(matrix = reads_older, fleet = NA_character_,
                              timing = NA_real_,
                              observed_ages = data_big_ages) {
  tibble::tibble(
    type = "ageing_error",
    fleet = fleet,
    age = rep(observed_ages, times = nrow(matrix)),
    length = NA_real_,
    timing = timing,
    observed = as.vector(t(matrix)),
    unit = "proportion",
    uncertainty = as.character(rep(data_big_ages, each = ncol(matrix)))
  )
}
