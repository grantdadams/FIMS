# Expected composition data
#
# Matrices have 1 row per model year. Age compositions have 1 column per
# age, length compositions 1 column per length bin.
#
# Each function returns `proportion` (rows sum to 1, and this is what the
# multinomial uses) and `expected`, reported as agecomp_expected and
# lengthcomp_expected. When the fleet has that data, `expected` is the
# proportions times the observed sample size in each year. Otherwise it is
# the expected numbers at age for an age composition, and the expected
# proportions at age converted to length for a length composition.
#
# Age-to-length keys and ageing-error matrices are either 1 matrix used in
# every year or a list with 1 matrix per model year.

#' Expected age composition
#'
#' Proportions at age come from the fleet's expected numbers at age: catch
#' numbers if the fleet has catch data, otherwise index numbers. The
#' proportions are also scaled to the observed sample size in each year. This
#' only matters for reporting, because the multinomial uses the proportions.
#'
#' @param numbers_at_age Expected numbers at age for the fleet, years by ages.
#' @param observed Observed age composition counts, or NULL if the fleet has
#'   none.
#' @return A list with `proportion` and `expected`.
#' @noRd
evaluate_age_comp <- function(numbers_at_age, observed = NULL) {
  proportion <- numbers_at_age / RTMB::rowSums(numbers_at_age)
  if (is.null(observed)) {
    return(list(proportion = proportion, expected = numbers_at_age))
  }
  sample_size <- rowSums(observed, na.rm = TRUE)
  list(proportion = proportion, expected = proportion * sample_size)
}

#' Move an age composition from true age to observed (read) age
#'
#' at observed age = at true age `%*%` A, where `A[true, observed]` is the
#' probability a fish of the true age is read as the observed age. Rows of A
#' sum to 1, so totals do not change.
#'
#' @param at_true_age Matrix, 1 row per year (or CAAL row), by true age.
#' @param ageing_error 1 matrix, or a list with 1 matrix per model year.
#' @param year Model year of each row of `at_true_age`.
#' @noRd
apply_ageing_error <- function(at_true_age, ageing_error,
                               year = seq_len(nrow(at_true_age))) {
  by_row(at_true_age, ageing_error, year)
}

#' Convert numbers or proportions at age to length with an age-to-length key
#'
#' at length = at age `%*%` K, where `K[age, l]` = P(length bin l | age).
#'
#' @param at_age Matrix, 1 row per year, by age.
#' @param age_to_length 1 key, or a list with 1 key per model year.
#' @noRd
apply_age_to_length_conversion <- function(at_age, age_to_length, year = seq_len(nrow(at_age))) {
  by_row(at_age, age_to_length, year)
}

# Multiply each row by the matrix for its year (or by 1 matrix for all rows).
by_row <- function(rows, matrices, year) {
  if (is.matrix(matrices)) {
    return(rows %*% matrices)
  }
  do.call(rbind, lapply(seq_along(year), function(i) {
    rows[i, , drop = FALSE] %*% matrices[[year[i]]]
  }))
}

#' Expected conditional age-at-length (CAAL)
#'
#' The age composition of fish within 1 length bin, in 1 year. Fish
#' caught at age a fall in length bin l with probability `K[a, l]`, so the
#' expected number at age a and length l is `E[y, a] * K[a, l]`. Dividing by the
#' total over ages in that bin gives P(age | length):
#'   `p(a | l, y) = E[y, a] K[a, l] / sum over a' of E[y, a'] K[a', l]`.
#' Selectivity at age is already inside E. With ageing error, the true ages
#' are then moved to observed (read) ages.
#'
#' @param numbers_at_age Expected numbers at age for the fleet, years by ages
#'   (the same numbers the age composition uses).
#' @param age_to_length The fleet's age-to-length key (1, or 1 per year).
#' @param year,length_bin Model-year index (1 = first model year) and
#'   length-bin index of each CAAL row.
#' @param ageing_error The fleet's ageing-error matrix or matrices, or NULL.
#' @return Matrix of P(age | length), 1 row per CAAL row.
#' @noRd
expected_conditional_age_at_length <- function(numbers_at_age, age_to_length,
                                               year, length_bin,
                                               ageing_error = NULL) {
  numbers <- numbers_at_age[year, , drop = FALSE]
  probability_of_bin <- if (is.matrix(age_to_length)) {
    t(age_to_length[, length_bin, drop = FALSE])
  } else {
    t(vapply(seq_along(year), function(i) {
      age_to_length[[year[i]]][, length_bin[i]]
    }, numeric(nrow(age_to_length[[1]]))))
  }
  at_age_and_length <- numbers * probability_of_bin
  proportion <- at_age_and_length / RTMB::rowSums(at_age_and_length)
  if (is.null(ageing_error)) {
    return(proportion)
  }
  apply_ageing_error(proportion, ageing_error, year)
}

#' Expected length composition
#'
#' Expected proportions at age are converted to length with the age-to-length
#' key, P(length bin | age), then normalized again so each year sums to 1:
#'   `L[y, l] = sum over a of P[y, a] * K[a, l]`.
#'
#' @param age_proportion Expected proportions at (true) age, years by ages.
#' @param age_to_length Age-to-length key (1, or 1 per year).
#' @param observed Observed length composition counts, or NULL.
#' @return A list with `proportion` and `expected`.
#' @noRd
evaluate_length_comp <- function(age_proportion, age_to_length,
                                 observed = NULL) {
  at_length <- apply_age_to_length_conversion(age_proportion, age_to_length)
  proportion <- at_length / RTMB::rowSums(at_length)
  if (is.null(observed)) {
    return(list(proportion = proportion, expected = at_length))
  }
  sample_size <- rowSums(observed, na.rm = TRUE)
  list(proportion = proportion, expected = proportion * sample_size)
}
