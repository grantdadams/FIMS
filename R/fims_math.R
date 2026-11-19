#' Bounded logit and inverse logit
#'
#' FIMS estimates parameters bounded between `a` and `b` on this logit scale,
#' e.g., steepness between 0.2 and 1:
#'   logit(a, b, x) = log(x - a) - log(b - x),
#'   inv_logit(a, b, logit_x) = a + (b - a) / (1 + exp(-logit_x)).
#'
#' @param a,b The lower and upper bounds.
#' @param x A value between `a` and `b`.
#' @param logit_x A value on the logit scale.
#' @return The value on the logit scale (`logit()`), or between `a` and `b`
#'   (`inv_logit()`).
#' @keywords distribution
#' @export
#' @examples
#' logit(0.2, 1, 0.75)
#' inv_logit(0.2, 1, logit(0.2, 1, 0.75))
logit <- function(a, b, x) {
  -log(b - x) + log(x - a)
}

#' @rdname logit
#' @export
inv_logit <- function(a, b, logit_x) {
  a + (b - a) / (1 + exp(-logit_x))
}

#' Logistic function
#'
#' 1 / (1 + exp(-slope * (x - inflection_point))): 0.5 at the inflection
#' point, rising toward 1 for a positive slope and falling toward 0 for a
#' negative one. Used by logistic maturity and selectivity.
#'
#' @param inflection_point The value of `x` where the function is 0.5.
#' @param slope The slope parameter, positive for a rising curve. The curve's
#'   slope at the inflection point is slope / 4.
#' @param x Values, e.g., ages.
#' @return The logistic function at each `x`.
#' @noRd
logistic <- function(inflection_point, slope, x) {
  1 / (1 + exp(-slope * (x - inflection_point)))
}

#' Smooth absolute value and maximum
#'
#' Numerical guards with derivatives everywhere, unlike abs() and max().
#' `ad_fabs(x) = sqrt(x^2 + C)` is about `|x| + C / (2 |x|)`, and
#' `ad_max(a, b) = (a + b + ad_fabs(a - b)) / 2`. The cost is a small shift
#' even where no guard is needed.
#'
#' @param x,a,b Values.
#' @param C A constant above 0 that sets how smooth the result is. `ad_max()`
#'   passes it to `ad_fabs()`, which stops if it is not above 0.
#' @return The smooth absolute value of `x` (`ad_fabs()`), or the smooth
#'   maximum of `a` and `b` (`ad_max()`).
#' @noRd
ad_fabs <- function(x, C = 1e-5) {
  # C = 0 gives the exact |x|, which has a NaN derivative at x = 0. C < 0
  # gives NaN when |x| is small. C is a fixed setting, not an estimated
  # value, so this `if` is safe on the tape.
  if (!isTRUE(C > 0)) {
    cli::cli_abort(c(
      "x" = "The smoothing constant {.arg C} is {.val {C}}, which is not
      greater than 0."
    ))
  }
  sqrt(x * x + C)
}

#' @rdname ad_fabs
#' @noRd
ad_max <- function(a, b, C = 1e-5) {
  (a + b + ad_fabs(a - b, C)) * 0.5
}
