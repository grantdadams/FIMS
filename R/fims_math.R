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
#' @param slope Steepness of the curve at the inflection point, positive for a
#'   rising curve.
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
#' @param C The smoothing constant.
#' @return The smooth absolute value of `x` (`ad_fabs()`), or the smooth
#'   maximum of `a` and `b` (`ad_max()`).
#' @noRd
ad_fabs <- function(x, C = 1e-5) {
  sqrt(x * x + C)
}

#' @rdname ad_fabs
#' @noRd
ad_max <- function(a, b, C = 1e-5) {
  (a + b + ad_fabs(a - b, C)) * 0.5
}
