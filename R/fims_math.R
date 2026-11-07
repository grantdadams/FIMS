#' Bounded logit and inverse logit
#'
#' FIMS estimates parameters bounded between `a` and `b` on this logit scale,
#' e.g. steepness between 0.2 and 1:
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
