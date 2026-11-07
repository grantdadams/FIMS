#' Clear the model from memory
#'
#' The model keeps nothing in memory between fits, so `clear()` does
#' nothing. Scripts that call it before or after a fit still run.
#'
#' @return `NULL`, invisibly.
#' @keywords fit_fims
#' @export
#' @examples
#' clear()
clear <- function() {
  invisible(NULL)
}
