#' Distributions not available in the stats package
#'
#' Family objects provide a convenient way to specify the details of the models
#' used by functions such as [stats::glm()]. These functions within this
#' package are not available within the stats package but are designed in a
#' similar manner.
#'
#' @param link A string specifying the model link function. For example,
#'   `"identity"` or `"log"` are appropriate names for the [stats::gaussian()]
#'   distribution. `"log"` and `"logit"` are the defaults for the lognormal and
#'   the multinomial, respectively.
#' @return
#' An object of class `family` (which has a concise print method). This
#' particular family has a truncated length compared to other distributions in
#' [stats::family()].
#' \item{family}{character: the family name.}
#' \item{link}{character: the link name.}
#'
#' @seealso
#' * [stats::family()]
#' * [stats::gaussian()]
#' * [stats::glm()]
#' * [stats::power()]
#' * [stats::make.link()]
#' @keywords distribution
#' @export
#' @examples
#' a_family <- multinomial()
#' a_family[["family"]]
#' a_family[["link"]]
lognormal <- function(link = "log") {
  family_class <- c(
    list(family = "lognormal", link = link),
    stats::make.link(link)
  )
  class(family_class) <- "family"
  return(family_class)
}

#' @rdname lognormal
#' @keywords distribution
#' @export
multinomial <- function(link = "logit") {
  family_class <- c(
    list(family = "multinomial", link = link),
    stats::make.link(link)
  )
  class(family_class) <- "family"
  return(family_class)
}
