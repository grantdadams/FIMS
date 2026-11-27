# Internal helper: validate numeric type and allowed lengths.
assert_numeric_length <- function(x, x_name, valid_lengths, requirement) {
  if (!is.numeric(x) || !length(x) %in% valid_lengths) {
    local_bullets <- c(
      "i" = "{.var {x_name}} argument must be {requirement}.",
      "x" = "{.var {x_name}} has a length of {length(x)}.",
      "x" = "{.var {x_name}} is of the class {class(x)}."
    )
    names(local_bullets)[2] <- ifelse(length(x) > max(valid_lengths), "x", "i")
    names(local_bullets)[3] <- ifelse(inherits(x, "numeric"), "i", "x")
    cli::cli_abort(local_bullets)
  }
}

#' Convert snake_case strings to PascalCase
#'
#' This function takes a vector of strings in snake_case format and converts
#' them to PascalCase.
#'
#' @param snake_strings A vector of strings in snake_case format.
#' @return A vector of strings in PascalCase format.
#' @examples
#' snake_to_pascal(c("age_comp", "length_comp"))
#' snake_to_pascal("index")
#' @noRd
snake_to_pascal <- function(snake_strings) {
  purrr::map_chr(snake_strings, \(x) {
    parts <- strsplit(x, "_")[[1]]
    paste(
      toupper(substring(parts, 1, 1)),
      substring(parts, 2),
      sep = "",
      collapse = ""
    )
  })
}

#' A list that stops on a missing name
#'
#' The model reads its parameters and data by name, and a plain list gives
#' NULL for a misspelled name. `[[` and `$` on a strict list stop with an
#' error instead, and `$` does not match the start of a longer name. Lists
#' inside `x` are made strict too.
#'
#' @param x A list.
#' @return `x` and each plain list inside it, with class "strict_list".
#' @noRd
strict_list <- function(x) {
  # Data frames and other objects keep their own class.
  is_plain_list <- vapply(
    x, function(element) is.list(element) && !is.object(element), logical(1)
  )
  x[is_plain_list] <- lapply(x[is_plain_list], strict_list)
  structure(x, class = c("strict_list", "list"))
}

#' Read an element of a strict list, stopping when the name is missing
#' @export
#' @noRd
`[[.strict_list` <- function(x, i, ...) {
  if (is.character(i) && !i[[1]] %in% names(x)) {
    # Some lists in the model data, such as the likelihoods, have no names.
    names_in_list <- if (is.null(names(x))) {
      "This list has no names, so it is read by position."
    } else {
      "The names in this list are {.val {names(x)}}."
    }
    cli::cli_abort(c(
      "x" = "There is no element named {.val {i[[1]]}}.",
      "i" = "Model code reads parameters and data by name. Read a name that
      can be missing only after {.code utils::hasName()} finds it.",
      "i" = names_in_list
    ))
  }
  NextMethod()
}

#' Read an element of a strict list by its full name only
#' @export
#' @noRd
`$.strict_list` <- function(x, name) {
  x[[name]]
}
