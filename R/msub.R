#' Multiple pattern matching and replacement
#'
#' \code{msub} calls \link[base]{gsub} for each pattern / replacement pair.
#' Where a pattern is not found, nothing is replaced for that pattern /
#' replacement pair.
#'
#' @param x A character vector.
#' @param pattern A character vector of patterns to replace.
#' @param replacement A character vector of the same length as pattern with the
#'   corresponding replacements.
#' @param ... Other arguments to \link[base]{gsub}
#'
#' @return A character vector of the same length as x
#' @export
#'
#' @examples
#' msub(letters[1:10], c("a", "c", "e"), c("A", "C", "E"))
#'
msub <- function(x, pattern, replacement, ...) {

  stopifnot(is.character(x))
  stopifnot(is.character(pattern))
  stopifnot(is.character(replacement))

  if (length(pattern) != length(replacement)) {
    stop("`pattern` and `replacement` must be the same length")
  }

  for (i in seq_along(pattern)) {
    if (!is.element(pattern[i], x)) {
      warning("\"", pattern[i], "\" not found in x.")
    }
    x <- gsub(pattern[i], replacement[i], x, ...)
  }

  x
}
