#' Multiple pattern matching and replacement
#'
#' @description
#'  `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated as it is replaced by
#' \link[harpCore]{psub}
#'
#' @keywords internal
#'
#' @param x A character vector.
#' @param pattern A character vector of patterns to replace.
#' @param replacement A character vector of the same length as pattern with the
#'   corresponding replacements.
#' @param regex A logical to denote whether \code{pattern} is a regular
#'   expression (TRUE) or a string to match exactly (FALSE). The default is
#'   FALSE so strings in \code{pattern} are matched exactly.
#' @param ... Other arguments to \link[base]{gsub}
#'
#' @return A character vector of the same length as x
#' @export
#'
#' @examples
#' msub(letters[1:10], c("a", "c", "e"), c("A", "C", "E"))
#' msub(c("a", "b", "ac", "ad"), c("a", "ac"), c("A", "AC"))
#' \dontrun{
#' # "ac" is replaced with "Ac" due to substring matching
#' msub(c("a", "b", "ac", "ad"), c("a", "ac"), c("A", "AC"), regex = TRUE)
#' }

# This function needs to some work. It should really replace whole
# strings rather than just patterns within strings.

msub <- function(x, pattern, replacement, regex = FALSE, ...) {

  lifecycle::deprecate_warn("0.1.0", "msub()", "psub()")

  stopifnot(is.character(x))
  stopifnot(is.character(pattern))
  stopifnot(is.character(replacement))

  if (length(pattern) != length(replacement)) {
    stop("`pattern` and `replacement` must be the same length")
  }

  for (i in seq_along(pattern)) {
    gsub_pattern <- pattern[i]
    if (!regex) {
      if (substr(gsub_pattern, 1, 1) != "^") {
        gsub_pattern <- paste0("^", gsub_pattern)
      }
      if (substr(gsub_pattern, nchar(gsub_pattern), nchar(gsub_pattern)) != "$") {
        gsub_pattern <- paste0(gsub_pattern, "$")
      }
    }
    if (!any(grepl(gsub_pattern, x))) {
      warning("\"", pattern[i], "\" not found in x.")
    }
    x <- gsub(gsub_pattern, replacement[i], x, ...)
  }

  x
}
