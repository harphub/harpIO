# Methods for harp_forecast objects

#' @export
print.harp_fcst <- function(x, ...) {
  .name <- names(x)
  print_fun <- function(.x, .y, ...) {
    cli::cat_bullet(.x, col = "#AAAAAA", bullet_col = "#AAAAAA")
      print(.y, ...)
      cat("\n")
  }
  purrr::walk2(.name, x, print_fun, ...)
}

#' @export
print.harp_analysis <- function(x, ...) {
  .name <- names(x)
  print_fun <- function(.x, .y, ...) {
    cli::cat_bullet(.x, col = "#AAAAAA", bullet_col = "#AAAAAA")
    print(.y, ...)
    cat("\n")
  }
  purrr::walk2(.name, x, print_fun, ...)
}

# Methods for geolist objects

# #' @export
# #' as_geolist <- function(...) {
# #'
# #'   x <- list(...)
# #'
# #'   if (length(x) == 1 && is.list(x[[1]])) {
# #'     x <- x[[1]]
# #'   }
# #'
# #'   # if (!all(sapply(x, meteogrid::is.geofield))) {
# #'   #   stop("All inputs must be geofields.")
# #'   # }
# #'
# #'   structure(
# #'     x,
# #'     class = c("geolist", class(x))
# #'   )
# #'
# #' }
# #'
# #' #' @export
# #' `[.geolist` <- function(x, i, ...) {
# #'   as_geolist(NextMethod())
# #' }
# #'
# #' #' @export
# #' c.geolist <- function(x, ...) {
# #'   as_geolist(NextMethod())
# #' }
# #'
# #' #' @export
# #' Math.geolist <- function(x, ...) {
# #'   if (.Generic == "cumsum") {
# #'     return(
# #'       structure(
# #'         Reduce(`+`, x, accumulate = TRUE),
# #'         class = class(x)
# #'       )
# #'     )
# #'   }
# #'   structure(
# #'     lapply(x, .Generic, ...),
# #'     class = class(x)
# #'   )
# #' }
# #'
# #' #' @export
# #' Ops.geolist <- function(e1, e2) {
# #'   fun <- get(.Generic, envir = parent.frame(), mode = "function")
# #'   func <- function(.x, .y, .f) {
# #'     res <- .f(.x, .y)
# #'     if (!meteogrid::is.geofield(res)) {
# #'       res <- NA
# #'     }
# #'     res
# #'   }
# #'   structure(
# #'     mapply(func, e1, e2, MoreArgs = list(.f = fun), SIMPLIFY = FALSE),
# #'     class = class(e1)
# #'   )
# #' }
# #'
# #' #' @export
# #' Summary.geolist <- function(..., na.rm = FALSE) {
# #'   if (.Generic %in% c("all", "any", "range")) {
# #'     stop (.Generic, " not defined for geolist objects")
# #'   }
# #'
# #'   fun <- switch(
# #'     .Generic,
# #'     "sum"  = "+",
# #'     "prod" = "*",
# #'     "min"  = "pmin",
# #'     "max"  = "pmax"
# #'   )
# #'
# #'   if (fun == "+") {
# #'     fun <- function(x, y, na.rm) {
# #'       res <- x + y
# #'       if (na.rm) {
# #'         NAs <- which(is.na(res))
# #'         if (length(NAs) > 0) {
# #'           res[NAs] <- pmax(x[NAs], y[NAs], na.rm = TRUE) %>%
# #'             meteogrid::as.geofield(x)
# #'         }
# #'       }
# #'       res
# #'     }
# #'   } else if (fun == "*") {
# #'     fun <- function(x, y, na.rm) {
# #'       res <- x * y
# #'       if (na.rm) {
# #'         NAs <- which(is.na(res))
# #'         if (length(NAs) > 0) {
# #'           res[NAs] <- pmax(x[NAs], y[NAs], na.rm = TRUE) %>%
# #'             meteogrid::as.geofield(x)
# #'         }
# #'       }
# #'       res
# #'     }
# #'   } else {
# #'     fun <- get(fun, envir = parent.frame(), mode = "function")
# #'   }
# #'
# #'   purrr::reduce(.x = ..., .f = fun, na.rm = na.rm)
# #'
# #' }
# #'
# #' #' @export
# #' mean.geolist <- function(x, na.rm = FALSE) {
# #'   dom <- meteogrid::as.geodomain(x[[1]])
# #'   meteogrid::as.geofield(
# #'     sum(x, na.rm = na.rm) / sum(as_geolist(lapply(x, function(y) !is.na(y)))),
# #'     dom
# #'   )
# #' }
# #'
# #' #' @export
# #' variance <- function(x, na.rm = FALSE) {
# #'   UseMethod("variance")
# #' }
# #'
# #' #' @export
# #' variance.default <- function(x, na.rm = FALSE) {
# #'   var(x, na.rm = na.rm)
# #' }
# #'
# #' #' @export
# #' variance.geolist <- function(x, na.rm = FALSE) {
# #'   x_bar <- mean(x, na.rm = na.rm)
# #'
# #'   x <- structure(
# #'     lapply(x, function(y) (y - x_bar) ^ 2),
# #'     class = class(x)
# #'   )
# #'
# #'   dom <- meteogrid::as.geodomain(x_bar)
# #'   meteogrid::as.geofield(
# #'     sum(x, na.rm = na.rm) / (sum(as_geolist(lapply(x, function(y) !is.na(y)))) - 1),
# #'     dom
# #'   )
# #'
# #' }
# #'
# #' #' @export
# #' std_dev <- function(x, na.rm = FALSE) {
# #'   UseMethod("std_dev")
# #' }
# #'
# #' #' @export
# #' std_dev.default <- function(x, na.rm = FALSE) {
# #'   sd(x, na.rm = na.rm)
# #' }
# #'
# #' #' @export
# #' std_dev.geolist <- function(x, na.rm = FALSE) {
# #'   sqrt(variance(x, na.rm = na.rm))
# #' }
# #'
# #' #' @export
# #' diff.geolist <- function(x, lag = 1) {
# #'   as_geolist(x - dplyr::lag(x, n = lag))
# #' }

# Dummy function so that R CMD check doesn't complain about dbplyr not being
# used whe harpIO absolutely needs it
dummy <- function() {
  dbplyr::as.sql
}

# Dummy function to make := usable. Taken from rlang.
`:=` <- function(x, y)
{
  rlang::abort(
    "`:=` can only be used within dynamic dots.",
    call = rlang::caller_env()
  )
}
