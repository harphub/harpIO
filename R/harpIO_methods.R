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

