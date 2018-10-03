# Methods for harp_forecast objects

#' @export
print.harp_fcst <- function(.fcst, ...) {
  .name <- names(.fcst)
  print_fun <- function(.x, .y, ...) {
    cli::cat_bullet(.x, col = "#AAAAAA", bullet_col = "#AAAAAA")
      print(.y, ...)
      cat("\n")
  }
  purrr::walk2(.name, .fcst, print_fun, ...)
}

