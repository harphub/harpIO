#' Internal function to get grib parameter information
#'
#' Gets the shortname and parameter number for grib files. For
#' surface and screen level parameters the level type and level number are
#' assumed. For upper air parameters the level and, optionally, the level type
#' must be supplied. The default is for pressure levels.
#'
#' @param param Parameter name (or harp_param object)
#' @param vertical_coordinate The vertical coordinate for upper air data. May be
#'   "pressure", "model" or "height".
#' @return A list with \code{short_name}, \code{param_number},
#'   \code{level_type}, \code{level_numbe.r}
#'
get_grib_param_info <- function(param, vertical_coordinate = NA_character_) {

  if (!inherits(param, "harp_parameter")) {
    param <- parse_harp_parameter(
      param, vertical_coordinate = vertical_coordinate
    )
  }

  grib_info <- get_param_info(param, "grib")
  func      <- grib_info[["param_func"]]
  grib_info <- grib_info[["param_info"]]

  if (is.null(grib_info[["level_type"]])) {
    grib_info[["level_type"]] <- param[["level_type"]]
  }

  # if (is.null(grib_info[["level"]])) {
  #   grib_info[["level"]] <- param_info[["level"]]
  # }

  if (is.list(grib_info[["level_type"]])) {
    grib_info[["level_type"]] <-
      grib_info[["level_type"]][[param[["level_type"]]]]
  }

  if (is.null(grib_info[["level"]])) {
    if (is.null(param[["level"]]) || is.na(param[["level"]])) {
      grib_info[["level"]] <- -999
    } else {
      grib_info[["level"]] <- param[["level"]]
    }
  }

  names(grib_info) <- c(
    "short_name", "level_type", "level_number"
  )

  c(grib_info, list(func = func))

}
