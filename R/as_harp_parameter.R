#' Create a harp parameter
#'
#' harp includes a number of built in parameters, but you may wish to read other
#' parameters. To do this you need to create a harp parameter.
#'
#' @param fullname The full name of the parameter
#' @param basename The basename of the parameter. This is typically the fullname
#'   excluding accumulation time and vertical level infotmation from the
#'   parameter name and forms the basis of the parameter name that is searched
#'   for in files. For example, for reading parameters from grib files the
#'   basename should be exactly the same as the grib shortName.
#' @param level The vertical level of the parameter. For grib files, this is
#'   ignored if the basename begins with a number, or mx or mn followed by a number.
#' @param level_type The type of vertical level. Can be one of "unknown",
#'   "height", "msl", "surf", "pressure", or "model".
#' @param accum The accumulation time in seconds
#' @param accum_unit The unit of accumulation to be used when searching files.
#'
#' @return A harp paramater
#' @export
#'
#' @examples
#' # maximum 2m temperature in last 6 hours
#' as_harp_parameter("mx2t6")
#' # potential vorticity at 500 hPa
#' as_harp_parameter("pv500", "pv", level = 500, level_type = "pressure")
as_harp_parameter <- function(
  fullname,
  basename,
  level      = 0,
  level_type = c("unknown", "height", "msl", "surf", "pressure", "model"),
  accum      = 0,
  accum_unit = NULL
) {

  if (missing(basename)) {
    basename = fullname
  }

  level_type <- match.arg(level_type)

  structure(
    list(
      fullname   = fullname,
      basename   = basename,
      level      = level,
      level_type = level_type,
      accum      = accum,
      acc_unit   = accum_unit
    ),
    class = "harp_parameter"
  )
}

