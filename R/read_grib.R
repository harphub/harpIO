#' Read a field from a grib file
#'
#' @param filename The grib file name.
#' @param parameter The parameter to read. Standard HARP names are used.
#' @param meta If TRUE, also read all meta data (domain, time properties).
#' @param ... Arguments for \code{Rgrib2::Gdec}
#'
#' @return A geofield object with 2d array and projection information
#' @export
#'
#' @examples
#' file_name <- "/lustre/storeB/users/andrewts/mepsr_data/grib/fc2017052600+001grib_fp"
#' model_geofield <- read_grib(file_name, "t2m")
#' model_geofield <- read_grib(file_name, "t500")
#' model_geofield <- read_grib(file_name, "topo")

### EXAMPLES NEED UPDATING

read_grib <- function(filename, parameter, meta = TRUE, ...) {
  #
  param_info    <- get_grib_param_info(parameter)
  if (is.na(param_info$short_name)) {
    stop("Unknown parameter: ", parameter)
  }
  #
  if (!requireNamespace("Rgrib2", quietly = TRUE)) {
    stop("Package Rgrib2 is not available.")
  }
  grib_info     <- Rgrib2::Gopen(filename)
  #
  grib_position <- grib_info %>%
    dplyr::filter(
      shortName              == param_info$short_name,
      indicatorOfParameter   == param_info$param_number,
      indicatorOfTypeOfLevel == param_info$level_type,
      level                  == param_info$level_number
    ) %>%
    dplyr::pull(position)
  #
  # to do: for "atmospheric" variables, read 3d data
  Rgrib2::Gdec(filename, grib_position, get.meta = meta, ...)
}
