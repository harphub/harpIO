#' Read a field from a grib file
#'
#' @importFrom Rgrib2 Gopen Gdec
#' @param filename The grib file name.
#' @param parameter The parameter to read. Standard HARP names are used.
#' @param ... Arguments for \code{get_grib_param_info} for level and levtype.
#'
#' @return A geofield object with 2d array and projection information
#' @export
#'
#' @examples
#' file_name <- "/lustre/storeB/users/andrewts/mepsr_data/grib/fc2017052600+001grib_fp"
#' model_geofield <- read_grib(file_name, "t2m")
#' model_geofield <- read_grib(file_name, "t", level = 500)
#' model_geofield <- read_grib(file_name, "topo")
read_grib <- function(filename, parameter, ...) {
#
	param_info    <- get_grib_param_info(parameter, ...)
	if (is.na(param_info$short_name)) {
	  stop("Unknown parameter: ", parameter)
	}
#
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
	Rgrib2::Gdec(filename, grib_position)
}
