#' Read a 2d field from NetCDF file.
#'
#' \code{read_netcdf} returns a 2d field from a NetCDF file for a given
#' member and lead time. This function has only been tested on MEPS data.
#' The levels argument will currently only get data on pressure levels.
#'
#'
#' @param filename NetCDF file name to read from.
#' @param parameter Parameter to read - may be a parameter in the netcdf file
#'   or a standard HARP parameter name.
#' @param member The member to read from the file.
#' @param lead_time The lead time to read from the file.
#' @param level The pressure level for the data
#'
#' @return A 2-d array.
#' @export
#'
#' @examples
#' file_name <- get_filenames(file_date = 2017080100, template = "meps_met")
#' model_field <- read_netcdf(file_name, "air_temperature_2m", 0, 0)
#' model_field <- read_netcdf(file_name, "T2m", 0, 0)
#'
read_netcdf <- function(filename, parameter, member, lead_time, level = NULL) {

  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("Package ncdf4 required for read_netcdf() - Please install from CRAN",
      call. = FALSE
    )
  }

  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Package stringr required for read_netcdf() - Please install from CRAN",
      call. = FALSE
    )
  }

  ncID      <- ncdf4::nc_open(filename)
  nc_fields <- names(ncID$var)

  if (parameter %in% nc_fields) {
    nc_param <- parameter
  } else {
    nc_param  <- get_netcdf_param_MET(parameter)
  }

  if (is.na(nc_param)) stop("Requested parameter ", parameter, " is unknown")
  if (stringr::str_detect(nc_param, "_pl") && is.null(level)) {
    stop("Pressure level must be supplied for ", parameter, "with e.g. level = 1000")
  }

  nc_lead_times	  <- ncdf4::ncvar_get(ncID, "time")
  nc_lead_times   <- (nc_lead_times - nc_lead_times[1]) / 3600
  nc_members      <- ncdf4::ncvar_get(ncID, "ensemble_member")
  nc_pressure     <- ncdf4::ncvar_get(ncID, "pressure")

  lead_time_index <- which(nc_lead_times == lead_time)
  if (length(lead_time_index) == 0) stop("Requested lead time ", lead_time, " not found")

  member_index <- which(nc_members == member)
  if (length(member_index) == 0) stop("Requested member ", member, " not found")

  if (!is.null(level)) {
    level_index   <- which(nc_pressure == level)
    if(length(level_index) == 0) stop("Requested pressure level ", level, "not found")
  } else {
    level_index   <- 1
  }

  if (stringr::str_detect(filename, "sfx")) {
    nc_start <- c(1, 1, member_index, lead_time_index)
    nc_count <- c(-1, -1, 1, 1)
  } else {
    nc_start <- c(1, 1, member_index, level_index, lead_time_index)
    nc_count <- c(-1, -1, 1, 1, 1)
  }

  nc_data <- ncdf4::ncvar_get(ncID, nc_param, start = nc_start, count = nc_count)
  ncdf4::nc_close(ncID)
  nc_data

}
