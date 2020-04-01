# Read projection information from a WRF output file.

get_wrf_projection <- function(wrf_file) {

  if (inherits(wrf_file, "ncdf4")) {
    wrf_id     <- wrf_file
    was_closed <- FALSE
  } else {
    wrf_id     <- ncdf4::nc_open(wrf_file)
    was_closed <- TRUE
  }

  wrf_proj <- ncdf4::ncatt_get(wrf_id, 0, "MAP_PROJ")
  if (!wrf_proj[["hasatt"]]) {
    stop("File is missing the 'MAP_PROJ' attribute. Cannot get projection information.", call. = FALSE)
  } else {
    wrf_proj <- wrf_proj[["value"]]
  }

  get_att_value <- function(att) {
    if (att[["hasatt"]]) {
      att[["value"]]
    } else {
      ""
    }
  }

  lat0 <- get_att_value(ncdf4::ncatt_get(wrf_id, 0, "MOAD_CEN_LAT"))
  lon0 <- get_att_value(ncdf4::ncatt_get(wrf_id, 0, "STAND_LON"))
  lat1 <- get_att_value(ncdf4::ncatt_get(wrf_id, 0, "TRUELAT1"))
  lat2 <- get_att_value(ncdf4::ncatt_get(wrf_id, 0, "TRUELAT2"))


  proj4_string <- switch(
    as.character(wrf_proj),
    "0" = paste0("+proj=eqc +lon_0=", lon0, " +lat0=", lat0, " +lat_ts=", lat1),
    "1" = paste0("+proj=lcc +lon_0=", lon0, " +lat0=", lat0, " +lat_1=", lat1, " +lat_2=", lat2),
    "2" = paste0("+proj=stere +lon_0=", lon0, " +lat0=", lat0, " +lat_ts=", lat1),
    "3" = paste0("+proj=merc +lon_0=", lon0, " +lat_ts=", lat1),
    "6" = "+proj=latlong",
    NA
  )

  if (is.na(proj4_string)) {
    stop("Do not know how to process 'MAP_PROJ = ", wrf_proj, "'", call. = FALSE)
  }

  if (was_closed) ncdf4::nc_close(wrf_id)

  paste(proj4_string, "+R=6.371e+06")

}
