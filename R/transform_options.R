#' Generate Transformation Options
#'
#' When reading gridded data, three transformations are available: interpolate,
#' regrid and xsection. Each of these transformations requires their own set of
#' options, and these functions are used to generate those options.
#'
#' \code{interpolate_opts} generates options for interpolating gridded data to
#' points, or stations. If no stations are passed, a default list of stations,
#' accessed from the harpIO built in dataset stations_list is used. If 2m
#' temperature is to be read, the deafult behaviour is to attempt to height
#' correct for differences between the model elevation and the elevation at the
#' actual station. The default interpolation method is nearest-neighbour.
#'
#' \code{regrid_opts} generates options to regrid the data from its native grid
#' to a new grid. The new grid is specified either as a
#' \link[meteogrid]{geofield} or \link[meteogrid]{geodomain} object.
#' \link[harpIO]{read_grid} can be used to read a geofield, or
#' \link[meteogrid]{Make.domain} can be used to make a geodomain. The default
#' interpolation method is nearest-neighbour.
#'
#' \code{xsection_opts} generates options to extract a vertical cross section
#' from three dimensional gridded data. \code{a} and \code{b} denote the left
#' and right hand extents of the vertical cross section. By defualt the cross
#' sections are interpolated horizontally to a 2.5 km grid length. In the
#' vertical, if data are on model levels or pressure levels, these are converted
#' to log(pressure) and by default interpolated to to log(10) hPa levels. The
#' default interpolation method is bilinear interpolation.
#'
#' @param stations A data frame of points to interpolate to. Must have the
#'   columns 'SID', for a station ID, lat for latitude and lon for longitude.
#' @param method The interpolation method. Can be 'nearest' or 'closest' for
#'   nearest neighbour interpolation, 'bilinear', or 'bicubic'. For
#'   \code{regrid_opts} 'mean' is also available for upscaling.
#' @param correct_t2m Logical. Whether to make a height correction to 2m
#'   temperature to account for differences between the model elevation and the
#'   station elevation.
#' @param keep_model_t2m Logical. Whether to keep the uncorrected 2m temperature
#'   if \code{correct_t2m = TRUE}.
#' @param lapse_rate The lapse rate in K/m to use for the 2m temperature heigth
#'   correction. The default is the standard moist adiabitic lapse rate of
#'   0.0065 K/m.
#' @param clim_file A file containing model orography or surface geopential on
#'   the same grid as the data to be read in so that height corrections to 2m
#'   temperature can be made. It will also contain a land sea mask if masked
#'   interpolation is to be done. For initialising the domain, any parameter,
#'   passed in 'clim_param' can be used.
#' @param clim_file_format The format of the clim_file. If set to NULL, a guess
#'   will be made.
#' @param clim_file_opts Options for reading the clim_file, depending on the
#'   format of the clim_file.
#' @param clim_param The parameter to read from 'clim_file'. If
#'   \code{correct_t2m = TRUE} then this should be either surface geopotential
#'   or terrain height in meters, otherwise for the purposes of intialising
#'   interpolation weights, any paramter that exists in 'clim_file' can be used.
#' @param use_mask Logical. Whether to use a mask in the interpolation. Requires
#'   that 'stations' has an 'lsm' column and clim_file includes a land-sea mask.
#' @param weights Interpolation weights if they have already been calculated.
#' @param keep_raw_data Logical. Whether to keep the untransformed full gridded
#'   data field. The default is FALSE.
#' @param correct_ps Logical. Whether to make a height correction to surface
#'   pressure to account for differences between the model elevation and the
#'   station elevation. This correction also requires uncorrected T2m.
#'   Only relevant when reading vfld and/or converting to sqlite. 
#' @param keep_model_ps Logical. Whether to keep the uncorrected surface pressure
#'   if \code{correct_ps = TRUE}.
#'
#' @return A list of options that will be used in the transformation.
#' @export
#'
#' @examples
#' interpolate_opts()
interpolate_opts <- function(
  stations,
  method           = c("nearest", "bilinear", "bicubic", "closest"),
  correct_t2m      = TRUE,
  keep_model_t2m   = FALSE,
  lapse_rate       = 0.0065,
  clim_file        = NULL,
  clim_file_format = NULL,
  clim_file_opts   = NULL,
  clim_param       = "sfc_geo",
  use_mask         = FALSE,
  weights          = NULL,
  keep_raw_data    = FALSE,
  correct_ps       = TRUE,
  keep_model_ps    = FALSE
) {

  if (missing(stations)) {
    message("No stations specified. Using default stations: 'station_list'")
    stations = harpCore::station_list
  }

  stopifnot(is.data.frame(stations))

  stations <- tibble::as_tibble(stations)

  stations_cols <- colnames(stations)
  missing_cols  <- setdiff(c("SID", "lat", "lon"), stations_cols)

  if (length(missing_cols) > 0) {
    stop("'stations' is missing the columns: ", paste(missing_cols, collapse = ", "))
  }

  if (correct_t2m && !is.element("elev", stations_cols)) {
    warning(
      "No 'elev' column found in stations, and correct_t2m = TRUE. Setting correct_t2m = FALSE",
      call.      = FALSE,
      immediate. = TRUE
    )
    correct_t2m <- FALSE
  }
  
  if (correct_ps && !is.element("elev", stations_cols)) {
    warning(
      "No 'elev' column found in stations, and correct_ps = TRUE. Setting correct_ps = FALSE",
      call.      = FALSE,
      immediate. = TRUE
    )
    correct_ps <- FALSE
  }

  if (use_mask && !is.element("lsm", stations_cols)) {
    stop("For interpolation with a mask, 'stations' must contain an 'lsm' column.")
  }

  method   = match.arg(method)

  list(
    stations         = stations,
    method           = method,
    correct_t2m      = correct_t2m,
    keep_model_t2m   = keep_model_t2m,
    lapse_rate       = lapse_rate,
    clim_file        = clim_file,
    clim_file_format = clim_file_format,
    clim_file_opts   = clim_file_opts,
    clim_param       = clim_param,
    use_mask         = use_mask,
    weights          = weights,
    keep_raw_data    = keep_raw_data,
    correct_ps       = correct_ps,
    keep_model_ps    = keep_model_ps
  )

}

#' @rdname interpolate_opts
#' @param new_domain A geofield or geodomain object on the grid to which the
#'   data should be regridded.
#'
#' @export
#'
#' @examples
#' if (requireNamespace("Rgrib2", quietly = TRUE) & requireNamespace("harpData", quietly = TRUE)) {
#'   new_domain = read_grid(
#'     system.file(
#'       "grib/AROME_Arctic/2018/07/10/00/fc2018071000+000grib_fp",
#'       package = "harpData"
#'     ),
#'     parameter = "T2m"
#'   )
#'   regrid_opts(new_domain = new_domain)
#' }
regrid_opts <- function(
  new_domain       = NULL,
  method           = "nearest",
  clim_file        = NULL,
  clim_file_format = NULL,
  clim_file_opts   = NULL,
  clim_param       = "sfc_geo",
  weights          = NULL,
  keep_raw_data    = FALSE
) {

  if (!is.null(new_domain)) {
    stopifnot(inherits(new_domain, "geofield") || inherits(new_domain, "geodomain"))
  } else {
    if (is.null(clim_file)) {
      stop("Either 'new_domain' or 'clim_file' need to be passed.")
    }
  }

  list(
    new_domain       = new_domain,
    method           = method,
    clim_file        = clim_file,
    clim_file_format = clim_file_format,
    clim_file_opts   = clim_file_opts,
    clim_param       = clim_param,
    weights          = weights,
    keep_raw_data    = keep_raw_data
  )

}

#' @rdname interpolate_opts
#'
#' @param a A length 2 numeric vector with the longitude and latitude of the
#'   left hand edge of the cross section.
#' @param b A length 2 numeric vector with the longitude and latitude of the
#'   right hand edge of the cross section.
#' @param horizontal_res The horizontal grid length of the cross section in
#'   meters. The default is 2500m.
#' @param vertical_res The vertical grid length of the cross section. For data
#'   on pressure levels or model levels this should be in hPa. For data on
#'   height levels this should be in meters. The default is log(10).
#' @param levels_ascending The order of the level values. In the output the
#'   levels are numbered for simple plotting. Set to \code{TRUE} (the default)
#'   for the levels to be numbered in ascending order of the level values and
#'   \code{FALSE} for the numbering to be done in descending order.
#' @export
#'
#' @examples
#' xsection_opts(a = c(5.3, 60.5), b = c(10.8, 59.9))
xsection_opts <- function(
  a,
  b,
  horizontal_res   = 2500,
  vertical_res     = log(10),
  clim_file        = NULL,
  clim_file_format = NULL,
  clim_file_opts   = NULL,
  clim_param       = "sfc_geo",
  method           = "bilinear",
  levels_ascending = TRUE,
  keep_raw_data    = FALSE,
  ...
) {

  stopifnot(is.numeric(a) && length(a) == 2)
  stopifnot(is.numeric(b) && length(b) == 2)

  c(
    list(
      a                = a,
      b                = b,
      horizontal_res   = horizontal_res,
      vertical_res     = vertical_res,
      clim_file        = clim_file,
      clim_file_format = clim_file_format,
      clim_file_opts   = clim_file_opts,
      clim_param       = clim_param,
      method           = method,
      levels_ascending = levels_ascending,
      keep_raw_data    = keep_raw_data
    ),
    list(...)
  )

}

#' @rdname interpolate_opts
#'
#' @param x1 left x index of the subdomain
#' @param x2 right x index of the subdomain
#' @param y1 bottom y index of the subdomain
#' @param y2 top y index of the subdomain
#'
#' @export
subgrid_opts <- function(x1, x2, y1, y2, ...) {
  list(x1 = x1, x2 = x2, y1 = y1, y2 = y2)
}
