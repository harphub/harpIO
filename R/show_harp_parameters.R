#' Show default harp parameter names
#'
#' Prints a table of default parameter names in harp and their descriptions
#'
#' @param cf_name Set to TRUE to show CF convention names where available rather
#'   than description.
#'
#' @export
#' @examples
#' show_harp_parameters()
#' show_harp_parameters(cf_name = TRUE)
#'
show_harp_parameters <- function(cf_name = FALSE) {

  harp_params <- tibble::tribble(
    ~harp_parameter_name, ~description,
    "AccPcp<X>h", "Accumulated precipitation over <X> hours, e.g. AccPcp12h",
    "CCtot"     , "Total cloud cover",
    "CClow"     , "Low level cloud cover",
    "CCmed"     , "Medium level cloud cover",
    "CChigh"    , "High level cloud cover",
    "Cbase"     , "Height of cloud base",
    "D10m"      , "10m wind direction",
    "G10m"      , "10m wind gust - period depends on input data",
    "Gmax"      , "10m maximum wind gust - period depends on input data",
    "Pcp"       , "Precipitation direct from model - usually accumulated from start time",
    "Pmsl"      , "Pressure at mean sea level",
    "Ps"        , "Pressure at surface",
    "Q2m"       , "2m specific humidity",
    "RH2m"      , "2m relative humidity",
    "T2m"       , "2m temperature",
    "Td2m"      , "2m dewpoint temperature",
    "Tmin"      , "Minimum 2m temperature",
    "Tmax"      , "Maximum 2m temperature",
    "S10m"      , "10m wind speed",
    "Smax"      , "Maximum 10m wind speed - period depends on input data",
    "vis"       , "Horizontal visibility",
    "Z"         , "Height above sea level"
  )

  if (cf_name) {
    harp_params <- dplyr::transmute(
      harp_params,
      .data$harp_parameter_name,
      cf_name = get_netcdf_param_MET(.data$harp_parameter_name)
    )
  }

  print(dplyr::arrange(harp_params, .data$harp_parameter_name), n = nrow(harp_params))
  cat(
    "\n",
    "For upper air parameters, Z, T, RH, D, S, Q, and Td are available. Follow the\n",
    "letter with a number to denote pressure level, e.g. T850, S925, Z300 etc.\n"
  )
}
