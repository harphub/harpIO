#' Get MET Norway NetCDF parameter name from a HARP parameter name
#'
#' @param param HARP parameter name.
#'
#' @return The parameter name in a MET Norway NetCDF file
#' @export
#'
#' @examples
#' get_netcdf_param_MET("T2m")
#' get_netcdf_param_MET("PMSL")
#' get_netcdf_param_MET("S10m")
#' get_netcdf_param_MET("Pcp")

get_netcdf_param_MET <- Vectorize(function (param, vc = NA) {

  param_table <- tibble::tribble(
    ~harp_param, ~nc_param,
    "t"        , "air_temperature",
    "t0m"      , "air_temperature_0m",
    "t2m"      , "air_temperature_2m",
    "tmin"     , "air_temperature_min",
    "tmax"     , "air_temperature_max",
    "q"        , "specific_humidity",
    "q2m"      , "specific_humidity_2m",
    "rh"       , "relative_hunidity",
    "rh2m"     , "relative_humidity_2m",
    "caf"      , "cloud_area_fraction",
    "cctot"    , "cloud_area_fraction",
    "cchigh"   , "high_type_cloud_area_fraction",
    "ccmed"    , "medium_type_cloud_area_fraction",
    "cclow"    , "low_type_cloud_area_fraction",
    "cbase"    , "cloud_base_altitude",
    "pmsl"     , "air_pressure_at_sea_level",
    "ps"       , "surface air pressure",
    "u"        , "x_wind",
    "v"        , "y_wind",
    "w"        , "upward_air_velocity",
    "ugust10m" , "x_wind_gust_10m",
    "vgust10m" , "y_wind_gust_10m",
    "g10m"     , "wind_speed_of_gust",
    "u10m"     , "x_wind_10m",
    "v10m"     , "y_wind_10m",
    "s10m"     , "wind_speed",
    "d10m"     , "wind_direction",
    "tke"      , "turbulent_kinetic_energy",
    "vis"      , "visibility_in_air",
    "z"        , "geopotential",
    "z0m"      , "surface_geopotential",
    "altitude" , "surface_geopotential",
    "terrain"  , "surface_geopotential",
    "topo"     , "surface_geopotential",
    "pcp"      , "precipitation_amount_acc",
    "tg1"      , "TG1",
    "tg2"      , "TG2",
    "tg3"      , "TG3",
    "wg1"      , "WG1",
    "wg2"      , "WG2",
    "wg3"      , "WG3",
    "fog"      , "fog_area_fraction",
    "sst"      , "SST",
    "snow"     , "snowfall_amount_acc"
  )

  netcdf_param <- param_table %>%
    dplyr::filter(.data$harp_param == tolower(param)) %>%
    dplyr::pull(.data$nc_param)

  if (length(netcdf_param) < 1) {
    netcdf_param <- param
  } else {
    if (!is.na(vc) && is_temp(param, vc)) {
      suffix <- switch(
        vc,
        "pressure" = "_pl",
        "model"    = "_ml",
        ""
      )
      netcdf_param <- paste0(netcdf_param, suffix)
    }
  }

  netcdf_param

})
