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

get_netcdf_param_MET <- function (param) {
#
  ncParam <- switch (tolower(param),
                     "t"        = "air_temperature_pl",
                     "t0m"      = "air_temperature_0m",
                     "t2m"      = "air_temperature_2m",
                     "q"        = "specific_humidity_pl",
                     "q2m"      = "specific_humidity_2m",
                     "rh"       = "relative_hunidity_pl",
                     "rh2m"     = "relative_humidity_2m",
                     "caf"      = "cloud_area_fraction_pl",
                     "cctot"    = "cloud_area_fraction",
                     "cchigh"   = "high_type_cloud_area_fraction",
                     "ccmed"    = "medium_type_cloud_area_fraction",
                     "cclow"    = "low_type_cloud_area_fraction",
                     "pmsl"     = "air_pressure_at_sea_level",
                     "u"        = "x_wind_pl",
                     "v"        = "y_wind_pl",
                     "w"        = "upward_air_velocity_pl",
                     "ugust10m" = "x_wind_gust_10m",
                     "vgust10m" = "y_wind_gust_10m",
                     "g10m"     = "wind_speed_of_gust",
                     "u10m"     = "x_wind_10m",
                     "v10m"     = "y_wind_10m",
                     "s10m"     = "wind_speed",
                     "d10m"     = "wind_direction",
                     "tke"      = "turbulent_kinetic_energy_pl",
                     "z"        = "geopotential_pl",
                     "z0m"      = "surface_geopotential",
                     "altitude" = "surface_geopotential",
                     "terrain"  = "surface_geopotential",
                     "topo"     = "surface_geopotential",
                     "pcp"      = "precipitation_amount_acc",
                     "g10m"     = "wind_speed_of_gust",
                     "tg1"      = "TG1",
                     "tg2"      = "TG2",
                     "tg3"      = "TG3",
                     "wg1"      = "WG1",
                     "wg2"      = "WG2",
                     "wg3"      = "WG3",
                     "fog"      = "fog_area_fraction",
                     "sst"      = "SST",
                     "snow"     = "snowfall_amount_acc",
                     NA)
#
  ncParam
#
}
