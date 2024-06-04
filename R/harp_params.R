# Function to pad the end of a string - useful for FA param names that
# must be 16 characters long.

pad_string <- function(x, string_length, text = " ") {
  if (nchar(text) != 1) {
    stop("`text` must be a 1 character string")
  }
  if (nchar(x) > string_length) {
    stop("`x` must have <= ", string_length, " characters")
  }
  out <- paste0(rep(as.character(text), string_length), collapse = "")
  substr(out, 1, nchar(x)) <- x
  out
}


# harp_params is a list with all harp parameters and how they map
# to different file formats. The functions below the data are used for
# printing, adding or replacing harp entries in the harp_params list

define_harp_params <- function() {
  list(

    ###################
    ### TEMPERATURE ###
    ###################
    t2m = list(

      description = "Air temperature at 2m above the ground",

      min = 223,
      max = 333,

      grib = list(
        name       = c("2t", "t"),
        level_type = c("heightAboveGround", "surface"),
        level      = 2
      ),

      netcdf = list(
        name = "air_temperature_2m"
      ),

      v = list(
        harp_param  = "T2m",
        name        = "TT",
        param_units = "K",
        type        = "SYNOP"
      ),

      wrf = list(
        name = "T2"
      ),

      fa = list(
        name  = pad_string("CLSTEMPERATURE", 16),
        units = "K"
      ),

      obsoul = list(
        name      = 39,
        units     = "K",
        harp_name = "T2m"
      )
    ),

    ###

    t0m = list(

      description = "Skin temperature",

      other_names = c("sfct", "skt"),

      min = 223,
      max = 333,

      grib = list(
        name       = c("0t", "t"),
        level_type = c("heightAboveGround", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "air_temperature_0m"
      ),

      wrf = list(
        name = "TSK"
      ),

      fa = list(
        name  = pad_string("SURFTEMPERATURE", 16),
        units = "K"
      )
    ),

    ###

    tmin = list(

      description = "Minimum air temperature at 2m above the ground",

      min = 223,
      max = 333,

      grib = list(
        name       = c("mn2t", "mnt"),
        level_type = c("heightAboveGround", "surface"),
        level      = 2
      ),

      netcdf = list(
        name = "air_temperature_min"
      ),

      v = list(
        harp_param  = "Tmin",
        name        = c("TM", "TN"),
        param_units = "K",
        type        = "SYNOP"
      ),

      fa = list(
        name  = pad_string("CLSMINI.TEMPERAT", 16),
        units = "K"
      ),

      obsoul = list(
        name = 81,
        units = "K",
        harp_name = "Tmin"
      )
    ),

    ###

    tmax = list(

      description = "Maximum air temperature at 2m above the ground",

      min = 223,
      max = 333,

      grib = list(
        name       = c("mx2t", "mxt"),
        level_type = c("heightAboveGround", "surface"),
        level      = 2
      ),

      netcdf = list(
        name = "air_temperature_max"
      ),

      v = list(
        harp_param  = "Tmax",
        name        = "TX",
        param_units = "K",
        type        = "SYNOP"
      ),

      fa = list(
        name  = pad_string("CLSMAXI.TEMPERAT", 16),
        units = "K"
      ),

      obsoul = list(
        name = 82,
        units = "K",
        harp_name = "Tmax"
      )
    ),
    ###

    t = list(

      description = "Air temperature",

      min = 173,
      max = 333,

      grib = list(
        name       = "t",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "air_temperature",
        suffix = get("nc_level_suffixes")
      ),

      v = list(
        harp_param  = "T",
        name        = "TT",
        param_units = "K",
        type        = "TEMP"
      ),

      wrf = list(
        name = "T"
      ),

      fa = list(
        name  = pad_string("TEMPERATURE", 16),
        units = "K"
      )
    ),
    ###

    td2m = list(

      description = "Dew point temperature at 2m above the ground",

      min = 223,
      max = 333,

      grib = list(
        name       = "td",
        level_type = c("heightAboveGround", "surface"),
        level      = 2
      ),

      netcdf = list(
        name = "dew_point_temperature_2m"
      ),

      v = list(
        harp_param  = "Td2m",
        name        = "TD",
        param_units = "K",
        type        = "SYNOP"
      ),

      fa = list(
        name = list(
          tc = pad_string("CLSTEMPERATURE", 16),
          rh = pad_string("CLSHUMI.RELATIVE", 16)
        ),
        units = "K",
        func  = "rh2tdew"
      )
    ),
    ###

    td = list(

      description = "Dew point temperature",

      grib = list(
        name       = "td",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "dew_point_temperature",
        suffix = get("nc_level_suffixes")
      ),

      v = list(
        harp_param  = "Td",
        name        = "TD",
        param_units = "K",
        type        = "TEMP"
      ),

      fa = list(
        name  = list(
          tc = pad_string("TEMPERATURE", 16),
          rh = pad_string("HUMI_RELATIVE", 16)
        ),
        units = "K",
        func  = "rh2dew"
      )
    ),
    ###

    sst = list(

      description = "Sea surface temperature",

      grib = list(
        name       = "sst",
        level_type = c("meanSea", "heightAboveSea", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "sea_surface_temperature"
      ),

      wrf = list(
        name        = "SST"
      ),

      fa = list(
        name  = "SFX.SST",
        units = "K"
      )
    ),

    ############
    ### WIND ###
    ############
    s10m = list(

      description = "Wind speed at 10m above the ground",

      min = 0,
      max = 100,

      grib = list(
        name       = c("10si", "ws"),
        level_type = c("heightAboveGround", "surface"),
        level      = 10
      ),

      netcdf = list(
        name = "wind_speed"
      ),

      v = list(
        harp_param  = "S10m",
        name        = "FF",
        param_units = "m/s",
        type        = "SYNOP"
      ),

      obsoul = list(
        name        = 41,
        units       = "m/s",
        harp_name   = "S10m",
        common_name = "wind"
      )

    ),

    ###

    s = list(

      description = "Wind speed",

      min = 0,
      max = 150,

      grib = list(
        name       = "ws",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "wind_speed",
        suffix = get("nc_level_suffixes")
      ),

      v = list(
        harp_param  = "S",
        name        = "FF",
        param_units = "m/s",
        type        = "TEMP"
      ),

      fa = list(
        name  = pad_string("VITESSE.VENT", 16),
        units = "m/s"
      )

    ),
    ###

    smax = list(

      description = "Maximum wind speed at 10m above the ground",

      min = 0,
      max = 100,

      v = list(
        harp_param  = "Smax",
        name        = "WX",
        param_units = "m/s",
        type        = "SYNOP"
      )
    ),
    ###

    ws10m  = list(

      description = paste(
        "Wind speed at 10m above the ground calculated",
        "from U and V winds at 10m above the ground"
      ),

      min = 0,
      max = 100,

      grib = list(
        name       = list(u = c("10u", "u"), v = c("10v", "v")),
        level_type = c("heightAboveGround", "surface"),
        level      = 10
      ),

      netcdf = list(
        name = list(u = "x_wind_10m", v = "y_wind_10m")
      ),

      wrf = list(
        name = list(u = "U10", v = "V10")
      ),

      fa = list(
        name = list(
          u = pad_string("CLSVENT.ZONAL", 16),
          v = pad_string("CLSVENT.MERIDIEN", 16)
        ),
        units = "m/s"
      ),

      func = function(u, v) {
        res <- sqrt(u ^ 2 + v ^ 2)
        attr(res, "info")[["name"]] <- "Wind speed"
        res
      }

    ),
    ###

    ws = list(

      description = "Wind speed calculated from U and V winds",

      min = 0,
      max = 150,

      grib = list(
        name       = list(u = "u", v = "v"),
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = list(u = "x_wind", v = "y_wind"),
        suffix = get("nc_level_suffixes")
      ),

      wrf = list(
        name = list(u = "U", v = "V")
      ),

      fa = list(
        name  = list(
          u = pad_string("VENT_ZONAL", 16),
          v = pad_string("VENT_MERIDIEN", 16)
        ),
        units = "m/s"
      ),

      func = function(u, v) sqrt(u ^ 2 + v ^ 2)

    ),
    ###

    d10m = list(

      description = "Wind direction at 10m above the ground",

      min = 0,
      max = 360,

      grib = list(
        name       = "wdir",
        level_type = c("heightAboveGround", "surface"),
        level      = 10
      ),

      netcdf = list(
        name = "wind_direction"
      ),

      v = list(
        harp_param  = "D10m",
        name        = "DD",
        param_units = "degrees",
        type        = "SYNOP"
      ),

      obsoul = list(
        name        = 41,
        units       = "degrees",
        harp_name   = "D10m",
        common_name = "wind"
      )

    ),
    ###

    d = list(

      description = "Wind direction",

      min = 0,
      max = 360,

      grib = list(
        name       = "wdir",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "wind_direction",
        suffix = get("nc_level_suffixes")
      ),

      v = list(
        harp_param  = "D",
        name        = "DD",
        param_units = "degrees",
        type        = "TEMP"
      ),

      fa = list(
        name  = pad_string("DIR.VENT", 16),
        units = "degrees"
      )

    ),
    ###

    wd10m  = list(

      description = paste(
        "Wind direction at 10m above the ground calculated",
        "from U and V winds at 10m above the ground"
      ),

      min = 0,
      max = 360,

      grib = list(
        name       = list(u = c("10u", "u"), v = c("10v", "v")),
        level_type = c("heightAboveGround", "surface"),
        level      = 10
      ),

      netcdf = list(
        name = list(u = "x_wind_10m", v = "y_wind_10m")
      ),

      wrf = list(
        name = list(u = "U10", v = "V10")
      ),

      fa = list(
        name = list(
          u = pad_string("CLSVENT.ZONAL", 16),
          v = pad_string("CLSVENT.MERIDIEN", 16)
        ),
        units = "m/s"
      ),

      func = function(u, v) {
        meteogrid::wind.dirspeed(u, v, rotate_wind = TRUE)[["wdir"]]
      }
    ),
    ###

    wd = list(

      description = "Wind direction calculated from U and V winds",

      min = 0,
      max = 360,

      grib = list(
        name       = list(u = "u", v = "v"),
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = list(u = "x_wind", v = "y_wind"),
        suffix = get("nc_level_suffixes")
      ),

      wrf = list(
        name = list(u = "U", v = "V")
      ),

      fa = list(
        name  = list(
          u = pad_string("VENT_ZONAL", 16),
          v = pad_string("VENT_MERIDIEN", 16)
        ),
        units = "m/s"
      ),

      func = function(u, v) {
        meteogrid::wind.dirspeed(u, v, rotate_wind = TRUE)[["wdir"]]
      }

    ),
    ###

    u10m = list(

      description = "Wind speed in u direction at 10m above the ground",

      min = 0,
      max = 100,

      grib = list(
        name       = c("10u", "u"),
        level_type = c("heightAboveGround", "surface"),
        level      = 10
      ),

      netcdf = list(
        name = "x_wind_10m"
      ),

      wrf = list(
        name = "U10"
      ),

      fa = list(
        name  = pad_string("CLSVENT.ZONAL", 16),
        units = "m/s"
      )
    ),

    ###

    u = list(

      description = "Wind speed in u direction",

      min = 0,
      max = 150,

      grib = list(
        name       = "u",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "x_wind",
        suffix = get("nc_level_suffixes")
      ),

      wrf = list(
        name = "U"
      ),

      fa = list(
        name  = pad_string("VENT_ZONAL", 16),
        units = "m/s"
      )
    ),
    ###

    v10m = list(

      description = "Wind speed in v direction at 10m above the ground",

      min = 0,
      max = 100,

      grib = list(
        name       = c("10v", "v"),
        level_type = c("heightAboveGround", "surface"),
        level      = 10
      ),

      netcdf = list(
        name = "y_wind_10m"
      ),

      wrf = list(
        name = "V10"
      ),

      fa = list(
        name  = pad_string("CLSVENT.MERIDIEN", 16),
        units = "m/s"
      )
    ),

    ###

    v = list(

      description = "Wind speed in v direction",

      min = 0,
      max = 150,

      grib = list(
        name       = "v",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "y_wind",
        suffix = get("nc_level_suffixes")
      ),

      wrf = list(
        name = "V"
      ),

      fa = list(
        name  = pad_string("VENT_MERIDIEN", 16),
        units = "m/s"
      )
    ),
    ###

    w = list(

      description = "Vertical (upward) wind speed",

      min = 0,
      max = 150,

      grib = list(
        name       = "w",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "upward_air_velocity",
        suffix = get("nc_level_suffixes")
      ),

      wrf = list(
        name = "W"
      )

    ),
    ###

    g10m = list(

      description = "Wind gust at 10m above the ground",

      min = 0,
      max = 150,

      grib = list(
        name       = "fg",
        level_type = c("heightAboveGround", "surface"),
        level      = 10
      ),

      netcdf = list(
        name = "wind_speed_of_gust"
      ),

      v = list(
        harp_param  = "G10m",
        name        = "GW",
        param_units = "m/s",
        type        = "SYNOP"
      )

    ),

    g = list(

      description = "Wind gust",

      min = 0,
      max = 150,

      grib = list(
        name       = "fg",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "wind_speed_of_gust",
        suffix = get("nc_level_suffixes")
      ),

      fa = list(
        name  = pad_string("VIT.RAFALES", 16),
        units = "m/s"
      )
    ),
    ###

    wg10m = list(

      description = paste(
        "Wind gust at 10m above the ground calculated",
        "from U gust and V gust at 10m above the ground"
      ),

      min = 0,
      max = 150,

      grib = list(
        name       = list(u = "ugst", v = "vgst"),
        level_type = c("heightAboveGround", "surface"),
        level      = 10
      ),

      netcdf = list(
        name = list(u = "x_wind_gust_10m", v = "y_wind_gust_10m")
      ),

      fa = list(
        name  = list(
          u = pad_string("CLSU.RAF.MOD.XFU", 16),
          v = pad_string("CLSV.RAF.MOD.XFU", 16)
        ),
        units = "m/s"
      ),

      func = function(u, v) sqrt(u ^ 2 + v ^ 2)

    ),
    ###

    wg = list(

      description = "Wind gust calculated from U and V gusts",

      min = 0,
      max = 150,

      grib = list(
        name       = list(u = "ugst", v = "vgst"),
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = list(u = "x_wind_gust", v = "y_wind_gust"),
        suffix = get("nc_level_suffixes")
      ),

      fa = list(
        name  = list(
          u = pad_string("U.RAF.MOD.XFU", 16),
          v = pad_string("V.RAF.MOD.XFU", 16)
        ),
        units = "m/s"
      ),

      func = function(u, v) sqrt(u ^ 2 + v ^ 2)

    ),
    ###

    gmax = list(

      description = "Maximum wind gust at 10m above the ground",

      min = 0,
      max = 150,

      v = list(
        harp_param  = "Gmax",
        name        = "GX",
        param_units = "m/s",
        type        = "SYNOP"
      )
    ),
    ###

    ugust10m = list(

      description = "Wind gust at 10m above the ground in U direction",

      min = 0,
      max = 150,

      grib = list(
        name       = "ugst",
        level_type = c("heightAboveGround", "surface"),
        level      = 10
      ),

      netcdf = list(
        name = "x_wind_gust_10m"
      ),

      fa = list(
        name  = pad_string("CLSU.RAF.MOD.XFU", 16),
        units = "m/s"
      )

    ),

    ugust = list(

      description = "Wind gust in U direction",

      min = 0,
      max = 150,

      grib = list(
        name       = "ugst",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "x_wind_gust",
        suffix = get("nc_level_suffixes")
      ),

      fa = list(
        name  = pad_string("U.RAF.MOD.XFU", 16),
        units = "m/s"
      )

    ),
    ###

    vgust10m = list(

      description = "Wind gust at 10m above the ground in V direction",

      min = 0,
      max = 150,

      grib = list(
        name       = "vgst",
        level_type = c("heightAboveGround", "surface"),
        level      = 10
      ),

      netcdf = list(
        name = "y_wind_gust_10m"
      ),

      fa = list(
        name  = pad_string("CLSV.RAF.MOD.XFU", 16),
        units = "m/s"
      )

    ),

    vgust = list(

      description = "Wind gust in V direction",

      min = 0,
      max = 150,

      grib = list(
        name       = "vgst",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "y_wind_gust",
        suffix = get("nc_level_suffixes")
      ),

      fa = list(
        name  = pad_string("V.RAF.MOD.XFU", 16),
        units = "m/s"
      )

    ),
    ###

    ################
    ### HUMIDITY ###
    ################
    q2m = list(

      description = "Specific humidity of air at 2m above the ground",

      grib = list(
        name       = c("2q", "q"),
        level_type = c("heightAboveGround", "surface"),
        level      = 2
      ),

      netcdf = list(
        name = "specific_humidity_2m"
      ),

      v = list(
        harp_param  = "Q2m",
        name        = "QQ",
        param_units = "kg/kg",
        type        = "SYNOP"
      ),

      wrf = list(
        name = "Q2"
      ),

      fa = list(
        name  = pad_string("CLSHUMI.SPECIFIQ", 16),
        units = "kg/kg"
      ),

      obsoul = list(
        name      = 7,
        units     = "kg/kg",
        harp_name = "Q2m"
      )
    ),



    ###

    q = list(

      description = "Specific humidity of air",

      grib = list(
        name       = "q",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "specific_humidity",
        suffix = get("nc_level_suffixes")
      ),

      v = list(
        harp_param  = "Q",
        name        = "QQ",
        param_units = "kg/kg",
        type        = "TEMP"
      ),

      wrf = list(
        name = "QVAPOR"
      ),

      fa = list(
        name  = pad_string("HUMI.SPECIFIQ", 16),
        units = "kg/kg"
      )
    ),

    ###

    rh2m = list(

      description = "Relative humidity of air at 2m above the ground",

      grib = list(
        name       = c("2r", "r"),
        level_type = c("heightAboveGround", "surface"),
        level      = 2
      ),

      netcdf = list(
        name = "relative_humidity_2m"
      ),

      v = list(
        harp_param  = "RH2m",
        name        = "RH",
        param_units = "percent",
        type        = "SYNOP"
      ),

      fa = list(
        name  = pad_string("CLSHUMI.RELATIVE", 16),
        units = "fraction"
      ),

      obsoul = list(
        name      = 58,
        units     = "percent",
        harp_name = "RH2m"
      )
    ),

    ###

    rh = list(

      description = "Relative humidity of air",

      grib = list(
        name       = "r",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "relative_humidity",
        suffix = get("nc_level_suffixes")
      ),

      v = list(
        harp_param  = "RH",
        name        = "RH",
        param_units = "percent",
        type        = "TEMP"
      ),

      fa = list(
        name  = pad_string("HUMI_RELATIVE", 16),
        units = "fraction"
      )
    ),

    #############
    ### CLOUD ###
    #############
    caf = list(

      description = "Cloud area fraction at vertical levels",

      min = 0,
      max = 8,

      grib = list(
        name       = "tcc",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "cloud_area_fraction",
        suffix = get("nc_level_suffixes")
      ),

      wrf = list(
        name = "CLDFRA"
      )
    ),

    ###

    cctot = list(

      description = "Total integrated cloud cover",

      other_names = "tcc",

      min = 0,
      max = 8,

      grib = list(
        name       = "tcc",
        level_type = c("heightAboveGround", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "cloud_area_fraction"
      ),

      v = list(
        harp_param  = "CCtot",
        name        = "NN",
        param_units = "oktas",
        type        = "SYNOP"
      ),

      wrf = list(
        name = "CLDFRA"
      ),

      fa = list(
        name  = pad_string("SURFNEBUL.TOTALE", 16),
        units = "fraction"
      ),

      obsoul = list(
        name = 91,
        units = "percent",
        harp_name = "NN"
      )
    ),

    ###

    cchigh = list(

      description = "High level cloud cover",

      other_names = "hcc",

      min = 0,
      max = 8,

      grib = list(
        name       = "hcc",
        level_type = c("heightAboveGround", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "high_type_cloud_area_fraction"
      ),

      v = list(
        harp_param  = "CChigh",
        name        = "HC",
        param_units = "oktas",
        type        = "SYNOP"
      ),

      fa = list(
        name  = pad_string("SURFNEBUL.HAUTE", 16),
        units = "fraction"
      )
    ),

    ###

    ccmed = list(

      description = "Medium level cloud cover",

      other_names = "mcc",

      min = 0,
      max = 8,

      grib = list(
        name       = "mcc",
        level_type = c("heightAboveGround", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "medium_type_cloud_area_fraction"
      ),

      v = list(
        harp_param  = "CCmed",
        name        = "MC",
        param_units = "oktas",
        type        = "SYNOP"
      ),

      fa = list(
        name  = pad_string("SURFNEBUL.MOYENN", 16),
        units = "fraction"
      )
    ),

    ###

    cclow = list(

      description = "Low level cloud cover",

      other_names = "lcc",

      min = 0,
      max = 8,

      grib = list(
        name       = "lcc",
        level_type = c("heightAboveGround", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "low_type_cloud_area_fraction"
      ),

      v = list(
        harp_param  = "CClow",
        name        = "LC",
        param_units = "oktas",
        type        = "SYNOP"
      ),

      fa = list(
        name  = pad_string("SURFNEBUL.BASSE", 16),
        units = "fraction"
      )
    ),
    ###

    cbase = list(

      description = "Height of cloud base",

      min = 0,

      netcdf = list(
        name = "cloud_base_altitude"
      ),

      v = list(
        harp_param  = "Cbase",
        name        = "CH",
        param_units = "m",
        type        = "SYNOP"
      )
    ),
    ###

    cc_below_7500 = list(

      description = "Cloud cover below 7500m",

      min = 0,
      max = 8,

      v = list(
        harp_param  = "N75",
        name        = "N75",
        param_units = "oktas",
        type        = "SYNOP"
      )
    ),

    #####################
    ### PRECIPITATION ###
    #####################
    pcp = list(

      description = "Accumulated precipitaion",

      min = 0,
      max = 1000,

      grib = list(
        name       = "tp",
        level_type = c("heightAboveGround", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "precipitation_amount_acc"
      ),

      wrf = list(
        name = list("RAINNC", "RAINC"),
        func = "sum"
      ),

      v = list(
        harp_param  = "Pcp",
        name        = "PE",
        param_units = "kg/m^2",
        type        = "SYNOP"
      ),

      fa = list(
        name = list(
          alaro = list(
            pad_string("SURFPREC.EAU.GEC", 16), pad_string("SURFPREC.EAU.CON", 16),
            pad_string("SURFPREC.NEI.GEC", 16), pad_string("SURFPREC.NEI.CON", 16)
          ),
          arome = list(
            pad_string("SURFACCPLUIE", 16), pad_string("SURFACCNEIGE", 16),
            pad_string("SURFACCGRAUPEL", 16)
          )
        ),
        units = "kg/m^2",
        func  = "sum"
      )

    ),
    ###

    accpcp1h = list(

      description = "1-hour accumulated precipitation",

      min = 0,
      max = 500,

      v = list(
        harp_param  = "AccPcp1h",
        name        = "PE1",
        param_units = "kg/m^2",
        type        = "SYNOP"
      ),

      obsoul = list(
        name = 79,
        units = "mm",
        harp_name = "AccPcp1h"

      )
    ),
    ###

    accpcp3h = list(

      description = "3-hour accumulated precipitation",

      min = 0,
      max = 600,

      v = list(
        harp_param  = "AccPcp3h",
        name        = "PE3",
        param_units = "kg/m^2",
        type        = "SYNOP"
      )
    ),
    ###

    accpcp6h = list(

      description = "6-hour accumulated precipitation",

      min = 0,
      max = 750,

      v = list(
        harp_param  = "AccPcp6h",
        name        = "PE6",
        param_units = "kg/m^2",
        type        = "SYNOP"
      ),

      obsoul = list(
        name = 80,
        units = "mm",
        harp_name = "AccPcp6h"

      )
    ),
    ###

    accpcp12h = list(

      description = "12-hour accumulated precipitation",

      min = 0,
      max = 1000,

      v = list(
        harp_param  = "AccPcp12h",
        name        = "PE12",
        param_units = "kg/m^2",
        type        = "SYNOP"
      )
    ),
    ###

    accpcp24h = list(

      description = "24-hour accumulated precipitation",

      min = 0,
      max = 1000,

      v = list(
        harp_param  = "AccPcp24h",
        name        = "PE24",
        param_units = "kg/m^2",
        type        = "SYNOP"
      )
    ),
    ###

    snow = list(

      description = "Snow depth",

      obsoul = list(
        name      = 92,
        units     = "cm",
        harp_name = "Snow"
      )
    ),

    ################
    ### PRESSURE ###
    ################
    pmsl = list(

      description = "Air pressure at mean sea level",

      other_names = "mslp",

      min = 90000,
      max = 110000,

      grib = list(
        name       = "msl",
        level_type = c("meanSea", "heightAboveSea"),
        level      = 0
      ),

      netcdf = list(
        name = "air_pressure_at_sea_level"
      ),

      v = list(
        harp_param  = "Pmsl",
        name        = "PS",
        param_units = "hPa",
        type        = "SYNOP"
      ),

      fa = list(
        name  = pad_string("MSLPRESSURE", 16),
        units = "Pa"
      ),

      obsoul = list(
        name      = 1,
        units     = "Pa",
        harp_name = "Pmsl"
      )
    ),
    ###

    psfc = list(

      description = "Surface air pressure",

      other_names = "ps",

      min = 70000,
      max = 110000,

      grib = list(
        name       = "pres",
        level_type = c("heightAboveGround", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "surface_air_pressure"
      ),

      v = list(
        harp_param  = "Ps",
        name        = "PSS",
        param_units = "hPa",
        type        = "SYNOP"
      ),

      fa = list(
        name  = pad_string("SURFPRESSION", 16),
        units = "Pa"
      )
    ),

    ##############
    ### OTHERS ###
    ##############

    z = list(

      description = "Geopotential",

      grib = list(
        name       = "z",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "geopotential",
        suffix = get("nc_level_suffixes")
      ),

      wrf = list(
        name = "PH"
      ),

      fa = list(
        name  = pad_string("GEOPOTENTIEL", 16),
        units = "m^2/s^2"
      )

    ),
    ###

    gh = list(

      description = "Geopotential height",

      grib = list(
        name       = "gh",
        level_type = get("grib_level_types")
      ),

      netcdf = list(
        name   = "geopotential_height",
        suffix = get("nc_level_suffixes")
      ),

      wrf = list(
        name = "Z"
      ),

      v = list(
        harp_param  = "Z",
        name        = "FI",
        param_units = "m",
        type        = "TEMP"
      )

    ),
    ###

    sfc_geo = list(

      description = "Surface geopotential",

      other_names = c("z0m", "sfc_geopotential"),

      grib = list(
        name       = "z",
        level_type = c("heightAboveGround", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "surface_geopotential"
      ),

      fa = list(
        name  = pad_string("SURFGEOPOTENTIEL", 16),
        units = "m^2/s^2"
      )
    ),
    ###

    topo = list(

      description = "Height of topography above sea level",

      other_names = c(
        "oro", "orog", "orography", "topo", "topog", "topography",
        "altitude", "terrain", "model_elevation", "elev", "elevation"
      ),

      grib = list(
        name       = "orog",
        level_type = c("heightAboveGround", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "altitude"
      ),

      wrf = list(
        name = "HGT"
      ),

      v = list(
        harp_param  = "model_elevation",
        name        = "FI",
        param_units = "m",
        type        = "SYNOP"
      )
    ),
    ###

    lsm = list(

      description = "land-sea mask",

      other_names = c("land_sea_mask", "mask"),

      grib = list(
        name       = "lsm",
        level_type = c("heightAboveGround", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "land_binary_mask"
      ),

      wrf = list(
        name = "LANDMASK"
      ),

      fa = list(
        name  = pad_string("SURFIND.TERREMER", 16),
        units = "1"
      )

    ),
    ###

    sea_ice = list(

      description = "Sea ice concentration",

      other_names = c("sea_ice_concentration", "icec"),

      grib = list(
        name       = "icec",
        level_type = c("meanSea", "heightAboveSea", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "ga_icec_102"
      )

    ),
    ###

    vis = list(

      description = "Visibility in air",

      grib = list(
        name       = "vis",
        level_type = c("heightAboveGround", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "visibility_in_air"
      ),

      v = list(
        harp_param  = "vis",
        name        = "VI",
        param_units = "m",
        type        = "SYNOP"
      )

    ),
    ###

    pressure = list(

      description = "Atmospheric air pressure",

      min = 5,
      max = 1100,

      grib = list(
        name       = "pres",
        level_type = get("grib_level_types")
      ),

      v = list(
        harp_param  = "p",
        name        = "PP",
        param_units = "hPa",
        type        = "TEMP"
      )
    ),
    ###

    cape = list(

      description = "Convective available potential energy",

      grib = list(
        name       = "cape",
        level_type = c("heightAboveGround", "surface"),
        level      = 0
      ),

      netcdf = list(
        name = "specific_convective_available_potential_energy"
      ),

      fa = list(
        name  = pad_string("SURFCAPE.POS.F00", 16),
        units = "J/kg"
      )
    ),
    ###

    cin = list(

      description = "Convective inhibition",

      netcdf = list(
        name = "atmosphere_convective_inhibition"
      ),

      fa = list(
        name  = pad_string("SURFCIEN.POS.F00", 16),
        units = "J/kg"
      )

    )
    ###

  )

}

#' Parameter definitions for reading from files
#'
#' \code{add_param_def} adds a new parameter definition to the input list and
#' \code{modify_param_def} modifies an entry for an existing parameter. Use
#' \code{show_param_defs} to show the names and descriptions of the paramaters
#' in the \code{param_defs} list, and pass the file format as an argument to
#' show the parameters for that file format.
#'
#' @name parameter_definitions
#'
#' @param param The name of the parameter to add
#' @param param_defs A list of parameter definitions to modify. Defaults to the
#'   built in list of parameters.
#' @param description A description of the parameter.
#' @param min,max The minimum and maximumvalues the parameter can have - only
#'   used when reading observations to do a gross error check.
#' @param grib A list with the grib shortName, and optionally levelType and
#' level. Use \link{new_grib_param} to generate.
#' @param netcdf A list with the name of the parameter in the netcdf file and
#'   optionally a named list of suffixes to add to the parameter name for
#'   different vertical coordinate systems. Use \link{new_netcdf_param} to
#'   generate.
#' @param v A list with the name of the parameter in vfld / vobs files, the
#'   output name, units and type (synop or temp). Use \link{new_v_param} to
#'   generate.
#' @param fa A list with the name of the parameter in FA files and its units.
#'   Use \link{new_fa_param} to generate.
#' @param obsoul A list with the name of the paramater in obsoul files (this is
#'   usually numeric), its units and the output name. Use
#'   \link{new_obsoul_param} to generate.
#' @param func A function to apply on reading. For the function to be used, the
#'   "name" element of the list for the file type must be a named list with
#'   names matching the names of the function arguments. See, for example,
#'   \code{harp_params$ws10m} for how this should be done.
#' @param ... Other file types.
#'
#' @return A modified version of the \code{param_defs} input to the function.
NULL

#'
#' @rdname parameter_definitions
#' @export
#'
#' @examples
#' add_param_def(
#'   "u100m",
#'   description = "Zonal wind Speed at 100m above ground",
#'   grib = new_grib_param("u", "heightAboveGround", 100),
#'   netcdf = new_netcdf_param("x_wind_100m"),
#'   v = new_v_param("U100", "u100m", "m/s", "SYNOP")
#' )
#'
add_param_def <- function(
  param,
  param_defs  = getExportedValue("harpIO", "harp_params"),
  description = "",
  min         = -Inf,
  max         = Inf,
  grib        = NULL,
  netcdf      = NULL,
  v           = NULL,
  fa          = NULL,
  obsoul      = NULL,
  func        = NULL,
  ...
) {

  if (is.element(param, names(param_defs))) {
    cli::cli_abort(c(
      "{.arg {param}} already exists in `param_defs`.",
      "i" = "Use `modify_param_def()` instead."
    ))
  }

  param_defs[[param]] <- list(
    description = description,
    min         = min,
    max         = max,
    grib        = grib,
    netcdf      = netcdf,
    v           = v,
    fa          = fa,
    obsoul      = obsoul,
    func        = func,
    ...
  )

  non_nulls <- !vapply(param_defs[[param]], is.null, logical(1))
  param_defs[[param]] <- param_defs[[param]][non_nulls]
  check_param_defs(param_defs)
  param_defs
}

#' @rdname parameter_definitions
#' @export
#'
#' @examples
#' # Use a function to convert sea level pressure to hPa from Pa for grib files
#' my_params <- modify_param_def(
#'   "pmsl",
#'   grib = new_grib_param(
#'     name = list(p = "msl"),
#'     level_type = c("meanSea", "heightAboveSea"),
#'     level = 0
#'   ),
#'   func = function(p) p / 100
#' )
#' my_params$pmsl
modify_param_def <- function(
  param,
  param_defs  = getExportedValue("harpIO", "harp_params"),
  description = NULL,
  min         = NULL,
  max         = NULL,
  grib        = NULL,
  netcdf      = NULL,
  v           = NULL,
  fa          = NULL,
  obsoul      = NULL,
  func        = NULL,
  ...
) {

  if (!is.element(param, names(param_defs))) {
    cli::cli_abort(c(
      "{.arg {param}} does not exist `param_defs`.",
      "i" = "Use `add_param_def() instead."
    ))
  }

  arg_names <- c(rlang::fn_fmls_names(), names(list(...)))
  arg_names <- arg_names[!arg_names %in% c("param", "param_defs", "...")]

  for (arg_name in arg_names) {
    if (!is.element(arg_name, rlang::fn_fmls_names())) {
      assign(arg_name, list(...)[[arg_name]])
    }
    arg_val <- get(arg_name)
    if (!is.null(arg_val)) {
      param_defs[[param]][[arg_name]] <- arg_val
    }
  }

  param_defs
}

#' @rdname parameter_definitions
#' @param param The name of a parameter to extract from \code{param_defs}
#' @param file_format The file format for which to get the parameter definition
#' @export
#' @examples
#'
#' # Get parameter definition for topography
#' get_param_def("topo")
#'
#' # For grib files
#' get_param_def("topo", "grib")
#'
#' # Aliases also work - stored in the \code{other_names} entry
#' get_param_def("terrain")
get_param_def <- function(
  param, file_format = NULL,
  param_defs = getExportedValue("harpIO", "harp_params")
) {

  res <- param_defs[[param]]
  if (length(res) < 1) {
    idx <- param_from_other_names(param, param_defs)
    if (length(idx) < 1) {
      cli::cli_abort(c(
        "{.arg param} not found in {.arg param_defs}.",
        "x" = "param = {.var {param}} not found."
      ))
    }
    res <- param_defs[[idx]]
  }

  if (is.null(file_format)) {
    return(res)
  }

  res <- res[[file_format]]
  if (length(res) < 1) {
    cli::cli_abort(c(
      "{.arg file_format} not found for {.arg param} in {.arg param_defs}.",
      "x" = "file_format = {.var {file_format}} not found for {.var {param}}."
    ))
  }

  res

}

#' Make a parameter definition list for a file format
#'
#' These functions are to be used together with \link{add_param_def} and
#' \link{modify_param_def} to add a parameter definition entry for a particular
#' file format.
#'
#' @name file_parameter_definitions
#'
#' @param name The name of the parameter in the file. In the case of grib files,
#' this should be the grib shortName and in the case of obsoul files, should be
#' the number used to define what the variable is. In addition, can be expressed
#' as a character vector for grib files in order to try different shortNames
#' until the paramatet is found. Should be a named list if a function is to be
#' applied with the names matching the argument names in the function.
NULL

#' @param level_type The grib levelType. Can be a character vector in order to
#'   try different levelTypes until the variable is found in the file. The
#'   default is a named list using the in built grib_level_types to define the
#'   names for diffferent vertical coordinate systems.
#' @param level The level number. Set to -999 for all levels.
#'
#' @rdname file_parameter_definitions
#' @export
new_grib_param <- function(
  name,
  level_type = get("grib_level_types"),
  level      = NULL
) {
  grib_param <- list(
    name = name, level_type = level_type, level = level
  )
  non_nulls <- !vapply(grib_param, is.null, logical(1))
  grib_param[non_nulls]
}

#' @param suffix The suffix to add to \code{name} for different vertical
#'   coordinate systems.
#'
#' @rdname file_parameter_definitions
#' @export
new_netcdf_param <- function(name, suffix = get("nc_level_suffixes")) {
  nc_param <- list(name = name, suffix = suffix)
  non_nulls <- !vapply(nc_param, is.null, logical(1))
  nc_param[non_nulls]
}

#' @param units The units of the parameter in the FA files.
#' @param name_len The number of characters that \code{name} should have in the
#'   FA files. This is 16 by default and adds blank spaces to the end of
#'   \code{name} to make it the required length.
#'
#' @rdname file_parameter_definitions
#' @export
new_fa_param <- function(name, units, name_len = 16) {
  list(name = pad_string(name, name_len), units = units)
}

#' @param harp_param The name of the parameter to be used in outputs from read
#'   functions.
#' @param units The units of the parameter in vfld / vobs files.
#' @param type The type of the parmater - either "SYNOP" or "TEMP"
#'
#' @rdname file_parameter_definitions
#' @export
new_v_param <- function(name, harp_param, units, type = c("SYNOP", "TEMP")) {
  type <- match.arg(type)
  list(
    harp_param = harp_param, name = name, param_units = units, type = type
  )
}

#' @rdname file_parameter_definitions
#' @export
new_obsoul_param <- function(name, harp_param, units) {
  list(name = name, units = units, harp_param = harp_param)
}

check_param_defs <- function(
  param_defs = getExportedValue("harpIO", "harp_params"),
  caller = rlang::caller_env()
) {
  params_with_func <- vapply(
    param_defs, function(x) !is.null(x[["func"]]), logical(1)
  )
  param_defs <- param_defs[params_with_func]

  checks <- mapply(check_formals, names(param_defs), param_defs)

  bad_params <- which(vapply(checks, function(x) x == "error", logical(1)))
  if (length(bad_params) > 0) {
    cli::cli_abort(
      "Mismatch between function formals and param names", call = caller
    )
  }

}

check_formals <- function(param_name, param) {
  func_vars      <- sort(names(formals(param[["func"]])))
  param          <- param[vapply(param, is.list, logical(1))]
  file_type_idx  <- vapply(param, function(x) !is.null(x[["name"]]), logical(1))

  file_type_name_err <- vapply(
    param[file_type_idx],
    function(x) is.list(x[["name"]]) && !all(sort(names(x[["name"]])) == func_vars),
    logical(1)
  )

  bad_file_types <- which(file_type_name_err)
  if (length(bad_file_types) > 0) {
    var <- names(param[file_type_idx][bad_file_types])
    cli::cli_bullets(c(
      "Error for {.var {param_name}}",
      "x" = "Function formals and param names do not match for {.var {var}}.",
      "i" = "Function formals = {.var {func_vars}}"
    ))
    return("error")
  }
  return("ok")
}
