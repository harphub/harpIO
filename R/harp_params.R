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

harp_params <- function() {
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
      )
    ),
    ###

    t = list(

      description = "Air temperature",

      min = 173,
      max = 333,

      grib = list(
        name       = "t",
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "air_temperature",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "dew_point_temperature",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "wind_speed",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = list(u = "x_wind", v = "y_wind"),
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "wind_direction",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = list(u = "x_wind", v = "y_wind"),
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "x_wind",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "y_wind",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "upward_air_velocity",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "wind_speed_of_gust",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = list(u = "x_wind_gust", v = "y_wind_gust"),
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "x_wind_gust",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "y_wind_gust",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "specific_humidity",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "relative_humidity",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "cloud_area_fraction",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "geopotential",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
      ),

      netcdf = list(
        name   = "geopotential_height",
        suffix = nc_level_suffixes()
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
        level_type = grib_level_types()
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
