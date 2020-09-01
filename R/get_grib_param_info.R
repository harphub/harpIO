#' Get grib paramater information
#'
#' Gets the shortname and parameter number for a HarmonEPS grib file. For
#' surface and screen level parameters the level type and level number are
#' assumed. For upper air parameters the level and, optionally, the level type
#' must be supplied. The default is for pressure levels.
#'
#' @param param Parameter name (or harp_param object)
#' @param vertical_coordinate The vertical coordinate for upper air data. May be
#'   "pressure", "model" or "height".
#' @return A list with \code{short_name}, \code{param_number},
#'   \code{level_type}, \code{level_numbe.r}
#' @export
#'
#' @examples
#' get_grib_param_info("pcp")
#' get_grib_param_info("t2m")
#' get_grib_param_info("t500")
#' get_grib_param_info("rh32h")
#'
get_grib_param_info <- function(param, vertical_coordinate = NA_character_) {
  if (!inherits(param, "harp_parameter")) {
    param <- parse_harp_parameter(param, vertical_coordinate = vertical_coordinate)
  }
  # NOTE: the following allows for local exceptions to be implemented:
  if (existsFunction(grib_override)) {
    if (!is.null(grib_override(param$fullname))) return(grib_override(param$fullname))
  }
  # NOTE: - the parameter number is NEVER used, only kept for the time being
  #           it would not work for grib-2 in any case
  #       - level_type is DIFFERENT in grib2! So we need a second entry
  #           We use the standard 255 for missing level_type
  #       - There is no standard for missing level_number (it could signify 3D extraction)
  #           But NA may problematic for comparisons, so (for now) we reset to -999.
  #       - GRIB1 level_types: table3 (but only the most important ones)
  # NOTE: some definitions may need to be adapted if GRIB files turn up 
  #    with different conventions. For instance, maybe not everybody encodes cloud cover 
  #    with level type 105, level 0 (0m above surface). It could also be lev. type 1.
  #    In fact, there may be more cases where it could be best to set level_type=255
  #    then it could be ignored in the message filtering process.
  levtype <- switch(
    param$level_type,
    "surf"     = 1,
    "pressure" = 100,
    "msl"      = 102, # NOTE: sometimes 103 may be used: 0m above MSL
    "height"   = 105,
    "model"    = 109,
    "unknown"  = 255,
    param$level_type
  )

  level <- param$level
  if (tolower(param$basename) %in% c("caf", "t", "z", "u", "v", "w", "q", "rh") && is.null(level)) {
    stop("Level must be supplied for ", param$fullname)
  }

  switch(tolower(param$basename),
    "pcp"      = {
      short_name   <-  "tp"
#      param_number <-  61
      level_type <- c(1, 105)
      level_number <-  0
    },
    "fog"      = {probably
      short_name   <-  "tcc"
#      param_number <-  71
      level_type   <-  109
## AD: this is hard-coded for one specific project...
      level_number <-  65
    },
    "caf"      = {
      short_name   <-  "tcc"
#      param_number <-  71
      level_type   <-  levtype
      level_number <-  level
    },
    "tcc"      =,
    "cctot"    = {
      short_name   <-  "tcc"
#      param_number <-  71
      level_type   <-  105
      level_number <-  0
    },
    "hcc"      =,
    "cchigh"   = {
      short_name   <-  "hcc"
#      param_number <-  71
      level_type   <-  105
      level_number <-  0
    },
    "mcc"      =,
    "ccmed"    = {
      short_name   <-  "mcc"
#      param_number <-  71
      level_type   <-  105
      level_number <-  0
    },
    "lcc"      =,
    "cclow"    = {
      short_name   <-  "lcc"
#      param_number <-  71
      level_type   <-  105
      level_number <-  0
    },
    "sst"      = {
      short_name   <-  "t"
#      param_number <-  11
      level_type   <-  102
      level_number <-  0
    },
    ## AD: these names will cause problems: the number would be seen as a pressure!
    ## So we need new names for this!
    "tg1"      = {
      short_name   <-  "t"
#      param_number <-  11
      level_type   <-  105
      level_number <-  800
    },
    "tg2"      = {
      short_name   <-  "t"
#      param_number <-  11
      level_type   <-  105
      level_number <-  801
    },
    "t"        = {
      short_name   <-  if (levtype==105 && level==2) "2t" else "t"
#      param_number <-  11
      level_type   <-  levtype
      level_number <-  level
    },
    "mslp"     = ,
    "pmsl"     = {
      short_name   <-  "msl"
#      param_number <-  1
      level_type   <-  c(102, 103)
      level_number <-  0
    },
    "ps" = {
      short_name   <- "pres"
#      param_number <- 1
      level_type   <- c(105, 1)
      level_number <- 0
    },
    "terrain"    = ,
    "elev"       = ,
    "elevation"  = ,
    "altitude"   = ,
    "topo"       = ,
    "topog"      = ,
    "topography" = ,
    "oro"        = ,
    "orog"       = ,
    "orography"  = {
      short_name   <- "orog"
#      param_number <- 7
      level_type   <- c(105, 1)
      level_number <- -999
    },
    # FIXME: "z0m" should never arrise: that would have basename "z" (see next)
    "sfc_geopotential" = ,
    "sfc_geo"          = ,
    "z0m"              = {
      short_name   <-  "z"
#      param_number <-  6
      level_type   <-  c(105, 1)
      level_number <-  0
    },
    "z"        = {
    # NOTE: z0m *may* have levtype=1 rather than 105!
      short_name   <-  "z"
#      param_number <-  6
      level_type   <-  if (levtype==105 && level==0) c(levtype, 1) else levtype
      level_number <-  level
    },
    # NOTE: ECMWF uses "10si" for 10m wind speed
    #   so "ws" may not work for some parameter tables!
    #   grib2 needs 10si
    "s"     = {
#      short_name   <-  "ws"
      short_name   <-  if (levtype==105 && level==10) c("ws", "10si") else "ws"
#      param_number <-  32
      level_type   <-  levtype
      level_number <-  level
    },
    "d"     = {
      short_name   <-  "wdir"
#      param_number <-  31
      level_type   <-  levtype
      level_number <-  level
    },
    "u"        = {
      short_name   <-  "u"
#      param_number <-  33
      level_type   <-  levtype
      level_number <-  level
      if (levtype==105) {
        if (level==10) {
          short_name <- "10u"
        } else if (level==100) {
          short_name <- "100u"
#          param_number <- 246
          level_type   <-  1
          level_number <-  0
        }
      }
    },
    "v"        = {
      short_name   <-  "v"
#      param_number <-  34
      level_type   <-  levtype
      level_number <-  level
      if (levtype==105) {
        if (level==10) {
          short_name <- "10v"
        } else if (level==100) {
          short_name <- "100v"
#          param_number <- 247
          level_type   <-  1
          level_number <-  0
        }
      }
    },
    "w"        = {
      short_name   <-  "tw"
#      param_number <-  40
      level_type   <-  levtype
      level_number <-  level
    },
    "q"        = {
      short_name   <-  "q"
#      param_number <-  51
      level_type   <-  levtype
      level_number <-  level
    },
    "rh"       = {
      short_name   <-  "r"
#      param_number <-  52
      level_type   <-  levtype
      level_number <-  level
    },
    "ugust10m" = {
      short_name   <-  "ugst"
#      param_number <-  162
      level_type   <-  105
      level_number <-  10
    },
    "vgust10m" = {
      short_name   <-  "vgst"
#      param_number <-  163
      level_type   <-  105
      level_number <-  10
    },
    {
      short_name   <- tolower(param$basename)
#      param_number <- NA
      level_type   <- levtype
      level_number <- level
    }
  )
  
  # for GRIB2, we need different level_type values!
  # Or should we use the name of these types?
  level_type_2 <- vapply(level_type, FUN.VAL=1, 
                         FUN=function (x) switch(as.character(x),
                                                 "1"  = 1,
                                                 "100"  = 100,
                                                 "102"  = 101,
                                                 "105"  = 103,
                                                 "109"  = 105,
                                                 "255"  = 255,
                                                 255))
  if (any(is.na(level_type))) level_type[is.na(level_type)] <- 255
  if (any(is.na(level_type_2))) level_type_2[is.na(level_type_2)] <- 255
  if (is.na(level_number)) level_number <- -999

  list(
    short_name   = short_name,    # shortName
#    param_number = param_number,  # indicatorOfParameter or parameterNumber
    level_type   = level_type,    # indicatorOfTypeOfLevel (grib1),
    level_type_2 = level_type_2,  # typeOfFirstFixedSurface
    level_number = level_number   # level
  )
}
