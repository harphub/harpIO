#' Get grib paramater information
#'
#' Gets the shortname and parameter number for a HarmonEPS grib file. For
#' surface and screen level parameters the level type and level number are
#' assumed. For upper air parameters the level and, optionally, the level type
#' must be supplied. The default is for pressure levels.
#'
#' @param param Parameter name (or harp_param object)
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

  levtype <- switch(param$level_type,
                    "height"   = 105,
                    "msl"      = 102,
                    "surf"     = 1,
                    "pressure" = 100,
                    "model"    = 109,
                    NULL)
  level <- param$level
  if (tolower(param$basename) %in% c("caf", "t", "z", "u", "v", "w", "q", "rh") && is.null(level)) {
    stop("Level must be supplied for ", param$fullname)
  }

  switch(tolower(param$basename),
    "pcp"      = {
      short_name   <-  "tp"
      param_number <-  61
      level_type   <-  105
      level_number <-  0
    },
    "fog"      = {
      short_name   <-  "tcc"
      param_number <-  71
      level_type   <-  109
## AD: this is hard-coded for one specific project...
      level_number <-  65
    },
    "caf"      = {
      short_name   <-  "tcc"
      param_number <-  71
      level_type   <-  levtype
      level_number <-  level
    },
    "tcc"      =,
    "cctot"    = {
      short_name   <-  "tcc"
      param_number <-  71
      level_type   <-  105
      level_number <-  0
    },
    "hcc"      =,
    "cchigh"   = {
      short_name   <-  "hcc"
      param_number <-  71
      level_type   <-  105
      level_number <-  0
    },
    "mcc"      =,
    "ccmed"    = {
      short_name   <-  "mcc"
      param_number <-  71
      level_type   <-  105
      level_number <-  0
    },
    "lcc"      =,
    "cclow"    = {
      short_name   <-  "lcc"
      param_number <-  71
      level_type   <-  105
      level_number <-  0
    },
    "sst"      = {
      short_name   <-  "t"
      param_number <-  11
      level_type   <-  102
      level_number <-  0
    },
    ## AD: these names will cause problems: the number would be seen as a pressure!
    ## So we need new names for this!
    "tg1"      = {
      short_name   <-  "t"
      param_number <-  11
      level_type   <-  105
      level_number <-  800
    },
    "tg2"      = {
      short_name   <-  "t"
      param_number <-  11
      level_type   <-  105
      level_number <-  801
    },
    "t"        = {
      short_name   <-  if (levtype==105 && level==2) "2t" else "t"
      param_number <-  11
      level_type   <-  levtype
      level_number <-  level
    },
    "mslp"     = ,
    "pmsl"     = {
      short_name   <-  "msl"
      param_number <-  1
      level_type   <-  103
      level_number <-  0
    },
    "topo"     = ,
    "terrain"  = ,
    "altitude" = {
      short_name   <-  "z"
      param_number <-  6
      level_type   <-  c(105, 1)
      level_number <-  0
    },
    "z"        = {
      short_name   <-  "z"
      param_number <-  6
      level_type   <-  levtype
      level_number <-  level
    },
    "s"     = {
      short_name   <-  "ws"
      param_number <-  32
      level_type   <-  levtype
      level_number <-  level
    },
    "d"     = {
      short_name   <-  "wdir"
      param_number <-  31
      level_type   <-  levtype
      level_number <-  level
    },
    "u"        = {
      short_name   <-  "u"
      param_number <-  33
      level_type   <-  levtype
      level_number <-  level
      if (levtype==105) {
        if (level==10) {
          short_name <- "10u"
        } else if (level==100) {
          short_name <- "100u"
          param_number <- 246
          level_type   <-  1
          level_number <-  0
        }
      }
    },
    "v"        = {
      short_name   <-  "v"
      param_number <-  34
      level_type   <-  levtype
      level_number <-  level
      if (levtype==105) {
        if (level==10) {
          short_name <- "10v"
        } else if (level==100) {
          short_name <- "100v"
          param_number <- 247
          level_type   <-  1
          level_number <-  0
        }
      }
    },
    "w"        = {
      short_name   <-  "tw"
      param_number <-  40
      level_type   <-  levtype
      level_number <-  level
    },
    "q"        = {
      short_name   <-  "q"
      param_number <-  51
      level_type   <-  levtype
      level_number <-  level
    },
    "rh"       = {
      short_name   <-  "r"
      param_number <-  52
      level_type   <-  levtype
      level_number <-  level
    },
    "ugust10m" = {
      short_name   <-  "ugst"
      param_number <-  162
      level_type   <-  105
      level_number <-  10
    },
    "vgust10m" = {
      short_name   <-  "vgst"
      param_number <-  163
      level_type   <-  105
      level_number <-  10
    },
    {
      short_name   <- NA
      param_number <- NA
      level_type   <- NA
      level_number <- NA
    }
  )

  list(
    short_name   = short_name,
    param_number = param_number,
    level_type   = level_type,
    level_number = level_number
  )
}
