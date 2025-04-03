#' Parse the parameter name and decide on level, accumulation...
#'
#' Parses a harp-style parameter name and returns accumulation time, level (if
#' any) and the base name of the parameter
#'
#' @param param Parameter name - available names can be found from
#'   \link{show_param_defs}.
#' @param vertical_coordinate If the parameter is for the upper air, the
#'   vertical coordinate system must be given. Can be "pressure" for pressure
#'   levels, "model" for model levels or "height" for altitude levels.
#'
#' @return A list (of class harp_parameter) with \code{fullname},
#'   \code{basename}, \code{level_type}, \code{level}, \code{accum}
#' @export
#'
#' @examples
#' parse_harp_parameter("AccPcp1h")
#' parse_harp_parameter("z500")
#' parse_harp_parameter("s100m")
#' parse_harp_parameter("T", vertical_coordinate = "pressure")
#' parse_harp_parameter("S", vertical_coordinate = "model")
#' parse_harp_parameter("RH", vertical_coordinate = "height")
parse_harp_parameter <- function(
  param,
  vertical_coordinate = c(NA_character_, "pressure", "model", "height", "isotherm", "unknown")
) {
  # NOTE: - we use level_number -999 to represent "missing" or "all levels"
  #          both contexts mean that you don't filter on level_number
  #       - level_type should ideally never be NA but "unknown" or such.
  #          but in any case the different file formats (grib, fa...)
  #          may need to modify this. e.g. GRIB uses 255 for missing level_type.
  vertical_coordinate <- match.arg(vertical_coordinate)
  ## TODO: radiation, surface properties...
  if (inherits(param, "harp_parameter")) return(param)
  # all comparisons are done in lower case
  if (tolower(param) %in% c("caf", "t", "z", "u", "v", "w", "q", "rh", "s", "d", "td", "ws")  |
      grepl("_ml$", param) | grepl("_pl$", param)) {
    if (is.na(vertical_coordinate)) {
      if (grepl("_ml$", param)) {
        lev_type = "s"
      } else if (grepl("_pl$", param)) {
        lev_type = "p"
      } else {
        stop(
          "Level not supplied for ", param, ". Either give a numeric level,\n",
          "or set \"vertical_coordinate\".",
          call. = FALSE
        )

      }
    } else {
      lev_type <- switch(
        vertical_coordinate,
        "pressure" = "p",
        "model"    = "s",
        "height"   = "m",
        "isotherm" = "i"
      )
    }
    basename <- paste0(param, "-999", lev_type)
  } else {
    basename <- param
  }

  fullname <- param

  plen <- nchar(basename)

  ## 1. accumulated field? must be AccXXnnh (last character is h, m, s)
  ## we assume the only digits in the parameter name are for accumulation
  if (grepl("^acc[[:alpha:]]+[[:digit:]]+[hms]", tolower(basename))){
    acc1 <- regexpr("[[:digit:]]", basename)
    accum <- as.numeric(substr(basename, acc1, plen-1))
    acc_unit <- substring(basename, plen)
    accum <- switch(acc_unit,
      "s"=accum,
      "m"=accum*60,
      "h"=accum*3600,
      stop("unknown accumulation unit in ", basename))

    basename <- substr(basename, 4, acc1 - 1)
    plen <- nchar(basename)
  } else {
    accum <- 0
    acc_unit <- NULL
  }

  ## 2. 'basic' field or atmospheric with level information added?
  if (grepl("^[[:graph:]]+-?[[:digit:]]+[mpshi]?$", basename)){
    ## trailing digits are interpreted as pressure levels
    ## possibly there's an extra "m": then they are height levels
    ## or an "s", "h": hyprid level (model level)
    ## or "p": pressure level (usually not written)
    lev1 <- regexpr("-?[[:digit:]]", basename)
    lev <- substr(basename, lev1, plen)
    basename <- substr(basename, 1, lev1 - 1)

    lt <- regexpr("[[:alpha:]]", lev)
    if (lt>0) {
      ## parse the "level" part
      level_type <- switch(substring(lev, lt),
        "m" = "height",
        "h" =,
        "s" = "model",
        "p" = "pressure",
        "i" = "isotherm",
        substring(lev, lt))
      level <- as.numeric(substr(lev, 1, lt-1))
    } else {
      ## default level type is "pressure"
      level_type <- "pressure"
      level <- as.numeric(lev)
    }
  } else {
    ## no level defined. could be surface, but also MSL, CLOUDS, ...
    level <- -999
    level_type <- "unknown"
  }
  # AD: probably not useful at all:
  #     this is quite "GRIB" specific and get_grib_param_info() can deal with it
  level_type <- switch(basename,
         "pmsl" = ,
         "mslp" = "msl",
         "psfc" = ,
         "lsm"  = ,
         "pcp"  = ,
         "rain" = ,
         "snow" = ,
         "sst"  = , # surface also includes sea surface
         "tg"   = "surface",
         level_type)

  if (grepl("[[:digit:]]$", level_type)) {
    level_type <- "unknown"
    basename   <- fullname
  }

  result <- list(fullname = fullname, basename = basename,
       level = level, level_type = level_type,
       accum = accum, acc_unit = acc_unit)
  class(result) <- "harp_parameter"
  result
}


is_synop <- function(prm, vertical_coordinate = NA_character_, param_defs = getExportedValue("harpIO", "harp_params")) {
  # return TRUE if a parameter is a typical "synop"
  # ideally, we want this to be a "pure" logical: TRUE or FALSE, never "NA"
  if (!inherits(prm, "harp_parameter")) prm <- parse_harp_parameter(prm, vertical_coordinate)
  par.synop <- names(param_defs)[vapply(
    param_defs,
    function(x) !is.null(x$v) && tolower(x$v$type) == "synop",
    logical(1)
  )]
  par.synop.other <- names(param_defs)[vapply(
    param_defs,
    function(x) !is.null(x$v) && tolower(x$v$type) == "synop" && !is.null(x$other_names),
    logical(1)
  )]
  par.synop.other <- unlist(lapply(param_defs[par.synop.other],function(x) x$other_names),
                            use.names = F)
  if (!is.null(par.synop.other)) {
    par.synop <- c(par.synop,par.synop.other)
  }
  sfc  <- switch(prm$level_type,
                "sea"      = ,
                "cloud"    = ,
                "surface"  = TRUE,
                "height"   = prm$level %in% c(0, 2, 10, NA),
                "isotherm" = prm$level == 0,
                "model"    = ,
                "pressure" = FALSE,
                FALSE
                )

#  result <- tolower(prm$fullname) %in% par.synop
  result <- sfc || ( tolower(prm$fullname) %in% par.synop)
  result
}

is_temp <- function(prm, vertical_coordinate = NA_character_, param_defs = getExportedValue("harpIO", "harp_params")) {
  # return TRUE if a parameter is a typical "temp" (atmospheric)
  # infact you should look at level_type *and* level (not 0, 2 or 10)
  # ideally, we want this to be a "pure" logical: TRUE or FALSE, never "NA"
#  return(!is.synop(prm))
  par.atmo <- names(param_defs)[vapply(
    param_defs,
    function(x) !is.null(x$v) && tolower(x$v$type) == "temp",
    logical(1)
  )]
  if (!inherits(prm, "harp_parameter")) prm <- parse_harp_parameter(prm, vertical_coordinate)
  atmo <- switch(prm$level_type,
                "sea"      =,
                "cloud"    =,
                "surface"  = FALSE,
                "height"   = !(prm$level %in% c(0, 2, 10, NA)),
                "isotherm" = prm$level != 0,
                "model"    =,
                "pressure" = TRUE,
                FALSE
                )
  result <- atmo && ((tolower(prm$basename) %in% par.atmo) | grepl("_ml$", prm$basename) | grepl("_pl$", prm$basename))
  result
}

