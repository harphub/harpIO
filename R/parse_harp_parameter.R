#' Parse the parameter name and decide on level, accumulation...
#'
#' Parses a harp-style parameter name and returns accumulation time, level (if
#' any) and the base name of the parameter
#'
#' @param param Parameter name
#' @return A list (of class harp_parameter) 
#'   with \code{fullname}, \code{basename}, \code{level_type},
#'   \code{level}, \code{accum}
#' @export
#'
#' @examples
#' parse_harp_param("AccPcp1h")
#' parse_harp_param("z500")
#' parse_harp_param("s100m")
parse_harp_parameter <- function(param) {
  ## TODO: radiation, surface properties...
  if (inherits(param, "harp_parameter")) return(param)
  # all comparisons are done in lower case
  if (tolower(param) %in% c("caf", "t", "z", "u", "v", "w", "q", "rh") ) {
    stop("Level must be supplied for ", param)
  }

  fullname <- param

  basename <- tolower(fullname)
  plen <- nchar(basename)

  ## 1. accumulated field? must be AccXXnnh (last character is h, m, s)
  ## we assume the only digits in the parameter name are for accumulation
  if (grepl("^acc[[:alpha:]]+[[:digit:]]+[hms]", basename)){
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
  if (grepl("^[[:alpha:]]+[[:digit:]]+[mpsh]?$", basename)){
    ## trailing digits are interpreted as pressure levels
    ## possibly there's an extra "m": then they are height levels
    ## or an "s", "h": hyprid level (model level)
    ## or "p": pressure level (usually not written)
    lev1 <- regexpr("[[:digit:]]", basename)
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
        substring(lev, lt))
      level <- as.numeric(substr(lev, 1, lt-1))
    } else {
      ## default level type is "pressure"
      level_type <- "pressure"
      level <- as.numeric(lev)
    }
  } else {
    ## no level defined. could be surface, but also MSL, CLOUDS, ...
    level <- NA
    level_type <- "unknown"
  }

  level_type <- switch(basename,
         "pmsl" = ,
         "mslp" = "msl",
         "lsm"  = ,
         "pcp"  = ,
         "snow" = ,
         "tg"   = "surface",
         "sst"  = "sea",
         level_type)

#TODO
# special levels like msl, surface, cloud, soil...
# allow level indicators longer than 1 char!
# but often, this is so format-dependent, you can just leave it NULL

  result <- list(fullname = fullname, basename = basename,
       level = level, level_type = level_type,
       accum = accum, acc_unit = acc_unit)
  class(result) <- "harp_parameter"
  result
}


is.synop <- function(prm) {
  # return TRUE if a parameter is a typical "synop"
  # ideally, we want this to be a "pure" logical: TRUE or FALSE, never "NA"
  if (!inherits(prm, "harp_parameter")) prm <- parse_harp_parameter(prm)
  par.synop <- c("pmsl", "pcp",
                 "u10m", "v10m", "s10m", "d10m", "g10m",
                 "t2m", "q2m", "rh2m", "td2m",
                 "tcc", "hcc", "mcc", "lcc", "cbase",
#                 "cctot", "cchigh", "ccmed", "cclow", "cbase",
                 "v75", "vis",
                 "tmax", "tmin", "gmax")
  sfc  <- switch(prm$level_type,
                "sea"  =,
                "cloud" =,
                "surface" = TRUE,
                "height" = level %in% c(0, 2, 10, NA),
                "hybrid" =,
                "pressure" = FALSE,
                FALSE
                )

#  result <- tolower(prm$fullname) %in% par.synop
  result <- sfc || ( tolower(prm$fullname) %in% par.synop)
  result
}
is.temp <- function(prm) {
  # return TRUE if a parameter is a typical "temp" (atmospheric)
  # infact you should look at level_type *and* level (not 0, 2 or 10)
  # ideally, we want this to be a "pure" logical: TRUE or FALSE, never "NA"
#  return(!is.synop(prm))
  par.atmo <- c("t", "z", "u", "v", "s", "d", "g", "q", "rh", "td")
  if (!inherits(prm, "harp_parameter")) prm <- parse_harp_parameter(prm)
  atmo <- switch(prm$level_type,
                "sea"  =,
                "cloud" =,
                "surface" = FALSE,
                "height" = !(level %in% c(0, 2, 10, NA)),
                "hybrid" =,
                "pressure" = TRUE,
                FALSE
                )
  result <- atmo && (par$basename %in% par.atmo)
  result
}

