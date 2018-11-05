#' parse the parameter name and decide on level, accumulation...
#'
#' Parses a harp-style parameter name and returns accumulation time, level (if
#' any) and the base name of the parameter
#'
#' @param param Parameter name
#' @return A list with \code{fullname}, \code{basename}, \code{level_type},
#'   \code{level}, \code{accum}
#' @export
#'
#' @examples
#' parse_harp_param("AccPcp1h")
#' parse_harp_param("z500")
#' parse_harp_param("s100m")
parse_harp_parameter <- function(param) {
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
      "h"=accum*3600)
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
      levelType <- switch(substring(lev, lt),
        "m" = "height",
        "h" =,
        "s" = "model",
        "p" = "pressure",
        substring(lev, lt))
      level <- as.numeric(substr(lev, 1, lt-1))
    } else {
      ## default level type is "pressure"
      levelType <- "pressure"
      level <- as.numeric(lev)
    }
  } else {
    ## no level defined. could be surface, but also MSL, CLOUDS, ...
    level <- NULL
    levelType <- NULL
  }

  levelType <- switch(basename,
         "pmsl" = , 
         "mslp" = "msl",
         "tg"   = "surface",
         "sst"  = "sea",
         levelType)
 
#TODO
# special levels like msl, surface, cloud, soil...
# allow level indicators longer than 1 char!
# but often, this is so format-dependent, you can just leave it NULL

  list(fullname = fullname, basename = basename,
       level = level, levelType = levelType,
       accum = accum)
}
