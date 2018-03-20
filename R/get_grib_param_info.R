#' Get grib paramater information
#'
#' Gets the shortname and parameter number for a HarmonEPS grib file. For
#' surface and screen level parameters the level type and level number are
#' assumed. For upper air parameters the level and, optionally, the level type
#' must be supplied. The default is for pressure levels.
#'
#' @param param Parameter name
#' @param level The level of the field required - may be pressure level, hybrid
#'   level or height above ground.
#' @param levtype The level type given as a grib \code{indicatorOfTypeOfLevel}
#'   integer: 100 = pressure levels, 105 = height above ground, 109 = hybrid
#'   levels.
#' @return A lisy with \code{short_name}, \code{param_number},
#'   \code{level_type}, \code{level_numbe.r}
#' @export
#'
#' @examples
#' get_grib_param_info("pcp")
#' get_grib_param_info("t2m")
#' get_grib_param_info("t", level = 500)
#' get_grib_param_info("rh", level = 32, levtype = 109)
get_grib_param_info <- function(param, level = NULL, levtype = 100) {
  if (tolower(param) %in% c("caf", "t", "z", "u", "v", "w", "q", "rh") && is.null(level)) {
    stop("Level must be supplied for ", param)
  }
	switch(tolower(param),
				 "pcp"      = {short_name   <-  "tp"
				               param_number <-  61
				               level_type   <-  105
				               level_number <-  0},
				 "fog"      = {short_name   <-  "tcc"
				               param_number <-  71
				               level_type   <-  109
				               level_number <-  65},
				 "caf"      = {short_name   <-  "tcc"
				               param_number <-  71
				               level_type   <-  levtype
				               level_number <-  level},
				 "cctot"    = {short_name   <-  "tcc"
				               param_number <-  71
				               level_type   <-  105
				               level_number <-  0},
				 "cchigh"   = {short_name   <-  "hcc"
				               param_number <-  71
				               level_type   <-  105
				               level_number <-  0},
				 "ccmed"    = {short_name   <-  "mcc"
				               param_number <-  71
				               level_type   <-  105
				               level_number <-  0},
				 "cclow"    = {short_name   <-  "lcc"
				               param_number <-  71
				               level_type   <-  105
				               level_number <-  0},
				 "sst"      = {short_name   <-  "t"
				               param_number <-  11
				               level_type   <-  102
				               level_number <-  0},
				 "tg1"      = {short_name   <-  "t"
				               param_number <-  11
				               level_type   <-  105
				               level_number <-  800},
				 "tg2"      = {short_name   <-  "t"
				               param_number <-  11
				               level_type   <-  105
				               level_number <-  801},
				 "t2m"      = {short_name   <-  "2t"
				               param_number <-  11
				               level_type   <-  105
				               level_number <-  2},
				 "t0m"      = {short_name   <-  "t"
				               param_number <-  11
				               level_type   <-  105
				               level_number <-  0},
				 "t"        = {short_name   <-  "t"
				               param_number <-  11
				               level_type   <-  levtype
				               level_number <-  level},
				 "pmsl"     = {short_name   <-  "msl"
				               param_number <-  1
				               level_type   <-  103
				               level_number <-  0},
				 "z0m"      = {short_name   <-  "z"
				               param_number <-  6
				               level_type   <-  105
				               level_number <-  0},
				 "altitude" = {short_name   <-  "z"
				               param_number <-  6
				               level_type   <-  105
				               level_number <-  0},
				 "terrain"  = {short_name   <-  "z"
				               param_number <-  6
				               level_type   <-  105
				               level_number <-  0},
				 "topo"     = {short_name   <-  "z"
				               param_number <-  6
				               level_type   <-  105
				               level_number <-  0},
				 "z"        = {short_name   <-  "z"
				               param_number <-  6
				               level_type   <-  levtype
				               level_number <-  level},
				 "s10m"     = {short_name   <-  "ws"
				               param_number <-  32
				               level_type   <-  105
				               level_number <-  10},
				 "d10m"     = {short_name   <-  "wdir"
				               param_number <-  31
				               level_type   <-  105
				               level_number <-  10},
				 "u10m"     = {short_name   <-  "10u"
				               param_number <-  33
				               level_type   <-  105
				               level_number <-  10},
				 "v10m"     = {short_name   <-  "10v"
				               param_number <-  34
				               level_type   <-  105
				               level_number <-  10},
				 "u100m"   = {short_name   <-  "100u"
				               param_number <-  246
				               level_type   <-  1
				               level_number <-  0},
				 "v100m"   = {short_name   <-  "100v"
				               param_number <-  247
				               level_type   <-  1
				               level_number <-  0},
				 "u"        = {short_name   <-  "u"
				               param_number <-  33
				               level_type   <-  levtype
				               level_number <-  level},
				 "v"        = {short_name   <-  "v"
				               param_number <-  34
				               level_type   <-  levtype
				               level_number <-  level},
				 "w"        = {short_name   <-  "tw"
				               param_number <-  40
				               level_type   <-  levtype
				               level_number <-  level},
				 "q2m"      = {short_name   <-  "q"
				               param_number <-  51
				               level_type   <-  105
				               level_number <-  2},
				 "q"        = {short_name   <-  "q"
				               param_number <-  51
				               level_type   <-  levtype
				               level_number <-  level},
				 "rh2m"     = {short_name   <-  "r"
				               param_number <-  52
				               level_type   <-  105
				               level_number <-  2},
				 "rh"       = {short_name   <-  "r"
				               param_number <-  52
				               level_type   <-  levtype
				               level_number <-  level},
				 "ugust10m" = {short_name   <-  "ugst"
				               param_number <-  162
				               level_type   <-  105
				               level_number <-  10},
				 "vgust10m" = {short_name   <-  "vgst"
				               param_number <-  163
				               level_type   <-  105
				               level_number <-  10},
				 {short_name   <- NA
				  param_number <- NA
				  level_type   <- NA
				  level_number <- NA}
	)
#
	list(
		short_name   = short_name,
		param_number = param_number,
		level_type   = level_type,
		level_number = level_number
	)
}
