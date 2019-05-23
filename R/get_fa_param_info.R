#' FA parameter names
#'
#' a harp-style parameter is translated to the field name in
#' ALARO/AROME/HARMONIE model output in FA format
#'
#' @param param Parameter name
#' @param fa_type could be 'alaro' or 'arome'. These models store e.g. precipitation under a
#'        different name.
#' @param fa_vector A logical value. TRUE means that e.g. wind speed will be calculated from wind vector
#'         components, not searched as a separate field.
#' @return A 16 character string, or in rare cases a vector of several strings denoting the
#'   components (e.g. total precipitation may be the sum of up to 4 fields).
#' @export
get_fa_param_info <- function(param, fa_type="arome", fa_vector=TRUE){
  ### FA names are very inconsistent ("_" vs "." separators...)
  ### so we have to do some hard-coding
  hardcoded_fields <- c("t2m", "u10m", "v10m", "s10m", "d10m", "rh2m", "g10m", "pmsl", "td2m", "topo", "lsm")
  # strictly speaking, there *could* be fields like H00002TEMPERATURE, I guess
  if (!inherits(param, "harp_parameter")) param <- parse_harp_parameter(param)
  # generic templates (there are exceptions!)
  if (tolower(param$fullname) %in% hardcoded_fields) {
    FAname <- switch(tolower(param$fullname),
                     "t2m"  = "CLSTEMPERATURE  ",
                     "u10m" = "CLSVENT.ZONAL   ",
                     "v10m" = "CLSVENT.MERIDIEN",
                     "s10m" = c("CLSVENT.ZONAL   ","CLSVENT.MERIDIEN"),
                     "d10m" = c("CLSVENT.ZONAL   ","CLSVENT.MERIDIEN"),
                     "rh2m" = "CLSHUMI.RELATIVE",
                     "pmsl" = "MSLPRESSURE     ",
                     "g10m" = c("CLSU.RAF.MOD.XFU", "CLSV.RAF.MOD.XFU"),
                     "td2m" = c("CLSHUMI.RELATIVE", "CLSTEMPERATURE  "),
                     "topo" = "SURFGEOPOTENTIEL",
                     "lsm"  = "SURFIND.TERREMER",
                     stop("unknown parameter ", param$fullname))
  } else if (param$level_type %in% c("hybrid", "pressure", "height") ) {
    if (param$level_type != "pressure") plev <- param$level
    else if (param$level < 1000) plev <- param$level * 100
    else plev <- param$level * 100 - 100000
    ## In FA files, pressure level 1000hPa is written as "P00000TEMPERATUR"
    ftemplate <- switch(param$level_type,
      "pressure" = sprintf("P%05i%%-10.10s", plev),
      "model"    = sprintf("S%03i%%-12.12s", plev),
      "height"   = if (plev %in% c(2, 10)) "CLS%-13.13s" else
        sprintf("H%05i%%-10.10s", plev),
      NULL)
    #  if (tolower(param$fullname) %in% cls.fields) ftemplate <- "CLS%-13.13s"

    FAbase <- switch(tolower(param$basename),
      "t" = "TEMPERATURE",
      "q" = "HUMI.SPECIFIQ",
      "u" = "VENT_ZONAL",
      "v" = "VENT_MERIDIEN",
      "z" = "GEOPOTENTIEL",
      "rh" = "HUMI_RELATIVE",
      #
      "s" = if (fa_vector) c("VENT_ZONAL   ","VENT_MERIDIEN") else "VITESSE.VENT",
      "d" = if (fa_vector) c("VENT_ZONAL   ","VENT_MERIDIEN") else "DIR.VENT",
      "g" = if (fa_vector) c("U.RAF.MOD.XFU","V.RAF.MOD.XFU") else "VIT.RAFALES",
      #
      "td" = c("HUMI_RELATIVE", "TEMPERATURE  "),
      "UNKNOWN")
    FAname <- sprintf(ftemplate, FAbase)
  } else {
    # -- no level information found: precip, clouds, radiation, CAPE...
    FAname <- switch(param$basename,
      "mslp" = ,
      "pmsl" = "MSLPRESSURE     ",
      "cctot"=,
      "tcc"  = if (is.null(param$accum)) "SURFNEBUL.TOTALE" else "ATMONEBUL.TOTALE",
      "cchigh"=,
      "hcc"  = if (is.null(param$accum)) "SURFNEBUL.HAUTE " else "ATMONEBUL.HAUTE ",
      "ccmed"=,
      "mcc"  = if (is.null(param$accum)) "SURFNEBUL.MOYENN" else "ATMONEBUL.MOYENN",
      "cclow"=,
      "lcc"  = if (is.null(param$accum)) "SURFNEBUL.BASSE " else "ATMONEBUL.BASSE ",
      "pcp"  = if (fa_type=="alaro") c("SURFPREC.EAU.GEC", "SURFPREC.EAU.CON",
                                       "SURFPREC.NEI.GEC", "SURFPREC.NEI.CON")
               else c("SURFACCPLUIE", "SURFACCNEIGE", "SURFACCGRAUPEL") ,
      "snow" = if (fa_type=="alaro") c("SURFPREC.NEI.GEC", "SURFPREC.NEI.CON")
               else "SURFACCNEIGE",
      "rain" = if (fa_type=="alaro") c("SURFPREC.EAU.GEC", "SURFPREC.EAU.CON")
               else "SURFACCPLUIE",
      #
      "topo" = "SPECSURFGEOPOTEN",
      # accumulated radiation fields:
      "lwrad" = "SURFRAYT THER DE",  # thermal
      "swrad" = "SURFRAYT SOLA DE") # direct + diffuse solar
      param$fullname)
  }
  FAname <- format(FAname, width=16)
  ## TODO: conversions : also some single fields need a conversion
  ##   RH : 1-100 or 0-1 ?
  ##   CC : 0-1 or 0-8 ?
  ##   P : Pa or hPa ?
  apply_function <- NULL
  if (length(FAname)>1) {
    # add the function as an attribute
    # it should be a function expecting a 3d geofield as input, not point wise!
    # because "apply'ing" such a pointwise function would be SLOW.
    # default is to take the sum of the fields
    # fastest way (without C code) is rowSums()
    # ATTENTION: when applying a function, you may loose all geofield information
    # so we use a new wrapper "apply_geo3d"
    apply_function <- switch(param$basename,
              "s" = function(x) meteogrid::apply_geo3d(x, "norm", newname="Wind speed"),
              "g" = function(x) meteogrid::apply_geo3d(x, "norm", newname="Wind gust speed"),
              "d" = function(x) meteogrid::apply_geo3d(x, "wdir", newname="Wind direction"),
              "td" = function(x) meteogrid::as.geofield(rh2tdew(tc=x[,,2], rh=x[,,1]), domain=x),
              "snow" = function(x) meteogrid::apply_geo3d(x, "sum", newname="Snow"),
              "rain" = function(x) meteogrid::apply_geo3d(x, "sum", newname="Rain"),
              "pcp" = function(x) meteogrid::apply_geo3d(x, "sum", newname="Precipitation"),
              function(x) meteogrid::apply_geo3d(x, "sum", newname=param$basename))
  }
  ### NEW: add units (of the FA field!) as an attribute
  units <- switch(tolower(param$basename),
                 "t" = "K",
                 "u" =,
                 "v" =,
                 "s" = "m/s",
                 "cctot"=, "cchigh"=, "ccmed"=, "cclow"=,
                 "tcc"=, "hcc"=, "mcc" =, "lcc"=,
                 "rh" = "fraction",
                 "rain"=, "snow" =,
                 "pcp"="kg/m^2",
                 "p" = "Pa",
                 "unknown")
  list(fa_name=FAname, apply_function=apply_function, units=units)
}
