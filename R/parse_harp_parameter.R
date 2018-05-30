#' Parse a harp paramter.
#'
#' \code{parse_harp_parameter} returns vertical level information, accumulation time and parameter names
#' for grib and FA reading.
#'
#' @param param The parameter name.
#'
#' @return A list with the following elements: \cr
#' - fullname: the original input name (==param) \cr
#' - basename: the name without any accumulation info \cr
#' - shortName: as found by grib_api (and/or Rgrib2) \cr
#' - levelType: either numeric (100,105 etc) or S,P,H or NULL \cr
#' - level: NULL if not needed (e.g. if shortName is enough) or numerical \cr
#' - accum: the accumulation period (0 or higher) \cr
#' - FAname : the corresponding name in FA format files \cr
#' @export
#'
#' @examples
#' parse_harp_parameter("T2m")
#' parse_harp_parameter("AccPcp12h")
#' parse_harp_parameter("Z500")
#' parse_harp_parameter("U100m")
#' parse_harp_parameter("Q61s")
#'
parse_harp_parameter <- function(param){
  # this is the default format, but all comparisons are done in lower case
  par.synop <- c("T2m","Pmsl","Pcp","U10m","V10m","S10m","Q2m","RH2m","Td2m","CCtot","D10m","N75","vis","Cbase","CClow","Tmax","Tmin","Gmax")
  par.atmo <- c("T","Z","U","V","S","D","G","Q","RH","Td")

  plen <- nchar(param)

  fullname <- param
  level <- NULL
  levelType <- NULL

  ## 1. accumulated field? must be AccXXnnh (last character is h, m, s)
  if (grepl("^acc[[:alpha:]]+[[:digit:]]+[hms]", tolower(param))){
    ttt <- regexpr("[[:digit:]]", param)
    accum <- as.numeric(substr(param, ttt, plen-1))
    ac.unit <- substring(param, plen)
    attr(accum,"unit") <- ac.unit
    nocumname <- substr(param, 4, ttt-1)
    plen <- nchar(nocumname)
    cat("Accumulated field:", accum, ac.unit,"\n")
  } else {
    accum <- 0
    ac.unit <- NA
    nocumname <- param
  }

  ## 2. 'basic' field or atmospheric with height added?
  if (tolower(nocumname) %in% tolower(par.synop)) {
    ## no need to change levelType: if level==NULL, it will not be taken into account
    ## don't set level=2 etc, because not all GRIB tables use the same conventions for this.
    ## That's exactly why we need this special category of shortcuts.
    basename <- nocumname
    shortName <- switch(tolower(basename),
      "t2m"="2t",
      "q2m"="2s",
      "u10m"="10u",
      "v10m"="10v",
      "pcp"="tp",
      "pmsl"="msl",
      "rh2m"="2r",
      "td2m"="2d",
      "s10m"="10si",
      "cctot"="tcc",
      "d10m"="10wdir",
      "vis"="vis",
      "cbase"="cbh",
      "cclow"="lcc",
      "tmax"="mxt2",
      "tmin"="mnt2",
      "gmax"="10fg"
    )
    FAname <- switch(tolower(basename),
      "t2m"  ="CLSTEMPERATURE  ",
      "q2m"  ="CLSHUMI.SPECIFIQ",
      "u10m" ="CLSVENT.ZONAL   ",
      "v10m" ="CLSVENT.MERIDIEN",
      "pcp"  =c("SURFPREC.EAU.GEC","SURFPREC.EAU.CON",
        "SURFPREC.NEI.GEC","SURFPREC.NEI.CON"),
      "pmsl" ="MSLPRESSURE     ",
      "rh2m"="CLSHUMI.RELATIVE",
      #                "Td2m"=???,
      "s10m"=c("CLSVENT.ZONAL   ","CLSVENT.MERIDIEN"),
      "unknown")

  } else {
    if (grepl("^[[:alpha:]]+[[:digit:]]+[mps]?$", tolower(nocumname))){
      ## trailing digits are interpreted as pressure levels
      ## possibly there's an extra "m": then they are height levels
      ## or an "s": hyprid level (model level)
      ## or "p": pressure level (usually not written)
      ll <- regexpr("[[:digit:]]", nocumname)
      lev <- substr(nocumname, ll, plen)
      basename <- substr(nocumname,1,ll-1)

      lt <- regexpr("[[:alpha:]]", lev)
      if (lt>0){
        ## parse the "level" part
        levelType <- switch(tolower(substring(lev, lt)),
          "m"="H",
          "s"="S",
          "p"="P",
          {cat("unknown level type\n");NULL})
        level <- as.numeric(substr(lev, 1, lt-1))
      } else {
        levelType <- "P"
        level <- as.numeric(lev)
      }
    } else basename <- nocumname

    if (tolower(basename) %in% tolower(par.atmo)) {
      ## if param is not of type Xnnn[m], we consider it as purely a "shortName" entry
      ## the only level types are ..m (height) or pressure level (hPa)
      ## NOTICE: a few cases may slpi through here: any capital letter followed by [Rd]
      ## but that is not too bad & it is solved further down
      shortName <- switch(tolower(basename),
        "t"="t",
        "q"="q",
        "u"="u",
        "v"="v",
        "s"="ws",
        "d"="wdir",
        "g"="gust",
        "z"="z",
        "rh"="r",
        "td"="dpt")
      FAname1 <- switch(tolower(basename),
        "t"="TEMPERATURE",
        "q"="HUMI.SPECIFIQ",
        "u"="VENT.ZONAL",
        "v"="VENT.MERIDIEN",
        "s"="VITESSE.VENT",
        "d"="DIR.VENT",
        "g"="VIT.RAFALES",
        "z"="GEOPOTENTIEL",
        "rh"="HUMI.RELATIVE",
        "UNKNOWN")
      FAprefix <- switch(levelType,
        "H" = sprintf("H%05i",level),
        "S" = sprintf("S%3i", level),
        "P" = sprintf("P%05i", level*100))
      FAname <- format(paste0(FAprefix, FAname1), width=16)
    } else {
      cat("WARNING: parameter",param,"not recognised. Interpreting as shortName.\n")
      shortName <- basename
      FAname <- "UNKNOWN"
    }
  }
  list(fullname=fullname, basename=basename, shortName=shortName,
    level=level, levelType=levelType, accum=accum, ac.unit=ac.unit,
    FAname=substr(FAname, 1, 16))
}

