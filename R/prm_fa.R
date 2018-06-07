#' FA parameter names
#'
#' a harp-style parameter is translated to the field name in
#' ALARO/AROME/HARMONIE model output in FA format
#'
#' @param param Parameter name
#' @param type could be 'alaro' or 'arome'. These models store e.g. precipitation under a
#'        different name.
#' @return A 16 character string, or in rare cases a vector of several strings denoting the
#'   components (e.g. total precipitation may be the sum of up to 4 fields).
#' @export
prm_fa <- function(param, type="alaro"){
  # TODO: arome/alaro switch
  #       radiation fields (accumulated), CAPE, surface/soil, snow height...
  #       wind fields could have several "solutions"...
  #       accumulated cloudiness flux (ATMONEBUL...) -> AccTCC1h?

  #  cls.fields <- c("t2m", "u10m", "v10m", "s10m", "q2m", "rh2m", "td2m", "g10m")
  # strictly speaking, there *could* be fields like H00002TEMPERATURE, I guess
  prm_info <- param_parse(param)
  # generic templates (there are exceptions!)
  if (!is.null(prm_info$levelType)) {
    ftemplate <- switch(prm_info$levelType,
      "pressure" = sprintf("P%05i%%-10.10s", 100*prm_info$level),
      "model"    = sprintf("S%03i%%-12.12s", prm_info$level),
      "height"   = if (prm_info$level %in% c(2, 10)) "CLS%-13.13s" else
        sprintf("H%05i%%-10.10s", prm_info$level),
      NULL)
    #  if (param %in% cls.fields) ftemplate <- "CLS%-13.13s"

    FAbase <- switch(tolower(prm_info$basename),
      "t" = "TEMPERATURE",
      "q" = "HUMI.SPECIFIQ",
      "u" = "VENT.ZONAL",
      "v" = "VENT.MERIDIEN",
      "z" = "GEOPOTENTIEL",
      "rh" = "HUMI.RELATIVE",
      #
      "s" = "VITESSE.VENT",
      "d" = "DIR.VENT",
      "g" = "VIT.RAFALES",
      # or
      "s" = c("VENT.ZONAL   ","VENT.MERIDIEN"),
      "d" = c("VENT.ZONAL   ","VENT.MERIDIEN"),
      "g" = c("U.RAF.MOD.XFU","V.RAF.MOD.XFU"),
      "td" = c("HUMI.RELATIVE", "TEMPERATURE  "),
      # alaro
      "UNKNOWN")
    FAname <- sprintf(ftemplate, FAbase)
    # -- no level information found: precip, clouds, radiation, CAPE...
  } else {
    FAname <- switch(prm_info$basename,
      "pmsl" = "MSLPRESSURE     ",
      "tcc"  = "SURFNEBUL.TOTALE",
      "hcc"  = "SURFNEBUL.HAUTE ",
      "mcc"  = "SURFNEBUL.MOYENN",
      "lcc"  = "SURFNEBUL.BASSE ",
      "pcp"  = c("SURFPREC.EAU.GEC","SURFPREC.EAU.CON",
        "SURFPREC.NEI.GEC","SURFPREC.NEI.CON"),
      "snow" = c("SURFPREC.NEI.GEC","SURFPREC.NEI.CON"),
      # arome
      "pcp"  = c("SURFACCPLUIE","SURFACCNEIGE","SURFACCGRAUPEL"),
      "snow" = "SURFACCNEIGE",
      #
      "topo" = "SPECSURFGEOPOTEN",
      param)
  }
  FAname
}
