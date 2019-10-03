#' Parameters for vobs and vfld files
#'
#' Convert parameter names from vobs or vfld files into harp formate parameters.
#' Use parse_v_parameter_synop for SYNOP parameters and parse_v_parameter_temp
#' for TEMP parameters.
#' @param v_parameter vobs or vfld parameter
#' @return A list with the harp paramater name and the units of the parameter.
#' @name v_parameters
NULL

#' @rdname v_parameters
#' @export
#'
#' @examples
#' parse_v_parameter_synop("TT")
#' parse_v_parameter_synop("RH")
#' parse_v_parameter_synop("FI")
#' parse_v_parameter_synop("PE")
#'
parse_v_parameter_synop <- function(v_parameter) {

  switch(v_parameter,
    "FI"  = {harp_param <- "model_elevation"; param_units <- "m"},
    "NN"  = {harp_param <- "CCtot";           param_units <- "oktas"}, # total cloud cover
    "DD"  = {harp_param <- "D10m";            param_units <- "degrees"},
    "FF"  = {harp_param <- "S10m";            param_units <- "m/s"},
    "TT"  = {harp_param <- "T2m";             param_units <- "K"},
    "TD"  = {harp_param <- "Td2m";            param_units <- "K"},
    "RH"  = {harp_param <- "RH2m";            param_units <- "percent"},
    "QQ"  = {harp_param <- "Q2m";             param_units <- "kg/kg"},
    "PSS" = {harp_param <- "Ps";              param_units <- "hPa"},
    "PS"  = {harp_param <- "Pmsl";            param_units <- "hPa"},
    "VI"  = {harp_param <- "vis";             param_units <- "m"},
    "LC"  = {harp_param <- "CClow";           param_units <- "oktas"}, # low cloud cover
    "N75" = {harp_param <- "N75";             param_units <- "oktas"}, # some sort of cloud cover
    "CH"  = {harp_param <- "Cbase";           param_units <- "m"}, # cloud base
    "TX"  = {harp_param <- "Tmax";            param_units <- "K"},
    "TN"  = {harp_param <- "Tmin";            param_units <- "K"},
    "GW"  = {harp_param <- "G10m";            param_units <- "m/s"}, # instantaneous (10minutes) wind gust speed
    "GX"  = {harp_param <- "Gmax";            param_units <- "m/s"}, # max gust past 6 hours
    "WX"  = {harp_param <- "Smax";            param_units <- "m/s"}, # max mean wind speed (6h ???)
    "PE"  = {harp_param <- "Pcp";             param_units <- "kg/m^2"},
    "PE1" = {harp_param <- "AccPcp1h";        param_units <- "kg/m^2"},
    "PE3" = {harp_param <- "AccPcp3h";        param_units <- "kg/m^2"},
    "PE6" = {harp_param <- "AccPcp6h";        param_units <- "kg/m^2"},
    "PE12"= {harp_param <- "AccPcp12h";       param_units <- "kg/m^2"},
    "PE24"= {harp_param <- "AccPcp24h";       param_units <- "kg/m^2"},
    {harp_param <- v_parameter; param_units <- "unknown"}
  )
  list(harp_param = harp_param, param_units = param_units)
}
