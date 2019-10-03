#' @rdname v_parameters
#' @export
#' @examples
#' parse_v_parameter_temp("TT")
#' parse_v_parameter_temp("RH")
#' parse_v_parameter_temp("FI")
#' parse_v_parameter_temp("PP")
#'
parse_v_parameter_temp <- function(v_parameter) {

  switch (v_parameter,
    "PP" = {harp_param <- "p";  param_units <- "hPa"},
    "FI" = {harp_param <- "Z";  param_units <- "m"},
    "TT" = {harp_param <- "T";  param_units <- "K"},
    "RH" = {harp_param <- "RH"; param_units <- "percent"},
    "DD" = {harp_param <- "D";  param_units <- "degrees"},
    "FF" = {harp_param <- "S";  param_units <- "m/s"},
    "QQ" = {harp_param <- "Q";  param_units <- "kg/kg"},
    "TD" = {harp_param <- "Td"; param_units <- "K"},
    {harp_param <- v_parameter; param_units <- "unknown"}
  )
  list(harp_param = harp_param, param_units = param_units)
}
