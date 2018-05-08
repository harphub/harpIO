#' Parse a temp parameter from a vobs or vfld convention to harp convention
#'
#' @param v_parameter_synop vobs or vfld parameter
#'
#' @return harp paramater name
#' @export
#'
#' @examples
#' parse_v_parameter_synop("TT")
#' parse_v_parameter_synop("RH")
#' parse_v_parameter_synop("FI")
#' parse_v_parameter_synop("PP")
#'
parse_v_parameter_temp <- function(v_parameter_temp) {

  switch (v_parameter_temp,
    "PP"="p",
    "FI"="Z",
    "TT"="T",
    "RH"="RH",
    "DD"="D",
    "FF"="S",
    "QQ"="Q",
    "TD"="Td",
    v_parameter_temp
  )

}
