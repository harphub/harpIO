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
