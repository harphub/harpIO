parse_v_parameter_synop <- function(v_parameter_synop) {

  switch(v_parameter_synop,
    "FI"="melev",  ### ATTENTION: this is MODEL elevation, not actual station
    "NN"="CCtot", # total cloud cover
    "DD"="D10m",
    "FF"="S10m",
    "TT"="T2m",
    "TD"="Td2m",
    "RH"="RH2m",
    "QQ"="Q2m",
    "PSS"="Ps",
    "PS"="Pmsl",
    "VI"="vis",
    "LC"="CClow", # low cloud cover
    "CH"="Cbase", # cloud base
    "TX"="Tmax",
    "TN"="Tmin",
    "GW"="G10m", # instantaneous (10minutes) wind gust speed
    "GX"="Gmax", # max gust past 6 hours
    "WX"="Smax", # max mean wind speed (6h ???)
    "PE"="Pcp",
    "PE1"="AccPcp1h",
    "PE3"="AccPcp3h",
    "PE6"="AccPcp6h",
    "PE12"="AccPcp12h",
    "PE24"="AccPcp24h",
    v_parameter_synop
  )

}
