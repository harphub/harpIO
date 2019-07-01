# Internal function to guess the units of forecast or observations
# read by read_point_forecast or read_point_obs

guess_units <- function(df, param) {

  # The last column should always be data. The mean should be
  # the best indicator of what the units should be. However,
  # for some parameters (e.g. precipitation in m or mm), a second
  # test of maximum may also be helpful...

  test_value  <- mean(as.data.frame(df)[, ncol(df)], na.rm = TRUE)
  second_test <- max(as.data.frame(df)[, ncol(df)], na.rm = TRUE)

  if (inherits(param, "harp_parameter")) {
    test_param <- tolower(param$basename)
    text_param <- param$fullname
  } else {
    test_param <- tolower(param)
    text_param <- param
  }

  param_units <- switch(
    test_param,
    "ps"        = ,
    "pmsl"      = ifelse(test_value < 2000, "hPa", "Pa"),
    "accpcp1h"  = ,
    "accpcp3h"  = ,
    "accpcp6h"  = ,
    "accpcp12h" = ,
    "accpcp24h" = ,
    "pcp"       = ifelse(test_value < 0.2 & second_test < 0.2, "m", "kg/m^2"),
    "u10m"      = ,
    "v10m"      = ,
    "s"         = ,
    "u"         = ,
    "v"         = ,
    "g"         = ,
    "s10m"      = ,
    "gmax"      = ,
    "smax"      = ifelse(test_value < 10, "m/s", "km/h"),
    "t2m"       = ,
    "td2m"      = ,
    "t"         = ,
    "td"        = ,
    "tmax"      = ,
    "tmin"      = ifelse(test_value < 100, "degC", "K"),
    "d10m"      = ,
    "d"         = ifelse(second_test < (2 * pi), "radians", "degrees"),
    "rh2m"      = ,
    "rh"        = ifelse(test_value < 1, "fraction", "percent"),
    "q2m"       = ,
    "q"         = ifelse(test_value > 0.1, "g/kg", "kg/kg"),
    "cclow"     = ,
    "ccmed"     = ,
    "cchigh"    = ,
    "cctot"     = {if (test_value <= 1 && second_test <= 1) {
                    "fraction"
                  } else if (test_value <= 8 && second_test > 1) {
                    "oktas"
                  } else {
                    "percent"
                  }},
    "vis"       = ifelse(test_value < 50, "km", "m"),
    "unknown"
  )

  warning(
    "Units for ", text_param, " GUESSED to be ", param_units, ". You can change units by calling set_units.",
    call.      = FALSE,
    immediate. = TRUE
  )

  param_units
}
