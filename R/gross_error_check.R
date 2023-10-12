# Internal functions for default values for gross error checks on observations

get_min_obs_allowed <- function(parameter, param_units = "") {
  min_val <- switch(tolower(parameter),
    "t2m"       = 223,
    "td2m"      = 223,
    "tmin"      = 223,
    "tmax"      = 223,
    "rh2m"      = 0,
    "pmsl"      = 90000,
    "ps"        = 70000,
    "u10m"      = 0,
    "v10m"      = 0,
    "s10m"      = 0,
    "smax"      = 0,
    "g10m"      = 0,
    "gmax"      = 0,
    "d10m"      = 0,
    "accpcp1h"  = 0,
    "accpcp3h"  = 0,
    "accpcp6h"  = 0,
    "accpcp12h" = 0,
    "accpcp24h" = 0,
    "cctot"     = 0,
    "cclow"     = 0,
    "ccmed"     = 0,
    "cchigh"    = 0,
    "cbase"     = 0,
    "rH"        = 0,
    "t"         = 173,
    -Inf
  )
  if (tolower(parameter) %in% c("t2m", "td2m", "t", "tmax", "tmin") && param_units == "degC") {
    min_val <- min_val - 273.15
  }
  if (tolower(parameter) %in% c("pmsl", "ps") && param_units == "hPa") {
    min_val <- min_val / 100
  }
  min_val
}
#
get_max_obs_allowed <- function(parameter, param_units = "") {
  max_val <- switch(tolower(parameter),
    "t2m"       = 333,
    "td2m"      = 333,
    "tmin"      = 333,
    "tmax"      = 333,
    "rh2m"      = 100,
    "pmsl"      = 110000,
    "u10m"      = 100,
    "v10m"      = 100,
    "s10m"      = 100,
    "smax"      = 100,
    "g10m"      = 150,
    "gmax"      = 100,
    "d10m"      = 360,
    "accpcp1h"  = 500,
    "accpcp3h"  = 600,
    "accpcp6h"  = 750,
    "accpcp12h" = 1000,
    "accpcp24h" = 1000,
    "cctot"     = 8,
    "cclow"     = 8,
    "ccmed"     = 8,
    "cchigh"    = 8,
    "cbase"     = 10000,
    "rh"        = 100,
    "t"         = 333,
    Inf
  )
  if (tolower(parameter) %in% c("t2m", "td2m", "t", "tmax", "tmin") && param_units == "degC") {
    max_val <- max_val - 273.15
  }
  if (tolower(parameter) %in% c("pmsl", "ps") && param_units == "hPa") {
    max_val <- max_val / 100
  }
  if (tolower(parameter) %in% c("rh2m", "rh") && param_units == "fraction") {
    max_val <- max_val / 100
  }
  if (grepl("^accpcp[[:digit:]]+h$", tolower(parameter)) && param_units == "m") {
    max_val <- max_val / 1000
  }
  if (grepl("^cc[[:alpha:]]+$", tolower(parameter))) {
    if (param_units == "percent") {
      max_val <- 100
    }
    if (param_units == "fraction") {
      max_val <- 1
    }
  }
  if (tolower(parameter) %in% c("s10m", "g10m", "gmax", "smax", "u10m", "v10m") && param_units == "km/h") {
    max_val <- max_val * 3.6
  }
  max_val
}
