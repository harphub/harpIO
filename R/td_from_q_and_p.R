#' Compute dewpoint temperature from specific humidity and pressure
#'
#' @param q Specific humidity - can be in g / kg or kg / kg
#' @param p pressure - can be Pa or hPa (mb)
#' @param kelvin Logical - if TRUE (the defualt) return temperature in Kelvin
#'   otherwise in degrees C.
#'
#' @return Dew point temperature
#' @export
#'
#' @examples
#' td_from_q_and_p(0.008, 1000)
#' td_from_q_and_p(8, 10000, kelvin = FALSE)
td_from_q_and_p <- function(q, p, kelvin = TRUE) {
  if (max(q) > 0.5) q <- q / 1000
  if (max(p) > 10000) p <- p / 100
  e  <- q * p / (0.622 + 0.378 * q)
  td <- log(e / 6.112) * 243.5 / (17.67 - log(e / 6.112)) + 273.15
  if (kelvin) {
    td       <- td + 273.15
    td_units <- "K"
  } else {
    td_units <- "degC"
  }

  if (meteogrid::is.geofield(td)) {
    attr(td, "info")[["name"]] <- paste("Td Dewpoint temperature", td_units)
    attr(td, "info")[["unit"]] <- td_units
  }

  td

}
