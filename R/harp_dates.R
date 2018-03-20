#
# Datetime functions for converting between strings and unix time
#
YMD <- function(x) {
  paste0(
    lubridate::year(x),
    formatC(lubridate::month(x), width = 2, flag = 0),
    formatC(lubridate::day(x), width = 2, flag = 0)
  )
}

YMDh <- function(x) {
  paste0(
    YMD(x),
    formatC(lubridate::hour(x), width = 2, flag = 0)
  )
}

YMDhm <- function(x) {
  paste0(
    YMDh(x),
    formatC(lubridate::minute(x), width = 2, flag = 0)
  )
}

YMD2unix <- function(x) {
  lubridate::ymd(x) %>%
    lubridate::as_datetime() %>%
    as.numeric()
}

YMDh2unix <- function(x) {
  lubridate::ymd_h(x) %>%
    lubridate::as_datetime() %>%
    as.numeric()
}

YMDhm2unix <- function(x) {
  lubridate::ymd_hm(x) %>%
    lubridate::as_datetime() %>%
    as.numeric()
}

unix2YMD <- function(x) {
  lubridate::seconds(x) %>%
    lubridate::as_datetime() %>%
    YMD()
}

unix2YMDh <- function(x) {
  lubridate::seconds(x) %>%
    lubridate::as_datetime() %>%
    YMDh()
}

unix2YMDhm <- function(x) {
  lubridate::seconds(x) %>%
    lubridate::as_datetime() %>%
    YMDhm()
}

unix2datetime <- function(x) {
  lubridate::seconds(x) %>%
    lubridate::as_datetime()
}

#' Convert unix time to a date-time string
#'
#' @param unixtime The unix time: number of seconds since 01/01/1970 00:00:00
#'   UTC
#' @param str_function The date-time string format to convert to. Available
#'   options are YMD, YMDh and YMDhm where Y is year (4 digits), M is month (2
#'   digits), D is day (2 digits), H is hour (2 digits) and m is minutes (2
#'   digits).
#'
#' @return A date-time string
#' @export
#'
#' @examples
#' unixtime_to_str_datetime(as.numeric(Sys.time()), YMD)
#' unixtime_to_str_datetime(as.numeric(Sys.time()), YMDh)
#' unixtime_to_str_datetime(as.numeric(Sys.time()), YMDhm)
#'
unixtime_to_str_datetime <- function(unixtime, str_function) {
  lubridate::seconds(unixtime) %>%
    lubridate::as_datetime() %>%
    str_function()
}

#' Convert a date-time string to unix time
#'
#' @param str_datetime A date-time string - its format is guessed from the
#'   number of characters in the string: 8 = YYYYMMDD, 10 = YYYYMMDDHH, 12 =
#'   YYYYMMDDHHmm. May be passed as a character string or numeric.
#'
#' @return The unix time in seconds since 01/01/1970 00:00:00
#' @export
#'
#' @examples
#' str_datetime_to_unixtime("20170101")
#' str_datetime_to_unixtime("2017010112")
#' str_datetime_to_unixtime("201701011230")
#'
str_datetime_to_unixtime <- function(str_datetime) {
  switch(as.character(nchar(str_datetime)),
    "8"  = {message("YYYYMMDD")
            date_function <- lubridate::ymd},
    "10"= {message("YYYYMMDDHH")
            date_function <- lubridate::ymd_h},
    "12"  = {message("YYYYMMDDHHmm")
            date_function <- lubridate::ymd_hm},
    date_function <- NA
  )
  if (!is.function(date_function)) stop("Unknown date-time string format")
  date_function(str_datetime) %>%
    lubridate::as_datetime() %>%
    as.numeric()
}

