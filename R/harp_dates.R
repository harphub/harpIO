#
# Datetime functions for converting between strings and unix time
#
#' Convert date type to YYYYMMDD string
#'
#' @param x A date object.
#'
#' @return A YYYYMMDD string
#' @export
#'
#' @examples
#' YMD(Sys.time())
YMD <- function(x) {
  paste0(
    lubridate::year(x),
    formatC(lubridate::month(x), width = 2, flag = 0),
    formatC(lubridate::day(x), width = 2, flag = 0)
  )
}

#' Convert date type to YYYYMMDDHH string
#'
#' @param x A date object.
#'
#' @return A YYYYMMDDHH string
#' @export
#'
#' @examples
#' YMDh(Sys.time())
YMDh <- function(x) {
  paste0(
    YMD(x),
    formatC(lubridate::hour(x), width = 2, flag = 0)
  )
}

#' Convert date type to YYYYMMDDHHmm string
#'
#' @param x A date object.
#'
#' @return A YYYYMMDDHHmm string
#' @export
#'
#' @examples
#' YMDhm(Sys.time())
YMDhm <- function(x) {
  paste0(
    YMDh(x),
    formatC(lubridate::minute(x), width = 2, flag = 0)
  )
}

#' Convert YYYYMMDD string to unix time
#'
#' @param x A YYYYMMDD string
#'
#' @return unix time in seconds since 01/01/1970 00:00:00
#' @export
#'
#' @examples
#' YMD2unix(20170101)
YMD2unix <- function(x) {
  lubridate::ymd(x) %>%
    lubridate::as_datetime() %>%
    as.numeric()
}

#' Convert YYYYMMDDHH string to unix time
#'
#' @param x A YYYYMMDDHH string
#'
#' @return unix time in seconds since 01/01/1970 00:00:00
#' @export
#'
#' @examples
#' YMDh2unix(2017010112)
YMDh2unix <- function(x) {
  lubridate::ymd_h(x) %>%
    lubridate::as_datetime() %>%
    as.numeric()
}

#' Convert YYYYMMDDHHmm string to unix time
#'
#' @param x A YYYYMMDDHHmm string
#'
#' @return unix time in seconds since 01/01/1970 00:00:00
#' @export
#'
#' @examples
#' YMDhm2unix(201701011230)
YMDhm2unix <- function(x) {
  lubridate::ymd_hm(x) %>%
    lubridate::as_datetime() %>%
    as.numeric()
}

#' Convert unix time to YYYYMMDD string
#'
#' @param x unix time in seconds since 01/01/1970 00:00:00
#'
#' @return A YYYYMMDD string
#' @export
#'
#' @examples
#' unix2YMD(as.numeric(Sys.time()))
unix2YMD <- function(x) {
  lubridate::seconds(x) %>%
    lubridate::as_datetime() %>%
    YMD()
}

#' Convert unix time to YYYYMMDDHH string
#'
#' @param x unix time in seconds since 01/01/1970 00:00:00
#'
#' @return A YYYYMMDDHH string
#' @export
#'
#' @examples
#' unix2YMDh(as.numeric(Sys.time()))
unix2YMDh <- function(x) {
  lubridate::seconds(x) %>%
    lubridate::as_datetime() %>%
    YMDh()
}

#' Convert unix time to YYYYMMDDHHmm string
#'
#' @param x unix time in seconds since 01/01/1970 00:00:00
#'
#' @return A YYYYMMDDHHmm string
#' @export
#'
#' @examples
#' unix2YMDhm(as.numeric(Sys.time()))
unix2YMDhm <- function(x) {
  lubridate::seconds(x) %>%
    lubridate::as_datetime() %>%
    YMDhm()
}

#' Convert unix time to date type
#'
#' @param x unix time in seconds since 01/01/1970 00:00:00
#'
#' @return A date type object
#' @export
#'
#' @examples
#' unix2datetime(as.numeric(Sys.time()))
unix2datetime <- function(x) {
  lubridate::seconds(x) %>%
    lubridate::as_datetime()
}

#' Convert unix time to a date-time string
#'
#' This is a convenient wrapper for the unix2YMD... functions
#'
#' @param unixtime The unix time: number of seconds since 01/01/1970 00:00:00
#'   UTC
#' @param str_function The date-time string format to convert to. Available
#'   options are YMD, YMDh and YMDhm where Y is year (4 digits), M is month (2
#'   digits), D is day (2 digits), h is hour (2 digits) and m is minutes (2
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
#' This is a convenient wrapper for the YMD..2unix functions
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
    "8"  = {message("Date assumed to be YYYYMMDD")
            date_function <- lubridate::ymd},
    "10"= {message("Date assumed to be YYYYMMDDHH")
            date_function <- lubridate::ymd_h},
    "12"  = {message("Date assumed to be YYYYMMDDHHmm")
            date_function <- lubridate::ymd_hm},
    date_function <- NA
  )
  if (!is.function(date_function)) stop("Unknown date-time string format")
  date_function(str_datetime) %>%
    lubridate::as_datetime() %>%
    as.numeric()
}

