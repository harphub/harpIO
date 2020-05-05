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

#' Convert date type to YYYYMMDDHHmm string
#'
#' @param x A date object.
#'
#' @return A YYYYMMDDHHmm string
#' @export
#'
#' @examples
#' YMDhms(Sys.time())
YMDhms <- function(x) {
  paste0(
    YMDhm(x),
    formatC(lubridate::second(x), width = 2, flag = 0)
  )
}

# Convert YYYYMMDD string to unix time
#
# @param x A YYYYMMDD string
#
# @return unix time in seconds since 01/01/1970 00:00:00
# Not exported - use str_datetime_to_unixtime instead
#
# @examples
# YMD2unix(20170101)
YMD2unix <- function(x) {
  lubridate::ymd(x) %>%
    lubridate::as_datetime() %>%
    as.numeric()
}

# Convert YYYYMMDDHH string to unix time
#
# @param x A YYYYMMDDHH string
#
# @return unix time in seconds since 01/01/1970 00:00:00
# Not exported - use str_datetime_to_unixtime instead
#
# @examples
# YMDh2unix(2017010112)
YMDh2unix <- function(x) {
  lubridate::ymd_h(x) %>%
    lubridate::as_datetime() %>%
    as.numeric()
}

# Convert YYYYMMDDHHmm string to unix time
#
# @param x A YYYYMMDDHHmm string
#
# @return unix time in seconds since 01/01/1970 00:00:00
# Not exported - use str_datetime_to_unixtime instead
#
# @examples
# YMDhm2unix(201701011230)
YMDhm2unix <- function(x) {
  lubridate::ymd_hm(x) %>%
    lubridate::as_datetime() %>%
    as.numeric()
}

# Convert unix time to YYYYMMDD string
#
# @param x unix time in seconds since 01/01/1970 00:00:00
#
# @return A YYYYMMDD string
# Not exported - use unixtime_to_str_datetime instead
#
# @examples
# unix2YMD(as.numeric(Sys.time()))
unix2YMD <- function(x) {
  lubridate::seconds(x) %>%
    lubridate::as_datetime() %>%
    YMD()
}

# Convert unix time to YYYYMMDDHH string
#
# @param x unix time in seconds since 01/01/1970 00:00:00
#
# @return A YYYYMMDDHH string
# Not exported - use unixtime_to_str_datetime instead
#
# @examples
# unix2YMDh(as.numeric(Sys.time()))
unix2YMDh <- function(x) {
  lubridate::seconds(x) %>%
    lubridate::as_datetime() %>%
    YMDh()
}

# Convert unix time to YYYYMMDDHHmm string
#
# @param x unix time in seconds since 01/01/1970 00:00:00
#
# @return A YYYYMMDDHHmm string
# Not exported - use unixtime_to_str_datetime instead
#
# @examples
# unix2YMDhm(as.numeric(Sys.time()))
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
#' str_datetime_to_unixtime(2017010112)
#' str_datetime_to_unixtime("201701011230")
#'
str_datetime_to_unixtime <- function(str_datetime) {
  switch(as.character(nchar(str_datetime[1])),
    "8"  = {message("Date assumed to be YYYYMMDD")
      date_function <- lubridate::ymd},
    "10"= {message("Date assumed to be YYYYMMDDHH")
      date_function <- lubridate::ymd_h},
    "12"  = {message("Date assumed to be YYYYMMDDHHmm")
      date_function <- lubridate::ymd_hm},
    "14"  = {message("Date assumed to be YYYYMMDDHHmmss")
      date_function <- lubridate::ymd_hms},
    date_function <- NA
  )
  if (!is.function(date_function)) stop("Unknown date-time string format")
  date_function(str_datetime) %>%
    lubridate::as_datetime() %>%
    as.numeric()
}

#' @rdname str_datetime_to_unixtime
#' @export
#' @examples
#' str_datetime_to_datetime("20170101")
#' str_datetime_to_datetime(2017010112)
#' str_datetime_to_datetime("201701011230")

str_datetime_to_datetime <- function(str_datetime) {
  unix2datetime(
    str_datetime_to_unixtime(
      str_datetime
    )
  )
}

#' Generate a sequence of dates
#'
#' @param start_date Start date as YYYYMMDD, YYYYMMDDhh, or YYYYMMDDhhmm. Can be
#'   string or numeric.
#' @param end_date End date as YYYYMMDD, YYYYMMDDhh, or YYYYMMDDhhmm. Can be
#'   string or numeric.
#' @param by Increment of the sequence. Should be a string of a number followed
#'   by a letter, where the letter gives the units - may be d for days, h for
#'   hours or m for minutes. The default is '1h'.
#'
#' @return A sequence of date-times in YYYYMMDDhhmm format
#' @export
#'
#' @examples
#' seq_dates(20170101, 20170131, by = "1d")
#' seq_dates(201701010600, 201701051200)
seq_dates <- function(start_date, end_date, by = "1h") {

  if (lubridate::is.POSIXct(start_date)) {
    start_date <- as.numeric(start_date)
  } else {
    start_date <- suppressMessages(str_datetime_to_unixtime(start_date))
  }

  if (lubridate::is.POSIXct(start_date)) {
    end_date <- as.numeric(end_date)
  } else {
    end_date <- suppressMessages(str_datetime_to_unixtime(end_date))
  }

  by_secs <- readr::parse_number(by) * units_multiplier(by)

  if (is.na(by_secs)) stop("Unable to parse units. Use d, h, m or s. e.g. by = '6h'")

  seq(start_date, end_date, by = by_secs) %>%
    unix2YMDhm()

}

### Helper function to find the multiplier to convert a given time unit to seconds.

units_multiplier <- function(x) {
  time_units <- stringr::str_extract(tolower(x), "[a-z]+")
  switch(time_units,
    "d" = 60 * 60 * 24,
    "h" = 60 * 60,
    "m" = 60,
    "s" = 1,
    NA_real_
  )
}


