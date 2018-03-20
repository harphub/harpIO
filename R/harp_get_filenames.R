#' Get filename(s) for meteorological data
#'
#' Given a path, a date or start date and end date, a template and other inputs
#' (see Arguments) a vector of filenames is returned.
#'
#' @importFrom magrittr %>%
#'
#' @param file_path The static path to the data.
#' @param file_date A single date - may be YYYYMMDD, YYYYMMDDHH, YYYYMMDDHHmm.
#'   If it is not specified and \code{start_date} is not specified then the
#'   system date is used with a time of 00:00.
#' @param start_date If specified it is assumed a range of dates will be
#'   requested and \code{end_date} must also be specified. The range is given by
#'   \code{start_date} and \code{end_date} with the frequency given by
#'   \code{by}. If \code{start_date} is passed then \code{file_date} is ignored.
#'   The format is the same as that for \code{file_date}.
#' @param end_date The end of a date range. Same format as for \code{file_date}.
#' @param by The frequecny of the date range. Should be a string of a number
#'   followed by a letter, where the letter gives the units - may be d for days,
#'   h for hours or m for minutes.
#' @param parameter If $\{parameter\} exists in the the template this must be
#'   specified.
#' @param experiment If $\{experiment\} exists in the the template this must be
#'   specified.
#' @param lead_time The lead times to be included in the file names if $\{LDTx\}
#'   is in the template. Given as a vector of numbers.
#' @param member The members to be included in the file names of $\{MBRx\} is in
#'   the template. Given as a vector of numbers.
#' @param template The file type to generate the template for. Can be
#'   "harmoneps_grib", "harmeoneps_grib_fp", "harmoneps_grib_sfx",
#'   "harmonie_grib", "harmonie_grib_fp", "harmone_grib_sfx", "vfld", "vobs", or
#'   "fctable". If anything else is passed, it is returned unmodified. In this
#'   case substitutions can be used. Available substitutions are $\{YYYY\} for
#'   year, $\{MM\} for 2 digit month with leading zero, $\{M\} for month with no
#'   leading zero, and similarly $\{DD\} or $\{D\} for day, $\{HH\} or $\{H\}
#'   for hour, $\{mm\} or $\{m\} for minute. Also $\{LDTx\} for lead time and
#'   $\{MBRx\} for ensemble member where x is the length of the string including
#'   leading zeros - can be omitted or 2, 3 or 4. Note that the full path to the
#'   file will always be file_path/template.
#'
#' @return A vector of filenames
#' @export
#'
#' @examples
#' harp_get_filenames("/my/path", experiment = "my_exp", parameter = "T2m")
#' harp_get_filenames("/my/path", start_date = 20170101, end_date = 20170131,
#'   by = "1d", experiment = "my_exp", parameter = "T2m", template = "harmonie_grib")
#' harp_get_filenames("/my/path", start_date = 20170101, end_date = 20170131,
#'   by = "1d", experiment = "my_exp", parameter = "T2m", template = "harmoneps_grib_fp")
#' harp_get_filenames("/my/path", file_date = 20170101, parameter = "T2m",
#'   template = "harmoneps_grib_fp")
#' harp_get_filenames("/my/path", start_date = 20170101, end_date = 20170105,
#'   by = "6h", experiment = "my_exp", parameter = "T2m", member = seq(0,3), lead_time = seq(0, 24, 6),
#'   template = "${experiment}/${YYYY}${MM}${DD}${HH}_mbr${MBR2}+${LDT}h")
#'
harp_get_filenames <- function(
  file_path,
  file_date     = Sys.Date(),
  start_date    = NULL,
  end_date      = NULL,
  by            = "6h",
  parameter     = NULL,
  experiment    = NULL,
  lead_time     = seq(0, 48, 3),
  member        = seq(0,9),
  template      = "FCTABLE"
) {
#
  add_zeros <- function(x) {
    switch(as.character(nchar(x)),
      "8"  = paste0(x, "0000"),
      "10" = paste0(x, "00"),
      "12" = as.character(x),
      NA
    )
  }

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

  template <- get_template(template)

  if (is.null(start_date)) {

    if (lubridate::is.Date(file_date)) file_date <- lubridate::as_datetime(file_date) %>%
      lubridate::seconds() %>%
      unix2YMDh()
    file_dates <- add_zeros(file_date)
    if (is.na(file_dates)) stop(paste0("Incorrect format for file_date : ", file_date))

  } else {

    if (is.null(end_date)) stop ("end_date must be passed as well as start_date")
    start_date <- add_zeros(start_date)
    end_date   <- add_zeros(end_date)
    by         <- readr::parse_number(by) * units_multiplier(by)

    if (is.na(by)) stop("Unable to parse units. Use d, h, m or s. e.g. by = '6h'")

    file_dates <- seq(YMDhm2unix(start_date), YMDhm2unix(end_date), by = by) %>%
      unix2YMDhm()
  }
#
  file_dates <- tibble::tibble(
    YYYY = file_dates %>% lubridate::ymd_hm() %>% lubridate::year(),
    MM   = file_dates %>% lubridate::ymd_hm() %>% lubridate::month() %>% formatC(width = 2, flag = "0"),
    DD   = file_dates %>% lubridate::ymd_hm() %>% lubridate::day() %>% formatC(width = 2, flag = "0"),
    HH   = file_dates %>% lubridate::ymd_hm() %>% lubridate::hour() %>% formatC(width = 2, flag = "0"),
    mm   = file_dates %>% lubridate::ymd_hm() %>% lubridate::minute() %>% formatC(width = 2, flag = "0"),
    M    = file_dates %>% lubridate::ymd_hm() %>% lubridate::month() %>% as.character(),
    D    = file_dates %>% lubridate::ymd_hm() %>% lubridate::day() %>% as.character(),
    H    = file_dates %>% lubridate::ymd_hm() %>% lubridate::hour() %>% as.character(),
    m    = file_dates %>% lubridate::ymd_hm() %>% lubridate::minute() %>% as.character()
  )
#
  strings_in_template <- names(file_dates)[stringr::str_detect(template, paste0("\\{", names(file_dates), "\\}"))]
#
  file_dates <- file_dates %>%
    dplyr::select(!! rlang::quo(strings_in_template)) %>%
    dplyr::distinct()
#
  files <- file_path %>%
    purrr::map( ~ cbind(file_dates, file_path = .x, stringsAsFactors = FALSE)) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()
#
  if (stringr::str_detect(template, "\\{experiment\\}")) {
    if (is.null(experiment)) stop (paste0("experiment is in template, but not passed to the function\n", template))
    files <- experiment %>%
      purrr::map( ~ cbind(files, experiment = .x, stringsAsFactors = FALSE)) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()
  }
#
  if (stringr::str_detect(template, "\\{LDT")) {
    files <- lead_time %>%
      purrr::map( ~ cbind(files, LDT = as.character(.x), stringsAsFactors = FALSE)) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        LDT2 = formatC(as.numeric(LDT), width = 2, flag = "0"),
        LDT3 = formatC(as.numeric(LDT), width = 3, flag = "0"),
        LDT4 = formatC(as.numeric(LDT), width = 4, flag = "0")
      )
  }
#
  if (stringr::str_detect(template, "\\{MBR")) {
    files <- member %>%
      purrr::map( ~ cbind(files, MBR = as.character(.x), stringsAsFactors = FALSE)) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        MBR2 = formatC(as.numeric(MBR), width = 2, flag = "0"),
        MBR3 = formatC(as.numeric(MBR), width = 3, flag = "0"),
        MBR4 = formatC(as.numeric(MBR), width = 4, flag = "0")
      )
  }
#
  if (stringr::str_detect(template, "\\{parameter\\}")) {
    if (is.null(parameter)) stop (paste0("parameter is in template, but not passed to the function\n", template))
    files <- files %>%
      dplyr::mutate(parameter = parameter)
  }
#
  files <- files %>%
    dplyr::rowwise() %>%
    dplyr::transmute(data_file = stringr::str_interp(string = template)) %>%
    dplyr::pull() %>%
    unique()
#
  files
}
