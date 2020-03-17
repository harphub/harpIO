#' Get filename(s) for meteorological data
#'
#' Given a path, a date or start date and end date, a template and other inputs
#' (see Arguments) a vector of filenames is returned.
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
#' @param by The frequency of the date range. Should be a string of a number
#'   followed by a letter, where the letter gives the units - may be d for days,
#'   h for hours or m for minutes.
#' @param lags For reading files from a lagged forecast with members run at
#'   different times, the lag times are set here. The times are expressed as a
#'   character vector, with a number followed by a letter giving the units. The
#'   avialable units are d, h, m, s for days, hours, minutes and seconds. If
#'   \{MBRx\} is in the template, it must be the same length as \code{members}.
#' @param parameter If \{parameter\} exists in the the template this must be
#'   specified.
#' @param det_model If \{det_model\} exists in the the template this must be
#'   specified.
#' @param eps_model If \{eps_model\} exists in the the template this must be
#'   specified.
#' @param sub_model If \{sub_model\} exists in the the template this must be
#'   specified.
#' @param lead_time The lead times to be included in the file names if \{LDTx\}
#'   is in the template. Given as a vector of numbers.
#' @param members The members to be included in the file names of \{MBRx\} is in
#'   the template
#' @param file_template The file type to generate the template for. Can be any
#'   of the build in templates from \link{show_file_templates}. If anything else
#'   is passed, it is returned unmodified. In this case substitutions can be
#'   used. Available substitutions are {YYYY} for year, \{MM\} for 2 digit month
#'   with leading zero, \{M\} for month with no leading zero, and similarly
#'   \{DD\} or \{D\} for day, \{HH\} or \{H\} for hour, \{mm\} or \{m\} for
#'   minute. Also \{LDTx\} for lead time and \{MBRx\} for ensemble member where
#'   x is the length of the string including leading zeros - can be omitted or
#'   2, 3 or 4. Note that the full path to the file will always be
#'   file_path/template.
#' @param filenames_only Logical. Set to TRUE to return a vector of unique file
#'   names. Set to FALSE to return a detailed data frame.
#' @return If filenames_only is TRUE, a vector of unique file names. If
#'   filenames_only is FALSE, a tibble with columns eps_model, sub_model,
#'   fcdate, file_name, lead_time and member.
#' @export
#'
#' @examples
#' get_filenames("/my/path", eps_model = "my_eps", parameter = "T2m")
#'
#' get_filenames("/my/path", start_date = 20170101, end_date = 20170131, by
#' = "1d", eps_model = "my_eps", parameter = "T2m", file_template =
#' "harmonie_grib")
#'
#' get_filenames("/my/path", start_date = 20170101, end_date = 20170131, by
#' = "1d", eps_model = "my_eps", parameter = "T2m", members = seq(0, 2),
#' file_template = "harmoneps_grib_fp")
#'
#' get_filenames("/my/path", file_date = 20170101, parameter = "T2m",
#' members = seq(0, 2), file_template = "harmoneps_grib_fp")
#'
#' get_filenames("/my/path", start_date = 20170101, end_date = 20170105, by
#' = "6h", eps_model = "my_eps", parameter = "T2m", members = seq(0,3),
#' lead_time = seq(0, 24, 6), file_template =
#' "{eps_model}/{YYYY}{MM}{DD}{HH}_mbr{MBR2}+{LDT}h")
#'
get_filenames <- function(
  file_path      = "",
  file_date      = Sys.Date(),
  start_date     = NULL,
  end_date       = NULL,
  by             = "6h",
  lags           = NULL,
  parameter      = NULL,
  det_model      = NULL,
  eps_model      = NULL,
  sub_model      = NULL,
  lead_time      = seq(0, 48, 3),
  members        = NA_character_,
  file_template  = "fctable_eps",
  filenames_only = TRUE
) {

  add_zeros <- function(x) {
    switch(as.character(nchar(x)),
      "8"  = paste0(x, "0000"),
      "10" = paste0(x, "00"),
      "12" = as.character(x),
      NA
    )
  }

  units_multiplier_vec <- Vectorize(units_multiplier, USE.NAMES = FALSE)

  template <- get_template(file_template)

  lags_passed  <- FALSE
  if (!is.null(lags)) {
    lags_passed <- TRUE
    if (stringr::str_detect(template, "\\{MBR")) {
      if (any(is.na(members))) {
        stop("If lags are given with {MBR*} in the file_template, members must also be passed.", call. = FALSE)
      }
      if (length(lags) != length(members)) {
        stop(
          paste(
            "The number of lags should be the same as the number of members.",
            paste("Number of lags:   ", length(lags)),
            paste("Number of members:", length(members)),
            sep = "\n"
          ),
          call. = FALSE
        )
      }
    }
    lags_seconds <- unique(readr::parse_number(lags) * units_multiplier_vec(lags))
    if (any(is.na(lags_seconds))) {
      stop(
        "Lags must be expressed as a number followed by a unit\n",
        "Available units are d, h, m, s for days, hours, minutes, seconds respectively.",
        call. = FALSE
      )
    }
  }

  if (grepl("_det", file_template)) {
    if (!is.null(eps_model) & is.null(det_model)) {
      det_model <- eps_model
      eps_model <- NULL
    }
  }

  if (is.null(start_date)) {

    if (lubridate::is.Date(file_date)) {
      file_dates <- lubridate::as_datetime(file_date) %>%
        as.numeric()
    } else if (lubridate::is.POSIXct(file_date)) {
      file_dates <- as.numeric(file_date)
    } else {
      file_dates <- suppressMessages(str_datetime_to_unixtime(file_date))
    }

  } else {

    if (is.null(end_date)) {
      stop ("end_date must be passed as well as start_date", call. = FALSE)
    }
    file_dates <- seq_dates(start_date, end_date, by = by)
    file_dates <- suppressMessages(str_datetime_to_unixtime(file_dates))
  }

  if (!lags_passed) {
    lags <- "0s"
  }

  lag_col <- sort(unique(lags))

  file_lags <- tibble::tibble(
    lag         = lag_col,
    lag_seconds = readr::parse_number(.data$lag) * units_multiplier_vec(.data$lag)
  )

  file_dates <- file_lags %>%
    dplyr::mutate(
      lag    = readr::parse_number(.data$lag),
      fcdate = list(file_dates)
    )

  if (tidyr_new_interface()) {
    file_dates <- tidyr::unnest(file_dates, tidyr::one_of("fcdate"))
  } else {
    file_dates <- tidyr::unnest(file_dates)
  }

  file_dates <- file_dates %>%
    dplyr::mutate(
      fcdate = .data$fcdate - .data$lag_seconds,
      YYYY   = unix2datetime(.data$fcdate) %>% lubridate::year(),
      MM     = unix2datetime(.data$fcdate) %>% lubridate::month() %>% formatC(width = 2, flag = "0"),
      DD     = unix2datetime(.data$fcdate) %>% lubridate::day() %>% formatC(width = 2, flag = "0"),
      HH     = unix2datetime(.data$fcdate) %>% lubridate::hour() %>% formatC(width = 2, flag = "0"),
      mm     = unix2datetime(.data$fcdate) %>% lubridate::minute() %>% formatC(width = 2, flag = "0"),
      M      = unix2datetime(.data$fcdate) %>% lubridate::month() %>% as.character(),
      D      = unix2datetime(.data$fcdate) %>% lubridate::day() %>% as.character(),
      H      = unix2datetime(.data$fcdate) %>% lubridate::hour() %>% as.character(),
      m      = unix2datetime(.data$fcdate) %>% lubridate::minute() %>% as.character(),
      fcdate = unixtime_to_str_datetime(.data$fcdate, YMDhm)
    )

  strings_in_template <- names(file_dates)[stringr::str_detect(template, paste0("\\{", names(file_dates), "\\}"))]

  file_dates <- file_dates %>%
    dplyr::select(.data$fcdate, .data$lag, !! rlang::quo(strings_in_template)) %>%
    dplyr::distinct()

  files <- file_path %>%
    purrr::map( ~ cbind(file_dates, file_path = .x, stringsAsFactors = FALSE)) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  if (stringr::str_detect(template, "\\{det_model\\}")) {
    if (is.null(det_model)) stop (paste0("det_model is in template, but not passed to the function\n", template))
    files <- det_model %>%
      purrr::map( ~ cbind(files, det_model = .x, stringsAsFactors = FALSE)) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()
  } else {
    if (is.null(det_model)) det_model <- NA_character_
    files <- dplyr::mutate(files, det_model = det_model)
  }

  if (stringr::str_detect(template, "\\{eps_model\\}")) {
    if (is.null(eps_model)) stop(paste0("eps_model is in template, but not passed to the function\n", template))
    files <- eps_model %>%
      purrr::map( ~ cbind(files, eps_model = .x, stringsAsFactors = FALSE)) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()
  } else {
    if (is.null(eps_model)) eps_model <- NA_character_
    files <- dplyr::mutate(files, eps_model = eps_model)
  }

  if (stringr::str_detect(template, "\\{sub_model\\}")) {
    if (is.null(sub_model)) stop(paste0("sub_model is in template, but not passed to the function\n", template))
    files <- sub_model %>%
      purrr::map( ~ cbind(files, sub_model = .x, stringsAsFactors = FALSE)) %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()
  } else {
    if (is.null(sub_model)) sub_model <- NA_character_
    files <- dplyr::mutate(files, sub_model = sub_model)
  }

  if (stringr::str_detect(template, "\\{LDT")) {
    files <- files %>%
      dplyr::mutate(LDT = list(lead_time))

    if (tidyr_new_interface()) {
      files <- tidyr::unnest(files, tidyr::one_of("LDT"))
    } else {
      files <- tidyr::unnest(files)
    }

    files <- files %>%
      dplyr::mutate(
        LDT = .data$LDT + .data$lag,
        LDT2 = formatC(as.numeric(.data$LDT), width = 2, flag = "0"),
        LDT3 = formatC(as.numeric(.data$LDT), width = 3, flag = "0"),
        LDT4 = formatC(as.numeric(.data$LDT), width = 4, flag = "0"),
        LDT  = as.character(.data$LDT)
      ) %>%
      dplyr::filter(as.numeric(.data$LDT) >= 0)
  } else {
    files <- files %>%
      dplyr::mutate(
        LDT = list(lead_time)
      )
  }

  if (stringr::str_detect(template, "\\{MBR")) {
    files <- files %>%
      dplyr::mutate(
        MBR = purrr::map(
          .data$lag,
          ~ members[readr::parse_number(lags) == .x]
        )
      )

    if (tidyr_new_interface()) {
      files <- tidyr::unnest(files, tidyr::one_of("MBR"))
    } else {
      files <- tidyr::unnest(files)
    }

    files <- files %>%
      dplyr::mutate(
        MBR2 = formatC(as.numeric(.data$MBR), width = 2, flag = "0"),
        MBR3 = formatC(as.numeric(.data$MBR), width = 3, flag = "0"),
        MBR4 = formatC(as.numeric(.data$MBR), width = 4, flag = "0")
      )
  } else {
    if (!is.null(eps_model)) {
      files <- files %>%
        dplyr::mutate(MBR = list(members))
    }
  }

  if (stringr::str_detect(template, "\\{parameter\\}")) {
    if (is.null(parameter)) stop(paste0("parameter is in template, but not passed to the function\n", template))
    files <- files %>%
      dplyr::mutate(parameter = parameter)
  }

  files <- files %>%
    dplyr::mutate(
      file_name = as.vector(glue::glue_data(files, template))
    )

  if (is.na(eps_model)) {
    if (is.na(sub_model)) {
      model_cols <- rlang::syms(c("det_model", "fcdate"))
    } else {
      files <- dplyr::mutate(files, eps_model = .data$sub_model)
      model_cols <- rlang::syms(c("eps_model", "sub_model", "fcdate", "MBR"))
    }
  } else {
    model_cols <- rlang::syms(c("eps_model", "sub_model", "fcdate", "MBR"))
  }
  files <- files %>% dplyr::transmute(
    !!! model_cols,
    lead_time = .data$LDT,
    .data$file_name
  )

  if (is.na(eps_model)) {
    if (tidyr_new_interface()) {
      files <- tidyr::unnest(files, tidyr::one_of("lead_time"))
    } else {
      files <- tidyr::unnest(files, .data$lead_time, .drop = FALSE)
    }
    files <- dplyr::mutate_at(files, dplyr::vars(.data$lead_time), as.numeric)
  } else {
    files <- dplyr::rename(files, member = .data$MBR)
    if (tidyr_new_interface()) {
      files <- files %>%
        tidyr::unnest(tidyr::one_of("lead_time")) %>%
        tidyr::unnest(tidyr::one_of("member"))
    } else {
      files <- files %>%
        tidyr::unnest(.data$lead_time, .drop = FALSE) %>%
        tidyr::unnest(.data$member, .drop = FALSE)
    }
    files <- dplyr::mutate_at(files, dplyr::vars(.data$lead_time,.data$member), as.numeric)
  }

  if (filenames_only) {
    unique(files$file_name)
  } else {
    files
  }

}


