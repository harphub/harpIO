#' Read observations and output to sqlite OBSTABLE file(s)
#'
#' Read observations from any source (currently only vobs...) and output the
#' data to sqlite files.
#'
#' This function is used for reading point observations from files and ouputting
#' the data to sqlite files. Where observations are stored in files, the time
#' taken to read in the data can be heavily dependent on the number of files and
#' the amount of data in them. Therefore, to make the observation data available
#' more quickly for future use this function should be used to save the
#' interpolated data in sqlite files.
#'
#' Sqlite is a portable file based database solution with the ability to query
#' sqlite files using SQL syntax. This makes accessing data fast, and ensures
#' that you only read the data that you need.
#'
#' To output the data to sqlite files, a path to where you want the files to be
#' written must be given in the \code{sqlite_path} argument. To return the data
#' to the calling environment you must set \code{return_data = TRUE} - by
#' default no data are returned. This is because \code{read_det_interpolate}
#' could be processing large volumes of data and returning those data to the
#' environment could result in exceeding memory capacity. If you set neither
#' \code{sqlite_path}, nor \code{return_data} explicitly, it can appear that
#' this function does nothing.
#'
#' For observations already stored in databases, it may be better to read
#' directly from the database.
#'
#' @param start_date Date of the first observations to be read in. Should be in
#'   YYYYMMDDhh format. Can be numeric or charcter.
#' @param end_date Date of the last observations to be read in. Should be in
#'   YYYYMMDDhh format. Can be numeric or charcter.
#' @param by The time between observations. Should be a string of a number
#'   followed by a letter (the defualt is "6h"), where the letter gives the
#'   units - may be d for days, h for hours or m for minutes.
#' @param obs_path The path for the input observation files. obs_path will, in
#'   most cases, form part of the file template.
#' @param obs_format The format of the input observation files. Currently only
#'   "vobs".
#' @param obsfile_template The template for the observation files. Currently
#'   only "vobs".
#' @param parameter Not used for vobs.
#' @param sqlite_path If not NULL, sqlite files are generated and written to the
#'   directory specified here.
#' @param sqlite_template The template for the sqlite observations file. The
#'   default is "obstable", which is "{sqlite_path}/OBSTABLE_{YYYY}.sqlite".
#' @param return_data Whether to return the data to the calling environment. The
#'   default is FALSE.
#' @param iterations_per_write The number of iterations of "by" before each
#'   write to the sqlite file. The default is 24.
#' @param sqlite_synchronous The synchronus setting for sqlite files. The
#'   defualt is "off", but could also be "normal", "full", or "extra". See
#'   \url{https://www.sqlite.org/pragma.html#pragma_synchronous} for more
#'   information.
#' @param sqlite_journal_mode The journal mode for the sqlite files. The default
#'   is "delete", but can also be "truncate", "persist", "memory", "wal", or
#'   "off". See \url{https://www.sqlite.org/pragma.html#pragma_journal_mode} for
#'   more information.
#' @param ... Arguments to read functions. Not currently used.
#'
#' @return If return_data is TRUE - a list with four data frames - one for synop
#'   (near surface) observations, one for the units of the synop observations,
#'   one for the temp (upper air) observations, and one for the units of the
#'   temp observations.
#' @export
#'
#' @examples
#' if (requireNamespace("harpData", quietly = TRUE)) {
#'   read_obs_convert(
#'     start_date  = 2019021700,
#'     end_date    = 2019022023,
#'     by          = "1h",
#'     obs_path    = system.file("vobs", package = "harpData"),
#'     return_data = TRUE
#'   )
#' }
#'
read_obs_convert <- function(
  start_date,
  end_date,
  by                   = "3h",
  obs_path             = ".",
  obs_format           = "vobs",
  obsfile_template     = "vobs",
  parameter            = NULL,
  sqlite_path          = NULL,
  sqlite_template      = "obstable",
  return_data          = FALSE,
  iterations_per_write = 24,
  sqlite_synchronous   = c("off", "normal", "full", "extra"),
  sqlite_journal_mode  = c("delete", "truncate", "persist", "memory", "wal", "off"),
  ...
) {

  sqlite_synchronous  <- match.arg(sqlite_synchronous)
  sqlite_journal_mode <- match.arg(sqlite_journal_mode)

  all_dates <- seq_dates(start_date, end_date, by)

  all_dates <- split(all_dates, seq_along(all_dates) %/% iterations_per_write)

  start_date_list <- purrr::map(all_dates, 1)
  end_date_list   <- purrr::map(all_dates, ~ .x[length(.x)])

  num_iterations  <- length(start_date_list)

  if (return_data) {
    function_output <- list()
    list_counter    <- 0
  }

  sqlite_template <- get_template(sqlite_template)

  for (i in 1:num_iterations) {

    if (return_data) list_counter <- list_counter + 1

    data_files <- get_filenames(
      file_path      = obs_path,
      start_date     = start_date_list[[i]],
      end_date       = end_date_list[[i]],
      by             = by,
      det_model      = NA_character_,
      parameter      = parameter,
      lead_time      = 0,
      file_template  = obsfile_template,
      filenames_only = FALSE
    )

    read_func <- get(paste("read", obs_format, sep = "_"))

    obs_data  <- data_files %>%
      dplyr::transmute(
        .data$fcdate,
        YYYY        = substr(.data$fcdate, 1, 4),
        MM          = substr(.data$fcdate, 5, 6),
        DD          = substr(.data$fcdate, 7, 8),
        HH          = substr(.data$fcdate, 9, 10),
        obs         = purrr::map(
          .data$file_name, read_func, .data$fcdate, vfile_opts("vobs")
        ),
        file_path   = ifelse(is.null(sqlite_path), NA, sqlite_path)
      )
    obs_data <- dplyr::mutate(
      obs_data,
      file_name = purrr::map_chr(
        purrr::transpose(obs_data),
        glue::glue_data,
        sqlite_template
      )
    )

    synop_data <- obs_data %>%
      dplyr::transmute(
        .data$file_name,
        validdate = suppressMessages(str_datetime_to_unixtime(.data$fcdate)),
        synop     = purrr::map(.data$obs, "synop")
      )
    if (tidyr_new_interface()) {
      synop_data <- synop_data %>%
        tidyr::unnest(tidyr::one_of("synop")) %>%
        tidyr::nest(synop = -tidyr::one_of("file_name"))
    } else {
      synop_data <- synop_data %>%
        tidyr::unnest() %>%
        dplyr::group_by(.data$file_name) %>%
        tidyr::nest(.key = "synop")
    }

    synop_params <- purrr::map_df(obs_data$obs, "synop_params") %>%
      dplyr::distinct()

    temp_data <- obs_data %>%
      dplyr::transmute(
        .data$file_name,
        validdate = suppressMessages(str_datetime_to_unixtime(.data$fcdate)),
        temp      = purrr::map(.data$obs, "temp")
      )
    if (tidyr_new_interface()) {
      temp_data <- temp_data %>%
        tidyr::unnest(tidyr::one_of("temp")) %>%
        tidyr::nest(temp = -tidyr::one_of("file_name"))
    } else {
      temp_data <- temp_data %>%
        tidyr::unnest() %>%
        dplyr::group_by(.data$file_name) %>%
        tidyr::nest(.key = "temp")
    }

    temp_params <- purrr::map_df(obs_data$obs, "temp_params") %>%
      dplyr::distinct()

    if (!is.null(sqlite_path)) {
      purrr::walk2(
        synop_data$synop,
        synop_data$file_name,
        write_obstable_to_sqlite,
        table_name   = "SYNOP",
        primary_key  = c("validdate", "SID"),
        params_table = synop_params,
        synchronous  = sqlite_synchronous,
        journal_mode = sqlite_journal_mode
      )
      purrr::walk2(
        temp_data$temp,
        temp_data$file_name,
        write_obstable_to_sqlite,
        table_name   = "TEMP",
        primary_key  = c("validdate", "SID", "p"),
        params_table = temp_params,
        synchronous  = sqlite_synchronous,
        journal_mode = sqlite_journal_mode
      )
    }

    if (return_data) {
      function_output[[i]] <- list(
        synop        = synop_data,
        temp         = temp_data,
        synop_params = synop_params,
        temp_params  = temp_params
      )
    }

  }

  if (return_data) {
    list(
      synop = purrr::map(function_output, "synop") %>%
        purrr::map(dplyr::pull, .data$synop) %>%
        purrr::flatten_dfr(),
      temp  = purrr::map(function_output, "temp") %>%
        purrr::map(dplyr::pull, .data$temp) %>%
        purrr::flatten_dfr(),
      synop_params = dplyr::distinct(purrr::map_df(function_output, "synop_params")),
      temp_params  = dplyr::distinct(purrr::map_df(function_output, "temp_params"))
    )
  }

}
