#' Read observations and output to sqlite OBSTABLE file(s)
#'
#' Read observations from any source (currently only vobs...) and output the
#' data to sqlite files.
#'
#' @param start_date First to read observations for. Must be at least YYYYMMDD
#'   format.
#' @param end_date Last to read observations for. Must be at least YYYYMMDD
#'   format.
#' @param by How frequently to read observations. Must be a chracter string with
#'   a number followed by a letter, where "m" is minutes, "h" is hours, and "d"
#'   is days.
#' @param obs_path The path for the input observation files.
#' @param obs_format The format of the input observation files. Currently only
#'   "vobs".
#' @param obsfile_template The template for the observation files. Currently
#'   only "vobs".
#' @param parameter Not used for vobs.
#' @param sqlite_path The path to write the sqlite output to.
#' @param sqlite_template The template for the sqlite observations file. The
#'   default is "{sqlite_path}/OBSTABLE_{YYYY}.sqlite".
#' @param return_data Whether to return the data to the R session. The default
#'   is FALSE.
#' @param iterations_per_write The number of iterations of "by" before each
#'   write to the sqlite file. The default is 24.
#' @param ... Arguments to read functions.
#'
#' @return If return_data is TRUE - a list with two data frames - one for synop
#'   (near surface) observations and one for temp (upper air) observatoions.
#' @export
#'
#' @examples
read_obs_convert <- function(
  start_date,
  end_date,
  by                   = "3h",
  obs_path             = ".",
  obs_format           = "vobs",
  obsfile_template     = "vobs",
  parameter            = NULL,
  sqlite_path          = NULL,
  sqlite_template      = "{sqlite_path}/OBSTABLE_{YYYY}.sqlite",
  return_data          = FALSE,
  iterations_per_write = 24,
  ...
) {

  all_dates <- seq_dates(start_date, end_date, by)

  all_dates <- split(all_dates, seq_along(all_dates) %/% iterations_per_write)

  start_date_list <- purrr::map(all_dates, 1)
  end_date_list   <- purrr::map(all_dates, ~ .x[length(.x)])

  num_iterations  <- length(start_date_list)

  if (return_data) {
    function_output <- list()
    list_counter    <- 0
  }

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
        obs         = purrr::map(file_name, read_func, ...),
        sqlite_path = ifelse(is.null(sqlite_path), NA, sqlite_path)
      ) %>%
      dplyr::mutate(
        file_name = purrr::map_chr(
          purrr::transpose(.),
          glue::glue_data,
          sqlite_template
        )
      )

    synop_data <- obs_data %>%
      dplyr::transmute(
        .data$file_name,
        validdate = suppressMessages(str_datetime_to_unixtime(.data$fcdate)),
        synop     = purrr::map(obs, "synop")
      ) %>%
      tidyr::unnest() %>%
      dplyr::group_by(.data$file_name) %>%
      tidyr::nest(.key = "synop")

    temp_data <- obs_data %>%
      dplyr::transmute(
        .data$file_name,
        validdate = suppressMessages(str_datetime_to_unixtime(.data$fcdate)),
        temp      = purrr::map(obs, "temp")
      ) %>%
      tidyr::unnest() %>%
      dplyr::group_by(.data$file_name) %>%
      tidyr::nest(.key = "temp")

    if (!is.null(sqlite_path)) {
      purrr::walk2(synop_data$synop, synop_data$file_name, write_obstable_to_sqlite, table_name = "SYNOP")
      purrr::walk2(temp_data$temp, temp_data$file_name, write_obstable_to_sqlite, table_name = "TEMP", primary_key = (c("validdate", "SID", "p")))
    }

    if (return_data) {
      function_output[[i]] <- list(synop = synop_data, temp = temp_data)
    }

  }

  if (return_data) {
    list(
      synop = purrr::map(function_output, "synop") %>%
        purrr::map(dplyr::pull, .data$synop) %>%
        purrr::flatten_dfr(),
      temp  = purrr::map(function_output, "temp") %>%
        purrr::map(dplyr::pull, .data$temp) %>%
        purrr::flatten_dfr()
    )
  }

}
