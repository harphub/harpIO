#' Read point observations from OBSTABLE files
#'
#' @param start_date The start date of the observations to read.
#' @param end_date The end date of the observations to read.
#' @param parameter Which parameter to read. This will normally be a harp3
#'   parameter name.
#' @param obs_path The path to the OBSTABLE files
#' @param obsfile_template The template for the OBSTABLE file name.
#' @param gross_error_check Logical of whether to perform a gross error check.
#' @param min_allowed The minimum value of observation to allow in the gross error
#'   check. If set to NULL the default value for the parameter is used.
#' @param max_allowed The maximum value of observation to allow in the gross error
#'   check. If set to NULL the default value for the parameter is used.
#'
#' @return A tibble with columns for validdate, SID and the parameter.
#' @export
#'
#' @examples
read_point_obs <- function(
  start_date,
  end_date,
  parameter,
  obs_path           = ".",
  obsfile_template   = "obstable",
  gross_error_check  = TRUE,
  min_allowed        = NULL,
  max_allowed        = NULL
) {

  obs_files <- get_filenames(
    obs_path,
    start_date    = start_date,
    end_date      = end_date,
    file_template = obsfile_template
  )

  available_files <- obs_files[file.exists(obs_files)]
  missing_files   <- obs_files[!file.exists(obs_files)]

  if (length(available_files) < 1) {
    stop(paste("Files not found:\n", paste(missing_files, collapse = "\n")), call. = FALSE)
  }

  if (length(missing_files) > 0) {
    warning(paste("Files not found:\n", paste(missing_files, collapse = "\n")), call. = FALSE, immediate. = TRUE)
  }

  date_start <- suppressMessages(str_datetime_to_unixtime(start_date))
  date_end   <- suppressMessages(str_datetime_to_unixtime(end_date))

  harp_param <- parse_harp_parameter(parameter)
  if (!is.null(harp_param$levelType) & harp_param$levelType == "pressure") {
    sqlite_table <- "TEMP"
    obs_param    <- rlang::sym(harp_param$basename)
  } else {
    sqlite_table <- "SYNOP"
    obs_param    <- rlang::sym(parameter)
  }

  obs <- list()
  list_counter <- 0
  for (in_file in available_files) {
    list_counter <- list_counter + 1

    obs_db <- DBI::dbConnect(RSQLite::SQLite(), (in_file))

    message("\nReading: ", in_file, ":")
    message(parameter, " obs for ", start_date, "-", end_date)
    if (sqlite_table == "SYNOP") {
      obs[[list_counter]] <- dplyr::tbl(obs_db, sqlite_table) %>%
        dplyr::select(validdate, SID, !!obs_param) %>%
        dplyr::filter(validdate >= date_start & validdate <= date_end) %>%
        dplyr::collect(n = Inf) %>%
        tidyr::drop_na()
    } else {
      obs[[list_counter]] <- dplyr::tbl(obs_db, sqlite_table) %>%
        dplyr::select(validdate, SID, p, !!obs_param) %>%
        dplyr::filter(validdate >= date_start & validdate <= date_end) %>%
        dplyr::filter(p == harp_param$level) %>%
        dplyr::collect(n = Inf) %>%
        tidyr::drop_na()
    }
    DBI::dbDisconnect(obs_db)
    message(" ---> DONE \n")
  }

  obs <- dplyr::bind_rows(obs)

  if (gross_error_check) {
    if (is.null(min_allowed)) min_allowed <- get_min_obs_allowed(parameter)
    if (is.null(max_allowed)) max_allowed <- get_max_obs_allowed(parameter)
    obs_removed <- dplyr::filter(obs, !dplyr::between(!! obs_param, min_allowed, max_allowed))
    obs         <- dplyr::filter(obs, dplyr::between(!! obs_param, min_allowed, max_allowed))
  } else {
    obs_removed = "No gross error check done."
  }

  attr(obs, "bad_obs") <- obs_removed
  obs
}

