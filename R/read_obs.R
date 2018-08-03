#' Title
#'
#' @param obs_files A vector of observation files in sqlite format to read.
#' @param start_date The start date of the observations to read.
#' @param end_date The end date of the observations to read.
#' @param parameter Which parameter to read. This will normally be a harp3
#'   parameter name.
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
read_obs <- function(
  obs_files,
  start_date,
  end_date,
  parameter,
  gross_error_check  = TRUE,
  min_allowed        = NULL,
  max_allowed        = NULL
) {

  date_start <- str_datetime_to_unixtime(start_date)
  date_end   <- str_datetime_to_unixtime(end_date)

  obs <- list()
  list_counter <- 0
  for (in_file in obs_files) {
    list_counter <- list_counter + 1

    obs_db <- DBI::dbConnect(RSQLite::SQLite(), (in_file))

    message(in_file,":\n")
    message("Reading ", parameter, " obs for ", start_date, "-", end_date)
    obs_param <- rlang::sym(parameter)
    obs[[list_counter]] <- dplyr::tbl(obs_db, "SYNOP") %>%
      dplyr::select(validdate, SID, !!obs_param) %>%
      dplyr::filter(between(validdate, date_start, date_end)) %>%
      dplyr::collect(n = Inf) %>%
      tidyr::drop_na()
    DBI::dbDisconnect(obs_db)
    message(" ---> DONE \n")
  }

  obs <- bind_rows(obs)

  if (gross_error_check) {
    if (is.null(min_allowed)) min_allowed <- get_min_obs_allowed(parameter)
    if (is.null(max_allowed)) max_allowed <- get_max_obs_allowed(parameter)
    obs_removed <- dplyr::filter(obs, !between(.data[[parameter]], min_allowed, max_allowed))
    obs         <- dplyr::filter(obs, between(.data[[parameter]], min_allowed, max_allowed))
  }

  attr(obs, "bad_obs") <- obs_removed
  obs
}

