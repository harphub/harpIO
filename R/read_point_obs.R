#' Read point observations from OBSTABLE files
#'
#' Data are read directly from OBSTABLE files with no modification except in the
#' case of precipitation accumulations. Where possible, observations that are
#' not available for the given accumulation time, they are derived from
#' observations for other accumulation times, either by subtraction or addition.
#'
#' @param start_date The start date of the observations to read.
#' @param end_date The end date of the observations to read.
#' @param parameter Which parameter to read. This will normally be a harp3
#'   parameter name.
#' @param obs_path The path to the OBSTABLE files
#' @param obsfile_template The template for the OBSTABLE file name.
#' @param gross_error_check Logical of whether to perform a gross error check.
#' @param min_allowed The minimum value of observation to allow in the gross
#'   error check. If set to NULL the default value for the parameter is used.
#' @param max_allowed The maximum value of observation to allow in the gross
#'   error check. If set to NULL the default value for the parameter is used.
#' @param stations The stations to retrieve observations for. This should be a
#'   vector of station ID numbers. Set to NULL to retrieve all stations.
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
  max_allowed        = NULL,
  stations           = NULL
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
  if (!is.null(harp_param$level_type) && harp_param$level_type == "pressure") {
    sqlite_table <- "TEMP"
    obs_param    <- rlang::sym(harp_param$basename)
  } else {
    sqlite_table <- "SYNOP"
    obs_param    <- rlang::sym(parameter)
  }

  message("Getting ", parameter, " observations for ", start_date, "-", end_date)
  obs <- read_obstable(available_files, !!obs_param, sqlite_table, date_start, date_end, stations, harp_param$level)
  if (parameter %in% c("AccPcp3h", "AccPcp6h", "AccPcp12h")) {
    message("Deriving 6h precipitation from 12h precipitation")
    obs <- derive_6h_precip(obs, available_files, date_start, date_end, stations)
    if (parameter == "AccPcp12h") {
      message("Deriving 12h precipitation from 6h precipitation")
      obs <- derive_12h_precip(obs)
    } else {
      message("Deriving 3h precipitation from 6h precipitation")
      obs <- derive_3h_precip(obs)
    }
    obs <- obs %>%
      dplyr::select(.data$validdate, .data$SID, !! obs_param) %>%
      tidyr::drop_na()
  }


  obs_column <- rlang::sym(parameter)
  if (gross_error_check) {
    if (is.null(min_allowed)) min_allowed <- get_min_obs_allowed(parameter)
    if (is.null(max_allowed)) max_allowed <- get_max_obs_allowed(parameter)
    obs_removed <- dplyr::filter(obs, !dplyr::between(!! obs_column, min_allowed, max_allowed))
    obs         <- dplyr::filter(obs, dplyr::between(!! obs_column, min_allowed, max_allowed))
  } else {
    obs_removed = "No gross error check done."
  }

  attr(obs, "bad_obs") <- obs_removed
  obs
}



### Functions

# Read data from a set of obstable files
read_obstable <- function(files, .obs_param, .sqlite_table, .date_start, .date_end, .stations, .level = NULL) {

  obs_param_quo  <- rlang::enquo(.obs_param)
  obs_param_name <- rlang::quo_name(obs_param_quo)

  .obs         <- list()
  list_counter <- 0

  for (in_file in files) {
    list_counter <- list_counter + 1

    obs_db <- DBI::dbConnect(RSQLite::SQLite(), in_file, flags = RSQLite::SQLITE_RO, synchronous = NULL)

    message("\nReading: ", in_file)
    if (.sqlite_table == "SYNOP") {
      .obs[[list_counter]] <- dplyr::tbl(obs_db, .sqlite_table) %>%
        dplyr::select(.data$validdate, .data$SID, .data$lon, .data$lat, .data$elev, !!obs_param_quo) %>%
        dplyr::filter(.data$validdate >= .date_start & .data$validdate <= .date_end)
        if (DBI::dbExistsTable(obs_db, paste0(.sqlite_table, "_params"))) {
          .obs_units <- dplyr::tbl(obs_db, paste0(.sqlite_table, "_params")) %>%
            dplyr::filter(.data$parameter == obs_param_name) %>%
            dplyr::pull(.data$units)
        } else {
          .obs_units <- NULL
        }
    } else {
      .obs[[list_counter]] <- dplyr::tbl(obs_db, .sqlite_table) %>%
        dplyr::select(.data$validdate, .data$SID, .data$lon, .data$lat, .data$elev, .data$p, !!obs_param_quo) %>%
        dplyr::filter(.data$validdate >= .date_start & .data$validdate <= .date_end) %>%
        dplyr::filter(.data$p == .level)
        if (DBI::dbExistsTable(obs_db, paste0(.sqlite_table, "_params"))) {
          .obs_units <- dplyr::tbl(obs_db, paste0(.sqlite_table, "_params")) %>%
            dplyr::filter(.data$parameter == obs_param_name) %>%
            dplyr::pull(.data$units) %>%
            unique()
        } else {
          .obs_units <- NULL
        }
    }

    if (!is.null(.stations)) {
      .obs[[list_counter]] <- .obs[[list_counter]] %>%
        dplyr::filter(.data$SID %in% .stations)
    }

    .obs[[list_counter]] <- .obs[[list_counter]] %>%
      dplyr::collect(n = Inf) %>%
      tidyr::drop_na()

    if (length(.obs_units) > 0) {
      .obs[[list_counter]] <- .obs[[list_counter]] %>%
        dplyr::mutate(units = .obs_units)
    } else {
      .obs[[list_counter]] <- .obs[[list_counter]] %>%
        dplyr::mutate(units = guess_units(.obs[[list_counter]], obs_param_name))
    }

    DBI::dbDisconnect(obs_db)
  }

  .obs <- dplyr::bind_rows(.obs)

  if (.sqlite_table == "TEMP" && !is.null(.level)) {
    param_name <- rlang::sym(paste0(rlang::quo_name(obs_param_quo), .level))
    .obs       <- dplyr::rename(.obs, !! param_name := !! obs_param_quo)
  }

  .obs

}

# Derive 6h precipitation from 12h precipitation
derive_6h_precip <- function(pcp_data, obs_files, first_date, last_date, station_ids) {

  pcp_AccPcp6h  <- NULL
  pcp_AccPcp12h <- NULL

  pcp_in_data   <- grep("AccPcp*[[:digit:]]", names(pcp_data), perl = TRUE, value = TRUE)
  switch(
    pcp_in_data,
    "AccPcp12h" = {acc <- "AccPcp6h"; assign(paste0("pcp_", pcp_in_data), pcp_data)},
    "AccPcp6h"  = {acc <- "AccPcp12h"; assign(paste0("pcp_", pcp_in_data), pcp_data)},
    "AccPcp3h"  = acc  <- c("AccPcp6h", "AccPcp12h")
  )

  for (pcp_acc in acc) {
    message("Getting ", pcp_acc, " observations.")
    acc_sym <- rlang::sym(pcp_acc)
    assign(
      paste0("pcp_", pcp_acc),
      read_obstable(obs_files, !!acc_sym, "SYNOP", first_date, last_date, station_ids)
    )
  }

  pcp_AccPcp6h <- pcp_AccPcp6h %>%
    dplyr::full_join(
      dplyr::transmute(
        pcp_AccPcp6h,
        validdate = .data$validdate + 3600 * 6,
        .data$SID,
        AccPcp6h_lag = .data$AccPcp6h
      )
    ) %>%
    dplyr::full_join(pcp_AccPcp12h) %>%
    dplyr::mutate(
      AccPcp6h = dplyr::case_when(
        is.na(.data$AccPcp6h) ~ (.data$AccPcp12h - .data$AccPcp6h_lag),
        TRUE                  ~ .data$AccPcp6h
      )
    ) %>%
    dplyr::select(-.data$AccPcp6h_lag)

  dplyr::full_join(pcp_data, pcp_AccPcp6h)

}

derive_12h_precip <- function(pcp_data) {
  pcp_data %>%
  dplyr::full_join(
    dplyr::transmute(
      pcp_data,
      validdate = .data$validdate + 3600 * 6,
      .data$SID,
      AccPcp6h_lag = .data$AccPcp6h
    )
  ) %>% dplyr::mutate(
    AccPcp12h = dplyr::case_when(
      is.na(.data$AccPcp12h) ~ .data$AccPcp6h + .data$AccPcp6h_lag,
      TRUE                   ~ .data$AccPcp12h
    )
  )
}

derive_3h_precip <- function(pcp_data) {
  pcp_data %>%
  dplyr::full_join(
    dplyr::transmute(
      pcp_data,
      validdate = .data$validdate + 3600 * 3,
      .data$SID,
      AccPcp3h_lag = .data$AccPcp3h
    )
  ) %>% dplyr::mutate(
    AccPcp3h = dplyr::case_when(
      is.na(.data$AccPcp3h) ~ .data$AccPcp6h - .data$AccPcp3h_lag,
      TRUE                  ~ .data$AccPcp3h
    )
  )
}

