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
#' @param vertical_coordinate If upper air for multiple levels are to be read,
#'   the vertical coordinate of the data is given here. The default is
#'   "pressure", but can also be "model" for model levels, or "height" for
#'   height above ground /sea level.
#'
#' @return A tibble with columns for validdate, SID and the parameter.
#' @export
#'
#' @examples
#' if (requireNamespace("harpData", quietly = TRUE)) {
#'   read_point_obs(
#'     2019021700,
#'     2019022023,
#'     "T2m",
#'     obs_path = system.file("OBSTABLE", package = "harpData")
#'   )
#'
#'   # stations can be specified using a vector of station ID numbers
#'   read_point_obs(
#'     2019021700,
#'     2019022023,
#'     "T2m",
#'     obs_path = system.file("OBSTABLE", package = "harpData"),
#'     stations = c(1001, 1010)
#'   )
#'
#'   # Gross error checks are done automatically but the allowable values
#'   # can be changed with min_allowed and max_allowed.
#'   obs <- read_point_obs(
#'     2019021700,
#'     2019022023,
#'     "T2m",
#'     obs_path = system.file("OBSTABLE", package = "harpData"),
#'     min_allowed = 260,
#'     max_allowed = 280
#'   )
#'
#'   # The removed observations are stored in the attribute "bad_obs"
#'   attr(obs, "bad_obs")
#'
#'   # For vertical profiles, the vertical coordinate must be specified
#'   read_point_obs(
#'     2019021700,
#'     2019022023,
#'     "Z",
#'     obs_path            = system.file("OBSTABLE", package = "harpData"),
#'     vertical_coordinate = "pressure"
#'   )
#' }
#'
read_point_obs <- function(
  start_date,
  end_date,
  parameter,
  obs_path            = ".",
  obsfile_template    = "obstable",
  gross_error_check   = TRUE,
  min_allowed         = NULL,
  max_allowed         = NULL,
  stations            = NULL,
  vertical_coordinate = c(NA_character_, "pressure", "model", "height")
) {

  vertical_coordinate <- match.arg(vertical_coordinate)

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

  harp_param <- parse_harp_parameter(parameter, vertical_coordinate)
  if (!is.null(harp_param$level_type) && is_temp(harp_param)) {
    sqlite_table   <- "TEMP"
    obs_param      <- rlang::sym(harp_param$basename)
    vertical_level <- harp_param$level
    level_col      <- switch(
      harp_param$level_type,
      "pressure" = "p",
      "model"    = "ml",
      "height"   = "z"
    )
  } else {
    sqlite_table   <- "SYNOP"
    obs_param      <- rlang::sym(parameter)
    vertical_level <- NULL
    level_col      <- NULL
  }

  message("Getting ", parameter, " observations for ", start_date, "-", end_date)
  obs <- read_obstable(
    available_files,
    !!obs_param,
    sqlite_table,
    date_start,
    date_end,
    stations,
    level_col,
    vertical_level
  )

  if (
    parameter %in% c("AccPcp3h", "AccPcp6h", "AccPcp12h") &&
      any(grepl("AccPcp3h|AccPcp6h|AccPcp12h", colnames(obs)))
    ) {

    metadata_cols <- rlang::syms(colnames(obs)[colnames(obs) != parameter])
    message("Deriving 6h precipitation from 12h precipitation")
    obs <- derive_6h_precip(obs, available_files, date_start, date_end, stations)
    if (parameter == "AccPcp12h") {
      message("Deriving 12h precipitation from 6h precipitation")
      obs <- derive_12h_precip(obs)
    }
    if (parameter == "AccPcp3h") {
      message("Deriving 3h precipitation from 6h precipitation")
      obs <- derive_3h_precip(obs)
    }
    obs <- obs %>%
      dplyr::select(!!! metadata_cols, !! obs_param) %>%
      tidyr::drop_na()
  }

  if (gross_error_check && nrow(obs) > 0) {
    if (is.element("units", colnames(obs))) {
      param_units <- unique(obs$units)
    } else {
      param_units <- ""
    }
    if (is.null(min_allowed)) min_allowed <- get_min_obs_allowed(parameter, param_units)
    if (is.null(max_allowed)) max_allowed <- get_max_obs_allowed(parameter, param_units)
    obs_removed <- dplyr::filter(
      obs, !dplyr::between(!! obs_param, min_allowed, max_allowed)
    ) %>%
      dplyr::mutate(validdate = unix2datetime(.data[["validdate"]]))
    obs         <- dplyr::filter(obs, dplyr::between(!! obs_param, min_allowed, max_allowed))
  } else {
    if (nrow(obs) > 0) {
      obs_removed = "No gross error check done"
    } else {
      obs_removed = "No observations found"
    }
  }

  attr(obs, "bad_obs") <- obs_removed
  colnames(obs)[colnames(obs) == harp_param[["basename"]]] <- harp_param[["fullname"]]

  if (nrow(obs_removed) > 0) {
    warning(
      nrow(obs_removed), " observations removed due to gross error check.",
      call. = FALSE
    )
  }

  dplyr::mutate(obs, validdate = unix2datetime(.data[["validdate"]]))
}



### Functions

# Read data from a set of obstable files
read_obstable <- function(
  files,
  .obs_param,
  .sqlite_table,
  .date_start,
  .date_end,
  .stations,
  .level_col = NULL,
  .level     = NULL
) {

  obs_param_quo  <- rlang::enquo(.obs_param)
  obs_param_name <- rlang::quo_name(obs_param_quo)

  .obs         <- list()
  list_counter <- 0

  for (in_file in files) {
    list_counter <- list_counter + 1

    obs_db <- DBI::dbConnect(RSQLite::SQLite(), in_file, flags = RSQLite::SQLITE_RO, synchronous = NULL)

    message("\nReading: ", in_file)
    if (.sqlite_table == "SYNOP") {
      obstable <- dplyr::tbl(obs_db, .sqlite_table)
      if (!is.element(obs_param_name, colnames(obstable))) {
        warning("'", obs_param_name, "' not found.", call. = FALSE, immediate. = TRUE)
        .obs[[list_counter]] <- NULL
        DBI::dbDisconnect(obs_db)
        next()
      }
      .obs[[list_counter]] <- obstable %>%
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
      obstable <- dplyr::tbl(obs_db, .sqlite_table)
      if (!is.element(obs_param_name, colnames(obstable))) {
        warning("'", obs_param_name, "' not found.", call. = FALSE, immediate. = TRUE)
        .obs[[list_counter]] <- NULL
        DBI::dbDisconnect(obs_db)
        next()
      }
      if (!is.element(.level_col, colnames(obstable))) {
        warning("'", .level_col, "' column for vertical coordinate not found.", call. = FALSE, immediate. = TRUE)
        .obs[[list_counter]] <- NULL
        DBI::dbDisconnect(obs_db)
        next()
      }
      .obs[[list_counter]] <- obstable %>%
        dplyr::select(.data$validdate, .data$SID, .data$lon, .data$lat, .data$elev, .data[[.level_col]], !!obs_param_quo) %>%
        dplyr::filter(.data$validdate >= .date_start & .data$validdate <= .date_end)
      if (.level != -999) {
        .obs[[list_counter]] <- dplyr::filter(.obs[[list_counter]], .data[[.level_col]] == .level)
      }
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

  dplyr::bind_rows(.obs)

}

# Derive 6h precipitation from 12h precipitation
derive_6h_precip <- function(pcp_data, obs_files, first_date, last_date, station_ids) {

  pcp_AccPcp6h  <- NULL
  pcp_AccPcp12h <- NULL

  first_date    <- first_date - 3600 * 12

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
        .data$lon,
        .data$lat,
        .data$elev,
        .data$units,
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
        .data$lon,
        .data$lat,
        .data$elev,
        .data$units,
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

