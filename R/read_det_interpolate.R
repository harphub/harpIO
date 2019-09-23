#' Read deterministic forecast files and interpolate to stations.
#'
#' @param start_date Date of the first forecast to read.
#' @param end_date Date of the last forecast to read.
#' @param det_model The name of the deterministic model. Maybe expressed as a
#'   vector if more than one model is wanted.
#' @param parameter The parameters to read as a character vector. For reading
#'   from vfld files, set to NULL to read all parameters.
#' @param lead_time The lead times to read as a numeric vector.
#' @param by The time between forecasts. Should be a string of a number followed
#'   by a letter, where the letter gives the units - may be d for days, h for
#'   hours or m for minutes.
#' @param file_path The top level path for the forecast files to read.
#' @param file_format The format of the files to read. Can be "vfld", "grib" or
#'   "netcdf_met"
#' @param file_template The file type to generate the template for. Can be
#'   "harmoneps_grib", "harmeoneps_grib_fp", "harmoneps_grib_sfx", "meps_met",
#'   "harmonie_grib", "harmonie_grib_fp", "harmone_grib_sfx", "vfld", "vobs", or
#'   "fctable". If anything else is passed, it is returned unmodified. In this
#'   case substitutions can be used. Available substitutions are {YYYY} for
#'   year, \{MM\} for 2 digit month with leading zero, \{M\} for month with no
#'   leading zero, and similarly \{DD\} or \{D\} for day, \{HH\} or \{H\} for
#'   hour, \{mm\} or \{m\} for minute. Also \{LDTx\} for lead time and \{MBRx\}
#'   for ensemble member where x is the length of the string including leading
#'   zeros - can be omitted or 2, 3 or 4. Note that the full path to the file
#'   will always be file_path/template.
#' @param clim_file A file containing constant data for the domain: topology,
#'   land/sea mask.
#' @param clim_format The file format of the clim_file may be different than
#'   that of the forecast files.
#' @param stations A data frame of stations with columns SID, lat, lon, elev. If
#'   this is supplied the forecasts are interpolated to these stations. In the
#'   case of vfld files, all stations found in the vfld are used. In the case of
#'   gridded files (e.g. grib, netcdf, FA), if no data frame of stations is
#'   passed a default list of stations is used. This list can be accessed via
#'   \code{station_list}.
#' @param correct_T2m Whether to correct the 2m temperature forecast from the
#'   model elevation to the observation elevation.
#' @param interpolation_method The method used for interpolating from forecast
#'   grid to station points. Default is "closest" (mearest neighbour).
#'   Alternatives include "bilin".
#' @param use_mask If TRUE, a land/sea mask is used when interpolating. It must
#'   be available in the forecast files or in a clim_file.
#' @param sqlite_path If specified, SQLite files are generated and written to
#'   this directory.
#' @param ... Arguments dependent on \code{file_format} (More info to be added).
#' @param keep_model_t2m
#' @param lapse_rate
#' @param sqlite_template
#' @param return_data

#'
#' @return A tibble with columns eps_model, sub_model, fcdate, lead_time,
#'   member, SID, lat, lon, <parameter>.
#' @export
#'
#' @examples
#'
read_det_interpolate <- function(
  start_date,
  end_date,
  det_model,
  parameter,
  lead_time            = seq(0, 48, 3),
  by                   = "6h",
  file_path            = "",
  file_format          = "vfld",
  file_template        = "vfld",
  stations             = NULL,
  correct_t2m          = TRUE,
  keep_model_t2m       = FALSE,
  lapse_rate           = 0.0065,
  vertical_coordinate  = c("pressure", "model", "height", NA),
  clim_file            = NULL,
  clim_format          = NULL,
  interpolation_method = "closest",
  use_mask             = FALSE,
  sqlite_path          = NULL,
  sqlite_template      = "fctable_det",
  sqlite_synchronous   = c("off", "normal", "full", "extra"),
  sqlite_journal_mode  = c("delete", "truncate", "persist", "memory", "wal", "off"),
  return_data          = FALSE,
  ...
) {

  if (any(is.na(vertical_coordinate))) vertical_coordinate <- as.character(vertical_coordinate)
  vertical_coordinate <- match.arg(vertical_coordinate)
  sqlite_synchronous  <- match.arg(sqlite_synchronous)
  sqlite_journal_mode <- match.arg(sqlite_journal_mode)

  # Loop over dates to prevent excessive data volumes in memory

  all_dates <- seq_dates(start_date, end_date, by)

  if (return_data) {
    function_output <- list()
    list_counter    <- 0
  }

  # initialise interpolation weights
  # if no clim file given, use something from data_files
  # find first existing file (if none: give an error)
  # use that to get domain
  # TODO: maybe for GRIB, we would want to pass a FA climfile for initialisation?
  #       so should we use the same file_format?

  if (is.null(stations)) {
    warning(
      "No stations specified. Default station list used.",
      call.      = FALSE,
      immediate. = TRUE
    )
    stations <- get("station_list")
  }

  if (!is.null(clim_file)) {

    message("Initialising interpolation.")
    init <- initialise_interpolation(
      file_format = clim_format,
      clim_file   = clim_file,
      correct_t2m = correct_t2m,
      method      = interpolation_method,
      use_mask    = use_mask,
      stations    = stations
    )

  } else {
    # just leave it uninitialised for now
    init <- list(stations = stations)
  }

  for (fcst_date in all_dates) {

    if (return_data) list_counter <- list_counter + 1

    # Get the file names NEEED TO TEST FROM HERE!

    message("Generating file names.")

    data_files <- purrr::map2(
      det_model, file_template,
      ~ get_filenames(
        file_path      = file_path,
        start_date     = fcst_date,
        end_date       = fcst_date,
        by             = by,
        parameter      = parameter,
        det_model      = .x,
        lead_time      = lead_time,
        file_template  = .y,
        filenames_only = FALSE
      )
    ) %>%
      dplyr::bind_rows()

    # Get the data

    message("Reading data.")

    read_function <- get(paste("read", file_format, "interpolate", sep = "_"))
    forecast_data <- data_files %>%
      dplyr::mutate(
        fcdate = str_datetime_to_unixtime(.data$fcdate)
      ) %>%
      dplyr::mutate(validdate = .data$fcdate + .data$lead_time * 3600) %>%
      dplyr::group_by(.data$file_name)
    if (tidyr_new_interface()) {
      forecast_data <- tidyr::nest(forecast_data, metadata = -tidyr::one_of("file_name"))
    } else {
      forecast_data <- tidyr::nest(forecast_data, .key = "metadata")
    }
    forecast_data <- forecast_data %>%
      dplyr::mutate(
        forecast = purrr::map2(
          .data$file_name,
          .data$metadata,
          function(x, y) read_function(
            file_name           = x,
            parameter           = parameter,
            lead_time           = y$lead_time,
            vertical_coordinate = vertical_coordinate,
            init                = init,
            ...
          )
        )
      )

    # Remove empty rows and join the forecast data to the metadata

    param_units <- purrr::map_dfr(forecast_data$forecast, "units") %>%
      dplyr::distinct()

    message("Joining data.")

    forecast_data <- forecast_data %>%
      dplyr::mutate(forecast = purrr::map(.data$forecast, "fcst_data")) %>%
      dplyr::filter(purrr::map_lgl(.data$forecast, ~ !is.null(.x)))

    if (nrow(forecast_data) < 1) next

    forecast_data <- purrr::map2_df(
      forecast_data$metadata,
      forecast_data$forecast,
      dplyr::inner_join,
      by = "lead_time"
    ) %>%
      tidyr::drop_na(.data$forecast)

    # Put data into tidy long format and correct T2m if required

    sqlite_params <- unique(forecast_data$parameter)

    if (any(tolower(sqlite_params) == "t2m") && correct_t2m) {

      t2m_df <- forecast_data %>%
        dplyr::filter(tolower(.data$parameter) == "t2m") %>%
        dplyr::inner_join(init$stations, by = "SID", suffix = c("", ".station")) %>%
        dplyr::mutate(
          forecast = .data$forecast + lapse_rate * (.data$model_elevation - .data$elev)
        ) %>%
        dplyr::select(
          -dplyr::contains(".station"),
          -.data$elev,
          -.data$name
        )

      if (keep_model_t2m) {
        forecast_data <- forecast_data %>%
          dplyr::mutate(
            parameter = dplyr::case_when(
              tolower(parameter) == "t2m" ~ paste0(.data$parameter, "_uncorrected"),
              TRUE                        ~ .data$parameter
            )
          )
        t2m_param   <- paste0(param_units$parameter[tolower(param_units$parameter) == "t2m"], "_uncorrected")
        t2m_units   <- param_units$units[tolower(param_units$parameter) == "t2m"]
        param_units <- rbind(param_units, c(t2m_param, t2m_units))
      } else {
        forecast_data <- forecast_data %>%
          dplyr::filter(tolower(.data$parameter) != "t2m")
      }

      forecast_data <- dplyr::bind_rows(forecast_data, t2m_df)

    }

    forecast_data <- forecast_data %>%
      tidyr::drop_na(.data$forecast) %>%
      dplyr::left_join(param_units, by = "parameter")

    # If sqlite_path is passed write data to sqlite files

    if (!is.null(sqlite_path)) {

      message("Preparing data to write.")
      group_cols <- c("fcdate", "parameter", "lead_time", "det_model")
      if (tidyr_new_interface()) {
        sqlite_data <- tidyr::nest(forecast_data, data = -tidyr::one_of(group_cols))
      } else {
        sqlite_data <- forecast_data %>%
          dplyr::group_by(!!!rlang::syms(group_cols)) %>%
          tidyr::nest()
      }
      sqlite_data <- sqlite_data %>%
        dplyr::mutate(
          file_path = sqlite_path,
          YYYY      = unixtime_to_str_datetime(.data$fcdate, lubridate::year),
          MM        = formatC(unixtime_to_str_datetime(.data$fcdate, lubridate::month), width = 2, flag = "0"),
          HH        = formatC(unixtime_to_str_datetime(.data$fcdate, lubridate::hour), width = 2, flag = "0"),
          LDT3      = formatC(lead_time, width = 3, flag = "0")
        )

      sqlite_data <- sqlite_data %>%
        dplyr::mutate(
          file_name = as.vector(glue::glue_data(sqlite_data, get_template(sqlite_template)))
        )
      if (tidyr_new_interface()) {
        sqlite_data <- tidyr::unnest(sqlite_data, tidyr::one_of("data"))
      } else {
        sqlite_data <- tidyr::unnest(sqlite_data)
      }

      sqlite_primary_key <- c("fcdate", "leadtime", "SID")

      fixed_trasmute_cols <- c("SID", "lat", "lon")
      if (is.element("model_elevation", colnames(forecast_data))) {
        fixed_trasmute_cols <- c(fixed_trasmute_cols, "model_elevation")
      }

      vertical_coordinate_colnames <- c("p", "ml", "z")
      vertical_coordinate_col      <- which(vertical_coordinate_colnames %in% colnames(forecast_data))
      if (length(vertical_coordinate_col) > 0) {
        fixed_trasmute_cols <- c(fixed_trasmute_cols, vertical_coordinate_colnames[vertical_coordinate_col])
        sqlite_primary_key  <- c(sqlite_primary_key, vertical_coordinate_colnames[vertical_coordinate_col])
      }

      fixed_trasmute_cols <- rlang::syms(fixed_trasmute_cols)

      sqlite_data <- sqlite_data %>%
        dplyr::transmute(
          !!! fixed_trasmute_cols,
          fcdate    = as.integer(.data$fcdate),
          leadtime  = .data$lead_time,
          validdate = as.integer(.data$validdate),
          member    = paste(.data$det_model, "det", sep = "_"),
          .data$forecast,
          .data$parameter,
          .data$units,
          .data$file_name
        ) %>%
        dplyr::group_by(.data$file_name)
      if (tidyr_new_interface()) {
        sqlite_data <- tidyr::nest(sqlite_data, data = -tidyr::one_of("file_name"))
      } else {
        sqlite_data <- tidyr::nest(sqlite_data)
      }

      purrr::walk2(
        sqlite_data$data,
        sqlite_data$file_name,
        write_fctable_to_sqlite,
        primary_key  = sqlite_primary_key,
        synchronous  = sqlite_synchronous,
        journal_mode = sqlite_journal_mode
      )

    }

    if (return_data) function_output[[list_counter]] <- dplyr::select(forecast_data, -.data$member)

  }

  if (return_data) dplyr::bind_rows(function_output)

}
