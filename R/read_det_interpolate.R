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
#' @param stations A data frame of stations with columns SID, lat, lon, elev. If
#'   this is supplied the forecasts are interpolated to these stations. In the
#'   case of vfld files, all stations found in the vfld are used. In the case of
#'   gridded files (e.g. grib, netcdf, FA), if no data frame of stations is
#'   passed a default list of stations is used. This list can be accessed via
#'   \code{station_list}.
#' @param correct_T2m Whether to correct the 2m temperature forecast from the
#'   model elevation to the observation elevation.
#' @param sqlite_path If specified, SQLite files are generated and written to
#'   this directory.
#' @param ... Arguments dependent on \code{file_format}. (More info to be
#'   added).
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
  lead_time      = seq(0, 48, 3),
  by             = "6h",
  file_path      = "",
  file_format    = "vfld",
  file_template  = "vfld",
  stations       = NULL,
  correct_t2m    = TRUE,
  keep_model_t2m = FALSE,
  lapse_rate     = 0.0065,
  sqlite_path    = NULL,
  return_data    = FALSE,
  ...
) {

  # Loop over dates to prevent excessive data volumes in memory

  all_dates <- seq_dates(start_date, end_date, by)

  if (return_data) {
    function_output <- list()
    list_counter    <- 0
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
      dplyr::mutate(validdate = fcdate + lead_time * 3600) %>%
      dplyr::group_by(file_name) %>%
      tidyr::nest(.key = "metadata") %>%
      dplyr::mutate(
        forecast = purrr::map2(
          .data$file_name,
          .data$metadata,
          function(x, y) read_function(
            file_name   = x,
            parameter   = parameter,
            lead_time   = y$lead_time,
            stations    = stations,
            is_ensemble = FALSE,
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

    forecast_data <- purrr::map2_df(
      forecast_data$metadata,
      forecast_data$forecast,
      dplyr::inner_join,
      by = "lead_time"
    )

    # Put data into tidy long format and correct T2m if required

    sqlite_params <- forecast_data[1,] %>%
      dplyr::select(
        -dplyr::contains("model"),
        -dplyr::contains("lead"),
        -dplyr::contains("date"),
        -dplyr::contains("lat"),
        -dplyr::contains("lon"),
        -dplyr::contains("SID")
      ) %>%
      colnames()
    gather_cols <- rlang::syms(sqlite_params)

    if (any(tolower(sqlite_params) == "t2m") && correct_t2m) {
      t2m_param       <- sqlite_params[which(tolower(sqlite_params) == "t2m")]
      t2m_uncorrected <- paste0(t2m_param, "_uncorrected")
      t2m_col         <- rlang::sym(t2m_param)
      t2m_uc_col      <- rlang::sym(t2m_uncorrected)

      if (is.null(stations)) {
        warning(
          "No stations specified for 2m temperature correction. Default station list used.",
          call. = FALSE
        )
        stations <- station_list
      }

      forecast_data <- forecast_data %>%
        dplyr::inner_join(stations, by = "SID", suffix = c("", ".station")) %>%
        dplyr::mutate(!! t2m_uncorrected := !! t2m_col) %>%
        dplyr::mutate(
          !! t2m_param := !! t2m_uc_col + lapse_rate * (.data$model_elevation - .data$elev)
        ) %>%
        dplyr::select(-dplyr::contains(".station"))

      if (keep_model_t2m) {
        gather_cols <- rlang::syms(c(sqlite_params, t2m_uncorrected))
      } else {
        forecast_data <- dplyr::select(forecast_data, - !! t2m_uc_col)
      }
    }

    forecast_data <- forecast_data %>%
      tidyr::gather(key = "parameter", value = "forecast", !!!gather_cols) %>%
      tidyr::drop_na(.data$forecast) %>%
      dplyr::left_join(param_units, by = "parameter")

    # If sqlite_path is passed write data to sqlite files

    if (!is.null(sqlite_path)) {

      message("Writing data.")
      sqlite_data <- forecast_data %>%
        dplyr::group_by(.data$fcdate, .data$parameter, .data$lead_time, .data$det_model) %>%
        tidyr::nest() %>%
        dplyr::mutate(
          file_path = sqlite_path,
          YYYY      = unixtime_to_str_datetime(fcdate, lubridate::year),
          MM        = formatC(unixtime_to_str_datetime(fcdate, lubridate::month), width = 2, flag = "0"),
          HH        = formatC(unixtime_to_str_datetime(fcdate, lubridate::hour), width = 2, flag = "0"),
          LDT3      = formatC(lead_time, width = 3, flag = "0")
        ) %>%
        dplyr::mutate(
          file_name = purrr::map_chr(
            purrr::transpose(.),
            glue::glue_data,
            get_template("fctable_det")
          )
        ) %>%
        tidyr::unnest()

      sqlite_data <- sqlite_data %>%
        dplyr::transmute(
          .data$SID,
          .data$fcdate,
          leadtime = .data$lead_time,
          .data$validdate,
          member   = paste(.data$det_model, "det", sep = "_"),
          .data$forecast,
          .data$parameter,
          .data$units,
          .data$file_name
        ) %>%
        dplyr::group_by(.data$file_name) %>%
        tidyr::nest()

      purrr::walk2(sqlite_data$data, sqlite_data$file_name, write_fctable_to_sqlite)

    }

    if (return_data) function_output[[list_counter]] <- forecast_data

  }

  if (return_data) dplyr::bind_rows(function_output)

}
