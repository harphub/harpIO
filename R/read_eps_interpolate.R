#' Read EPS forecast files and interpolate to stations.
#'
#' @param start_date Date of the first forecast to read.
#' @param end_date Date of the last forecast to read.
#' @param eps_model The name of the EPS model. Maybe expressed as a vector if
#'   more than one EPS model is wanted, or a list for multimodel EPS.
#' @param parameter The parametrs to read as a character vector.
#' @param lead_time The lead times to read as a numeric vector.
#' @param members_in The input member numbers. If only one EPS is set in
#'   \code{eps_model} then this is a vector. If more than one EPS is set in
#'   \code{eps_model}, or a multimodel EPS is wanted, then this is a list. See
#'   the vignette for more details.
#' @param members_out The ouput member numbers. Must be the same form as
#'   members_in. If not passed, members_out is set to the same as members_in.
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
#'   case of vfld files, the common stations between the vfld files and the
#'   stations in this data frame are selected. In the case of gridded files
#'   (e.g. grib, netcdf, FA), if no data frame of stations is passed a default
#'   list of stations is used. This list can be accessed via
#'   \code{data(stations)}.
#' @param correct_T2m Whether to correct the 2m temperature forecast from the
#'   model elevation to the observation elevation.
#' @param sqlite_path If specified, SQLite files are generated and written to
#'   this directory.
#'
#' @return A tibble with columns eps_model, sub_model, fcdate, lead_time,
#'   member, SID, lat, lon, <parameter>.
#' @export
#'
#' @examples
#'
read_eps_interpolate <- function(
  start_date,
  end_date,
  eps_model,
  parameter,
  lead_time      = seq(0, 48, 3),
  members_in     = seq(0,9),
  members_out    = members_in,
  by             = "6h",
  file_path      = "",
  file_format    = "vfld",
  file_template  = "vfld",
  stations       = NULL,
  correct_t2m    = TRUE,
  keep_model_t2m = FALSE,
  lapse_rate     = 0.0065,
  sqlite_path    = NULL
) {

  # Sanity checks and organisation of members_in as a list

  if (is.list(eps_model)) {

    multimodel <- TRUE
    eps_models <- names(eps_model)

    if (!is.list(members_in) | !identical(eps_models, names(members_in))) {
      stop(
        paste(
          "For multimodel, members_in must be a list with the",
          "same names as in the eps_model argument.",
          sep = "\n  "
        )
      )
    }

    for (eps in eps_models) {
      if (!identical(eps_model[[eps]], names(members_in[[eps]]))) {
        stop(
          paste(
            "Model names specified in members_in do not match those in eps_model.",
            paste0("eps_model = ", eps, ": ", paste0(eps_model[[eps]], collapse = ", ")),
            paste0("members_in = ", eps, ": ", paste0(names(members_in[[eps]]), collapse = ", ")),
            sep = "\n  "
          )
        )
      }
    }

    for (eps in eps_models) {
      if (!identical(names(members_out[[eps]]), names(members_in[[eps]]))) {
        stop(
          paste(
            "Model names specified in members_out do not match those in members_in.",
            paste0("members_in = ", eps, ": ", paste0(names(members_in[[eps]]), collapse = ", ")),
            paste0("members_out = ", eps, ": ", paste0(names(members_out[[eps]]), collapse = ", ")),
            sep = "\n "
          )
        )
      }
      for (sub_eps in names(members_in[[eps]])) {
        if (length(members_out[[eps]][[sub_eps]]) != length(members_in[[eps]][[sub_eps]])) {
          stop(
            paste(
              "Number of members specified in members_out is not the same as in members_in.",
              paste0("members_in = ", eps, ": ", sub_eps, ": ", length(members_in[[eps]][[sub_eps]]), " members"),
              paste0("members_out = ", eps, ": ", sub_eps, ": ", length(members_out[[eps]][[sub_eps]]), " members"),
              sep = "\n "
            )
          )
        }
      }
    }

  } else {

    multimodel <- FALSE
    eps_models <- eps_model

    if (length(eps_models) > 1) {

      if (!is.list(members_in) |
          !is.list(members_out) |
          !identical(eps_models, names(members_in)) |
          !identical(eps_models, names(members_out))
        ) {
        stop(
          paste(
            "If more than one eps_model is specified, the members must",
            "be passed as a named list with the names as those specified",
            "in eps_model",
            sep = "\n  "
          )
        )
      }

    } else {

      if (!is.list(members_in)) {
        members_temp <- list()
        members_temp[[eps_models]] <- members_in
        members_in <- members_temp
      }
      if (!is.list(members_out)) {
        members_temp <- list()
        members_temp[[eps_models]] <- members_out
        members_out <- members_temp
      }
      if (!identical(eps_models, names(members_in)) | !identical(eps_models, names(members_out))) {
        stop(
          paste(
            "If specifying members as a named list for a single eps, the",
            "name in the list for members_in must match that specified",
            "in eps_model.",
            sep = "\n  "
          )
        )
      }

    }

    members_in_temp <- list()
    members_out_temp <- list()
    for (eps in eps_models) {
      if (length(members_out[[eps]]) != length(members_in[[eps]])) {
        stop(
          paste(
            "Number of members specified in members_out is not the same as in members_in.",
            paste0("members_in = ", eps, ": ", length(members_in[[eps]]), " members"),
            paste0("members_out = ", eps, ": ", length(members_out[[eps]]), " members"),
            sep = "\n "
          )
        )
      }
      members_in_temp[[eps]]         <- list()
      members_out_temp[[eps]]        <- list()
      members_in_temp[[eps]][[eps]]  <- members_in[[eps]]
      members_out_temp[[eps]][[eps]] <- members_out[[eps]]
    }
    members_in  <- members_in_temp
    members_out <- members_out_temp

  } # end of input checks

  ########################### THE ACTUAL WORK STARTS HERE! ##################################

  # Convert members_in to a tibble for easier manipulation

  members_in <- tibble::tibble(
    eps_model = names(members_in)
  ) %>%
    dplyr::mutate(sub_model = purrr::map(members_in, names)) %>%
    dplyr::mutate(
      member      = purrr::modify_depth(members_in, 2, `[`),
      members_out = purrr::modify_depth(members_out, 2, `[`)
    ) %>%
    tidyr::unnest()

  # Get the file names

  message("Generating file names.")

  data_files <- members_in %>%
    dplyr::transmute(
      file_names = purrr::pmap(
        list(eps_model = .data$eps_model, sub_model = .data$sub_model, members = .data$member),
        function(eps_model, sub_model, members) get_filenames(
          file_path     = file_path,
          start_date    = start_date,
          end_date      = end_date,
          by            = by,
          parameter     = parameter,
          eps_model     = eps_model,
          sub_model     = sub_model,
          lead_time     = lead_time,
          members       = members,
          file_template = file_template
        )
      )
    ) %>%
    tidyr::unnest() %>%
    dplyr::left_join(tidyr::unnest(members_in), by = c("eps_model", "sub_model", "member"))

  # Get the data

  message("Reading data.")

  read_function <- get(paste("read", file_format, "interpolate", sep = "_"))
  forecast_data <- data_files %>%
    dplyr::mutate(
      fcdate = str_datetime_to_unixtime(.data$fcdate),
      member = paste0("mbr", formatC(.data$member, width = 3, flag = "0"))
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
          members     = y$member,
          lead_time   = y$lead_time,
          stations    = stations
        )
      )
    )

  # Remove empty rows and join the forecast data to the metadata

  message("Joining data.")

  forecast_data <- forecast_data %>%
    dplyr::filter(purrr::map_lgl(.data$forecast, ~ !is.null(.x)))

  forecast_data <- purrr::map2_df(
    forecast_data$metadata,
    forecast_data$forecast,
    dplyr::inner_join,
    by = c("lead_time", "member")
  ) %>%
    dplyr::mutate(
      members_out = paste0("mbr", formatC(.data$members_out, width = 3, flag = "0"))
    )

  # Put data into tidy long format and correct T2m if required

  sqlite_params <- forecast_data[1,] %>%
    dplyr::select(
      -dplyr::contains("model"),
      -dplyr::contains("lead"),
      -dplyr::contains("date"),
      -dplyr::contains("lat"),
      -dplyr::contains("lon"),
      -dplyr::contains("member"),
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
      warning("No stations passed for 2m temperature correction. Using default stations")
      stations <- station_list
    }

    forecast_data <- forecast_data %>%
      dplyr::inner_join(station_list, by = "SID", suffix = c("", ".station")) %>%
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
    tidyr::drop_na(.data$forecast)

# If sqlite_path is passed write data to sqlite files

  if (!is.null(sqlite_path)) {

    message("Writing data.")
    sqlite_data <- forecast_data %>%
      dplyr::group_by(.data$fcdate, .data$parameter, .data$lead_time, .data$eps_model) %>%
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
          get_template("fctable")
        )
      ) %>%
      tidyr::unnest()

    sqlite_data <- sqlite_data %>%
      dplyr::transmute(
        .data$SID,
        .data$fcdate,
        leadtime = .data$lead_time,
        .data$validdate,
        member   = paste(.data$sub_model, .data$members_out, sep = "_"),
        .data$forecast,
        .data$file_name
      ) %>%
      dplyr::group_by(.data$file_name) %>%
      tidyr::nest()

    purrr::walk2(sqlite_data$data, sqlite_data$file_name, write_fctable_to_sqlite)

  }

}
