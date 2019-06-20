#' Read EPS forecast files and interpolate to stations.
#'
#' @param start_date Date of the first forecast to read.
#' @param end_date Date of the last forecast to read.
#' @param eps_model The name of the EPS model. Maybe expressed as a vector if
#'   more than one EPS model is wanted, or a list for multimodel EPS.
#' @param parameter The parameters to read as a character vector. For reading
#'   from vfld files, set to NULL to read all parameters.
#' @param lead_time The lead times to read as a numeric vector.
#' @param members_in The input member numbers. If only one EPS is set in
#'   \code{eps_model} then this is a vector. If more than one EPS is set in
#'   \code{eps_model}, or a multimodel EPS is wanted, then this is a list. See
#'   the vignette for more details.
#' @param members_out The ouput member numbers. Must be the same form as
#'   members_in. If not passed, members_out is set to the same as members_in.
#' @param lags For reading files from a lagged forecast with members run at
#'   different times, the lag times are set here. The times are expressed as a
#'   character vector, or a list of character vectors in the case of multi model
#'   ensembles, with a number followed by a letter giving the units. The
#'   avialable units are d, h, m, s for days, hours, minutes and seconds. The
#'   lags argument, if not set to NULL must have exactly the same dimensions as
#'   members_in.
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

#' @param clim_file A file containing constant data for the domain:
#'   topology, land/sea mask.
#' @param clim_format The file format of the clim_file may be different
#'   than that of the forecast files.
#' @param correct_T2m Whether to correct the 2m temperature forecast from the
#'   model elevation to the observation elevation.
#' @param interp_method The method used for interpolating from forecast grid
#'   to station points. Default is "closest" (mearest neighbour).
#'   Alternatives include "bilin".
#' @param use_mask If TRUE, a land/sea mask is used when interpolating. It must be
#'   available in the forecast files or in a clim_file.
#' @param sqlite_path If specified, SQLite files are generated and written to
#'   this directory.
#' @param ... Arguments dependent on \code{file_format}. (More info to be
#'   added).
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
read_eps_interpolate <- function(
  start_date,
  end_date,
  eps_model,
  parameter,
  lead_time      = seq(0, 48, 3),
  members_in     = seq(0,9),
  members_out    = members_in,
  lags           = NULL,
  by             = "6h",
  file_path      = "",
  file_format    = "vfld",
  file_template  = "vfld",
  stations       = NULL,
  correct_t2m    = TRUE,
  keep_model_t2m = FALSE,
  lapse_rate     = 0.0065,
  clim_file      = NULL,
  clim_format    = NULL,
  interp_method  = "closest",
  use_mask       = FALSE,
  sqlite_path          = NULL,
  sqlite_template      = "fctable_eps",
  sqlite_synchronous   = c("off", "normal", "full", "extra"),
  sqlite_journal_mode  = c("delete", "truncate", "persist", "memory", "wal", "off"),
  return_data          = FALSE,
  ...
){

  sqlite_synchronous  <- match.arg(sqlite_synchronous)
  sqlite_journal_mode <- match.arg(sqlite_journal_mode)

  # Sanity checks and organisation of members_in as a list

  lags_passed <- !is.null(lags)
  if (!lags_passed) {
    lags <- list()
  }

  if (is.list(eps_model)) {

    multimodel <- TRUE
    eps_models <- names(eps_model)

    if (!is.list(members_in) | !identical(eps_models, names(members_in))) {
      stop(
        paste(
          "For multimodel, members_in must be a list with the",
          "same names as in the eps_model argument.",
          sep = "\n  "
        ),
        call. = FALSE
      )
    }

    if (lags_passed && !is.list(lags)) {
      stop(
        paste(
          "If lags are desired, you must treat as multimodel",
          "with the same names as in the eps_model argument",
          sep = "\n"
        ),
        call. = FALSE
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
          ),
          call. = FALSE
        )
      }

      if (!identical(names(members_out[[eps]]), names(members_in[[eps]]))) {
        stop(
          paste(
            "Model names specified in members_out do not match those in members_in.",
            paste0("members_in  = ", eps, ": ", paste0(names(members_in[[eps]]), collapse = ", ")),
            paste0("members_out = ", eps, ": ", paste0(names(members_out[[eps]]), collapse = ", ")),
            sep = "\n "
          ),
          call. = FALSE
        )
      }

      if (!lags_passed) {
        lags[[eps]] <- list()
      }

      if (lags_passed && !identical(names(lags[[eps]]), names(members_in[[eps]]))) {
        stop(
          paste(
            "Model names specified in lags do not match those in members_in.",
            paste0("members_in = ", eps, ": ", paste0(names(members_in[[eps]]), collapse = ", ")),
            paste0("lags       = ", eps, ": ", paste0(names(lags[[eps]]), collapse = ", ")),
            sep = "\n "
          ),
          call. = FALSE
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
            ),
          call. = FALSE
          )
        }
        if (lags_passed && length(lags[[eps]][[sub_eps]]) != length(members_in[[eps]][[sub_eps]])) {
          stop(
            paste(
              "Number of members specified in lags is not the same as in members_in.",
              paste0("members_in = ", eps, ": ", sub_eps, ": ", length(members_in[[eps]][[sub_eps]]), " members"),
              paste0("lags       = ", eps, ": ", sub_eps, ": ", length(lags[[eps]][[sub_eps]]), " members"),
              sep = "\n "
            ),
          call. = FALSE
          )
        }

        if (!lags_passed) {
          lags[[eps]][[sub_eps]] <- rep(0, length(members_in[[eps]][[sub_eps]]))
        }

      }

    } # end loop over eps_models

  } else {

    multimodel  <- FALSE
    eps_models  <- eps_model

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
          ),
          call. = FALSE
        )
      }

    } else {

      if (!is.list(members_in)) {
        members_temp               <- list()
        members_temp[[eps_models]] <- members_in
        members_in                 <- members_temp
      }
      if (!is.list(members_out)) {
        members_temp               <- list()
        members_temp[[eps_models]] <- members_out
        members_out                <- members_temp
      }
      if (lags_passed && !is.list(lags)) {
        lags_temp              <- list()
        lags_temp[[eps_model]] <- lags
        lags                   <- lags_temp
      }

      if (!identical(eps_models, names(members_in)) | !identical(eps_models, names(members_out))) {
        stop(
          paste(
            "If specifying members as a named list for a single eps, the",
            "name in the list for members_in must match that specified",
            "in eps_model.",
            sep = "\n  "
          ),
          call. = FALSE
        )
      }

      if (lags_passed && !identical(eps_models, names(lags)))  {
        stop(
          paste(
            "If specifying lags as a named list for a single eps, the",
            "name in the list for lags must match that specified",
            "in eps_model.",
            sep = "\n  "
          ),
          call. = FALSE
        )
      }

    }

    members_in_temp  <- list()
    members_out_temp <- list()
    lags_temp        <- list()
    for (eps in eps_models) {
      if (length(members_out[[eps]]) != length(members_in[[eps]])) {
        stop(
          paste(
            "Number of members specified in members_out is not the same as in members_in.",
            paste0("members_in  = ", eps, ": ", length(members_in[[eps]]), " members"),
            paste0("members_out = ", eps, ": ", length(members_out[[eps]]), " members"),
            sep = "\n "
          ),
          call. = FALSE
        )
      }
      if (!lags_passed) {
        lags[[eps]] <- rep("0s", length(members_in[[eps]]))
      }
      if (length(lags[[eps]]) != length(members_in[[eps]])) {
        stop(
          paste(
            "Number of members specified in lags is not the same as in members_in.",
            paste0("members_in = ", eps, ": ", length(members_in[[eps]]), " members"),
            paste0("lags       = ", eps, ": ", length(lags[[eps]]), " members"),
            sep = "\n "
          ),
          call. = FALSE
        )
      }
      members_in_temp[[eps]]         <- list()
      members_out_temp[[eps]]        <- list()
      lags_temp[[eps]]               <- list()
      members_in_temp[[eps]][[eps]]  <- members_in[[eps]]
      members_out_temp[[eps]][[eps]] <- members_out[[eps]]
      lags_temp[[eps]][[eps]]        <- lags[[eps]]
    }
    members_in  <- members_in_temp
    members_out <- members_out_temp
    lags        <- lags_temp

  } # end handling of inputs related to multiple or single models

  if (is.null(stations)) {
    warning(
      "No stations specified. Default station list used.",
      call. = FALSE, immediate. = TRUE
    )
    stations <- get("station_list")
  }

  # initialise interpolation weights
  # if no clim file given, use something from data_files:
  # find first existing file (if none: give an error) and  use that to get domain
  # TODO: maybe for GRIB, we would want to pass a FA climfile for initialisation?
  #       so should we use the same file_format?

  if (!is.null(clim_file)) {
    message("Initialising interpolation.")
    init <- initialise_interpolation(
      file_format = clim_format,
      clim_file   = clim_file,
      correct_t2m = correct_t2m,
      method      = interp_method,
      use_mask    = use_mask,
      stations    = stations,
      is_ensemble = TRUE
    )
  } else {
    # just leave it uninitialised for now
    init <- list(stations = stations, is_ensemble = TRUE)
  }


  ########################### THE ACTUAL WORK STARTS HERE! ##################################

  # Convert members_in to a tibble for easier manipulation

  members_in <- tibble::tibble(
    eps_model = names(members_in)
  ) %>%
    dplyr::mutate(sub_model = purrr::map(members_in, names)) %>%
    dplyr::mutate(
      member      = purrr::modify_depth(members_in, 2, `[`),
      members_out = purrr::modify_depth(members_out, 2, `[`),
      lag         = purrr::modify_depth(lags, 2, `[`)
    ) %>%
    tidyr::unnest()

  # Loop over dates to prevent excessive data volumes in memory

  all_dates <- seq_dates(start_date, end_date, by)

  if (return_data) {
    function_output <- list()
    list_counter    <- 0
  }


  for (fcst_date in all_dates) {

    if (return_data) list_counter <- list_counter + 1

    # Get the file names

    message("Generating file names.")

    data_files <- members_in %>%
      dplyr::transmute(
        file_names = purrr::pmap(
          list(
            eps_model = .data$eps_model,
            sub_model = .data$sub_model,
            members   = .data$member,
            lags      = .data$lag
          ),
          function(eps_model, sub_model, members, lags) get_filenames( # change to lambda function?
            file_path      = file_path,
            start_date     = fcst_date,
            end_date       = fcst_date,
            by             = by,
            lags           = lags,
            parameter      = parameter,
            eps_model      = eps_model,
            sub_model      = sub_model,
            lead_time      = lead_time,
            members        = members,
            file_template  = file_template,
            filenames_only = FALSE
          )
        )
      ) %>%
      tidyr::unnest() %>%
      dplyr::left_join(tidyr::unnest(members_in), by = c("eps_model", "sub_model", "member"))

    # Get the data

    message("Reading data for ", fcst_date)

    read_function <- get(paste("read", file_format, "interpolate", sep = "_"))
    forecast_data <- data_files %>%
      dplyr::mutate(
        fcdate = str_datetime_to_unixtime(.data$fcdate),
        member = paste0("mbr", formatC(.data$member, width = 3, flag = "0"))
      ) %>%
      dplyr::mutate(validdate = fcdate + lead_time * 3600) %>%
      dplyr::group_by(file_name) %>%
      tidyr::nest(.key = "metadata")

    forecast_data <- forecast_data %>%
      dplyr::mutate(
        forecast = purrr::map2(
          .data$file_name,
          .data$metadata,
          function(x, y) read_function(
            file_name   = x,
            parameter   = parameter,
            members     = y$member,
            lead_time   = y$lead_time,
            init        = init,
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
        -dplyr::contains("SID"),
        -dplyr::contains("lag")
      ) %>%
      colnames()
    gather_cols <- rlang::syms(sqlite_params)

    if (any(tolower(sqlite_params) == "t2m") && correct_t2m) {
      t2m_param       <- sqlite_params[which(tolower(sqlite_params) == "t2m")]
      t2m_uncorrected <- paste0(t2m_param, "_uncorrected")
      t2m_col         <- rlang::sym(t2m_param)
      t2m_uc_col      <- rlang::sym(t2m_uncorrected)

      forecast_data <- forecast_data %>%
        dplyr::inner_join(init$stations, by = "SID", suffix = c("", ".station")) %>%
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

    if (ncol(forecast_data) < 3) next

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
        )

      sqlite_data <- sqlite_data %>%
        dplyr::mutate(
          file_name = as.vector(glue::glue_data(sqlite_data, get_template(sqlite_template)))
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
          .data$parameter,
          .data$units,
          .data$file_name
        ) %>%
        dplyr::group_by(.data$file_name) %>%
        tidyr::nest()

      purrr::walk2(
        sqlite_data$data,
        sqlite_data$file_name,
        write_fctable_to_sqlite,
        synchronous  = sqlite_synchronous,
        journal_mode = sqlite_journal_mode
      )

    }

    if (return_data) function_output[[list_counter]] <- forecast_data

  }

  if (return_data) dplyr::bind_rows(function_output)

}
