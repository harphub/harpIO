# Generic function to read both vobs and vfld files

read_vfile <- function(
  v_file_name,
  members       = NA,
  lead_time     = NA,
  v_type        = c("vfld", "vobs"),
  missing_value = -99.0
) {

  v_type <- match.arg(v_type)

  data_type <- switch(
    v_type,
    "vfld" = "fcst",
    "vobs" = "obs"
  )

  empty_data <- empty_data_interpolate(members, lead_time, empty_type = data_type)

  file_connection <- file(v_file_name, "r")

  # metadata

  v_metadata   <- scan(file_connection, nlines = 1, quiet = TRUE)

  if (length(v_metadata) < 2 | length(v_metadata) > 3) {
    warning("Unable to read: ", v_file_name, "\n", call. = FALSE, immediate. = TRUE)
    close(file_connection)
    return(list(synop_data = empty_data, temp_data = empty_data))
  }

  if (length(v_metadata) == 2) {
    num_synop <- v_metadata[1]
    num_temp  <- 0
    v_version <- v_metadata[2]
  } else {
    num_synop <- v_metadata[1]
    num_temp  <- v_metadata[2]
    v_version <- v_metadata[3]
  }

  if (v_version < 2 | v_version > 4) {
    warning("Unable to read: ", v_file_name, "\nv version = ", v_version, "\n", call. = FALSE, immediate. = TRUE)
    close(file_connection)
    return(list(synop_data = empty_data, temp_data = empty_data))
  }

  # synop data

  if (num_synop < 1) {

    synop_data <- empty_data

  } else {

    if (v_version == 4) {

      num_param    <- scan(file_connection, nmax = 1, quiet = TRUE)
      params_synop <- utils::read.table(
        file_connection,
        col.names        = c("parameter", "accum_hours"),
        nrows            = num_param,
        stringsAsFactors = FALSE
      )

    } else {

      num_param       <- 16
      num_temp_levels <- scan(file_connection, nmax = 1, quiet = TRUE)
      params_synop    <- data.frame(
        parameter        = v_default_names("synop"),
        accum_hours      = rep(0, num_param),
        stringsAsFactors = FALSE
      )

    }

    if (v_type == "vobs") {
      params_synop$parameter[params_synop$parameter == "PE"]   <- "PE12"
      params_synop$accum_hours[params_synop$parameter == "PE"] <- 12
    }

    params_synop <- dplyr::mutate(
      params_synop,
      parameter   = purrr::map(.data$parameter, parse_v_parameter_synop),
      units       = purrr::map_chr(.data$parameter, "param_units"),
      parameter   = purrr::map_chr(.data$parameter, "harp_param")
    )

    if (v_type == "vfld") {
      synop_columns <- c("SID", "lat", "lon", params_synop$parameter)
    } else {
      synop_columns <- c("SID", "lat", "lon", "elev", params_synop$parameter)
    }

    synop_data <- read.table(
      file_connection,
      col.names = synop_columns,
      nrows     = num_synop
    )

  }

  # temp data

  if (num_temp < 1) {

    temp_data <- empty_data

  } else {

    if (v_version == 4) {
      temp_metadata   <- scan(file_connection, nmax = 2, quiet = TRUE)
      num_temp_levels <- temp_metadata[1]
      num_param       <- temp_metadata[2]
      params_temp     <- read.table(
        file_connection,
        col.names        = c("parameter", "accum_hours"),
        nrows            = num_param,
        stringsAsFactors = FALSE
      )
    } else {
      num_param   <- 8
      params_temp <- data.frame(
        parameter        = v_default_names("temp"),
        accum_hours      = rep(0, 8),
        stringsAsFactors = FALSE
      )
    }

    params_temp <- dplyr::mutate(
      params_temp,
      parameter   = purrr::map(.data$parameter, parse_v_parameter_temp),
      units       = purrr::map_chr(.data$parameter, "param_units"),
      parameter   = purrr::map_chr(.data$parameter, "harp_param")
    )

    temp_data    <- list()

    if (num_temp_levels < 1) {

      temp_data <- empty_data

    } else {

      for (temp_station in 1:num_temp) {

        station_metadata <- scan(file_connection, nmax = 4, quiet = TRUE)
        temp_data[[temp_station]] <- data.frame(
          SID             = rep(as.integer(station_metadata[1]), num_temp_levels),
          lat             = rep(station_metadata[2], num_temp_levels),
          lon             = rep(station_metadata[3], num_temp_levels),
          model_elevation = rep(station_metadata[4], num_temp_levels)
        ) %>%
          dplyr::bind_cols(
            read.table(
              file_connection,
              nrows = num_temp_levels,
              col.names = params_temp$parameter
            )
          )

      }

      temp_data <- dplyr::bind_rows(temp_data)

      if (v_type == "vobs") {
        names(temp_data)[names(temp_data) == "model_elevation"] <- "elev"
      }

    }

  }

  close(file_connection)

  if (ncol(synop_data) > 4) {
    synop_data[,5:ncol(synop_data)][synop_data[,5:ncol(synop_data)] == missing_value] <- NA
  }
  if (ncol(temp_data) > 4) {
    temp_data[,5:ncol(temp_data)][temp_data[,5:ncol(temp_data)] == missing_value] <- NA
  }

  if (nrow(synop_data) == 1 && all(is.na(synop_data[1,]))) {
    params_synop <- data.frame(parameter = character(), units = character(), stringsAsFactors = FALSE)
  }
  if (nrow(temp_data) == 1 && all(is.na(temp_data[1,]))) {
    params_temp  <- data.frame(parameter = character(), units = character(), stringsAsFactors = FALSE)
  }

  list(synop = synop_data, temp = temp_data, synop_params = params_synop, temp_params = params_temp)

}

v_default_names <- function(data_type) {
  if (data_type == "synop") {
    c("FI",
      "NN",
      "DD",
      "FF",
      "TT",
      "RH",
      "PS",
      "PE",
      "QQ",
      "VI",
      "TD",
      "TX",
      "TN",
      "GW",
      "GX",
      "WX"
    )
  } else if (data_type == "temp") {
    c("PP","FI","TT","RH","DD","FF","QQ","TD")
  } else {
    NA_character_
  }
}

