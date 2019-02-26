#' Read data from a vfld file
#'
#' \code{read_vfld_interpolate} returns the content of a named vfld file as a
#' data frame. Although the data in vfld files are already interpolated to
#' points, the function is called \code{read_vfld_interpolate} for consistency
#' with other harpIO functions that interpolate to points from gridded fields.
#'
#' @param file_name Name of the vfld file
#' @param parameter If a parameter is named only this parameter will be
#'   extracted, otherwise all parameters from the vfld will be returned.
#' @param lead_time Value to fill the lead_time column with in the output
#'   tibble.
#' @param members Value to fill the member column with in the output tibble.
#' @param stations A data frame of stations to filter to. Must contain a column
#'   named SID.
#' @param ... Absorb arguments for other read_*_interpolate functions.
#'
#' @return A data frame with columns SID, lat, lon, model_elevation, and a
#'   column for each parameter.
#' @export
#'
#' @examples
#' my_dir <- "/lustre/storeB/users/andrewts/HarpResults/vfld"
#' vfld_file <- get_filenames(
#'   my_dir,
#'   start_date = 2017052600,
#'   end_date = 2017052600,
#'   eps_model = "MEPS_summer2017_sfcPertRef",
#'   sub_model = "MEPS_summer2017_sfcPertRef",
#'   lead_time = 20,
#'   member = 0,
#'   file_template = "vfld")$file_name
#' vfld_data <- read_vfld_interpolate(vfld_file)
#'

### EXAMPLES NEED UPDATING!!!!

read_vfld_interpolate <- function(
  file_name,
  parameter   = NULL,
  lead_time   = NA_real_,
  members     = NA_character_,
  stations    = NULL,
  ...
) {

  empty_data <- empty_data_interpolate(members, lead_time)

  if (is.numeric(members)) members <- paste0("mbr", formatC(members, width = 3, flag = "0"))

  if (file.exists(file_name)) {
    message("Reading: ", file_name)
  } else {
    warning("File not found: ", file_name, "\n", call. = FALSE, immediate. = TRUE)
    return(empty_data)
  }

  vfld_data <- readr::read_lines(file_name) %>%
    stringr::str_trim(side = "both") %>%
    strsplit("\\s+")

  ### SYNOP DATA

  # The first row is num_synop, num_temp, vfld_version
  num_synop    <- as.numeric(vfld_data[[1]][1])
  num_temp     <- as.numeric(vfld_data[[1]][2])
  vfld_version <- as.numeric(vfld_data[[1]][3])

  # The second row is the number of parameters
  num_param <- as.numeric(vfld_data[[2]])

  synop_start_row <- 3 + num_param
  synop_end_row   <- synop_start_row + num_synop - 1

  # The following num_param rows are parameter, accum_hours
  params_synop <- t(dplyr::bind_cols(vfld_data[3:(synop_start_row - 1)])) %>%
    tibble::as_tibble()
  colnames(params_synop) <- c("parameter", "accum_hours")
  params_synop <- params_synop %>%
    dplyr::mutate(
      parameter   = purrr::map(.data$parameter, parse_v_parameter_synop),
      units       = purrr::map_chr(.data$parameter, "param_units"),
      parameter   = purrr::map_chr(.data$parameter, "harp_param"),
      accum_hours = as.numeric(.data$accum_hours)
    )

  # The next num_synop rows are the synop data
  synop_data <- t(dplyr::bind_cols(vfld_data[synop_start_row:synop_end_row])) %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(as.numeric)
  colnames(synop_data) <- c("SID", "lat", "lon", params_synop$parameter)
  synop_data$SID <- as.integer(synop_data$SID)

  # Filter to stations and correct 2m temperature if required - this might not be the place to do this now
  # Shouold be taken care of in read_members_interpolate.

  # no_sid_col <- FALSE
  # if (!is.null(stations)) {
  #   if (!grepl("SID", colnames(stations))) {
  #     cat(
  #       "No SID column found in stations data frame. \n",
  #       "All stations will be kept."
  #     )
  #     no_sid_col <- TRUE
  #   }
  #   synop_data <- dplyr::inner_join(synop_data, stations, by = "SID")
  # }

  ### TEMP DATA

  # The following two rows are the temp metadata
  num_temp_levels <- as.numeric(vfld_data[[(synop_end_row + 1)]])
  num_param       <- as.numeric(vfld_data[[(synop_end_row + 2)]])
  temp_start_row  <- synop_end_row + 3 + num_param

  # The following num_param rows are parameter, accum_hours
  params_temp<- t(dplyr::bind_cols(vfld_data[(synop_end_row + 3):(temp_start_row - 1)])) %>%
    tibble::as.tibble()
  colnames(params_temp) <- c("parameter", "accum_hours")
  params_temp <- params_temp %>%
    dplyr::mutate(
      parameter   = purrr::map(.data$parameter, parse_v_parameter_temp),
      units       = purrr::map_chr(.data$parameter, "param_units"),
      parameter   = purrr::map_chr(.data$parameter, "harp_param"),
      accum_hours = as.numeric(.data$accum_hours)
    )

  # Loop over the temp stations
  temp_data <- list()
  if (num_temp < 1) {

    temp_data <- empty_data

  } else {

    for (temp_station in 1:num_temp) {

      temp_data[[temp_station]] <- tibble::tibble(
        SID             = rep(vfld_data[[temp_start_row]][1], num_temp_levels),
        lat             = rep(vfld_data[[temp_start_row]][2], num_temp_levels),
        lon             = rep(vfld_data[[temp_start_row]][3], num_temp_levels),
        model_elevation = rep(vfld_data[[temp_start_row]][4], num_temp_levels)
      )

      temp_values <- t(
        dplyr::bind_cols(
          vfld_data[(temp_start_row + 1):(temp_start_row + num_temp_levels)]
        )
      ) %>%
        tibble::as_tibble()
      colnames(temp_values) <- params_temp$parameter

      temp_data[[temp_station]] <- temp_data[[temp_station]] %>%
        dplyr::bind_cols(temp_values)

      temp_start_row <- temp_start_row + num_temp_levels + 1
    }

    temp_data <- dplyr::bind_rows(temp_data) %>%
      dplyr::mutate_all(as.numeric) %>%
      dplyr::mutate(SID = as.integer(.data$SID))

  }

  # Filter to stations and correct 2m temperature if required

  # if (!is.null(stations)) {
  #   if (!grepl("SID", colnames(stations))) {
  #     if (!no_sid_col) {
  #       cat(
  #         "No SID column found in stations data frame. \n",
  #         "All stations will be kept."
  #       )
  #     }
  #   }
  #   temp_data <- dplyr::inner_join(temp_data, stations, by = "SID")
  # }

  ### GET THE PARAMETER(S)

  if (!is.null(parameter)) {

    if (is.list(parameter)) {
      has_level <- purrr::map(parameter, names) %>%
        purrr::map_lgl(~ any(grepl("level", .x))) %>%
        all()
      if (!has_level) {
        stop ("Parameter is a list but not all elements have a level component.")
      }
    } else {
      parameter <- purrr::map(parameter, parse_harp_parameter)
    }
    param_level_type <- purrr::map(parameter, "levelType")
    param_level      <- purrr::map(parameter, "level")

    is_synop <- function(.level_type, .level) {
      is.null(.level_type) | (.level_type == "height" && .level %in% c(2, 10))
    }

    synop_parameters <- parameter[which(purrr::map2_lgl(param_level_type, param_level, is_synop))]
    temp_parameters  <- parameter[which(purrr::map2_lgl(param_level_type, param_level, ~ !is_synop(.x, .y)))]

  } else { # Get all parameters from the file

    synop_parameters <- synop_data %>%
      dplyr::select(-.data$SID, -.data$lat, -.data$lon, -.data$model_elevation) %>%
      colnames() %>%
      purrr::map(parse_harp_parameter)

    if (num_temp < 1) {
      temp_parameters <- NULL
    } else {
      temp_parameters <- temp_data %>%
        dplyr::select(-.data$SID, -.data$lat, -.data$lon, -.data$model_elevation, -.data$p) %>%
        colnames() %>%
        purrr::map(~ paste0(.x, unique(temp_data$p))) %>%
        unlist() %>%
        purrr::map(parse_harp_parameter)
    }

  }

  # Extract the synop parameters

  if (length(synop_parameters) > 0) {
    synop_parameter <- unique(purrr::map_chr(synop_parameters, "fullname"))
    param_cols_out  <- rlang::syms(synop_parameter)
    synop_data      <- synop_data %>%
      dplyr::select(.data$SID, .data$lat, .data$lon, .data$model_elevation, !!!param_cols_out) %>%
      dplyr::mutate(
        member    = members,
        lead_time = lead_time
      )
  } else {
    synop_data <- empty_data
  }

  # Extract the temp parameters

  if (length(temp_parameters) > 0) {
    p_level_elements <- which(purrr::map_chr(temp_parameters, "levelType") == "pressure")
    if (length(p_level_elements) < length(temp_parameters)) {
      warning(
        paste0(
          "Some parameters are not on pressure levels.\n",
          "vfld files only contain pressure level data."
        )
      )
    }
    temp_parameters <- temp_parameters[p_level_elements]
    if (length(temp_parameters) > 0) {
      temp_parameter_base <- purrr::map_chr(temp_parameters, "basename")
      param_cols_in       <- rlang::syms(temp_parameter_base)
      temp_parameter_full <- purrr::map_chr(temp_parameters, "fullname")
      param_cols_out      <- rlang::syms(temp_parameter_full)
      temp_data <- temp_data %>%
        dplyr::select(.data$SID, .data$lat, .data$lon, .data$model_elevation, .data$p, !!!param_cols_in) %>%
        tidyr::gather(key = param, value = forecast, !!!param_cols_in) %>%
        dplyr::mutate(param = paste0(.data$param, .data$p)) %>%
        dplyr::select(-.data$p) %>%
        tidyr::spread(.data$param, .data$forecast) %>%
        dplyr::select(.data$SID, .data$lat, .data$lon, .data$model_elevation, !!!param_cols_out) %>%
        dplyr::mutate(
          member    = members,
          lead_time = lead_time
        )
    } else {
      temp_data <- empty_data
    }

  } else {

    temp_data <- empty_data

  }

  vfld_data <- dplyr::full_join(
    synop_data,
    temp_data,
    by     = c("SID", "lead_time", "member"),
    suffix = c("", ".temp")
  )

  params <- dplyr::bind_rows(params_synop, params_temp) %>%
    dplyr::select(-.data$accum_hours) %>%
    dplyr::filter(.data$parameter != "p")

  param_units <- tibble::tibble(
    parameter = colnames(vfld_data)
  )

  special_cases <- c("T2m", "RH2m", "Td2m", "S10m", "G10m", "D10m", "N75", "Q2m")
  unwanted_rows <- c("SID", "lead_time", "member", "lat", "lon", "lat.temp", "lon.temp")
  param_units <- param_units %>%
    dplyr::filter(!.data$parameter %in% unwanted_rows) %>%
    dplyr::mutate(
      param_basename = purrr::map_chr(purrr::map(.data$parameter, parse_harp_parameter), "basename"),
      param_basename = dplyr::case_when(
        grepl(".temp", .data$param_basename) ~ gsub(".temp", "", .data$param_basename),
        .data$parameter %in% special_cases   ~ .data$parameter,
        TRUE ~ param_basename
      )
    ) %>%
    dplyr::full_join(dplyr::rename(params, param_basename = .data$parameter), by = "param_basename") %>%
    dplyr::select(-.data$param_basename)

  list(fcst_data = vfld_data, units = param_units)

}
