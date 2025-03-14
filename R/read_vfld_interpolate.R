# Read data from a vfld file
#
# \code{read_vfld_interpolate} returns the content of a named vfld file as a
# data frame. Although the data in vfld files are already interpolated to
# points, the function is called \code{read_vfld_interpolate} for consistency
# with other harpIO functions that interpolate to points from gridded fields.
#
# @param file_name Name of the vfld file
# @param parameter If a parameter is named only this parameter will be
#   extracted, otherwise all parameters from the vfld will be returned.
# @param lead_time Value to fill the lead_time column with in the output
#   tibble.
# @param members Value to fill the member column with in the output tibble.
# @param init All initialisation data. For vfld, a list containing only a
#        data frame of stations to filter to. Must contain a column
#        named SID. Currently not used.
# @param ... Absorb arguments for other read_*_interpolate functions.
#
# @return A data frame with columns SID, lat, lon, model_elevation, and a
#   column for each parameter.
# NOT exported - used internally.
#
# @examples
# my_dir <- "/lustre/storeB/users/andrewts/HarpResults/vfld"
# vfld_file <- get_filenames(
#   my_dir,
#   start_date = 2017052600,
#   end_date = 2017052600,
#   eps_model = "MEPS_summer2017_sfcPertRef",
#   sub_model = "MEPS_summer2017_sfcPertRef",
#   lead_time = 20,
#   member = 0,
#   file_template = "vfld")$file_name
# vfld_data <- read_vfld_interpolate(vfld_file)
#

### EXAMPLES NEED UPDATING!!!!

read_vfld_interpolate <- function(
  file_name,
  parameter           = NULL,
  lead_time           = NA_real_,
  members             = NA_character_,
  vertical_coordinate = c(NA_character_, "pressure", "model", "height"),
  init                = list(),
  format_opts         = vfile_opts(),
  param_defs          = getExportedValue("harpIO", "harp_params"),
  ...
) {

  vertical_coordinate <- match.arg(vertical_coordinate)

  if (length(format_opts) < 1) {
    format_opts <- vfile_opts()
  }

  if (is.numeric(members)) members <- paste0("mbr", formatC(members, width = 3, flag = "0"))
  if (is.null(members)) members <- NA_character_

  empty_data <- empty_data_interpolate(members, lead_time, empty_type = "fcst")

  if (!file.exists(file_name)) {
    warning("File not found: ", file_name, "\n", call. = FALSE, immediate. = TRUE)
    return(
      list(
        fcst_data = empty_data,
        units     = tibble::tibble(
          parameter = NA_character_,
          units     = NA_character_
        )
      )
    )
  }


  vfld_data <- read_vfile(
    file_name,
    members = members,
    lead_time = lead_time,
    v_type = "vfld",
    missing_value = format_opts$missing_value,
    synop_cols    = format_opts$synop_cols,
    temp_cols     = format_opts$temp_cols,
    param_defs    = param_defs
  )

  if (is.null(vfld_data)) {
    return(
      list(
        fcst_data = empty_data,
        units     = tibble::tibble(
          parameter = NA_character_,
          units     = NA_character_
        )
      )
    )
  }
  # Parameter selection

  if (!is.null(parameter)) {

    if (is.list(parameter)) {
      has_level <- purrr::map(parameter, names) %>%
        purrr::map_lgl(~ any(grepl("level", .x))) %>%
        all()
      if (!has_level) {
        stop("Parameter is a list but not all elements have a level component.")
      }
    } else {
      parameter <- purrr::map(parameter, parse_harp_parameter, vertical_coordinate)
    }

    synop_parameters <- parameter[
      which(purrr::map_lgl(parameter, is_synop, param_defs = param_defs))
    ]
    temp_parameters  <- parameter[
      which(purrr::map_lgl(parameter, is_temp, param_defs = param_defs))
    ]

    if (any(purrr::map_lgl(temp_parameters, ~.x$level != -999))) {
      stop(
        "Do not give vertical levels for reading from vfld files.\n",
        "Set veritcal_coordinate = \"pressure\" to read upper air parameters.",
        call. = FALSE
      )
    }

  } else { # Get all parameters from the file

    synop_parameters <- vfld_data$synop %>%
      dplyr::select(-dplyr::any_of(c("SID", "lat", "lon", "model_elevation"))) %>%
      #dplyr::select(-.data$SID, -.data$lat, -.data$lon, -.data$model_elevation) %>%
      colnames() %>%
      purrr::map(parse_harp_parameter)

    if (ncol(vfld_data$temp) < 5 | is.na(vertical_coordinate)) {
      temp_parameters <- NULL
    } else {
      temp_parameters <- vfld_data$temp %>%
        dplyr::select(-dplyr::any_of(c("SID", "lat", "lon", "model_elevation", "p"))) %>%
        colnames() %>%
        purrr::map(parse_harp_parameter, vertical_coordinate)
    }

  }

  # Extract the synop parameters

  if (length(synop_parameters) > 0) {
    synop_parameter <- unique(purrr::map_chr(synop_parameters, "fullname")) %>%
      intersect(colnames(vfld_data$synop))
    if (length(synop_parameter) < 1) {
      vfld_data$synop <- empty_data
    } else {
      param_cols_out  <- rlang::syms(synop_parameter)
      vfld_data$synop      <- vfld_data$synop %>%
        dplyr::select(dplyr::any_of(c("SID", "lat", "lon", "model_elevation")), !!!param_cols_out) %>%
        #dplyr::select(.data$SID, .data$lat, .data$lon, .data$model_elevation, !!!param_cols_out) %>%
        tidyr::gather(key = "parameter", value = "forecast", !!!param_cols_out) %>%
        dplyr::mutate(
          member    = members,
          lead_time = lead_time
        )
    }
  } else {
    vfld_data$synop <- empty_data
  }

  # Extract the temp parameters

  if (length(temp_parameters) > 0 && nrow(tidyr::drop_na(vfld_data$temp)) > 0) {
    p_level_elements <- which(purrr::map_chr(temp_parameters, "level_type") == "pressure")
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
      vfld_data$temp <- vfld_data$temp %>%
        dplyr::select(dplyr::any_of(c("SID", "lat", "lon", "model_elevation", "p")), !!!param_cols_in) %>%
        #dplyr::select(.data$SID, .data$lat, .data$lon, .data$model_elevation, .data$p, !!!param_cols_in) %>%
        tidyr::gather(key = "parameter", value = "forecast", !!!param_cols_in) %>%
        dplyr::mutate(
          member    = members,
          lead_time = lead_time
        )
    } else {
      vfld_data$temp <- empty_data
    }

  } else {

    vfld_data$temp <- empty_data

  }

  params <- dplyr::bind_rows(vfld_data$synop_params, vfld_data$temp_params) %>%
    dplyr::select(-.data$accum_hours) %>%
    dplyr::filter(.data$parameter != "p")

  vfld_data <- dplyr::bind_rows(vfld_data$synop, vfld_data$temp) %>%
    dplyr::filter_all(dplyr::any_vars(!is.na(.)))

  param_units <- tibble::tibble(
    parameter = unique(stats::na.omit(vfld_data$parameter))
  )

  unwanted_rows <- c("SID", "lead_time", "member", "lat", "lon")
  param_units <- param_units %>%
    dplyr::filter(!.data$parameter %in% unwanted_rows) %>%
    dplyr::mutate(
      param_basename = purrr::map_chr(purrr::map(.data$parameter, parse_harp_parameter, vertical_coordinate), "basename"),
      synop_param    = purrr::map_lgl(param_units$parameter, is_synop, vertical_coordinate, param_defs = param_defs),
      param_basename = dplyr::case_when(
        synop_param  ~ .data$parameter,
        grepl("Acc[[:alpha:]]+[[:digit:]]+[[:alpha:]]", .data$parameter, perl = TRUE) ~ .data$parameter,
        TRUE ~ param_basename
      )
    ) %>%
    dplyr::inner_join(dplyr::rename(params, param_basename = .data$parameter), by = "param_basename") %>%
    dplyr::select(-.data$param_basename, -.data$synop_param)

  list(fcst_data = tibble::as_tibble(vfld_data), units = tibble::as_tibble(param_units))

}

read_vfld <- function(...) {
  res <- read_vfld_interpolate(...)
  res[["units"]] <- dplyr::distinct(res[["units"]])
  res <- suppressMessages(dplyr::left_join(res[["fcst_data"]], res[["units"]]))
  names(res)[names(res) == "member"]   <- "members"
  names(res)[names(res) == "forecast"] <- "station_data"
  res[["members"]] <- as.numeric(gsub("[[:alpha:]]", "", res[["members"]]))
  res
}

