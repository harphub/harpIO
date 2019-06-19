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


  empty_data <- empty_data_interpolate(members, lead_time, empty_type = "fcst")

  if (file.exists(file_name)) {
    message("Reading: ", file_name)
  } else {
    warning("File not found: ", file_name, "\n", call. = FALSE, immediate. = TRUE)
    return(empty_data)
  }

  if (is.numeric(members)) members <- paste0("mbr", formatC(members, width = 3, flag = "0"))

  vfld_data <- read_vfile(file_name, members = members, lead_time = lead_time, v_type = "vfld")

  # Parameter selection

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

    synop_parameters <- vfld_data$synop %>%
      dplyr::select(-.data$SID, -.data$lat, -.data$lon, -.data$model_elevation) %>%
      colnames() %>%
      purrr::map(parse_harp_parameter)

    if (ncol(vfld_data$temp) < 5) {
      temp_parameters <- NULL
    } else {
      temp_parameters <- vfld_data$temp %>%
        dplyr::select(-.data$SID, -.data$lat, -.data$lon, -.data$model_elevation, -.data$p) %>%
        colnames() %>%
        purrr::map(~ paste0(.x, unique(vfld_data$temp$p))) %>%
        unlist() %>%
        purrr::map(parse_harp_parameter)
    }

  }

  # Extract the synop parameters

  if (length(synop_parameters) > 0) {
    synop_parameter <- unique(purrr::map_chr(synop_parameters, "fullname")) %>%
      intersect(colnames(vfld_data$synop))
    param_cols_out  <- rlang::syms(synop_parameter)
    vfld_data$synop      <- vfld_data$synop %>%
      dplyr::select(.data$SID, .data$lat, .data$lon, .data$model_elevation, !!!param_cols_out) %>%
      dplyr::mutate(
        member    = members,
        lead_time = lead_time
      )
  } else {
    vfld_data$synop <- empty_data
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
      vfld_data$temp <- vfld_data$temp %>%
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
      vfld_data$temp <- empty_data
    }

  } else {

    vfld_data$temp <- empty_data

  }

  params <- dplyr::bind_rows(vfld_data$synop_params, vfld_data$temp_params) %>%
    dplyr::select(-.data$accum_hours) %>%
    dplyr::filter(.data$parameter != "p")

  vfld_data <- dplyr::full_join(
    vfld_data$synop,
    vfld_data$temp,
    by     = c("SID", "lead_time", "member"),
    suffix = c("", ".temp")
  )

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
        grepl("Acc[[:alpha:]]+[[:digit:]]+[[:alpha:]]", .data$parameter, perl = TRUE) ~ .data$parameter,
        TRUE ~ param_basename
      )
    ) %>%
    dplyr::full_join(dplyr::rename(params, param_basename = .data$parameter), by = "param_basename") %>%
    dplyr::select(-.data$param_basename)

  list(fcst_data = tibble::as_tibble(vfld_data), units = tibble::as_tibble(param_units))

}

