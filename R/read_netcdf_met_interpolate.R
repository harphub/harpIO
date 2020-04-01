# Read a netcdf file and interpolate to stations.
#
# The function reads a single file. To read more than 1 file and / or use for
# verificationuse the wrappers \link[harpIO]{read_eps_interpolate} and
# \link[harpIO]{read_det_interpolate}. The function has only been tested on
# netcdf files archived at MET Norway.
#
# @param file_name The netcdf file to read.
# @param parameter The parameters to read from the netcdf file. Can be harp
#   parameters are the names used in the netcdf files.
# @param lead_time The lead times to read (in hours).
# @param members The ensemble members to read.
# @param init A list, currently containing only the stations to interpolate to.
#   By default all of the
#   stations from \link[harpIO]{station_list} that are inside the domain are
#   used.
# @param ... Absorb arguments for other read_*_interpolate functions.
#
# @return A data frame with the data read from the netcdf file.
# NOT exported. Used internally.
#
# @examples
read_netcdf_met_interpolate <- function(
  file_name,
  parameter           = NULL,
  members             = NA_character_,
  lead_time           = NA_real_,
  vertical_coordinate = NA_character_,
  init                = list(),
  ...
) {

  if (!requireNamespace("miIO", quietly = TRUE)) {
    stop(
      paste0(
        "You need to install the miIO package from to read from netcdf files. \n",
        "It is not on CRAN but available from MET Norway."
      ),
      call. = FALSE
    )
  }

  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop(
      paste0(
        "You need to install the ncdf4 package from to read from netcdf files. \n",
        "It is available from CRAN ."
      ),
      call. = FALSE
    )
  }

  if (is.null(init$is_ensemble)) init$is_ensemble <- FALSE

  empty_data <- empty_data_interpolate(members, lead_time)

  if (!file.exists(file_name)) {
    warning("File not found: ", file_name, call. = FALSE, immediate. = TRUE)
    return(empty_data)
  }

  if (init$is_ensemble) {
    members   <- readr::parse_number(unique(members))
  }
  lead_time <- unique(lead_time)

  stations <- init$stations
  if (is.null(stations)) {
    warning("No stations specified for interpolating to. Default station list used.", call. = FALSE)
    stations <- station_list
  }

  message("Reading:", file_name)

  # Need to get the model elevation before anything else
  if (init$is_ensemble) {
    prm_nc <- list(model_elevation = list(name = "surface_geopotential", mbr = 0))
  } else {
    prm_nc <- list(model_elevation = list(name = "surface_geopotential"))
  }
  model_elevation <- miIO::miReadNetCDFinterpolation(
    file_name,
    sites = as.data.frame(stations),
    prm   = prm_nc,
    lt    = 0
  ) %>%
    dplyr::rename(
      SID             = .data$SITE,
      lead_time       = .data$LT
    )

  if (init$is_ensemble) {
    model_elevation <- dplyr::rename(model_elevation, model_elevation = .data$model_elevation.0)
  }

  model_elevation <- model_elevation %>%
    dplyr::mutate(model_elevation = .data$model_elevation / 9.80665) %>%
    dplyr::select(-dplyr::contains("TIME")) %>%
    tidyr::drop_na()

  stations <- dplyr::inner_join(
    stations,
    model_elevation,
    by = "SID"
  )

  if (nrow(stations) < 1) {
    stop("No stations found inside model domain.", call. = FALSE)
  }

  netcdf_data <- purrr::map_dfr(
    parameter,
    get_netcdf_data,
    file_name,
    stations,
    lead_time,
    members,
    init$is_ensemble,
    vertical_coordinate
  )

  netcdf_id   <- ncdf4::nc_open(file_name)
  param_units <- purrr::map_dfr(parameter, get_netcdf_units, vertical_coordinate, netcdf_id)
  ncdf4::nc_close(netcdf_id)

  ncdf_data <- dplyr::inner_join(
    stations,
    netcdf_data,
    by = "SID"
  ) %>%
    dplyr::select(-.data$elev, -.data$name)

  list(fcst_data = ncdf_data, units = param_units)

}



### Helper functions

# function to read the data
get_netcdf_data <- function(.param, .file, .sites, .lead_time, .member, .is_ensemble, .vertical_coordinate) {

  harp_param <- parse_harp_parameter(.param, .vertical_coordinate)
  if (is.na(.vertical_coordinate) && is_temp(harp_param, .vertical_coordinate)) {
    .vertical_coordinate <- harp_param$level_type
  }


  nc_prm <- list()
  if (.is_ensemble && !is.na(.member)) {
    nc_prm[[.param]] <- list(name = get_netcdf_param_MET(.param, .vertical_coordinate), mbr = .member)
  } else {
    nc_prm[[.param]] <- list(name = get_netcdf_param_MET(.param, .vertical_coordinate))
  }

  netcdf_data <- miIO::miReadNetCDFinterpolation(
    .file,
    sites = as.data.frame(.sites),
    lt    = .lead_time * 3600,
    prm   = nc_prm
  ) %>%
    dplyr::rename(
      SID       = .data$SITE,
      lead_time = .data$LT
    ) %>%
    dplyr::select(-dplyr::starts_with("TIME")) %>%
    tibble::as_tibble()

  split_col_names(netcdf_data, .param, .is_ensemble, .vertical_coordinate)

}

# Function to split the column names
split_col_names <- function(df, prm, is_ens, vc) {

  # For deterministic and no vertical coordinate, do nothing
  if (!is_ens & is.na(vc)) {
    df <- df
  }

  # For deterministic with vertical coordinate need to split <param>.L<level>
  if (!is_ens & !is.na(vc)) {
    df <- df %>%
      tidyr::gather(
        dplyr::starts_with(paste0(prm, ".")),
        key   = "lvl",
        value = "forecast"
      )
    if (vc == "pressure") {
      return(tidyr::separate(df, .data$lvl, c("parameter", "p"), "\\."))
    }
    else if (vc == "model") {
      df <- df %>%
        dplyr::mutate(
          parameter = regmatches(.data$lvl, regexpr("[[:graph:]]+(?=.L)", .data$lvl, perl = TRUE)),
          ml        = regmatches(.data$lvl, regexpr("(?<=L)[[:digit:]]+.[[:digit:]]+", member, perl = TRUE))
        )
    } else {
      stop ("Reading ", vc, " level data from netcdf files not yet implemented", call. = FALSE)
    }
  }

  if (is_ens) {
    df <- df %>%
      tidyr::gather(
        dplyr::starts_with(paste0(prm, ".")),
        key   = "member",
        value = "forecast"
      )

    # For ensemble with no vertical coordinate need to split <param>.<member>
    if (is.na(vc) | !is_temp(prm, vc)) {
      df <- df %>%
        tidyr::separate(.data$member, c("parameter", "member"), "\\.") %>%
        dplyr::mutate(member = paste0("mbr", formatC(as.numeric(.data$member), width = 3, flag = "0")))
    }

    # For ensemble with vertical coordinate need to split <param>.L<level>.<member>
    if (!is.na(vc) && is_temp(prm, vc)) {
      param_regexp  <- "[[:graph:]]+(?=.L)"
      member_regexp <- "[[:digit:]]+$"
      if (vc == "pressure") {
        level_regexp <- "(?<=L)[[:digit:]]+"
        level_col    <- rlang::sym("p")
      } else if (vc == "model") {
        level_regexp <- "(?<=L)[[:digit:]]+.[[:digit:]]+"
        level_col    <- rlang::sym("ml")
      } else {
        stop ("Reading ", vc, " level data from netcdf files not yet implemented", call. = FALSE)
      }
      df <- df %>%
        dplyr::mutate(
          parameter    = regmatches(.data$member, regexpr(param_regexp, .data$member, perl = TRUE)),
          mbr          = regmatches(.data$member, regexpr(member_regexp, .data$member, perl = TRUE)),
          !!level_col := regmatches(.data$member, regexpr(level_regexp, .data$member, perl = TRUE)),
          !!level_col := as.numeric(!!level_col),
          member       = paste0("mbr", formatC(as.numeric(.data$mbr), width = 3, flag = "0"))
        ) %>%
        dplyr::select(-.data$mbr)
    }
  }
  df
}

# Function to get parameter units
get_netcdf_units <- function(param, vc, nc_id){
  tibble::tibble(
    parameter = param,
    units     = ncdf4::ncatt_get(nc_id, get_netcdf_param_MET(param, vc), "units")$value
  )
}

