#' Read a netcdf file and interpolate to stations.
#'
#' The function reads a single file. To read more than 1 file and / or use for
#' verificationuse the wrappers \link[harpIO]{read_eps_interpolate} and
#' \link[harpIO]{read_det_interpolate}. The function has only been tested on
#' netcdf files archived at MET Norway.
#'
#' @param file_name The netcdf file to read.
#' @param parameter The parameters to read from the netcdf file. Can be harp
#'   parameters are the names used in the netcdf files.
#' @param lead_time The lead times to read (in hours).
#' @param members The ensemble members to read.
#' @param stations The stations to interpolate to. By default all of the
#'   stations from \link[harpIO]{station_list} that are inside the domain are
#'   used.
#' @param is_ensemble Logical - whether the file contains ensemble data. The
#'   default is FALSE (i.e. deterministic data).
#' @param ... Absorb arguments for other read_*_interpolate functions.
#'
#' @return A data frame with the data read from the netcdf file.
#' @export
#'
#' @examples
read_netcdf_interpolate <- function(
  file_name,
  parameter   = NULL,
  lead_time   = NA_real_,
  members     = NA_character_,
  stations    = NULL,
  is_ensemble = FALSE,
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


  empty_data <- empty_data_interpolate(members, lead_time)

  if (!file.exists(file_name)) {
    warning("File not found: ", file_name, call. = FALSE, immediate. = TRUE)
    return(empty_data)
  }

  if (is_ensemble) {
    members   <- readr::parse_number(unique(members))
  }
  lead_time <- unique(lead_time)

  if (is.null(stations)) {
    warning("No stations specified for interpolating to. Default station list used.", call. = FALSE)
    stations <- station_list
  }

  message("Reading:", file_name)

  # Need to get the model elevation before anything else
  if (is_ensemble) {
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

  if (is_ensemble) {
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

  # function to read the data
  get_netcdf_data <- function(.param, .file, .sites, .lead_time, .member, .is_ensemble) {

    nc_prm <- list()
    if (is_ensemble) {
      nc_prm[[.param]] <- list(name = get_netcdf_param_MET(.param), mbr = .member)
    } else {
      nc_prm[[.param]] <- list(name = get_netcdf_param_MET(.param))
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

    if (.is_ensemble) {

      netcdf_data <- netcdf_data %>%
        tidyr::gather(
          dplyr::contains(.param),
          key   = "member",
          value = !!rlang::sym(.param)
        ) %>%
        tidyr::separate(.data$member, c("param", "member"), "\\.") %>%
        dplyr::mutate(member = paste0("mbr", formatC(as.numeric(.data$member), width = 3, flag = "0"))) %>%
        dplyr::select(-.data$param)

    }

    netcdf_data

  }

  netcdf_data <- purrr::map(
    parameter,
    get_netcdf_data,
    file_name,
    stations,
    lead_time,
    members,
    is_ensemble
  )

  if (is_ensemble) {
    join_cols <- c("SID", "lead_time", "member")
  } else {
    join_cols <- c("SID", "lead_time")
  }

  if (length(netcdf_data) > 1) {
    netcdf_data <- purrr::reduce(netcdf_data, dplyr::inner_join, by = join_cols)
  } else {
    netcdf_data <- netcdf_data[[1]]
  }

  get_netcdf_units <- function(param, nc_id){
    tibble::tibble(
      parameter = param,
      units     = ncdf4::ncatt_get(nc_id, get_netcdf_param_MET(param), "units")$value
    )
  }

  netcdf_id   <- ncdf4::nc_open(file_name)
  param_units <- purrr::map_dfr(parameter, get_netcdf_units, netcdf_id)
  ncdf4::nc_close(netcdf_id)

  ncdf_data <- dplyr::inner_join(
    stations,
    netcdf_data,
    by = "SID"
  ) %>%
    dplyr::select(-.data$elev, -.data$name)

  list(fcst_data = ncdf_data, units = param_units)

}
