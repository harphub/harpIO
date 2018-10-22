#' Title
#'
#' @param file_name
#' @param parameter
#' @param lead_time
#' @param members
#' @param stations
#'
#' @return
#' @export
#'
#' @examples
read_netcdf_interpolate <- function(
  file_name,
  parameter   = NULL,
  lead_time   = NA_real_,
  members     = NA_character_,
  stations    = NULL,
  is_ensemble = FALSE
) {

  members   <- readr::parse_number(unique(members))
  lead_time <- unique(lead_time)

  if (is.null(stations)) {
    warning("No stations specified for interpolating to. Default station list used.", call. = FALSE)
    stations <- station_list
  }

  # Need to get the model elevation before anything else
  model_elevation <- miIO::miReadNetCDFinterpolation(
    file_name,
    sites = as.data.frame(stations),
    prm   = list(model_elevation = list(name = "surface_geopotential", mbr = 0)),
    lt    = 0
  ) %>%
    dplyr::rename(
      SID             = .data$SITE,
      lead_time       = .data$LT,
      model_elevation = .data$model_elevation.0
    ) %>%
    dplyr::mutate(model_elevation = .data$model_elevation / 9.80665) %>%
    dplyr::select(-dplyr::contains("TIME")) %>%
    tidyr::drop_na()

  stations <- dplyr::inner_join(
    stations,
    model_elevation,
    by = "SID"
  )

  # function to read the data
  get_netcdf_data <- function(.param, .file, .sites, .lead_time, .member, .is_ensemble) {

    nc_prm <- list()
    nc_prm[[.param]] <- list(name = get_netcdf_param_MET(.param), mbr = .member)
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

  dplyr::inner_join(
    stations,
    netcdf_data,
    by = "SID"
  ) %>%
    dplyr::select(-.data$elev, -.data$name)

}
