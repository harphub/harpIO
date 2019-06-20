#' Read a field from a grib file
#'
#' @param filename The grib file name.
#' @param parameter The parameter to read. Standard HARP names are used.
#' @param meta If TRUE, also read all meta data (domain, time properties).
#' @param ... Arguments for \code{Rgrib2::Gdec}
#'
#' @return A geofield object with 2d array and projection information
#'
#' @examples
#' file_name <- "/lustre/storeB/users/andrewts/mepsr_data/grib/fc2017052600+001grib_fp"
#' model_geofield <- read_grib(file_name, "t2m")
#' model_geofield <- read_grib(file_name, "t500")
#' model_geofield <- read_grib(file_name, "topo")

### EXAMPLES NEED UPDATING

read_grib <- function(filename, parameter, meta = TRUE, ...) {
  #TODO: parameter may be a vector...
  param_info    <- get_grib_param_info(parameter)
  if (is.na(param_info$short_name)) {
    stop("Unknown parameter: ", parameter)
  }
  #
  if (!requireNamespace("Rgrib2", quietly = TRUE)) {
    stop(
      "read_grib requires the Rgrib2 package. Install with the following command:\n",
      "devtools::install_github(\"adeckmyn/Rgrib2\")",
      call. = FALSE
    )
  }
  grib_info     <- Rgrib2::Gopen(filename)
  #
  grib_position <- grib_info %>%
    dplyr::filter(
      shortName              == param_info$short_name,
      indicatorOfParameter   == param_info$param_number,
      indicatorOfTypeOfLevel == param_info$level_type,
      level                  == param_info$level_number
    ) %>%
    dplyr::pull(position)
  #

  if (length(grib_position) == 0) {
    stop("Parameter \"", parameter, "\" not found in grib file.", call. = FALSE)
  }

  # TODO: for "atmospheric" variables, read 3d data
  Rgrib2::Gdec(filename, grib_position, get.meta = meta, ...)
}

#' Read a field from a grib file & interpolate
#'
#' @param filename The grib file name.
#' @param parameter The parameter to read. Standard HARP names are used.
#' @param lead_time lead time
#' @param members ens members
#' @param init Initialisation for interpolation. A list that contains
#'    station locations and (possibly) pre-calculated interpolation weights etc.
#' @param meta If TRUE, also read all meta data (domain, time properties).
#' @param ... Arguments for \code{Rgrib2::Gdec}
#'
#' @return A tibble
read_grib_interpolate <- function(file_name, parameter,
                                  lead_time, members=NULL,
                                  init=list(), method="closest", use_lsm=FALSE, ...) {

  stop("Grib support for interpolation is not properly implemented yet.", call. = FALSE)

  if (!requireNamespace("Rgrib2", quietly = TRUE)) {
    stop(
      "read_grib requires the Rgrib2 package. Install with the following command:\n",
      "devtools::install_github(\"adeckmyn/Rgrib2\")",
      call. = FALSE
    )
  }

  if (file.exists(file_name)) {
    message("Reading: ", file_name)
  } else {
    warning("File not found: ", file_name, "\n", call. = FALSE, immediate. = TRUE)
    return(empty_data)
  }

  # TODO: grib-2 /can/ also have multiple members...
  #       multiple parameters
  all_data <- read_grib(file_name, parameter, lead_time, ...)
# fix the interpolation weights (they may already exist)
  if (is.null(init$weights) || attr(init$weights, "method") != method) {
    init <- initialise_weights(model, domain=all_data, stations=init$stations,
                             method=method, use_mask=use_lsm, drop_NA=TRUE)
    ## assign init to the calling function, so it can be re-used?
    assign("init", init, env = parent.frame())
  }
  fcpoints <- meteogrid::point.interp(all_data, weights=init$weights)
  # this (currently) creates an array width dimensions (station[,ldt][,prm])
  result <- tibble::tibble(lead_time = rep(lead_time, each=dim(init$stations)[1]))
  if (length(parameter)>1) {
    for (prm in seq_along(parameter)) result[[parameter[prm]]] <- as.vector(fcpoints[,,prm])
  } else {
    result[[parameter]] <- as.vector(fcpoints[,])
  }
  for (nn in names(init$stations)) result[[nn]] <- rep(init$stations[[nn]], length(lead_time))
  # add some (constant value) columns if requested
  if (!is.null(members)) result$members <- members
  list(fcst_data = result,
       units = tibble::tibble(parameter = parameter,
                              units = attr(all_data, "info")$unit))

}

