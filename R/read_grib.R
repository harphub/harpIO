# Read a field from a grib file
#
# @param filename The grib file name.
# @param parameter The parameter to read. Standard HARP names are used.
# @param meta If TRUE, also read all meta data (domain, time properties).
# @param ... Arguments for \code{Rgrib2::Gdec}
#
# @return A geofield object with 2d array and projection information
#
# NOT exported - used internally.
# @examples
# file_name <- "/lustre/storeB/users/andrewts/mepsr_data/grib/fc2017052600+001grib_fp"
# model_geofield <- read_grib(file_name, "t2m")
# model_geofield <- read_grib(file_name, "t500")
# model_geofield <- read_grib(file_name, "topo")

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
  grib_info <- Rgrib2::Gopen(filename)
  #

  if (grepl("[[:digit:]]+[[:alpha:]]", param_info$short_name)) {
    grib_position <- dplyr::filter(grib_info, .data$shortName == param_info$short_name)
  } else {
    grib_position <- grib_info %>%
      dplyr::filter(
        .data$shortName              == param_info$short_name,
        .data$indicatorOfTypeOfLevel == param_info$level_type[1],
        .data$level                  == param_info$level_number
      )
  }
  if (nrow(grib_position) < 1 && length(param_info$level_type) == 2) {
    grib_position <- grib_info %>%
      dplyr::filter(
        .data$shortName              == param_info$short_name,
        .data$indicatorOfTypeOfLevel == param_info$level_type[2],
        .data$level                  == param_info$level_number
      )
  }
  grib_position <- dplyr::pull(grib_position, .data$position)
  #

  if (length(grib_position) == 0) {
    stop("Parameter \"", parameter, "\" not found in grib file.", call. = FALSE)
  }

  dots <- list(...)
  if (is.null(dots$multi)) {
    multi <- FALSE
  } else {
    multi <- dots$multi
  }

  # TODO: for "atmospheric" variables, read 3d data
  out <- Rgrib2::Gdec(
    filename,
    grib_position,
    level     = param_info$level_number,
    levelType = param_info$level_type,
    get.meta  = meta,
    multi     = multi
  )

  out

}

# Read a field from a grib file & interpolate
#
# @param file_name The grib file name.
# @param parameter The parameter to read. Standard HARP names are used.
# @param lead_time lead time
# @param members ens members
# @param vertical_coordinate Not yet used.
# @param init Initialisation for interpolation. A list that contains
#    station locations and (possibly) pre-calculated interpolation weights etc.
# @param method Interpolation method (only necessary if the weights are not yet initialised)
# @param use_mask If TRUE, use land/sea mask in interpolation
# @param meta If TRUE, also read all meta data (domain, time properties).
# @param ... Arguments for \code{Rgrib2::Gdec}
#
# @return A tibble
# NOT exported. Used internally.
read_grib_interpolate <- function(file_name,
                                  parameter,
                                  lead_time = NA_real_,
                                  members   = NA_character_,
                                  vertical_coordinate = NA_character_,
                                  init=list(),
                                  method = "closest", use_mask=FALSE, ...) {
  # FIXME: grib2 files can contain multiple ensemble members!
  #stop("Grib support for interpolation is not properly implemented yet.", call. = FALSE)

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
    empty_data <- empty_data_interpolate(members, lead_time, empty_type = "fcst")
    return(empty_data)
  }

  # TODO: grib-2 /can/ also have multiple members...
  #       multiple parameters
  all_data <- read_grib(file_name, parameter, ...)
# fix the interpolation weights (they may already exist)
  if (is.null(init$weights) || attr(init$weights, "method") != method) {
    init <- initialise_interpolation(
      domain      = attr(all_data, "domain"),
      stations    = init$stations,
      method      = method,
      use_mask    = use_mask,
      drop_NA     = TRUE
    )
  }
  fcpoints <- meteogrid::point.interp(all_data, weights=init$weights)
  # this (currently) creates an array width dimensions (station[,ldt][,prm])
  result <- tibble::tibble(lead_time = rep(lead_time, each=dim(init$stations)[1]))
  if (length(parameter)>1) {
    # FIXME: multiple parameters all in same columns "forecast" and "parameter"
    for (prm in seq_along(parameter)) result[[parameter[prm]]] <- as.vector(fcpoints[,,prm])
    # AS: I moved the below line to here - I'm pretty sure it doesn't belong outside the if block
    result <- tidyr::gather(result, key = parameter, value = forecast, parameter)
  } else {
#    result[[parameter]] <- fcpoints
    result[["forecast"]] <- fcpoints
    result[["parameter"]] <- parameter
  }

  for (nn in names(init$stations)) result[[nn]] <- rep(init$stations[[nn]], length(lead_time))
  # add some (constant value) columns if requested
  if (!is.null(members)) result$member <- members
  list(fcst_data = dplyr::select(result, -.data$elev, -.data$name),
       units = tibble::tibble(parameter = parameter,
                              units = attr(all_data, "info")$unit))

}

