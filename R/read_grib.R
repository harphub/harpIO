# Read a field from a grib file
#
# @param file_name The grib file name.
# @param parameter The parameter to read. Standard HARP names are used.
# @param meta If TRUE, also read all meta data (domain, time properties).
# @param vertical_coordinate The vertical_coordinate for upper air data.
# @param transformation The transformation to apply to the gridded data. Can be
#   "none", "interpolate", "regrid", or "xsection".
# @param transformation_opts = Options for the the transformation. Depends on the
#   transformation. For interpolation this should include:
#     - method: the interpolation method to use. See meteogrid.
#     - use_mask: Logical. Whether to use a land-sea mask in the interpolation.
#     - stations: a dataframe of stations with columns SID, lat, lon and possibly elev
#     or
#     - weights: the interpolation weights if they have already been calculated.
#     Note that when weights are included all other options are ignored. If stations
#     are not given, the harpIO default station list is used.
#   All transformations can include the logical keep_raw_data. If this is set to
#   TRUE, the raw gridded data will be kept. If FALSE, or not set the raw gridded
#   data will be discarded.
# @param ... Arguments for Rgrib2::Gdec
#
# @return A data frame with columns of metadata taken from the file and a list
#   column of the gridded and / or transformed data.
#
# NOT exported - used internally.
#
# @examples
# file_name <- system.file("grib/HARMUK20171015T12Z+003.grib", package = "harpData")
# t2m_gridded <- read_grib(file_name, "t2m")
# t2m_points  <- read_grib(
#   file_name,
#   t2m",
#   transformation = "interpolate",
#   transformation_opts = list(method = "nearest", use_mask = TRUE)
# )
# model_geofield <- read_grib(file_name, "topo")

### EXAMPLES NEED UPDATING

read_grib <- function(
  file_name,
  parameter,
  meta                = TRUE,
  vertical_coordinate = NA_character_,
  transformation      = "none",
  transformation_opts = list(),
  show_progress       = FALSE,
  ...
) {

  if (!requireNamespace("Rgrib2", quietly = TRUE)) {
    stop(
      "read_grib requires the Rgrib2 package. Install with the following command:\n",
      "remotes::install_github(\"adeckmyn/Rgrib2\")",
      call. = FALSE
    )
  }

  parameter      <- lapply(parameter, parse_harp_parameter, vertical_coordinate)
  param_info     <- lapply(parameter, get_grib_param_info)
  unknown_params <- which(sapply(param_info, function(x) is.na(x$short_name)))

  if (length(unknown_params) > 0) {
    lapply(
      unknown_params,
      function(x) warning(
        "Don't know how to read '", parameter[[x]]$fullname, "' from grib files.",
        immediate. = TRUE,
        call.      = FALSE
      )
    )
  }

  parameter  <- parameter[setdiff(seq_along(parameter), unknown_params)]
  param_info <- param_info[setdiff(seq_along(parameter), unknown_params)]

  if (length(parameter) < 1) {
    stop("None of the requested parameters can be read from grib files.", call. = FALSE)
  }

  grib_info <- Rgrib2::Gopen(file_name)

  # filter_grib_info function defined at end of file
  grib_info <- purrr::map2_dfr(parameter, param_info, filter_grib_info, grib_info)

  dots <- list(...)
  if (is.null(dots$multi)) {
    multi <- FALSE
  } else {
    multi <- dots$multi
  }

  if (
    transformation == "interpolate" &&
      (is.null(transformation_opts[["weights"]]) ||
          attr(transformation_opts[["weights"]], "method") != transformation_opts[["method"]])
  ) {

    if (!is.null(transformation_opts[["keep_raw_data"]])) {
      keep_raw_data <- transformation_opts[["keep_raw_data"]]
    } else {
      keep_raw_data <- FALSE
    }

    # Assume grib message at position 1 has the same domain information as all messages
    message("Computing interpolation weights.")
    transformation_opts <- initialise_interpolation(
      domain   = attr(Rgrib2::Gdec(file_name, 1), "domain"),
      stations = transformation_opts[["stations"]],
      method   = transformation_opts[["method"]],
      use_mask = transformation_opts[["use_mask"]],
      drop_NA  = TRUE
    )
    transformation_opts[["keep_raw_data"]] <- keep_raw_data
  }

  grib_opts <- list(meta = meta, multi = multi)

  message("Reading data from ", file_name, ".")

  grib_data <- purrr::map_dfr(
    1:nrow(grib_info),
    read_and_transform_grib,
    file_name,
    grib_info,
    grib_opts,
    transformation,
    transformation_opts,
    show_progress
  )

  if (show_progress) cat("\n")

  grib_data

}

# Read a field from a grib file & interpolate
#
# @param file_name The grib file name.
# @param parameter The parameter to read. Standard HARP names are used.
# @param lead_time lead time
# @param members ens members
# @param vertical_coordinate The vertical coordinate for upper air parameters
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
  lead_time           = NA_real_,
  members             = NA_character_,
  vertical_coordinate = NA_character_,
  init                = list(),
  method              = "closest",
  use_mask            = FALSE,
  show_progress       = FALSE,
  ...
) {
  # FIXME: grib2 files can contain multiple ensemble members!
  #stop("Grib support for interpolation is not properly implemented yet.", call. = FALSE)

  if (!requireNamespace("Rgrib2", quietly = TRUE)) {
    stop(
      "read_grib requires the Rgrib2 package. Install with the following command:\n",
      "devtools::install_github(\"adeckmyn/Rgrib2\")",
      call. = FALSE
    )
  }

  if (!file.exists(file_name)) {
    warning("File not found: ", file_name, "\n", call. = FALSE, immediate. = TRUE)
    empty_data <- empty_data_interpolate(members, lead_time, empty_type = "fcst")
    return(empty_data)
  }

  fcst_data <- read_grib(
    file_name,
    parameter,
    vertical_coordinate = vertical_coordinate,
    transformation      = "interpolate",
    transformation_opts = list(
      stations = init$stations,
      method   = method,
      use_mask = use_mask,
      weights  = init$weights
    ),
    show_progress = show_progress,
    ...
  )

  list(
    fcst_data = dplyr::transmute(
      fcst_data,
      .data$SID,
      .data$lat,
      .data$lon,
      .data$parameter,
      forecast  = .data$value,
      member    = members,
      lead_time = .data$lead_time,
      p         = dplyr::case_when(
        .data$level_type == "pressure" ~ .data$level,
        TRUE                     ~ NA_integer_,
      )
    ),
    units = dplyr::distinct(dplyr::select(fcst_data, .data$parameter, .data$units))
  )

}


#####

# Function to get the grib information for parameters

filter_grib_info <- function(parameter, param_info, grib_info) {
  if (grepl("[[:digit:]]+[[:alpha:]]", param_info$short_name)) {
    grib_info <- dplyr::filter(grib_info, .data$shortName == param_info$short_name)
  } else {
    grib_info <- grib_info %>%
      dplyr::filter(
        .data$shortName              == param_info$short_name,
        .data$indicatorOfTypeOfLevel == param_info$level_type[1],
      )
    if (param_info$level_number != -999) {
      grib_info <- dplyr::filter(grib_info, .data$level == param_info$level_number)
    }
  }
  if (nrow(grib_info) < 1 && length(param_info$level_type) == 2) {
    grib_info <- grib_info %>%
      dplyr::filter(
        .data$shortName              == param_info$short_name,
        .data$indicatorOfTypeOfLevel == param_info$level_type[2],
        .data$level                  == param_info$level_number
      )
  }
  grib_info[["level_type"]] <- parameter[["level_type"]]
  grib_info[["parameter"]]  <- parameter[["fullname"]]

  if (nrow(grib_info) == 0) {
    warning("Parameter \"", parameter, "\" not found in grib file.", call. = FALSE, immediate. = TRUE)
  }

  grib_info

}

# Function to read and transform data from grib file to be used in map_dfr below.
# This function should also include calls to interpolate, regrid and xsection so
# that no more data is kept in memory than is necessary.
read_and_transform_grib <- function(
  x, file_name, grib_info, grib_opts, transformation = "none", opts = list(), show_progress
) {

  fcdate    <- suppressMessages(
    str_datetime_to_unixtime(paste0(grib_info$dataDate[x], grib_info$dataTime[x]))
  )
  validdate <- suppressMessages(
    str_datetime_to_unixtime(paste0(grib_info$validityDate[x], grib_info$validityTime[x]))
  )
  leadtime  <- (validdate - fcdate) / 3600

  result <- tibble::tibble(
    fcdate       = fcdate,
    validdate    = validdate,
    lead_time    = leadtime,
    parameter    = grib_info$parameter[x],
    level_type   = grib_info$level_type[x],
    level        = grib_info$level[x],
    units        = grib_info$units[x],
    gridded_data = list(
      Rgrib2::Gdec(
        file_name,
        grib_info$position[x],
        get.meta  = grib_opts[["meta"]],
        multi     = grib_opts[["multi"]]
      )
    )
  )

  if (transformation == "interpolate") {
    result[["station_data"]] <- interpolate_geofield(result[["gridded_data"]], opts)
    if (is.null(opts[["keep_raw_data"]]) || !opts[["keep_raw_data"]]) {
      result <- result[, which(names(result) != "gridded_data")]
      result <- tidyr::unnest(result, .data[["station_data"]])
    }
  }

  if (show_progress) cat(".")

  result

}
