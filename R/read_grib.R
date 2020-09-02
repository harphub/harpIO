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
# @param opts Options for reading grib files. Usually set by grib_opts()
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
  lead_time           = NULL,
  members             = NULL,
  vertical_coordinate = NA_character_,
  transformation      = "none",
  transformation_opts = list(),
  format_opts         = grib_opts(),
  show_progress       = FALSE
) {

  if (!requireNamespace("Rgrib2", quietly = TRUE)) {
    stop(
      "read_grib requires the Rgrib2 package. Install with the following command:\n",
      "remotes::install_github(\"harphub/Rgrib2\")",
      call. = FALSE
    )
  }

  if (is.null(parameter)) {
    stop("For grib files, parameter = '<parameter>' must be passed.", call. = FALSE)
  }

  # FIXME: if there is partial overlap, keep the "unchanged" part of grib_opts() ?
  #        then it all fits on 1 line
  # format_opts <- c(format_opts, grib_opts()[setdiff(names(grib_opts()), names(format_opts))] 
  #        OR: always expect format_opts to be complete, don't add grib_opts()
  #        then just have format_opts=grib_opts() in the function header
  format_opts <- do.call(grib_opts, format_opts)

  if (is.list(parameter) && inherits(parameter, "harp_parameter")) {
    parameter <- list(parameter)
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
    parameter  <- parameter[-unknown_params]
    param_info  <- param_info[-unknown_params]
  }

  if (length(parameter) < 1) {
    stop("None of the requested parameters can be read from grib files.", call. = FALSE)
  }

  grib_info <- Rgrib2::Gopen(
    file_name,
    IntPar = c(
      "editionNumber",
      "dataDate",
      "dataTime",
      "validityDate",
      "validityTime",
      "Nx",
      "Ny",
      "table2Version",
      "indicatorOfParameter",
      "parameterCategory",
      "parameterNumber",
      "levelType",
      "level",
      "perturbationNumber"
    ),
    multi = format_opts[["multi"]]
  )

  if (packageVersion("Rgrib2") >= "1.3.4.9001") {
    grib_file <- Rgrib2::Gindex(file_name)
  } else {
    grib_file <- file_name
  }

  grib_info[["fcdate"]]    <- suppressMessages(
    str_datetime_to_unixtime(paste0(grib_info$dataDate, formatC(grib_info$dataTime, width = 4, flag = "0")))
  )
  grib_info[["validdate"]] <- suppressMessages(
    str_datetime_to_unixtime(paste0(grib_info$validityDate, formatC(grib_info$validityTime, width = 4, flag = "0")))
  )
  grib_info[["leadtime"]]  <- (grib_info[["validdate"]] - grib_info[["fcdate"]]) / 3600
  colnames(grib_info)[colnames(grib_info) == "perturbationNumber"] <- "member"

  # filter_grib_info function defined at end of file
  # For dplyr methods in filter_grib_info the new class has to be after
  class(grib_info) <- rev(class(grib_info))
  grib_info <- purrr::map2_dfr(parameter, param_info, filter_grib_info, grib_info, lead_time, members)
  class(grib_info) <- rev(class(grib_info))

  if (nrow(grib_info) < 1) {
    stop("None of the requested data could be read from grib file: ", file_name, call. = FALSE)
  }

  if (transformation != "none") {
    domain <- attr(grib_info, "domain")
  } else {
    domain <- NULL
  }

  transformation_opts <- compute_transformation_weights(
    domain,
    transformation,
    transformation_opts
  )

  # Function to read and transform data from grib file to be used in map_dfr below.
  # This function should also include calls to interpolate, regrid and xsection so
  # that no more data is kept in memory than is necessary.
  read_and_transform_grib <- function(
    row_num,
    file_name,
    grib_info,
    format_opts,
    transformation = "none",
    opts           = list(),
    show_progress  = FALSE
  ) {

    result <- tibble::tibble(
      fcdate       = grib_info$fcdate[row_num],
      validdate    = grib_info$validdate[row_num],
      lead_time    = grib_info$leadtime[row_num],
      parameter    = grib_info$parameter[row_num],
      members      = grib_info$member[row_num],
      level_type   = grib_info$level_type[row_num],
      level        = grib_info$level[row_num],
      units        = grib_units_to_harp_units(grib_info$units[row_num]),
      gridded_data = list(
        Rgrib2::Gdec(
          file_name,
          grib_info$position[row_num],
          get.meta  = format_opts[["meta"]],
          multi     = format_opts[["multi"]]
        )
      )
    )

    result <- transform_geofield(result, transformation, opts)

    if (show_progress) pb$tick()

    result

  }

  if (show_progress) {
    pb <- progress::progress_bar$new(format = "[:bar] :percent eta: :eta", total = nrow(grib_info))
  }

  grib_data <- purrr::map_dfr(
    1:nrow(grib_info),
    read_and_transform_grib,
    grib_file,
    grib_info,
    format_opts,
    transformation,
    transformation_opts,
    show_progress
  )

  attr(grib_data, "transformation_opts") <- transformation_opts

  grib_data

}

#####

# Function to get the grib information for parameters
filter_grib_info <- function(parameter, param_info, grib_info, lead_time, members) {
#  if (grepl("(?:^mn|^mx|^)[[:digit:]]+[[:alpha:]]", param_info$short_name)) {
#    grib_info_f <- dplyr::filter(grib_info, .data$shortName == param_info$short_name)
#  } else {
  # Some parameters may be encoded in two different ways, so we may need a second try
  #  e.g. precip can be on "surface" or "0m above ground")
  # 10m wind speed can be "ws" or "10si" (or even "SP_10M" in DWD files)
  # TODO: for "unknown" shortNames we could try to use parameter number?
  #       that would be useful when we need a local "grib_override"
    for (i in seq_along(param_info$short_name)) {
      for(j in seq_along(param_info$level_type)) {
        if (param_info$level_type[j]==255) {
          grib_info_f <- grib_info %>% dplyr::filter(
              .data$shortName  == param_info$short_name[i])
        } else {
          grib_info_f <- grib_info %>% dplyr::filter(
              .data$shortName  == param_info$short_name[i],
              (.data$editionNumber == 1 && .data$levelType  == param_info$level_type[j]) ||
              (.data$editionNumber == 2 && .data$levelType  == param_info$level_type_2[j]))
          if (nrow(grib_info_f) >= 1) break;
        }
        if (nrow(grib_info_f) >= 1) break;
      }
    }

    if (param_info$level_type != 255 && param_info$level_number != -999) {
      grib_info_f <- dplyr::filter(grib_info_f, .data$level == param_info$level_number)
    }
#  }

  grib_info <- grib_info_f

  if (nrow(grib_info) == 0) {
    warning(
      "Parameter \"", parameter[["fullname"]], "\" ",
      "(", param_info[["short_name"]], ") not found in grib file.",
      call. = FALSE, immediate. = TRUE
    )
    return(grib_info)
  }
# AD: this should depend on gribEdition, and be careful for length
  grib_info[["level_type"]] <- grib_info[["levelType"]] #parameter[["level_type"]]
  grib_info[["parameter"]]  <- parameter[["fullname"]]

  if (!is.null(lead_time)) {
    grib_info <- dplyr::filter(grib_info, .data[["leadtime"]] %in% lead_time)
    if (nrow(grib_info) == 0) {
      warning(
        "'lead_time' [", paste(lead_time, collapse = ", "), "] not found in grib file.",
        call. = FALSE, immediate. = TRUE
      )
      return(grib_info)
    }
  }

  if (!is.null(members) && !all(is.na(grib_info[["member"]]))) {
    grib_info <- dplyr::filter(grib_info, .data[["member"]] %in% members)
    if (nrow(grib_info) == 0) {
      warning(
        "'members' [", paste(members, collapse = ", "), "] not found in grib file.",
        call. = FALSE, immediate. = TRUE
      )
      return(grib_info)
    }
  }

  grib_info

}

get_domain_grib <- function(file_name, opts) {
  Rgrib2::Gdomain(Rgrib2::Ghandle(file_name))
}

grib_units_to_harp_units <- function(x) {
  switch(
    x,
    "m s**-1" = "m/s",
    "(0 - 1)" = "fraction",
    x
  )
}
