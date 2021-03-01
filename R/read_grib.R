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
    IntPar = c("perturbationNumber", "indicatorOfTypeOfLevel"),
    StrPar = "typeOfLevel",
    multi = format_opts[["multi"]]
  )

  grib_file <- grib_info

  grib_info[["fcdate"]]    <- suppressMessages(
    str_datetime_to_unixtime(
      paste0(
        grib_info$dataDate,
        formatC(grib_info$dataTime, width = 4, flag = "0")
      )
    )
  )

  grib_info[["validdate"]] <- suppressMessages(
    str_datetime_to_unixtime(
      paste0(
        grib_info$validityDate,
        formatC(grib_info$validityTime, width = 4, flag = "0")
      )
    )
  )

  grib_info[["leadtime"]]  <-
    (grib_info[["validdate"]] - grib_info[["fcdate"]]) / 3600

  colnames(grib_info)[colnames(grib_info) == "perturbationNumber"] <- "member"

  # filter_grib_info function defined at end of file
  # For dplyr methods in filter_grib_info the new class has to be after
  class(grib_info) <- rev(class(grib_info))
  grib_info <- purrr::map2_dfr(
    parameter, param_info, filter_grib_info,
    grib_info, lead_time, members, format_opts
  )
  class(grib_info) <- rev(class(grib_info))

  if (nrow(grib_info) < 1) {
    stop(
      "None of the requested data could be read from grib file: ",
      file_name,
      call. = FALSE
    )
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
      level_type   = grib_info$level_type[row_num], #FIXME: this is grib-1 specific
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
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent eta: :eta", total = nrow(grib_info)
    )
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

  # grib_data <- grib_info[c(fcdate, validdate, leadtime....)]
  # if keep_raw_data or transf="none":
  #   grib_data$gridded_data <- lapply(grib_info$position, function(i) Gdec(grib_info, i))
  # else
  #   function(i) transform_geofield(Gdec(...
  # --> hard to have different column names for transformed data

  attr(grib_data, "transformation_opts") <- transformation_opts

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
  show_progress       = FALSE
) {
  # FIXME: grib2 files can contain multiple ensemble members!
  #stop("Grib support for interpolation is not properly implemented yet.", call. = FALSE)

  if (!requireNamespace("Rgrib2", quietly = TRUE)) {
    stop(
      "read_grib requires the Rgrib2 package. Install with the following command:\n",
      "remotes::install_github(\"harphub/Rgrib2\")",
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
    show_progress = show_progress
  )

  list(
    fcst_data = dplyr::transmute(
      fcst_data,
      .data$SID,
      .data$lat,
      .data$lon,
      .data$parameter,
      forecast  = .data$station_data,
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
filter_grib_info <- function(parameter, param_info, grib_info, lead_time, members, opts) {
  #  if (grepl("(?:^mn|^mx|^)[[:digit:]]+[[:alpha:]]", param_info$short_name)) {
  #    grib_info_f <- dplyr::filter(grib_info, .data$shortName == param_info$short_name)
  #  } else {
  # Some parameters may be encoded in two different ways, so we may need a second try
  #  e.g. precip can be on "surface" or "0m above ground")
  # 10m wind speed can be "ws" or "10si" (or even "SP_10M" in DWD files)
  # TODO: for "unknown" shortNames we could try to use parameter number?
  #       that would be useful when we need a local "grib_override"
  # TODO: what if we need 2 component-fields followed by transformation (e.g. wind speed from u & v)
  #       that should be part of the "transformation", but it requires 2 fields, not one.

  param_find <- opts[["param_find"]][[parameter[["fullname"]]]]

  if (is.null(param_find)) {
    param_find <- use_grib_shortName(param_info[["short_name"]])
  }

  level_find <- opts[["level_find"]][[parameter[["fullname"]]]]

  if (is.null(level_find)) {

    level_find_grib1 <- use_grib_key_level(
      "levelType",
      param_info[["level_type"]],
      param_info[["level_number"]]
    )

    level_find_grib2 <- use_grib_key_level(
      "levelType",
      param_info[["level_type_2"]],
      param_info[["level_number"]]
    )

    level_find <- level_find_grib1

  } else {

    level_find_grib1 <- level_find
    level_find_grib2 <- level_find

  }

  for (i in seq_along(param_find[["value"]])) {
    for(j in seq_along(level_find[["value"]])) {

      if (level_find[["value"]][j] == 255 | level_find[["value"]][j] == "unknown") {
        grib_info_f <- grib_info %>% dplyr::filter(
          .data[[param_find[["key"]]]] == param_find[["value"]][i])
        level_type_grib1 <- "unknown"
        level_type_grib2 <- "unknown"

      } else {

        grib_info_f <- grib_info %>% dplyr::filter(
          .data[[param_find[["key"]]]] == param_find[["value"]][i] &
            (
              .data$editionNumber == 1 &
                .data[[level_find_grib1[["key"]]]] == level_find_grib1[["value"]][j]
            ) | (
              .data$editionNumber == 2 &
                .data[[level_find_grib2[["key"]]]] == level_find_grib2[["value"]][j]
            )
        )

      }

      if (nrow(grib_info_f) >= 1) {
        level_type_grib1 <- level_find_grib1[["value"]][j]
        level_type_grib2 <- level_find_grib2[["value"]][j]
        break
      }

    }

    if (nrow(grib_info_f) >= 1) {
      break
    }

  }

  if (
    length(level_find[["value"]]) > 1 ||
      level_find[["value"]] != 255 &&
      level_find[["value"]] != "unknown" &&
      level_find[["level"]] != -999
  ) {

    grib_info_f <- dplyr::filter(
      grib_info_f, .data[["level"]] %in% level_find[["level"]]
    )

  }

  grib_info <- grib_info_f

  if (nrow(grib_info) == 0) {

    warning(
      "Parameter \"", parameter[["fullname"]], "\" ",
      "(", param_find[["key"]], ": \"",
      paste(
        param_find[["value"]], collapse = "\" / \""
      ),
      "\", ", level_find[["key"]], ": \"",
      paste(
        level_find[["value"]], collapse = "\" / \""
      ),
      "\" for level(s) ",
      paste(
        level_find[["level"]], collapse = ","
      ),
      ") not found in grib file.",
      call. = FALSE, immediate. = TRUE
    )

    return(grib_info)

  }
  # AD: this should depend on gribEdition, and be careful for length
  grib_info[["level_type"]] <- grib_edition_level_name(level_type_grib1, 1)
  grib_info[["level_type"]][grib_info[["editionNumber"]] == 2] <-
    grib_edition_level_name(level_type_grib2, 2)
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

grib_edition_level_name <- function(id, edition) {

  if (edition == 1) {
    level_name <- sapply(id, function(x) switch(
      as.character(x),
      "sfc"               = ,
      "surface"           = ,
      "1"                 = "surface",
      "isobaricInhPa"     = ,
      "100"               = "pressure",
      "102"               = ,
      "meanSea"           = ,
      "msl"               = "MSL",
      "103"               = ,
      "heightAboveSea"    = "ASL",
      "105"               = ,
      "heightAboveGround" = "height",
      "107"               = ,
      "sigma"             = "sigma",
      "109"               = ,
      "hybrid"            = "model",
      "4"                 = ,
      "isothermZero"      = "isotherm_zero",
      "20"                = ,
      "isotherm"          = "isotherm",
      "unknown"
    ))

  }

  if (edition == 2) {
    level_name <- sapply(id, function(x) switch(
      as.character(x),
      "sfc"               = ,
      "surface"           = ,
      "1"                 = "surface",
      "isobaricInhPa"     = ,
      "100"               = "pressure",
      "101"               = ,
      "meanSea"           = ,
      "msl"               = "MSL",
      "102"               = ,
      "heightAboveSea"    = "ASL",
      "103"               = ,
      "heightAboveGround" = "height",
      "104"               = ,
      "sigma"             = "sigma",
      "105"               = ,
      "hybrid"            = "model",
      "4"                 = ,
      "isothermZero"      = "isotherm_zero",
      "20"                = ,
      "isotherm"          = "isotherm",
      "unknown"
    ))

  }

  level_name

}
