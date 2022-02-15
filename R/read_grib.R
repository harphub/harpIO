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
  is_forecast         = TRUE,
  date_times          = NULL,
  lead_time           = NULL,
  members             = NULL,
  vertical_coordinate = NA_character_,
  transformation      = "none",
  transformation_opts = list(),
  format_opts         = grib_opts(),
  show_progress       = FALSE,
  ...
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
  unknown_params <- which(sapply(param_info, function(x) any(is.na(x$short_name))))

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
    IntPar = c("perturbationNumber", "indicatorOfTypeOfLevel", "paramId"),
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
  # For dplyr methods in filter_grib_info the "GRIBlist" class
  # has to come before the "data.frame" class
  class(grib_info) <- rev(class(grib_info))
  grib_info <- purrr::map2_dfr(
    parameter, param_info, filter_grib_info,
    grib_info, date_times, lead_time, members, is_forecast, format_opts
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
    grib_info,
    file_name,
    format_opts,
    transformation = "none",
    opts           = list(),
    show_progress  = FALSE
  ) {

    result <- tibble::tibble(
      fcdate       = unique(grib_info[["fcdate"]]),
      validdate    = unique(grib_info[["validdate"]]),
      lead_time    = unique(grib_info[["leadtime"]]),
      parameter    = unique(grib_info[["parameter"]]),
      members      = unique(grib_info[["member"]]),
      level_type   = unique(grib_info[["level_type"]]),
      level        = unique(grib_info[["level"]]),
      units        = grib_units_to_harp_units(unique(grib_info[["units"]])),
      gridded_data = list(lapply(
        grib_info[["position"]],
        function(x) Rgrib2::Gdec(
          file_name,
          x,
          get.meta  = format_opts[["meta"]],
          multi     = format_opts[["multi"]]
        )
      ))
    )

    func <- unique(grib_info[["func"]])

    if (is.na(func)) {
      result[["gridded_data"]][[1]] <- result[["gridded_data"]][[1]][[1]]
    } else {
      func <- func[[1]]
      if (is.null(grib_info[["func_var"]])) {
        result[["gridded_data"]][[1]] <- func(
          as_geolist(result[["gridded_data"]][[1]])
        )
      } else {
        names(result[["gridded_data"]][[1]]) <- grib_info[["func_var"]]
        result[["gridded_data"]][[1]] <- do.call(
          func, result[["gridded_data"]][[1]]
        )
      }

      if (!meteogrid::is.geofield(result[["gridded_data"]][[1]])) {
        stop(
          "`func` must return a single geofield", call. = FALSE
        )
      }

    }

    result <- transform_geofield(result, transformation, opts)

    if (show_progress) pb$tick()

    result

  }

  grib_info <- split(
    grib_info, paste(grib_info[["parameter"]], grib_info[["id"]])
  )

  if (show_progress) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent eta: :eta", total = length(grib_info)
    )
  }

  grib_data <- purrr::map_dfr(
    grib_info,
    read_and_transform_grib,
    grib_file,
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


#####

# Function to get the grib information for parameters
filter_grib_info <- function(
  parameter, param_info, grib_info, date_times,
  lead_time, members, is_forecast, opts
) {

  param_finds <- opts[["param_find"]][[parameter[["fullname"]]]]

  if (is.null(param_finds)) {
    if (is.list(param_info[["short_name"]])) {
      param_finds <- lapply(param_info[["short_name"]], use_grib_shortName)
    } else {
      param_finds <- list(use_grib_shortName(param_info[["short_name"]]))
    }
  }

  level_find <- opts[["level_find"]][[parameter[["fullname"]]]]

  if (is.null(level_find)) {

    level_find <- use_grib_key_level(
      "typeOfLevel",
      param_info[["level_type"]],
      param_info[["level_number"]]
    )

  }

  get_grib_info_df <- function(param_find) {

    if (!is.element(param_find[["key"]], colnames(grib_info))) {
      stop(
        "`", param_find[["key"]], "` not found in grib keys for file.",
        call. = FALSE
      )
    }

    if (!is.element(level_find[["key"]], colnames(grib_info))) {
      stop(
        "`", level_find[["key"]], "` not found in grib keys for file.",
        call. = FALSE
      )
    }

    single_surfaces <- c(
      "surface", "meanSea", "isothermZero", "tropopause", "cloudBase",
      "cloudTop", "entireAtmosphere"
    )

    for (i in seq_along(param_find[["value"]])) {
      for (j in seq_along(level_find[["value"]])) {

        if (
          level_find[["value"]][j] == 255 |
            level_find[["value"]][j] == "unknown"
        ) {
          grib_info_f <- grib_info %>% dplyr::filter(
            .data[[param_find[["key"]]]] == param_find[["value"]][i])
          level_type <- "unknown"

        } else {

          grib_info_f <- grib_info %>% dplyr::filter(
            .data[[param_find[["key"]]]] == param_find[["value"]][i] &
              .data[[level_find[["key"]]]] == level_find[["value"]][j]
          )

          if (
            !level_find[["value"]] %in% single_surfaces &&
              level_find[["level"]] != -999
          ) {
            grib_info_f <- dplyr::filter(
              grib_info_f, .data[["level"]] %in% level_find[["level"]]
            )
          }
        }

        if (nrow(grib_info_f) >= 1) {
          level_type <- level_find[["value"]][j]
          break
        }

      }

      if (nrow(grib_info_f) >= 1) {
        break
      }

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

    }

    list(
      grib_info = grib_info, level_type = level_type
    )

  }

  grib_info  <- purrr::map(param_finds, get_grib_info_df)
  level_type <- unique(purrr::map_chr(grib_info, "level_type"))

  if (is.null(names(grib_info))) {
    grib_info <- purrr::map_dfr(grib_info, "grib_info")
  } else {
    grib_info <- purrr::map_dfr(grib_info, "grib_info", .id = "func_var")
  }

  if (nrow(grib_info) < 1) {
    return(grib_info)
  }

  if (level_type == "unknown") {
    level_type <- paste(unique(grib_info[["typeOfLevel"]]), collapse = ",")
  }
  if (nchar(level_type) < 1) {
    level_type <- "unknown"
  }
  level_type <- names(grib_level_types())[grib_level_types() == level_type]
  if (length(level_type) < 1) {
    level_type <- "unknown"
  }
  grib_info[["level_type"]] <- level_type[1]



  grib_info[["parameter"]]  <- parameter[["fullname"]]

  if (is.function(param_info[["func"]])) {
    grib_info[["func"]] <- list(param_info[["func"]])
  } else {
    grib_info[["func"]] <- param_info[["func"]]
  }

  if (!is.null(lead_time) && is_forecast) {
    grib_info <- dplyr::filter(grib_info, .data[["leadtime"]] %in% lead_time)
    if (nrow(grib_info) == 0) {
      warning(
        "'lead_time' [", paste(lead_time, collapse = ", "), "] not found in grib file.",
        call. = FALSE, immediate. = TRUE
      )
      return(grib_info)
    }
  }

  if (!is.null(date_times)) {
    date_col <- ifelse(is_forecast, "fcdate", "validdate")
    grib_info <- dplyr::filter(grib_info, .data[[date_col]] %in% date_times)
    if (nrow(grib_info) == 0) {
      warning(
        "'date_times' [", paste(date_times, collapse = ", "), "] not found in grib file.",
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

  row_id <- dplyr::pull(
    dplyr::mutate(
      dplyr::group_by(
        grib_info,
        dplyr::across(
          dplyr::matches("date|time|level|member")
        ),
        .data[["func"]]
      ),
      id = dplyr::cur_group_id()
    ),
    .data[["id"]]
  )

  grib_info[["id"]] <- row_id

  grib_info
}

get_domain_grib <- function(file_name, opts) {
  Rgrib2::Gdomain(Rgrib2::Ghandle(file_name))
}

grib_units_to_harp_units <- function(x) {
  switch(
    x,
    "m s**-1"    = "m/s",
    "m**2 s**-2" = "m^2/s^2",
    "(0 - 1)"    = "fraction",
    "kg m**-2"   = "kg/m^2",
    x
  )
}


