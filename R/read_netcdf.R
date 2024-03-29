read_netcdf <- function(
    file_name,
    parameter,
    is_forecast         = TRUE,
    date_times          = NULL,
    lead_time           = NULL,
    members             = NULL,
    vertical_coordinate = NA_character_,
    transformation      = "none",
    transformation_opts = list(),
    format_opts         = netcdf_opts(),
    param_defs          = get("harp_params"),
    show_progress       = FALSE,
    ...
) {

  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("Please install the 'ncdf4' package to read netcdf files.", call. = FALSE)
  }

  if (transformation == "none") transformation_opts[["keep_raw_data"]] <- TRUE

  if (is.null(parameter)) {
    stop("For NetCDF files, parameter = '<parameter>' must be passed.", call. = FALSE)
  }

  if (is.null(format_opts) | length(format_opts) < 1) {
    # warning(
    #   "No 'format_opts' passed for NetCDF file. Using default netcdf_opts()",
    #   call. = FALSE, immediate. = TRUE
    # )
    format_opts <- netcdf_opts()
  }

  check_param_defs(param_defs)

  # Convert parameter name to harp parameter and then to netcdf
  if (inherits(parameter, "harp_parameter")) {
    parameter <- list(parameter)
  }
  if (length(parameter) == 1 && !is.null(format_opts[["param_find"]])) {
    if (length(format_opts[["param_find"]]) == 1 && is.null(names(format_opts[["param_find"]]))) {
      if (!is.list(format_opts[["param_find"]])) {
        format_opts[["param_find"]] <- list(format_opts[["param_find"]])
      }
      if (inherits(parameter, "harp_parameter")) {
        names(format_opts[["param_find"]]) <- parameter[["fullname"]]
      } else {
        names(format_opts[["param_find"]]) <- parameter
      }
    }
  }
  parameter  <- lapply(parameter, parse_harp_parameter, vertical_coordinate)
  param_info <- lapply(
    parameter, get_netcdf_param_info, opts = format_opts,
    vc = vertical_coordinate, param_defs = param_defs
  )

  # Open the file and get the time and domain information
  nc_id     <- ncdf4::nc_open(file_name)
  time_info <- get_time_nc(nc_id, format_opts)
  nc_domain <- get_domain_netcdf(nc_id, format_opts)

  # Check for variables in the file
  nc_vars        <- names(nc_id$var)
  requested_vars <- unlist(lapply(param_info, function(x) x[["nc_param"]]))
  missing_vars   <- setdiff(requested_vars, nc_vars)
  warning_func   <- function(x) {
    index      <- which(requested_vars == x)
    harp_param <- parameter[[index]][["fullname"]]
    nc_param   <- requested_vars[index]
    if (nc_param == harp_param) {
      warning_text <- paste0("Parameter: '", harp_param, "'")
    } else {
      warning_text <- paste0("Parameter: '", harp_param, "' ('", nc_param, "')")
    }
    warning(warning_text, " not found in file: ", file_name, call. = FALSE, immediate. = TRUE)
  }

  if (length(missing_vars) > 0) {
    lapply(missing_vars, warning_func)
  }

  if (length(missing_vars) >= length(requested_vars)) {
    stop("None of the requested parameters found in file: ", file_name, call. = FALSE)
  }

  nc_vars    <- intersect(nc_vars, requested_vars)
  param_info <- param_info[sapply(param_info, function(x) all(unlist(x[["nc_param"]]) %in% nc_vars))]

  nc_dims        <- names(nc_id$dim)
  requested_dims <- unique(stats::na.omit(unlist(lapply(
    param_info, function(x) x[["opts"]][c("x_dim", "y_dim", "z_var", "member_var", "time_var")]
  ))))
  missing_dims   <- setdiff(requested_dims, nc_dims)
  if (length(missing_dims) > 0) {
    # Check if the requested dimension is just a variable (e.g. Times for WRF)
    missing_dims <- setdiff(missing_dims, names(nc_id$var))
    if (length(missing_dims) > 0) {
      stop(
        "Requested dimensions: '", paste0(missing_dims, collapse = ", ", "' not found in file:\n"),
        file_name, "\nUse netcdf_opts() to set the correct dimension names.",
        call. = FALSE
      )
    }
  }

  # Set up a data frame getting the available levels using opts$z_var to get the levels from the file
  # Do the same if ensemble members are requested.

  nc_info <- lapply(param_info, make_nc_info, time_info, nc_id, file_name)

  param_info <- lapply(nc_info, function(x) x[[2]])
  nc_info    <- lapply(nc_info, function(x) x[[1]])

  if (all(sapply(nc_info, is.null))) {
    stop("Cannot read from netcdf file: ", file_name, call. = FALSE)
  }
  # Filter the lead times and ensemble members

  nc_info <- mapply(
    filter_nc, nc_info, param_info,
    MoreArgs = list(date_times, lead_time, members, is_forecast),
    SIMPLIFY = FALSE
  )
  nc_info <- nc_info[sapply(nc_info, function(x) nrow(x) > 0)]
  if (length(nc_info) < 1) {
    stop("None of the requested data could be read from netcdf file: ", file_name, call. = FALSE)
  }

  # Get weights for transformations

  transformation_opts <- compute_transformation_weights(
    nc_domain,
    transformation,
    transformation_opts
  )

  # Read and transform the data

  first_only <- FALSE
  if (!is.null(format_opts[["first_only"]]) && format_opts[["first_only"]]) {
    first_only <- TRUE
  }

  # Add row index to identify rows that should be read together so that a
  # function can be applied

  index_func <- function(.df) {
    if (is.element("func_var", colnames(.df))) {
      .df <- dplyr::ungroup(
        dplyr::mutate(
          dplyr::group_by(.df, .data[["func_var"]]),
          index = seq_len(dplyr::n())
        )
      )
    } else {
      .df[["index"]] <- seq_len(nrow(.df))
    }
    .df
  }

  nc_info <- lapply(nc_info, index_func)

  result <- purrr::map2_dfr(
    nc_info,
    param_info,
    read_and_transform_netcdf,
    nc_id,
    nc_domain,
    transformation,
    transformation_opts,
    first_only,
    show_progress
  )

  ncdf4::nc_close(nc_id)

  if (is.null(lead_time)) {
    result[["lead_time"]] = sapply(
      paste0(result[["lead_time"]], "s"),
      char_to_time,
      "leadtime",
      "h"
    )
  } else {
    if (is.numeric(lead_time)) lead_time = paste0(lead_time, "h")
    data_leads <- sapply(lead_time, char_to_time, "")
    result[["lead_time"]] <- as.numeric(
      gsub(
        "[[:alpha:]]",
        "",
        as.character(
          factor(result[["lead_time"]], levels = data_leads, labels = names(data_leads)))
      )
    )
  }

  attr(result, "transformation_opts") <- transformation_opts

  result

}

###

# function to check that the dimension names are correct for each parameter,
# extract and filter the vertical levels from the file and extract the
# ensemble members for later filtering.

make_nc_info <- function(param, info_df, nc_id, file_name) {

  nc_param    <- param[["nc_param"]]
  harp_param  <- param[["harp_param"]]

  nc_info_for_param <- lapply(nc_param, get_nc_info, param, nc_id, info_df)

  if (is.null(names(nc_param))) {
    return(nc_info_for_param[[1]])
  }

  nc_info_for_param <- mapply(
    function(x, y) {
      x[[1]][["func_var"]] = y
      x
    },
    nc_info_for_param,
    names(nc_param),
    SIMPLIFY = FALSE
  )

  list(
    purrr::map_dfr(nc_info_for_param, 1),
    lapply(nc_info_for_param, function(x) x[[2]])
  )

}

# Function to get nc info for a harp parameter
get_nc_info <- function(nc_param, param, nc_id, info_df) {
  nc_dims_raw <- sapply(nc_id[["var"]][[nc_param]][["dim"]], function(x) x[["name"]])
  param[["opts"]] <- dimnames_from_var(param[["opts"]], nc_dims_raw)
  nc_dims     <- sort(nc_dims_raw)
  opts_dims   <- sort(stats::na.omit(unlist(
    param[["opts"]][c("x_dim", "y_dim", "z_var", "member_var", "time_var")], use.names = FALSE
  )))


  # For WRF, variable dimensions aren't used. Therefore we need to check if the dimensions
  # for opts_dims match nc_dims

  missing_dims <- setdiff(opts_dims, nc_dims)
  if (length(missing_dims) > 0) {
    var_dims <- lapply(
      missing_dims,
      function(x) sapply(nc_id[["var"]][[x]][["dim"]], function(y) y[["name"]])
    )

    var_dims <- lapply(var_dims, function(x) x[x != "DateStrLen"])
    if (all(sapply(var_dims, length) == 1)) {
      nc_dims <- sort(mapply(function(x, y, z) {z[z == x] <- y; z}, var_dims, missing_dims, nc_dims))
    }
  }


  if (!identical(nc_dims, opts_dims)) {
    warn_dims <- TRUE
    if (length(missing_dims) == 1) {
      missing_nc_dim <- nc_dims[opts_dims == missing_dims]
      idx            <- which(nc_dims_raw == missing_nc_dim)
      if (nc_id[["var"]][[nc_param]][["dim"]][[idx]][["len"]] == 1) {
        dim_to_rename <- names(which(param[["opts"]] == missing_dims))
        param[["opts"]][[dim_to_rename]] <- missing_nc_dim
        warn_dims <- FALSE
      } else if (gsub("[[:digit:]]", "", missing_dims) == gsub("[[:digit:]]", "", missing_nc_dim)) {
        param[["opts"]][[names(which(param[["opts"]] == missing_dims))]] <- missing_nc_dim
        warn_dims <- FALSE
      }
    }
    if (warn_dims) {
      warning(
        "Requested dimensions do not match for '", param[["harp_param"]][["fullname"]],
        "' in ", nc_id[["filename"]], ".\n",
        "Requested dimensions: (", paste(opts_dims, collapse = ", "), ")\n",
        "Dimensions in file: (", paste(nc_dims, collapse = ", "), ").",
        call. = FALSE, immediate. = TRUE
      )
      return(NULL)
    }
  }

  if (!is.na(param[["opts"]][["z_var"]])) {
    z_levels <- ncdf4::ncvar_get(nc_id, param[["opts"]][["z_var"]])
    # If there is only one vertical level in the file, assume we are getting the correct one
    if (param[["harp_param"]][["level"]] != -999 && length(z_levels) > 1) {
      z_levels <- z_levels[z_levels == param[["harp_param"]][["level"]]]
      if (length(z_levels) < 1) {
        warning(
          param[["opts"]][["z_var"]], " == ", param[["harp_param"]][["level"]],
          "not found in file: ", nc_id[["filename"]],
          call. = FALSE, immediate. = TRUE
        )
        return(NULL)
      }
    }
  } else {
    z_levels <- NULL
  }

  info_df[["parameter"]]  <- param[["harp_param"]][["fullname"]]
  info_df[["nc_param"]]   <- nc_param
  info_df[["level_type"]] <- param[["harp_param"]][["level_type"]]
  if (!is.null(z_levels) && param[["opts"]][["z_var"]] != "surface") {
    info_df[["level"]] <- list(z_levels)
    info_df            <- tidyr::unnest(info_df, .data[["level"]])
  }
  if (!is.na(param[["opts"]][["member_var"]])) {
    info_df[["member"]] <- list(ncdf4::ncvar_get(nc_id, param[["opts"]][["member_var"]]))
    info_df             <- tidyr::unnest(info_df, .data[["member"]])
  }

  param_units <- ncdf4::ncatt_get(nc_id, nc_param, "units")
  if (param_units[["hasatt"]]) {
    info_df[["units"]] <- param_units[["value"]]
  } else {
    info_df[["units"]] <- "unknown"
  }

  list(info_df, param)
}


###

# function to filter available netcdf data to requested lead times and ensemble members

filter_nc <- function(nc_info, param_info, date_times, lead_times, members, is_forecast) {

  parameter <- unique(nc_info[["parameter"]])

  if (!is.null(date_times)) {

    date_col <- ifelse(is_forecast, "fcdate", "validdate")
    missing_date_times <- which(!date_times %in% nc_info[[date_col]])
    if (length(missing_date_times) > 0) {
      missing_date_times <- date_times[missing_date_times]
      date_times         <- date_times[-missing_date_times]
      warning(
        "date_times: ", paste(missing_date_times, collapse = ","),
        " for '", parameter, "' not found in file.",
        call. = FALSE, immediate. = TRUE
      )
    }
    nc_info <- dplyr::filter(nc_info, .data[[date_col]] %in% date_times)
    if (nrow(nc_info) < 1) {
      warning(
        "None of the requested date_times were found for '", parameter, "' in file.",
        call. = FALSE, immediate. = TRUE
      )
      return(nc_info)
    }
  }

  if (!is.null(lead_times) && is_forecast) {
    if (is.numeric(lead_times)) { # assume in hours
      lead_times <- paste0(lead_times, "h")
    }
    requested_lead_times <- sapply(
      lead_times,
      function(x) readr::parse_number(x) * units_multiplier(x),
      USE.NAMES = FALSE
    )
    missing_lead_times <- which(!requested_lead_times %in% nc_info[["leadtime"]])
    if (length(missing_lead_times) > 0) {
      missing_leads <- lead_times[missing_lead_times]
      lead_times    <- lead_times[-missing_lead_times]
      warning(
        "Lead times: ", paste(missing_leads, collapse = ","), " for '", parameter, "' not found in file.",
        call. = FALSE, immediate. = TRUE
      )
    }
    nc_info <- dplyr::filter(nc_info, .data[["leadtime"]] %in% requested_lead_times)
    if (nrow(nc_info) < 1) {
      warning(
        "None of the requested lead times were found for '", parameter, "' in file.",
        call. = FALSE, immediate. = TRUE
      )
      return(nc_info)
    }
  }

  if (!is.null(members)) {
    if (is.element("member", colnames(nc_info))) {
      nc_info[["mbr"]] <- as.numeric(gsub("[[:alpha:]]|[[:punct:]]", "", nc_info[["member"]]))
      missing_members <- which(!members %in% nc_info[["mbr"]])
      if (length(missing_members) > 0) {
        warning(
          "Members: ", paste(members, collapse = ","), " for '", parameter, "' not found in file.",
          call. = FALSE, immediate. = TRUE
        )
      }
      nc_info <- dplyr::filter(nc_info, .data[["mbr"]] %in% members)
      nc_info <- nc_info[, colnames(nc_info) != "mbr"]
    } else {
      warning(
        "Ensemble members were requested for '", parameter, "' but there is no member information.",
        call. = FALSE, immediate. = TRUE
      )
    }
    if (nrow(nc_info) < 1) {
      warning(
        "None of the requested ensemble members were found for '", parameter, "' in file.",
        call. = FALSE, immediate. = TRUE
      )
      return(nc_info)
    }
  }

  if (!is.null(param_info[["level"]]) && !is.na(param_info[["level"]]) && param_info[["level"]] != -999) {
    if (is.element("level", colnames(nc_info))) {
      missing_levels <- which(!param_info[["level"]] %in% nc_info[["level"]])
      if (length(missing_members) > 0) {
        warning(
          "Levels: ", paste(members, collapse = ","), " for '", parameter, "' not found in file.",
          call. = FALSE, immediate. = TRUE
        )
      }
      nc_info <- dplyr::filter(nc_info, .data[["level"]] %in% param_info[["level"]])
    } else {
      warning(
        "Vertical levels were requested for '", parameter, "' but there is no vertical level information.",
        call. = FALSE, immediate. = TRUE
      )
    }
    if (nrow(nc_info) < 1) {
      warning(
        "None of the requested vertical levels were found for '", parameter, "' in file.",
        call. = FALSE, immediate. = TRUE
      )
      return(nc_info)
    }
  }

  nc_info

}

###

# Function to read and transform netcdf data

read_and_transform_netcdf <- function(
    nc_info, param_info, nc_id, nc_domain, transformation, opts, first_only, show_progress
) {

  func <- function(nc_info, nc_id, nc_opts, nc_domain, transformation = "none", opts = list()) {

    geofields <- list()

    if (is.element("harp_param", names(nc_opts))) {
      nc_opts <- list(nc_opts)
    }

    for (i in 1:nrow(nc_info)) {

      file_opts <- nc_opts[[i]][["opts"]]

      nc_var_dims <- sapply(nc_id$var[[nc_info$nc_param[i]]]$dim, function(x) x$name)
      x_pos       <- which(nc_var_dims == file_opts[["x_dim"]])
      y_pos       <- which(nc_var_dims == file_opts[["y_dim"]])

      geofield_info <- list()
      geofield_info[["name"]] <- paste(
        nc_info[["parameter"]][i],
        ifelse(nc_info[["units"]][i] != "unknown", nc_info[["units"]][i], "")
      )
      geofield_info[["time"]] <- list()

      start <- rep(1, length(stats::na.omit(unlist(file_opts[c("x_dim", "y_dim", "z_var", "time_var", "member_var")]))))
      count <- rep(-1, length(stats::na.omit(unlist(file_opts[c("x_dim", "y_dim", "z_var", "time_var", "member_var")]))))

      if (is.element("level", colnames(nc_info))) {
        z_var <- file_opts[["z_var"]]
        if (grepl("height", file_opts[["z_var"]])) {
          z_var <- grep(gsub("[[:digit:]]", "", file_opts[["z_var"]]), nc_var_dims, value = TRUE)
        }
        nc_levels                    <- ncdf4::ncvar_get(nc_id, z_var)
        z_pos                        <- which(nc_var_dims == z_var)
        start[z_pos]                 <- which(nc_levels == nc_info[["level"]][i])
        count[z_pos]                 <- 1
        geofield_info[["level"]]     <- nc_info[["level"]][i]
        geofield_info[["leveltype"]] <- nc_info[["level_type"]][i]
      }

      if (is.element("member", colnames(nc_info))) {
        nc_members                     <- ncdf4::ncvar_get(nc_id, file_opts[["member_var"]])
        nc_members                     <- as.numeric(gsub("[[:alpha:]]|[[:punct:]]", "", nc_members))
        member_pos                     <- which(nc_var_dims == file_opts[["member_var"]])
        start[member_pos]              <- which(nc_members == nc_info[["member"]][i])
        count[member_pos]              <- 1
        geofield_info[["member"]]      <- nc_info[["member"]][i]
      }

      geofield_info[["unit"]] <- ifelse(nc_info[["units"]][i] != "unknown", nc_info[["units"]][i], "")

      if (is.element("leadtime", colnames(nc_info))) {
        nc_times      <- ncdf4::ncvar_get(nc_id, file_opts[["time_var"]])
        time_pos      <- which(nc_var_dims == file_opts[["time_var"]])

        # WRF uses Times to store the time data, but Time as the dimension
        if (length(time_pos) == 0) {
          time_pos <- which(nc_var_dims == sub("s$", "", file_opts[["time_var"]]))
        }
        if (length(time_pos) == 0) {
          stop("Cannot find position of the time dimension", call. = FALSE)
        }

        start[time_pos] <- select_nc_time(
          nc_times,
          nc_info[["time_units"]][i],
          nc_info[["validdate"]][i],
          nc_info[["leadtime"]][i]
        )

        count[time_pos] <- 1

        geofield_info[["time"]][["basedate"]] <- get_basedate(nc_times[1], nc_info[["time_units"]][i])
        geofield_info[["time"]][["start"]]    <- nc_info[["leadtime"]][i] / 3600
        geofield_info[["time"]][["end"]]      <- nc_info[["leadtime"]][i] / 3600
        geofield_info[["time"]][["stepUnit"]] <- "h"
      }

      geofields[[i]] <- meteogrid::as.geofield(
        orientate_data(
          ncdf4::ncvar_get(nc_id, nc_info[["nc_param"]][i], start = start, count = count),
          file_opts
        ),
        domain = nc_domain,
        info   = geofield_info
      )

    }

    if (length(geofields) > 1) {
      names(geofields) <- nc_info[["func_var"]]
      nc_geofield <- do.call(nc_opts[[1]][["func"]], geofields)
    } else {
      nc_geofield <- geofields[[1]]
    }

    result <- tibble::tibble(
      fcdate       = ifelse(is.null(unique(nc_info[["fcdate"]])), NA_real_, unique(nc_info[["fcdate"]])),
      validdate    = ifelse(is.null(unique(nc_info[["validdate"]])), NA_real_, unique(nc_info[["validdate"]])),
      lead_time    = ifelse(is.null(unique(nc_info[["leadtime"]])), NA_real_, unique(nc_info[["leadtime"]])),
      parameter    = ifelse(is.null(unique(nc_info[["parameter"]])), NA_character_, unique(nc_info[["parameter"]])),
      level_type   = ifelse(is.null(unique(nc_info[["level_type"]])), NA_character_, unique(nc_info[["level_type"]])),
      level        = ifelse(is.null(unique(nc_info[["level"]])), NA_real_, unique(nc_info[["level"]])),
      units        = ifelse(is.null(unique(nc_info[["units"]])), NA_character_, unique(nc_info[["units"]])),
      gridded_data = list(nc_geofield)
    )

    if (is.element("member", colnames(nc_info))) {
      result[["members"]] <- unique(nc_info[["member"]])
    }

    result <- transform_geofield(result, transformation, opts)

    result
  }

  if (first_only && nrow(nc_info) > 0) nc_info <- nc_info[1, ]

  if (show_progress) {
    show_progress <- list(
      name = cli::col_yellow("Reading nc file"),
      show_after = 1
    )
  }
  nc_info <- split(nc_info, nc_info[["index"]])

  purrr::map(
    nc_info,
    func,
    nc_id,
    param_info,
    nc_domain,
    transformation,
    opts,
    .progress = show_progress
  ) %>%
    purrr::list_rbind()

}

select_nc_time <- function(nc_times, time_units, validdate, lead_time) {

  if (grepl(" since", time_units)) {
    origin <- sub("^ ", "", gsub("[[:alpha:]]+ since", "", time_units))
    time_x <- substring(time_units, 1, 1)
    which(
      as.numeric(as.POSIXct(
        nc_times * units_multiplier(time_x),
        tz     = "UTC",
        origin = origin
      )) == validdate
    )
  } else if (time_units %in% c("h", "m", "s", "d")) {
    which(nc_times * units_multiplier(time_units) == leadtime)
  } else {
    nc_times <- suppressMessages(str_datetime_to_unixtime(gsub("[[:punct:]]", "", nc_times)))
    which(nc_times == validdate)
  }

}

get_basedate <- function(nc_time, time_units) {
  if (grepl(" since", time_units)) {
    origin <- sub("^ ", "", gsub("[[:alpha:]]+ since", "", time_units))
    time_x <- substring(time_units, 1, 1)
    as.POSIXct(
      nc_time * units_multiplier(time_x), tz = "UTC", origin = origin
    )
  } else if (time_units %in% c("h", "m", "s", "d")) {
    paste(nc_time, time_units)
  } else {
    nc_time <- suppressMessages(str_datetime_to_unixtime(gsub("[[:punct:]]", "", nc_time)))
    unix2datetime(nc_time)
  }

}

orientate_data <- function(x, opts) {
  if (opts[["x_rev"]]) x <- x[nrow(x):1, ]
  if (opts[["y_rev"]]) x <- x[, ncol(x):1]
  x
}

dimnames_from_var <- function(opts, var_dims) {

  possible_dim_names <- list(
    x_dim      = c("x", "longitude", "lon", "long"),
    y_dim      = c("y", "latitude", "lat"),
    z_var      = c(
      "t", "surface", "height", "pressure", "model", "hybrid", "sigma"
    ),
    time_var   = c("time", "date", "datetime"),
    member_var = c("ensemble_member", "member", "mbr")
  )

  for (dim_name in names(possible_dim_names)) {

    dim_index <- which(
      parse_dimname(var_dims) %in% possible_dim_names[[dim_name]]
    )
    if (length(dim_index) == 1) {
      opts[[dim_name]] <- var_dims[dim_index]
    }

  }

  opts

}

parse_dimname <- function(x) {
  gsub("\\d", "", x)
}
