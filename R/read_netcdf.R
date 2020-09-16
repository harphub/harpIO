read_netcdf <- function(
  file_name,
  parameter,
  lead_time           = NULL,
  members             = NULL,
  vertical_coordinate = NA_character_,
  transformation      = "none",
  transformation_opts = list(),
  format_opts         = netcdf_opts(),
  show_progress       = FALSE
) {

  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    stop("Please install the 'ncdf4' package to read netcdf files.", call. = FALSE)
  }

  if (transformation == "none") transformation_opts[["keep_raw_data"]] <- TRUE

  if (is.null(parameter)) {
    stop("For NetCDF files, parameter = '<parameter>' must be passed.", call. = FALSE)
  }

  if (is.null(format_opts) | length(format_opts) < 1) {
    warning(
      "No 'format_opts' passed for NetCDF file. Using default netcdf_opts()",
      call. = FALSE, immediate. = TRUE
    )
    format_opts <- list()
  }
  format_opts <- do.call(netcdf_opts, format_opts)

  # Convert parameter name to harp parameter and then to netcdf
  parameter  <- lapply(parameter, parse_harp_parameter, vertical_coordinate)
  param_info <- lapply(parameter, get_netcdf_param_info, opts = format_opts, vc = vertical_coordinate)

  # Open the file and get the time and domain information
  nc_id     <- ncdf4::nc_open(file_name)
  time_info <- get_time_nc(nc_id, format_opts)
  nc_domain <- get_domain_netcdf(nc_id, format_opts)

  # Check for variables in the file
  nc_vars        <- names(nc_id$var)
  requested_vars <- sapply(param_info, function(x) x[["nc_param"]])
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
  param_info <- param_info[sapply(param_info, function(x) x[["nc_param"]] %in% nc_vars)]

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
        "Requested dimensions: '", paste(missing_dims, collapse = ", ", " not found in file:\n"),
        file_name, "\nUse netcdf_opts() to set the correct dimension names.",
        call. = FALSE
      )
    }
  }

  # Set up a data frame getting the available levels using opts$z_var to get the levels from the file
  # Do the same if ensemble members are requested.

  nc_info <- lapply(param_info, make_nc_info, time_info, nc_id, file_name)
  if (all(sapply(nc_info, is.null))) {
    stop("Cannot read from netcdf file: ", file_name, call. = FALSE)
  }
  # Filter the lead times and ensemble members

  nc_info <- mapply(filter_nc, nc_info, param_info, MoreArgs = list(lead_time, members), SIMPLIFY = FALSE)
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

  nc_param   <- param[["nc_param"]]
  harp_param <- param[["harp_param"]][["fullname"]]
  nc_dims    <- sort(sapply(nc_id[["var"]][[nc_param]][["dim"]], function(x) x[["name"]]))
  opts_dims  <- sort(stats::na.omit(unlist(
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
    warning(
      "Requested dimensions do not match for '", harp_param, "' in ", file_name, ".\n",
      "Requested dimensions: (", paste(opts_dims, collapse = ","), ")\n",
      "Dimensions in file: (", paste(nc_dims, collapse = ", "),
      call. = FALSE, immediate. = TRUE
    )
    return(NULL)
  }

  if (!is.na(param[["opts"]][["z_var"]])) {
    z_levels <- ncdf4::ncvar_get(nc_id, param[["opts"]][["z_var"]])
    # If there is only one vertical level in the file, assume we are getting the correct one
    if (param[["harp_param"]][["level"]] != -999 && length(z_levels) > 1) {
      z_levels <- z_levels[z_levels == param[["harp_param"]][["level"]]]
      if (length(z_levels) < 1) {
        warning(
          param[["opts"]][["z_var"]], " == ", param[["harp_param"]][["level"]],
          "not found in file: ", file_name,
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

  info_df
}

###

# function to filter available netcdf data to requested lead times and ensemble members

filter_nc <- function(nc_info, param_info, lead_times, members) {

  parameter <- unique(nc_info[["parameter"]])

  if (!is.null(lead_times)) {
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

  func <- function(x, nc_id, nc_info, nc_opts, nc_domain, transformation = "none", opts = list(), show_progress) {

    nc_var_dims <- sapply(nc_id$var[[nc_info$nc_param[x]]]$dim, function(x) x$name)
    x_pos       <- which(nc_var_dims == nc_opts[["x_dim"]])
    y_pos       <- which(nc_var_dims == nc_opts[["y_dim"]])

    geofield_info <- list()
    geofield_info[["name"]] <- paste(
      nc_info[["parameter"]][x],
      ifelse(nc_info[["units"]][x] != "unknown", nc_info[["units"]][x], "")
    )
    geofield_info[["time"]] <- list()

    start <- rep(1, length(stats::na.omit(unlist(nc_opts[c("x_dim", "y_dim", "z_var", "time_var", "member_var")]))))
    count <- rep(-1, length(stats::na.omit(unlist(nc_opts[c("x_dim", "y_dim", "z_var", "time_var", "member_var")]))))

    if (is.element("level", colnames(nc_info))) {
      z_var <- nc_opts[["z_var"]]
      if (grepl("height", nc_opts[["z_var"]])) {
        z_var <- grep(gsub("[[:digit:]]", "", nc_opts[["z_var"]]), nc_var_dims, value = TRUE)
      }
      nc_levels                    <- ncdf4::ncvar_get(nc_id, z_var)
      z_pos                        <- which(nc_var_dims == z_var)
      start[z_pos]                 <- which(nc_levels == nc_info[["level"]][x])
      count[z_pos]                 <- 1
      geofield_info[["level"]]     <- nc_info[["level"]][x]
      geofield_info[["leveltype"]] <- nc_info[["level_type"]][x]
    }

    if (is.element("member", colnames(nc_info))) {
      nc_members                     <- ncdf4::ncvar_get(nc_id, nc_opts[["member_var"]])
      nc_members                     <- as.numeric(gsub("[[:alpha:]]|[[:punct:]]", "", nc_members))
      member_pos                     <- which(nc_var_dims == nc_opts[["member_var"]])
      start[member_pos]              <- which(nc_members == nc_info[["member"]][x])
      count[member_pos]              <- 1
      geofield_info[["member"]]      <- nc_info[["member"]][x]
    }

    geofield_info[["unit"]] <- ifelse(nc_info[["units"]][x] != "unknown", nc_info[["units"]][x], "")

    if (is.element("leadtime", colnames(nc_info))) {
      nc_times      <- ncdf4::ncvar_get(nc_id, nc_opts[["time_var"]])
      time_pos      <- which(nc_var_dims == nc_opts[["time_var"]])

      start[time_pos] <- select_nc_time(
        nc_times,
        nc_info[["time_units"]][x],
        nc_info[["validdate"]][x],
        nc_info[["leadtime"]][x]
      )

      count[time_pos] <- 1

      geofield_info[["time"]][["basedate"]] <- get_basedate(nc_times[1], nc_info[["time_units"]][x])
      geofield_info[["time"]][["start"]]    <- nc_info[["leadtime"]][x] / 3600
      geofield_info[["time"]][["end"]]      <- nc_info[["leadtime"]][x] / 3600
      geofield_info[["time"]][["stepUnit"]] <- "h"
    }

    result <- tibble::tibble(
      fcdate       = ifelse(is.null(nc_info$fcdate[x]), NA_real_, nc_info$fcdate[x]),
      validdate    = ifelse(is.null(nc_info$validdate[x]), NA_real_, nc_info$validdate[x]),
      lead_time    = ifelse(is.null(nc_info$leadtime[x]), NA_real_, nc_info$leadtime[x]),
      parameter    = ifelse(is.null(nc_info$parameter[x]), NA_character_, nc_info$parameter[x]),
      level_type   = ifelse(is.null(nc_info$level_type[x]), NA_character_, nc_info$level_type[x]),
      level        = ifelse(is.null(nc_info$level[x]), NA_real_, nc_info$level[x]),
      units        = ifelse(is.null(nc_info$units[x]), NA_character_, nc_info$units[x]),
      gridded_data = list(
        meteogrid::as.geofield(
          orientate_data(
            ncdf4::ncvar_get(nc_id, nc_info[["nc_param"]][x], start = start, count = count),
            nc_opts
          ),
          domain = nc_domain,
          info   = geofield_info
        )
      )
    )

    if (is.element("member", colnames(nc_info))) {
      result[["members"]] <- nc_info[["member"]][x]
    }

    result <- transform_geofield(result, transformation, opts)

    if (show_progress) pb$tick()

    result
  }

  if (first_only) nc_info <- nc_info[1, ]

  if (show_progress) {
    bar_info <- paste(param_info[["harp_param"]][["fullname"]], "[:bar] :percent eta: :eta")
    pb       <- progress::progress_bar$new(format = bar_info, total = nrow(nc_info))
  }

  purrr::map_dfr(
    1:nrow(nc_info),
    func,
    nc_id,
    nc_info,
    param_info[["opts"]],
    nc_domain,
    transformation,
    opts,
    show_progress
  )

}

select_nc_time <- function(nc_times, time_units, validdate, lead_time) {

  if (grepl(" since", time_units)) {
    origin <- sub("^ ", "", gsub("s[[:alpha:]]+ since", "", time_units))
    which(as.numeric(as.POSIXct(nc_times, tz = "UTC", origin = origin)) == validdate)
  } else if (time_units %in% c("h", "m", "s", "d")) {
    which(nc_times * units_multiplier(time_units) == leadtime)
  } else {
    nc_times <- suppressMessages(str_datetime_to_unixtime(gsub("[[:punct:]]", "", nc_times)))
    which(nc_times == validdate)
  }

}

get_basedate <- function (nc_time, time_units) {
  if (grepl(" since", time_units)) {
    origin <- sub("^ ", "", gsub("s[[:alpha:]]+ since", "", time_units))
    as.POSIXct(nc_time, tz = "UTC", origin = origin)
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
