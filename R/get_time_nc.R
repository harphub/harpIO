# Get the time information from a netcdf file. If no units are given for the
# time data in the file, it is assumed that they are date-time strings.

get_time_nc <- function(file, opts) {

  if (inherits(file, "ncdf4")) {
    nc_id      <- file
    was_closed <- FALSE
  } else {
    nc_id      <- ncdf4::nc_open(file)
    was_closed <- TRUE
  }

  # Check if there is a forecast reference time
  if (!is.na(opts[["ref_time_var"]])) {
    if (!is.element(opts[["ref_time_var"]], names(nc_id$var))) {
      stop("ref_time_var: '", opts[["ref_time_var"]], "' not found in file.", call. = FALSE)
    }
    ref_time       <- ncdf4::ncvar_get(nc_id, opts[["ref_time_var"]])
    ref_time_units <- ncdf4::ncatt_get(nc_id, opts[["ref_time_var"]], "units")
    if (ref_time_units[["hasatt"]]) {
      if (grepl(" since", ref_time_units[["value"]])) {
        origin   <- sub("^ ", "", gsub("[[:alpha:]]+ since", "", ref_time_units[["value"]]))
        ref_time <- ref_time * units_multiplier(substr(ref_time_units[["value"]], 1, 1))
        ref_time <- as.numeric(as.POSIXct(ref_time, tz = "UTC", origin = origin))
      }
    } else {
      ref_time <- gsub("[[:punct:]]", "", ref_time)
      ref_time <- gsub(" ", "", ref_time)
      ref_time <- suppressMessages(str_datetime_to_unixtime(ref_time))
    }
  } else {
    if (!is.element(opts[["time_var"]], union(names(nc_id$var), names(nc_id$dim)))) {
      stop("time_var: '", opts[["time_var"]], "' not found in file.", call. = FALSE)
    }
    time_start <- 1
    time_count <- 1
    # WRF stores times in a weird way!
    if (is.element(opts[["time_var"]], names(nc_id[["var"]])) && nc_id[["var"]][[opts[["time_var"]]]][["ndims"]] == 2) {
      time_start <- c(1, 1)
      time_count <- c(-1, 1)
    }
    ref_time       <- ncdf4::ncvar_get(nc_id, opts[["time_var"]], start = time_start, count = time_count)
    ref_time_units <- ncdf4::ncatt_get(nc_id, opts[["time_var"]], "units")
    if (ref_time_units[["hasatt"]]) {
      if (grepl(" since", ref_time_units[["value"]])) {
        origin   <- sub("^ ", "", gsub("[[:alpha:]]+ since", "", ref_time_units[["value"]]))
        ref_time <- ref_time * units_multiplier(substr(ref_time_units[["value"]], 1, 1))
        ref_time <- as.numeric(as.POSIXct(ref_time, tz = "UTC", origin = origin))
      } else {
        if (grepl("hour", ref_time_units[["value"]])) {
          ref_time = ref_time * 3600
        }
        if (grepl("minute", ref_time_units[["value"]])) {
          ref_time = ref_time * 60
        }
        if (grepl("day", ref_time_units[["value"]])) {
          ref_time = ref_time * 3600 * 24
        }
      }
    } else {
      ref_time <- gsub("[[:punct:]]", "", ref_time)
      ref_time <- gsub(" ", "", ref_time)
      ref_time <- suppressMessages(str_datetime_to_unixtime(ref_time))
    }
  }

  # Get the lead times
  if (!is.element(opts[["time_var"]], union(names(nc_id$var), names(nc_id$dim)))) {
    return(data.frame(fcdate = ref_time, leadtime = 0))
  }
  lead_time       <- ncdf4::ncvar_get(nc_id, opts[["time_var"]])
  lead_time_units <- ncdf4::ncatt_get(nc_id, opts[["time_var"]], "units")
  if (lead_time_units[["hasatt"]]) {
    if (grepl(" since", lead_time_units[["value"]])) {
      origin     <- sub("^ ", "", gsub("[[:alpha:]]+ since", "", lead_time_units[["value"]]))
      lead_time  <- lead_time * units_multiplier(substr(lead_time_units[["value"]], 1, 1))
      lead_time  <- as.numeric(as.POSIXct(lead_time, tz = "UTC", origin = origin)) - ref_time
      time_units <- lead_time_units[["value"]]
    } else {
      if (grepl("hour", lead_time_units[["value"]])) {
        lead_time  <- lead_time * 3600 - ref_time
        time_units <- "h"
      }
      if (grepl("min", lead_time_units[["value"]])) {
        lead_time  <- lead_time * 60 - ref_time
        time_units <- "m"
      }
      if (grepl("sec", lead_time_units[["value"]]) && !grepl("since", lead_time_units[["value"]])) {
        lead_time  <- lead_time - ref_time
        time_units <- "s"
      }
      if (grepl("day", lead_time_units[["value"]])) {
        lead_time  <- lead_time * 3600 * 24 - ref_time
        time_units <- "d"
      }
    }
  } else {
    lead_time  <- gsub("[[:punct:]]", "", lead_time)
    lead_time  <- gsub(" ", "", lead_time)
    lead_time  <- suppressMessages(str_datetime_to_unixtime(lead_time)) - ref_time
    time_units <- "date_time_string"
  }

  if (was_closed) ncdf4::nc_close(nc_id)

  data.frame(fcdate = ref_time, validdate = ref_time + lead_time, leadtime = lead_time, time_units = time_units)
}
