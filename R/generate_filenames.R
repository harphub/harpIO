#' Generate filenames from a template and arguments
#'
#' This is a more flexible replacement for the \code{\link{get_filenames}}
#' function. The main difference is that it allows you to pass any argument that
#' might exist to be substituted in the file name. These arguments are passed
#' through ...
#'
#' @param file_path The parent path to all file names. All file names are
#'   generated to be under the \code{file_path} directory. The default is the
#'   current working directory.
#' @param file_date Forecast date for file names. Can be in YYYYMMDD,
#'   YYYYMMDDhh, YYYYMMDDhhmm, or YYYYMMDDhhmmss format. Can be numeric or
#'   charcter. If not passed the system time in YYYYMMDD format is used.
#' @param start_date Date of the first forecast for file names. Can be in
#'   YYYYMMDD, YYYYMMDDhh, YYYYMMDDhhmm, or YYYYMMDDhhmmss format. Can be
#'   numeric or charcter. If NULL, \code{file_date} must be specified. If not
#'   NULL, \code{end_date} must also be specified.
#' @param end_date Date of the last forecast for file names. Can be in YYYYMMDD,
#'   YYYYMMDDhh, YYYYMMDDhhmm, or YYYYMMDDhhmmss format. Can be numeric or
#'   charcter. Used in conjunction with \code{start_date} and \code{by}.
#' @param by The time between new forecasts. If numeric, it is assumed to be in
#'   hours, but the time units may be given by a letter after the number where d
#'   = days, h = hours, m = minutes and s = seconds. A sequence of forecasts
#'   dates is generated from \code{start_date} to \code{end_date} every
#'   \code{by}.
#' @param lags A named list of members of an ensemble forecast model that are
#'   lagged and the amount by which they are lagged. The list names are the
#'   names of those forecast models, as given in \code{fcst_model} that have
#'   lagged members, and the lags are given as vectors that are the same length
#'   as the members vector. If the lags are numeric, it is assumed that they are
#'   in hours, but the units may be specified with a letter after each value
#'   where d = days, h = hours, m = minutes and s = seconds. \code{lags} is
#'   primarily used to generate the correct file names for lagged members - for
#'   example a lag of 1 hour will generate a file name with a date-time 1 hour
#'   earlier than the date-time in the sequence \code{(start_data, end_date, by
#'   = by)} and a lead time 1 hour longer.
#' @param file_template A template for the file names. For available built in
#'   templates see \code{\link{show_file_templates}}. If anything else is
#'   passed, it is returned unmodified, or with substitutions made for dynamic
#'   values. Available substitutions are {YYYY} for year, \{MM\} for 2 digit
#'   month with leading zero, \{M\} for month with no leading zero, and
#'   similarly \{DD\} or \{D\} for day, \{HH\} or \{H\} for hour, \{mm\} or
#'   \{m\} for minute. Also \{LDTx\} for lead time and \{MBRx\} where x is the
#'   length of the string including leading zeros. Note that the full path to
#'   the file will always be file_path/template.
#' @param filenames_only Logical. If TRUE (the default), a vector of file names
#'   will be returned, otherwise if FALSE a data frame will be returned with the
#'   metadata that was used to generated the file names included.
#' @param ... Other arguments for substitutions in \code{file_template}. Note
#'   that for ensemble members, members may be used for the \{MBRx\}
#'   substitution.
#'
#' @return A character vector or a data frame.
#' @export
#'
#' @examples
#' generate_filenames(fcst_model = "my_model", parameter = "T2m")
#' generate_filenames(
#'   fcst_model     = "my_model",
#'   lead_time      = seq(0, 12),
#'   file_template  = "vfld",
#'   filenames_only = FALSE
#' )
#' generate_filenames(
#'   sub_model      = "my_model",
#'   lead_time      = seq(0, 12),
#'   file_template  = "vfld_eps",
#'   members        = seq(0, 5)
#' )
#' generate_filenames(
#'   start_date     = 2020010100,
#'   end_date       = 2020013100,
#'   by             = "1d",
#'   fcst_model     = "my_model",
#'   lead_time      = seq(0, 3),
#'   file_path      = "/path/to/my/data",
#'   file_template  = "{fcst_model}_v{version}_{YYYY}{MM}{DD}{HH}{mm}{ss}_mbr{MBR5}+{LDT3}",
#'   members        = seq(0, 1),
#'   version        = c(2, 3, 5)
#' )
generate_filenames <- function(
  file_path      = getwd(),
  file_date      = Sys.Date(),
  start_date     = NULL,
  end_date       = NULL,
  by             = "6h",
  lags           = "0s",
  file_template  = "fctable",
  filenames_only = TRUE,
  ...
) {

  # Get the file dates

  if (is.null(start_date)) {

    if (lubridate::is.Date(file_date)) {
      file_dates <- lubridate::as_datetime(file_date) %>%
        as.numeric()
    } else if (lubridate::is.POSIXct(file_date)) {
      file_dates <- as.numeric(file_date)
    } else {
      file_dates <- suppressMessages(str_datetime_to_unixtime(file_date))
    }

  } else {

    if (is.null(end_date)) {
      stop("end_date must be passed as well as start_date", call. = FALSE)
    }
    file_dates <- seq_dates(start_date, end_date, by = by)
    file_dates <- suppressMessages(str_datetime_to_unixtime(file_dates))
  }


  lag_seconds <- sapply(lags, char_to_time, "lags")

  if (!is.list(lags))        lags        <- list(lags)
  if (!is.list(lag_seconds)) lag_seconds <- list(lag_seconds)
  if (!is.list(file_path))   file_path   <- list(file_path)

  # Construct a data frame from all of the arguments
  file_names_df <- data.frame(
    fcst_dttm        = file_dates,
    stringsAsFactors = FALSE
  )

  file_names_df[["file_path"]]   <- file_path
  file_names_df[["lags"]]        <- lags
  file_names_df[["lag_seconds"]] <- lag_seconds

  if (tidyr_new_interface()) {
    file_names_df <- tidyr::unnest(
      file_names_df, tidyselect::any_of(c("file_path"))
    )
    file_names_df <- tidyr::unnest(
      file_names_df, tidyselect::any_of(c("lags", "lag_seconds"))
    )
  } else {
    file_names_df <- tidyr::unnest(file_names_df, .data[["file_path"]])
    file_names_df <- tidyr::unnest(file_names_df)
  }


  args <- list(...)
  output_cols <- names(args)

  for (i in seq_along(args)) {

    arg <- args[[i]]

    if (!is.list(arg)) {
      if (is.null(arg)) arg <- NA
      arg <- list(arg)
    }

    unnest_col <- TRUE
    if (inherits(arg, "harp_parameter")) {
      arg <- list(arg)
      unnest_col <- FALSE
    }

    if (is.data.frame(arg)) {
      # Check for duplicate columns
      output_cols <- union(output_cols, names(arg))
      dupes_cols  <- intersect(colnames(file_names_df), colnames(arg))
      if (length(dupes_cols) > 0) {
        file_names_df <- file_names_df[!colnames(file_names_df) %in% dupes_cols]
        if (is.element("lags", dupes_cols) && !is.element("lag_seconds", colnames(arg))) {
          file_names_df <- file_names_df[colnames(file_names_df) != "lag_seconds"]
          arg[["lag_seconds"]] <- sapply(arg[["lags"]], char_to_time, "lags")
        }
      }
      arg <- list(arg)
    }
    file_names_df[[names(args)[i]]] <- arg

    if (unnest_col) {
      if (tidyr_new_interface()) {
        file_names_df <- tidyr::unnest(
          file_names_df, tidyselect::all_of(names(args)[i])
        )
      } else {
        file_names_df <- tidyr::unnest(file_names_df)
      }
    }

  }

  # Adjust the lead time and fcdate to the lags
  file_names_df[["fcst_dttm"]] <- file_names_df[["fcst_dttm"]] - file_names_df[["lag_seconds"]]
  lead_time_col <- grep("^lead|^ldt", tolower(colnames(file_names_df)))
  if (length(lead_time_col) > 1) {
    stop(
      "You appear to have more than one argument that can be interpreted as lead time:\n",
      "'", paste(colnames(file_names_df)[lead_time_col], collapse = ","), "'.\n",
      call. = FALSE
    )
  }
  if (length(lead_time_col) > 0) {
    leads_list         <- split_time(file_names_df[[lead_time_col]], "lead_time")
    lags_in_lead_units <- mapply(
      function(x, y) char_to_time(x, "leadtime", unit = y),
      paste0(file_names_df[["lag_seconds"]], "s"),
      leads_list[["unit"]]
    )
    file_names_df[[lead_time_col]] <- leads_list[["time"]] + lags_in_lead_units
    file_names_df[["LDT"]]         <- file_names_df[[lead_time_col]]
  }

  file_names_df[["YYYY"]] <- lubridate::year(unix2datetime(file_names_df[["fcst_dttm"]]))
  file_names_df[["M"]]    <- lubridate::month(unix2datetime(file_names_df[["fcst_dttm"]]))
  file_names_df[["D"]]    <- lubridate::day(unix2datetime(file_names_df[["fcst_dttm"]]))
  file_names_df[["H"]]    <- lubridate::hour(unix2datetime(file_names_df[["fcst_dttm"]]))
  file_names_df[["m"]]    <- lubridate::minute(unix2datetime(file_names_df[["fcst_dttm"]]))
  file_names_df[["s"]]    <- lubridate::second(unix2datetime(file_names_df[["fcst_dttm"]]))
  file_names_df[["MM"]]   <- formatC(file_names_df[["M"]], width = 2, flag = "0")
  file_names_df[["DD"]]   <- formatC(file_names_df[["D"]], width = 2, flag = "0")
  file_names_df[["HH"]]   <- formatC(file_names_df[["H"]], width = 2, flag = "0")
  file_names_df[["mm"]]   <- formatC(file_names_df[["m"]], width = 2, flag = "0")
  file_names_df[["ss"]]   <- formatC(file_names_df[["s"]], width = 2, flag = "0")

  # Check for member columns
  member_col <- grep("^member|^mbr", tolower(colnames(file_names_df)))
  member_col_name <- colnames(file_names_df)[member_col]
  if (length(member_col) > 1) {
    if (any(grepl("out$", tolower(colnames(file_names_df))))) {
      member_col_name <- grep(
        "out$",
        tolower(colnames(file_names_df))[member_col],
        value  = TRUE,
        invert = TRUE
      )
    }
  }
  if (length(member_col_name) > 1) {
    stop(
      "You appear to have more than one argument that can be interpreted as member:\n",
      "'", paste(member_col_name, collapse = ","), "'.\n",
      call. = FALSE
    )
  }
  if (length(member_col) > 0) {
    file_names_df[["MBR"]] <- file_names_df[[member_col_name]]
  }

  # Check the template
  template <- get_template(file_template)

  template_subs <- gsub(
    "\\{|\\}", "",
    unlist(regmatches(template, gregexpr("(\\{.*?\\})", template)))
  )

  # Ensure compatibility with older versions that use eps_model and det_model
  # in the templates.

  if (is.element("eps_model", template_subs)) {
    if (
      is.element("fcst_model", colnames(file_names_df)) &&
      !is.element("eps_model", colnames(file_names_df))
    ) {
      file_names_df[["eps_model"]] <- file_names_df[["fcst_model"]]
    }
  }

  if (is.element("det_model", template_subs)) {
    if (
      is.element("fcst_model", colnames(file_names_df)) &&
        !is.element("det_model", colnames(file_names_df))
    ) {
      file_names_df[["det_model"]] <- file_names_df[["fcst_model"]]
    }
  }

  # Get parts of template that have a set number of digits
  format_cols <- grep("[[:digit:]]+$", template_subs, value = TRUE)
  if (length(format_cols) > 0) {
    for (col in format_cols) {
      col_width <- regmatches(col, gregexpr("[[:digit:]]+$", col))[[1]]
      data_col  <- regmatches(col, gregexpr("[[:digit:]]+$", col), invert = TRUE)[[1]][1]
      if (!is.element(data_col, colnames(file_names_df))) {
        stop(
          "Cannot find ", data_col, " for ", col, "in template.",
          call. = FALSE
        )
      }
      file_names_df[[col]] <- formatC(file_names_df[[data_col]], width = col_width, flag = "0")
    }
  }

  missing_cols <- setdiff(template_subs, colnames(file_names_df))

  if (length(missing_cols) > 0) {
    stop(
      "{", paste(missing_cols, collapse = "}, {"), "} in template, but cannot find variables.",
      call. = FALSE
    )
  }

  file_names_df[["file_name"]] = as.vector(glue::glue_data(file_names_df, template))

  if (filenames_only) {

    unique(file_names_df[["file_name"]])

  } else {

    output_cols <- intersect(
      colnames(file_names_df),
      c("fcst_dttm", "lags", output_cols, "file_name")
    )

    dplyr::distinct(file_names_df[output_cols])

  }

}

char_to_time <- function(x, var, unit = c("s", "m", "h", "d")) {
  unit <- match.arg(unit)
  x    <- split_time(x, var)
  units_multiplier(x[["unit"]]) * as.numeric(x[["time"]]) / units_multiplier(unit)
}

split_time <- function(x, var) {
  if (is.numeric(x)) {
    time_unit  <- rep("h", length(x))
    time_value <- x
  } else {
    time_unit <- tolower(sub("[[:digit:]]+", "", x))
    time_unit[nchar(time_unit) < 1] <- "h"
    time_value <- sub("[[:alpha:]]+", "", x)
  }
  bad_units <- which(!time_unit %in% c("d", "h", "m", "s", "NA", "NAs", "nas"))
  if (length(bad_units) > 0) {
    stop(
      "Unrecognized time unit: '",
      paste(x[bad_units], collapse = ","),
      "' for '", var, "'.",
      call. = FALSE
    )
  }
  list(time = time_value, unit = time_unit)
}
