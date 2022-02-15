#' Read point observations from multiple files
#'
#' \code{read_obs} generates file names, based on the arguments given and reads
#' point observations data from them. The data can optionally be re-written to
#' files of a different format. Due to the large volumes of data that may be
#' read, the function will only return data to the calling environment if
#' \code{return_data = TRUE}
#'
#' \code{read_obs} is not intended to be used for reading gridded observations.
#' For this use \link{read_analysis} instead.
#'
#' @param date_times A vector of date time strings to read. Can be in YYYYMMDD,
#'   YYYYMMDDhh, YYYYMMDDhhmm, or YYYYMMDDhhmmss format. Can be numeric or
#'   character. A vector of date-times can be generated using \link{seq_dates}.
#'   If \code{date_times} is missing or set to NULL, the date-times to read will
#'   be generated from the \code{start_date}, \code{end_date} and \code{by}
#'   arguments.
#' @param parameter The names of the parameters to read. By default this is
#'   NULL, meaning that all parameters are read from the observations files.
#' @param stations The IDs of the stations to read from the files. By default
#'   this is NULL, meaning that observations for all stations are read from the
#'   observations files.
#' @param file_path The parent path to all forecast data. All file names are
#'   generated to be under the \code{file_path} directory. The default is the
#'   current working directory.
#' @param file_format The format of the files to read. By default this is
#'   "vobs", which is the standard format used by the HIRLAM consortium. If set
#'   to something else, \code{read_obs} will search the global environment for a
#'   function called \code{read_<file_format>} that it will use to read from the
#'   files.
#' @param file_template A template for the file names. For available built in
#'   templates see \code{\link{show_file_templates}}. If anything else is
#'   passed, it is returned unmodified, or with substitutions made for dynamic
#'   values. Available substitutions are {YYYY} for year, \{MM\} for 2 digit
#'   month with leading zero, \{M\} for month with no leading zero, and
#'   similarly \{DD\} or \{D\} for day, \{HH\} or \{H\} for hour, \{mm\} or
#'   \{m\} for minute. Note that the full path to the file will always be
#'   file_path/template. Other substitutions can be passed via \code{...}
#' @param file_format_opts Specific options for reading the file format
#'   specified in \code{file_format}. Should be a named list, with names
#'   corresponding to argument for \code{read_<file_format>}.
#' @param output_format The file format to re-write the data to. By default this
#'   is "obstable", which is an sqlite file desgined specifically for the harp
#'   ecosystem. If set to something else, \code{read_obs} will search the global
#'   environment for a function called \code{write_<file_format>} that it will
#'   use to write to the output file(s).
#' @param output_format_opts Specific options for writing to \code{file_format}
#'   files. Must be a named list and at least include the names \code{"path"}
#'   and \code{"template"}. By setting \code{output_format_opts$path} to
#'   something other than NULL, \code{read_obs} will attempt to write out the
#'   data.
#' @param return_data Logical - whether to return the data read in to the
#'   calling environment. Due to the potential for large volumes of data, this
#'   is set to FALSE by default.
#' @param start_date If \code{date_times} is not set, the start date-time for a
#'   sequence of dates to read. Can be in YYYYMMDD, YYYYMMDDhh, YYYYMMDDhhmm, or
#'   YYYYMMDDhhmmss format.
#' @param end_date If \code{date_times} is not set, the end date-time for a
#'   sequence of dates to read. Can be in YYYYMMDD, YYYYMMDDhh, YYYYMMDDhhmm, or
#'   YYYYMMDDhhmmss format.
#' @param by If \code{date_times} is not set, the time step for a sequence of
#'   dates to read. If numeric, it is assumed to be in hours, otherwise the the
#'   suffixes "d", "h", "m", "s", for days, hours, minutes and seconds
#'   respectively.
#' @param reads_per_write The number of files to read before writing out the
#'   data to new files. Set this to a low number to reduce memory usage. The
#'   default is 24 based on the assumption that there are observations files
#'   every hour and writing should be done once per observation day. For the
#'   default setting of writing to "obstable" files, this number has no impact
#'   on the output since these files can be appended to. For other formats, this
#'   setting might be important to prevent data from being overwritten.
#' @param ... Other arguments to \code{generate_filenames} for getting the names
#'   of files to read.
#'
#' @return If \code{return_data = TRUE}, a list with data frames of
#'   observations.
#' @export
#'
#' @examples
read_obs <- function(
  date_times,
  parameter,
  stations           = NULL,
  file_path          = getwd(),
  file_format        = NULL,
  file_template      = "vobs",
  file_format_opts   = vfile_opts("vobs"),
  output_format      = "obstable",
  output_format_opts = obstable_opts(),
  return_data        = FALSE,
  start_date         = NULL,
  end_date           = NULL,
  by                 = "1h",
  reads_per_write    = 24,
  ...
) {

  # Check inputs

  if (missing(parameter)) parameter <- NULL

  if (missing(date_times) || is.null(date_times)) {

    if (is.numeric(by)) {
      by = paste0(by, "h")
    }

    if (is.null(start_date)) {
      stop(
        paste(
          "If `date_times` is not passed `start_date` and `end_date`",
          "must be passed."
        ),
        call. = FALSE
      )
    }

    date_times <- seq_dates(start_date, end_date, by = by)

  }

  if (return_data) {
    function_output <- list()
    list_counter    <- 0
  }

  date_times <- split(
    date_times,
    (seq_along(date_times) - 1) %/% reads_per_write
  )

  for (iter_date_times in date_times) {

    if (return_data) {
      list_counter <- list_counter + 1
      function_output[[list_counter]] <- list()
    }

    data_files <- generate_filenames(
      file_path      = file_path,
      file_date      = iter_date_times,
      parameter      = parameter,
      file_template  = file_template,
      filenames_only = FALSE,
      ...
    )

    names(data_files)[names(data_files) == "fcdate"] <- "date_times"

    data_files[["date_times"]] <- unixtime_to_str_datetime(
      data_files[["date_times"]], YMDhms
    )

    data_files <- data_files[colnames(data_files) != "lags"]

    data_files <- dplyr::group_nest(data_files, .data[["file_name"]])

    obs_data <- dplyr::mutate(
      data_files,
      obs = purrr::map2(
        .data[["file_name"]],
        .data[["data"]],
        ~do.call(
          read_obs_file,
          c(
            list(file_name = .x),
            as.list(.y),
            list(
              file_format      = file_format,
              file_format_opts = file_format_opts
            )
          )
        )
      )
    )

    table_names <- unique(unlist(lapply(obs_data[["obs"]], names)))
    table_names <- grep("_params", table_names, invert = TRUE, value = TRUE)

    for (table_name in table_names) {

      table_data <- purrr::map_dfr(obs_data[["obs"]], table_name)

      if (is.null(table_data) || nrow(table_data) < 1) {
        next()
      }

      param_name <- paste0(table_name, "_params")
      table_params <- dplyr::distinct(
        purrr::map_dfr(obs_data[["obs"]], param_name)
      )

      if (!is.null(output_format_opts[["path"]])) {

        file_names <- do.call(
          generate_filenames,
          c(
            list(
              file_path      = output_format_opts[["path"]],
              file_template  = output_format_opts[["template"]],
              filenames_only = FALSE
            ),
            as.list(
              dplyr::select(
                dplyr::mutate(
                  table_data,
                  file_date = unixtime_to_str_datetime(
                    .data[["validdate"]], YMDhms
                  )
                ),
                -dplyr::any_of(
                  c(
                    "SID", "lat", "lon", "elev", "validdate",
                    table_params[["parameter"]]
                  )
                )
              )
            )
          )
        ) %>%
          dplyr::select(-.data[["lags"]])

        table_data <- suppressMessages(dplyr::inner_join(
          table_data,
          dplyr::rename(file_names, validdate = .data[["fcdate"]])
        )) %>%
          dplyr::group_nest(.data[["file_name"]])

        if (
          !is.null(output_format_opts[["index_cols"]]) &&
            output_format_opts[["index_cols"]][1] == "auto"
        ) {
          output_format_opts[["index_cols"]] <- intersect(
            c("validdate", "SID", "p", "h", "m"),
            unique(unlist(lapply(table_data[["data"]], colnames)))
          )
        }

        output_func <- paste0("write_", output_format)

        if (output_format == "obstable") {
          output_func <- paste0(output_func, "_to_sqlite")
          output_format_opts[["primary_key"]] <- output_format_opts[["index_cols"]]
        }

        output_func <- get(output_func)

        purrr::walk2(
          table_data[["data"]],
          table_data[["file_name"]],
          ~do.call(
            output_func,
            c(
              list(
                obs_data     = .x,
                file_name    = .y,
                table_name   = toupper(table_name),
                params_table = table_params
              ),
              output_format_opts
            )
          )
        )

      }

      if (return_data) {
        if (is.null(table_data[["data"]])) {
          function_output[[list_counter]][[table_name]] <- table_data
        } else {
          function_output[[list_counter]][[table_name]] <- table_data[["data"]]
        }
        function_output[[list_counter]][[table_name]] <- dplyr::mutate(
          function_output[[list_counter]][[table_name]],
          validdate = unix2datetime(.data[["validdate"]])
        )
        function_output[[list_counter]][[param_name]] <- table_params
      }

    }

  }

  if (return_data) {

    list_names <- unique(unlist(lapply(function_output, names)))

    function_output <- purrr::map(
      list_names,
      ~purrr::map_dfr(function_output, .x)
    )

    names(function_output) <- list_names

    return(
      purrr::map_at(
        function_output, grep("_params", list_names), dplyr::distinct
      )
    )

  } else {

    message(
      "No data returned. Set `return_data = TRUE` to return data."
    )

  }

}
