#'Read an observations file
#'
#'This function is for reading from an observations file. The data should be at
#'geographic points and either at a single vertical level, or multiple vertical
#'levels.
#'
#'The file format will be guessed if it is not explicitly passed and options for
#'each file format should be passed as a list via the \code{file_format_opts}
#'argument.
#'
#'No interpolation is done with this function. For reading and interpolating
#'gridded observations, use \link{read_analysis}.
#'
#'@param file_name The full path of the file to be read.
#'@param parameter Parameters to extract from the file. If missing or NULL, all
#'  parameters are extracted from the file.
#'@param date_times The date times to extract from the file. If NULL, all date
#'  times are extracted from the file.
#'@param stations Station IDs to extract from file. If NULL, all station IDs are
#'  extracted from the file.
#'@param file_format The format of the file to read.
#'@param file_format_opts Options specific to the file format as a named list.
#'@param ... Not used - absorbs other arguments from calling environment.
#'
#'@return A named list with up to four elements \itemize{ \item synop - a data
#'  frame of observations at a single vertical level. \item temp - a data frame
#'  of observations at multiple vertical levels. \item synop_params - a data
#'  frame of the units and accumulation times of the parameters in synop. \item
#'  temp_params - a data frame of units and accumulation times of the parameters
#'  in temp.}
#'@export
#'
#' @examples
read_obs_file <- function(
  file_name,
  parameter,
  date_times       = NULL,
  stations         = NULL,
  file_format      = NULL,
  file_format_opts = list(),
  ...
) {

  if (missing(parameter)) parameter <- NULL

  if (!is.null(date_times)) {
    date_times <- suppressMessages(str_datetime_to_unixtime(date_times))
  }

  file_format <- check_for_na(file_format)

  if (is.null(file_format)) {
    file_format <- guess_format(file_name)
  }

  if (length(file_format) > 1) {
    stop(
      paste0(
        "More than one 'file_format' passed: '",
        paste(file_format, collpase = "','"),
        "'."
      ),
      call. = FALSE
    )
  }

  if (file_format == "vfld") {
    file_format_opts <- do.call(
      vfile_opts, file_format_opts
    )
    file_format                <- "vobs"
    file_format_opts[["type"]] <- "vobs"
  }

  if (is.na(file_format)) {
    stop(
      paste0(
        "Please provide explicit file format for '",
        file_name,
        "'."
      ),
      call. = FALSE
    )
  }

  read_func <- get(paste0("read_", file_format))

  read_func(
    file_name  = file_name,
    parameter  = parameter,
    date_times = date_times,
    stations   = stations,
    opts       = file_format_opts
  )

}
