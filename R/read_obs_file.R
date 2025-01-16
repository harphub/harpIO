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
#'@param param_defs A list of parameter definitions that includes the file
#'  format to be read. By default the built in list \code{\link{harp_params}}
#'  is used. Modifications and additions to this list can be made using
#'  \code{\link{modify_param_def}} and \code{\link{add_param_def}} respectively.
#'@param dttm The date times to extract from the file. If NULL, all date times
#'  are extracted from the file. \code{\link[harpCore]{seq_dttm}} can be used to
#'  generate a vector of equally spaced date-time strings.

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
read_obs_file <- function(
  file_name,
  parameter,
  param_defs       = getExportedValue("harpIO", "harp_params"),
  dttm             = NULL,
  stations         = NULL,
  file_format      = NULL,
  file_format_opts = list(),
  ...
) {

  if (missing(parameter)) parameter <- NULL

  if (!is.null(dttm)) {
    dttm <- harpCore::as_unixtime(dttm)
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

  if (file_format == "vfld") {
    file_format_opts <- do.call(
      vfile_opts, file_format_opts
    )
    file_format                <- "vobs"
    file_format_opts[["type"]] <- "vobs"
  }

  read_func <- try(get(paste0("read_", file_format)), silent = TRUE)

  if (inherits(read_func, "try-error")) {
    stop(
      "Cannot read files of format: `", file_format,
      "`. There is no `read_", file_format, "()` function.",
      call. = FALSE
    )
  }

  read_func(
    file_name  = file_name,
    parameter  = unique(parameter),
    param_defs = param_defs,
    dttm       = dttm,
    stations   = stations,
    opts       = file_format_opts
  )

}
