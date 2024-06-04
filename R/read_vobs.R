# Read data from a vobs file.
#
# \code{read_vobs} returns the content of a named vobs file as a list of
# data frames - one for synop data and one for temp (upper air) data.
#
# @param file_name Name of the vobs file.
# @param missing_value Missing value indicator in vobs file.
# @param ... Not used. Absorbs unused arguments.
#
# @return A list with data frames for synop and temp data.
# NOT exported. Used internally.
#
# @examples
#
read_vobs <- function(
  file_name, dttm, opts,
  param_defs = getExportedValue("harpIO", "harp_params"), ...
) {

  if (length(opts) < 0) {
    opts <- vfile_opts(type = "vobs")
  }

  empty_data <- empty_data_interpolate(NA, NA, empty_type = "obs")

  if (file.exists(file_name)) {
    message("Reading: ", file_name)
  } else {
    warning("File not found: ", file_name, call. = FALSE, immediate. = TRUE)
    return(list(synop = empty_data, temp = empty_data))
  }

  v_data <- read_vfile(
    file_name,
    v_type        = "vobs",
    missing_value = opts[["missing_value"]],
    param_defs    = param_defs
  )

  if (is.null(v_data)) {
    return(list(synop = empty_data, temp = empty_data))
  }

  if (!missing(dttm) && !is.null(dttm)) {
    if (!is.null(v_data[["synop"]]) && nrow(v_data[["synop"]]) > 0) {
      v_data[["synop"]] <- dplyr::bind_cols(
        tibble::tibble(valid_dttm = dttm), v_data[["synop"]]
      )
    }
    if (!is.null(v_data[["temp"]]) && nrow(v_data[["temp"]]) > 0) {
      v_data[["temp"]] <- dplyr::bind_cols(
        tibble::tibble(valid_dttm = dttm), v_data[["temp"]]
      )
    }
  }

  v_data

}
