#' Generate options for vfld and vobs files
#'
#' @param type Type of vfile - "vfld" or "vobs"
#' @param missing_value Missing value indicator. Default is -99.0
#' @param synop_cols For vfld/vobs version < 4 the column names in the files
#'   are assumed. Use \code{synop_cols} to override the defaults for SYNOP
#'   data. Note that the variable names used here must be valid vfld/vobs
#'   names.
#' @param temp_cols For vfld/vobs version < 4 the column names in the files
#'   are assumed. Use \code{temp_cols} to override the defaults for TEMP data.
#'   Note that the variable names used here must be valid vfld/vobs names.
#'
#' @return A list of options
#' @export
#'
#' @examples
#' vfile_opts()
#' vfile_opts(type = "vobs")
#' vfile_opts(missing_value = -555)
vfile_opts <- function(
  type          = c("vfld", "vobs"),
  missing_value = -99.0,
  synop_cols    = NULL,
  temp_cols     = NULL
) {

  type <- match.arg(type)

  list(
    type = type,
    missing_value = missing_value,
    synop_cols    = synop_cols,
    temp_cols     = temp_cols
  )

}
