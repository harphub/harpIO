#' Generate options for vfld and vobs files
#'
#' @param type Type of vfile - "vfld" or "vobs"
#' @param missing_value Missing value indicator. Default is -99.0
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
  missing_value = -99.0
) {

  type <- match.arg(type)

  list(
    type = type,
    missing_value = missing_value
  )

}
