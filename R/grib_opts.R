#' Set options for grib files
#'
#' @param meta Logical. Weather to read metadata
#' @param multi Logical. Whether there are multi field grib messages.
#'
#' @return A list
#' @export
#'
#' @examples
#' grib_opts()
grib_opts <- function(
  meta  = TRUE,
  multi = FALSE
) {

  list(meta = meta, multi = multi)

}
