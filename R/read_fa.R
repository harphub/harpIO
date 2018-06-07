#' Read a field from a FA file
#'
#' @param filename The FA file name.
#' @param parameter The parameter to read. Standard HARP names are used.
#' @param meta If TRUE, also read all meta data (domain, time properties).
#' @param ... Arguments for \code{Rgrib2::Gdec}
#' @return A geofield object with 2d array and projection information
#' @export
#'
#' @examples
#' model_geofield <- read_fa(file_name, "t2m")
#' model_geofield <- read_fa(file_name, "t500")
#' model_geofield <- read_fa(file_name, "topo")

read_fa <- function(infile, parameter, meta=TRUE, ...) {
  # TODO: change units (Kevin -> Celsius ...) ???
  # TODO: arome vs alaro field names (precip) + other alternative names
  if (!requireNamespace("Rfa", quietly=TRUE)) {
    stop("The Rfa package must be installed to read FA files.")
  }
  param_info <- get_fa_prm_info(parameter)

  outform <- if (meta) "G" else "M"

  if (inherits(infile, "FAfile")) fafile <- infile
  else if (is.character(infile)) fafile <- Rfa::FAopen(infile)
  else stop("bad infile")

  try(result <- Rfa::FAdec(fa=fafile, field=param_info$field[1], outform=outform, ...))

  if (length(param_info$field)>1) {
    if (param_info$basename=="pcp") {
      for(fff in param_info$fields[-1]) {
        result <- result +  Rfa::FAdec(fa=fafile, field=fff, outform="M", ...)
      }
    } else if (param_info$basename %in% c("s", "g")) {
      U <- result
      V <-  Rfa::FAdec(fa=fafile, field=field[2], faframe=frame, outform="g", ...)
      result <- geogrid::wind.dirspeed(U,V)$wspeed
    } else if (param_info$basename=="d") {
      U <- result
      V <-  Rfa::FAdec(fa=fafile, field=field[2], faframe=frame, outform="g", ...)
      result <- geogrid::wind.dirspeed(U,V)$wdir
    }
  }
  result
}
