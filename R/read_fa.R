#' Read a field from a FA file
#'
#' @param filename The FA file name.
#' @param parameter The parameter to read. Standard HARP names are used.
#' @param meta If TRUE, also read all meta data (domain, time properties).
#' @param fa_type The kind of model file: "arome", "alaro", "surfex"...
#' @param fa_vector TRUE if the wind variable (speed, direction) must be calculated from components
#' @param ... Arguments for \code{Rfa::FAdec}
#' @return A geofield object with 2d array and projection information
#' @export
#'
#' @examples
#' model_geofield <- read_fa(file_name, "t2m")
#' model_geofield <- read_fa(file_name, "t500")
#' model_geofield <- read_fa(file_name, "topo")

.last_fa_archive <- local({
  archname <- ""
  arch <- NULL
  function(new) if (missing(new)) arch else arch <<- new
})
  
read_fa <- function(infile, parameter, meta=TRUE, fa_type="alaro", fa_vector=FALSE, ...) {
  # TODO: change units (Kevin -> Celsius ...) ???
  # TODO: arome vs alaro field names (precip) + other alternative names
  # somehow keep a (tar)file in memory

# harp_env$fa_infile <- infile
# harp_env$fa_domain <- 
## or use the same trick as meteogrid for e.g. .Last.domain()
  if (!requireNamespace("Rfa", quietly=TRUE)) {
    stop("The Rfa package must be installed to read FA files.")
  }
  if (!is.list(parameter)) parameter <- parse_harp_parameter(parameter)
  if (is.null(parameter$fa_info)) parameter$fa_info <- get_fa_param_info(parameter, fa_type, fa_vector)

  outform <- if (meta) "G" else "M"
  if (inherits(infile, "FAfile")) {
    fafile <- infile
  } else if (is.character(infile)) {
    namsplit <- strsplit(infile, "@")[[1]]
    fafile <- switch(length(namsplit),
                       Rfa::FAopen(infile=infile, ...),
                       Rfa::FAopen(infile=namsplit[1], archname=namsplit[2], ...),
                       stop("Undefined infile ",infile))
  } else {
    stop("bad infile")
  }

  try(result <- Rfa::FAdec(fa=fafile, field=parameter$fa_info, outform=outform, ...))
  if (length(parameter$fa_info)>=1) {
    if (dim(x)[3] != length(parameter$fa_info)) stop("Not all necessary fields are available.")
    result <- do.call(attr(parameter$fa_info, "myfunc"), result)
  }
  if (meta) attr(result, "info")$name <- parameter$fullname
  result
}

# ALARO doesn't write Tdew, so we calculate it from RH and T
#' Standard Magnus formula for dewpoint temperature
#' @param T Temperature in degrees Celsius (Kelvin is also OK). Could be a geofield.
#' @param RH Relative humidity 0..1 (percentage is also OK).
#' @param tuning A vector with two tuning parameters for the formula. Default is from NOAA.
#' @return Dewpoint temperature
#' @export
rh2tdew <- function (T, RH, tuning=c("a"=17.67, "b"=243.5)) {
# default tuning is from NOAA: (17.67, 243.5) 
# tuning <- (17.27,237.3) is valid for 0<T<60, so not very good
# other tunings may be 
# tuning <- c(a=17.62 , b=243.12)
# tuning <- c("a"=17.625, "b"=243.04)
# A.L. Buck '81: use two values depending on T: (17.368,238.88) T>0,
# (17.966,247.15) T<0
    if (max(RH) > 5) RH <- RH/100
    if (max(T) > 200) T <- T - 273.15
    a <- tuning[1]
    b <- tuning[2]
    minRH <- 0.001
    RH[RH < minRH] <- minRH
    RH[RH > 1] <- 1
    gg <- log(RH) + a*T/(T+b)
    b * gg/(a-gg)
}

