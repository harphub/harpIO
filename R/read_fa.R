#' Read a field from an FA file
#'
#' @param filename The FA file name. "file@arch" signifies a file inside a tar archive.
#'        It may also be a \code{FAfile} object.
#' @param parameter The parameter to read. Standard HARP names are used, but full FA field names will also
#'        work.
#' @param meta If TRUE, also read all meta data (domain, time properties).
#' @param fa_type The kind of model file: "arome", "alaro", "surfex"...
#' @param fa_vector TRUE if the wind variable (speed, direction) must be calculated from components
#' @param ... Ignored
#' @return A 2d geofield object (2d array with projection information)
#'
#' @examples
#' model_geofield <- read_fa(file_name, "t2m")
#' model_geofield <- read_fa(file_name, "t500")
#' model_geofield <- read_fa(file_name, "topo")

read_fa <- function(filename, parameter, meta=TRUE, fa_type="arome", fa_vector=TRUE, ...) {
  # TODO: if meta==TRUE, just return a simple array, no geofield or attributes
  # ?accumulated fields?
# harp_env$fa_infile <- infile
# harp_env$fa_domain <-
## or use the same trick as meteogrid for e.g. .Last.domain()
  if (!requireNamespace("Rfa", quietly=TRUE)) {
    stop("The Rfa package must be installed to read FA files.")
  }
  if (inherits(filename, "FAfile")) {
    fafile <- filename
  } else if (is.character(filename)) {
    namsplit <- strsplit(filename, "@")[[1]]
    fafile <- switch(length(namsplit),
                       Rfa::FAopen(filename=filename),
                       Rfa::FAopen(filename=namsplit[1], archname=namsplit[2]),
                       stop("Could not open file ", filename))
  } else {
    stop("bad filename")
  }

  extra_dim <- list(prm=parameter)
  if (meta) {
    result <- meteogrid::as.geofield(NA, domain=fafile, extra_dim=extra_dim,
                                     info=list(time=attr(fafile, "time")))
  } else {
    result <- array(NA, dim=c(attr(fafile, "domain")$nx, attr(fafile, "domain")$ny,
                              length(parameter)))
  }
  fa_info <- lapply(parameter, get_fa_param_info, fa_type=fa_type, fa_vector=fa_vector)

  for (prm in seq_along(parameter)) {
    # TODO: fix parameter
    # TODO: error handling: this is just a place holder for now
    # always set outform="M" (we already have domain) -> also pass faframe!
    ee <- tryCatch(fcdata <- Rfa::FAdec(fa = fafile, field = fa_info[[prm]]$fa_name,
                                        outform = "M", faframe = attr(fafile, "frame")),
                 error = function(e) e)
    if (inherits(ee, "error")) fcdata <- NA

    if (!is.null(fa_info[[prm]]$apply_function)) {
    # we drop this check: so we can decode arome/alaro versions in 1 call
#    if (dim(result)[3] != length(fa_info[[prm]]$fa_name))
#      warning("Not all necessary fields may be available:\n",
#           paste(parameter$fa_info, collapse="\n"))
      try(result[, , prm] <- fa_info[[prm]]$apply_function(fcdata))
    } else {
      try(result[, , prm] <- fcdata)
    }
  }
  # in some cases (RH, CC, ?) we may need to re-scale
  if (meta) {
    if (length(parameter)==1) {
      dim(result) <- dim(result)[dim(result) > 1]
      attr(result, "info")$name <- parameter
      attr(result, "info")$units <- fa_info[[1]]$units
    } else {
      attr(result, "info")$units <- vapply(fa_info, function(x) x$units, FUN.VAL="a")
      names(attr(result, "info")$units) <- parameter
    }
  } else {
    dim(result) <- dim(result)[dim(result) > 1]
  }
  result
}


#' Read FA files & interpolate
#' @param filename Name of a tar archive containing FA files
#' @param parameter The parameter(s) to be decoded.
#' @param lead_time The lead time(s) to be extracted. May be a vector!
#' @param model Name of the model. Used to find domain and interpolation weights.
#' @param iweights Interpolation weights (and domain information) can also be passed explicitely.
#' @param member Not used. Only there for API reasons.
#' @param lead_time Not used, only added to to the output table. Must be a single number.
#' @param ... Extra arguments for read_fa[tar]
#' @return A tibble with interpolated forecasts for the stations list
read_fa_interpolate <- function(file_name, parameter,
                                lead_time=0, member=NULL, model=NULL,
                                stations=NULL, method="closest", use_mask=FALSE,
                                ...) {
# TODO: pass "init" in stead of model name?
# no "hidden " arguments, but it *requires* separate initialisation
# get data as geofield
# TODO: what if you have a different set of stations, but init already exists
#       currently, no new weights are calculated
  if (!is.null(model) && exists(paste0("init_", model))) init <- get(paste0("init_", model))
  else init <- list()

  all_data <- read_fa(file_name, parameter, ...)

  if (is.null(init$weights) || attr(init$weights, "$method") != method) {
    init <- initialise_model(model, domain=all_data, stations=stations,
                             method=method, use_mask=use_mask, drop_NA=TRUE)
  }
  fcpoints <- meteogrid::point.interp(all_data, weights=init$weights)
  # this (currently) creates an array width dimensions (station,ldt,prm)
  result <- init$stations
  result$lead_time <- rep(lead_time, dim(init$stations)[1])
  for (prm in seq_along(parameter)) result[[parameter[prm]]] <- as.vector(fcpoints[,,prm])
  if (!is.null(member)) result$member <- member
  if (!is.null(model)) result$model <- model

  list(fc_data = result,
       units = tibble::tibble(parameter = parameter,
                              units = attr(all_data, "info")$units))
}


# ALARO doesn't write Tdew, so we calculate it from RH and T
#' Standard Magnus formula for dewpoint temperature
#' @param tc Temperature in degrees Celsius (Kelvin is also OK). Could be a geofield.
#' @param rh Relative humidity 0..1 (percentage is also OK).
#' @param tuning A vector with two tuning parameters for the formula. Default is from NOAA.
#' @return Dewpoint temperature
#' @export
rh2tdew <- function (tc, rh, tuning=c(17.67, 243.5)) {
# default tuning is from NOAA: (17.67, 243.5)
# tuning <- (17.27,237.3) is valid for 0<T<60, so not very good
# other tunings may be
# tuning <- c(a=17.62 , b=243.12)
# tuning <- c("a"=17.625, "b"=243.04)
# A.L. Buck '81: use two values depending on T: (17.368,238.88) T>0,
# (17.966,247.15) T<0
    if (max(rh) > 5) rh <- rh/100
    if (max(tc) > 200) tc <- tc - 273.15
    a <- tuning[1]
    b <- tuning[2]
    minrh <- 1.E-3
    rh[rh < minrh] <- minrh
    rh[rh > 1] <- 1
    gg <- log(rh) + a*tc/(tc+b)
    b * gg/(a-gg)
}

