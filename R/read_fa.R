# Read a field from an FA file
#
# @param filename The FA file name. "file@arch" signifies a file inside a tar archive.
#        It may also be a \code{FAfile} object.
# @param parameter The parameter to read. Standard HARP names are used, but full FA field names will also
#        work.
# @param meta If TRUE, also read all meta data (domain, time properties).
# @param fa_type The kind of model file: "arome", "alaro", "surfex"...
# @param fa_vector TRUE if the wind variable (speed, direction) must be calculated from components
# @param ... Ignored
# @return A 2d geofield object (2d array with projection information)
#
# NOT exported. Used internally.
# @examples
# model_geofield <- read_fa(file_name, "t2m")
# model_geofield <- read_fa(file_name, "t500")
# model_geofield <- read_fa(file_name, "topo")

read_fa <- function(filename, parameter, meta=TRUE, fa_type="arome",
                    fa_vector=TRUE, rotate_wind=TRUE, ...) {
  # TODO: if meta==TRUE, just return a simple array, no geofield or attributes
  # ?accumulated fields?
  # wind rotation, maybe with pre-calculated angle...
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
  fa_info <- lapply(parameter, get_fa_param_info, fa_type=fa_type,
                    fa_vector=fa_vector, rotate_wind = rotate_wind)

  for (prm in seq_along(parameter)) {
    # TODO: fix parameter
    # TODO: error handling: this is just a place holder for now
    # always set outform="M" (we already have domain) -> also pass faframe!
    ee <- tryCatch(fcdata <- Rfa::FAdec(fa = fafile, field = fa_info[[prm]]$fa_name,
                                        outform = "M", faframe = attr(fafile, "frame")),
                 error = function(e) e)
    if (inherits(ee, "error")) fcdata <- NA
    # FIXME: if fcdate==NA, apply_function may crash!
    # it's wrapped in try() so it shouldn't be too bad...

    if (!is.null(fa_info[[prm]]$apply_function)) {
    # we drop this check: so we can decode arome/alaro versions in 1 call
#    if (dim(result)[3] != length(fa_info[[prm]]$fa_name))
#      warning("Not all necessary fields may be available:\n",
#           paste(parameter$fa_info, collapse="\n"))
      # TODO: what if apply_function() accepts extra arguments (e.g. pre-initialised wind rotation)
      #       we could try to have them in the function environment
      #       but that would involve making a copy at every iteration
      #       some apply_functions require fcdate to be a geofield, but we just read it as an array (outform M)
      #       alternatively, we could pass domain separately, but that is against the spirit of "meteogrid"
      try(result[, , prm] <- fa_info[[prm]]$apply_function(as.geofield(fcdata, domain=fafile)))
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


# Read FA files & interpolate
# @param file_name Name of a FA file
# @param parameter The parameter(s) to be decoded.
# @param lead_time The lead time(s) to be extracted. May be a vector for some other file formats, 
#   But for FA files, a vector is not accepted. In fact,  like members this argument is completely ignored
#   except as a (constant) column to output.
# @param members Mostly ignored, but could be added as a (constant) column to output.
#        If present it must be a single string value (FA files do not contain multiple ensemble members)
# @param vertical_coordinate Not used. Only there for API reasons.
# @param init Interpolation weights (and domain information).
# @param method Interpolation method (only necessary if the weights are not yet initialised)
# @param use_mask If TRUE, use land/sea mask in interpolation
# @param fa_type For some fields (e.g. precipitation) arome and alaro
#    use different names, so we should specify.
# @param fa_vector If true, wind speed will be calculated from U and V components.

# @param ... Ignored and simply passed to read_fatar

# @param ... Extra arguments for read_fa[tar]
# @return A tibble with interpolated forecasts for the stations list
#
# NOT exported - used internally.
read_fa_interpolate <- function(file_name,
                                parameter,
                                lead_time = NA_real_,
                                members = NA_character_,
                                vertical_coordinate = NA_character_, # not taken into account
                                init = list(),
                                method = "closest", use_mask = FALSE,
                                fa_type = "arome",
                                fa_vector = TRUE,
                                ...) {

  if (length(members) > 1) stop("FA files can not contain multiple members.")
  if (length(lead_time) > 1) stop("FA files can not contain multiple lead times.")

  all_data <- read_fa(file_name, parameter, ...)

  if (is.null(init$weights) || attr(init$weights, "method") != method) {
    init <- initialise_interpolation(domain=attr(all_data, "domain"),
                                     stations=init$stations,
                                     method=method, use_mask=use_mask, drop_NA=TRUE)
  }
  fcpoints <- meteogrid::point.interp(all_data, weights=init$weights)
  # this (currently) creates an array width dimensions (station,ldt,prm)
  result <- init$stations
  result$lead_time <- rep(lead_time, dim(init$stations)[1])
  # FIXME: read_det_interpolate expects the column to be called "forecast"
  # and a separate "parameter" column
  if (length(parameter) > 1) {
    for (prm in seq_along(parameter)) result[[parameter[prm]]] <- as.vector(fcpoints[,,prm])
  } else {
    result[["forecast"]] <- as.vector(fcpoints)
    result[["parameter"]] <- parameter
  }
  if (!is.na(members)) result$member <- members

  list(fcst_data = result,
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

