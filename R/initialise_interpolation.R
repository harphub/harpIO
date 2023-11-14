# Initialise domain, interpolation, land/sea mask etc. for a model
#
# @param filename The path of a "clim" file containing topography & land/sea mask
# @param file_format File type of the clim file (fa, grib ...).
# @param parameter A parameter that can be read from the file (only for domain)
#        if correct_t2m == TRUE, parameter is always set to "topo"
# @param domain A \code{geodomain}, only needed if it can not be retrieved from clim file.
# @param stations The locations for which to initialise interpolation.
#    If NULL, a default (global) list is taken.
# @param method Interpolation method ("bilin", "nn"...)
# @param use_mask Should L/S mask be applied when initialising the interpolation?
# @param drop_NA Should weights/stations that are NA be removed?
# @correct_t2m Should model elevation be added for lapse rate correction?
# @param ... Arguments for interpolation method. Currently not used.
# @return A list with various initialisations.
# Not exported - used internally

initialise_interpolation <- function(file_name=NULL, file_format=NULL,
                                     parameter="sfc_geo",
                                     domain = NULL,
                                     stations=NULL,
                                     method="closest", use_mask=FALSE,
                                     wind_rotation=FALSE,
                                     correct_t2m=FALSE,
                                     drop_NA=TRUE, ...) {
  # default station list:
  if (is.null(stations)) stations <- harpCore::station_list
  # some file types need only the stations anyway
  # NO: also model_elevation should be initialised for vfld & netcdf
  #     so all formats are equivalent...
  # Also, we don't want e.g. netcdf to do the model_elevation for every file
  # but for now, we will leave it like this, because netcdf uses different
  # interpolation code. TODO: !!!
  # AD: I think we could extract topo for netcdf already?
  if (!is.null(file_name) && is.null(file_format)) {
    file_format <- guess_format(file_name)
  }

  if (!is.null(file_format) && file_format %in% c("netcdf", "vfld")) {
    return(list(stations=stations))
  }

  init <- list(stations = stations, method = method, use_mask = use_mask)
  if (!is.null(domain)) init$domain <- meteogrid::as.geodomain(domain)

  if (correct_t2m) {
    if (is.null(file_name)) {
      stop("For T2m correction, a file must be provided containing topography.")
    }

    ## if a filename is specified, we could extract topo in any case
    ## after all, we have to extract at least 1 field to get the domain
    ## unless we know an extra function like "open_XXX" (FAopen, Gopen return domain info)
    ## if the file doesn't contain "topo" we'll get a warning,
    ## but still have domain info (well, for most formats at least).
    err <- try(pfield <- read_grid(filename,
                                   "sfc_geo",
                                   file_format)/9.80655, silent=TRUE)

    if (inherits(err, "try-error")) {
      err <- try(pfield <- read_grid(filename,
                                     "oro",
                                     file_format), silent=TRUE)
    }

    if (inherits(err, "try-error") ) {
      warning("Error reading topography.", immediate.=TRUE)
    } else {
      if (is.null(init$domain)) init$domain <- attr(pfield, "domain")
      ## TODO: check that this never fails?
      if (!any(is.na(pfield))) init$topo <- pfield
      else warning("Topography field contains missing values.", immediate.=TRUE)
    }
  }

  if (use_mask) {
    if (!"lsm" %in% names(stations)) stop("Can not use L/S mask: station list does not have this data.")
    if (is.null(file_name)) {
      stop("For Land/Sea mask, a file must be provided containing LSM.")
    }
    err <- try(pfield <- read_grid(file_name,
                                       parameter="lsm",
                                       file_format=file_format), silent=TRUE)
    if (inherits(err, "try-error") || any(is.na(pfield))) {
      warning("Could not read land/sea mask (or it contains missing values).", immediate.=TRUE)
      stop("Can not use L/S mask without lsm field.")
    } else {
      init$lsm <- pfield
      if (is.null(init$domain)) init$domain <- attr(init$lsm, "domain")
    }
  }

  # maybe some read_XXX functions don't return domain information when "topo" is missing:
  if (is.null(init$domain) && !is.null(parameter)) {
    if (is.null(file_name)) {
      stop("If no domain is provided, a file must be provided containing at least one parameter.")
    }
    err <- try(pfield <- read_grid(file_name, parameter, file_format), silent=TRUE)
    if (inherits(err, "try-error") ) {
      warning("Could not read ", parameter,".", immediate.=TRUE)
    } else {
      init$domain <- attr(pfield, "domain")
    }
  }

  if (!is.null(init$domain)) {
    iweights <- meteogrid::point.interp.init(domain=init$domain,
                                               lon=stations$lon, lat=stations$lat,
                                               method=method,
                                               mask=if (use_mask) init$lsm else NULL,
                                               pointmask=stations$lsm, ...)
    ## keep only the stations/weights that are non-NA (e.g. inside the domain)?
    if (drop_NA) {
      drop <- apply(iweights, 1, function(x) any(is.na(x)))
      stations <- stations[!drop,]
      iweights <- iweights[!drop,]
    }

    if (!is.null(init$topo)) {
        stations$model_elevation <- meteogrid::point.interp(
                                    infield=init$topo, weights=iweights)
    }
    # initialise wind rotation
    if (wind_rotation) {
      geowind <- meteogrid::geowind.init(init$domain)
      # we can even interpolate the angle and map factor
      # so later, after inerpolating wind direction, just add this rotation correction
      stations$wind_rotation <- meteogrid::point.interp(geowind$angle, weights=iweights)
      stations$wind_factor <- meteogrid::point.interp(geowind$mapfactor, weights=iweights)
      init$geowind <- geowind
    }
    init$weights <- iweights
    init$stations <- stations
  } else {
    message("No domain information available. Can not initialise interpolation.")
  }
  invisible(init)
}
