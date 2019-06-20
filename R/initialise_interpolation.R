#' Initialise domain, interpolation, land/sea mask etc. for a model
#'
#' @param climfile The path of a "clim" file containing topography & land/sea mask
#' @param ftype File type of the clim file (fa, grib ...).
#' @param domain A \code{geodomain}, only needed if it can not be retrieved from clim file.
#' @param stations The locations for which to initialise interpolation
#' @param method Interpolation method ("bilin", "nn"...)
#' @param use_mask Should L/S mask be applied when initialising the interpolation?
#' @param drop_NA Should weights/stations that are NA be removed?
#' @param ... Arguments for interpolation method. Currently not used.
#' @return A list with various initialisations. It is also assigned to the global environment.
#' @export

initialise_interpolation <- function(clim_file=NULL, file_format="fa", 
                                     domain = NULL,
                                     stations=NULL, 
                                     method="closest", use_mask=FALSE,
                                     correct_t2m=FALSE,
                                     drop_NA=TRUE, ...) {
  # default station list:
  if (is.null(stations)) stations <- get("station_list")
  # some file types need only the stations anyway
  # NO: also model_elevantion should be initialised for vfld & netcdf
  #     so all formats are equivalent...
  # Also, we don't want e.g. netcdf to do the model_elevation for every file
  # but for now, we will leave it like this, because netcdf uses different 
  # interpolation code. TODO: !!!
  if (file_format %in% c("ncdf", "vfld")) return(list(stations=stations))
#  message("initialising interpolation")

  init <- list()
  if (!is.null(clim_file)) {
    ## if a clim_file is specified, we extract topo in any case
    ## after all, we have to extract at least 1 field to get the domain
    ## unless we know an extra function like "open_XXX" (FAopen, Gopen return domain info)
    ## if the file doesn't contain "topo" we'll get a warning, but still have domain info.
    read_function <- get(paste0("read_", file_format))
#    if (correct_t2m) {
      err <- try(init$topo <- read_function(clim_file, parameter="topo")/9.80655, silent=TRUE)
      # note that if topo is not available, there is no error, just a warning!
      # that's fine, because from an NA field we can still get domain information.
      if (inherits(err, "try-error")) warning("Could not read topographic data.", immediate.=TRUE)
      else init$domain <- attr(init$topo, "domain")
      if (any(is.na(init$topo))) {
        init$topo <- NULL
      }
#    }
    if (use_mask) {
      err <- try(init$lsm <- read_function(clim_file, parameter="lsm"), silent=TRUE)
      if (inherits(err, "try-error")) warning("Could not read topographic data.", immediate.=TRUE)
      else if (is.null(init$domain)) init$domain <- attr(init$topo, "domain")
    }
  } else if (!is.null(domain)) {
#    print(str(domain))
    message("Domain provided")
    init$domain <- meteogrid::as.geodomain(domain)
  } else {
#    if (!"domain" %in% names(init)) stop("You must provide a clim file or a domain definition.")
    # you have no domain information, so you can't initialise anything else (yet)
    return(list(stations=stations))
  }
  if (use_mask) {
    if (!"lsm" %in% names(init)) stop("Can not use L/S mask without lsm field.")
    if (!"lsm" %in% names(stations)) stop("Can not use L/S mask: station list does not have this data.")
  }
  if (is.null(init$domain)) stop("No domain information available. Can not interpolate.")
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
#  if (correct_t2m) {
  if ("topo" %in% names(init) && !any(is.na(init$topo))) {
      stations$model_elevation <- meteogrid::point.interp(
                                  infield=init$topo, weights=iweights)
#  } else {
#      if (! "model_elevation" %in% names(stations))
#      stop("No model elevation available. Can not do T2m corrections.\n",
#           "Please provide a clim_file containing model elevation.")
#    }
  }
  init$weights <- iweights
  init$stations <- stations
  init$method <- method

  invisible(init)
}
