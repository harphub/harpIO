# Read a field from an FA file in a tar archive
#
# @param filename A tar archive of FA files.
# @param parameter The parameter to read. Standard HARP names are used,
#        but full FA field names will also work. If NULL, only domain information is read.
# @param lead_time Expressed in hours.
# @param ... Arguments for \code{read_fa}
# @return A 2d geofield object (2d array with projection information).
#         If the parameter is not found, the geofield will have value NA.
#
# NOT exported - used internally.
# @examples
# model_geofield <- read_fatar(filename, "t2m", lead_time=0)
# model_geofield <- read_fa(filename, "t500", lead_time=6)

read_fatar <- function(filename, parameter, lead_time=0, levels=NULL, members=NULL,
                       fa_type="arome", fa_vector=TRUE, lt_unit="h", ...) {
  if (!requireNamespace("Rfa", quietly=TRUE)) {
    stop("The Rfa package must be installed to read FA files.")
  }
  ## TODO:
  ## fix the lead time scale?
  ##       use match() in stead of grep -> also for multiple ldt's
  ## find the most efficient way to fill an array with variable dimensions
  ## creating all dimensions always will cause overhead to "collapse" them again
  ## TODO: fatar should be able to decumulate precipitation
  if (is.list(filename)) {
    filelist <- filename
  } else {
    filelist <- Rfa::ParseTar(filename)
  }
  extra_dim <- list(ldt=lead_time, prm=parameter) #, level=levels)
#  if (is.null(parameter)) return()
  # make sure we read by ascending lead time
  # then we can do decumulations on the fly
  lead_time <- sort(lead_time)
  fa_info <- lapply(parameter, get_fa_param_info, fa_type=fa_type, fa_vector=fa_vector)

  for (ldt in seq_along(lead_time)) {
    fcfile <- grep(sprintf("+%04i$", lead_time[ldt]), names(filelist), val = TRUE)
    if (length(fcfile) != 1) {
      stop("Lead time ", lead_time[ldt], " not available in archive file\n",
           filename, "\n", length(fcfile), " hits")
    }
    fafile <- Rfa::FAopen(filelist[[fcfile]])
    if (ldt==1) {
      info <- list(origin = paste(attr(fafile, "filename"), "in", attr(fafile, "tarfile")),
                   time   = attr(fafile, "time")
                   )
      if (length(parameter) == 1) info$name <- parameter
      if (length(members)   == 1) info$mbr  <- members
      if (length(lead_time) == 1) info$time$leadtime <- lead_time

      result <- meteogrid::as.geofield(NA, domain=fafile, extra_dim=extra_dim,
                                                 info = info)
    }
    for (prm in seq_along(parameter)) {
    # TODO: read_fa also creates geofield, and FAdec too -> inefficient?
    #       need info$units
    #       either first assign read_fa output to variable and extract info$units
    #       or do a call to get_fa_
      try(result[,, ldt, prm] <- read_fa(fafile, parameter  = parameter[prm],
                                           meta=FALSE, faframe = attr(fafile, "frame"),
                                           fa_type = fa_type, fa_vector = fa_vector, ...))
      ## TODO: in fatar, it is possible to decumulate e.g. precipitation
      ## if prm$accum > 0 : if leadtime is
#      if (fa_info[[prm]]$accum > 0
    }
  }
  if (any(dim(result) == 1)) {
    dimna <- dimnames(result)[dim(result) > 1]
    dim(result) <- dim(result)[dim(result) > 1]
    # now we must fix dimnames again, because if is NULL if you change #dimensions
    dimnames(result) <- dimna
  }

  fa_info <- lapply(parameter, get_fa_param_info, fa_type = fa_type, fa_vector = fa_vector)
  attr(result, "info")$units <- vapply(fa_info, function(x) x$units, FUN.VAL="a")
  names(attr(result, "info")$units) <- parameter

  result
}

# Read fa-tar files and interpolate to a set of locations
# @param file_name Name of a tar archive containing FA files
# @param parameter The parameter(s) to be decoded.
# @param lead_time The lead time(s) to be extracted. May be a vector!
# @param members Mostly ignored, but could be added as a (constant) column to output.
#        If present it must be a single string value (FA files do not contain multiple ensemble members)
# @param vertical_coordinate Not used. Only there for API reasons.
# @param init Interpolation weights (and domain information).
# @param method Interpolation method (only necessary if the weights are not yet initialised)
# @param use_mask If TRUE, use land/sea mask in interpolation
# @param fa_type For some fields (e.g. precipitation) arome and alaro
#        use different names, so we should specify.
# @param fa_vector If true, wind speed will be calculated from U and V components.
# @param ... Ignored and simply passed to read_fatar
# @return a list of two tibbles. One with interpolated forecasts for the stations list,
#         and one with parameter units.

read_fatar_interpolate <- function(file_name, parameter,
                                   lead_time,
                                   members=NA_character_,
                                   vertical_coordinate=NULL, # not taken into account
                                   init=list(), method="closest", use_mask=FALSE,
                                   fa_type = "arome", fa_vector = TRUE, ...) {
# FIXME: it appears that there should always be a "member" column!
# TODO: pass "init" in stead of model name?
# no "hidden " arguments, but it *requires* separate initialisation
# get data as geofield
# TODO: what if you have a lagged member? then the file *is* called for multiple members?
  if (length(members) > 1) stop("FA-TAR archives do not contain multiple members.")
  all_data <- read_fatar(file_name, parameter=parameter, lead_time=lead_time,
                         fa_type = fa_type, fa_vector = fa_vector, ...)
# fix the interpolation weights (they may already exist)
  if (is.null(init$weights) || attr(init$weights, "method") != method) {
    ## TODO: use the input file as "clim_file" just in case it has topo & lsm?
    ##       only worth it if the parameter is t2m or use_mask=TRUE
    init <- initialise_interpolation(domain=attr(all_data, "domain"),
                                     stations=init$stations,
                                     method=method, use_mask=use_mask, drop_NA=TRUE)
    ## assign init to the calling function, so it can be re-used
    assign("init", init, env = parent.frame())
  }
  fcpoints <- meteogrid::point.interp(all_data, weights=init$weights)
  # this (currently) creates an array width dimensions (station[,ldt][,prm])
  fctable <- tibble::tibble(lead_time = rep(lead_time, each=dim(init$stations)[1]))
  if (length(parameter)>1) {
  # TODO: clean this up: if only 1 leadtime, you have 1 less dimension...
    if (length(lead_time)==1) {
      # FIXME: in fact you should create 1 column forecast and one column "parameter"
      for (prm in seq_along(parameter)) fctable[[parameter[prm]]] <- as.vector(fcpoints[,prm])
    } else {
      for (prm in seq_along(parameter)) fctable[[parameter[prm]]] <- as.vector(fcpoints[,,prm])
    }

  } else {
#    fctable[[parameter]] <- as.vector(fcpoints)
    fctable[["forecast"]] <- as.vector(fcpoints)
    fctable[["parameter"]] <- parameter
  }
  for (nn in names(init$stations)) fctable[[nn]] <- rep(init$stations[[nn]], length(lead_time))
  # add some (constant value) columns if requested
  if (!is.na(members)) fctable$member <- members
  units <- tibble::tibble(parameter = parameter,
                          units = attr(all_data, "info")$units)
  list(fcst_data = fctable, units = units)
}

