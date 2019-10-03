# Read/decode hdf5 files, mainly ODIM style
### assume the ODIM (Opera Data Information Model) data format
### ref: D. B. Michelson et al.
###        'EUMETNET OPERA weather radar information model
###         for implementation with the HDF5 file format'
###        version 2.2 (2014)

### read data from accumulated rainfall products
### reading the grid properties etc is about 30% of the time...
### reading raw matrix will work for any HDF5 file, but meteogrid attributes only for ODIM !
### ODIM data also might be in .../quality1/data, but for now we don't consider that.

### TO DO: optional: return more meta data like product type etc. ?
# @param filename The hdf5 file name.
# @param data The location of the data
# @param meta If TRUE, also read all meta data (domain, time properties).
# @param ... Ignored
# @return A geofield object (if meta is TRUE) or a plain matrix.
# NOT exported. Used internally.
read_hdf5 <- function(filename, data="dataset1/data1/data", meta=TRUE, ...) {
  if (!requireNamespace("h5", quietly=TRUE)) {
    stop("The h5 package is not installed!", "Please install from CRAN.")
  }
  # open hdf5 file
  fName <- path.expand(filename)
  if (is.na(fName) || !file.exists(fName)) {
    stop("File", filename, "missing or file not found.")
  }
  if (!h5::is.h5file(fName)) stop("Not a HDF5 file.")
  ff <- h5::h5file(fName, "r")
  on.exit(tryCatch(h5::h5close(ff), error=function(e){}, warning=function(w){}))

  # 1. get data itself
  if (!h5::existsDataSet(ff,data)) stop("Data not found.")
  zz <- t(h5::readDataSet(ff[data]))
  zz <- zz[, ncol(zz):1]  # transpose and put upside-down
  # ODIM-specific?

  # We need to find attributes that may be at different paths
  # so we start by setting up a hierarchy of 'where' and 'what' groups
  ## TODO: there must be a more efficient way (missing data and scaling are important)
  split.path1 <- unlist(strsplit(data, "/")[[1]])
  split.path <- split.path1[nchar(split.path1)>0]  # fixes e.g. leading "/"

  ### simple but effective:
  datasetN <- split.path[1]
  dataN <- split.path[2]

  if (h5::existsGroup(ff, "where") ) {
    root.where <- h5::list.attributes(ff["where"])
  } else root.where <- character(0)

  if (h5::existsGroup(ff[datasetN], "where")) {
    dataset.where <- h5::list.attributes(ff[datasetN]["where"])
  } else dataset.where <- character(0)

  if (h5::existsGroup(ff[datasetN][dataN], "where")) {
    data.where <- h5::list.attributes(ff[datasetN][dataN]["where"])
  } else data.where <- character(0)

  if (h5::existsGroup(ff, "what") ) {
    root.what <- h5::list.attributes(ff["what"])
  } else root.what <- character(0)

  if (h5::existsGroup(ff[datasetN], "what")) {
    dataset.what <- h5::list.attributes(ff[datasetN]["what"])
  } else dataset.what <- character(0)

  if (h5::existsGroup(ff[datasetN][dataN], "what")) {
    data.what <- h5::list.attributes(ff[datasetN][dataN]["what"])
  } else data.what <- character(0)

  get.where <- function(aname) {
    if (aname %in% data.where) return(h5::h5attr(ff[datasetN][dataN]["where"], aname))
    if (aname %in% dataset.where) return(h5::h5attr(ff[datasetN]["where"], aname))
    if (aname %in% root.where) return(h5::h5attr(ff["where"], aname))
    return(NA)
  }

  get.what <- function(aname) {
    if (aname %in% data.what) return(h5::h5attr(ff[datasetN][dataN]["what"], aname))
    if (aname %in% dataset.what) return(h5::h5attr(ff[datasetN]["what"], aname))
    if (aname %in% root.what) return(h5::h5attr(ff["what"], aname))
    return(NA)
  }

  # ODIM-specific?
  # offset and gain
  offset <- get.what("offset")
  gain <- get.what("gain")
  if (!is.na(gain) && gain != 1) zz <- zz * gain
  if (!is.na(offset) && offset != 0) zz <- zz + offset

  # missing data (this should be available, but it can be missing)
  nodata <- get.what("nodata")
  if (!is.na(nodata)) zz[zz == nodata] <- NA

  # no rainfall detected
  nodetect <- get.what("nodetect")
  if (!is.na(nodetect)) zz[zz == nodetect] <- 0

  # 2. extract domain specifications (only for ODIM, probably)
  if (meta) {
    if (!requireNamespace("meteogrid", quietly = TRUE)) {
      stop("Package meteogrid could not be found.\n",
        "Please install meteogrid or set meta=FALSE.")
    }
    # get projection definition and grid properties
    # SW and NE are shifted by half grid box to get box centers (A-grid !)
    pp <- get.where("projdef")
    dx <- get.where("xscale")
    dy <- get.where("yscale")
    SW.ll <- c(get.where("LL_lon"), get.where("LL_lat"))
    NE.ll <- c(get.where("UR_lon"), get.where("UR_lat"))
    if (any(is.na(c(pp, dx, dy, SW.ll, NE.ll)))) stop("Not all grid info was found.")

    projection <- meteogrid::proj4.str2list(pp)
    SW.xy <- meteogrid::project(SW.ll, proj=projection)
    NE.xy <- meteogrid::project(NE.ll, proj=projection)

    CXY <- (SW.xy + NE.xy) / 2

    # define the 'geodomain'
    domain <- list(
      projection = projection,
      dx = dx, dy = dy, nx = dim(zz)[1], ny = dim(zz)[2],
      clonlat = as.numeric(meteogrid::project(CXY, proj = projection, inv = TRUE)),
      SW = as.numeric(meteogrid::project(SW.xy + c(dx/2, dy/2), proj = projection, inv = TRUE)),
      NE = as.numeric(meteogrid::project(NE.xy - c(dx/2, dy/2), proj = projection, inv = TRUE)))

    class(domain)  <- "geodomain"


    # get time info
    bdate <- get.what("startdate") ## YYYYMMDD
    btime <- get.what("starttime") ## HHMMSS
    edate <- get.what("enddate") ## YYYYMMDD
    etime <- get.what("endtime") ## HHMMSS
    obsname <- get.what("product")
    if (any(is.na(c(bdate, btime, edate, etime)))) {
      warning("Not all date/time information was found.")
      accum <- NA
      #      obsname <- "?Accumulated precipitation?"
      gftime <- ""
    } else {
      bdate <- as.POSIXct(paste(bdate, btime), format="%Y%m%d %H%M%S", tz="UTC")
      edate <- as.POSIXct(paste(edate, etime), format="%Y%m%d %H%M%S", tz="UTC")
      gftime <- format(edate, "%Y%m%d %H:%M")
      # accumulation time in seconds
      accum <- as.numeric(edate) - as.numeric(bdate)
      if (accum %% 3600 == 0) obsname <- sprintf("%ih Accumulated precipitation", accum/3600)
      else if (accum %%60 == 0) obsname <- sprintf("%im Accumulated precipitation", accum/60)
      else obsname <- sprintf("%is Accumulated precipitation", accum)
    }
    # turn the data into a geofield object
    zz <- meteogrid::as.geofield(zz, domain=domain,
                                 info=list(name=obsname, origin=basename(fName),
                                           accum=accum,
                                           time=list(basedate=edate, accum=accum)))
  }
  # close file
  h5::h5close(ff)

  # that's all, folks
  return(zz)
}
