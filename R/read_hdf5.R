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

hdf5_opts <- function(data_path=NULL, odim=TRUE, meta=TRUE, invert_data=TRUE, ...) {
  list(data_path=data_path, odim=odim, meta=meta, invert_data=invert_data, ...)
}

# get hdf5 parameter names according to ODIM standard
get_hdf5_param_info <- function(param) {
  param <- parse_harp_parameter(param)
  result <- switch(tolower(param$basename),
                     "pcp" = list(quantity="ACRR", units="mm"),
                     "rr"  = list(quantity="RATE", units="mm/h"),
                     "u"   = list(quantity="UWIND", units="m/s"),
                     "v"   = list(quantity="VWIND", units="m/s"),
                     "s"   = list(quantity="ff", units="mm"),
                     "d"   = list(quantity="dd", units=""),
                     "ref" = list(quantity="DBHZ", units="dBZ"),
                     list(quantity="unknown", units="unknown"))
  result
}

# Read hdf5 data
# @param file_name The hdf5 file name.
# @param data The location of the data
# @param parameter The parameter to be read. May also be the path of the data inside the hdf5 file.
# @param ... Ignored
# @return A geofield object (if meta is TRUE) or a plain matrix.
# NOT exported. Used internally.
read_hdf5 <- function(
      file_name,
      parameter           = NULL,
      lead_time           = NULL,
      members             = NULL,
      vertical_coordinate = NA_character_,
      transformation      = "none",
      transformation_opts = list(),
      format_opts         = hdf5_opts(),
      show_progress       = FALSE,
      ...) {

  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop(
      "read_hdf5 requires the hdf5r package. Install (from CRAN) with the following command:\n",
      "tall_packages(\"hdf5r\")",
      call. = FALSE
    )
  }
  format_opts <- do.call(hdf5_opts, format_opts)
  if (inherits(parameter, "harp_parameter")) {
    parameter <- list(parameter)
  }
  parameter      <- lapply(parameter, parse_harp_parameter)
  param_info     <- lapply(parameter, get_hdf5_param_info)
  unknown_params <- which(sapply(param_info, function(x) x$quantity == "unknown" ))
  if (length(unknown_params) > 0) {
    lapply(
      unknown_params,
      function(x) warning(
        "Don't know how to read '", parameter[[x]]$fullname, "' from hdf5 files.",
        immediate. = TRUE,
        call.      = FALSE
      )
    )
    parameter  <- parameter[-unknown_params]
    param_info  <- param_info[-unknown_params]
  }
  if (length(parameter) < 1) {
    stop("None of the requested parameters can be read from HDF5 files.", call. = FALSE)
  }

  # prepare the transformation (interpolation, regrid...):
  # FIXME: calling Hopen for the domain is pretty slow
  #        if the domain never changes, you may want to skip this?
  if (transformation != "none") {
    domain <- attr(Hopen(file_name), "domain")
  } else {
    domain <- NULL
  }

  transformation_opts <- compute_transformation_weights(
    domain,
    transformation,
    transformation_opts
  )
  # FIXME: If you know the data is always in a particular data path, it is faster to
  #        call Hdec immediately! Just set the path via hdf5_options()?
  #        Hopen may be useful for extracting domain information
  #        BUT: then we have no way of knowing validdate -> can not be in result
#  if (!is.null(format_opts$data_path)) {
#    result <- tibble(
#      validdate    = prm_list$validdate[1],
#      parameter    = prm_list$quantity[],
#      gridded_data = list(Hdec(hdf5file, prm_list$data_path[row_num], meta=FALSE))
#                     gdata <- Hdec(file_name, format_opts$data_path)

  hdf5_info <- Hopen(file_name)
  # We get a tibble with validdate, parameter, data_path,
  # now we select only those rows that we want to decode
  # for now, we make this very, very simple
  # but that won't last...
  filter_info <- function(parameter, param_info, hdf5_info) {
    hdf5_info <- hdf5_info[hdf5_info$quantity == param_info$quantity,]
    if (parameter$accum > 0) hdf5_info <- hdf5_info[hdf5_info$accum == parameter$accum,]
    hdf5_info
  }
#  class(hdf5_info) <- rev(class(hdf5_info))
  hdf5_info <- purrr::map2_dfr(parameter, param_info, filter_info, hdf5_info)
#  class(hdf5_info) <- rev(class(hdf5_info))

  # Function to read and transform data from HDF5 file to be used in map_dfr below.
  # This function should also include calls to interpolate, regrid and xsection so
  # that no more data is kept in memory than is necessary.
  # fa_info is a data.frame where every row represents a field to be decoded
  # row_num selects a single row
  read_and_transform_hdf5 <- function(
    row_num, hdf5file, prm_list, format_opts, transformation = "none",
    transformation_opts = list(), show_progress = FALSE) {

    # NOTE: in case of failure (field not available) we may want NA entries

#    if (!is.null(hdf5_info[[row_num]]$apply_function)) {
#      gdat <- fa_info[[row_num]]$apply_function(gdat)
#    }
    result <- tibble::tibble(
#      fcdate       = prm_list[[row_num]]$fcdate,
      validdate    = prm_list$validdate[row_num],
#      lead_time    = prm_list[[row_num]]$leadtime,
      parameter    = prm_list$quantity[row_num],
#      members      = prm_list[[row_num]]$member, # FIXME: should the default be NA or 0 ???
#      level_type   = prm_list[[row_num]]$level_type,
#      level        = prm_list[[row_num]]$level,
#      units        = prm_list[[row_num]]$units,
      gridded_data = list(Hdec(hdf5file, prm_list$data_path[row_num],
                               odim=format_opts$odim, meta=format_opts$meta,
			       invert_data=format_opts$invert_data))
    )

    result <- transform_geofield(result, transformation, transformation_opts)
# FIXME: to have a geofield output, I need to extract domain info every time again

    result
  }

  if (show_progress) {
    show_progress <- list(
      name = "Reading hdf5 file",
      show_after = 1
    )
  }

  # create a data.frame with 1 row per parameter
  hdf5_data <- purrr::map(
    1:nrow(hdf5_info),
    read_and_transform_hdf5,
    file_name,
    hdf5_info,
    format_opts,
    transformation,
    transformation_opts,
    .progress = show_progress
  ) %>%
    purrr::list_rbind()

  attr(hdf5_data, "transformation_opts") <- transformation_opts

  hdf5_data

}
# like FAdec, Gdec, Bdec we add Hdec
# And also Hopen: scan a hdf5 file for all data sets etc.
#       could be very useful
#
Hopen <- function(file_name, odim=TRUE, meta=TRUE) {
   # open hdf5 file
   # return: object pointer, data list, ...
   # try to make a list of parameter/data_path available in file
   # maybe also geodomain extraction?
   # NOTE: odim=TRUE & meta=TRUE can double the system time of Hopen()
  file_name <- path.expand(file_name)
  if (is.na(file_name) || !file.exists(file_name)) {
    stop("File", file_name, "missing or file not found.")
  }
  if (!hdf5r::is.h5file(file_name)) stop("Not a HDF5 file.")
  hf <- hdf5r::H5File$new(file_name, "r")
  on.exit(tryCatch(hf$close_all(), error=function(e){}, warning=function(w){}))

  # listing
  full_list <- hf$ls(recursive=TRUE)
  dataset_list <- full_list[which(full_list$obj_type == "H5I_DATASET"),]
  data_list <- data.frame(data_path=dataset_list$name,
#                          rank=dataset_list$dataset.rank,
                          dim=dataset_list$dataset.dims)
#  data_list$dims <- lapply(strsplit(data_list$dim, "x"), as.numeric) # list of dim vectors
  if (odim) {
    data_list$quantity <- NA_character_
    data_list$accum <- NA
    data_list$edate <- as.POSIXct(NA)
    for(i in seq_along(data_list$data_path)) {
      all_what <- do.call(c,
                          lapply(c(sub("data$", "what", data_list$data_path[i]),
                               sub("data[[:digit:]]+/data$", "what", data_list$data_path[i]),
                               "what"),
                      function(x) if (hdf5r::existsGroup(hf,x)) hdf5r::h5attributes(hf[[x]]) else NULL))
      data_list$quantity[i] <- all_what$quantity
      bdate <- all_what$startdate ## YYYYMMDD
      btime <- all_what$starttime ## HHMMSS
      edate <- all_what$enddate ## YYYYMMDD
      etime <- all_what$endtime ## HHMMSS
      bdate <- as.POSIXct(paste(bdate, btime), format="%Y%m%d %H%M%S", tz="UTC")
      edate <- as.POSIXct(paste(edate, etime), format="%Y%m%d %H%M%S", tz="UTC")
      data_list$accum[i] <- as.numeric(edate) - as.numeric(bdate)
      data_list$validdate <- edate
    }
    # extract domain information from first dataset
    if (meta) {
      all_where <- do.call(c,
                        lapply(c(sub("data$", "where", data_list$data_path[i]),
                                 sub("data[[:digit:]]+/data$", "where", data_list$data_path[i]),
                                 "where"),
                        function(x) if (hdf5r::existsGroup(hf,x)) hdf5r::h5attributes(hf[[x]]) else NULL))

      pp <- all_where$projdef
      dx <- all_where$xscale
      dy <- all_where$yscale
      nx <- all_where$xsize
      ny <- all_where$ysize
      SWlon <- all_where$LL_lon
      SWlat <- all_where$LL_lat
      NElon <- all_where$UR_lon
      NElat <- all_where$UR_lat
      if (any(is.null(c(pp, dx, dy, nx, ny, SWlon, SWlat, NElon, NElat)))) stop("Not all grid info was found.")

      projection <- meteogrid::proj4.str2list(pp)
      SW.xy <- meteogrid::project(c(SWlon, SWlat), proj=projection)
      NE.xy <- meteogrid::project(c(NElon, NElat), proj=projection)

      CXY <- (SW.xy + NE.xy) / 2

      # define the 'geodomain'
      domain <- list(
        projection = projection,
        dx = dx, dy = dy, nx = nx, ny = ny,
        clonlat = as.numeric(meteogrid::project(CXY, proj = projection, inv = TRUE)),
        SW = as.numeric(meteogrid::project(SW.xy + c(dx/2, dy/2), proj = projection, inv = TRUE)),
        NE = as.numeric(meteogrid::project(NE.xy - c(dx/2, dy/2), proj = projection, inv = TRUE)))

      class(domain)  <- "geodomain"
      attr(data_list, "domain") <- domain
      class(data_list) <- c("HDF5list", class(data_list))
    }
  }
  attr(data_list, "filename") <- file_name
  hf$close_all()
  data_list
}

# Main HDF5 decoding: filename & single data_path
Hdec <- function(file_name, data_path="dataset1/data1/data", meta=TRUE, invert_data=TRUE, ...) {
#    data="dataset1/data1/data", meta=TRUE, ...) {
  if (!requireNamespace("hdf5r", quietly=TRUE)) {
    stop("The hdf5r package is not installed!", "Please install from CRAN.")
  }
#  print(file_name)
#  print(data_path)
  # open hdf5 file
  file_name <- path.expand(file_name)
  if (is.na(file_name) || !file.exists(file_name)) {
    stop("File ", file_name, " not found.")
  }
  if (!hdf5r::is.h5file(file_name)) stop(file_name, " is not a HDF5 file.")

  hf <- hdf5r::H5File$new(file_name, "r")
  on.exit(tryCatch(ff$close_all(), error=function(e){}, warning=function(w){}))

  # 1. get data itself
#  if (!hdf5r::existsDataSet(hf,data)) stop("Data not found.")
  # TODO: there may be multiple data sets -> make a loop? Maybe only in read_hdf5()
  #       if the path is not defined, we should LOOK FOR IT via the parameter? Or in read_hdf5()
  #### my_data <- t(hf[[data_path]]$read())

  if (invert_data){
	  my_data <- t(hf[[data_path]]$read())
	  my_data <- my_data[, ncol(my_data):1]  # transpose and put upside-down
  } else {
	  my_data <- hf[[data_path]]$read()
  }
  # ODIM-specific?

  # We need to find attributes that may be at different paths
  # so we start by setting up the hierarchy of 'where' and 'what' groups in 1 list
  all_what <- do.call(c,
                      lapply(c(sub("data$", "what", data_path),
                               sub("data[[:digit:]]+/data$", "what", data_path),
                               "what"),
                      function(x) if (hdf5r::existsGroup(hf,x)) hdf5r::h5attributes(hf[[x]]) else NULL))
  all_where <- do.call(c,
                      lapply(c(sub("data$", "where", data_path),
                               sub("data[[:digit:]]+/data$", "where", data_path),
                               "where"),
                      function(x) if (hdf5r::existsGroup(hf,x)) hdf5r::h5attributes(hf[[x]]) else NULL))

  # we now have vectors with all attributes
  # BUT: what if there are duplicates? apparantly, all_what[NAME] will give the first -> OK

  # ODIM-specific?
  # offset and gain
  offset <- all_what$offset
  gain <- all_what$gain
  if (!is.null(gain) && gain != 1) my_data <- my_data * gain
  if (!is.null(offset) && offset != 0) my_data <- my_data + offset

  # missing data (this should be available, but it can be missing)
  nodata <- all_what$nodata
  if (!is.null(nodata)) my_data[my_data == nodata] <- NA

  # no rainfall detected
  nodetect <- all_what$nodetect
  if (!is.null(nodetect)) my_data[my_data == nodetect] <- 0
  result <- my_data

  # 2. extract domain specifications (only for ODIM, probably)
  if (meta) {
    if (!requireNamespace("meteogrid", quietly = TRUE)) {
      stop("Package meteogrid could not be found.\n",
        "Please install meteogrid or set meta=FALSE.")
    }
    # get projection definition and grid properties
    # SW and NE are to be shifted by half grid box to get box centers (A-grid !)
    pp <- all_where$projdef
    dx <- all_where$xscale
    dy <- all_where$yscale
    SWlon <- all_where$LL_lon
    SWlat <- all_where$LL_lat
    NElon <- all_where$UR_lon
    NElat <- all_where$UR_lat
    if (any(is.null(c(pp, dx, dy, SWlon, SWlat, NElon, NElat)))) stop("Not all grid info was found.")

    projection <- meteogrid::proj4.str2list(pp)
    SW.xy <- meteogrid::project(c(SWlon, SWlat), proj=projection)
    NE.xy <- meteogrid::project(c(NElon, NElat), proj=projection)

    CXY <- (SW.xy + NE.xy) / 2

    # define the 'geodomain'
    domain <- list(
      projection = projection,
      dx = dx, dy = dy, nx = dim(my_data)[1], ny = dim(my_data)[2],
      clonlat = as.numeric(meteogrid::project(CXY, proj = projection, inv = TRUE)),
      SW = as.numeric(meteogrid::project(SW.xy + c(dx/2, dy/2), proj = projection, inv = TRUE)),
      NE = as.numeric(meteogrid::project(NE.xy - c(dx/2, dy/2), proj = projection, inv = TRUE)))

    class(domain)  <- "geodomain"

    # type of data
    obs_type <- all_what$object # e.g. "COMP", "CVOL", "IMAGE"
    obs_quantity <- all_what$quantity # e.g. "ACCR"
    # also product?  obsname <- all_what["product"]
    obsname <- if (!is.null(obs_quantity)) obs_quantity else "unknown"

    # get time info
    bdate <- all_what$startdate ## YYYYMMDD
    btime <- all_what$starttime ## HHMMSS
    edate <- all_what$enddate ## YYYYMMDD
    etime <- all_what$endtime ## HHMMSS
    if (any(is.null(c(bdate, btime, edate, etime)))) {
      warning("Not all date/time information was found.")
      accum <- NA
      gftime <- ""
    } else {
      bdate <- as.POSIXct(paste(bdate, btime), format="%Y%m%d %H%M%S", tz="UTC")
      edate <- as.POSIXct(paste(edate, etime), format="%Y%m%d %H%M%S", tz="UTC")
      gftime <- format(edate, "%Y%m%d %H:%M")
      # accumulation time in seconds
      accum <- as.numeric(edate) - as.numeric(bdate)

      if (accum %% 3600 == 0) obsname <- paste(obsname, sprintf("%ih", accum/3600))
      else if (accum %%60 == 0) obsname <- paste(obsname, sprintf("%im", accum/60))
      else obsname <- paste(obsname, sprintf("%is", accum))
    }
    # turn the data into a geofield object
    # NOTE: we use the "enddate" as reference date.
    result <- meteogrid::as.geofield(result,
                                     domain=domain,
                                     info=list(name=obsname, origin=basename(file_name),
                                               accum=accum,
                                               time=list(basedate=edate, accum=accum)))
  }
  # close file
#  hdf5r::h5close(ff)
  hf$close_all()

  # that's all, folks
  return(result)
}
