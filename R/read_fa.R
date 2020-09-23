# Set options for FA decoding
#' @param fa_type The kind of model file: "arome", "alaro", "surfex"... Mainly important for precipitation fields.
#' @param fa_vector TRUE if the wind variable (speed, direction) must be calculated from components U & V
#' @param rotate_wind TRUE means wind U,V (along axes of the grid) should be rotated to actual N.
#' @param meta If TRUE, the time and grid details are also decoded. This is slower.
#' @param ... Any non-standard options that don't have default values.
#' @result Returns a list of options. Either the defaults or any modification.
fa_opts <- function(meta=TRUE, fa_type="arome", fa_vector=TRUE, rotate_wind=TRUE, ...) {
  list(meta=meta, fa_type=fa_type, fa_vector=fa_vector, rotate_wind=rotate_wind, ...)
}

# Read a field from an FA file
#
# @param filename The FA file name. "file@arch" signifies a file inside a tar archive.
#        It may also be a \code{FAfile} object.
# @param parameter The parameter to read. Standard HARP names are used, but full FA field names will also
#        work.
# @param lead_time Mostly ignored. FA files contain only 1 lead time. But added to the output.
# @param members Does not influence data, but may be added a a column to output. As a FA file can only contain 1 model,
#        this must be NULL or a single integer value, not a vector.
# @param vertical_coordinate For extracting 3D data
# @param transformation The transformation to apply to the gridded data. Can be
#   "none", "interpolate", "regrid", or "xsection".
# @param transformation_opts = Options for the the transformation. Depends on the
#   transformation. For interpolation this should include:
#     - method: the interpolation method to use. See meteogrid.
#     - use_mask: Logical. Whether to use a land-sea mask in the interpolation.
#     - stations: a dataframe of stations with columns SID, lat, lon and possibly elev
#     or
#     - weights: the interpolation weights if they have already been calculated.
#     Note that when weights are included all other options are ignored. If stations
#     are not given, the harpIO default station list is used.
#   All transformations can include the logical keep_raw_data. If this is set to
#   TRUE, the raw gridded data will be kept. If FALSE, or not set the raw gridded
#   data will be discarded.

# @param format_opts Extra options for reading FA files. See fa_opts() for details.
# @param show_progress Verbosity. Ignored.
#' @param ... Ignored
# @return A data frame with columns of metadata taken from the file and a list
#   column of the gridded and / or transformed data.
#
# NOT exported. Used internally.

read_fa <- function(file_name,
                    parameter,
                    lead_time=NULL,
                    members=NULL,
                    vertical_coordinate=NA_character_,
                    transformation="none",
                    transformation_opts=list(),
                    format_opts=fa_opts(),
                    show_progress=FALSE) {
  # TODO: if meta==TRUE, just return a simple array, no geofield or attributes
  # ?accumulated fields?
  # wind rotation, maybe with pre-calculated angle...
# harp_env$fa_infile <- infile
# harp_env$fa_domain <-
## or use the same trick as meteogrid for e.g. .Last.domain()
  if (!requireNamespace("Rfa", quietly=TRUE)) {
    stop("The Rfa package must be installed to read FA files.\n",
         "Please contact alex.deckmyn@meteo.be for the package code.")
  }

  if (is.null(parameter)) {
    stop("For fa files, parameter = '<parameter>' must be passed.", call. = FALSE)
  }

  if (!is.null(members) && length(members) > 1) stop("FA files can not contain multiple members.")

  # make sure the format options are complete
  format_opts <- do.call(fa_opts, format_opts)

  # TODO: check whether running FAopen is really optimal
  #       FAdec(filename, ...) may be faster unless we are decoding several fields at once (e.g. 3D)
  #       but we may want to have date information before decoding
  if (inherits(file_name, "FAfile")) {
    # "file_name" may in fact already be a FAfile object
    fafile <- file_name
  } else if (is.character(file_name)) {
    # if file_name contains "@" this is interpreted as a file inside a tar archive
    namsplit <- strsplit(file_name, "@")[[1]]
    fafile <- switch(length(namsplit),
                       Rfa::FAopen(filename=file_name),
                       Rfa::FAopen(filename=namsplit[1], archname=namsplit[2]),
                       stop("Could not open file ", file_name))
  } else {
    stop("bad filename")
  }

  # make a list of all parameters
  # NOTE: if the parameter is already in "harp_parameter" format (i.e. a list), wrap it into a list
  #       otherwise the lapply() will give weird results.
  if (inherits(parameter, "harp_parameter"))  {
    prm_info <- list(parameter)
  } else {
    prm_info <- lapply(parameter, parse_harp_parameter)
  }
  fa_info <- lapply(prm_info, get_fa_param_info,
                    fa_type     = format_opts$fa_type,
                    fa_vector   = format_opts$fa_vector,
                    rotate_wind = format_opts$rotate_wind)
  # a temporary hack. not very pretty.
  # we need to add fcdate & validdate  in unixdate, leadtime in hours
  # and while we're at it, we also add "members" (which is NULL or a single integer)
  blist <- list(fcdate    = as.numeric(attr(fafile, "time")$basedate),
                validdate = as.numeric(attr(fafile, "time")$validdate),
                leadtime  = attr(fafile, "time")$leadtime,
                member = members)

  prm_list <- lapply(1:length(prm_info),
                     function(i) c(blist, prm_info[[i]], fa_info[[i]]))

  # prepare the transformation (interpolation, regrid...):
  if (transformation != "none") {
    domain <- attr(fafile, "domain")
  } else {
    domain <- NULL
  }

  transformation_opts <- compute_transformation_weights(
    domain,
    transformation,
    transformation_opts
  )

  # Function to read and transform data from FA file to be used in map_dfr below.
  # This function should also include calls to interpolate, regrid and xsection so
  # that no more data is kept in memory than is necessary.
  # fa_info is a data.frame where every row represents a field to be decoded
  # row_num selects a single row
  read_and_transform_fa <- function(
    row_num, fafile, prm_list, fa_opts, transformation = "none", opts = list(), show_progress = FALSE) {
    gdat <- try(do.call(Rfa::FAdec, c(list(fafile, prm_list[[row_num]]$fa_name), fa_opts)),
                silent = TRUE)
    # NOTE: in case of failure (field not available) we may want NA entries
    if (inherits(gdat, "try-error")) return(NULL)
    # NOTE: fa_info is not part of the argument list. No real problem, though.
    if (!is.null(fa_info[[row_num]]$apply_function)) {
      gdat <- fa_info[[row_num]]$apply_function(gdat)
    }
    result <- tibble::tibble(
      fcdate       = prm_list[[row_num]]$fcdate,
      validdate    = prm_list[[row_num]]$validdate,
      lead_time    = prm_list[[row_num]]$leadtime,
      parameter    = prm_list[[row_num]]$fullname,
#      members      = prm_list[[row_num]]$member, # FIXME: should the default be NA or 0 ???
      level_type   = prm_list[[row_num]]$level_type,
      level        = prm_list[[row_num]]$level,
      units        = prm_list[[row_num]]$units,
      gridded_data = list(gdat)
    )

    result <- transform_geofield(result, transformation, opts)

    if (show_progress) pb$tick()

    result

  }

  if (show_progress) {
    pb <- progress::progress_bar$new(format = "[:bar] :percent eta: :eta", total = nrow(fa_info))
  }

  # create a data.frame with 1 row per parameter
  fa_data <- purrr::map_dfr(
    1:length(fa_info),
    read_and_transform_fa,
    fafile,
    prm_list,
    format_opts,
    transformation,
    transformation_opts,
    show_progress
  )

  attr(fa_data, "transformation_opts") <- transformation_opts

  fa_data

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

