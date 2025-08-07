#' Set options for FA decoding
#'
#' @param fa_type The kind of model file: "arome", "alaro", "surfex"... Mainly important for precipitation fields.
#' @param fa_vector TRUE if the wind variable (speed, direction) must be calculated from components U & V
#         OBSOLETE! We now distinguish by using a different parameter name, e.g. "S" vs "WS"
#' @param rotate_wind TRUE means wind U,V (along axes of the grid) should be rotated to actual N.
#' @param meta If TRUE, the time and grid details are also decoded. This is slower.
#' @param ... Any non-standard options that don't have default values.
#' @return Returns a list of options. Either the defaults or any modification.
#' @export
fa_opts <- function(meta=TRUE, fa_type="arome", fa_vector=TRUE, rotate_wind=TRUE, ...) {
  list(meta=meta, fa_type=fa_type, fa_vector=fa_vector, rotate_wind=rotate_wind, ...)
}

# Read a field from an FA file
#
# @param file_name The FA file name. "file@arch" signifies a file inside a tar archive.
#        It may also be a \code{FAfile} object.
# @param parameter The parameter(s) to read. Standard HARP names are used, 
#        but full FA field names will also work.
# @param is_forecast
# @param date_times Vector of requested forecast dates. Note that FA files can only contain 1 date.
#        Normally, this date should correspond to the internal date of the FA file.
#        UNIX date (i.e. numerical).
# @param lead_time Mostly ignored. FA files contain only 1 lead time. But added to the output.
# @param members Does not influence data, but may be added a a column to output. 
#        As a FA file can only contain 1 model,
#        this must be NULL or a single integer value, not a vector.
# @param vertical_coordinate For extracting 3D data.
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
# @param_defs
# @param show_progress Verbosity. Ignored.
# @param ... Ignored
# @return A data frame with columns of metadata taken from the file and a list
#   column of the gridded and / or transformed data.
#
# NOT exported. Used internally.

read_fa <- function(
  file_name,
  parameter,
  is_forecast         = TRUE,
  date_times          = NULL,
  lead_time           = NULL,
  members             = NULL,
  vertical_coordinate = NA_character_,
  transformation      = "none",
  transformation_opts = list(),
  format_opts         = fa_opts(),
  param_defs          = getExportedValue("harpIO", "harp_params"),
  show_progress       = FALSE,
  ...
) {
  # TODO: if meta==TRUE, just return a simple array, no geofield or attributes
  # ?accumulated fields?
  # wind rotation, maybe with pre-calculated angle...
# harp_env$fa_infile <- infile
# harp_env$fa_domain <-
## or use the same trick as meteogrid for e.g. .Last.domain()
  if (!requireNamespace("Rfa", quietly = TRUE)) {
    stop(
      "read_grib requires the Rfa package. Install with the following command:\n",
      "remotes::install_github(\"harphub/Rfa\")",
      call. = FALSE
    )
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
    stop("Bad filename: ", file_name, call. = FALSE)
  }

  # make a list of all parameters
  # NOTE: if the parameter is already in "harp_parameter" format (i.e. a list), wrap it into a list
  #       otherwise the lapply() will give weird results.
  if (is.list(parameter) && inherits(parameter, "harp_parameter"))  {
    parameter <- list(parameter)
  }
  parameter <- lapply(parameter, harpIO:::parse_harp_parameter, vertical_coordinate)

  param_info     <- lapply(
    parameter, get_fa_param_info, vertical_coordinate, param_defs
  )

  # AT THIS POINT:
  # param_info is a list (one entry per parameter), where every element itself is a list with 2 entries:
  #   $name etc: a list with name (may be a list of component fields!), level, etc.
  #   $func : a function to apply to to the component fields to get final field

  #unknown_params <- which(sapply(param_info, function(x) any(is.na(x$fa_name))))

  #FIXME
  # fa_info should be a list (1 entry per parameter) of full descriptors
  # fa_info[[1]]$name $func, where $name is itself a vector or list (?) ...
#  fa_info <- lapply(param_info, get_fa_param_info,
#                    fa_type     = format_opts$fa_type,
#                    fa_vector   = format_opts$fa_vector,
#                    rotate_wind = format_opts$rotate_wind)

  # a temporary hack. not very pretty.
  # we need to add fcdate & validdate  in unixdate, leadtime in hours
  # and while we're at it, we also add "members" (which is NULL or a single integer)
  info_list <- list(fcdate    = as.numeric(attr(fafile, "time")$basedate),
                validdate = as.numeric(attr(fafile, "time")$validdate),
                leadtime  = attr(fafile, "time")$leadtime,
                member = members)

#  prm_list <- lapply(1:length(param_info),
#                     function(i) c(blist, param_info[[i]], fa_info[[i]]))
# NOTE:  fa_info is preferably a data.frame: then it is easy to add constant columns
#        and you just apply the decoding per row.
  # create a list (data.frame) with 1 entry (row) per parameter/level
  fa_all_fields <- do.call(c, lapply(1:length(parameter), function(i) filter_fa_info(
                                parameter   = parameter[[i]],
                                fa_info     = param_info[[i]],
                                fafile      = fafile,
                                date_times  = date_times,
                                lead_time   = lead_time,
                                members     = members,
                                is_forecast = is_forecast,
                                opts        = opts
                                )
  ))
  #parameter, fa_info, fafile, date_times,
  #lead_time, members, is_forecast=TRUE, opts=NULL

  # are there any fields that can be decoded?
  missing_fields <- vapply(fa_all_fields,
                           function(x) length(x) == 0,
                           FUN.VALUE = TRUE)
  if (any(missing_fields)) {
    pm <- parameter[missing_fields]
    warning("No data available for fields ", names(pm))
    fa_all_fields <- fa_all_fields[!missing_fields]
  }
  if (length(fa_all_fields) == 0) {
    stop("None of the requested data could be read from FA file\n", attr(fafile, "filename"))
  }

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
    fa_info,
    fafile,
#    prm_list,
    format_opts,
    transformation = "none",
    opts = list(),
    info_list = list(), # this should contain fcdate etc.
    show_progress = FALSE
  ) {
    # fa_info is a list containing parameter and level info
    # it is 1 element of the param_info list above.
    pnam <- fa_info$name
    # FIXME: is this a completely safe way of checking for version-dependent components?
    if (is.list(pnam) && ("arome" %in% names(pnam)) ) {
      pnam <- pnam[[format_opts$fa_type]]
    }
    # if it's still a list: several components required to form final field
    # NOTE: a single field may also require a function...
    if (is.list(pnam)&& length(pnam) > 1) {
      if (is.null(fa_info$func)) {
        stop("Combined fields require a function.", call. = FALSE)
      }
    }

    if (!is.null(fa_info$apply_function)) {
      gdat <- fa_info[[row_num]]$apply_function(gdat)
    }
    # fa_grib may have several rows: component fields that need to be combined
    # for fa we can simplify a bit, because a single descriptor (name) is enough.
    result <- tibble::tibble(
      fcdate       = info_list$fcdate,
      validdate    = info_list$validdate,
      lead_time    = info_list$leadtime,
      parameter    = fa_info$parameter,
      members      = info_list$members,
      level_type   = fa_info$level_type,
      level        = fa_info$fa_level,
      units        = fa_info$units,
    # NOTE: using the index rather than name may be a bit faster...
      gridded_data = list(lapply(
               fa_info$name,
               function(x) do.call(Rfa::FAdec, c(list(fafile, x), format_opts))))
    )

    # Apply any function to the input(s) as taken from param_defs

    func <- fa_info[["func"]]
    if (is.list(func)) {
      func <- func[[1]]
    }
    # we will call "func" with the elements of gridded_data as argument
    # note that in some cases, the function may require named argumens
    # or arguments in a specific order: WS <- (U,V) doesn't matter, but Tdew <- (T, RH) does!
    # So either define a "func_var" entry (but this isn't done anywhere that I know)
    # but usually, in harp_params, the names are correctly assigned!
    if (!is.function(func)) {
      result[["gridded_data"]][[1]] <- result[["gridded_data"]][[1]][[1]]
    } else {
      result[["gridded_data"]][[1]] <- do.call(
        func, result[["gridded_data"]][[1]]
      )

      if (!meteogrid::is.geofield(result[["gridded_data"]][[1]])) {
        stop(
          "`func` must return a single geofield", call. = FALSE
        )
      }

    }

    result <- transform_geofield(result, transformation, opts)
    result
  }

  if (show_progress) {
    show_progress <- list(
      name = cli::col_yellow("Reading fa file"),
      show_after = 1
    )
  }

  fa_data <- purrr::map(
    fa_all_fields,
    read_and_transform_fa,
    fafile,
    format_opts,
    transformation,
    transformation_opts,
    info_list,
    .progress = show_progress
  ) %>%
    purrr::list_rbind()

  attr(fa_data, "transformation_opts") <- transformation_opts

  fa_data

}


filter_fa_info <- function(
  parameter, fa_info, fafile, date_times,
  lead_time, members, is_forecast=TRUE, opts=NULL
) {
# for every parameter, create the definitive list of field names (or positions)
# in this call, parameter is a single harp_parameter object!
  #result <- param_info
  # fa_info is a list containing parameter and level info
  # it is 1 element of the param_info list above.
  pnam <- fa_info$name
  # TODO: is this a completely safe way of checking for version-dependent components?
  if (is.list(pnam) && ("arome" %in% names(pnam)) ) {
    pnam <- pnam[[format_opts$fa_type]]
  }

  # if it's still a list: several components required to form final field
  # NOTE: a single field may also require a function...
  if (is.list(pnam)) {
    if (is.null(fa_info$func)) {
      stop("Combined fields require a function.", call. = FALSE)
    }
#    namlist <- lapply(fa_info$name,
#                      function(x) sprintf(fa_info$fa_template, fa_info$level, x))

  }
  # check that date & time from the file are correct!
  fcdate <- as.numeric(attr(fafile, "time")$basedate)
  ldt <- attr(fafile, "time")$leadtime

  # TODO: Is it possible that date_times is a vector rather than a single element?
  #       FA can not handle it, but read_grid may pass it that way? It would be an error,
  #       of course.
  if (!is.null(date_times)) {
    if (! fcdate %in% date_times) {
      warning("File ", attr(fafile, "filename"),
  	    "does not contain data for requested dates:\n",
  	    paste(date_times, collapse=","))
      return(list())
    } else if (length(date_times) > 1) {
      warning("File ", attr(fafile, "filename"), " does not contain data for some dates:\n",
  	    paste(date_times[date_times != fcdate], collapse=","))
      date_times <- fcdate
    }
  }

  if (!is.null(lead_time)) {
    if (!ldt %in% lead_time) {
      warning("File ", attr(fafile, "filename"),
              "does not contain data for requested lead times:\n",
      paste(lead_time, collapse=","))
      return(list())
    } else if (length(lead_time) > 1) {
      warning("File ", attr(fafile, "filename"), " does not contain data for some lead times:\n",
  	    paste(lead_time[lead_time != ldt], collapse=","))
      lead_time <- ldt
    }
  }

  # "-999" means "all available levels"
  # for some types that does not mean much
  if (!is.null(fa_info$level) && fa_info$level == -999) {
    if (is.list(fa_info$name)) ft <- fa_info$name[[1]]
    else  ft <- fa_info$name
    # search for all available levels in the file
    if (fa_info$level_type == "model") {
      #rege <- sprintf("^S[[:digit:]]{3}%-12.12s", ft)
      #flist <- grep(rege, fafile$list$name, value=TRUE)
      #levlist <- sort(as.integer(substr(flist,2,4)))
      # the number of model levels is encoded in the file meta-data
      levlist <- seq(1, attr(fafile, "frame")$nlev)
    } else if (fa_info$level_type == "pressure") {
      # here you should already know the component field names
      # NOTE: name may be a list, but may also be a string...
      rege <- sprintf("^P[[:digit:]]{5}%-10.10s", ft)
      flist <- grep(rege, fafile$list$name, value=TRUE)
      levlist <- sort(as.integer(substr(flist,2,6)))
      # pressure 100000 Pa is encoded as "00000" !!!
      # so 1025 hPa -> "02500" ?
      # Where does this stop? "05000" = 50 hPa, I guess.
      levlist[levlist <= 2500] <- levlist[levlist <= 2500] + 100000
      levlist <- sort(levlist)
    } else if (fa_info$level_type == "height") {
      rege <- sprintf("^H[[:digit:]]{5}%-10.10s", ft)
      flist <- grep(rege, fafile$list$name, value=TRUE)
      levlist <- sort(as.integer(substr(flist,2,6)))
    } else if (fa_info$level_type == "isotherm") {
      rege <- sprintf("^KB[[:digit:]]{3}%-11.11s", ft)
      flist <- grep(rege, fafile$list$name, value=TRUE)
      levlist <- sort(as.integer(substr(flist,3,5)))
    } else if (fa_info$level_type %in% c("surface", "unknown")) {
      # for these cases, "-999" just means "undefined"
      levlist <- "-999"
    } else {
      stop("Don't know how to expand level_type ", fa_info$level_type, call. = FALSE)
    }
  } else {
    # just a single level
    # TODO: make sure it's not NULL or NA?
    levlist <- fa_info$level
  }
  result <- lapply(levlist,
                   function(lev) list(
                      name = lapply(pnam, function(nn) sprintf(fa_info$fa_template, lev, nn)),
                      #fa_info = fa_info,
                      level   = lev,
                      level_type = fa_info$level_type,
                      units = fa_info$units,
                      func    = fa_info[["func"]]
                      )
  )
  # %>%  lapply(function(x) x$level <- x$fa_lev)
  # TODO: is there a simple way to change $level in every entry without an explicit loop? Does it matter?
  names(result) <- paste(parameter$fullname, levlist)
  # NOW CHECK WHETHER ALL FIELDS EXIST
  fields_missing <- vapply(result,
                           function(x) any(is.na(match(x$name, fafile$list$name))),
                           FUN.VALUE=TRUE)
  if (any(fields_missing)) {
    warning("Can not read ", parameter$fullname, " for levels ",
            paste(levlist[fields_missing], collapse = ","))
    result <- result[!fields_missing]
  }
  if (length(result) == 0) {
    warning("Field ", parameter$fullname, " not available.")
  }

  result
}

get_domain_fa <- function(file_name, opts) {
  Rfa::FAdomain(Rfa::FAframe(Rfa::FAread_meta(filename=file_name)))
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
  # Make sure tc is in Celsius (not Kelvin) and rh in fraction (not precent)
  if (max(rh) > 5) rh <- rh/100
  if (max(tc) > 200) tc <- tc - 273.15

  a <- tuning[1]
  b <- tuning[2]
  minrh <- 1.E-3
  rh[rh < minrh] <- minrh
  rh[rh > 1] <- 1
  gg <- log(rh) + a*tc/(tc+b)
  result <- b * gg/(a-gg)
  if (meteogrid::is.geofield(result)) {
    # the geofield information is taken from "tc"
    # we want to replace the name
    # FIXME: level information ("S025") is no longer in the name
    # but at least we explicitly copy level etc. info from "tc"
    attr(result, "info") <- attr(tc, "info")
    attr(result, "info")$name <- "Dewpoint temperature"
    attr(result, "info")$variable <- "Dewpoint temperature"
  }
  result
}

