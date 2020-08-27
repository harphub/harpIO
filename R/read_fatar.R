# Read a field from an FA file in a tar archive
#
# @param file_name A tar archive of FA files.
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

read_fatar <- function(file_name,
                       parameter,
                       lead_time=NULL,
                       ...) {

  if (!requireNamespace("Rfa", quietly=TRUE)) {
    stop("The Rfa package must be installed to read FA files.")
  }

  if (is.null(parameter)) {
    stop("For fa files, parameter = '<parameter>' must be passed.", call. = FALSE)
  }

  if (length(lead_time) < 1) {
    # either an error OR just default to taking all available lead times
    # FIXME: we could also extract /all/ lead times in this case, or the first file...
    stop("For fatar files, lead time must be passed.")
  }

  ## TODO: fatar should be able to decumulate precipitation
  if (is.list(file_name)) {
    # we suppose that it is the listing of a tar file
    filelist <- file_name
  } else {
    # NOTE: we only parse the tar file, we don't use FAopenTar
    #       that could be "FAopen'ing" too many lead_times, so would be inefficient
    filelist <- Rfa::ParseTar(file_name)
  }

  # now we call read_fa for individual lead times and combine in a tibble
  ffun <- function(ldt, filelist) {
    fcfile <- grep(sprintf("+%04i$", ldt), filelist, value = TRUE)
    if (length(fcfile) != 1) {
      stop("Lead time ", ldt, " not available in archive file\n",
           file_name, "\n", length(fcfile), " hits")
    }
    fafile <- Rfa::FAopen(filelist[[fcfile]])
    harpIO:::read_fa(fafile,
                                parameter=parameter,
                                lead_time=ldt,
                                ...)
  }

  result <- purrr::map_dfr(lead_time, ffun, names(filelist))
#  result <- NULL
#  for (ldt in seq_along(lead_time)) {
#    fcfile <- grep(sprintf("+%04i$", lead_time[ldt]), names(filelist), value = TRUE)
#    if (length(fcfile) != 1) {
#      stop("Lead time ", lead_time[ldt], " not available in archive file\n",
#           file_name, "\n", length(fcfile), " hits")
#    }
#    fafile <- Rfa::FAopen(filelist[[fcfile]])
#    presult <- harpIO:::read_fa(fafile,
#                                parameter=parameter,
#                                lead_time=lead_time[ldt],
#                                ...)
#    if (is.null(result)) result <- presult
#    else result <- rbind(result, presult)
#  }

  result
}


