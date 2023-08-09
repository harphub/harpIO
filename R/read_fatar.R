#' @param ldt_template a character string that describes (part of) the file name used for finding a lead time within a tar archive.
#' @rdname fa_opts
#' @export
fatar_opts <- function(ldt_template="+%04i$", ...) {
  fa_opts(ldt_template=ldt_template, ...)
}
#fatar_opts <- function(meta=TRUE, fa_type="arome", fa_vector=TRUE, rotate_wind=TRUE, ldt_template="+%04i$", ...) {
#  list(meta=meta, fa_type=fa_type, fa_vector=fa_vector, rotate_wind=rotate_wind, ldt_template=ldt_template, ...)
#}

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
                       format_opts=fatar_opts(),
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
    warning("For fatar files, lead time should be passed. Using 0.")
    lead_time <- 0
  }

  # make sure the format options are complete
  format_opts <- do.call(fatar_opts, format_opts)

  ## TODO: fatar should be able to decumulate precipitation
  if (is.list(file_name)) {
    # we suppose that it is the listing of a tar file
    filelist <- file_name
  } else {
    # NOTE: we only parse the tar file, we don't use FAopenTar
    #       that could be "FAopen'ing" too many lead_times, so would be inefficient
    filelist <- Rfa::ParseTar(file_name)
  }

  # call read_fa for individual lead times and combine in a tibble
  ffun <- function(ldt, filelist) {
    for (tt in format_opts$ldt_template) {
    # find the lead time as a file within the tar archive
      fcfile <- grep(sprintf(tt, ldt), names(filelist), value = TRUE)
      if (length(fcfile) > 0) break
    }
    if (length(fcfile) != 1) {
      stop("Lead time ", ldt, " not available in archive file\n",
           file_name, "\n", length(fcfile), " hits")
    }
    fafile <- Rfa::FAopen(filelist[[fcfile]])
    # pass format_opts to read_fa (ldt_template will just be ignored)
    harpIO:::read_fa(fafile,
                     parameter=parameter,
                     lead_time=ldt,
                     format_opts=format_opts,
                     ...)
  }

  result <- purrr::map_dfr(lead_time, ffun, filelist)
  result
}

get_domain_fatar <- function(file_name, opts) {
  # This is not perfect, as ParseTar will go through the whole file rather than just taking the first FA file in the archive.
  # But currently Rfa doesn't really offer better ways. Unless you use FindInTar(archive, 1) as byte position
  # FAread_meta( filename=1, archname=file_name)
  Rfa::FAdomain(Rfa::FAframe(Rfa::FAread_meta(Rfa::ParseTar(file_name)[[1]])))
}

