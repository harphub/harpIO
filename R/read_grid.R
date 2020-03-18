#' Read gridded data in various formats.
#'
#' @param filename A character string with the (full) file name.
#' @param parameter The parameter to be read.
#' @param file_format The file format. Possible values include grib, netcdf, FA,
#'   hdf5... Whatever the value is, it is suposed to correspond to a function
#'   "read_XXX" that can deal with the format. If not specified, the format
#'   can often be guessed correctly from file extension or the first few bytes.
#' @param ... All arguments passed to the specified reader function.
#' @return A geofield or (possibly) a plain  matrix.
#' @export

#' @examples
#' if (requireNamespace("Rgrib2", quietly = TRUE) & requireNamespace("harpData", quietly = TRUE)) {
#'   read_grid(
#'     system.file("grib/HARMUK20171015T12Z+003.grib", package = "harpData"),
#'     parameter = "T2m"
#'   )
#'   read_grid(
#'     system.file("grib/HARMUK20171015T12Z+003.grib", package = "harpData"),
#'     parameter = "RH2m"
#'   )
#'   read_grid(
#'     system.file("grib/HARMUK20171015T12Z+003.grib", package = "harpData"),
#'     parameter = "tcc"
#'   )
#' }
read_grid <- function(filename, parameter, file_format = NULL, ...) {
  if (is.null(file_format)) file_format <- guess_format(filename)
  if (is.na(file_format)) stop("Please provide explicit file format for ", filename, call. = FALSE)
  reader <- get(paste0("read_", file_format))
  reader(filename = filename, parameter = parameter, ...)
}

#' @rdname read_grid
#' @export
read_grid_interpolate <- function(filename, parameter, file_format = NULL, ...) {
  if (is.null(file_format)) file_format <- guess_format(filename)
  if (is.na(file_format)) stop("Please provide explicit file format for ", filename, call. = FALSE)
  reader <- get(paste0("read_", file_format, "_interpolate"))
  reader(filename = filename, parameter = parameter, ...)
}

# Try to guess the binary format of a gridded data file.
# @param filename The filename
# @return A character string with the format, or NA.
guess_format <- function(filename) {
  if (!file.exists(filename)) stop("File not found.", call. = FALSE)

  # first we try the file extension:
  ## TODO: tar? csv, txt...?
  ## for now, we assume tar -> fatar (it's the only one we know, anyway)
  ext <- tools::file_ext(filename)
  ff <- switch(tolower(ext),
               "grib" = ,
               "grb"  = "grib",
               "nc"   = ,
               "nc4"  = ,
               "ncf"  = "netcdf",
               "h5"   = ,
               "hdf"  = ,
               "hdf5" = "hdf5",
               "tar"  = "fatar",
               NA_character_)
  ## try reading the first few bytes of the file
  if (is.na(ff)) {
    df <- file(filename, open="rb")
    ttt <- readBin(df, "raw", n=4 * 8)
    close(df)
    if (rawToChar(ttt[1:4]) == "GRIB") return("grib")
    if (rawToChar(ttt[1:4]) == "BUFR") return("bufr")
    if (rawToChar(ttt[2:4]) == "HDF") return("hdf5")
    ## FA files have a header of 22 8-byte integers
    ## 2 and 4 should always have the same value
    header <- readBin(ttt, "integer", size=8, n=22, endian="big")
    if (as.numeric(header[2])==16 && as.numeric(header[4])==22) return("fa")
  }
  return(ff)
}

