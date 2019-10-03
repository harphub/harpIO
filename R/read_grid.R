#' Read gridded data in various formats.
#'
#' @param filename A character string with the (full) file name.
#' @param format The file format. Possible values include grib, netcdf, FA,
#'   hdf5... Whatever the value is, it is suposed to correspond to a function
#'   "read_XXX" that can deal with the format.
#' @param ... All arguments passed to the specified reader function.
#' @return A geofield or (possibly) a plain  matrix.
#' @export
read_grid <- function(filename, file_format=NULL, ...) {
  if (is.null(file_format)) file_format <- guess_format(filename)
  if (is.na(file_format)) stop("Please provide explicit file for ", filename)
  reader <- get(paste0("read_", file_format))
  reader(filename = filename, ...)
}

# Try to guess the binary format of a gridded data file.
# @param filename The filename
# @return A character string with the format, or NA.
guess_format <- function(filename) {
  ## TODO: tar? csv, txt...?
  ext <- tools::file_ext(filename)
  ff <- switch(tolower(ext),
               "grib" = ,
               "grb"  = "grib",
               "nc"   = ,
               "nc4"  = ,
               "ncf"  = "netcdf",
               "hdf"  = ,
               "hdf5" = "hdf5",
               NA_character_)
  if (is.na(ff)) {
    ## try reading the first few bytes of the file
    df <- file(filename, open="rb")
    ttt <- readBin(df, "raw", n=4 * 8)
    close(df)
    if (rawToChar(ttt[1:4]) == "GRIB") ff <- "grib"
    if (rawToChar(ttt[1:4]) == "BUFR") ff <- "bufr"
    if (rawToChar(ttt[2:4]) == "HDF") ff <- "hdf5"
    ## FA files have a header of 22 8-byte integers
    ## 2 and 4 should always have the same value
    header <- readBin(ttt, "integer", size=8, n=22, endian="big")
    if (as.numeric(header[2])==16 && as.numeric(header[4])==22) ff <- "fa"
  }
  return(ff)
}




