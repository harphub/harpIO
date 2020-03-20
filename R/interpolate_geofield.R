# Internal function to interpolate a geofield to stations
# It is assumed that opts is a list and includes the named
# elements "stations" and "weights", and that geofield is
# either a geofield or a list of geofields.


interpolate_geofield <- function(geofield, opts) {

  fun <- function(x, opts) {
    stopifnot(meteogrid::is.geofield(x))
    res <- cbind(
      opts[["stations"]][c("SID", "lat", "lon")],
      value = meteogrid::point.interp(x, weights = opts[["weights"]])
    )
  }

  if (is.list(geofield)) {
    lapply(geofield, fun, opts)
  } else {
    fun(geofield, opts)
  }

}
