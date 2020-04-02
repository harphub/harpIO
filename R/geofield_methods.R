#' Convert a Geofield to a Raster
#'
#' If you wish to work with the raster package, you can convert geofield objects
#' to raster using this function.
#'
#' @param x A geofield
#'
#' @return A raster
#' @export
#'
#' @examples
#' if (requireNamespace("Rgrib2", quietly = TRUE) & requireNamespace("harpData", quietly = TRUE)) {
#'   geo <- read_grid(system.file("grib/HARMUK20171015T12Z+003.grib", package = "harpData"), "T2m")
#'   meteogrid::iview(geo, legend = TRUE)
#'   ras <- geofield_to_raster(geo)
#'   if (require("raster")) {
#'     plot(ras)
#'   }
#' }
#'
geofield_to_raster <- function(x) {

  if (!requireNamespace("raster", quitely = TRUE)) {
    stop("Please install the raster package to convert a geofield to a raster.", call. = FALSE)
  }

  proj4_string  <- meteogrid::proj4.list2str(attr(x, "domain")[["projection"]], join = TRUE)
  domain_extent <- meteogrid::DomainExtent(x)

  raster_extent <- raster::extent(c(
    domain_extent[["x0"]] - domain_extent[["dx"]] / 2,
    domain_extent[["x1"]] + domain_extent[["dx"]] / 2,
    domain_extent[["y0"]] - domain_extent[["dy"]] / 2,
    domain_extent[["y1"]] + domain_extent[["dy"]] / 2
  ))

  raster_object <- raster::raster(t(unclass(x)[, ncol(x):1]))
  raster_object <- raster::setExtent(raster_object, raster_extent)
  crs(raster_object) <- proj4_string

  raster_object
}
