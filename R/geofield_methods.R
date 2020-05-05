#' Convert a Geofield to a Raster
#'
#' If you wish to work with the raster package, you can convert geofield objects
#' to raster using this function.
#'
#' @param x A geofield for \code{geofield_to_raster}, or a raster for
#'   \code{raster_to_geofield}.
#'
#' @return A raster for \code{geofield_to_raster}, or a geofield for
#'   \code{raster_to_geofield}.
#' @export
#'
#' @examples
#' if (
#'   requireNamespace("Rgrib2", quietly = TRUE) &
#'   requireNamespace("harpData", quietly = TRUE)
#' ) {
#'   geo <- read_grid(
#'     system.file(
#'       "grib/HARMUK20171015T12Z+003.grib",
#'       package = "harpData"
#'     ),
#'     "T2m"
#'   )
#'
#'   meteogrid::iview(geo, legend = TRUE)
#'   ras <- geofield_to_raster(geo)
#'   if (require("raster")) {
#'     plot(ras)
#'   }
#'
#'   geo1 <- raster_to_geofield(ras)
#'   meteogrid::iview(geo1, legend = TRUE)
#' }
#'
geofield_to_raster <- function(x) {

  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("Please install the raster package to convert a geofield to a raster.", call. = FALSE)
  }

  stopifnot(inherits(x, "geofield"))

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
  raster::crs(raster_object) <- proj4_string

  raster_object
}

#' @rdname geofield_to_raster
#' @export
raster_to_geofield <- function(x) {

  if (!requireNamespace("raster", quitely = TRUE)) {
    stop("Please install the raster package to convert a geofield to a raster.", call. = FALSE)
  }

  stopifnot(inherits(x, "raster") | inherits(x, "RasterLayer"))

  proj4_string <- as.character(raster::crs(x))
  proj4_list   <- meteogrid::proj4.str2list(proj4_string)
  proj4_list   <- proj4_list[sapply(proj4_list, function(x) !is.na(x))]

  raster_extent <- raster::extent(x)

  dx           <- raster::res(x)[1]
  dy           <- raster::res(x)[2]
  nx           <- dim(x)[2]
  ny           <- dim(x)[1]
  x0           <- raster_extent@xmin + dx / 2
  x1           <- raster_extent@xmax - dx / 2
  y0           <- raster_extent@ymin + dy / 2
  y1           <- raster_extent@ymax - dy / 2
  latlon_xy    <- meteogrid::project(c(x0, x1), c(y0, y1), proj = proj4_list, inv = TRUE)
  SW           <- as.numeric(latlon_xy[1,])
  NE           <- as.numeric(latlon_xy[2,])


  geo_domain <- structure(
    list(
      projection = proj4_list,
      nx         = nx,
      ny         = ny,
      SW         = SW,
      NE         = NE,
      dx         = dx,
      dy         = dy
    ),
    class = "geodomain"
  )

  x <- t(raster::as.matrix(x))
  meteogrid::as.geofield(x[, ncol(x):1], domain = geo_domain)

}
