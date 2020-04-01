# Internal functions to interpolate a geofield to stations
# It is assumed that opts is a list and includes the named
# elements "stations" and "weights", and that geofield is
# either a geofield or a list of geofields.


transform_geofield <- function(geofield, transformation, opts) {

  if (transformation == "interpolate") {
    fun <- function(x, opts) {
      stopifnot(meteogrid::is.geofield(x))
      res <- cbind(
        opts[["stations"]][c("SID", "lat", "lon")],
        value = meteogrid::point.interp(x, weights = opts[["weights"]])
      )
    }
  }

  if (transformation == "regrid") {
    fun <- function(x, opts) {
      stopifnot(meteogrid::is.geofield(x))
      geofield_info <- attr(x, "info")
      x <- meteogrid::regrid(x, weights = opts[["weights"]])
      attr(x, "info") <- geofield_info
      x
    }
  }

  if (transformation == "xsection") {
    fun <- function(x, opts) {
      stopifnot(meteogrid::is.geofield(x))
      x <- meteogrid::point.interp(x, weights = opts[["weights"]])
      attr(x, "point_a") <- opts[["a"]]
      attr(x, "point_b") <- opts[["b"]]
      attr(x, "dx")      <- opts[["horizontal_res"]]
      x
    }
  }

  if (is.list(geofield)) {
    lapply(geofield, fun, opts)
  } else {
    fun(geofield, opts)
  }

}

xsection_init <- function(x, opts) {

  stopifnot(meteogrid::is.geodomain(x))
  geofield_proj <- x[["projection"]]
  end_points    <- meteogrid::project(rbind(opts[["a"]], opts[["b"]]), proj = geofield_proj)
  y_length      <- end_points$y[2] - end_points$y[1]
  x_length      <- end_points$x[2] - end_points$x[1]
  xs_length     <- sqrt(x_length ^ 2 + y_length ^ 2)
  xs_angle      <- atan2(y_length, x_length)
  # Make xs_length a multiple of the resolution
  num_xs_points <- ceiling(xs_length / opts[["horizontal_res"]])
  extra_length  <- (opts[["horizontal_res"]] * num_xs_points - xs_length) / 2
  xs_start_x    <- end_points$x[1] - cos(xs_angle) * extra_length
  xs_start_y    <- end_points$y[1] - sin(xs_angle) * extra_length

  xs_points     <- lapply(
    seq(0, (num_xs_points - 1)),
    function(x) data.frame(
      x = xs_start_x + x * opts[["horizontal_res"]] * cos(xs_angle),
      y = xs_start_y + x * opts[["horizontal_res"]] * sin(xs_angle)
    )
  )

  xs_points <- Reduce(rbind, xs_points)
  xs_points <- meteogrid::project(xs_points, proj = geofield_proj, inv = TRUE)
  if (is.null(opts[["method"]])) {
    warning(
      "'method' for interpolation to xsection points not passed. Using 'bilinear'",
      call. = FALSE, immediate. = TRUE
    )
    opts[["method"]] <- "bilin"
  }

  meteogrid::point.interp.init(
    domain = x, lon = xs_points[["x"]], lat = xs_points[["y"]], method = opts[["method"]]
  )

}
