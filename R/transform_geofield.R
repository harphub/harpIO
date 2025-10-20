# Internal functions to interpolate a geofield to stations
# It is assumed that opts is a list and includes the named
# elements "stations" and "weights", and that geofield is
# either a geofield or a list of geofields.


transform_geofield <- function(data, transformation, opts) {

  col_name <- switch(
    transformation,
    "none"        = "gridded_data",
    "interpolate" = "station_data",
    "regrid"      = "regridded_data",
    "xsection"    = "xsection_data",
    "subgrid"     = "subgrid_data"
  )

  if (transformation == "none") {
    return(data)
  }

  if (transformation == "interpolate") {
    fun <- function(x, opts) {
      stopifnot(meteogrid::is.geofield(x))
      res <- cbind(
        opts[["stations"]][c("SID", "lat", "lon")],
        station_data = meteogrid::point.interp(x, weights = opts[["weights"]])
      )
    }
  }

  if (transformation == "subgrid") {
    fun <- function(x, opts) {
      stopifnot(meteogrid::is.geofield(x))
      geofield_info <- attr(x, "info")
      x <- meteogrid::subgrid(x, opts[["x1"]], opts[["x2"]], opts[["y1"]], opts[["y2"]])
      attr(x, "info") <- geofield_info
      x
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
      xs_endpoints <- data.frame(do.call(rbind, opts[c("a", "b")]))
      xs_endpoints[["end"]] <- row.names(xs_endpoints)
      colnames(xs_endpoints) <- c("lon", "lat", "end")
      xs_endpoints <- harpCore::geo_reproject(xs_endpoints, x)
      xs_endpoints <- xs_endpoints[c("end", "lon", "lat", "x", "y")]
      dom <- harpCore::get_domain(x)
      x <- meteogrid::point.interp(x, weights = opts[["weights"]])
      x <- tibble::tibble(
        distance = seq(
          0, by = opts[["horizontal_res"]], length.out = length(x)
        ),
        value = x
      )
      attr(x, "dx")         <- opts[["horizontal_res"]]
      attr(x, "domain")     <- dom
      attr(x, "end_points") <- xs_endpoints
      x
    }
  }

  if (is.list(data)) {

    if (is.data.frame(data)) {

      if (!is.element("gridded_data", colnames(data))) {
        stop("data frame must have a 'gridded_data' column to transform-")
      }

      data[[col_name]] <- lapply(data[["gridded_data"]], fun, opts)

      if (is.null(opts[["keep_raw_data"]]) || !opts[["keep_raw_data"]]) {
        data <- data[, which(colnames(data) != "gridded_data")]
        if (transformation == "interpolate") {
          data <- tidyr::unnest(data, .data[["station_data"]])
        }
      }

      data

    } else {

      lapply(data, fun, opts)

    }

  } else {

    fun(data, opts)

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
