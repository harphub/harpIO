# Function to create a geodomain from a netcdf file
# Probably to just be used internally
get_domain_netcdf <- function(file_name, opts) {

  if (is.null(opts)) {
    message("Using default netcdf_opts()")
    opts <- netcdf_opts()
  }

  proj4_var <- opts[["proj4_var"]]
  proj4_att <- opts[["proj4_att"]]
  proj4     <- opts[["proj4"]]
  x_dim     <- opts[["x_dim"]]
  y_dim     <- opts[["y_dim"]]
  lon_var   <- opts[["lon_var"]]
  lat_var   <- opts[["lat_var"]]
  x_rev     <- opts[["x_rev"]]
  y_rev     <- opts[["y_rev"]]

  if (inherits(file_name, "ncdf4")) {
    nc_id      <- file_name
    was_closed <- FALSE
  } else {
    nc_id      <- ncdf4::nc_open(file_name)
    was_closed <- TRUE
  }

  if (is.null(proj4)) {
    if (proj4_var != 0 && !is.element(proj4_var, names(nc_id[["var"]]))) {
      stop(
        "Projection variable '", proj4_var, "' not found in netcdf file.\n",
        "Use netcdf_opts() to set.",
        call. = FALSE
      )
    }
    if (is.null(proj4_att)) {
      proj4 <- ncdf4::ncvar_get(proj4_var)
      if (!grepl("+proj", proj4)) {
        stop(
          "Projection variable '", proj4_var, "' does not appear to be a proj string:\n",
          proj4,
          call. = FALSE
        )
      }
    }
    proj4 <- ncdf4::ncatt_get(nc_id, proj4_var, proj4_att)
    if (!proj4[["hasatt"]]) {
      stop(
        "Projection variable '", proj4_var, "' does not have the attribute '", proj4_att, "'.\n",
        "Use netcdf_opts() to set.",
        call. = FALSE
      )
    }
    proj4 <- proj4[["value"]]
  }

  if (proj4 == "wrf") {
    proj4 <- get_wrf_projection(nc_id)
    nx    <- nc_id[["dim"]][[x_dim]][["len"]]
    ny    <- nc_id[["dim"]][[y_dim]][["len"]]
    dx    <- ncdf4::ncatt_get(nc_id, 0, "DX")
    if (!dx[["hasatt"]]) {
      stop("Cannot retrieve DX from WRF file.", call. = FALSE)
    }
    dx    <- dx[["value"]]
    dy    <- ncdf4::ncatt_get(nc_id, 0, "DY")
    if (!dy[["hasatt"]]) {
      stop("Cannot retrieve DY from WRF file.", call. = FALSE)
    }
    dy    <- dy[["value"]]
    if (is.null(lon_var)) {
      stop("'lon_var' must be passed for WRF files.", call. = FALSE)
    }
    if (is.null(lat_var)) {
      stop("'lat_var' must be passed for WRF files.", call. = FALSE)
    }
  } else {
    if (nc_id[["dim"]][[x_dim]][["create_dimvar"]]) {
      x  <- ncdf4::ncvar_get(nc_id, x_dim)
      nx <- length(x)
      dx <- diff(x[1:2])
    } else {
      if (is.null(opts[["dx"]])) {
        stop(
          "`", x_dim, "` is only a dimension and not a variable.\n",
          "You need to include `dx` in netcdf_opts.", call. = FALSE
        )
      }
      nx <- nc_id[["dim"]][[x_dim]][["len"]]
      dx <- opts[["dx"]]
      x  <- seq(0, by = dx, length.out = nx)
    }
    if (nc_id[["dim"]][[y_dim]][["create_dimvar"]]) {
      y  <- ncdf4::ncvar_get(nc_id, y_dim)
      ny <- length(y)
      dy <- abs(diff(y[1:2]))
    } else {
      if (is.null(opts[["dy"]])) {
        stop(
          "`", y_dim, "` is only a dimension and not a variable.\n",
          "You need to include `dy` in netcdf_opts.", call. = FALSE
        )
      }
      ny <- nc_id[["dim"]][[y_dim]][["len"]]
      dy <- opts[["dy"]]
      y  <- seq(0, by = dy, length.out = ny)
    }
  }

  proj4 <- meteogrid::proj4.str2list(proj4)
  proj4 <- proj4[sapply(proj4, function(x) !is.na(x))]

  x_start <- 1
  y_start <- 1
  x_end   <- nx
  y_end   <- ny

  if (x_rev) {
    x_start <- nx
    e_end   <- 1
  }
  if (y_rev) {
    y_start <- ny
    y_end   <- 1
  }

  # If lat and lon variables are not defined try and get the corners from x and y
  if (is.null(lon_var) && is.null(lat_var)) {

    projected_x <- c(
      ncdf4::ncvar_get(nc_id, x_dim, start = x_start, count = 1),
      ncdf4::ncvar_get(nc_id, x_dim, start = x_end, count = 1)
    )
    projected_y <- c(
      ncdf4::ncvar_get(nc_id, y_dim, start = y_start, count = 1),
      ncdf4::ncvar_get(nc_id, y_dim, start = y_end, count = 1)
    )
    if (proj4[["proj"]] %in% c("latlong", "merc")) {
      SW <- c(projected_x[1], projected_y[1])
      NE <- c(projected_x[2], projected_y[2])
    } else {
      latlon_xy <- meteogrid::project(projected_x, projected_y, proj = proj4, inv = TRUE)
      SW <- as.numeric(latlon_xy[1,])
      NE <- as.numeric(latlon_xy[2,])
    }

  } else {

    if (lon_var == x_dim && lat_var == y_dim) {
      SW <- c(
        ncdf4::ncvar_get(nc_id, lon_var, start = x_start, count = 1),
        ncdf4::ncvar_get(nc_id, lat_var, start = y_start, count = 1)
      )
      NE <- c(
        ncdf4::ncvar_get(nc_id, lon_var, start = x_end, count = 1),
        ncdf4::ncvar_get(nc_id, lat_var, start = y_end, count = 1)
      )
    } else {
      # Lat and lon variables have a time dimension for wrf to support moving domains
      # Assume domains are stationary and take the first time
      sw_start    <- c(x_start, y_start)
      ne_start    <- c(x_end, y_end)
      sw_ne_count <- c(1, 1)
      if (nc_id[["var"]][[lon_var]][["ndims"]] == 3) {
        sw_start <- c(sw_start, 1)
        ne_start <- c(ne_start, 1)
        sw_ne_count <- c(sw_ne_count, 1)
      }
      SW <- c(
        ncdf4::ncvar_get(nc_id, lon_var, start = sw_start, count = sw_ne_count),
        ncdf4::ncvar_get(nc_id, lat_var, start = sw_start, count = sw_ne_count)
      )
      NE <- c(
        ncdf4::ncvar_get(nc_id, lon_var, start = ne_start, count = sw_ne_count),
        ncdf4::ncvar_get(nc_id, lat_var, start = ne_start, count = sw_ne_count)
      )
    }
  }

  if (was_closed) ncdf4::nc_close(nc_id)

  structure(
    list(
      projection = proj4,
      nx         = nx,
      ny         = ny,
      SW         = SW,
      NE         = NE,
      dx         = dx,
      dy         = dy
    ),
    class = "geodomain"
  )

}
