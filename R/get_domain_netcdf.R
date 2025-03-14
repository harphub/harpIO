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
    # if (is.null(proj4_att)) {
    #   proj4 <- ncdf4::ncvar_get(proj4_var)
    #   if (!grepl("+proj", proj4)) {
    #     stop(
    #       "Projection variable '", proj4_var, "' does not appear to be a proj string:\n",
    #       proj4,
    #       call. = FALSE
    #     )
    #   }
    # }
    # proj4 <- ncdf4::ncatt_get(nc_id, proj4_var, proj4_att)
    # if (!proj4[["hasatt"]]) {
    #   stop(
    #     "Projection variable '", proj4_var, "' does not have the attribute '", proj4_att, "'.\n",
    #     "Use netcdf_opts() to set.",
    #     call. = FALSE
    #   )
    # }
    # proj4 <- proj4[["value"]]
    proj4 <- get_crs_from_attrs(ncdf4::ncatt_get(nc_id, proj4_var))
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
    clon  <- ncdf4::ncatt_get(nc_id, 0, "CEN_LON")[["value"]]
    clat  <- ncdf4::ncatt_get(nc_id, 0, "CEN_LAT")[["value"]]
    if (was_closed) ncdf4::nc_close(nc_id)
    return(
      harpCore::define_domain(
        centre_lon = clon,
        centre_lat = clat,
        nxny       = c(nx, ny),
        dxdy       = c(dx, dy),
        proj       = proj4
      )
    )

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

  # If variables are given for latitude and longitude, check if they exist.
  # If not, set to NULL.

  if (!is.null(lon_var)) {
    if (is.null(nc_id[["var"]][[lon_var]])) {
      lon_var <- NULL
    }
  }

  if (!is.null(lat_var)) {
    if (is.null(nc_id[["var"]][[lat_var]])) {
      lat_var <- NULL
    }
  }

  # If lat and lon variables are not defined try and get the corners from x and y
  if (is.null(lon_var) || is.null(lat_var)) {

    projected_x <- c(
      ncdf4::ncvar_get(nc_id, x_dim, start = x_start, count = 1),
      ncdf4::ncvar_get(nc_id, x_dim, start = x_end, count = 1)
    )
    projected_y <- c(
      ncdf4::ncvar_get(nc_id, y_dim, start = y_start, count = 1),
      ncdf4::ncvar_get(nc_id, y_dim, start = y_end, count = 1)
    )
    if (proj4[["proj"]] %in% c("latlong", "longlat", "lalo")) {
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

# Function is a copy of the .getCRSfromGridMap4 function from the terra package.
# The only difference is that no test is done to check if the CRS is valid
get_crs_from_attrs <- function(g) {

  if (!is.null(g$epsg_code)) {
    crs <- g$epsg_code
    if (!grep("EPSG:", crs, ignore.case = TRUE)) {
      crs <- paste0("epsg:", crs)
    }
    return(crs)
  }

  sp <- g$standard_parallel
  if (length(sp) > 1) {
    g$standard_parallel1 <- sp[1]
    g$standard_parallel2 <- sp[2]
    g$standard_parallel <- NULL
  }

  vals <- sapply(g, function(i) i[1])
  vars <- names(vals)
  if (any(vars %in% c("proj4"))) { #, "crs_wkt", "spatial_ref"))) {
    crs <- vals[vars %in% c("proj4", "crs_wkt", "spatial_ref")][1]
    return(crs)
  }
  # based on info at
  # http://trac.osgeo.org/gdal/wiki/NetCDF_ProjectionTestingStatus
  # accessed 7 October 2012
  prj <- matrix(
    c(
      "albers_conical_equal_area",
      "aea",
      "azimuthal_equidistant",
      "aeqd",
      "lambert_cylindrical_equal_area",
      "cea",
      "lambert_azimuthal_equal_area",
      "laea",
      "lambert_conformal_conic",
      "lcc",
      "latitude_longitude",
      "longlat",
      "mercator",
      "merc",
      "orthographic",
      "ortho",
      "polar_stereographic",
      "stere",
      "stereographic",
      "stere",
      "transverse_mercator",
      "tmerc"
    ),
    ncol  = 2,
    byrow = TRUE
  )

  m <- matrix(
    c(
      "grid_mapping_name",
      "+proj",
      "false_easting",
      "+x_0",
      "false_northing",
      "+y_0",
      "scale_factor_at_projection_origin",
      "+k_0",
      "scale_factor_at_central_meridian",
      "+k_0",
      "standard_parallel",
      "+lat_1",
      "standard_parallel1",
      "+lat_1",
      "standard_parallel2",
      "+lat_2",
      "longitude_of_central_meridian",
      "+lon_0",
      "longitude_of_projection_origin",
      "+lon_0",
      "latitude_of_projection_origin",
      "+lat_0",
      "straight_vertical_longitude_from_pole",
      "+lon_0",
      "longitude_of_prime_meridian",
      "+pm",
      "semi_major_axis",
      "+a",
      "semi_minor_axis",
      "+b",
      "inverse_flattening",
      "+rf",
      "earth_radius",
      "+a"
    ),
    ncol = 2,
    byrow = TRUE
  )

  # add logic that if prime merid is defined but not centr merid. centr merid is same as prime.

  i <- match(vars, m[,1])
  if (all(is.na(i))) {
    gg <- cbind(vars, vals)
    mtxt <- paste(
      apply(gg, 1, function(x) paste(x, collapse = '=')),
      collapse = '; '
    )
    warning("cannot process the crs\n", mtxt)
    return(NA)
  } else if (any(is.na(i))) {
    vr <- vars[is.na(i)]
    vl <- vals[is.na(i)]
    gg <- cbind(vr, vl)
    gg <- gg[!(gg[,1] %in% c("crs_wkt", "esri_pe_string")), ,drop = FALSE]
    if (NROW(gg) > 0) {
      mtxt <- paste(
        apply(gg, 1, function(x) paste(x, collapse = '=')),
        collapse = '\n'
      )
      warning("cannot process these parts of the crs:\n", mtxt)
    }
    vars <- vars[!is.na(i)]
    vals <- vals[!is.na(i)]
    i <- stats::na.omit(i)
  }
  tab <- cbind(m[i,], vals)
  rr <- which(tab[,1] == "earth_radius")
  if (length(rr) > 0) {
    bb <- tab[rr,]
    bb[2] <- "+b"
    tab <- rbind(tab, bb)
  }
  p <- which(tab[,2] == '+proj')
  if (length(p) == 0) {
    warning("cannot create a valid crs\n", mtxt)
    return(NA)
  } else {
    tab <- rbind(tab[p, ], tab[-p, ])
  }
  j <- match(tab[1,3], prj[,1])
  tab[1,3] <- prj[j,2]

  paste(
    apply(tab[,2:3], 1, function(x) paste(x, collapse = '=')),
    collapse = ' '
  )
}

