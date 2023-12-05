setup_transformation <- function(trans, opts) {

  if (trans %in% c("none")) {
    return(opts)
  }

  if (is.null(opts)) {
    if (trans == "interpolate") {
      warning(
        "'transformation_opts' not set for transformation = 'interpolate'. ",
        "Using default interpolate_opts()",
        call.      = FALSE,
        immediate. = TRUE
      )
      opts <- interpolate_opts()
    } else {
      stop(
        "'transformation_opts' must be set for transformation = '", trans, "'",
        "using tranformation_opts = ", trans, "_opts(...).",
        call. = FALSE
      )
    }
  }

  opts_func <- get(paste0(trans, "_opts"))
  opts      <- do.call(opts_func, opts[names(opts)[names(opts) != "weights"]])

  if (is.null(opts[["clim_file"]])) {
    return(opts)
  }

  if (!file.exists(opts[["clim_file"]])) {
    stop("Cannot find 'clim_file': ", opts[["clim_file"]], call. = FALSE)
  }

  if (is.null(opts[["clim_file_format"]]) || is.na(opts[["clim_file_format"]])) {
    clim_file_format <- guess_format(opts[["clim_file"]])
    if (is.na(clim_file_format)) {
      stop(
        "Cannot establish file format for: ", opts[["clim_file"]], ".\n",
        "Use transformation_opts = ", trans, "_opts(clim_file_format = '<format>') to set.",
        call. = FALSE
      )
    }
    if (clim_file_format == "vfld") {
      return(opts)
    }
    opts[["clim_file_format"]] <- clim_file_format
  }

  opts <- get_clim(opts, trans)

  get_weights(trans, opts)

}

get_clim <- function(opts, trans) {
  if (is.null(opts[["clim_param"]]) || is.na(opts[["clim_param"]])) {

    get_domain       <- get(paste0("get_domain_", opts[["clim_file_format"]]))
    opts[["domain"]] <- try(get_domain(opts[["clim_file"]], opts[["clim_file_opts"]]))

    if (inherits(opts[["domain"]], "try-error")) {
      stop(
        "Cannot get domain from: ", opts[["clim_file"]], ".\n",
        "Maybe you need to set correct file format options with: \n",
        "transformation_opts = ", trans, "_opts(clim_file_opts = ",
        opts[["clim_file_format"]], "_opts(<options>)",
        call. = FALSE
      )
    }

  } else {

    if (length(opts[["clim_file_opts"]]) < 1) {
      opts_fun <- paste0(opts[["clim_file_format"]], "_opts")
      if (exists(opts_fun)) {
        opts_fun <- get(opts_fun)
        if (is.function(opts_fun)) {
          opts[["clim_file_opts"]] <- opts_fun()
        }
      }
    }
    if (length(opts[["clim_file_opts"]]) < 1) {
      opts[["clim_file_opts"]] <- list()
    }

    opts[["clim_file_opts"]][["first_only"]] <- TRUE

    opts[["clim_field"]] <- try(
      read_grid(
        opts[["clim_file"]],
        opts[["clim_param"]],
        file_format      = opts[["clim_file_format"]],
        file_format_opts = opts[["clim_file_opts"]]
      )
    )

    if (inherits(opts[["clim_field"]], "try-error")) {
      stop(
        "Cannot get '", opts[["clim_param"]], "' from: ", opts[["clim_file"]], ".\n",
        "Maybe you need to set the correct 'clim_param' with: \n",
        "transformation_opts = ", trans, "_opts(clim_param = <param>)",
        call. = FALSE
      )
    }

    opts[["domain"]] <- meteogrid::as.geodomain(opts[["clim_field"]])

  }

  opts

}

get_clim_netcdf <- function(opts, trans) {

  if (is.null(opts[["clim_file_opts"]])) {
    message("Using default netcdf_opts to get domain from: ", opts[["clim_file"]])
    opts[["clim_file_opts"]] <- netcdf_opts()
  }

  if (is.null(opts[["clim_param"]]) || is.na(opts[["clim_param"]])) {
    clim_domain <- try(get_domain_netcdf(opts[["clim_file"]], opts[["clim_file_opts"]]))
    if (inherits(clim_domain, "try-error")) {
      stop(
        "Cannot get domain from: ", opts[["clim_file"]], ".\n",
        "Maybe you need to set correct netcdf options with: \n",
        "transformation_opts = ", trans, "_opts(clim_file_opts = netcdf_opts(<options>)",
        call. = FALSE
      )
    }
    opts[["domain"]] <- clim_domain
  } else {
    opts[["clim_file_opts"]][["first_only"]] <- TRUE # Only take the first 2d field
    opts[["clim_field"]] <- try(
      read_grid(
        opts[["clim_file"]],
        opts[["clim_param"]],
        file_format      = opts[["clim_file_format"]],
        file_format_opts = opts[["clim_file_opts"]]
      ),
      silent = TRUE
    )
    if (inherits(opts[["clim_field"]], "try-error")) {
      stop(
        "Cannot get '", opts[["clim_param"]], "' from: ", opts[["clim_file"]], ".\n",
        "Maybe you need to set correct netcdf options with: \n",
        "transformation_opts = ", trans, "_opts(clim_file_opts = netcdf_opts(<options>), clim_param = <param>)",
        call. = FALSE
      )
    }
    opts[["domain"]] <- meteogrid::as.geodomain(opts[["clim_field"]])
  }
  opts
}

get_weights <- function(trans, opts) {

  if (!is.null(opts[["keep_raw_data"]])) {
    keep_raw_data <- opts[["keep_raw_data"]]
  } else {
    keep_raw_data <- FALSE
  }

  # For interpolating gridded data to points - compute weights if they aren't already passed.

  method_check <- attr(opts[["weights"]], "method") != opts[["method"]]

  if (trans == "interpolate" && (is.null(opts[["weights"]]) || method_check)) {

    if (is.null(opts[["method"]])) {
      warning("Interpolation method not set. Using 'bilinear'.", call. = FALSE, immediate. = TRUE)
      opts[["method"]] <- "bilinear"
    }

    if (is.null(opts[["use_mask"]])) opts[["use_mask"]] <- FALSE

    xtra_opts <- initialise_interpolation(
      domain   = opts[["domain"]],
      stations = opts[["stations"]],
      method   = opts[["method"]],
      use_mask = opts[["use_mask"]],
      drop_NA  = TRUE
    )
    opts[["keep_raw_data"]] <- keep_raw_data
    opts <- opts[!names(opts) %in% names(xtra_opts)]
    opts <- c(opts, xtra_opts)
  }

  # For regridding gridded data - compute weights if they aren't already passed.
  if (trans == "regrid" && is.null(opts[["weights"]])) {
    message("Computing interpolation weights.")
    # Assume grib message at position 1 has the same domain information as all messages
    old_domain <- opts[["domain"]]
    new_domain <- try(meteogrid::as.geodomain(opts[["new_domain"]]))
    if (inherits(new_domain, "try-error")) {
      stop("'new_domain' in 'transformation_opts' must be a geofield or geodomain.", call. = FALSE)
    }
    opts[["weights"]] <- meteogrid::regrid.init(
      olddomain = old_domain,
      newdomain = new_domain,
      method    = opts[["method"]]
    )
  }

  # For xsection - compute interpolation weights
  if (trans == "xsection") {
    message("Computing interpolation weights.")
    # Assume grib message at position 1 has the same domain information as all messages
    geofield_domain <- opts[["domain"]]
    if (is.null(opts[["a"]]) || is.null(opts[["b"]])) {
      stop("End points of xsection, 'a' and 'b' must be specified.", call. = FALSE)
    }
    stopifnot(length(opts[["a"]]) == 2 && length(opts[["b"]] == 2))
    opts[["weights"]] <- xsection_init(geofield_domain, opts)
  }

  # meteogrid methods allow different names for the same method - ensure
  # that the names match
  opts[["method"]] <- attr(opts[["weights"]], "method")

  opts

}
