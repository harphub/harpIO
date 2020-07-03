compute_transformation_weights <- function(domain, trans, trans_opts) {

  if (trans == "none") return(trans_opts)

  if (is.null(trans_opts[["keep_raw_data"]])) {
    keep_raw_data <- FALSE
  } else {
    keep_raw_data <- trans_opts[["keep_raw_data"]]
  }

  if (!is.null(trans_opts[["domain"]])) {
    if (isFALSE(all.equal(domain, trans_opts[["domain"]]))) {
      warning("Domain mismatch. Recomputing ", trans, " weights.", call. = FALSE, immediate. = TRUE)
      trans_opts[["weights"]] <- NULL
    }
  }

  # For interpolating gridded data to points - compute weights if they aren't already passed.

  method_check <- attr(trans_opts[["weights"]], "method") != trans_opts[["method"]]

  if (trans == "interpolate" && (is.null(trans_opts[["weights"]]) || method_check)) {

    if (is.null(trans_opts[["method"]])) {
      warning("Interpolation method not set. Using 'bilinear'.", call. = FALSE, immediate. = TRUE)
      trans_opts[["method"]] <- "bilinear"
    }

    if (is.null(trans_opts[["use_mask"]])) trans_opts[["use_mask"]] <- FALSE

    trans_opts <- initialise_interpolation(
      domain   = domain,
      stations = trans_opts[["stations"]],
      method   = trans_opts[["method"]],
      use_mask = trans_opts[["use_mask"]],
      drop_NA  = TRUE
    )

  }

  # For regridding gridded data - compute weights if they aren't already passed.
  if (trans == "regrid" && (is.null(trans_opts[["weights"]]) || method_check)) {
    message("Computing interpolation weights for regridding.")

    new_domain <- try(meteogrid::as.geodomain(trans_opts[["new_domain"]]), silent = TRUE)

    if (inherits(new_domain, "try-error")) {
      stop("'new_domain' in 'trans_opts' must be a geofield or geodomain.", call. = FALSE)
    }

    trans_opts[["weights"]] <- meteogrid::regrid.init(
      olddomain = domain,
      newdomain = new_domain,
      method    = trans_opts[["method"]]
    )

  }

  # For xsection - compute interpolation weights
  if (trans == "xsection" && (is.null(trans_opts[["weights"]]) || method_check)) {

    message("Computing interpolation weights for xsectioning.")

    if (is.null(trans_opts[["a"]]) || is.null(trans_opts[["b"]])) {
      stop("End points of xsection, 'a' and 'b' must be specified.", call. = FALSE)
    }

    stopifnot(length(trans_opts[["a"]]) == 2 && length(trans_opts[["b"]] == 2))

    trans_opts[["weights"]] <- xsection_init(
      domain, trans_opts
    )

  }

  trans_opts[["keep_raw_data"]] <- keep_raw_data

  trans_opts

}
