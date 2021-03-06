weights_from_fcst_file <- function(
  file_name,
  file_format,
  format_opts,
  trans,
  trans_opts,
  parameter
) {

  if (trans == "none" || !is.null(trans_opts[["weights"]])) {
    return(trans_opts)
  }

  if (!is.null(trans_opts[["clim_param"]]) && !is.na(trans_opts[["clim_param"]])) {
    message("Initializing ", trans, " weights using '", trans_opts[["clim_param"]], "'.")
  } else {
    message("Initializing ", trans, " weights.")
  }

  trans_opts[["clim_file"]]        <- file_name
  trans_opts[["clim_file_format"]] <- file_format
  trans_opts[["clim_file_opts"]]   <- format_opts

  trans_opts_try <- try(setup_transformation(trans, trans_opts), silent = TRUE)

  # If a clim_param was specified, but not found in the file, set to NULL if
  # none of the parameters are T2m and correct_t2m = FALSE.
  if (inherits(trans_opts_try, "try-error")) {

    if (is.element("t2m", tolower(parameter)) && trans_opts[["correct_t2m"]]) {
      stop (
        "2m temperature height correction is selected, but ",
        "cannot find '", trans_opts[["clim_param"]], "' in ",
        trans_opts[["clim_file"]], ".\n",
        "You probably need to set transformation_opts = ", trans,
        "_opts(clim_file = '<clim_file>', clim_param = '<clim_param>'), ",
        "or switch off 2m temperature correction with transformation_opts = ",
        "interpolate_opts(correct_t2m = FALSE).",
        call. = FALSE
      )
    } else {
      message ("'", trans_opts[["clim_param"]], "' not found. Attempting to get domain from file.")
      trans_opts[["clim_param"]] <- NA
      trans_opts <- setup_transformation(trans, trans_opts)
    }

  } else {

    trans_opts <- trans_opts_try

  }

  trans_opts

}
