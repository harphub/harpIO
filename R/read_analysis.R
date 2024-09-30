#' Read analysis data from multiple files
#'
#' \code{read_analysis} generates file names, based on the arguments given,
#' reads data from them, and optionally performs a transformation on those data.
#' Works in much the same as \code{\link{read_forecast}} except dates must be
#' given explictly. These can be generated from \code{\link{seq_dates}}. Also,
#' data are returned by default. If just want to use this function for
#' interpolating to points and writing the results to sqlite files, make sure to
#' set \code{return_data = FALSE}.
#'
#' @param analysis_model The name of the analysis model(s) to read. Can be
#'   expressed as character vector if more than one model is wanted.
#' @inheritParams read_forecast
#'
#' @return When \code{return_date = TRUE}, a harp_analysis object.
#' @export
read_analysis <- function(
  dttm,
  analysis_model,
  parameter,
  members             = NULL,
  members_out         = members,
  lags                = NULL,
  vertical_coordinate = c("pressure", "model", "height", "depth", NA),
  file_path           = getwd(),
  file_format         = NULL,
  file_template       = "an{YYYY}{MM}{DD}{HH}.grib",
  file_format_opts    = list(),
  transformation      = c("none", "interpolate", "regrid", "xsection", "subgrid"),
  transformation_opts = NULL,
  param_defs          = getExportedValue("harpIO", "harp_params"),
  output_file_opts    = sqlite_opts(),
  return_data         = TRUE,
  merge_lags          = TRUE,
  show_progress       = TRUE,
  stop_on_fail        = FALSE,
  start_date          = NULL,
  end_date            = NULL,
  by                  = "6h",
  ...
) {

  if (missing(dttm)) {
    if (any(sapply(list(start_date, end_date, by), is.null))) {
      stop(
        "If `dttm` is not passed, `start_date`, `end_date` ",
        "and `by` must be passed."
      )
    }
    dttm <- harpCore::seq_dttm(start_date, end_date, by)
  }

  vertical_coordinate <- match.arg(vertical_coordinate)
  transformation      <- match.arg(transformation)

  check_param_defs(param_defs)

  analysis <- read_forecast(
    dttm                = dttm,
    fcst_model          = analysis_model,
    parameter           = parameter,
    lead_time           = 0,
    members             = members,
    members_out         = members_out,
    lags                = lags,
    vertical_coordinate = vertical_coordinate,
    file_path           = file_path,
    file_format         = file_format,
    file_template       = file_template,
    file_format_opts    = file_format_opts,
    transformation      = transformation,
    transformation_opts = transformation_opts,
    param_defs          = param_defs,
    output_file_opts    = output_file_opts,
    return_data         = return_data,
    merge_lags          = merge_lags,
    show_progress       = show_progress,
    stop_on_fail        = stop_on_fail,
    is_forecast         = FALSE,
    ...
  )

  num_members <- length(members)

  if (is.data.frame(analysis)) {
    return(fix_analysis_df(analysis, num_members, dttm))
  }
  harpCore::as_harp_list(
    lapply(analysis, fix_analysis_df, num_members, dttm)
  )
}

fix_analysis_df <- function(.df, num_members, dttm) {
  if (num_members < 2) {
    colnames(.df) <- suppressWarnings(harpCore::psub(
      colnames(.df),
      c("fcst_model", "fcst", "[[:graph:]]*_mbr[[:digit:]]{3}", "_lag"),
      c("anl_model", "anl", "anl", "")
    ))
  } else {
    colnames(.df) <- suppressWarnings(harpCore::psub(
      colnames(.df),
      c("^fcst_model$", "^fcst$", "[[:graph:]]*(?=_mbr[[:digit:]]{3})", "_lag"),
      c("anl_model", "anl", "anl", ""),
      exact = FALSE, perl = TRUE
    ))
  }
  if (nrow(.df) == 1) {
    if (is.na(.df[["valid_dttm"]]) && length(dttm) == 1) {
      .df[["valid_dttm"]] <- dttm
    }
  }
  harpCore::as_harp_df(.df)
}



