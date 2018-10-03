#' Read point forecasts from an sqlite file
#'
#' Reads point forecasts from an sqlite file produced by
#' \link{read_eps_interpolate} or \link{read_det_interpolate}. The function
#' generates the file names from the start date, end date, forecast model(s),
#' parameter, lead time etc. supplied as arguments. The members, stations,
#' forecast dates and lead times to be retrieved from the files can also be
#' passed as arguments.
#'
#' @param start_date Start date to read from. Should be numeric or character.
#'   YYYYMMDD(HH)(mm)
#' @param end_date End date to read to. Should be numeric or character.
#' @param fcst_model The forecast model to read - this is typically used to
#'   construct the file name. Can be a character vector of model names.
#' @param parameter The forecast parameter to read. This is usually only used to
#'   construct the filename.
#' @param lead_time The lead times to be retrieved. Can be used to construct the
#'   file names and to set which lead times are retrieved.
#' @param by Used in constructing the file names. A string of a number followed
#'   by a letter, where the letter can be "d" for days, "h" for hours, "m" for
#'   minutes and "s" for seconds. Should be set to the fastest varying time
#'   dimension in the desired file names.
#' @param file_path The path to the data.
#' @param file_template The file type to generate the template for. Can be
#'   "harmoneps_grib", "harmeoneps_grib_fp", "harmoneps_grib_sfx", "meps_met",
#'   "harmonie_grib", "harmonie_grib_fp", "harmone_grib_sfx", "vfld", "vobs", or
#'   "fctable". If anything else is passed, it is returned unmodified. In this
#'   case substitutions can be used. Available substitutions are {YYYY} for
#'   year, \{MM\} for 2 digit month with leading zero, \{M\} for month with no
#'   leading zero, and similarly \{DD\} or \{D\} for day, \{HH\} or \{H\} for
#'   hour, \{mm\} or \{m\} for minute. Also \{LDTx\} for lead time and \{MBRx\}
#'   for ensemble member where x is the length of the string including leading
#'   zeros - can be omitted or 2, 3 or 4. Note that the full path to the file
#'   will always be file_path/file_template.
#' @param stations The stations to retrieve forecasts for. This should be a
#'   vector of station ID numbers.
#' @param members The members to retrieve if reading an EPS forecast. Normally a
#'   vector of a member numbers. For multi model ensembles this can be a named
#'   list with sub model name followed by the desired members, e.g. \cr
#'   \code{members = list(sub_model1 = seq(0, 3), sub_model2 = c(2, 3))}
#' @param member_regexp A regular expression to describe the column headings for
#'   ensemble forecasts. This shouldn't normally need changing.
#' @return A list with an element for each forecast model, or in the case of a
#'   multi model ensemble, another list with an element for each sub model. The
#'   list elements each contain a data frame with columns for station ID (SID),
#'   the forecast initialisation time (fcdate), the lead time (leadtime), the
#'   time for which the forecast is valid (validdate), and a column for the
#'   forecast with a heading of the model name in the case of a deterministic
#'   forecast, or multiple columns with heading names usually of the form
#'   \code{<model_name>_mbrXXX}, where XXX is the member number, in the case of
#'   an ensemble forecast.
#' @export
#'
#' @examples
read_point_forecast <- function(
  start_date,
  end_date,
  fcst_model,
  parameter,
  lead_time     = seq(0, 48, 3),
  by            = "1d",
  file_path     = ".",
  file_template = "fctable",
  stations      = NULL,
  members       = NULL,
  member_regexp = "[[:graph:]]+(?=_mbr[[:digit:]]+)"
) {

  file_names <- purrr::map(
    fcst_model,
    ~ get_filenames(
      file_path     = file_path,
      start_date    = start_date,
      end_date      = end_date,
      by            = by,
      parameter     = parameter,
      eps_model     = .x,
      lead_time     = lead_time,
      file_template = file_template
    )
  )

  missing_files <- purrr::map(
    file_names,
    ~ { if (length(.x[!file.exists(.x)]) < 1) { "none" } else { .x[!file.exists(.x)] } }
  ) %>%
    rlang::set_names(fcst_model)

  available_files <- purrr::map(
    file_names,
    ~ .x[file.exists(.x)]
  )

  fcst <- purrr::map(
    available_files,
    read_fctable,
    suppressMessages(str_datetime_to_unixtime(start_date)),
    suppressMessages(str_datetime_to_unixtime(end_date)),
    lead_time = lead_time,
    stations  = stations,
    members   = members
  )

  split_sub_models <- function(df) {

    meta_cols  <- rlang::syms(c("SID", "fcdate", "leadtime", "validdate"))
    sub_models <- stringr::str_extract(
      names(df),
      member_regexp
    ) %>%
      na.omit() %>%
      unique()

    if (length(sub_models) == 1) {
      df
    } else {
      df <- purrr::map(
        sub_models,
        ~ dplyr::select(df, !!! meta_cols, dplyr::contains(.x))
      ) %>%
        rlang::set_names(sub_models)
      class(df) <- "harp_fcst"
      df
    }

  }

  fcst <- purrr::map(fcst, split_sub_models) %>%
    rlang::set_names(fcst_model)

  attr(fcst, "missing_files") <- missing_files
  class(fcst) <- "harp_fcst"

  fcst

}
