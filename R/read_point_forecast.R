#' Read point forecasts from FCTABLE files
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
#' @param fcst_type The type of forecast to read. Set to "det" for deterministic
#'   or "eps" for ensemble.
#' @param parameter The forecast parameter to read. This is usually only used to
#'   construct the filename, and in accumumlating precipitation.
#' @param lead_time The lead times to be retrieved. Can be used to construct the
#'   file names and to set which lead times are retrieved.
#' @param by Used in constructing the file names. A string of a number followed
#'   by a letter, where the letter can be "d" for days, "h" for hours, "m" for
#'   minutes and "s" for seconds. Should be set to the fastest varying time
#'   dimension in the desired file names.
#' @param file_path The path to the data.
#' @param stations The stations to retrieve forecasts for. This should be a
#'   vector of station ID numbers.
#' @param members The members to retrieve if reading an EPS forecast. Normally a
#'   vector of a member numbers. For multi model ensembles this can be a named
#'   list with sub model name followed by the desired members, e.g. \cr
#'   \code{members = list(sub_model1 = seq(0, 3), sub_model2 = c(2, 3))}
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
  fcst_type,
  parameter,
  lead_time     = seq(0, 48, 3),
  by            = "1d",
  file_path     = ".",
  stations      = NULL,
  members       = NULL
) {

  switch(tolower(fcst_type),
    "eps" = {
      file_template <- "fctable_eps"
      member_regexp <- "[[:graph:]]+(?=_mbr[[:digit:]]+)"
      fcst_suffix   <- "_mbr"
    },
    "det" = {
      file_template <- "fctable_det"
      member_regexp <- "[[:graph:]]+(?=_det)"
      fcst_suffix   <- "_det"
    },
    {
      file_template <- NULL
      member_regexp <- NULL
      fcst_suffix   <- NULL
    }
  )
  if (is.null(member_regexp)) {
    stop("Unknown fcst_type argument: ", fcst_type, ". \nMust be one of 'eps' or 'det'", call. = FALSE)
  }

  parameter  <- parse_harp_parameter(parameter)
  param_name <- parameter$fullname
  if (parameter$accum > 0) {
    param_name <- parameter$basename
    lead_time  <- lead_time[lead_time >= parse_accum(parameter)]
  }

  if (length(lead_time) < 1) {
    stop(
      "At least one of lead_time must be greater than or equal to the accumulation time: ",
      parse_accum(parameter), parameter$acc_unit,
      call. = FALSE
    )
  }

  file_names <- purrr::map(
    fcst_model,
    ~ get_filenames(
      file_path     = file_path,
      start_date    = start_date,
      end_date      = end_date,
      by            = by,
      parameter     = param_name,
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

  if (length(purrr::flatten(available_files)) < 1) {
    stop("No forecast files found \n", missing_files, call. = FALSE)
  }

  fcst <- purrr::map(
    available_files,
    read_fctable,
    suppressMessages(str_datetime_to_unixtime(start_date)),
    suppressMessages(str_datetime_to_unixtime(end_date)),
    lead_time = lead_time,
    stations  = stations,
    members   = members
  )

  fcst <- purrr::map(fcst, tidyr::drop_na)

  if (parameter$accum > 0) {

    accum <- parse_accum(parameter)

    fcst_accum <- purrr::map(
      fcst,
      ~ accumulate_forecast(
        tidyr::gather(.x, dplyr::contains(fcst_suffix), key = "member", value = "forecast"),
        accum,
        parameter$acc_unit
      )
    )

    # accumulate_forecast returns a vector of missing lead times rather than data if some lead times
    # to compute an accumlation are missing.

    if (any(purrr::map_lgl(fcst_accum, is.numeric))) {
      lead_time_accum <- fcst_accum


      file_names <- purrr::map2(
        fcst_model,
        lead_time_accum,
        ~ get_filenames(
          file_path     = file_path,
          start_date    = start_date,
          end_date      = end_date,
          by            = by,
          parameter     = param_name,
          eps_model     = .x,
          lead_time     = .y,
          file_template = file_template
        )
      )

      fcst_lead_time_accum <- purrr::map2(
        purrr::map(file_names, ~ .x[file.exists(.x)]),
        lead_time_accum,
        ~ read_fctable(
          .x,
          suppressMessages(str_datetime_to_unixtime(start_date)),
          suppressMessages(str_datetime_to_unixtime(end_date)),
          lead_time = .y,
          stations  = stations,
          members   = members
        )
      ) %>% purrr::map(tidyr::drop_na)

      fcst       <- purrr::map2(fcst, fcst_lead_time_accum, dplyr::bind_rows)
      fcst_accum <- purrr::map(
        fcst,
        ~ accumulate_forecast(
          tidyr::gather(.x, dplyr::contains(fcst_suffix), key = "member", value = "forecast"),
          accum,
          parameter$acc_unit,
          check_leads = FALSE
        )
      )

    }

    fcst <- purrr::map(fcst_accum, tidyr::spread, .data$member, .data$forecast)

  }

  split_sub_models <- function(df, .member_regexp) {

    meta_cols  <- rlang::syms(c("SID", "fcdate", "leadtime", "validdate", "fcst_cycle"))
    sub_models <- stringr::str_extract(
      names(df),
      .member_regexp
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

  fcst <- purrr::map(fcst, split_sub_models, member_regexp) %>%
    rlang::set_names(fcst_model)

  attr(fcst, "missing_files") <- missing_files
  class(fcst) <- "harp_fcst"

  fcst

}

parse_accum <- function(prm) {
  switch(prm$acc_unit,
    "h" = prm$accum / 3600,
    "m" = prm$accum / 60,
    prm$accum
  )
}
