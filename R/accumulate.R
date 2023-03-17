#' Compute accumulation between forecast lead times
#'
#' For data with equally spaced lead times, the difference between, or
#' accumulation of gridded fields between lead times is computed. The time over
#' which to compute the accumulation must be set and is calculated from the lead
#' times. If lead times are not equally spaced an attempt will be made to
#' compute all accumulations for the desired accumulation time from the
#' available lead times.
#'
#' This function would typically be used compute accumulations between lead
#' times for model outputs that are accumulated from the model start time, such
#' as precipitation and fluxes. However, \code{accumulate} could also be used
#' to compute the change in any value between lead times, such a change in
#' temperature or change in wind speed for example.
#'
#' @param .fcst A harp_fcst list or a harp_spatial_fcst data frame.
#' @param accum_time The time over which to compute the accumulation. Must be in
#'   the same units as the lead_time column.
#' @param .fcst_name Not to be set by the user, but used for consistency between
#'   classes.
#' @param ... Further arguments passed to or from methods.
#'
#' @return An object of the same class as .fcst with the accumulations computed.
#'   Rows for lead times for which it is not possible to compute the
#'   accumulation are dropped.
#' @export
#'
#' @examples
accumulate <- function(.fcst, accum_time, ...) {
  UseMethod("accumulate")
}

#' @export
accumulate.harp_spatial_fcst <- function(.fcst, accum_time, .fcst_name = "") {

  # Find forecast columns
  fcst_cols <- grep("_mbr[[:digit:]]+|_det$", colnames(.fcst))
  if (length(fcst_cols) < 1) {
    stop("No forecast columns found.", call. = FALSE)
  }

  # Check that forecast columns are geolists
  if (!all(sapply(fcst_cols, function(x) inherits(.fcst[, x][[1]], "geolist")))) {
    stop("Forecast columns must be geolists.", call. = FALSE)
  }

  # Check lead times and create empty data if required lead times do
  # not exist
  lead_times <- sort(unique(.fcst[["lead_time"]]))
  max_lt     <- ceiling(max(lead_times) / accum_time) * accum_time
  lt_res     <- unique(diff(lead_times))

  if (length(lt_res) > 1) {
    .fcst <- dplyr::full_join(
      .fcst, purrr::map_dfr(unique(.fcst$fcdate), ~data.frame(fcdate = .x, lead_time = seq(min(lead_times), max_lt, min(lt_res))))
    )
  }

  lt_res <- unique(diff(sort(unique(.fcst[["lead_time"]]))))
  if (length(lt_res) > 1) {
    stop("Something isn't right with the lead times.", call. = FALSE)
  }
  if (accum_time %% lt_res != 0) {
    warning(
      .fcst_name,
      ": Interval between lead times is not usable for accum_time = ",
      accum_time,
      call.      = FALSE,
      immediate. = TRUE
    )
    return(.fcst[FALSE, ])
  }
  lag_rows <- accum_time / lt_res

  # Group the data by fcdate, order by leadtime and compute the difference
  accum_func <- function(df, cols, lag) {
    dplyr::mutate_at(
      dplyr::arrange(df, .data[["lead_time"]]),
      cols,
      function(x) diff(x, lag = lag))
  }

  structure(
    dplyr::group_by(.fcst, .data[["fcdate"]]) %>%
      accum_func(fcst_cols, lag_rows) %>%
      dplyr::ungroup() %>%
      dplyr::filter(dplyr::if_any(dplyr::where(is.list), ~map_lgl(.x, ~!is.null(.x)))) %>%
      dplyr::arrange(.data[["fcdate"]]),
     class = class(.fcst)
   )
}

#' @export
accumulate.harp_fcst <- function(.fcst, accum_time, ...) {
  structure(
    mapply(
      function(x, y, z) accumulate(x, z, .fcst_name = y),
      .fcst,
      names(.fcst),
      MoreArgs = list(z = accum_time),
      SIMPLIFY = FALSE
    ),
    class = "harp_fcst"
  )
}

