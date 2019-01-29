#' Accumulate a forecast over a given time period.
#'
#' Given an object of class \code{harp_point_forecast},
#' \code{accumulate_forecast} will return the same object, but with the forecast
#' column having the amount accumulated over the specified
#' \code{accumulation_time}. This is done through subtraction of the forecast at
#' lead time T - \code{accumulation_time} from the forecast at lead time T.
#' \code{accumulation_time} must therefore be in the same units as the
#' \code{leadtime} column.
#'
#' @param .fcst A data frame of class \code{harp_point_forecast}.
#' @param accumulation_time The time over which the accumulation is to be
#'   calculated. Must be in the same time units as the \code{leadtime} column.
#'
#' @return A data frame of class \code{harp_point_forecast} with the
#'   \code{forecast} column containing the accumulated forecast.
#'
#' @examples

### CALLED BY read_point_forecast - doesn't need to be exported.
accumulate_forecast <- function(.fcst, accumulation_time, accumulation_unit) {

  message("Accumulating forecast for ", accumulation_time, accumulation_unit, " accumulations")

  lead_times <- sort(unique(.fcst$leadtime))

  if (any(lead_times == accumulation_time) && !any(lead_times == 0)) {
    first_accum_fcst <- dplyr::filter(.fcst, .data$leadtime == accumulation_time)
  } else {
    first_accum_fcst <- dplyr::filter(.fcst, .data$leadtime < min(lead_times))
  }

  if (length(lead_times) == 1  && nrow(first_accum_fcst) == 0) {
    warning("Not enough lead times to compute accumulation - will try to get more data\n", immediate. = TRUE, call. = FALSE)
    return(dplyr::filter(.fcst, .data$leadtime != lead_times))
  }

  lead_times_res <- unique(na.omit(lead_times - dplyr::lag(lead_times)))

  if (length(lead_times_res) == 0) {
    return(first_accum_fcst)
  }

  if (length(lead_times_res) > 1) {

    warning("Lead times are not equally spaced. Accumulating could take some time\n", immediate. = TRUE, call. = FALSE)

    .fcst <- .fcst %>%
      dplyr::mutate(lead_acc = .data$leadtime + accumulation_time)

    if (nrow(.fcst) > 0) {

      .fcst <- dplyr::inner_join(
        .fcst,
        dplyr::select(
          dplyr::filter(.fcst, .data$leadtime >= accumulation_time),
          .data$SID,
          .data$fcdate,
          lead_acc = .data$leadtime,
          fcst_acc = .data$forecast,
          .data$member
        ),
        by = c("SID", "fcdate", "lead_acc", "member")
      ) %>%
        dplyr::mutate(
          forecast = .data$fcst_acc - .data$forecast,
          forecast = dplyr::case_when(.data$forecast < 0 ~ 0, TRUE ~ .data$forecast),
          leadtime = .data$lead_acc
        )

    }

    .fcst <- dplyr::select(.fcst, -dplyr::contains("_acc"))

  } else {

    lag_step <- accumulation_time / lead_times_res

    .fcst <- .fcst %>%
      dplyr::group_by(.data$leadtime) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        lagged_data = dplyr::lag(.data$data, lag_step, order_by = .data$leadtime),
        type = purrr::map_lgl(.data$lagged_data, tibble::is_tibble)
      ) %>%
      dplyr::filter(.data$type) %>%
      dplyr::mutate(equal_rows = purrr::map2_lgl(.data$data, .data$lagged_data, ~ (nrow(.x) - nrow(.y)) == 0))

    # If there are missing data, the data have to be joined otherwise they can be unnested

    bad_data <- dplyr::filter(.fcst, !.data$equal_rows)
    .fcst    <- dplyr::filter(.fcst, .data$equal_rows)

    if (nrow(.fcst) > 0) {
      .fcst <- .fcst %>%
        tidyr::unnest() %>%
        dplyr::mutate(forecast = .data$forecast - .data$forecast1) %>%
        dplyr::select(-dplyr::ends_with("1"), -.data$type)
    }

    if (nrow(bad_data) > 0) {
      warning("Some lead times do not have equal amounts of data - doing a slower robust join.\n", immediate. = TRUE, call. = FALSE)
      bad_data_lagged <- tidyr::unnest(bad_data, .data$lagged_data) %>%
        dplyr::rename(forecast1 = .data$forecast)
      bad_data <- tidyr::unnest(bad_data, .data$data) %>%
        dplyr::inner_join(
          bad_data_lagged,
          by = c("leadtime", "SID", "fcdate", "fcst_cycle", "member")
        )
      bad_data <- bad_data %>%
        dplyr::mutate(forecast = .data$forecast - .data$forecast1) %>%
        dplyr::rename(validdate = .data$validdate.x) %>%
        dplyr::select(-dplyr::starts_with("type"), -dplyr::ends_with(".y"), -dplyr::ends_with(".x"), -.data$forecast1)
      if (nrow(.fcst) > 0) {
        bad_data <- dplyr::bind_rows(.fcst, bad_data) %>%
          dplyr::arrange(.data$leadtime)
      } else {
        .fcst <- bad_data
      }
    }
  }

  if (nrow(first_accum_fcst) > 0) {
    dplyr::bind_rows(first_accum_fcst, .fcst)
  } else {
    .fcst
  }

}
