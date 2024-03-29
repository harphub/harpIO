# Accumulate a forecast over a given time period.
#
# Given an object of class \code{harp_point_forecast},
# \code{accumulate_forecast} will return the same object, but with the forecast
# column having the amount accumulated over the specified
# \code{accumulation_time}. This is done through subtraction of the forecast at
# lead time T - \code{accumulation_time} from the forecast at lead time T.
# \code{accumulation_time} must therefore be in the same units as the
# \code{leadtime} column.
#
# @param .fcst A data frame of class \code{harp_point_forecast}.
# @param accumulation_time The time over which the accumulation is to be
#   calculated. Must be in the same time units as the \code{leadtime} column.
#
# @return A data frame of class \code{harp_point_forecast} with the
#   \code{forecast} column containing the accumulated forecast.
#


### CALLED BY read_point_forecast - doesn't need to be exported.
accumulate_forecast <- function(.fcst, accumulation_time, accumulation_unit, check_leads = TRUE) {

  message("Accumulating forecast for ", accumulation_time, accumulation_unit, " accumulations")

  fcst_cols  <- colnames(.fcst)
  fcst_lead  <- intersect(c("lead_time", "leadtime"), fcst_cols)
  fcst_dttm  <- intersect(c("fcst_dttm", "fcdate"), fcst_cols)
  valid_dttm <- intersect(c("valid_dttm", "validdate"), fcst_cols)

  lead_times          <- sort(unique(.fcst[[fcst_lead]]))
  required_lead_times <- union((lead_times - accumulation_time), lead_times)
  missing_lead_times  <- setdiff(required_lead_times, lead_times)
  missing_lead_times  <- missing_lead_times[missing_lead_times > 0]


  if (any(lead_times == accumulation_time) && !any(lead_times == 0)) {
    first_accum_fcst <- dplyr::filter(.fcst, .data[[fcst_lead]] == accumulation_time)
  } else {
    first_accum_fcst <- dplyr::filter(.fcst, .data[[fcst_lead]] < min(lead_times))
  }

  if (length(missing_lead_times) > 0 && check_leads) {
    warning("Not enough lead times to compute accumulation - will try to get more data\n", immediate. = TRUE, call. = FALSE)
    return(missing_lead_times)
  }

  lead_times_res <- unique(stats::na.omit(lead_times - dplyr::lag(lead_times)))

  if (length(lead_times_res) == 0) {
    return(first_accum_fcst)
  }

  if (length(lead_times_res) > 1) {

    warning("Lead times are not equally spaced. Accumulating could take some time\n", immediate. = TRUE, call. = FALSE)

    .fcst <- .fcst %>%
      dplyr::mutate(lead_acc = .data[[fcst_lead]] + accumulation_time)

    if (nrow(.fcst) > 0) {

      fcst_lead_sym <- rlang::sym(fcst_lead)

      .fcst <- dplyr::inner_join(
        .fcst,
        dplyr::select(
          dplyr::filter(.fcst, .data[[fcst_lead]] >= accumulation_time),
          .data$SID,
          .data[[fcst_dttm]],
          lead_acc = .data[[fcst_lead]],
          fcst_acc = .data$forecast,
          .data$member
        ),
        by = c("SID", fcst_dttm, "lead_acc", "member")
      ) %>%
        dplyr::mutate(
          forecast = .data$fcst_acc - .data$forecast,
          forecast = dplyr::case_when(.data$forecast < 0 ~ 0, TRUE ~ .data$forecast),
          !!fcst_lead_sym := .data$lead_acc
        )

    }

    .fcst <- dplyr::select(.fcst, -dplyr::contains("_acc"))

  } else {

    lag_step <- accumulation_time / lead_times_res

    if (tidyr_new_interface()) {
      .fcst <- tidyr::nest(.fcst, data = -tidyr::one_of(fcst_lead))
    } else {
    .fcst <- .fcst %>%
      dplyr::group_by(.data[[fcst_lead]]) %>%
      tidyr::nest()
    }
    .fcst <- .fcst %>%
      dplyr::mutate(
        lagged_data = dplyr::lag(.data$data, lag_step, order_by = .data[[fcst_lead]]),
        type = purrr::map_lgl(.data$lagged_data, tibble::is_tibble)
      ) %>%
      dplyr::filter(.data$type) %>%
      dplyr::mutate(equal_rows = purrr::map2_lgl(.data$data, .data$lagged_data, ~ (nrow(.x) - nrow(.y)) == 0))

    # If there are missing data, the data have to be joined otherwise they can be unnested

    bad_data <- dplyr::filter(.fcst, !.data$equal_rows)
    .fcst    <- dplyr::filter(.fcst, .data$equal_rows)

    if (nrow(.fcst) > 0) {
      if (tidyr_new_interface()) {
        .fcst <- tidyr::unnest(
          .fcst,
          tidyr::one_of(c("data", "lagged_data")),
          names_repair = function(x) make.names(x, unique = TRUE)
        )
        .fcst <- dplyr::rename_at(.fcst, grep(".1$", names(.fcst)), function(x) gsub(".1$", "1", x))
      } else {
        .fcst <- tidyr::unnest(.fcst)
      }
      .fcst <- .fcst %>%
        dplyr::mutate(forecast = .data$forecast - .data$forecast1) %>%
        dplyr::select(-dplyr::ends_with("1"), -.data$type, -.data$equal_rows)
    }

    if (nrow(bad_data) > 0) {
      warning("Some lead times do not have equal amounts of data - doing a slower robust join.\n", immediate. = TRUE, call. = FALSE)
      if (tidyr_new_interface()) {
        bad_data_lagged <- tidyr::unnest(bad_data, tidyr::one_of("lagged_data")) %>%
          dplyr::select(-.data[["data"]])
        bad_data        <- tidyr::unnest(bad_data, tidyr::one_of("data")) %>%
          dplyr::select(-.data[["lagged_data"]])
      } else {
        bad_data_lagged <- tidyr::unnest(bad_data, .data$lagged_data)
        bad_data        <- tidyr::unnest(bad_data, .data$data)
      }
      bad_data_lagged <- dplyr::rename(bad_data_lagged, forecast1 = .data$forecast)

      join_cols <- intersect(
        c(fcst_lead, "SID", fcst_dttm, "fcst_cycle", "member", "units", "parameter", "model_elevation"),
        names(bad_data)
      )
      bad_data <- bad_data %>%
        dplyr::inner_join(
          bad_data_lagged,
          by = join_cols
        )

      valid_dttm_sym <- rlang::sym(valid_dttm)
      bad_data <- bad_data %>%
        dplyr::mutate(forecast = .data$forecast - .data$forecast1) %>%
        dplyr::rename(!!valid_dttm_sym := .data[[paste0(valid_dttm, ".x")]]) %>%
        dplyr::select(-dplyr::starts_with("type"), -dplyr::ends_with(".y"), -dplyr::ends_with(".x"), -.data$forecast1)
      if (nrow(.fcst) > 0) {
        .fcst <- dplyr::bind_rows(.fcst, bad_data) %>%
          dplyr::arrange(.data[[fcst_lead]])
      } else {
        .fcst <- bad_data
      }
      .fcst <- dplyr::mutate(.fcst, forecast = dplyr::case_when(.data$forecast < 0 ~ 0, TRUE ~ .data$forecast))
    }
  }

  if (nrow(first_accum_fcst) > 0) {
    dplyr::bind_rows(first_accum_fcst, .fcst)
  } else {
    .fcst
  }

}
