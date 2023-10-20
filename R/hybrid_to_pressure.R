# This function needs more work before it can be exported
# #' Title
# #'
# #' @param .df
# #' @param level_definitions
# #' @param psfc_col
# #'
# #' @return
# #' @export
hybrid_to_pressure <- function(
  .df,
  level_definitions,
  psfc_col = psfc,
  a_col    = ap,
  b_col    = b
) {
  stopifnot(is.data.frame(level_definitions))
  mandatory_cols <- c("level", "ap", "b")
  stopifnot(
    intersect(mandatory_cols, colnames(level_definitions)) == mandatory_cols
  )
  UseMethod("hybrid_to_pressure")
}

# #' @export
hybrid_to_pressure.data.frame <- function(
  .df,
  level_definitions,
  psfc_col = psfc,
  a_col    = ap,
  b_col    = b
) {

  stopifnot(is.element("level", colnames(.df)))
  .df <- dplyr::inner_join(
    .df, level_definitions, by = "level"
  )

  if (nrow(.df) < 1) {
    stop("`.df` and `level_definitions do not have common levels")
  }

  a_col_quo    <- rlang::enquo(a_col)
  b_col_quo    <- rlang::enquo(b_col)
  psfc_col_quo <- rlang::enquo(psfc_col)

  dplyr::mutate(
    .df,
    pressure = !!a_col_quo + !!b_col_quo * !!psfc_col_quo
  )
}

# #' @export
hybrid_to_pressure.harp_xs_df <- function(
  .df,
  level_definitions,
  psfc_col = psfc,
  a_col    = ap,
  b_col    = b
) {

  a_col_quo    <- rlang::enquo(a_col)
  b_col_quo    <- rlang::enquo(b_col)
  psfc_col_quo <- rlang::enquo(psfc_col)

  dplyr::mutate(
    .df,
    xsection_data = purrr::map(
      .data[["xsection_data"]],
      hybrid_to_pressure,
      level_definitions,
      !!psfc_col_quo,
      !!a_col_quo,
      !!b_col_quo
    )
  )
}

# This function needs more work before it can be exported
# #' Title
# #'
# #' @param .df
# #'
# #' @return
# #' @export
# #'
# #' @examples
xs_to_regular_pressure_levels <- function(.df) {
  UseMethod("xs_to_regular_pressure_levels")
}

# #' @export
xs_to_regular_pressure_levels.data.frame <- function(.df) {

  res <- dplyr::group_by(.df, .data[["distance"]]) %>%
    dplyr::summarize(mm = min(diff(.data[["pressure"]]))) %>%
    dplyr::pull(dplyr::all_of("mm")) %>%
    min()

  res <- floor(res)

  dplyr::group_by(.df, .data[["distance"]]) %>%
    dplyr::summarise(
      interp_data = list(
        p_to_even_p(.data[["pressure"]], .data[["value"]], res)
      )
    ) %>%
    tidyr::unnest(.data[["interp_data"]])

}

# #' @export
xs_to_regular_pressure_levels.harp_xs_df <- function(.df) {
  dplyr::mutate(
    .df,
    xs_reg_p = purrr::map(
      .data[["xsection_data"]],
      xs_to_regular_pressure_levels
    )
  )
}

p_to_even_p <- function(p, val, res) {
  even_p <- seq(min(p), max(p), res)
  interp <- approx(p, val, even_p)
  tibble::tibble(
    pressure = even_p,
    value    = interp[["y"]]
  )
}

# This function needs more work before it can be exported
# #' Title
# #'
# #' @param .df
# #' @param ...
# #'
# #' @return
# #' @export
# #'
# #' @examples
psfc_to_polygon <- function(.df, ...) {
  UseMethod("psfc_to_polygon")
}

# #' @export
psfc_to_polygon.data.frame <- function(.df, xs_int) {

  xs <- dplyr::filter(.df, .data[["level"]] == min(.data[["level"]])) %>%
    dplyr::arrange(.data[["distance"]]) %>%
    dplyr::select(.data[["distance"]], .data[["psfc"]])

  dplyr::bind_rows(
    dplyr::mutate(
      .df[1, ],
      psfc = max(max(.df[["psfc"]]), max(xs_int[["pressure"]]))
    ),
    xs,
    dplyr::mutate(
      .df[nrow(xs), ],
      psfc = max(max(.df[["psfc"]]), max(xs_int[["pressure"]]))
    )
  ) %>%
    dplyr::left_join(
      dplyr::rename(.df, psfc_line = .data[["psfc"]]),
      by = "distance"
    )
}

# #' @export
psfc_to_polygon.harp_xs_df <- function(.df, ...) {
  dplyr::mutate(
    .df,
    psfc_polygon = purrr::map2(
      .data[["xsection_data"]],
      .data[["xs_reg_p"]],
      psfc_to_polygon
    )
  )
}
