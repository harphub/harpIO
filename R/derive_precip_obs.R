derive_precip_obs <- function(df) {
  res <- dplyr::select(
    df,
    dplyr::any_of(c("valid_dttm", "SID", "AccPcp12h", "AccPcp24h", "AccPcp6h", "AccPcp3h"))
  )

  res <- decum_from(res, 12, 6)
  res <- decum_from(res, 24, 12)
  res <- decum_from(res, 12, 6)
  res <- decum_from(res, 6, 3)

  filter(res, dplyr::if_any(dplyr::starts_with("AccPcp"), ~!is.na(.x)))
}

decum_from <- function(df, from = 12, to = 6) {

  from_col <- paste0("AccPcp", from, "h")
  to_col   <- paste0("AccPcp", to, "h")
  offset   <- 3600 * (from - to)

  if (
    !is.element(from_col, colnames(df)) || !is.element(to_col, colnames(df))
  ) {
    return(df)
  }

  df <- dplyr::left_join(
    df,
    dplyr::select(
      dplyr::mutate(df, valid_dttm = .data[["valid_dttm"]] + offset),
      dplyr::all_of(c("valid_dttm", "SID")),
      temp = dplyr::all_of(to_col)
    ),
    by = c("valid_dttm", "SID")
  )
  dplyr::select(
    dplyr::mutate(
      df,
      dplyr::across(
        dplyr::all_of(to_col),
        ~dplyr::case_when(
          is.na(.x) & !is.na(.data[[from_col]]) & !is.na(.data[["temp"]]) ~
            round(.data[[from_col]] - .data[["temp"]], 1),
          .default = .x
        )
      ),
    ),
    -dplyr::all_of("temp")
  )
}

