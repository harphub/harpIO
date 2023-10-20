# This function needs more work before it can be exported
# #' Title
# #'
# #' @param xsection
# #' @param psfc_param
# #' @param file_path
# #' @param file_template
# #' @param file_format
# #' @param file_format_opts
# #' @param show_progress
# #'
# #' @return
# #' @export
# #'
# #' @examples
join_psfc_to_xsection <- function(
  xsection,
  psfc_param       = "ps",
  fcst_model       = "psfc",
  file_path        = getwd(),
  file_template    = "harmonie_grib",
  file_format      = NULL,
  file_format_opts = list(),
  show_progress    = FALSE
) {
  UseMethod("join_psfc_to_xsection")
}

# #' @export
join_psfc_to_xsection.harp_xs_df <- function(
  xsection,
  psfc_param       = "ps",
  fcst_model       = "psfc",
  file_path        = getwd(),
  file_template    = "harmonie_grib",
  file_format      = NULL,
  file_format_opts = list(),
  show_progress    = FALSE
) {

  opts <- attr(xsection, "transformation_opts")

  members <- NULL

  if (length(grep("_mbr[[:digit:]]{3}", colnames(xsection))) > 0) {
    xsection <- harpCore::pivot_members(xsection)
    members <- unique(as.numeric(gsub("[[:alpha:]]", "", xsection$member)))
  }

  date_times <- unixtime_to_str_datetime(
    as.numeric(range(xsection[["fcdate"]])), YMDhms
  )

  lead_times <- unique(xsection[["lead_time"]])

  time_res <- min(diff(xsection[["fcdate"]]))
  time_res <- paste0(time_res, substring(attr(time_res, "units"), 1, 1))

  psfc <- read_forecast(
    date_times[1],
    date_times[2],
    fcst_model,
    psfc_param,
    lead_time           = lead_times,
    by                  = time_res,
    members             = members,
    file_path           = file_path,
    file_format         = file_format,
    file_template       = file_template,
    file_format_opts    = file_format_opts,
    transformation      = "xsection",
    transformation_opts = opts,
    return_data         = TRUE
  )[[1]]

  psfc <- dplyr::mutate(
    psfc,
    dplyr::across(
      dplyr::starts_with(paste0(fcst_model, "_")),
      ~structure(
          purrr::map(
            .x,
            ~dplyr::transmute(.x, .data[["distance"]], psfc = .data[["value"]])
          ),
        class = c("harp_xs_list", class(.x))
      )
    )
  ) %>%
    dplyr::select(
      -.data[["parameter"]], -.data[["units"]], -.data[["level_type"]]
    )

  if (!is.null(members)) {
    psfc <- harpCore::pivot_members(psfc)
  }

  xsection_col <- colnames(xsection)[sapply(
    xsection, inherits, "harp_xs_list"
  )]

  if (length(xsection_col) != 1) {
    stop(
      "Something went wrong. There are ", length(xsection_col), " xs columns."
    )
  }

  psfc_col <- grep(
    "_det$|^forecast$|xsection_data", colnames(xsection), value = TRUE
  )

  if (length(psfc_col) != 1) {
    stop(
      "Something went wrong. There are ", length(psfc_col), " xs columns."
    )
  }

  psfc_col_name <- "psfc_xs"

  colnames(psfc)[colnames(psfc) == psfc_col] <- psfc_col_name

  psfc_col <- psfc_col_name

  xsection <- dplyr::inner_join(
    xsection, psfc, by = intersect(colnames(xsection), colnames(psfc))
  ) %>%
    dplyr::mutate(
      !!rlang::sym(xsection_col) := purrr::map2(
        .data[[xsection_col]],
        .data[[psfc_col]],
        dplyr::inner_join,
        by = "distance"
      )
    ) %>%
    dplyr::select(-.data[[psfc_col]])

  if (!inherits(xsection[[xsection_col]], "harp_xs_list")) {
    class(xsection[[xsection_col]]) <- c(
      "harp_xs_list", class(xsection[[xsection_col]])
    )
  }

  dplyr::rename(xsection, xsection_data = .data[[xsection_col]])

}

# #' @export
join_psfc_to_xsection.harp_fcst <- function(
  xsection,
  psfc_param       = "ps",
  fcst_model       = "psfc",
  file_path        = getwd(),
  file_template    = "harmonie_grib",
  file_format      = NULL,
  file_format_opts = list(),
  show_progress    = FALSE
) {
  purrr::imap(
    xsection,
    ~join_psfc_to_xsection(
      .x,
      psfc_param       = psfc_param,
      fcst_model       = .y,
      file_path        = file_path,
      file_template    = file_template,
      file_format      = file_format,
      file_format_opts = file_format_opts,
      show_progress    = show_progress
    )
  ) %>%
    dplyr::bind_rows(.id = "fcst_model")
}
