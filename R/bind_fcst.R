#' Bind deterministic forecasts into a single data frame
#'
#' Given a harp_fcst list, \code{bind_fcst} binds the data frames in the
#' harp_fcst list into a single data frame for easy plotting. For ensemble
#' forecasts, the member data are gathered into a single column.
#'
#' @param .fcst A harp_fcst list
#'
#' @return A data frame
#' @export
#'
#' @examples
bind_fcst <- function(.fcst) {
  stopifnot(is.list(.fcst))
  stopifnot(!is.null(names(.fcst)))
  stopifnot(inherits(.fcst, "harp_fcst"))

  rename_func <- function(list_name) {
    if (any(grepl("_det$", colnames(.fcst[[list_name]])))) {
      fcst_col <- rlang::sym(paste0(list_name, "_det"))
      .fcst[[list_name]] <- dplyr::rename(
        .fcst[[list_name]],
        forecast = !! fcst_col
      )
    } else {
      .fcst <- harpPoint::gather_members(.fcst)
    }
    .fcst[[list_name]] <- dplyr::mutate(.fcst[[list_name]], mname = list_name) %>%
      dplyr::select(.data$mname, dplyr::everything())
  }

  fcst_names <- names(.fcst)
  dplyr::bind_rows(lapply(fcst_names, rename_func))
}
