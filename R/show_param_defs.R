#' @rdname parameter_definitions
#' @param file_format If given, the parameter names that exist in
#'   \code{param_defs} for the file format are shown.
#'
#' @export
#' @examples
#' show_param_defs()
#' show_param_defs("grib")
#' show_param_defs("netcdf")
show_param_defs <- function(
  file_format = NULL, param_defs = get("harp_params")
) {
  if (is.null(file_format)) {
    params <- dplyr::arrange(
      tibble::tibble(
        name = names(param_defs),
        description = sapply(param_defs, function(x) x[["description"]])
      ),
      .data[["name"]]
    )

  } else {

    param_elements <- purrr::map(param_defs, file_format)

    params <- param_elements %>%
      purrr::map_lgl(~!is.null(.x)) %>%
      select_elements(param_elements)

    if (length(params) < 1) {
      stop("Nothing found for file format: ", file_format, call. = FALSE)
    }

    if (file_format == "fa") {
      stop("Doesn't work FA params yet", call. = FALSE)
    }

    params <- dplyr::arrange(
      tibble::tibble(
        name     = names(params),
        fmt_name = vapply(
          params,
          function(x) gsub("\"", "", deparse(x[["name"]])),
          "a",
          USE.NAMES = FALSE
        )
      ),
      .data[["name"]]
    )
    colnames(params)[colnames(params) == "fmt_name"] <- paste0(
      file_format, "_name"
    )
  }

  print(params, n = Inf)

}
