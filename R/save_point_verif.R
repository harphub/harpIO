#' Save verification data
#'
#' @param verif_data A verification object produced by one of the harpPoint
#'   verification functions.
#' @param verif_path The path to save the data to. The default is a directory
#'   called Verification inside the working directory.
#' @param verif_file_template A template for the verification data file. For
#'   consistency with the default shiny app to plot verification, this should not be changed.
#' @param dir_mode The permissions mode to be used for creation of new
#'   directories on Unix-alike systems. The default is "0750".
#' @param ... Non default variables used in \code{verif_file_template}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   save_point_verif(verif_data)
#' }
save_point_verif <- function(
  verif_data,
  verif_path          = "./Verification",
  verif_file_template = "{verif_path}/harpPointVerif.harp.{parameter}.harp.{start_date}-{end_date}.harp.{models}.rds",
  dir_mode            = "0750",
  ...
) {

  parameter  <- attr(verif_data, "parameter")
  dttm       <- harpCore::as_str_dttm(
    sort(harpCore::as_dttm(range(attr(verif_data, "dttm"))))
  )
  start_date <- dttm[1]
  end_date   <- dttm[2]

  model_names <- unique(verif_data[[1]]$fcst_model)

  multi_models <- stringr::str_extract(model_names, "\\([[:graph:]]+\\)") %>%
    stringr::str_replace("\\(", "") %>%
    stringr::str_replace("\\)", "")

  models <- unique(c(model_names[is.na(multi_models)], multi_models[!is.na(multi_models)])) %>%
    paste(collapse = ".model.")

  dots <- list(...)
  if (length(dots) > 0) {
    for (dot_name in names(dots)) {
      assign(dot_name, dots[[dot_name]])
    }
  }

  file_name <- stringr::str_glue(verif_file_template)
  if (!dir.exists(dirname(file_name))) {
    dir.create(dirname(file_name), recursive = TRUE, mode = dir_mode)
  }

  message("Saving point verification scores to: \n", file_name)

  saveRDS(verif_data, file = file_name)

}
