#' Save verification data
#'
#' @param verif_data A verification object produced by one of the harpPoint
#'   verification functions.
#' @param verif_path The path to save the data to. The default is a directory
#'   called Verification inside the working directory.
#' @param verif_file_template A template for the verification data file. For
#'   consistency with the default shiny app to plot verification, this should not be changed.
#' @param ... Non default variables used in \code{verif_file_template}.
#'
#' @export
#'
#' @examples
save_point_verif <- function(
  verif_data,
  verif_path          = "./Verification",
  verif_file_template = "{verif_path}/harpPointVerif.harp.{parameter}.harp.{start_date}-{end_date}.harp.{models}.rds",
  ...
) {

  parameter  <- attr(verif_data, "parameter")
  start_date <- attr(verif_data, "start_date")
  end_date   <- attr(verif_data, "end_date")

  model_names <- unique(verif_data[[1]]$mname)

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
    dir.create(dirname(file_name), recursive = TRUE, mode = "0750")
  }

  message("Saving point verification scores to: \n", file_name)

  saveRDS(verif_data, file = file_name)

}
