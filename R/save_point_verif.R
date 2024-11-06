#' Save and read verification data
#'
#' `save_point_verif()` saves a verification object, as produced by one of
#'   harpPoint's verification functions, while `read_point_verif()` reads a
#'   saved verification object. The tempplate is very specific to point
#'   verification and generally should not be changed as harpVis's shiny app for
#'   point verification expects this template to be used. You should use the
#'   directory structure to add extra specification.
#'
#' @param verif_data A verification object produced by one of the harpPoint
#'   verification functions.
#' @param verif_path The path to save the data to or read the data from. For
#'   saving, the default is a directory called "Verification" inside the working
#'   directory.
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

  file_name <- generate_verif_filename(
    verif_path,
    start_date,
    end_date,
    parameter,
    model_names,
    verif_file_template,
    ...
  )

  if (!dir.exists(dirname(file_name))) {
    dir.create(dirname(file_name), recursive = TRUE, mode = dir_mode)
  }

  message("Saving point verification scores to: \n", file_name)

  saveRDS(verif_data, file = file_name)

}

#' @rdname save_point_verif
#' @param start_dttm,end_sttm The start and end date-time strings for which to
#'   read a verification file.
#' @param fcst_model A vector of the names of the forecast models for which to
#'   read a verification file.
#' @param parameter The parameter for which to read a verification file. This
#'   should match the parameter name used in the verification.
#'
#' @return An object of class harp_verif - usually a list of data frames with
#'   verification scores.
#' @export
#'
read_point_verif <- function(
  start_dttm,
  end_dttm,
  fcst_model,
  parameter,
  verif_path,
  verif_file_template = "{verif_path}/harpPointVerif.harp.{parameter}.harp.{start_date}-{end_date}.harp.{models}.rds",
  ...
) {

  file_name <- generate_verif_filename(
    verif_path,
    start_dttm,
    end_dttm,
    parameter,
    fcst_model,
    verif_file_template,
    ...
  )

  # Try truncating dttm if file not found
  found_file <- file.exists(file_name)

  if (!found_file) {
    start_dttm <- harpCore::as_str_dttm(harpCore::as_dttm(start_dttm))
    end_dttm   <- harpCore::as_str_dttm(harpCore::as_dttm(end_dttm))
  }

  file_name <- generate_verif_filename(
    verif_path,
    start_dttm,
    end_dttm,
    parameter,
    fcst_model,
    verif_file_template,
    ...
  )

  found_file <- file.exists(file_name)

  if (!found_file) {
    cli::cli_abort(c(
      "File not found!",
      "x" = paste("Cannot find file:", cli::col_br_red(file_name))
    ))
  }

  readRDS(file_name)
}

generate_verif_filename <- function(
  verif_path,
  start_date,
  end_date,
  parameter,
  model_names,
  template,
  ...
) {
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

  start_year  <- format(harpCore::as_dttm(start_date), "%Y")
  start_month <- format(harpCore::as_dttm(start_date), "%m")
  start_day   <- format(harpCore::as_dttm(start_date), "%d")
  start_week  <- format(harpCore::as_dttm(start_date), "%V")

  end_year  <- format(harpCore::as_dttm(end_date), "%Y")
  end_month <- format(harpCore::as_dttm(end_date), "%m")
  end_day   <- format(harpCore::as_dttm(end_date), "%d")
  end_week  <- format(harpCore::as_dttm(end_date), "%V")

  stringr::str_glue(template)

}
