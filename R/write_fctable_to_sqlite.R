#' Write a \code{harp_fcst} object to an sqlite file.
#'
#' This function should rarely be used as a standalone function. It is called by
#' \code{read_det_interpolate} and \code{read_eps_interpolate}.
#'
#' @param data The fcst data to write.
#' @param filename The name of the sqlite file to write the data to.
#' @param tablename The table in the sqlite file to write the data to.
#' @param primary_key The primary key to be used.
#'
#' @return This function only has a side effect (writing to sqlite file).
#' @export
#'
#' @examples
write_fctable_to_sqlite <- function(
  data,
  filename,
  tablename         = "FC",
  primary_key       = c("fcdate", "leadtime", "SID"),
  synchronous       = "off",
  journal_mode      = "delete",
  remove_model_elev = FALSE
) {

  newfile <- FALSE
  if (!file.exists(filename)) {
    newfile <- TRUE
    if (!dir.exists(dirname(filename))) dir.create(dirname(filename), recursive = TRUE, mode = "0750")
  }

  if (remove_model_elev && is.element("model_elevation", colnames(data))) {
    data <- data %>%
      dplyr::select(-.data$model_elevation)
  }

  data <- data %>%
    tidyr::spread(.data$member, .data$forecast)

  column_names  <- colnames(data)

  primary_key <- intersect(primary_key, column_names)

  message("Writing to: ", filename, "\n")

  sqlite_db <- dbopen(filename)


  dbquery(sqlite_db, paste("PRAGMA synchronous =", toupper(synchronous)))

  if (newfile) {

    dbquery(sqlite_db, paste("PRAGMA journal_mode =", toupper(journal_mode)))

    column_types <- DBI::dbDataType(sqlite_db, data)
    dbquery(
      sqlite_db,
      paste0("CREATE TABLE ", tablename, "(",
        paste(column_names, column_types, collapse = ", "),
        ")"
      )
    )
  }

  data <- dplyr::select_if(data, ~ !all(is.na(.))) %>%
    dplyr::filter(SID != -999, lat != -999, lon != -999)
  primary_key <- intersect(primary_key, colnames(data))
  db_clean_and_write(sqlite_db, tablename, data, primary_key, index_constraint = "unique")
  dbclose(sqlite_db)

}
