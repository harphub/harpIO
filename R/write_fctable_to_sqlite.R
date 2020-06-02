# Write a \code{harp_fcst} object to an sqlite file.
#
# This function should rarely be used as a standalone function. It is called by
# \code{read_det_interpolate} and \code{read_eps_interpolate}.
#
# @param data The fcst data to write.
# @param filename The name of the sqlite file to write the data to.
# @param tablename The table in the sqlite file to write the data to.
# @param primary_key The primary key to be used.
#
# @return This function only has a side effect (writing to sqlite file).
# NOT exported - used internally.
#
# @examples
write_fctable_to_sqlite <- function(
  data,
  filename,
  tablename         = "FC",
  primary_key       = c("fcdate", "leadtime", "SID"),
  synchronous       = "off",
  journal_mode      = "delete",
  remove_model_elev = FALSE
) {

  data <- check_level(data)

  newfile <- FALSE
  if (!file.exists(filename)) {
    newfile <- TRUE
    if (!dir.exists(dirname(filename))) dir.create(dirname(filename), recursive = TRUE, mode = "0750")
  }

  if (remove_model_elev && is.element("model_elevation", colnames(data))) {
    data <- data %>%
      dplyr::select(-.data$model_elevation)
  }

  missing_data <- dplyr::filter(data, is.na(.data$forecast)) %>%
    dplyr::distinct()
  if (nrow(missing_data) > 0) {
    data <- dplyr::bind_rows(
      dplyr::filter(data, !is.na(.data$forecast)),
      missing_data
    )
  }

  data <- data %>%
    tidyr::spread(.data$member, .data$forecast)

  column_names  <- colnames(data)

  primary_key <- intersect(primary_key, column_names)

  message("Opening connection to: ", filename)

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

  vertical_cols <- intersect(c("p", "ml", "z"), colnames(data))
  vertical_cols <- setdiff(vertical_cols, primary_key)
  if (length(vertical_cols) > 0) {
    message("Adding '", paste(vertical_cols, collapse = "','"), "' to index_cols.")
    primary_key <- c(primary_key, vertical_cols)
  }


  message("Writing data")
  if (nrow(data) > 0) {
    db_clean_and_write(sqlite_db, tablename, data, primary_key, index_constraint = "unique")
  }
  dbclose(sqlite_db)
  message("\n")

}

check_level <- function(df) {
  df_cols <- colnames(df)
  if (is.element("level_type", df_cols) && is.element("level", df_cols)) {
    level_type <- unique(df[["level_type"]])
    if (length(level_type) != 1) {
      stop(
        "Cannot have more than 1 vertical coordinate in an output sqlite file. ",
        "Perhaps you need {parameter} to be part of the file name template.",
        call. = FALSE
      )
    }
    if (!level_type %in% c("pressure", "model", "height")) {
      stop ("Unknown vertical coordinate: ", level_type, call. = FALSE)
    }
    col_name <- switch(
      level_type,
      "pressure" = "p",
      "model"    = "ml",
      "height"   = "z"
    )
    df <- replace_colname(df, "level", col_name)
    df <- df[!colnames(df) %in% c("level_type", "level")]
  }
  df
}
