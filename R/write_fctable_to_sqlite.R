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
  primary_key       = c("fcst_dttm", "lead_time", "SID"),
  synchronous       = "off",
  journal_mode      = "delete",
  dir_mode          = "0750",
  remove_model_elev = FALSE,
  ...
) {

  if (all(is.na(data[["forecast"]]))) return()

  data <- check_level(data)

  if (!remove_model_elev && member_elev_diff(data)) {
    stop(
      "Problem writing to: \n\"", filename, "\"\n",
      "`model_elevation` is not the same for all members. ",
      "\nSet `remove_model_elev = TRUE` in sqlite_opts()",
      "to write data without model elevation.",
      call. = FALSE
    )
  }

  newfile <- FALSE
  if (!file.exists(filename)) {
    newfile <- TRUE
    if (!dir.exists(dirname(filename))) dir.create(dirname(filename), recursive = TRUE, mode = dir_mode)
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

  sqlite_db <- dbopen(filename)

  # Make sure column names in data match those in the file - handling the
  # update of column names in v0.2 -
  # fcdate    -> fcst_dttm
  # validdate -> valid_dttm
  # leadtime  -> lead_time
  if (!newfile) {
    db_col_names <- colnames(dplyr::tbl(sqlite_db, tablename))

    old_fcdate    <- is.element("fcdate", db_col_names)
    old_validdate <- is.element("validdate", db_col_names)
    old_leadtime  <- is.element("leadtime", db_col_names)

    new_fcst_dttm  <- is.element("fcst_dttm", colnames(data))
    new_valid_dttm <- is.element("valid_dttm", colnames(data))
    new_lead_time  <- is.element("lead_time", colnames(data))

    if (old_fcdate && new_fcst_dttm) {
      colnames(data)[colnames(data) == "fcst_dttm"] <- "fcdate"
      primary_key[primary_key == "fcst_dttm"] <- "fcdate"
    }

    if (old_validdate && new_valid_dttm) {
      colnames(data)[colnames(data) == "valid_dttm"] <- "validdate"
      primary_key[primary_key == "valid_dttm"] <- "validdate"
    }

    if (old_leadtime && new_lead_time) {
      colnames(data)[colnames(data) == "lead_time"] <- "leadtime"
      primary_key[primary_key == "lead_time"] <- "leadtime"
    }

  }

  column_names  <- colnames(data)

  message("Opening connection to: ", filename)

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
    dplyr::filter(
      .data[["SID"]] != -999, .data[["lat"]] != -999, .data[["lon"]] != -999
    )

  primary_key <- intersect(primary_key, colnames(data))

  vertical_cols <- intersect(c("p", "ml", "z", "level"), colnames(data))
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
    level_type <- as.character(stats::na.omit(unique(df[["level_type"]])))
    if (length(level_type) != 1) {
      stop(
        "Cannot have more than 1 vertical coordinate in an output sqlite file. ",
        "Perhaps you need {parameter} to be part of the file name template.",
        call. = FALSE
      )
    }
    if (!level_type %in% c("pressure", "model", "height")) {
      if (length(unique(df[[level_type]])) < 2) {
        return(df[!colnames(df) %in% c("level_type", "level")])
      }
      warning(
        "Unknown vertical coordinate: ", level_type, "\n",
        "Setting column name to 'level'.",
        call. = FALSE
      )
    }
    col_name <- switch(
      as.character(level_type),
      "100"               = ,
      "isobaricInhPa"     = ,
      "pressure"          = "p",
      "hybrid"            = ,
      "model"             = "ml",
      "heightAboveGround" = ,
      "height"            = "z",
      as.character(level_type)
    )
    df <- replace_colname(df, "level", col_name)
    df <- df[!colnames(df) %in% c("level_type", "level")]
  }
  df
}

member_elev_diff <- function(df) {

  if (!is.element("model_elevation", colnames(df))) {
    return(FALSE)
  }

  num_elevs <- dplyr::group_by(
    df, .data[["fcst_dttm"]], .data[["lead_time"]], .data[["SID"]]
  ) %>%
    dplyr::summarise(ll = length(unique(.data[["model_elevation"]]))) %>%
    dplyr::pull(dplyr::all_of("ll"))

  any(num_elevs > 1)
}
