# Internal harpIO function to write observations to sqlite file. Called by read_obs_convert.

write_obstable_to_sqlite <- function(
  obs_data,
  file_name,
  table_name   = "SYNOP",
  primary_key  = c("valid_dttm", "SID"),
  params_table = NULL,
  synchronous  = "off",
  journal_mode = "delete",
  dir_mode     = "0750",
  ...
) {

  obs_data <- dplyr::filter(obs_data, !is.na(.data[["SID"]]))

  if (table_name == "TEMP" && is.element("p", colnames(obs_data))) {
    obs_data <- dplyr::filter(obs_data, !is.na(.data[["p"]]))
  }

  if (nrow(obs_data) == 0) return()

  newfile <- FALSE
  if (!file.exists(file_name)) {
    newfile <- TRUE
    if (!dir.exists(dirname(file_name))) dir.create(dirname(file_name), recursive = TRUE, mode = dir_mode)
    message("\n***\nNew SQLITE obs file created: ", file_name, "\n***\n")
  }

  sqlite_db <- dbopen(file_name)

  # Make sure column names in data match those in the file - handling the
  # update of column names in v0.2 -
  # validdate -> valid_dttm
  if (!newfile) {

    db_col_names <- colnames(dplyr::tbl(sqlite_db, table_name))

    old_validdate <- is.element("validdate", db_col_names)

    new_valid_dttm <- is.element("valid_dttm", colnames(obs_data))

    if (old_validdate && new_valid_dttm) {
      colnames(obs_data)[colnames(obs_data) == "valid_dttm"] <- "validdate"
      primary_key[primary_key == "valid_dttm"] <- "validdate"
    }

  }

  col_names    <- colnames(obs_data)
  meta_cols    <- c(primary_key, "lat", "lon", "elev")
  data_cols    <- setdiff(col_names, meta_cols)
  integer_cols <- primary_key
  real_cols    <- setdiff(col_names, integer_cols)
  col_types    <- c(rep("INTEGER", length(integer_cols)), rep("REAL", length(real_cols)))

  if (!is.null(params_table) && nrow(params_table) > 0) {
    params_cols  <- colnames(params_table)
    params_types <- toupper(unname(sapply(params_table, class)))
    params_types <- gsub("CHARACTER", "VARCHAR", params_types)
    params_types <- gsub("NUMERIC", "REAL", params_types)
  }

  message("Writing to: ", table_name, " in ", file_name, "\n")

  dbquery(sqlite_db, paste("PRAGMA synchronous =", toupper(synchronous)))

  create_obs_table <- function() {
    dbquery(
      sqlite_db,
      paste0("CREATE TABLE ", table_name, "(",
        paste(col_names, col_types, collapse = ", "),
        ")"
      )
    )
  }

  create_params_table <- function() {
    dbquery(
      sqlite_db,
      paste0("CREATE TABLE ", paste0(table_name, "_params"), "(",
        paste(params_cols, params_types, collapse = ", "),
        ")"
      )
    )
  }

  if (newfile) {

    dbquery(sqlite_db, paste("PRAGMA journal_mode =", toupper(journal_mode)))
    create_obs_table()
    if (!is.null(params_table) && nrow(params_table) > 0) {
      create_params_table()
      dbwrite(sqlite_db, paste0(table_name, "_params"), params_table)
    }

  } else if (!DBI::dbExistsTable(sqlite_db, table_name)) {

    create_obs_table()
    if (!is.null(params_table) && nrow(params_table) > 0) {
      create_params_table()
      dbwrite(sqlite_db, paste0(table_name, "_params"), params_table)
    }

  } else {

    # Add columns in data to the db schema if they do not exist there
    cols_db      <- dbquery(sqlite_db, paste0("PRAGMA table_info(", table_name, ")"))
    cols_db_name <- cols_db$name

    new_cols <- setdiff(col_names, cols_db_name)

    if (length(new_cols) > 0) {

      db.add.columns(sqlite_db, table_name, new_cols)
      cols_db_name <- c(cols_db_name, new_cols)

      if (!is.null(params_table) && nrow(params_table) > 0) {
        rows_to_add <- dplyr::filter(params_table, .data$parameter %in% new_cols)
        dbwrite(sqlite_db, paste0(table_name, "_params"), rows_to_add)
      }

    }

    # Set columns that exist in db schema, but not in the data to NA
    missing_cols <- setdiff(cols_db_name, col_names)
    for (missing_col in missing_cols) {
      obs_data[[missing_col]] <- NA
    }
    obs_data <- obs_data[cols_db_name]

  }

  db_clean_and_write(sqlite_db, table_name, obs_data, primary_key, index_constraint = "unique")

  dbclose(sqlite_db)

}
