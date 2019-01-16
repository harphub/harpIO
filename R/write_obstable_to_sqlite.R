# Internal harpIO function to write observations to sqlite file. Called by read_obs_convert.

write_obstable_to_sqlite <- function(
  obs_data,
  file_name,
  table_name  = "SYNOP",
  primary_key = c("SID", "validdate")
) {

  newfile <- FALSE
  if (!file.exists(file_name)) {
    newfile <- TRUE
    if (!dir.exists(dirname(file_name))) dir.create(dirname(file_name), recursive = TRUE, mode = "0750")
    message("\n***\nNew SQLITE obs file created: ", file_name, "\n***\n")
  }

  col_names    <- colnames(obs_data)
  meta_cols    <- c(primary_key, "lat", "lon", "elev")
  data_cols    <- setdiff(col_names, meta_cols)
  integer_cols <- primary_key
  real_cols    <- setdiff(col_names, integer_cols)
  col_types    <- c(rep("INTEGER", length(integer_cols)), rep("REAL", length(real_cols)))

  message("Writing to: ", table_name, " in ", file_name, "\n")

  sqlite_db <- dbopen(file_name)
  dbquery(sqlite_db, "PRAGMA synchronous = NORMAL")

  create_table <- function() {
    dbquery(
      sqlite_db,
      paste0("CREATE TABLE ", table_name, "(",
        paste(col_names, col_types, collapse = ", "),
        ", PRIMARY KEY(", paste(primary_key, collapse = ","), "))")
    )
  }

  if (newfile) {

    dbquery(sqlite_db, "PRAGMA journal_mode = WAL")
    create_table()

  } else if (!DBI::dbExistsTable(sqlite_db, table_name)) {

    create_table()

  } else {

    # Add columns in data to the db schema if they do not exist there
    cols_db      <- dbquery(sqlite_db, paste0("PRAGMA table_info(", table_name, ")"))
    cols_db_name <- cols_db$name

    new_cols <- setdiff(col_names, cols_db_name)

    if (length(new_cols) > 0) {
      db.add.columns(sqlite_db, table_name, new_cols)
      cols_db_name <- c(cols_db_name, new_cols)
    }

    # Set columns that exist in db schema, but not in the data to NA
    missing_cols <- setdiff(cols_db_name, col_names)
    for (missing_col in missing_cols) {
      obs_data[[missing_col]] <- NA
    }
    obs_data <- obs_data[cols_db_name]

  }

  dbwrite(sqlite_db, table_name, obs_data)

  dbclose(sqlite_db)

}
