#
#' Title
#'
#' @param data
#' @param filename
#' @param tablename
#' @param primary_key
#'
#' @return
#' @export
#'
#' @examples
write_fctable_to_sqlite <- function(data, filename, tablename = "FC", primary_key = "SID, fcdate, leadtime") {

  newfile <- FALSE
  if (!file.exists(filename)) {
    newfile <- TRUE
    if (!dir.exists(dirname(filename))) dir.create(dirname(filename), recursive = TRUE, mode = "0750")
  }

  data <- data %>%
    tidyr::spread(.data$member, .data$forecast)

  column_names  <- colnames(data)
  num_int_cols  <- which(!stringr::str_detect(column_names, "mbr")) %>% length()
  num_fcst_cols <- which(stringr::str_detect(column_names, "mbr")) %>% length()
  column_types  <- c(rep("INTEGER", num_int_cols), rep("REAL", num_fcst_cols))

  message("Writing to: ", filename, "\n")
  sqlite_DB <- dbopen(filename)
  if (newfile) {
    dbquery(
      sqlite_DB,
      paste0("CREATE TABLE ", tablename, "(",
        paste(column_names, column_types, collapse = ", "),
        ", PRIMARY KEY(",primary_key,"))")
    )
  }

  dbwrite(sqlite_DB, tablename, data)
  dbclose(sqlite_DB)

}
