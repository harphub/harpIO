# harpIO internal function to read data from FCTABLE sqlite files. Called by
# read_point_forecast.

read_fctable <- function(
  db_files,
  start_date,
  end_date,
  lead_time = NULL,
  stations  = NULL,
  members   = NULL,
  param     = NULL
) {

  fcst_out   <- list()
  list_count <- 0

  for (db_file in db_files) {

    meta_cols  <- c("SID", "fcdate", "leadtime", "validdate")

    message("Reading: ", db_file)

    fcst_db   <- DBI::dbConnect(RSQLite::SQLite(), db_file, flags = RSQLite::SQLITE_RO, synchronous = NULL)

    fcst_cols <- DBI::dbListFields(fcst_db, "FC")
    if (is.element("parameter", fcst_cols)) {
      meta_cols <- c(meta_cols, "parameter")
    }
    if (is.element("units", fcst_cols)) {
      meta_cols <- c(meta_cols, "units")
    }
    data_cols <- setdiff(fcst_cols, meta_cols)

    list_count <- list_count + 1

    fcst       <- dplyr::tbl(fcst_db, "FC") %>%
      dplyr::filter(between(fcdate, start_date, end_date))

    if (!is.null(lead_time)) {
      fcst <- dplyr::filter(fcst, leadtime %in% lead_time)
    }

    if (!is.null(stations)) {
      fcst <- dplyr::filter(fcst, SID %in% stations)
    }

    if (!is.null(members)) {

      if (is.list(members)) {
        col_members <- purrr::map2(
          names(members),
          members,
          ~ paste(.x, formatC(.y, width = 3, flag = "0"), sep = "_mbr")
        ) %>%
          unlist()
      } else {
        col_members <- paste0("mbr", formatC(members, width = 3, flag = "0"))
        col_members <- fcst_cols[grep(paste(col_members, collapse = "|"), fcst_cols)]
      }

      col_names <- rlang::syms(c(meta_cols, col_members))
      fcst <- dplyr::select(fcst, !!! col_names)

    }

    fcst_out[[list_count]] <- dplyr::collect(fcst, n = Inf)

    DBI::dbDisconnect(fcst_db)

    if (!is.element("units", meta_cols)) {
      fcst_out[[list_count]] <- fcst_out[[list_count]] %>%
        dplyr::mutate(units = guess_units(fcst_out[[list_count]], param))
      meta_cols              <- c(meta_cols, "units")
      fcst_out[[list_count]] <- fcst_out[[list_count]][c(meta_cols, data_cols)]
    }

  }

  dplyr::bind_rows(fcst_out) %>%
    dplyr::mutate(
      fcst_cycle = substr(unixtime_to_str_datetime(.data$fcdate, YMDh), 9, 10)
    )

}
