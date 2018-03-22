read_point_forecast <- function(
  fcst_files,
  start_date  = 197001010000,
  end_date    = 300001010000,
  gather_data = TRUE
) {

  start_date <- str_datetime_to_unixtime(start_date)
  end_date   <- str_datetime_to_unixtime(end_date)

  fcst            <- list()
  list_counter    <- 0
  missing_files   <- list()
  missing_counter <- 0

  for (db_file in fcst_files) {

    if (file.exists(db_file)) {
      message("Reading: ", db_file)
    } else {
      warning("File not found: ", db_file)
      missing_counter <- missing_counter + 1
      missing_files[[missing_counter]] <- db_file
      next
    }

    list_counter <- list_counter + 1
    fcst_db <- DBI::dbConnect(RSQLite::SQLite(), db_file)
    fcst[[list_counter]] <- dplyr::tbl(fcst_db, "FC") %>%
      dplyr::filter(between(fcdate, start_date, end_date)) %>%
      dplyr::collect(n = Inf)
    fcst[[list_counter]] <- fcst[[list_counter]] %>% tidyr::drop_na()

    if (gather_data) {
      fcst[[list_counter]] <- fcst[[list_counter]] %>%
        tidyr::gather(
          dplyr::contains("mbr"),
          key = "member",
          value = "forecast"
        ) %>%
        dplyr::mutate(
          mname = stringr::str_sub(
            member,
            1,
            stringr::str_locate(member, "_mbr")[1] - 1
          )
        ) %>%
        dplyr::mutate(
          member = stringr::str_sub(
            member,
            stringr::str_locate(member, "_mbr")[1] + 1,
            stringr::str_length(member)
          )
        )
    }

    DBI::dbDisconnect(fcst_db)
    cat (" ---> DONE\n")

  }

  if (missing_counter > 0) {
    warning("There were ", missing_counter, " files not found \n")
    missing_files <- missing_files %>%
      unlist() %>%
      tibble::as_tibble()
  } else {
    missing_files <- NULL
  }

  list(fcst = dplyr::bind_rows(fcst), missing_files = missing_files)
}
