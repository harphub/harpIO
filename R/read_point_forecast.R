#' Read point forecasts from an sqlite file
#'
#' Reads point forecasts from an sqlite file. The forecast date column is
#' expected to be called fcdate. Default start and end dates are set a long time
#' in the past and future respectively so that if no dates are passed all data
#' should be read.
#'
#' @param fcst_files A vector of filenames
#' @param start_date Start date to read from. Should be numeric or character
#'   YYYYMMDD(HH)(mm)
#' @param end_date End date to read to. Should be numeric or character
#'   YYYYMMDD(HH)(mm)
#' @param gather_data Whether to convert the data from wide form to long form
#'   (TRUE / FALSE). For use in harpPoint long form is required so the default
#'   is TRUE.
#'
#' @return A data frame
#' @export
#'
#' @examples
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
      warning("\nFile not found: ", db_file)
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
    if (any(grepl("_mbr", names(fcst[[list_counter]])))) {
      l_gather_data <- gather_data
    } else {
      l_gather_data <- FALSE
      names(fcst[[list_counter]][ncol(fcst[[list_counter]])]) <- "forecast"
    }

    if (l_gather_data) {
      fcst[[list_counter]] <- fcst[[list_counter]] %>%
        tidyr::gather(
          dplyr::contains("mbr"),
          key = "member",
          value = "forecast"
        ) %>%
        dplyr::group_by(member) %>%
        tidyr::nest() %>%
        tidyr::separate(member, "_(?=[^_]*$)",
          into = c("mname", "member"),
          remove = TRUE
        ) %>%
        tidyr::unnest(data)
    }

    DBI::dbDisconnect(fcst_db)
    message (" ---> DONE\n")

  }

  if (missing_counter > 0) {
    warning("There were ", missing_counter, " files not found \n")
    missing_files <- missing_files %>%
      unlist() %>%
      tibble::as_tibble()
  } else {
    missing_files <- NULL
  }

  fcst = dplyr::bind_rows(fcst)
  attr(fcst, "missing_files") <- missing_files
  if (l_gather_data) {
    attr(fcst, "dataframe_format") <- "long"
  } else {
    attr(fcst, "dataframe_format") <- "wide"
  }

  fcst
}
