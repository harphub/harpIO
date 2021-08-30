read_obs_convert <- function(
  start_date,
  end_date,
  by                   = "3h",
  obs_path             = ".",
  obs_format           = c("obsoul", "vobs"),
  obsfile_template     = c("obsoul", "vobs"),
  parameter            = NULL,
  sqlite_path          = NULL,
  sqlite_template      = "obstable",
  return_data          = NULL,
  country = NULL,
  iterations_per_write = 24,
  sqlite_synchronous   = c("off", "normal", "full", "extra"),
  sqlite_journal_mode  = c("delete", "truncate", "persist", "memory", "wal", "off"),
  ...
) {

  sqlite_synchronous  <- match.arg(sqlite_synchronous)
  sqlite_journal_mode <- match.arg(sqlite_journal_mode)

  all_dates <- seq_dates(start_date, end_date, by)

  all_dates <- split(all_dates, seq_along(all_dates) %/% iterations_per_write)

  start_date_list <- purrr::map(all_dates, 1)
  end_date_list   <- purrr::map(all_dates, ~ .x[length(.x)])

  num_iterations  <- length(start_date_list)

  if (return_data) {
    function_output <- list()
    list_counter    <- 0

  }

  sqlite_template <- get_template(sqlite_template)
 

  obsfile_template <- get_template(obsfile_template)
  

if (obs_format == "vobs"){
  for (i in 1:num_iterations) {

    if (return_data) list_counter <- list_counter + 1

    data_files <- get_filenames(
      file_path      = obs_path,
      start_date     = start_date_list[[i]],
      end_date       = end_date_list[[i]],
      by             = by,
      det_model      = NA_character_,
      parameter      = parameter,
      lead_time      = 0,
      file_template  = obsfile_template,
      filenames_only = FALSE
    )
 

    read_func <- get(paste("read", obs_format, sep = "_"))

    obs_data  <- data_files %>%
      dplyr::transmute(
        .data$fcdate,
        YYYY        = substr(.data$fcdate, 1, 4),
        MM          = substr(.data$fcdate, 5, 6),
        DD          = substr(.data$fcdate, 7, 8),
        HH          = substr(.data$fcdate, 9, 10),
        obs         = purrr::map(.data$file_name, read_func, ...),
        file_path   = ifelse(is.null(sqlite_path), NA, sqlite_path)
      )


      
    obs_data <- dplyr::mutate(
      obs_data,
      file_name = purrr::map_chr(
        purrr::transpose(obs_data),
        glue::glue_data,
        sqlite_template
      )
    )

    synop_data <- obs_data %>%
      dplyr::transmute(
        .data$file_name,
        validdate = suppressMessages(str_datetime_to_unixtime(.data$fcdate)),
        synop     = purrr::map(.data$obs, "synop")
      )
    if (tidyr_new_interface()) {
      synop_data <- synop_data %>%
        tidyr::unnest(tidyr::one_of("synop")) %>%
        tidyr::nest(synop = -tidyr::one_of("file_name"))
    } else {
      synop_data <- synop_data %>%
        tidyr::unnest() %>%
        dplyr::group_by(.data$file_name) %>%
        tidyr::nest(.key = "synop")
    }

    synop_params <- purrr::map_df(obs_data$obs, "synop_params") %>%
      dplyr::distinct()

    temp_data <- obs_data %>%
      dplyr::transmute(
        .data$file_name,
        validdate = suppressMessages(str_datetime_to_unixtime(.data$fcdate)),
        temp      = purrr::map(.data$obs, "temp")
      )
    if (tidyr_new_interface()) {
      temp_data <- temp_data %>%
        tidyr::unnest(tidyr::one_of("temp")) %>%
        tidyr::nest(temp = -tidyr::one_of("file_name"))
    } else {
      temp_data <- temp_data %>%
        tidyr::unnest() %>%
        dplyr::group_by(.data$file_name) %>%
        tidyr::nest(.key = "temp")
    }

    temp_params <- purrr::map_df(obs_data$obs, "temp_params") %>%
      dplyr::distinct()

    if (!is.null(sqlite_path)) {
      purrr::walk2(
        synop_data$synop,
        synop_data$file_name,
        write_obstable_to_sqlite,
        table_name   = "SYNOP",
        primary_key  = c("validdate", "SID"),
        params_table = synop_params,
        synchronous  = sqlite_synchronous,
        journal_mode = sqlite_journal_mode
      )
      purrr::walk2(
        temp_data$temp,
        temp_data$file_name,
        write_obstable_to_sqlite,
        table_name   = "TEMP",
        primary_key  = c("validdate", "SID", "p"),
        params_table = temp_params,
        synchronous  = sqlite_synchronous,
        journal_mode = sqlite_journal_mode
      )
    }

    if (return_data) {
      function_output[[i]] <- list(
        synop        = synop_data,
        temp         = temp_data,
        synop_params = synop_params,
        temp_params  = temp_params
      )
    }

  }

  if (return_data) {
    list(
      synop = purrr::map(function_output, "synop") %>%
        purrr::map(dplyr::pull, .data$synop) %>%
        purrr::flatten_dfr(),
      temp  = purrr::map(function_output, "temp") %>%
        purrr::map(dplyr::pull, .data$temp) %>%
        purrr::flatten_dfr(),
      synop_params = dplyr::distinct(purrr::map_df(function_output, "synop_params")),
      temp_params  = dplyr::distinct(purrr::map_df(function_output, "temp_params"))
    )
  }

}
else if (obs_format == "obsoul") {

for (i in 1:num_iterations) {

    if (return_data) list_counter <- list_counter + 1

    data_files <- get_filenames(
      file_path      = obs_path,
      start_date     = start_date_list[[i]],
      end_date       = end_date_list[[i]],
      by             = by,
      det_model      = NA_character_,
      parameter      = parameter,
      lead_time      = 0,
      file_template  = obsfile_template,
      country = country,
      filenames_only = FALSE
    )


    read_func <- get(paste("read", obs_format, sep = "_"))

    obs_data  <- data_files %>%
      dplyr::transmute(
        .data$fcdate,
        YYYY        = substr(.data$fcdate, 1, 4),
        MM          = substr(.data$fcdate, 5, 6),
        DD          = substr(.data$fcdate, 7, 8),
        HH          = substr(.data$fcdate, 9, 10),
        obs         = purrr::map2(.data$file_name,.data$country,read_func, ...),
        region = country,
        file_path   = ifelse(is.null(sqlite_path), NA, sqlite_path)
      )

    obs_data <- dplyr::mutate(
      obs_data,
      file_name = purrr::map_chr(
        purrr::transpose(obs_data),
        glue::glue_data,
        sqlite_template
      )
    )

    synop_data <- obs_data %>%
      dplyr::transmute(
        .data$file_name,
        validdate = suppressMessages(str_datetime_to_unixtime(.data$fcdate)),
        synop     = purrr::map(.data$obs, "synop")
      )
    if (tidyr_new_interface()) {
      synop_data <- synop_data %>%
        tidyr::unnest(tidyr::one_of("synop")) %>%
        tidyr::nest(synop = -tidyr::one_of("file_name"))
    } else {
      synop_data <- synop_data %>%
        tidyr::unnest() %>%
        dplyr::group_by(.data$file_name) %>%
        tidyr::nest(.key = "synop")
    }

    synop_params <- purrr::map_df(obs_data$obs, "synop_params") %>%
      dplyr::distinct()


    if (!is.null(sqlite_path)) {
      purrr::walk2(
        synop_data$synop,
        synop_data$file_name,
        write_obstable_to_sqlite,
        table_name   = "SYNOP",
        primary_key  = c("validdate", "SID"),
        params_table = synop_params,
        synchronous  = sqlite_synchronous,
        journal_mode = sqlite_journal_mode
      )

    }

    if (return_data) {
      function_output[[i]] <- list(
        synop        = synop_data,
        synop_params = synop_params
      )
    }

  }

  if (return_data) {
    list(
      synop = purrr::map(function_output, "synop") %>%
        purrr::map(dplyr::pull, .data$synop) %>%
        purrr::flatten_dfr(),
      synop_params = dplyr::distinct(purrr::map_df(function_output, "synop_params"))
    )
  }








}
}