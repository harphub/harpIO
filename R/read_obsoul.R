###
# Internal function to read obsoul data
###

# Note this function only works for synop data. For other obseravtion types
# Some new tidying functions will need to be written

read_obsoul <- function(
  file_name,
  param_defs = getExportedValue("harpIO", "harp_params"),
  ...
) {


  if (file.exists(file_name)) {
    message("Reading: ", file_name)
  } else {
    warning("File not found: ", file_name, call. = FALSE, immediate. = TRUE)
    return(list(synop = NULL))
  }

  # Get obsoul parameters from parameter definitions
  param_defs <- param_defs[
    sapply(param_defs, function(x) is.element("obsoul", names(x)))
  ]



  file_connection <- file(file_name, "r")
  on.exit(close(file_connection))

  # First line is date and time - time should by hms with leading zeroes
  # to make up a 6 character string
  date_time_check    <- scan(file_connection, nlines = 1, quiet = TRUE)
  date_time_check[2] <- formatC(date_time_check[2], width = 6, flag = "0")
  date_time_check    <- paste(date_time_check, collapse = "")

  # Read the data and check for problems
  obs_df <- try(read.table(file_connection, fill = TRUE), silent = TRUE)

  if (inherits(obs_df, "try-error")) {
    warning("Cannot read ", file_name, ".", call. = FALSE, immediate. = TRUE)
    return(list(synop = NULL))
  }

  if (ncol(obs_df) < 17) {
    warning("Cannot read ", file_name, ".", call. = FALSE, immediate. = TRUE)
    return(list(synop = NULL))
  }

  max_obs <- (ncol(obs_df) - 12) / 5

  # Add column names
  colnames(obs_df) <- obsoul_cols(max_obs)

  # Get the observation type based on the code
  obs_df <- dplyr::mutate(
    obs_df,
    type = dplyr::case_when(
      substr(.data[["xx"]],7,11) == 14  ~ "synop",
      substr(.data[["xx"]],7,11) == 24  ~ "ship",
      .data[["type"]] == 1   ~ "synop",
      .data[["type"]] == 2   ~ "airep",
      .data[["type"]] == 3   ~ "satob",
      .data[["type"]] == 4   ~ "dribu",
      .data[["type"]] == 5   ~ "temp",
      .data[["type"]] == 6   ~ "pilot",
      .data[["type"]] == 7   ~ "satem",
      .data[["type"]] == 8   ~ "paob",
      .data[["type"]] == 9   ~ "scatt",
      .data[["type"]] == 10  ~ "limb",
      .data[["type"]] == 13  ~ "radar",
      TRUE                   ~ "unknown"
    )
  )

  # Split the data by type

  obs_df <- split(obs_df, obs_df[["type"]])

  # Only works on synop data for now - will need other tidying functions
  # for other obs types
  if (!is.null(obs_df[["synop"]])) {
    synop <- tidy_obsoul_synop(
      obs_df[["synop"]], param_defs, max_obs
    )
  } else {
    synop = list(synop = NULL)
  }

  c(synop)

}

###
# Function to tidy synop data
###
tidy_obsoul_synop <- function(synop_df, param_defs, max_obs) {

  # Modify SID depending on country, set valid_dttm in unix time
  # and convert parameter codes to names
  synop_df <- dplyr::mutate(
    synop_df,
    SID  = modify_sid(.data[["SID"]]),
    valid_dttm = suppressMessages(
      harpCore::as_unixtime(
        paste0(
          .data[["date"]],
          formatC(as.integer(.data[["hms"]]), width = 6, flag = "0")
        )
      )
    ),
    dplyr::across(
      dplyr::contains("_code"), obsoul_param_code_to_name, param_defs
    )
  )

  # Gather all observations sections into common columns
  synop_df <- lapply(
    1:max_obs,
    function(x) dplyr::select(
      synop_df,
      !dplyr::matches("^obs[[:digit:]]+"),
      dplyr::starts_with(paste0("obs", x))
    ) %>%
      dplyr::rename_with(
        ~gsub("^obs[[:digit:]]+", "obs", .x)
      )
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::select(
      .data[["SID"]],
      .data[["lat"]],
      .data[["lon"]],
      .data[["elev"]],
      .data[["valid_dttm"]],
      dplyr::starts_with("obs_")
    ) %>%
    dplyr::filter(!is.na(.data[["obs_code"]]))

  # Pull out the correct columns for the observations
  synop_df <- dplyr::bind_rows(
    dplyr::transmute(
      dplyr::filter(synop_df, .data[["obs_code"]] != "wind"),
      .data[["SID"]],
      .data[["lat"]],
      .data[["lon"]],
      .data[["elev"]],
      .data[["valid_dttm"]],
      param = .data[["obs_code"]],
      obs   = dplyr::case_when(
        .data[["param"]] == "Pmsl" ~ .data[["obs_1"]] * -1,
        TRUE                       ~ .data[["obs_3"]]
      )
    ),
    tidyr::pivot_longer(
      dplyr::rename(
        dplyr::filter(synop_df, .data[["obs_code"]] == "wind"),
        S10m = .data[["obs_2"]],
        D10m = .data[["obs_3"]]
      ),
      cols = c(dplyr::all_of(c("S10m", "D10m"))),
      names_to  = "param",
      values_to = "obs"
    ) %>%
      dplyr::select(!dplyr::starts_with("obs_"))
  )

  # The Pmsl is only Pmsl if it's positive - otherwise it's -surface
  # pressure and we don't want that for now... (probably)
  synop_df <- dplyr::filter(
    synop_df,
    !(.data[["param"]] == "Pmsl" & .data[["obs"]] < 0)
  )

  # Pivot the observations to their own columns and generate
  # the units data frame
  params <- unique(synop_df[["param"]])

  list(
    synop = tidyr::pivot_wider(
      synop_df,
      names_from  = .data[["param"]],
      values_from = .data[["obs"]]
    ),
    synop_params = obsoul_params(params, param_defs)
  )

}

###
# Function to set column names for obsoul data
###
obsoul_cols <- function(max_obs) {
  col_names <- c(
    "num_col",
    "type",
    "xx",
    "lat",
    "lon",
    "SID",
    "date",
    "hms",
    "elev",
    "num_obs",
    "xx1",
    "xx2"
  )

  obs_cols <- unlist(
    lapply(
      1:max_obs,
      function(x) paste0("obs", x, "_", c("code", "1", "2", "3", "end"))
    )
  )

  c(col_names, obs_cols)

}

###
# Function to get the parameter name from the parameter code
# Unknown codes get NA
###
obsoul_param_code_to_name <- function(x, param_defs) {
  param_df <- dplyr::distinct(
    tibble::tibble(
      name = purrr::map_chr(
        param_defs,
        ~ifelse(
          is.null(.x[["obsoul"]][["common_name"]]),
          .x[["obsoul"]][["harp_name"]],
          .x[["obsoul"]][["common_name"]]
        )
      ),
      code = sapply(
        param_defs,
        function(x) x[["obsoul"]][["name"]])
    )
  )

  dplyr::pull(
    dplyr::left_join(
      tibble::tibble(code = x),
      param_df,
      by = "code"
    ),
    .data[["name"]]
  )

}

###
# Function to add a country indicator to site IDs
modify_sid <- function(x) {

  x <- gsub("^AT","90",
       gsub("^CR","91",
       gsub("^CZ","92",
       gsub("^HU","93",
       gsub("^PL","94",
       gsub("^RO","95",
       gsub("^SI","96",
       gsub("^SK","97", x ))))))))

       as.numeric(x)

}

###
# Function to generate the data frame for parameter units
###
obsoul_params <- function(params, param_defs) {

  param_names <-  sapply(
    param_defs,
    function(x) x[["obsoul"]][["harp_name"]],
    USE.NAMES = FALSE
  )

  param_units <- sapply(
    param_defs,
    function(x) x[["obsoul"]][["units"]],
    USE.NAMES = FALSE
  )

  param_df <- tibble::tibble(
    parameter   = param_names,
    accum_hours = rep(0, length(param_names)),
    units       = param_units
  )

  dplyr::filter(
    param_df,
    .data[["parameter"]] %in% params
  )

}

