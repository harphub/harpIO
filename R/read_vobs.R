#' Read data from a vobs file.
#'
#' \code{read_vobs} returns the content of a named vobs file as a list of
#' data frames - one for synop data and one for temp (upper air) data.
#'
#' @param file_name Name of the vobs file.
#' @param missing_value Missing value indicator in vobs file.
#' @param ... Not used. Absorbs unused arguments.
#'
#' @return A list with data frames for synop and temp data.
#' @export
#'
#' @examples
#'
read_vobs <- function(file_name, missing_value = -99, ...) {

  empty_data <- tibble::tibble(
    SID  = NA_real_,
    lat  = NA_real_,
    lon  = NA_real_,
    elev = NA_real_
  )

  if (file.exists(file_name)) {
    message("Reading: ", file_name)
  } else {
    warning("File not found: ", file_name, call. = FALSE, immediate. = TRUE)
    return(list(synop = empty_data, temp = empty_data))
  }

  vobs_data <- readr::read_lines(file_name) %>%
    stringr::str_trim(side = "both") %>%
    strsplit("\\s+")

### SYNOP DATA

# The first row is num_synop, num_temp, vobs_version
  num_synop    <- as.numeric(vobs_data[[1]][1])
  num_temp     <- as.numeric(vobs_data[[1]][2])
  vobs_version <- as.numeric(vobs_data[[1]][3])

# The second row is the number of parameters
  num_param <- as.numeric(vobs_data[[2]])

  synop_start_row <- 3 + num_param
  synop_end_row   <- synop_start_row + num_synop - 1

# The following num_param rows are parameter, accum_hours
  params <- t(dplyr::bind_cols(vobs_data[3:(synop_start_row - 1)])) %>%
    tibble::as_tibble()
  colnames(params) <- c("parameter", "accum_hours")
  params <- params %>%
    dplyr::mutate(
      parameter   = purrr::map(.data$parameter, parse_v_parameter_synop),
      accum_hours = as.numeric(.data$accum_hours)
    ) %>%
    dplyr::mutate(
      units     = purrr::map_chr(.data$parameter, "param_units"),
      parameter = purrr::map_chr(.data$parameter, "harp_param")
    )
# In vobs PE can stupidly be 12h precipitation so is converted to Pcp - go figure!
  params <- params %>%
    dplyr::mutate(
      parameter = dplyr::case_when(
        .data$parameter == "Pcp" ~ "AccPcp12h",
        TRUE                     ~ .data$parameter
      ),
      accum_hours = dplyr::case_when(
        .data$parameter == "Pcp" ~ 12,
        TRUE                     ~ .data$accum_hours
      )
    )

# The next num_synop rows are the synop data
  synop_data <- t(dplyr::bind_cols(vobs_data[synop_start_row:synop_end_row])) %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(as.numeric)
  colnames(synop_data) <- c("SID", "lat", "lon", "elev", params$parameter)
  synop_data$SID <- as.integer(synop_data$SID)
  synop_data[synop_data == missing_value] <- NA


### TEMP DATA

# The following two rows are the temp metadata
  num_temp_levels <- as.numeric(vobs_data[[(synop_end_row + 1)]])
  num_param       <- as.numeric(vobs_data[[(synop_end_row + 2)]])
  temp_start_row  <- synop_end_row + 3 + num_param

# The following num_param rows are parameter, accum_hours
  params <- t(dplyr::bind_cols(vobs_data[(synop_end_row + 3):(temp_start_row - 1)])) %>%
    tibble::as.tibble()
  colnames(params) <- c("parameter", "accum_hours")
  params <- params %>%
    dplyr::mutate(
      parameter   = purrr::map(.data$parameter, parse_v_parameter_temp),
      accum_hours = as.numeric(.data$accum_hours)
    ) %>%
    dplyr::mutate(
      units     = purrr::map_chr(.data$parameter, "param_units"),
      parameter = purrr::map_chr(.data$parameter, "harp_param")
    )

# Loop over the temp stations
  temp_data <- list()
  if (num_temp < 1) {

    temp_data <- empty_data

  } else {

    for (temp_station in 1:num_temp) {

      temp_data[[temp_station]] <- tibble::tibble(
        SID  = rep(vobs_data[[temp_start_row]][1], num_temp_levels),
        lat  = rep(vobs_data[[temp_start_row]][2], num_temp_levels),
        lon  = rep(vobs_data[[temp_start_row]][3], num_temp_levels),
        elev = rep(vobs_data[[temp_start_row]][4], num_temp_levels)
      )

      temp_values <- t(
        dplyr::bind_cols(
          vobs_data[(temp_start_row + 1):(temp_start_row + num_temp_levels)]
        )
      ) %>%
        tibble::as_tibble()
      colnames(temp_values) <- params$parameter

      temp_data[[temp_station]] <- temp_data[[temp_station]] %>%
        dplyr::bind_cols(temp_values)

      temp_start_row <- temp_start_row + num_temp_levels + 1
    }

    temp_data <- dplyr::bind_rows(temp_data) %>%
      dplyr::mutate_all(as.numeric) %>%
      dplyr::mutate(SID = as.integer(.data$SID))

    temp_data[temp_data == missing_value] <- NA

  }

  list(synop = synop_data, temp = temp_data)
}
