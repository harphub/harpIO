#' Read data from a vfld file
#'
#' \code{read_vfld} returns the content of a named vfld file as a named list of
#' data frames - synop for the the surface data (not necessarily from synop
#' stations, but just the content of the file), and temp for the upper air data.
#'
#' @param file_name Name of the vfld file
#'
#' @return A named list containing: \cr \code{synop}: the surface data, \cr
#'   \code{temp}: the upper air data.
#' @export
#'
#' @examples
#' my_dir <- "/lustre/storeB/users/andrewts/HarpResults/vfld"
#' vfld_file <- harp_get_filenames(
#'   my_dir,
#'   start_date = 2017052600,
#'   end_date = 2017052600,
#'   experiment = "MEPS_summer2017_sfcPertRef",
#'   lead_time = 20,
#'   member = 0,
#'   file_template = "vfld")
#' vfld_data <- read_vfld(vfld_file)
#'
read_vfld <- function(file_name) {

  vfld_data <- readr::read_lines(file_name) %>%
    stringr::str_trim(side = "both") %>%
    strsplit("\\s+")

# The first row is num_synop, num_temp, vfld_version
  num_synop    <- as.numeric(vfld_data[[1]][1])
  num_temp     <- as.numeric(vfld_data[[1]][2])
  vfld_version <- as.numeric(vfld_data[[1]][3])

# The second row is the number of parameters
  num_param <- as.numeric(vfld_data[[2]])

  synop_start_row <- 3 + num_param
  synop_end_row   <- synop_start_row + num_synop - 1

# The following num_param rows are parameter, accum_hours
  params <- t(data.frame(vfld_data[3:(synop_start_row - 1)])) %>%
    tibble::as.tibble()
  colnames(params) <- c("parameter", "accum_hours")
  params <- params %>%
    mutate(
      parameter   = purrr::map_chr(parameter, parse_v_parameter_synop),
      accum_hours = as.numeric(accum_hours)
    )

# The next num_synop rows are the synop data
  synop_data <- t(data.frame(vfld_data[synop_start_row:synop_end_row])) %>%
    tibble::as.tibble() %>%
    dplyr::mutate_all(as.numeric)
  colnames(synop_data) <- c("SID", "lat", "lon", params$parameter)
  synop_data$SID <- as.numeric(synop_data$SID)

# The following two rows are the temp metadata
  num_temp_levels <- as.numeric(vfld_data[[(synop_end_row + 1)]])
  num_param       <- as.numeric(vfld_data[[(synop_end_row + 2)]])
  temp_start_row  <- synop_end_row + 3 + num_param

# The following num_param rows are parameter, accum_hours
  params <- t(data.frame(vfld_data[(synop_end_row + 3):(temp_start_row - 1)])) %>%
    tibble::as.tibble()
  colnames(params) <- c("parameter", "accum_hours")
  params <- params %>%
    dplyr::mutate(
      parameter   = purrr::map_chr(parameter, parse_v_parameter_temp),
      accum_hours = as.numeric(accum_hours)
    )

# Loop over the temp stations
  temp_data <- list()
  for (temp_station in 1:num_temp) {

    temp_data[[temp_station]] <- tibble::tibble(
      SID   = rep(vfld_data[[temp_start_row]][1], num_temp_levels),
      lat   = rep(vfld_data[[temp_start_row]][2], num_temp_levels),
      lon   = rep(vfld_data[[temp_start_row]][3], num_temp_levels),
      melev = rep(vfld_data[[temp_start_row]][4], num_temp_levels)
    )

    temp_values <- t(
      data.frame(
        vfld_data[(temp_start_row + 1):(temp_start_row + num_temp_levels)]
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
    dplyr::mutate(SID = as.integer(SID))

  list(synop = synop_data, temp = temp_data)
}
