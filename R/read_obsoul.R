read_obsoul <- function(file_name,country,missing_value = -99, ...) {

  empty_data <- empty_data_interpolate(NA, NA, empty_type = "obs")

  if (file.exists(file_name)) {
    message("Reading: ", file_name)
  } else {
    warning("File not found: ", file_name, call. = FALSE, immediate. = TRUE)
    return(list(obsoul = empty_data))
  }

  v_data <- read_obfile(file_name, v_type = "obsoul",country=country)
  if (is.null(v_data)) {
    return(list(obsoul = empty_data))
  }

  v_data

}
