
parse_by <- function(by = "3h") {
  # return the time step in seconds
  time_units <- stringr::str_extract(tolower(by), "[a-z]+")
  tstep <- readr::parse_number(by)
  tscale <- switch(time_units,
      "d" = 60 * 60 * 24,
      "h" = 60 * 60,
      "m" = 60,
      "s" = 1,
      NA_real_
    )
  tstep * tscale
}
