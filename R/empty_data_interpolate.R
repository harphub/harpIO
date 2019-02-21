# functions to create empty data for read_*_interpolate functions

empty_data_interpolate <- function(members, lead_time) {
  tibble::tibble(
    SID             = NA_real_,
    lat             = NA_real_,
    lon             = NA_real_,
    model_elevation = NA_real_,
    member          = members,
    lead_time       = lead_time
  )
}
