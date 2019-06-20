# functions to create empty data for read_*_interpolate functions

empty_data_interpolate <- function(members, lead_time, empty_type = c("fcst", "obs")) {
  empty_type <- match.arg(empty_type)
  switch(
    empty_type,
    "fcst" = data.frame(
      SID              = NA_integer_,
      lat              = NA_real_,
      lon              = NA_real_,
      model_elevation  = NA_real_,
      member           = members,
      lead_time        = lead_time,
      stringsAsFactors = FALSE
    ),
    "obs" = data.frame(
      SID              = NA_integer_,
      lat              = NA_real_,
      lon              = NA_real_,
      elev             = NA_real_,
      stringsAsFactors = FALSE
    )
  )
}
