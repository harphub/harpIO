# #' List of Weather Stations
# #'
# #' A dataset containing site ID, latitude, longitude, elevation and name of
# #' 13417 weather stations world wide.
# #'
# #' @format A data frame with 13417 rows and 5 variables: \describe{
# #'   \item{SID}{station ID number} \item{lat}{latitude of the station, in
# #'   decimal degrees} \item{lon}{longitude of the station, in decimal degrees}
# #'   \item{elev}{elevation of the station, in metres} \item{name}{the name of
# #'   the station}}
# #'
# #' @source HIRLAM station list
# "station_list"
#
# #' Weather station IDs with regional groups
# #'
# #' A dataset with station IDs and geographic groups they belong it. Can be used
# #' to do grouped verification by joining the station_groups to a harp_fcst list
# #' and running the verification with \code{groupings = c("leadtime", "group")}.
# #' Note that many stations belong to more than one group
# #'
# #' @format A data frame with 6001 rows and 2 variables: \describe{
# #'   \item{SID}{station ID number} \item{group}{Geographic group the station
# #'   belongs to}}
# #'
# #' @source selecttion.pm from HARMONIE / HIRLAM monitor
# "station_groups"

#' Parameter definitions
#'
#' A list of parameters with name definitions for different file formats.
#'
#' @format A list of 60 parameters that have entries for different file formats
#'   including grib, netcdf, fa, v (cfld/vobs) and obsoul.
"harp_params"
