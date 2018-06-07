#' Read ensemble members from a forecast file
#'
#' \code{read_members} reads all esemble members from a forecast file for a
#' single lead time. It is possible to read from grib or netcdf files via the
#' helper functions. It is assumed that grib files only contain one member and
#' ntcdf files contain all members.
#'
#' @param model_files Files to read. For NetCDF only 1 file is expected, but for
#'   grib a vector of files can be passed. The character string 'mbr' must be
#'   somewhere in the paths for the member numbes to be identified. It is
#'   assumed that grib files only contain one member. For grib2 (yet to be
#'   implemented) this might not be the case.
#' @param parameter The parameter to read.
#' @param members A vector of numbers identifying the members to read. This is
#'   ignored for grib files as it is assumed that the members were already
#'   decided when the filenames were obtained.
#' @param file_type The forecast file format. The function can attempt to
#'   ascertain the format from the file name, but if it can't \code{file_type}
#'   must be passed as an argument.
#' @param lead_time The lead time to read.
#' @param ... Arguments to be passed to \code{read_netcf} or \code{read_grib}
#'
#' @return A list containing: \cr \code{model_data}: The 3d field (the 3rd
#'   dimension is ensemble member). \cr \code{x}: The x coordinates in
#'   the projection of the forecast file. \cr \code{y}: The y coordinates
#'   in the projection of the forecast file. \cr \code{proj4_string}: The
#'   proj4 projection string for the forecast file. \cr \code{parameter}:
#'   The parameter that the 3d field represents. \cr \code{filename}: The
#'   full path to the forecast files.
#' @export
#'
#' @examples
#' fname <- get_filenames(file_date = 2017080100, template = "meps_met")
#' model_field <- read_members(fname, "precipitation_amount_acc", lead_time = 24)
#' model_field <- read_members(fname, "Pcp", members = c(2, 4, 6), lead_time = 24)
#'
#' my_static_path <- "/lustre/storeB/users/andrewts/surfacePerturbations/grib"
#' my_expt <- "MEPS_summer2017_sfcPertRef"
#' my_template <- file.path(
#'   "${YYYY}${MM}${DD}${HH}",
#'   "${experiment}",
#'   "mbr${MBR3}/fc${YYYY}${MM}${DD}${HH}+${LDT3}.grib"
#' )
#' fname <- get_filenames(
#'   file_path = my_static_path,
#'   file_date = 2017052700,
#'   experiment = my_expt,
#'   template = my_template,
#'   lead_time = 3, member = seq(0, 10)
#' )
#' model_field <- read_members(fname, "fog")

read_members <- function(model_files,
  parameter,
  members   = seq(0, 9),
  file_type = NULL,
  lead_time = NULL,
  ...) {

#
# read control and get domain info
#
  if (is.null(file_type)) {

    file_type <- tolower(tools::file_ext(model_files[1]))
    if (! file_type %in% c("grib", "grb", "nc", "nc4", "netcdf")) {

      if (stringr::str_detect(model_files[1], "grib")) {
        file_type = "grib"
      } else {
        stop("Unable to ascertain file type. Call the function with file_type = '<file_type>'",
          call. = FALSE
        )
      }

    } else {

      file_type <- switch(
        file_type,
        "grb" = "grib",
        "nc"  = "netcdf",
        "nc4" = "netcdf",
        file_type
      )

    }

  }

  if (tolower(file_type) == "grib") {

    if (!requireNamespace("Rgrib2", quietly = TRUE)) {
      stop("Package Rgrib2 required for read_members() - you can get it from HARP",
        call. = FALSE
      )
    }

    num_perturbed_members <- length(model_files) - 1
    model_file            <- model_files[1]
    geofield_data         <- read_grib(model_file, parameter)
    domain_data           <- geogrid::DomainExtent(geofield_data)
    x                     <- seq(domain_data$x0, domain_data$x1, domain_data$dx)
    y                     <- seq(domain_data$y0, domain_data$y1, domain_data$dy)
    proj4_string          <- paste0(
      "+", paste(
        geogrid::proj4.list2str(attr(geofield_data, "domain")$projection), collapse = " +"
      )
    ) %>%
      stringr::str_replace("latlong", "longlat") %>%
      stringr::str_replace_all(" = ", "=") %>%
      stringr::str_replace_all(" =", "=") %>%
      stringr::str_replace_all("= ", "=")
    members               <- ifelse(length(model_files) > 1,
      model_files %>%
        strsplit("/") %>%
        purrr::map(~ stringr::str_subset(., "mbr")) %>%
        purrr::map_dbl(readr::parse_number),
      0
    )

    data_all        <- array(NA, c(dim(geofield_data), num_perturbed_members + 1))
    data_all[, , 1] <- geofield_data

  } else if (tolower(file_type) == "netcdf") {

    if (!requireNamespace("ncdf4", quietly = TRUE)) {
      stop("Package ncdf4 required for read_members() - Please install from CRAN",
        call. = FALSE
      )
    }

    if (is.null(lead_time)) stop("lead_time must be supplied for NetCDF data")
    model_file      <- model_files[1]
    ncID            <- ncdf4::nc_open(model_file)
    x               <- ncdf4::ncvar_get(ncID, "x")
    y               <- ncdf4::ncvar_get(ncID, "y")
    proj4_string    <- ncdf4::ncatt_get(ncID, "projection_lambert", "proj4")$value
    num_members     <- length(members)
    nc_members      <- ncdf4::ncvar_get(ncID, "ensemble_member")

    ncdf4::nc_close(ncID)

    if (num_members > length(nc_members)) {
      cat("\nWARNING: Number of members in file   =", length(nc_members))
      cat("\n         Number of members requested =", num_members)
      cat("\n         All members will be read from the file")
      cat("\n")
      members     <- nc_members
      num_members <- length(members)
    }

    num_perturbed_members <- num_members - 1
    data_all        <- array(NA, c(length(x), length(y), num_perturbed_members + 1))
    data_all[, , 1] <- read_netcdf(model_file, parameter, members[1], lead_time, ...)

  } else {
    stop("Unknown file type: ", file_type, ". Can only deal with netcdf or grib",
      call. = FALSE
    )
  }
#
# Get perturbed members
#
  if (num_perturbed_members > 0) {
    pb <- utils::txtProgressBar(min = 1, max = num_perturbed_members, initial = 1, style = 3)
    for (member in 1:num_perturbed_members) {
      member_name <- paste0("mbr", formatC(member, width = 3, flag = "0"))
      if (file_type == "grib") {
        model_file               <- model_files[member + 1]
        data_all[, , member + 1] <- read_grib(model_file, parameter)
      } else if (file_type == "netcdf") {
        data_all[, , member + 1] <- read_netcdf(model_file, parameter, members[member], lead_time, ...)
      }
      utils::setTxtProgressBar(pb, member)
    }
  }
#
# Convert units - it is assumed that when geopotential is requested, geopential
# height is what is wanted
#
  is_temperature <- function(x) {
    tolower(x) %in% c("t", "t2m", "sst") | stringr::str_detect(x, "temperature")
  }
  is_pressure <- function(x) {
    tolower(x) == "pmsl" | stringr::str_detect(x, "pressure")
  }
  is_geopotential <- function(x) {
    tolower(x) %in% c("z0m", "z") | stringr::str_detect(x, "geopotential")
  }

  if (is_temperature(parameter) & min(data_all, na.rm = TRUE) > 200) {
    data_all <- data_all - 273.15
  }
  if (is_pressure(parameter) & min(data_all, na.rm = TRUE) > 2000) {
    data_all <- data_all / 100
  }
  if (is_geopotential(parameter)) {
    data_all <- data_all / 9.80665
  }

  list(
    model_data   = data_all,
    x            = x,
    y            = y,
    member       = members,
    proj4_string = proj4_string,
    parameter    = parameter,
    filename     = model_files
  )
}
