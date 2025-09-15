# Reads a dataset of parquet files using the arrow package. A dataset is a
# collection of parquet files organised in directories using "hive"
# partitioning. The file_name argument is therefore not quite correctly named,
# but needs to be consistent with the call from read_point_forecast() /
# read_point_obs(). file_name is essentially the path to the dataset.


# Read a forecast dataset - called by read_point_forecast()
read_fcparquet <- function(
  file_name,
  fcst_dttm, # in unix time. Already adjusted to take account of lags
  lead_time        = NULL, # in seconds with names. Already adjusted for lags
  stations         = NULL,
  members          = NULL,
  valid_dttm       = NULL, # in unix time
  param            = NULL,
  get_latlon       = FALSE,
  force_param_name = FALSE, # Used if (e.g) pressuere level is in param name rather than separate rows for levels
  use_dttm         = TRUE, # Probably don't need this
  meta_only        = FALSE, # Do not get the actual forecast data
  complete_cases   = FALSE,
  file_format_opts = fcparquet_opts(),
  ...
) {

  file_format_opts <- do.call(fcparquet_opts, file_format_opts)

  # The year, month, day, hour need to be extracted from fcst_dttm for the
  # most efficient filtering
  dttm <- dttm_components(fcst_dttm)

  # param has already been sent to parse_harp_parameter(), but if it's NULL, it
  # needs to have level_type set to NA.
  if (is.null(param)) {
    param <- list(level_type = NA_character_)
  }

  # If param is to be filtered by vertical level it needs to have a level_type.
  # It also needs to be a "TEMP" parameter. However, sometimes data are stored
  # for a single level (e.g. "T850"), so no filtering should be done by vertical
  # level - in this case force_param_name should be TRUE.
  level_col <- NULL
  if (!is.na(param$level_type) && is_temp(param) && !force_param_name) {
    level_col <- switch(
      param$level_type,
      "pressure" = "p",
      "model"    = "ml",
      "height"   = "z",
      "isotherm" = "t"
    )
  }

  message(
    cli::col_br_blue("Opening "),
    cli::col_br_red("parquet "),
    cli::col_br_blue("dataset: "),
    cli::col_br_yellow(file_name),
    appendLF = FALSE
  )

  # The top level of the hive partitioning is fcst_hour. The schema is inferred
  # for the "current" fcst_hour is it can be different, depending on which lag
  # is being read in.
  dataset_schema <- arrow::schema(
    arrow::open_dataset(
      file.path(
        file_name,
        paste0("fcst_hour=", dttm$hour[1])
      )
    )
  )

  # When the data are actually read, the dataset is opened from the top level
  # directory of the partitioning so fcst_hour needs adding back into the schema
  dataset_schema <- arrow::unify_schemas(
    dataset_schema, arrow::schema(fcst_hour = arrow::int32())
  )

  fcst <- arrow::open_dataset(file_name, schema = dataset_schema)

  # Get the column names
  all_cols   <- colnames(fcst)
  fcst_regex <- "_mbr[0-9]{3}|^fcst$|^forecast$|_det$"
  fcst_cols  <- grep(fcst_regex, all_cols, value = TRUE)

  # Filter based on fcst_dttm - filtering on the hive partitioning prevents
  # having to read data from more parquet files than necessary
  fcst <- dplyr::filter(
    fcst,
    .data[["fcst_hour"]]  %in% dttm$hour,
    .data[["fcst_year"]]  %in% dttm$year,
    .data[["fcst_month"]] %in% dttm$month,
    .data[["fcst_day"]]   %in% dttm$day,
    .data[["fcst_dttm"]]  %in% dttm$unixtime
  )

  # The hive partitioning columns aren't going to be needed anymore
  fcst <- dplyr::select(
    fcst,
    -dplyr::any_of(
      paste0("fcst_", c("minute", "hour", "day", "month", "year"))
    )
  )

  # If lead times are to be filtered on, they are already in seconds. The
  # lead time column for parquet files will always be in seconds.
  if (!is.null(lead_time)) {
    # Avoid clash of variable names with column names
    lt   <- lead_time
    fcst <- dplyr::filter(fcst, .data[["lead_time"]] %in% lt)
  }

  # Filter for stations and valid_dttm
  if (!is.null(stations)) {
    fcst <- dplyr::filter(fcst, .data[["SID"]] %in% stations)
  }

  if (!is.null(valid_dttm)) {
    vdt  <- valid_dttm
    fcst <- dplyr::filter(fcst, .data[["valid_dttm"]] %in% vdt)
  }

  # Filter to vertical level
  if (!is.null(level_col)) {
    fcst <- dplyr::filter(fcst, .data[[level_col]] %in% param$level)
  }

  meta_cols <- setdiff(all_cols, fcst_cols)

  if (meta_only) {
    return(collect_dataset(
      dplyr::distinct(
        dplyr::select(fcst, dplyr::any_of(meta_cols))
      )
    ))
  }

  # Members are in columns - select only those members that are asked for
  is_ens <- any(grepl("mbr[0-9]{3}", fcst_cols))
  if (!is.null(members) && is_ens) {
    # members can be a named list if it's a multi-model ensemble
    if (is.list(members)) {
      member_cols <- purrr::map2(
        names(members),
        members,
        ~paste(.x, formatC(.y, width = 3, flag = "0"), sep = "_mbr")
      ) %>%
        unlist()
    } else {
      member_cols <- paste0("_mbr", formatC(members, width = 3, flag = "0"))
    }

    member_cols <- fcst_cols[
      grep(paste(member_cols, collapse = "|"), fcst_cols)
    ]

    fcst <- dplyr::select(
      fcst, dplyr::any_of(meta_cols), dplyr::any_of(member_cols)
    )
  }

  if (!get_latlon) {
    fcst <- dplyr::select(
      fcst, -dplyr::any_of(c("lat", "latitude", "lon", "long", "longitude"))
    )
  }

  if (complete_cases) {
    fcst <- dplyr::filter(
      fcst,
      dplyr::if_all(dplyr::matches(paste(fcst_cols, sep = "|")), ~!is.na(.x))
    )
  }

  message(" ", cli::col_br_green(cli::symbol$tick))

  fcst <- collect_dataset(fcst)

  if (nrow(fcst) < 1) {
    message(" ", cli::col_yellow(cli::symbol$cross))
    cli::cli_warn(c(
      "!" = "No data found for {cli::col_br_yellow(file_name)}."
    ))
  }

  list(fcst = fcst, lead_unit = "s", lead_has_zero = any(fcst$lead_time == 0))

}

# Probably only needs to be used for writing

#' Options for writing parquet datasets.
#'
#' @description
#'
#' Parquet datasets can be used as point data storage for harp as an
#' alternative to SQLite. A parquet dataset is a multi-file dataset made up
#' of parquet files stored in directories that are partitioned based on the
#' data. This makes it effeicient to query the data and tends to result in
#' faster performance than SQLite and less use of storage space. Internally, the
#' \href{https://arrow.apache.org/docs/r/index.html}{arrow} package is used to
#' read from and write to parquet datasets. `fcparquet_opts()` and
#' `obsparquet_opts()` generate options for writing point forecast using
#' \code{\link{read_forecast()}} and \code{\link{read_obs()}} respectively.
#'
#' @param path The path to which to write the dataset.
#' @param partitioning How the data are to be partitioned in the directory
#'   structure. This should not be changed.
#' @param dir_mode The numeric permission mode of the directories to be written.
#'   The default is "0750".
#' @param ... Not used.
#'
#' @returns A named list.
#' @export
#'
fcparquet_opts <- function(
  path,
  partitioning = c("fcst_hour", "fcst_year", "fcst_month", "fcst_day"),
  dir_mode     = "0750",
  ...
) {
  if (missing(path)) path <- NULL
  list(
    format       = "fcparquet",
    path         = path,
    partitioning = partitioning,
    template     = "{fcst_model}/{parameter}",
    dir_mode     = dir_mode
  )
}

#' @rdname fcparquet_opts
#' @export
obsparquet_opts <- function(
  path,
  partitioning = c("valid_year", "valid_month", "valid_day"),
  dir_mode     = "0750",
  ...
) {
  list(
    format       = "obsparquet",
    path         = path,
    partitioning = partitioning,
    template     = "{YYYY}{MM}{DD}",
    dir_mode     = dir_mode
  )
}

# Get all components of dttm
dttm_components <- function(unixtime) {
  dttm <- harpCore::unixtime_to_dttm(unixtime)
  list(
    dttm     = dttm,
    unixtime = unixtime,
    year     = as.numeric(unique(format(dttm, "%Y"))),
    month    = as.numeric(unique(format(dttm, "%m"))),
    day      = as.numeric(unique(format(dttm, "%d"))),
    hour     = as.numeric(unique(format(dttm, "%H")))
  )
}

# Write a dataset of parquet files for forecast data - called by read_forecast()
# when transformation = "intperpolate" or "points"

write_fctable_to_parquet <- function(data, file_name, hive_partitioning) {

  if (missing(hive_partitioning) || is.null(hive_partitioning)) {
    hive_partitioning = paste0("fcst_", c("hour", "year", "month", "day"))
  }

  message(cli::col_blue("Preparing data"), appendLF = FALSE)
  data <- tidyr::pivot_wider(
    data,
    names_from  = dplyr::one_of("member"),
    values_from = dplyr::one_of("forecast")
  )

  data[["fcst_dttm"]] <- harpCore::unixtime_to_dttm(data[["fcst_dttm"]])

  data <- harpCore::expand_date(data, "fcst_dttm")

  data[["fcst_dttm"]] <- as.integer(data[["fcst_dttm"]])
  data[["lead_time"]] <- data[["valid_dttm"]] - data[["fcst_dttm"]]

  hive_partitioning <- intersect(hive_partitioning, colnames(data))

  message(" ", cli::col_green(cli::symbol$tick))

  message(
    cli::col_blue("Writing to dataset at: "),
    cli::col_red(file_name),
    appendLF = FALSE
  )

  # need to convert to data frame otherwise a warning is triggered
  data <- as.data.frame(data)
  data <- dplyr::mutate(
    arrow::arrow_table(data),
    fcst_dttm = .data[["fcst_dttm"]]$cast(arrow::int64()),
    valid_dttm = .data[["valid_dttm"]]$cast(arrow::int64())
  )

  arrow::write_dataset(data, file_name, partitioning = hive_partitioning)
  message(" ", cli::col_green(cli::symbol$tick))

}

write_obsparquet <- function(
  obs_data,
  file_name,
  partitioning,
  table_name = "SYNOP",
  params_table = NULL,
  dir_mode = "0750",
  ...
) {

  # To ensure that data are separated by day, file_name includes YYYYMMDD at
  # the end. This isn't the directory we want to write to, so remove it
  data_date <- format(harpCore::as_dttm(basename(file_name)), "%d %b %Y")
  file_name <- dirname(file_name)

  # Get schema for incoming data - note that valid_dttm should be int64 and
  # SID should be a character string. Also need to expand the valid_dttm first
  # to allow for partitioning
  obs_data[["valid_dttm"]] <- harpCore::unixtime_to_dttm(obs_data[["valid_dttm"]])

  obs_data   <- harpCore::expand_date(obs_data, "valid_dttm")
  data_range <- paste(
    format(range(obs_data[["valid_dttm"]]), "%H:%M"), collapse = "-"
  )

  obs_data[["valid_dttm"]] <- as.integer(obs_data[["valid_dttm"]])
  obs_data[["SID"]]        <- as.character(obs_data[["SID"]])

  obs_data <- as.data.frame(obs_data)

  obs_data <- dplyr::mutate(
    arrow::arrow_table(obs_data),
    valid_dttm = .data[["valid_dttm"]]$cast(arrow::int64())
  )
  data_schema <- arrow::schema(obs_data)

  # Need to check for a schema file - assume there isn't one to begin with
  has_schema  <- FALSE
  schema_file <- file.path(
    file_name, "schema", paste0(tolower(table_name), "-schema.parquet")
  )
  if (file.exists(schema_file)) {
    has_schema <- TRUE
  }

  ignore_prefix <- switch(
    tolower(table_name),
    "synop" = "temp",
    "temp"  = "synop"
  )

  if (has_schema) {
    obs_schema <- arrow::schema(
      arrow::open_dataset(
        dirname(schema_file),
        factory_options = list(selector_ignore_prefixes = ignore_prefix)
      )
    )
    data_schema <- arrow::unify_schemas(data_schema, obs_schema)
  } else {
    if (!dir.exists(dirname(schema_file))) {
      dir.create(dirname(schema_file), mode = dir_mode, recursive = TRUE)
    }
  }

  # Write out the whole schema
  arrow::write_parquet(
    data_schema, schema_file
  )

  # Check if there is a units file, and add to it if necessary.
  params_file <- file.path(file_name, "params", "params.parquet")
  has_params  <- file.exists(params_file)

  if (!is.null(params_table)) {
    write_params <- TRUE
    params_table <- dplyr::select(params_table, -dplyr::any_of("accum_hours"))
    if (!has_params && !dir.exists(dirname(params_file))) {
      dir.create(dirname(params_file), recursive = TRUE, mode = dir_mode)
      params_status <- cli::col_br_green("Writing new ")
    } else {
      saved_params <- arrow::read_parquet(params_file)
      new_params   <- setdiff(
        params_table[["parameter"]], saved_params[["parameter"]]
      )
      if (length(new_params) < 1) {
        write_params <- FALSE
      } else {
        params_table <- dplyr::bind_rows(
          saved_params,
          dplyr::filter(params_table, .data[["parameter"]] %in% new_params)
        )
        params_status <- cli::col_yellow("Updating ")
      }
    }
    if (write_params) {
      message(params_status, "params table", appendLF = FALSE)
      arrow::write_parquet(params_table, params_file)
      message(" ", cli::col_green(cli::symbol$tick))
    }
  }

  # Write out the data
  message(
    cli::col_blue("Writing "),
    cli::col_yellow(table_name),
    cli::col_blue(" observations for "),
    cli::col_yellow(paste(data_date, data_range)),
    cli::col_blue(" to parquet dataset at: "),
    cli::col_red(file_name),
    appendLF = FALSE
  )
  arrow::write_dataset(
    obs_data, file_name, partitioning = partitioning,
    basename_template = paste(
      tolower(table_name), gsub(":", "", data_range), "{i}.parquet",
      sep = "-"
    )
  )
  message(" ", cli::col_green(cli::symbol$tick))
}

read_obsparquet <- function(
  dataset_path,
  .obs_param,
  table_name,
  dttm,
  stations,
  level_col = NULL,
  level     = NULL,
  ...
) {
  obs_param_quo <- rlang::enquo(.obs_param)
  obs_param     <- rlang::quo_name(obs_param_quo)

  schema_dir  <- file.path(dataset_path, "schema")
  params_file <- file.path(dataset_path, "params", "params.parquet")

  if (!dir.exists(dataset_path)) {
    cli::cli_abort(paste("obsparquet dataset", dataset_path, "not found"))
  }

  ignore <- switch(
    tolower(table_name),
    "synop" = "temp",
    "temp"  = "synop"
  )

  obs_schema <- tryCatch(
    arrow::open_dataset(
      schema_dir, factory_options = list(selector_ignore_prefixes = ignore)
    ),
    error = function(e) NULL
  )

  if (!is.null(obs_schema)) {
    obs_schema <- arrow::schema(obs_schema)
  } else {
    message("Inferring dataset schema from data")
  }

  message(
    cli::col_blue("Opening dataset at "),
    cli::col_red(dataset_path),
    appendLF = FALSE
  )

  ignore <- c(ignore, "params", "schema")

  obs_data <- arrow::open_dataset(
    dataset_path,
    schema = obs_schema,
    factory_options = list(selector_ignore_prefixes = ignore)
  )

  all_cols  <- colnames(obs_data)
  meta_cols <- c(
    grep("^valid", all_cols, value = TRUE),
    "SID", "lat", "lon", "elev", level_col
  )
  data_cols <- setdiff(all_cols, meta_cols)

  if (!is.element(obs_param, data_cols)) {
    cli::cli_abort(c(
      "Observation parameter not found!",
      "x" = "Column name \"{obs_param}\" does not exist in dataset.",
      "i" = "Available colums {?is/are} {data_cols}."
    ))
  }

  obs_data <- dplyr::select(
    obs_data,
    dplyr::any_of(c(meta_cols, obs_param))
  )

  obs_data <- dplyr::filter(obs_data, !is.na(.data[[obs_param]]))

  if (!is.null(dttm)) {
    obs_data <- dplyr::filter(obs_data, .data[["valid_dttm"]] %in% dttm)
  }

  if (!is.null(stations)) {
    obs_data <- dplyr::filter(obs_data, .data[["SID"]] %in% stations)
  }

  if (tolower(table_name) == "temp") {
    if (!is.element(level_col, meta_cols) && level != -999) {
      cli::cli_abort("Level column \"{level_col}\" not found in dataset")
    }
    if (length(level[level != -999]) > 0) {
      obs_data <- dplyr::filter(obs_data, .data[[level_col]] %in% level)
    }
  }

  obs_data <- dplyr::select(
    obs_data,
    -dplyr::any_of(
      paste("valid", c("year", "month", "day", "hour", "minute"), sep = "_")
    )
  )

  message(" ", cli::col_br_green(cli::symbol$tick))

  message(
    cli::col_blue("Collecting dataset"),
    appendLF = FALSE
  )

  obs_data <- dplyr::collect(obs_data)

  if (file.exists(params_file)) {
    params <- arrow::read_parquet(params_file)
    units <- dplyr::filter(params, .data[["parameter"]] == obs_param)[["units"]]
  } else {
    units <- NULL
  }

  if (length(units) > 0) {
    obs_data <- dplyr::mutate(obs_data, units = units)
  } else {
    obs_data <- dplyr::mutate(
      obs_data, units = guess_units(obs_data, obs_param)
    )
  }


  message(" ", cli::col_br_green(cli::symbol$tick))

  obs_data
}
