#' Read point forecasts from FCTABLE files
#'
#' Reads point forecasts from an sqlite file produced by
#' \link{read_eps_interpolate} or \link{read_det_interpolate}. The function
#' generates the file names from the start date, end date, forecast model(s),
#' parameter, lead time etc. supplied as arguments. The members, stations,
#' forecast dates and lead times to be retrieved from the files can also be
#' passed as arguments.
#'
#' In the case of lagged forecasts, no lagged members are created at this stage,
#' but the \code{lags} argument is used to ensure that all of the necessary data
#' for creating lagged members are read in.
#'
#' @param start_date Date of the first forecast to be read in. Should be in
#'   YYYYMMDDhh format. Can be numeric or charcter.
#' @param end_date Date of the last forecast to be read in. Should be in
#'   YYYYMMDDhh format. Can be numeric or charcter.
#' @param fcst_model The forecast model to read - this is typically used to
#'   construct the file name. Can be a character vector of model names.
#' @param fcst_type The type of forecast to read. Set to "det" for deterministic
#'   or "eps" for ensemble.
#' @param parameter The forecast parameter to read. This is usually only used to
#'   construct the filename, and in accumumlating precipitation.
#' @param lead_time The lead times to be retrieved. Can be used to construct the
#'   file names and to set which lead times are retrieved.
#' @param lags The lags that were used when the forecast was run. If, for
#'   example, the FCTABLE files are constructed from lagged model runs the lags
#'   must be given here to ensure that the correct file names are generated. If,
#'   however, you simply want to add lagged members to a forecast, you should do
#'   that using \link[harpPoint]{lag_forecast}.
#' @param merge_lags A logical that, if set to TRUE (the default), lagged
#'   ensemble members will be shifted in time and joined to the parent forecast
#'   as derived from \code{start_date} and \code{by}.
#' @param by Used in constructing the file names. A string of a number followed
#'   by a letter (the default is "6h"), where the letter can be "d" for days,
#'   "h" for hours, "m" for minutes and "s" for seconds. Should be set to the
#'   fastest varying time dimension in the desired file names.
#' @param file_path The path to the data.
#' @param file_template The template for the file names of the files to be read
#'   from. This would normally be one of the "fctable_*" templates that can be
#'   seen in \link{show_file_templates}. Can be a single string, a character
#'   vector or list of the same length as \code{fcst_model}. If not named, the
#'   order of templates is assumed to be the same as in \code{fcst_model}. If
#'   named, the names must match the entries in \code{fcst_model}.
#' @param drop_any_na Set to TRUE (the default) to remove all cases where there
#'   is at least one missing value. This ensures that when you come to analyse a
#'   forecast, only those forecasts with a full set of ensmeble members / data
#'   are read in. For reading lagged ensembles, this is automatically set to
#'   FALSE. The cases with at least one missing member are then dropped when the
#'   lagged members are created using \link[harpPoint]{lag_forecast}.
#' @param stations The stations to retrieve forecasts for. This should be a
#'   vector of station ID numbers. Set to NULL to retrieve all stations.
#' @param members The members to retrieve if reading an EPS forecast. To select
#'   the same members for all forecast models, this should be a numeric vector.
#'   For specific members from specific models a named list with each element
#'   having the name of the forecast model and containing a a numeric vector.
#'   e.g. \cr \code{members = list(eps_model1 = seq(0, 3), eps_model2 = c(2,
#'   3))}. \cr For multi model ensembles, each element of this named list should
#'   contain another named list with sub model name followed by the desired
#'   members, e.g. \cr \code{members = list(eps_model1 = list(sub_model1 =
#'   seq(0, 3), sub_model2 = c(2, 3)))}
#' @param accumulate TRUE or FALSE. Whether to automatically accumulate
#'   parameters based on the accumulation time. Set to FALSE if the data to be
#'   read in have already been accumulated.
#' @param vertical_coordinate If upper air for multiple levels are to be read,
#'   the vertical coordinate of the data is given here. The default is
#'   "pressure", but can also be "model" for model levels, or "height" for
#'   height above ground /sea level.
#' @param get_lat_and_lon Logical indicating whether to also extract the
#'   latitude and longitude of the point forecasts from the sqlite files.
#' @param force_param_name Logical. Default is FALSE. Force the parameter name
#'   in the file template. For upper air data it is assumed that vertical
#'   profiles of the data are in the files so the vertical level information is
#'   removed when constructing the file name. If you want the full parameter
#'   name to be used in the file name set to TRUE.
#' @return A list with an element for each forecast model, or in the case of a
#'   multi model ensemble, another list with an element for each sub model. The
#'   list elements each contain a data frame with columns for station ID (SID),
#'   the forecast initialisation time (fcdate), the lead time (leadtime), the
#'   time for which the forecast is valid (validdate), and a column for the
#'   forecast with a heading of the model name in the case of a deterministic
#'   forecast, or multiple columns with heading names usually of the form
#'   \code{<model_name>_mbrXXX}, where XXX is the member number, in the case of
#'   an ensemble forecast.
#' @export
#'
#' @examples
#' if (requireNamespace("harpData", quietly = TRUE)) {
#'   # Read data from one deterministic model
#'   read_point_forecast(
#'     start_date = 2019021700,
#'     end_date   = 2019021718,
#'     fcst_model = "AROME_Arctic_prod",
#'     fcst_type  = "det",
#'     parameter  = "S10m",
#'     by         = "6h",
#'     file_path  = system.file("FCTABLE/deterministic", package = "harpData")
#'   )
#'
#'   # Read data from more than one deterministic model
#'   read_point_forecast(
#'     start_date = 2019021700,
#'     end_date   = 2019021718,
#'     fcst_model = c("AROME_Arctic_prod", "MEPS_prod"),
#'     fcst_type  = "det",
#'     parameter  = "S10m",
#'     by         = "6h",
#'     file_path  = system.file("FCTABLE/deterministic", package = "harpData")
#'   )
#'
#'   # Reading ensemble data works exactly the same way, but with fcst_type = "eps".
#'   # Note that all lead times are combined in the same file so
#'   # file_template = "fctable_eps_all_leads".
#'   read_point_forecast(
#'     start_date    = 2019021700,
#'     end_date      = 2019021718,
#'     fcst_model    = "MEPS_prod",
#'     fcst_type     = "eps",
#'     parameter     = "T2m",
#'     lead_time     = seq(0, 12, 3),
#'     by            = "6h",
#'     file_path     = system.file("FCTABLE/ensemble", package = "harpData"),
#'     file_template = "fctable_eps_all_leads"
#'   )
#'
#'   # You can select which members you want to read
#'   read_point_forecast(
#'     start_date    = 2019021700,
#'     end_date      = 2019021718,
#'     fcst_model    = "MEPS_prod",
#'     fcst_type     = "eps",
#'     parameter     = "T2m",
#'     lead_time     = seq(0, 12, 3),
#'     by            = "6h",
#'     file_path     = system.file("FCTABLE/ensemble", package = "harpData"),
#'     file_template = "fctable_eps_all_leads",
#'     members       = c(0, 3, 6)
#'   )
#'
#'   # If reading in vertical profiles, you must specify the vertical coordinate
#'   read_point_forecast(
#'     start_date          = 2019021700,
#'     end_date            = 2019021718,
#'     fcst_model          = "MEPS_prod",
#'     fcst_type           = "eps",
#'     parameter           = "T",
#'     lead_time           = seq(0, 12, 3),
#'     by                  = "6h",
#'     file_path           = system.file("FCTABLE/ensemble", package = "harpData"),
#'     file_template       = "fctable_eps_all_leads",
#'     members             = c(0, 3, 6),
#'     vertical_coordinate = "pressure"
#'   )
#'
#'   # When reading a lagged ensemble, the lags must be set for times at which
#'   # lagged members exist. If member numbers are duplicated "_lag" is
#'   # added to the member name.
#'   read_point_forecast(
#'     start_date    = 2019021700,
#'     end_date      = 2019021718,
#'     fcst_model    = "CMEPS_prod",
#'     fcst_type     = "EPS",
#'     parameter     = "T2m",
#'     lead_time     = seq(0, 12, 3),
#'     by            = "6h",
#'     lags          = paste0(seq(0, 5), "h"),
#'     file_path     = system.file("FCTABLE/ensemble", package = "harpData"),
#'     file_template = "fctable_eps_all_leads"
#'   )
#'
#'   # If more than one forecast model, and at least one is lagged, lags must
#'   # be specified for all forecast models as a named list.
#'   read_point_forecast(
#'     start_date    = 2019021700,
#'     end_date      = 2019021718,
#'     fcst_model    = c("CMEPS_prod", "MEPS_prod"),
#'     fcst_type     = "EPS",
#'     parameter     = "T2m",
#'     lead_time     = seq(0, 12, 3),
#'     by            = "6h",
#'     lags          = list(
#'       CMEPS_prod = paste0(seq(0, 5), "h"),
#'       MEPS_prod  = "0h"
#'     ),
#'     file_path     = system.file("FCTABLE/ensemble", package = "harpData"),
#'     file_template = "fctable_eps_all_leads"
#'   )
#' }
read_point_forecast <- function(
  start_date,
  end_date,
  fcst_model,
  fcst_type,
  parameter,
  lead_time           = seq(0, 48, 3),
  lags                = "0s",
  merge_lags          = TRUE,
  by                  = "6h",
  file_path           = ".",
  file_template       = "fctable",
  drop_any_na         = TRUE,
  stations            = NULL,
  members             = NULL,
  accumulate          = TRUE,
  vertical_coordinate = c(NA_character_, "pressure", "model", "height"),
  get_lat_and_lon     = FALSE,
  force_param_name    = FALSE
) {

  switch(tolower(fcst_type),
    "eps" = {
      if (is.null(file_template)) file_template <- "fctable_eps"
      member_regexp <- "[[:graph:]]+(?=_mbr[[:digit:]]+)"
      fcst_suffix   <- "_mbr"
    },
    "det" = {
      if (is.null(file_template)) file_template <- "fctable_det"
      member_regexp <- "[[:graph:]]+(?=_det)"
      fcst_suffix   <- "_det"
    },
    {
      file_template <- NULL
      member_regexp <- NULL
      fcst_suffix   <- NULL
    }
  )
  if (is.null(member_regexp)) {
    stop("Unknown fcst_type argument: ", fcst_type, ". \nMust be one of 'eps' or 'det'", call. = FALSE)
  }

  if (any(readr::parse_number(unlist(lags)) != 0)) {
    lags_passed <- TRUE
  } else {
    lags_passed <- FALSE
  }

  if (drop_any_na && merge_lags) {
    drop_function <- dplyr::all_vars(!is.na(.))
  } else {
    drop_function <- dplyr::any_vars(!is.na(.))
  }

  vertical_coordinate <- match.arg(vertical_coordinate)

  parameter  <- parse_harp_parameter(parameter, vertical_coordinate)
  param_name <- parameter$fullname
  if (parameter$accum > 0 && accumulate) {
    param_name <- parameter$basename
    lead_time  <- lead_time[lead_time >= parse_accum(parameter)]
  }
  if (is_temp(parameter) && !force_param_name) {
    param_name <- parameter$basename
  }

  if (length(lead_time) < 1) {
    stop(
      "At least one of lead_time must be greater than or equal to the accumulation time: ",
      parse_accum(parameter), parameter$acc_unit,
      call. = FALSE
    )
  }

  if (is.list(lags)) {
    if (length(lags) != length(fcst_model)) {
      stop("lags must be a list of the same length as fcst_model.", call. = FALSE)
    }
  } else {
    if (length(fcst_model) > 1) {
      if (lags == "0s") {
        lags <- as.list(rep("0s", length(fcst_model)))
      } else {
        stop("lags must be passed as a list of the same length as fcst_model.", call. = FALSE)
      }
    } else {
      lags <- list(lags)
    }
  }

  if (length(file_template) == 1) {
    if (length(fcst_model) > 1) {
      warning("Only 1 'file_template' defined. Recycling for all 'fcst_model'.", call. = FALSE, immediate. = TRUE)
    }
    template_table <- data.frame(
      fcst_model       = fcst_model,
      file_template    = unlist(file_template),
      stringsAsFactors = FALSE
    )
  } else {
    if (length(file_template) != length(fcst_model)) {
      stop(
        "You must have either 1 'file_template', or there must be the same number of elements\n",
        "in 'file_template' as in 'fcst_model'.",
        call. = FALSE
      )
    } else {
      if (is.null(names(file_template))) {
        warning(
          "No names specified for 'file_template'. Assuming the same order as 'fcst_model'.",
          call.      = FALSE,
          immediate. = TRUE
        )
        template_table <- data.frame(
          fcst_model       = fcst_model,
          file_template    = unlist(file_template),
          stringsAsFactors = FALSE
        )
      } else {
        if (!identical(sort(fcst_model), sort(names(file_template)))) {
          stop("'file_template' names must be the same as 'fcst_model'.", call. = FALSE)
        }
        template_table <- data.frame(
          fcst_model       = names(file_template),
          file_template    = unlist(file_template),
          stringsAsFactors = FALSE)
      }
    }
  }

  lag_table <- purrr::map2_dfr(
    fcst_model,
    lags,
    ~ expand.grid(fcst_model = .x, lag = .y, stringsAsFactors = FALSE)
  ) %>%
    dplyr::inner_join(template_table)

  file_names <- purrr::pmap(
    as.list(lag_table),
    ~ generate_filenames(
      file_path     = file_path,
      start_date    = start_date,
      end_date      = end_date,
      by            = by,
      lags          = ..2,
      parameter     = param_name,
      fcst_model    = gsub("_unshifted", "", ..1),
      lead_time     = lead_time,
      file_template = ..3
    )
  )

  missing_files <- purrr::map(
    file_names,
    ~ { if (length(.x[!file.exists(.x)]) < 1) { "none" } else { .x[!file.exists(.x)] } }
  ) %>%
    rlang::set_names(lag_table$fcst_model) %>%
    merge_names()

  available_files <- purrr::map(
    file_names,
    ~ .x[file.exists(.x)]
  )

  check_for_missing <- purrr::flatten_chr(missing_files)
  if (any(check_for_missing != "none")) {
    std_warn_length <- getOption("warning.length")
    options(warning.length = 8170)
    warning(
      "Cannot find files:\n",
      paste(check_for_missing[check_for_missing != "none"], collapse = "\n"),
      "\n",
      immediate. = TRUE,
      call. = FALSE
    )
    options(warning.length = std_warn_length)
  }

  if (any(purrr::map_int(available_files, length) < 1)) {
    model_with_no_files <- paste(
      lag_table$fcst_model[purrr::map_int(available_files, length) < 1],
      lag_table$lag[purrr::map_int(available_files, length) < 1],
      sep = " - lag: "
    )
    stop("No forecast files found for: \n", paste(model_with_no_files, collapse = "\n"), call. = FALSE)
  }

  if (!is.null(members)) {
    if (!is.list(members)) {
      if (length(fcst_model) > 1) {
        warning("Only one set of members specified. Using same set for all forecast models.", immediate. = TRUE, call. = FALSE)
      }
      members <- rep(list(members), length(fcst_model))
      names(members) <- fcst_model
    } else {
      if (is.null(names(members))) {
        if (length(members) == 1) {
          if (length(fcst_model) > 1) {
            warning("Only one set of members specified. Using same set for all forecast models.", immediate. = TRUE, call. = FALSE)
          }
          members <- rep(members, length(fcst_model))
          names(members) <- fcst_model
        } else {
          stop("'members' must either be a named list or a single numeric vector.", call. = FALSE)
        }
      } else {
        all_members <- setdiff(fcst_model, names(members))
        if (length(all_members) > 0) {
          members[all_members] <- NULL
        }
        bad_members <- setdiff(names(members), fcst_model)
        if (length(bad_members) > 0) {
          stop(paste(bad_members, collapse = ", "), " specified in 'members' but not in 'fcst_model'", call. = FALSE)
        }
      }
    }
    members <- tibble::tibble(fcst_model = names(members)) %>% dplyr::mutate(members = members)
  } else {
    members <- tibble::tibble(fcst_model = fcst_model, members = list(NULL))
  }

  lag_table <- dplyr::left_join(lag_table, members)

  fcst <- purrr::pmap(
    list(
      available_files,
      lag_table$lag,
      lag_table$members
    ),
    ~ read_fctable(
      .x,
      suppressMessages(str_datetime_to_unixtime(start_date)) - (readr::parse_number(.y) * units_multiplier(.y)),
      suppressMessages(str_datetime_to_unixtime(end_date)),
      lead_time  = lead_time + readr::parse_number(.y),
      stations   = stations,
      members    = ..3,
      param      = parameter,
      get_latlon = get_lat_and_lon
    )
  )


  no_members <- sapply(fcst, function(x) !any(grepl(fcst_suffix, names(x))))
  no_members_warning <- function(mname, lag_time, mbr, no_mbrs) {
    if (no_mbrs) {
      warning("Members ", paste(mbr, collapse = ","), " not found for ", mname, ", lag: ", lag_time, immediate. = TRUE, call. = FALSE)
    }
  }
  if (any(no_members)) {
    purrr::pwalk(
      list(lag_table$fcst_model, lag_table$lag, lag_table$members, no_members),
      no_members_warning
    )
    message("Dropping entries with no members")
    lag_table <- lag_table[which(!no_members),]
    fcst      <- fcst[which(!no_members)]
  }

  fcst <- purrr::map(fcst, dplyr::filter_at, dplyr::vars(dplyr::contains(fcst_suffix)), drop_function)

  if (parameter$accum > 0 && accumulate) {

    accum <- parse_accum(parameter)

    fcst_accum <- purrr::map(
      fcst,
      ~ accumulate_forecast(
        tidyr::gather(.x, dplyr::contains(fcst_suffix), key = "member", value = "forecast"),
        accum,
        parameter$acc_unit
      )
    )

    # accumulate_forecast returns a vector of missing lead times rather than data if some lead times
    # to compute an accumlation are missing.


    if (any(purrr::map_lgl(fcst_accum, is.numeric))) {
      lead_time_accum <- fcst_accum
      unread_leads <- which(purrr::map_lgl(fcst_accum, is.numeric))

      file_names <- purrr::pmap(
        list(
          lag_table$fcst_model[unread_leads],
          lag_table$lag[unread_leads],
          lead_time_accum[unread_leads],
          lag_table$file_template[unread_leads]
        ),
        ~ generate_filenames(
          file_path     = file_path,
          start_date    = start_date,
          end_date      = end_date,
          by            = by,
          lags          = .y,
          parameter     = param_name,
          fcst_model    = gsub("_unshifted", "", .x),
          lead_time     = ..3 - readr::parse_number(.y),
          file_template = ..4
        )
      )

      fcst_lead_time_accum <- purrr::pmap(
        list(
          purrr::map(file_names, ~ .x[file.exists(.x)]),
          lag_table$lag[unread_leads],
          lead_time_accum[unread_leads],
          lag_table$members[unread_leads]
        ),
        ~ read_fctable(
          .x,
          suppressMessages(str_datetime_to_unixtime(start_date)) - (readr::parse_number(.y) * units_multiplier(.y)),
          suppressMessages(str_datetime_to_unixtime(end_date)),
          lead_time  = ..3,
          stations   = stations,
          members    = ..4,
          param      = parameter,
          get_latlon = get_lat_and_lon
        )
      ) %>% purrr::map(dplyr::filter_at, dplyr::vars(dplyr::contains(fcst_suffix)), drop_function)

      fcst[unread_leads]       <- purrr::map2(fcst[unread_leads], fcst_lead_time_accum, dplyr::bind_rows)
      fcst_accum[unread_leads] <- purrr::map(
        fcst[unread_leads],
        ~ accumulate_forecast(
          tidyr::gather(.x, dplyr::contains(fcst_suffix), key = "member", value = "forecast"),
          accum,
          parameter$acc_unit,
          check_leads = FALSE
        )
      )

    }

    fcst <- purrr::map(fcst_accum, tidyr::spread, .data$member, .data$forecast)

  }

  split_sub_models <- function(df, .member_regexp) {

    meta_cols  <- rlang::syms(c("SID", "fcdate", "leadtime", "validdate", "fcst_cycle"))
    sub_models <- stringr::str_extract(
      names(df),
      .member_regexp
    ) %>%
      stats::na.omit() %>%
      unique()

    if (length(sub_models) == 1) {
      df
    } else {
      df <- purrr::map(
        sub_models,
        ~ dplyr::select(df, !!! meta_cols, dplyr::contains(.x))
      ) %>%
        rlang::set_names(sub_models)
      class(df) <- "harp_fcst"
      df
    }

  }

  fcst <- purrr::map(fcst, split_sub_models, member_regexp)

  if (merge_lags) {
    fcst <- lag_and_join(fcst, lag_table)
  } else {
    fcst <- merge_names_df(fcst, lag_table$fcst_model)
  }

  fcst <- purrr::map(
    fcst,
    ~ dplyr::select(
      dplyr::mutate(
        .x,
        fcdate = unix2datetime(.data[["fcdate"]]),
        validdate = unix2datetime(.data[["validdate"]])
      ),
      .data[["fcdate"]],
      .data[["validdate"]],
      .data[["leadtime"]],
      .data[["SID"]],
      dplyr::matches("^parameter$"),
      dplyr::matches("^p$"), dplyr::matches("^m$"), dplyr::matches("^z$"),
      dplyr::matches("_det$"),
      dplyr::matches("_mbr[[:digit:]]+$"),
      dplyr::matches("_mbr[[:digit:]]+_lag[[:digit:]]*$"),
      dplyr::everything()
    ) %>%
      dplyr::transmute(
        dplyr::across(where(~!all(is.na(.x))))
      )
  )

  attr(fcst, "missing_files") <- missing_files
  class(fcst) <- "harp_fcst"

  fcst

}

parse_accum <- function(prm) {
  switch(prm$acc_unit,
    "h" = prm$accum / 3600,
    "m" = prm$accum / 60,
    prm$accum
  )
}

merge_names <- function(x) {
  names_x <- unique(names(x))
  y       <- list()
  for (element_x in names_x) {
    y[[element_x]] <- unlist(x[which(names(x) == element_x)], use.names = FALSE)
  }
  y
}

### This needs modifying to deal with lagged multimodel...
merge_names_df <- function(df_list, df_names) {
  names(df_list) <- df_names
  merged <- list()
  for (df_name in unique(df_names)) {
    df_elements       <- which(df_names == df_name)
    if (length(df_elements) > 1) {
      merged[[df_name]] <- dplyr::bind_rows(df_list[df_elements])
    } else {
      merged[[df_name]] <- df_list[df_elements][[1]]
    }
  }
  merged
}

# Adjust fcdate and lead time of lagged members and join to unlagged.
lag_and_join <- function(fcst_list, lags_df) {

  lag_seconds <- purrr::map2_dbl(
    as.numeric(gsub("\\D", "", lags_df$lag)),
    lags_df$lag,
    ~ .x * units_multiplier(.y)
  )

  non_zero_values <- which(lag_seconds > 0)
  if (!any(non_zero_values)) {
    fcst_list <- merge_names_df(fcst_list, lags_df$fcst_model)
    return(fcst_list)
  }

  fcst_list[non_zero_values] <- purrr::map2(
    fcst_list[non_zero_values],
    lag_seconds[non_zero_values],
    ~ dplyr::mutate(
      .x,
      fcdate     = .data$fcdate + .y,
      leadtime   = .data$leadtime - .y / 3600,
      fcst_cycle = substr(unixtime_to_str_datetime(.data$fcdate, YMDh), 9, 10)
    )
  )

  join_lags <- function(inner_list) {
    if (length(inner_list) > 1) {
      join_cols <- purrr::map(inner_list, colnames) %>%
        purrr::reduce(intersect)
      purrr::reduce(inner_list, dplyr::inner_join, by = join_cols, suffix = c("", "_lag"))
    } else {
      inner_list[[1]]
    }
  }

  fcst_list <- split(fcst_list, lags_df$fcst_model) %>%
    purrr::map(join_lags)

  fcst_list

}
