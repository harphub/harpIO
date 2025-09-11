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
#' @param dttm A vector of date time strings to read. Can be in YYYYMMDD,
#'   YYYYMMDDhh, YYYYMMDDhhmm, or YYYYMMDDhhmmss format. Can be numeric or
#'   character. \code{\link[harpCore]{seq_dttm}} can be used to generate a
#'   vector of equally spaced date-time strings.
#' @param fcst_model The forecast model to read - this is typically used to
#'   construct the file name. Can be a character vector of model names.
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
#' @param file_path The path to the data.
#' @param file_template The template for the file names of the files to be read
#'   from. This would normally be one of the "fctable_*" templates that can be
#'   seen in \link{show_file_templates}. Can be a single string, a character
#'   vector or list of the same length as \code{fcst_model}. If not named, the
#'   order of templates is assumed to be the same as in \code{fcst_model}. If
#'   named, the names must match the entries in \code{fcst_model}.
#' @param file_format The format of the file to read. By default this is
#'   "fctable", which is _SQLite_ files with a schema that is the same as that
#'   in files produced by \code{\link{read_forecast()}} with an output format of
#'   "fctable". Could also be "fc_dataset" which is an _Arrow_ dataset of
#'   _parquet_ files with a schema that is the same as that in a dataset
#'   produced by \code{\link{read_forecast()}} with an output format of
#'   "fc_dataset". Otherwise the running environment will be be searched for a
#'   function called `read_<file_format>()`.
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
#' @param valid_dttm The valid date-times to retrieve in YYYYMMDD{hh}{mm}{ss}
#'   format. This is currently only available for `file_format = "fc_dataset"`.
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
#' @param meta_only Logical. Default id FALSE. Set to TRUE to only read the
#'   metadata. This is useful as a preprocessing step to identify which cases
#'   exist for each `fcst_model`.
#' @param start_date,end_date,by `r lifecycle::badge("deprecated")` The use of
#'   `start_date`, `end_date` and `by` is no longer supported. `dttm` together
#'   with \code{\link[harpCore]{seq_dttm}} should be used to generate equally
#'   spaced date-times.
#' @param fcst_type `r lifecycle::badge("deprecated")` `fcst_type` is no longer
#'   required and this its use is deprecated and will be removed as an argument.
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
  dttm,
  fcst_model,
  parameter,
  lead_time           = seq(0, 48, 3),
  lags                = "0s",
  merge_lags          = TRUE,
  file_path           = ".",
  file_template       = "fctable",
  file_format         = "fctable",
  file_format_opts    = list(),
  drop_any_na         = TRUE,
  stations            = NULL,
  members             = NULL,
  valid_dttm          = NULL,
  accumulate          = TRUE,
  vertical_coordinate = c(NA_character_, "pressure", "model", "height"),
  get_lat_and_lon     = FALSE,
  force_param_name    = FALSE,
  meta_only           = FALSE,
  start_date          = NULL,
  end_date            = NULL,
  by                  = "6h",
  fcst_type           = NULL
) {

  use_dttm <- TRUE
  if (missing(dttm)) {
    if (any(sapply(list(start_date, end_date, by), is.null))) {
      stop(
        "If `dttm` is not passed, `start_date`, `end_date` ",
        "and `by` must be passed."
      )
    }
    lifecycle::deprecate_warn(
      "0.1.0",
      I(paste(
        "The use of `start_date`, `end_date`, and `by`",
        "arguments of `read_point_forecast()`"
      )),
      "read_point_forecast(dttm)"
    )
    use_dttm <- FALSE
    dttm <- harpCore::seq_dttm(start_date, end_date, by)
  }

  if (!is.null(fcst_type)) {
    lifecycle::deprecate_warn(
      "0.2.3",
      "read_point_forecast(fcst_type)"
    )
  }

  fcst_suffix <- "_mbr[[:digit:]]{3}|_det$|^forecast$|^fcst$"

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

  # Get lead time in seconds - the numeric values of the names will be used
  # to generate file names
  lead_time <- harpCore::to_seconds(lead_time)

  if (!is.null(parameter)) {
    parameter  <- parse_harp_parameter(parameter, vertical_coordinate)
    param_name <- parameter$fullname
    if (parameter$accum > 0 && accumulate) {
      param_name <- parameter$basename
      #lead_time  <- lead_time[lead_time >= parse_accum(parameter)]
    }
    if (is_temp(parameter) && !force_param_name) {
      param_name <- parameter$basename
    }
  }

  if (length(lead_time) < 1) {
    stop(
      "At least one of lead_time must be greater than or equal to the accumulation time: ",
      parse_accum(parameter), parameter$acc_unit,
      call. = FALSE
    )
  }


  # Make sure to get all lead times needed for accumulation
  if (accumulate && parameter$accum > 0) {
    acc                  <- parameter$accum
    acc_lead_time        <- harpCore::fix_seconds_names(lead_time - acc)
    acc_lead_time        <- acc_lead_time[acc_lead_time >= 0]
    lead_time_all        <- union(lead_time, acc_lead_time)
    names(lead_time_all) <- union(names(lead_time), names(acc_lead_time))
  } else {
    lead_time_all <- lead_time
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
    dplyr::inner_join(
      template_table,
      by = intersect(colnames(.), colnames(template_table))
    )

  file_names <- purrr::pmap(
    as.list(lag_table),
    ~ generate_filenames(
      file_path     = file_path,
      file_date     = dttm,
      lags          = ..2,
      parameter     = param_name,
      fcst_model    = gsub("_unshifted", "", ..1),
      lead_time     = harpCore::extract_numeric(names(lead_time)),
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
    action <- warning
    if (all(purrr::map_int(available_files, length) < 1)) {
      action <- stop
    }
    action("No forecast files found for: \n", paste(model_with_no_files, collapse = "\n"), call. = FALSE)
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

  fcst_model <- fcst_model[purrr::map_int(available_files, length) > 0]
  available_files <- available_files[
    purrr::map_int(available_files, length) > 0
  ]
  lag_table <- lag_table[lag_table[["fcst_model"]] %in% fcst_model, ]

  lag_table <- dplyr::left_join(
    lag_table, members,
    by = intersect(colnames(lag_table), colnames(members))
  )

  read_func <- paste("read", file_format, sep = "_")
  if (!exists(read_func) || !is.function(get(read_func))) {
    cli::cli_abort(c(
      "Cannot find a function to read {file_format} files.",
      "x" = "{read_func} not found!"
    ))
  } else {
    read_func <- get(read_func)
  }

  fmt_opts_func <- paste(file_format, "opts", sep = "_")
  if (exists(fmt_opts_func) && is.function(get(fmt_opts_func))) {
    file_format_opts <- do.call(get(fmt_opts_func), file_format_opts)
  }

  fcst <- purrr::pmap(
    list(
      available_files,
      lag_table$lag,
      lag_table$members
    ),
    ~read_func(
      .x,
      harpCore::as_unixtime(dttm) - harpCore::to_seconds(.y),
      lead_time        = harpCore::fix_seconds_names(
        lead_time_all + harpCore::to_seconds(.y)
      ),
      stations         = stations,
      members          = ..3,
      valid_dttm       = valid_dttm,
      param            = parameter,
      get_latlon       = get_lat_and_lon,
      force_param_name = force_param_name,
      use_dttm         = use_dttm,
      meta_only        = meta_only,
      complete_cases   = drop_any_na,
      file_format_opts = file_format_opts
    )
  )

  lt_units <- lapply(fcst, function(x) x$lead_unit)
  zero_lt  <- lapply(fcst, function(x) x$lead_has_zero)
  fcst     <- lapply(fcst, function(x) dplyr::collect(x$fcst, n = Inf))

  if (!meta_only) {
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
      lt_units  <- lt_units[which(!no_members)]
      zero_lt   <- zero_lt[which(!no_members)]
    }

    fcst <- purrr::map(fcst, dplyr::filter_at, dplyr::vars(dplyr::matches(fcst_suffix)), drop_function)

    if (parameter$accum > 0 && accumulate) {

      if (
        all(
          vapply(
            fcst, function(x) inherits(x, "arrow_dplyr_query"), logical(1)
          )
        )
      ) {

        if (interactive()) {
          pb     <- cli::cli_progress_bar("Decumulating", total = length(fcst))
          pb_env <- environment()
        } else {
          pb_env = NULL
        }
        fcst <- mapply(
          function(fc, lg, lt_unit, lt_zero) {
            arrow_decum_fcst(
              fc,
              harpCore::fix_seconds_names(lead_time + lg),
              harpCore::fix_seconds_names(acc_lead_time + lg),
              acc,
              lt_unit,
              lt_zero,
              pb_env
            )
          },
          fcst,
          harpCore::to_seconds(lag_table$lag),
          lt_units,
          zero_lt,
          SIMPLIFY = FALSE
        )

      } else {

        accum <- parse_accum(parameter)

        fcst <- purrr::map(
          fcst,
          ~ accumulate_forecast(
            .x,
            accum,
            parameter$acc_unit,
            check_leads = FALSE
          )
        )

        # accumulate_forecast returns a vector of missing lead times rather than data if some lead times
        # to compute an accumlation are missing.

        # DON'T THINK I NEED THIS ANYMORE - ALL LEAD TIMES ARE READ NOW


        # if (any(purrr::map_lgl(fcst_accum, is.numeric))) {
        #   lead_time_accum <- fcst_accum
        #   unread_leads <- which(purrr::map_lgl(fcst_accum, is.numeric))
        #
        #   file_names <- purrr::pmap(
        #     list(
        #       lag_table$fcst_model[unread_leads],
        #       lag_table$lag[unread_leads],
        #       lead_time_accum[unread_leads],
        #       lag_table$file_template[unread_leads]
        #     ),
        #     ~generate_filenames(
        #       file_path     = file_path,
        #       file_date     = dttm,
        #       lags          = .y,
        #       parameter     = param_name,
        #       fcst_model    = gsub("_unshifted", "", .x),
        #       lead_time     = ..3 - readr::parse_number(.y),
        #       file_template = ..4
        #     )
        #   )
        #
        #   fcst_lead_time_accum <- purrr::pmap(
        #     list(
        #       purrr::map(file_names, ~ .x[file.exists(.x)]),
        #       lag_table$lag[unread_leads],
        #       lead_time_accum[unread_leads],
        #       lag_table$members[unread_leads]
        #     ),
        #     ~ read_fctable(
        #       .x,
        #       harpCore::as_unixtime(dttm) - harpCore::to_seconds(.y),
        #       lead_time  = ..3,
        #       stations   = stations,
        #       members    = ..4,
        #       param      = parameter,
        #       get_latlon = get_lat_and_lon
        #     )
        #   ) %>% purrr::map(dplyr::filter_at, dplyr::vars(dplyr::matches(fcst_suffix)), drop_function)
        #
        #   no_data_for_accum <- which(vapply(fcst_lead_time_accum, nrow, numeric(1)) < 1)
        #
        #   if (any(no_data_for_accum)) {
        #     missing_leads <- unique(unlist(lead_time_accum))
        #     cli::cli_abort(c(
        #       "Unable to find data to compute accumulations.",
        #       "x" = "Cannot find lead times: {missing_leads} in sqlite files."
        #     ))
        #   }
        #
        #   fcst[unread_leads]       <- purrr::map2(fcst[unread_leads], fcst_lead_time_accum, dplyr::bind_rows)
        #   fcst_accum[unread_leads] <- purrr::map(
        #     fcst[unread_leads],
        #     ~ accumulate_forecast(
        #       tidyr::gather(.x, dplyr::matches(fcst_suffix), key = "member", value = "forecast"),
        #       accum,
        #       parameter$acc_unit,
        #       check_leads = FALSE
        #     )
        #   )
        #
        # }

     }
    }
  }

  ### Multimodel ensembles should be handled by the verification functions

  # split_sub_models <- function(df, .member_regexp) {
  #
  #   meta_cols  <- c(
  #     "SID", "fcdate", "leadtime", "validdate", "fcst_cycle",
  #     "fcst_dttm", "valid_dttm", "lead_time"
  #   )
  #   sub_models <- stringr::str_extract(
  #     names(df),
  #     .member_regexp
  #   ) %>%
  #     stats::na.omit() %>%
  #     unique()
  #
  #   if (length(sub_models) == 1) {
  #     df
  #   } else {
  #     df <- purrr::map(
  #       sub_models,
  #       ~ dplyr::select(df, dplyr::any_of(meta_cols), dplyr::contains(.x))
  #     ) %>%
  #       rlang::set_names(sub_models)
  #     as_harp_list(df)
  #   }
  #
  # }

  # fcst <- purrr::map(fcst, split_sub_models, member_regexp)

  if (merge_lags) {
    lag_table[["lt_units"]] <- unlist(lt_units)
    fcst                    <- lag_and_join(fcst, lag_table, meta_only)
  } else {
    fcst <- merge_names_df(fcst, lag_table$fcst_model)
  }

  #Collect the data if necessary
  if (is.null(file_format_opts$collect) || file_format_opts$collect) {
    fcst <- purrr::imap(
      fcst, function(ds, nm) collect_dataset(ds, nm, names(lead_time))
    )
  }




  fcst <- mapply(
    function(x, y) dplyr::select(
      dplyr::mutate(x, fcst_model = y),
      dplyr::all_of("fcst_model"),
      dplyr::everything()
    ),
    fcst,
    names(fcst),
    SIMPLIFY = FALSE
  )

  fcst <- harpCore::as_harp_list(fcst)
  attr(fcst, "missing_files") <- missing_files

  if (length(fcst) == 1) {
    return(fcst[[1]])
  }

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
lag_and_join <- function(fcst_list, lags_df, meta_only) {

  lag_seconds <- harpCore::to_seconds(lags_df$lag)
  lag_lead    <- mapply(
    function(lt, unit) {
      harpCore::extract_numeric(harpCore::from_seconds(lt, unit))
    },
    lag_seconds,
    lags_df$lt_units
  )

  non_zero_values <- which(lag_seconds > 0)
  if (!any(non_zero_values)) {
    fcst_list <- merge_names_df(fcst_list, lags_df$fcst_model)
    return(fcst_list)
  }

  fcst_list[non_zero_values] <- mapply(
    function(fc, lg, lg_ld) {
      col_names   <- get_data_col_names(fc)
      fc_dttm_col <- intersect(c("fcdate", "fcst_dttm"), col_names)
      lt_col      <- intersect(c("leadtime", "lead_time"), col_names)
      dplyr::mutate(
        fc,
        {{fc_dttm_col}} := .data[[fc_dttm_col]] + lg,
        {{lt_col}}      := as.integer(.data[[lt_col]] - lg_ld) #,
        #fcst_cycle = strftime(.data[[fc_dttm_col]], "%H", tz = "UTC")
      )
    },
    fcst_list[non_zero_values],
    lag_seconds[non_zero_values],
    lag_lead[non_zero_values],
    SIMPLIFY = FALSE
  )

  # Add a lag suffix to all lagged members
  lag_suffix <- function(chr_lag) {
    if (harpCore::to_seconds(chr_lag) == 0) {
      return("")
    }
    paste0("_lag", chr_lag)
  }

  if (!meta_only) {
    fcst_list <- purrr::map2(
      fcst_list,
      lags_df[["lag"]],
      function(a, b) {
        dplyr::rename_with(
          a, ~paste0(.x, lag_suffix(b)), dplyr::matches("_mbr[[:digit:]]{3}")
        )
      }
    )
  }

  fcst_list <- split(fcst_list, lags_df$fcst_model)
  lag_idx <- which(vapply(fcst_list, function(x) length(x) > 1, logical(1)))
  if (length(lag_idx) > 0) {
    message(cli::col_br_blue("Constructing lagged ensemble "), appendLF = FALSE)
  }
  fcst_list <- lapply(
    fcst_list,
    function(x) {
      Reduce(function(a, b) suppressMessages(dplyr::inner_join(a, b)), x)
    }
  )
  if (length(lag_idx) > 0) {
    message(cli::col_br_green(cli::symbol$tick))
  }

  purrr::imap(
    fcst_list,
    function(x, nm) {
      if (!is.element("fcst_model", get_data_col_names(x))) {
        x <- dplyr::mutate(x, fcst_model = nm)
      }
      x
    }
  )

}

# For arrow datasets, the actual data can be nested beneath a bunch of queries.
# Recursive function to get the column names from $.data
get_data_col_names <- function(x, tries = 1, max_tries = 10) {
  col_names <- colnames(x)
  if (is.null(col_names) && tries < max_tries) {
    return(get_data_col_names(x$.data, tries + 1))
  }
  if (is.null(col_names)) {
    cli::cli_abort(c(
      "Cannot extract column names after nesting down {tries} levels",
      "i" = "There was some problem reading the data - try without lagging."
    ))
  }
  col_names
}

get_data_num_rows <- function(x, tries = 1, max_tries = 10) {
  num_rows <- nrow(x)
  if (is.na(num_rows) && tries < max_tries) {
    return(get_data_num_rows(x$.data, tries + 1))
  }
  if (is.null(num_rows)) {
    cli::cli_abort(c(
      "Cannot extract number of rows after nesting down {tries} levels",
      "i" = "There was some problem reading the data - try without lagging."
    ))
  }
  num_rows
}



