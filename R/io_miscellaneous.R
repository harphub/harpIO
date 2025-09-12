# Miscellaneous unctions used in IO

get_lead_units <- function(fcst) {

  col_names <- get_data_col_names(fcst)

  lt_col      <- intersect(c("lead_time", "leadtime"), col_names)
  fc_dttm_col <- intersect(c("fcst_dttm", "fcdate"), col_names)
  vd_dttm_col <- intersect(c("valid_dttm", "validdate"), col_names)

  lt_check <- dplyr::collect(
    dplyr::select(
      dplyr::mutate(
        head(
          dplyr::filter(fcst, .data[[lt_col]] > 0), n = 1
        ),
        vt_ft_diff = as.numeric(.data[[vd_dttm_col]]) -
          as.numeric(.data[[fc_dttm_col]])
      ),
      dplyr::all_of(c(lt_col, "vt_ft_diff"))
    )
  )

  if (nrow(lt_check) < 1) {
    return("s")
  }

  if (lt_check[[lt_col]] == lt_check$vt_ft_diff) {
    return("s")
  }
  if (lt_check$vt_ft_diff / 60 == lt_check[[lt_col]]) {
    return("m")
  }
  if (lt_check$vt_ft_diff / 3600 == lt_check[[lt_col]]) {
    return("h")
  }
  if (lt_check$vt_ft_diff / (3600 * 24) == lt_check[[lt_col]]) {
    return("d")
  }
  if (lt_check$vt_ft_diff / (3600 * 24 * 7) == lt_check[[lt_col]]) {
    return("w")
  }
  NA
}

arrow_decum_fcst <- function(ff, lt, lt_acc, acc, lt_unit, lt_zero, pb_env) {

  col_names <- get_data_col_names(ff)

  lt_col      <- intersect(c("lead_time", "leadtime"), col_names)
  fc_dttm_col <- intersect(c("fcst_dttm", "fcdate"), col_names)

  lt       <- harpCore::extract_numeric(harpCore::from_seconds(lt, lt_unit))
  lt_acc   <- harpCore::extract_numeric(harpCore::from_seconds(lt_acc, lt_unit))
  acc      <- harpCore::extract_numeric(harpCore::from_seconds(acc, lt_unit))

  fc_regex <- "_det$|^fcst$|^forecast$|_mbr[0-9]{3}"
  fc_cols  <- grep(fc_regex, col_names, value = TRUE)

  ff_now <- dplyr::filter(ff, .data[[lt_col]] %in% lt)
  ff_acc <- dplyr::filter(
    dplyr::select(ff, dplyr::any_of(c(fc_dttm_col, lt_col, "SID", fc_cols))),
    .data[[lt_col]] %in% lt_acc
  )
  # For cases where there are no data at time zero, create a fake dataset of
  # zeros based on the accumulation time.

  if (!lt_zero) {
    ff_acc <- dplyr::full_join(
      ff_zero <- dplyr::mutate(
        dplyr::filter(ff_acc, .data[["lead_time"]] == acc),
        lead_time = 0L,
        dplyr::across(dplyr::any_of(fc_cols), ~0)
      ),
      ff_acc
    )
  }

  ff_acc <- dplyr::rename_with(
    ff_acc, ~paste0(.x, "_acc"), dplyr::matches(fc_regex)
  )
  ff_acc <- dplyr::mutate(ff_acc, lead_time = .data[[lt_col]] + acc)

  ff <- dplyr::inner_join(ff_now, ff_acc)
  for (fc_col in fc_cols) {
    ff <- dplyr::mutate(
      ff,
      dplyr::across(
        dplyr::any_of(fc_col),
        ~.x - .data[[paste0(fc_col, "_acc")]]
      )
    )
  }

  # Replace negative values with 0 -
  # they're just due to weird rounding behaviour
  ff <- dplyr::mutate(
    ff,
    dplyr::across(dplyr::any_of(fc_cols), ~ifelse(.x < 0, 0, .x))
  )
  if (!is.null(pb_env)) {
    cli::cli_progress_update(.envir = pb_env)
  }
  dplyr::select(ff, -dplyr::matches("_acc$"))
}

collect_dataset <- function(dataset, dataset_name = NULL, lead_time = NULL) {
  if (inherits(dataset, "harp_list_uncollected")) {
    return(harpCore::as_harp_list(
      purrr::imap(
        dataset, function(x, nm) collect_dataset(x, nm, lead_time = lead_time)
      )
    ))
  }
  message(
    cli::col_br_blue("Collecting dataset: "),
    cli::col_br_yellow(dataset_name), ifelse(is.null(dataset_name), "", " "),
    appendLF = FALSE
  )
  dataset <- dplyr::collect(dataset)
  colnames(dataset) <- suppressWarnings(harpCore::psub(
    colnames(dataset),
    c("fcdate", "validdate", "leadtime"),
    c("fcst_dttm", "valid_dttm", "lead_time")
  ))
  if (length(intersect(c("valid_dttm", "fcst_dttm"), colnames(dataset))) == 2) {
    dataset <- dplyr::mutate(
      dataset,
      lead_time = as.numeric(.data[["valid_dttm"]]) -
        as.numeric(.data[["fcst_dttm"]])
    )
  }
  dataset <- dplyr::mutate(
    dataset,
    dplyr::across(dplyr::ends_with("_dttm"), ~harpCore::unixtime_to_dttm(.x))
  )
  col_names <- get_data_col_names(dataset)
  sort_cols <- intersect(
    c("fcst_dttm", "SID", "lead_time", "valid_dttm"), col_names
  )
  dataset <- dplyr::arrange(dataset, dplyr::across(sort_cols))
  if (!is.null(lead_time)) {
    lt <- harpCore::to_seconds(lead_time)
    dataset <- dplyr::mutate(
      dataset,
      lead_time = names(lt)[match(.data[["lead_time"]], lt)]
    )
  }
  dataset <- dplyr::rename_with(
    dataset,
    ~suppressWarnings(harpCore::psub(
      .x,
      c("fcdate", "leadtime", "validdate"),
      c("fcst_dttm", "lead_time", "valid_dttm")
    ))
  )

  dataset <- dplyr::select(
    dataset,
    dplyr::any_of(c(
      "fcst_model", "sub_model", "anl_model",
      "fcst_dttm", "valid_dttm", "lead_time", "SID", "fcst_cycle",
      "parameter", "p", "m", "z"
    )),
    dplyr::matches("_det$"),
    dplyr::matches("_mbr[[:digit:]]+$"),
    dplyr::matches("_mbr[[:digit:]]+_lag[[:digit:]]*"),
    dplyr::everything()
  )

  if (nrow(dataset) < 1) {
    if (is.null(dataset_name)) {
      dataset_name_msg <- "."
    } else {
      dataset_name_msg <- paste0(" for ", dataset_name, ".")
    }
    message(cli::col_br_red(cli::symbol$cross))
    cli::cli_warn("No data found{dataset_name_msg}")
    return(dataset)
  }

  dataset <- dplyr::select(
    dataset,
    dplyr::where(~!all(is.na(.x)))
  )

  message(cli::col_br_green(cli::symbol$tick))
  harpCore::as_harp_df(dataset)
}


