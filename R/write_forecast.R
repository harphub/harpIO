write_forecast <- function(df, opts) {

  # Get the place holders from the template and only select columns that match
  template <- get_template(opts[["template"]])

  template_subs <- gsub(
    "\\{|\\}", "",
    unlist(regmatches(template, gregexpr("(\\{.*?\\})", template)))
  )

  # Make sure fcdate and parameter have a value where there are no data
  missing_dates <- df[is.na(df[["fcst_dttm"]]), ]
  if (nrow(missing_dates) > 0) {
    add_col <- function(.df, col_name, col_data) {
      .df[[col_name]] <- list(col_data)
      if (tidyr_new_interface()) {
        .df <- tidyr::unnest(.df, tidyselect::all_of(col_name))
      } else {
        .df <- tidyr::unnest(.df)
      }
      .df
    }
    data_for_missing <- lapply(
      as.list(df[c("fcst_dttm", "parameter", "lead_time")]),
      function(x) as.vector(stats::na.omit(unique(x)))
    )
    missing_dates <- purrr::map_dfr(
      1:nrow(missing_dates),
      function(x) {
        res <- missing_dates[x, ]
        for (i in seq_along(data_for_missing)) {
          res <- add_col(res, names(data_for_missing)[i], data_for_missing[[i]])
        }
        res
      }
    )
    df <- dplyr::bind_rows(df[!is.na(df[["fcst_dttm"]]), ], missing_dates)
  }

  # Get the correct column names
  unused_subs   <- c(
    "YYYY", "MM", "DD", "HH", "mm", "ss", "M", "D", "H", "m", "s",
    "eps_model", "det_model",
    "file_path"
  )
  template_subs <- setdiff(template_subs, unused_subs)

  df[["file_date"]]    <- harpCore::as_YMDhms(df[["fcst_dttm"]])
  df[["lags"]]         <- NULL

  df <- replace_colname(df, "station_data", "forecast")

  if (any(grepl("MBR", template_subs))) {
    df[["MBR"]] <- df[["members_out"]]
  }

  if (any(grepl("LDT", template_subs))) {
    df[["LDT"]] <- df[["lead_time"]]
  }

  if (is.element("sub_model", template_subs) &&
      !is.element("sub_model", colnames(df)) ||
      all(is.na(df[["sub_model"]]))
  ) {
    df[["sub_model"]] <- df[["fcst_model"]]
  }

  group_cols <- intersect(
    colnames(df),
    c("file_date", "fcst_model", "eps_model", "det_model", "sub_model")
  )

  if (tidyr_new_interface()) {
    df_filenames <- tidyr::nest(df, data = -tidyr::one_of(group_cols))
  } else {
    df_filenames <- dplyr::group_by(df, !!!rlang::syms(group_cols)) %>%
      tidyr::nest()
  }

  template_subs <- gsub("[[:digit:]]+$", "", template_subs)

  # Generate the file names
  filename_args <- c(
    as.list(df_filenames[colnames(df_filenames) != "data"]),
    as.list(df_filenames[["data"]][[1]][setdiff(template_subs, group_cols)])
  )

  filename_args <- lapply(filename_args, function(x) as.vector(stats::na.omit(unique(x))))

  df_filenames  <- do.call(
    generate_filenames,
    c(
      filename_args,
      list(
        file_path      = opts[["path"]],
        file_template  = template,
        filenames_only = FALSE
      )
    )
  )

  df <- suppressMessages(dplyr::inner_join(df, df_filenames[colnames(df_filenames) != "lags"]))

  #df <- replace_colname(df, "lead_time", "leadtime")
  df <- replace_colname(df, "members_out", "member")

  if (!is.element("member", colnames(df))) {
    df[["member"]] <- NA_character_
  }

  df[["member"]][is.na(df[["member"]])] <- "det"
  df[["member"]] <- paste(df[["sub_model"]], df[["member"]], sep = "_")
  bad_rows <- grep("^NA_|^_", df[["member"]])

  if (length(bad_rows) > 0) {
    df[["member"]][bad_rows] <- paste(
      df[["eps_model"]][bad_rows],
      sub("^NA_|^_", "", df[["member"]][bad_rows]),
      sep = "_"
    )
  }

  if (length(bad_rows) > 0) {
    df[["member"]][bad_rows] <- paste(
      df[["det_model"]][bad_rows],
      sub("^NA_|^_", "", df[["member"]][bad_rows]),
      sep = "_"
    )
  }

  df[["lead_time"]]  <- as.integer(df[["lead_time"]])
  df[["fcst_dttm"]]    <- as.integer(df[["fcst_dttm"]])
  df[["valid_dttm"]] <- as.integer(df[["valid_dttm"]])

  possible_cols <- c(
    "file_name", "fcst_dttm", "valid_dttm",  "lead_time", "SID", "lat", "lon",
    "model_elevation", "p", "ml", "z", "member", "parameter", "units", "forecast",
    "level_type", "level"
  )

  df <- df[intersect(colnames(df), possible_cols)]

  if (tidyr_new_interface()) {
    df <- tidyr::nest(df, data = -tidyr::one_of("file_name"))
  } else {
    df <- tidyr::nest(dplyr::group_by(df, .data[["file_name"]]))
  }

  if (opts[["format"]] %in% c("sqlite", "fctable")) {

    purrr::walk2(
      df[["data"]],
      df[["file_name"]],
      write_fctable_to_sqlite,
      primary_key       = opts[["index_cols"]],
      synchronous       = opts[["synchronous"]],
      journal_mode      = opts[["journal_mode"]],
      remove_model_elev = opts[["remove_model_elev"]]
    )

  }

  if (opts[["format"]] %in% c("fcparquet", "parquet")) {

    purrr::walk2(
      df[["data"]],
      df[["file_name"]],
      write_fctable_to_parquet,
      hive_partitioning = opts[["partitioning"]]
    )

  }

}


replace_colname <- function(df, old_name, new_name) {

  if (is.element(old_name, colnames(df))) {
    colnames(df)[colnames(df) == old_name] <- new_name
  }
  df

}
