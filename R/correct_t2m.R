correct_t2m <- function(data_df, opts) {

  if (is.null(opts)) {
    stop(
      "To correct 2m temperature, options must be passed via",
      "transformation_opts = interpolate_opts(...)",
      call. = FALSE
    )
  }

  if (is.null(opts[["correct_t2m"]]) || !opts[["correct_t2m"]]) {
    message("Height correction for 2m temperature NOT selected.")
    return(list(data_df = data_df, opts = opts))
  }

  message("Doing height correction for 2m temperature.")

  if (is.null(opts[["lapse_rate"]])) {
    stop(
      "No lapse rate supplied. Cannot correct 2m temperature.\n",
      "Use e.g. transformation_opts = interpolate_opts(lapse_rate = 0.0065),",
      "OR transformation_opts = interpolate_opts(correct_t2m = FALSE).",
      call. = FALSE
    )
  }

  if (is.null(opts[["model_elev"]]) && !is.element("model_elevation", colnames(data_df))) {

    if (is.null(opts[["clim_field"]])) {
      stop(
        "Unable to height correct 2m temperature. No model elevation supplied.\n",
        "Use transformation_opts = interpolate_opts(clim_file = ..., clim_param = ...)",
        call. = FALSE
      )
    }

    opts[["model_elev"]] <- transform_geofield(
      opts[["clim_field"]], "interpolate", opts
    )

    names(opts[["model_elev"]])[names(opts[["model_elev"]]) == "station_data"] <- "model_elevation"

    if (is.null(opts[["clim_param"]])) {
      stop(
        "No 'clim_param' provided for 2m temperature correction. Use\n",
        "transformation_opts = interpolate_opts(clim_param = '<param>') to set.",
        call. = FALSE
      )
    }

    height_params <- c("terrain", "elev", "elevation", "altitude", "topo", "oro")
    geo_params    <- c("sfc_geopotential", "sfc_geo", "z0m", "z0")

    if (opts[["clim_param"]] %in% height_params) {
      elev_multiplier <- 1
    } else if (opts[["clim_param"]] %in% geo_params) {
      elev_multiplier <- 1 / 9.08665
    } else {
      stop(
        "Don't know how to correct 2m temperature with clim_param = '",
        opts[["clim_param"]], "'.",
        call. = FALSE
      )
    }
    opts[["model_elev"]][["model_elevation"]] <-
      opts[["model_elev"]][["model_elevation"]] * elev_multiplier
  }

  if (is.null(opts[["stations"]])) {
    warning(
      "No stations supplied for 2m temperature corrrection. Using default station list.",
      call. = FALSE, immediate. = TRUE
    )
    opts[["stations"]] <- get("station_list")
  }

  if (!is.element("elev", colnames(opts[["stations"]]))) {
    stop("No 'elev' column found in 'stations' for 2m temperature correction.", call. = FALSE)
  }

  if (!is.element("model_elevation", colnames(data_df))) {
    data_df <- dplyr::inner_join(
      data_df,
      opts[["model_elev"]][c("SID", "model_elevation")],
      by = "SID"
    )
  }

  data_t2m <- dplyr::filter(data_df, tolower(.data[["parameter"]]) == "t2m")

  if (!is.element("elev", colnames(data_t2m))) {
    data_t2m <- dplyr::inner_join(data_t2m, opts[["stations"]][c("SID", "elev")], by = "SID")
  }

  data_t2m <- dplyr::filter(data_t2m, .data[["elev"]] > -9999)

  data_t2m[["station_data"]] <- data_t2m[["station_data"]] +
    opts[["lapse_rate"]] * (data_t2m[["model_elevation"]] - data_t2m[["elev"]])

  if (is.null(opts[["keep_model_t2m"]])) opts[["keep_model_t2m"]] <- FALSE

  if (opts[["keep_model_t2m"]]) {
    data_df[["parameter"]][tolower(data_df[["parameter"]]) == "t2m"] <- paste0(
      data_df[["parameter"]][tolower(data_df[["parameter"]]) == "t2m"],
      "_uncorrected"
    )
  } else {
    data_df <- dplyr::filter(data_df, tolower(.data[["parameter"]]) != "t2m")
  }

  list(
    data_df = dplyr::bind_rows(
      data_df, data_t2m[colnames(data_df)]
    ),
    opts = opts
  )

}
