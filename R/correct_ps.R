
# pmslcom function from Monitor (mod/module_functions.f90)
pmslcom <- function(df) {
  
  # Constants
  rair   <- 287.04
  gravit <- 9.80665
  tlapse <- 0.0065
  
  tdf <- df %>% dplyr::mutate(zdiff = (model_elevation-elev)*gravit,
                              tstar = t2m,
                              alfa  = NA,
                              tzero = tstar + (tlapse*zdiff/gravit))
  
  # Add logicals
  tdf <- tdf %>% dplyr::mutate(tstar_255 = tstar < 255.0,
                               tzero_290 = tzero > 290.5,
                               tstar_290 = tstar <= 290.5)
  
  # Compute tstar
  tdf <- dplyr::mutate(tdf,
                       tstar = dplyr::case_when(
                         tstar_255 ~ 0.5*(tstar + 255.0),
                         .default = dplyr::case_when(
                           tzero_290 ~ dplyr::case_when(
                             tstar_290 ~ tstar,
                             .default = 0.5*(290.5+tstar)),
                           .default = tstar)
                         )
                       )
  # Compute alfa
  tdf <- mutate(tdf,
                alfa = dplyr::case_when(
                  tstar_255 ~ tlapse*rair/gravit,
                  .default = dplyr::case_when(
                    tzero_290 ~ dplyr::case_when(
                      tstar_290 ~ rair*(290.5 - tstar)/zdiff,
                      .default = 0.0),
                    .default = tlapse*rair/gravit)
                  )
                )
  
  tdf <- tdf %>%
    dplyr::mutate(arg = zdiff/rair/tstar,
                  station_data = station_data*exp(arg*(1-0.5*alfa*arg+(1/3)*(alfa*arg)**2)))
  
  return(tdf)
  
}

correct_ps <- function(data_df, opts) {

  if (is.null(opts)) {
    stop(
      "To correct surface pressure, options must be passed via",
      "transformation_opts = interpolate_opts(...)",
      call. = FALSE
    )
  }

  if (is.null(opts[["correct_ps"]]) || !opts[["correct_ps"]]) {
    message("Height correction for surface pressure NOT selected.")
    return(list(data_df = data_df, opts = opts))
  }
  
  if (!is.element("T2m", unique(data_df[["parameter"]]))) {
    message("Did not find uncorrected T2m for height correction of surface pressure - skipping")
    return(list(data_df = data_df, opts = opts))
  }

  message("Doing height correction for surface pressure.")

  if (is.null(opts[["model_elev"]]) && !is.element("model_elevation", colnames(data_df))) {

    if (is.null(opts[["clim_field"]])) {
      stop(
        "Unable to height correct surface pressure. No model elevation supplied.\n",
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
        "No 'clim_param' provided for surface pressure correction. Use\n",
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
        "Don't know how to correct surface pressure with clim_param = '",
        opts[["clim_param"]], "'.",
        call. = FALSE
      )
    }
    opts[["model_elev"]][["model_elevation"]] <-
      opts[["model_elev"]][["model_elevation"]] * elev_multiplier
  }

  if (is.null(opts[["stations"]])) {
    warning(
      "No stations supplied for surface pressure corrrection. Using default station list.",
      call. = FALSE, immediate. = TRUE
    )
    opts[["stations"]] <- harpCore::station_list
  }

  if (!is.element("elev", colnames(opts[["stations"]]))) {
    stop("No 'elev' column found in 'stations' for surface pressure correction.", call. = FALSE)
  }

  if (!is.element("model_elevation", colnames(data_df))) {
    data_df <- dplyr::inner_join(
      data_df,
      opts[["model_elev"]][c("SID", "model_elevation")],
      by = "SID"
    )
  }

  data_ps <- dplyr::filter(data_df, tolower(.data[["parameter"]]) == "ps")
  data_t2m <- dplyr::filter(data_df, tolower(.data[["parameter"]]) == "t2m") %>%
    dplyr::rename(t2m = "station_data")
  
  if (!is.element("elev", colnames(data_ps))) {
    data_ps <- dplyr::inner_join(data_ps, opts[["stations"]][c("SID", "elev")], by = "SID")
  }

  # Extra check in case -99 is used as missing elev in stations (e.g. from vobs)
  data_ps <- dplyr::filter(data_ps,
                           .data[["elev"]] > -9999,
                           .data[["elev"]] != -99)
  
  # Merge ps and t2m
  data_ps <- dplyr::inner_join(
    data_ps,
    data_t2m %>% dplyr::select(-parameter,-units),
    by = dplyr::setdiff(colnames(data_ps),c("parameter","units","station_data","elev"))
  )
  
  # Compute correction
  ps_cor <- pmslcom(data_ps)

  if (is.null(opts[["keep_model_ps"]])) opts[["keep_model_ps"]] <- FALSE

  if (opts[["keep_model_ps"]]) {
    data_df[["parameter"]][tolower(data_df[["parameter"]]) == "ps"] <- paste0(
      data_df[["parameter"]][tolower(data_df[["parameter"]]) == "ps"],
      "_uncorrected"
    )
  } else {
    data_df <- dplyr::filter(data_df, tolower(.data[["parameter"]]) != "ps")
  }

  list(
    data_df = dplyr::bind_rows(
      data_df, ps_cor[colnames(data_df)]
    ),
    opts = opts
  )

}
