# #' Internal function to get NetCDF parameter name from a HARP parameter name
# #'
# #' @param param HARP parameter name.
# #' @param vc The vertical coordinate. Set to NA for no vertical coordinate,
# #'   otherwise "pressure" or "model".
# #' @param opts Options for netcdf files as generated by
# #'   \code{\link{netcdf_opts}}. When certain options_set values are passed the
# #'   options may be modified depending on the parameter.
# #' @return A list with the harp parameter name, the netcdf parameter name and
# #'   the netcdf options, that may have been modified.

get_netcdf_param_info <- function(
  param, vc = NA_character_, opts = netcdf_opts(),
  param_defs = get("harp_params")
) {

  if (!inherits(param, "harp_parameter")) {
    param <- parse_harp_parameter(param, vertical_coordinate = vc)
  }

  if (opts[["force_param_name"]]) {
    return(
      list(
        harp_param = param,
        nc_param   = param[["fullname"]],
        func       = NA,
        opts       = opts
      )
    )
  }

  if (!is.null(opts[["param_find"]])) {
    if (is.element(param[["fullname"]], names(opts[["param_find"]]))) {
      return(
        list(
          harp_param = param,
          nc_param   = opts[["param_find"]][[param[["fullname"]]]],
          func       = NA,
          opts       = opts
        )
      )
    }
  }

  if (grepl("wrf", opts[["options_set"]])) {

    nc_param_info <- get_param_info(param, "wrf", param_defs)

  } else {

    nc_param_info <- get_param_info(param, "netcdf", param_defs)

  }

  func          <- nc_param_info[["param_func"]]
  nc_param_info <- nc_param_info[["param_info"]]

  if (is.null(nc_param_info[["name"]])) {
    warning(
      param[["fullname"]], " is not a built in parameter name for netcdf ",
      "files. Using name as is.",
      call. = FALSE, immediate. = TRUE
    )
  }

  if (is.list(nc_param_info[["suffix"]])) {

      if (is.list(nc_param_info[["name"]])) {
        nc_param_info[["name"]] <- lapply(
          nc_param_info[["name"]],
          paste0,
          nc_param_info[["suffix"]][[param[["level_type"]]]]
        )
      } else {
        nc_param_info[["name"]] <-paste0(
          nc_param_info[["name"]],
          nc_param_info[["suffix"]][[param[["level_type"]]]]
        )
      }

  }

  # if (grepl("^met_norway", opts[["options_set"]])) {
  #   n_last <- 3
  #   suffix <- substr(
  #     nc_param_info[["name"]],
  #     nchar(nc_param_info[["name"]]) - n_last + 1,
  #     nchar(nc_param_info[["name"]])
  #   )
  #   opts[["z_var"]] <- switch(
  #     suffix,
  #     "_pl" = "pressure",
  #     "_ml" = "hybrid",
  #     opts[["z_var"]]
  #   )
  # }
  # if (opts[["options_set"]] %in% c("met_norway_eps", "met_norway_det"))  {
  #   if (grepl("surface", nc_param_info[["name"]])) {
  #     opts[["z_var"]] <- "height0"
  #   }
  # }
  #
  # if (opts[["options_set"]] %in% c("met_norway_ifsens", "met_norway_ifshires"))  {
  #   if (grepl("_+[[:digit:]]m$", nc_param_info[["name"]])) {
  #     opts[["z_var"]] <- "surface"
  #   }
  # }
  #
  # if (grepl("^met_norway", opts[["options_set"]])) {
  #   if (grepl("_pl$", nc_param_info[["name"]])) {
  #     opts[["z_var"]] <- "pressure"
  #   }
  #   if (grepl("_ml$", nc_param_info[["name"]])) {
  #     opts[["z_var"]] <- "hybrid"
  #   }
  # }

  list(
    harp_param = param,
    nc_param   = nc_param_info[["name"]],
    func       = func,
    opts       = opts
  )

}
