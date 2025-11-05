#' FA parameter names
#'
#' a harp-style parameter is translated to the field name in
#' ALARO/AROME/HARMONIE model output in FA format
#'
#' @param param Parameter name
#' @param fa_type could be 'alaro' or 'arome'. These models store e.g.
#'   precipitation under a different name.
#' @param fa_vector A logical value. TRUE means that e.g. wind speed will be
#'   calculated from wind vector components, not searched as a separate field.
#' @param rotate_wind If TRUE, wind fields (U, V, direction)
#'   are rotated from model axes to earth axes.
#' @return A 16 character string, or in rare cases a vector of several strings
#'   denoting the components (e.g. total precipitation may be the sum of up to 4
#'   fields).
#' @export
get_fa_param_info <- function(
  param, vertical_coordinate = NA_character_,
  param_defs = getExportedValue("harpIO", "harp_params")
) {

  if (!inherits(param, "harp_parameter")) {
    param <- parse_harp_parameter(
      param, vertical_coordinate = vertical_coordinate
    )
  }

  # NOTE: the following allows for local exceptions to be implemented:
  # OBSOLETE --- get_param_info
  #if (existsFunction("fa_override")) {
  #  if (!is.null(fa_override(param$fullname))) return(fa_override(param$fullname))
  #}
  fa_info <- harpIO:::get_param_info(param, "fa", param_defs)
  func    <- fa_info[["param_func"]]
  fa_info <- fa_info[["param_info"]]

  if (is.null(fa_info[["level_type"]])) {
    fa_info[["level_type"]] <- param[["level_type"]]
  }

  # If level_type has many possibilities (grib gives c("surface", "model", "height", ...) )
  #   make sure to select only the desired one.
#  if (is.list(fa_info[["level_type"]])) {
#    grib_info[["level_type"]] <-
#      grib_info[["level_type"]][[param[["level_type"]]]]
#  }

  if (is.null(fa_info[["level"]])) {
    if (is.null(param[["level"]]) || is.na(param[["level"]])) {
      fa_info[["level"]] <- -999
    } else {
      fa_info[["level"]] <- param[["level"]]
    }
  }

  # -999 signifies "all available levels"
  # NOTE: for model level, you can not pass "-999" in only 3 characters...
  # Find a better way to signal "all available levels" in this case?
  # or leave this to be resolved in read_fa

  # NOTE: there may be alaro/arome entries, so fa_info$name might be a LIST
  # generic templates (there are exceptions!)

  # final field name will be sprintf($fa_template, $level, $name)
  # but we can not fill in the level yet, because it may be "-999"
  # and that depends on the file itself.
  # NOTE: 2m and 10m height, surface: only 1 component in the template, so use "%.0s" for level
  # NOTE: the final option (e.g. "unknown") is just to keep the fa name.
  # NOTE: We assume "CLS", "SURF" or other surface indicators are included inthe base name!
  fa_info[["fa_template"]] <- switch(fa_info[["level_type"]],
                        "model"    = "S%03i%-12.12s",
                        "height"   = if (fa_info$level %in% c(0, 2, 10)) "%.0s%-16.16s" else "H%05i%-10.10s" ,
                        "pressure" = "P%05i%-10.10s",
                        "isotherm" = "KB%03i%-11.11s",
                        "surface"  = "%.0s%-16.16s",
                        "%.0s%-16.16s")

  c(fa_info, list(func = func))
}
