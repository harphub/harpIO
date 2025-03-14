# #' Parameters for vobs and vfld files
# #'
# #' Internal function to convert parameter names from vobs or vfld files into
# #' harp format parameters. Use parse_v_parameter_synop for SYNOP parameters and
# #' parse_v_parameter_temp for TEMP parameters.
# #' @param v_parameter vobs or vfld parameter
# #' @param param_type type of parameter - "SYNOP" or "TEMP"
# #' @return A list with the harp paramater name and the units of the parameter.
# #'
parse_v_parameter <- function(
  v_parameter, param_type,
  param_defs = getExportedValue("harpIO", "harp_params")
) {

  v_elements <- purrr::map(param_defs, "v")

  v_params <- v_elements %>%
    purrr::map_lgl(~!is.null(.x)) %>%
    select_elements(v_elements)

  v_param_num <- which(
    purrr::map_lgl(
      v_params,
      ~is.element(v_parameter, .x[["name"]]) && .x[["type"]] == param_type
    )
  )

  if (length(v_param_num) < 1) {
    return(
      list(
        harp_param = v_parameter, param_units = "unknown"
      )
    )
  }

  v_params[[v_param_num]][c("harp_param", "param_units")]

}

parse_v_parameter_synop <- function(
  v_parameter, param_defs = getExportedValue("harpIO", "harp_params")
) {
  parse_v_parameter(v_parameter, "SYNOP", param_defs)
}

parse_v_parameter_temp <- function(
  v_parameter, param_defs = getExportedValue("harpIO", "harp_params")
) {
  parse_v_parameter(v_parameter, "TEMP", param_defs)
}

select_elements <- function(el, x) {
  x[el]
}

