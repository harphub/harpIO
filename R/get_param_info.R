get_param_info <- function(
  param, file_format, param_defs = getExportedValue("harpIO", "harp_params")
) {

  param_names <- names(param_defs)

  list_element <- which(param_names == tolower(param[["fullname"]]))

  if (length(list_element) < 1) {

    list_element <- param_from_other_names(
      tolower(param[["fullname"]]),
      param_defs
    )

  }

  if (length(list_element) < 1) {

    list_element <- which(param_names == tolower(param[["basename"]]))

  }


  if (length(list_element) < 1) {
    return(
      list(
        param_info = list(
          name = param[["fullname"]]
        )
      )
    )
  }

  result <- list(
    param_info = param_defs[[list_element]][[file_format]],
    param_func = get_function(
      names(param_defs)[[list_element]],
      file_format,
      param_defs
    )
  )
  # FIXME: if a function is defined for only 1 format
  #        it will ALSO appear in param_info !
  #        So eventually you may have 2 $func entries.
  result$param_info$func <- NULL
  result
}

param_from_other_names <- function(param, param_defs) {

  param <- paste0("^", tolower(param), "$")
  lapply(param_defs, function(x) x[["other_names"]]) %>%
    vapply(function(x) any(grepl(param, x)), TRUE) %>%
    which()

}
