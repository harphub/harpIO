get_param_info <- function(
  param, file_format, param_defs = harp_params()
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

  list(
    param_info = param_defs[[list_element]][[file_format]],
    param_func = get_function(
      names(param_defs)[[list_element]],
      file_format,
      param_defs
    )
  )

}

param_from_other_names <- function(param, param_defs) {

  lapply(param_defs, function(x) x[["other_names"]]) %>%
    vapply(function(x) any(grepl(tolower(param), x)), TRUE) %>%
    which()

}
