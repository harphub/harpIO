get_function <- function(
  param, file_format, param_defs = getExportedValue("harpIO", "harp_params")
) {

  param_info <- param_defs[[param]][[file_format]]
  func       <- param_info[["func"]]

  if (is.null(func)) {
    param_info <- param_defs[[param]]
    func       <- param_info[["func"]]
  }

  if (is.null(func)) {
    return(NA)
  }

  if (is.function(func)) {
    return(func)
  }

  if (!is.character(func)) {
    stop(
      "`func` must be a function or a string representing a function",
      call. = FALSE
    )
  }

  if (is.null(param_info[["func_pkg"]])) {
    func <- try(get(func), silent = TRUE)
    if (is.function(func)) {
      return(func)
    }
    stop(
      "Could not find function ", param_info[["func"]], "\n",
      "`func` must be a function or a string representing a function",
      call. = FALSE
    )
  }

  func <- try(
    get(func, envir = asNamespace(param_info[["func_pkg"]])),
    silent = TRUE
  )

  if (is.function(func)) {
    return(func)
  }

  stop(
    "Could not find function ", func, " in package ", param_info[["func_pkg"]], "\n",
    "`func` must be a function or a string representing a function",
    call. = FALSE
  )

}
