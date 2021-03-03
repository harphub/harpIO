#' Set options for grib files
#'
#' In general options for grib files do not need to be explicitly set. However,
#' if you wish to be explicit about how to find a field in a grib file you make
#' use of the \code{param_find} argument. Here you pass a named list with the
#' parameters given in e.g. \link{read_forecast} or \link{read_grid} you wish to
#' be explicit about as the names. The helper functions use_grib_* can be used
#' to set key value pairs to use in the search.
#'
#' If harp is not aware of the grib shortName for the field you wish to read, or
#' harp has the wrong shortName you can set \code{param_find} using
#' \code{use_grib_shortName()}. Alternatively if the field you wish to read does
#' not have a shortName, but has a value for indicatorOfParameter you can set
#' \code{param_find} using \code{use_grib_indicatorOfParameter()}, and the same
#' applies for paramId using \code{use_grib_paramId()}.
#'
#' You can use other grib key value pairs to find fields in a grib file by
#' setting \code{param_find} with \code{use_grib_key} giving it the key and
#' value to search for. Finally, each element of \code{param_find} can be simply
#' set using a list with names "key" and "value".
#'
#' Similarly, although harp knows the type of vertical level most parameters are
#' on, you may need to set this explicitly with the \code{level_find} argument
#' which is a list that selects which grib key to use to find the type of level,
#' the value of that grib key that corresponds to the desired type of level and
#' the value of the level itself. As for \code{param_find}, there are helper
#' functions to help you to set the desired values for \code{level_find}.
#'
#' You can use \code{use_grib_key_level} to set the grib key, value and level
#' explicitly; \code{use_grib_typeOfLevel} to use the typeOfLevel grib key and
#' explicitly set the value and level, similarly with
#' \code{use_grib_indicatorOfTypeOfLevel}; \code{use_grib_heightAboveGround},
#' \code{use_grib_pressure}, \code{use_grib_hybrid} to set a height, pressure or
#' hybrid level respectively; and \code{use_grib_surface} and
#' \code{use_grib_meanSea} to select the surface and mean sea levels
#' respectively.
#'
#' @param meta Logical. Whether to read metadata
#' @param multi Logical. Whether there are multi field grib messages.
#' @param param_find A named list with names being the parameters to find in the
#'   grib files. Each element of the named list is a list with names "key" and
#'   "value". The default is NULL, which means to attempt to automatically find
#'   the parameter using shortName from the appropriate grib table.
#' @param level_find A named list with names being the parameters to find in the
#'   grib files. Each element of the named list is a list with names "key",
#'   "value" and "level". Here "key" is the grib key to be used to find the
#'   level, "value" is the value of the grib key and "level" is the value of the
#'   level. Set level = -999 to select all available levels. The default is
#'   NULL, which means to attempt to automatically find the level.
#' @param ...  Any other options that may exist in future Rgrib2 versions.
#'
#' @return A list of options for reading grib files.
#' @export
#'
#' @examples
#' grib_opts()
#' grib_opts(param_find = list(S10m = list(key = "shortName", value = "WS10")))
#' grib_opts(param_find = list(S10m = use_grib_key("shortName", "WS10")))
#' grib_opts(param_find = list(S10m = use_grib_shortName("WS10")))
#' grib_opts(
#'   param_find = list(T2m = use_grib_shortName("t")),
#'   level_find = list(T2m = use_grib_typeOfLevel("heightAboveGround", 2))
#' )
grib_opts <- function(
  meta       = TRUE,
  multi      = FALSE,
  param_find = NULL,
  level_find = NULL,
  ...
) {

  check_inputs <- function(arg, arg_name) {

    if (!is.null(arg)) {

      if (!is.list(arg)) {
        stop("`", arg_name, "` must be a named list", call. = FALSE)
      }

      if (is.null(names(arg)) || any(names(arg) %in% c("key", "value", "level"))) {
        stop(
          "`", arg_name, "` must be a named list.\n",
          "Names cannot include `key`, `value`, or `level`",
          call. = FALSE
        )
      }

      if (arg_name == "param_find") {
        list_names  <- "keyvalue"
        error_names <- "`key` and `value`."
      } else {
        list_names  <- "keylevelvalue"
        error_names <- "`key`, `value` and `level`."
      }

      good_list_names <- sapply(
        lapply(
          arg,
          function(x) paste(sort(names(x)), collapse = "")
        ),
        function(x) x == list_names
      )

      if (!all(good_list_names)) {
        stop(
          "Elements of `",
          arg_name,
          "` must be a list with names ", error_names,
          call. = FALSE
        )
      }

    }

  }

  check_inputs(param_find, "param_find")
  check_inputs(level_find, "level_find")

  list(
    meta = meta, multi = multi,
    param_find = param_find, level_find = level_find,
    ...
  )

}

#' @rdname grib_opts
#' @export
#' @examples
#' use_grib_key("indicatorOfParameter", 65)
#' use_grib_indicatorOfParameter(65)
#' use_grib_key("paramId", 33)
#' use_grib_paramId(33)
#' use_grib_key("shortName", "tp")
#' use_grib_shortName("tp")
#' use_grib_key_level("typeOfLevel", "heightAboveGround", 2)
#' use_grib_typeOfLevel("heightAboveGround", 2)
#' use_grib_heightAboveGround(2)
#' use_grib_indicatorOfTypeOfLevel(105, 2)
#' use_grib_pressure(500)
#' use_grib_surface()
use_grib_key = function(key, value) {
  list(key = key, value = value)
}

#' @rdname grib_opts
#' @export
use_grib_indicatorOfParameter = function(value) {
  stopifnot(is.numeric(value))
  use_grib_key("indicatorOfParameter", value)
}

#' @rdname grib_opts
#' @export
use_grib_paramId = function(value) {
  stopifnot(is.numeric(value))
  use_grib_key("paramId", value)
}

#' @rdname grib_opts
#' @export
use_grib_shortName = function(value) {
  stopifnot(is.character(value))
  use_grib_key("shortName", value)
}

#' @rdname grib_opts
#' @export
use_grib_key_level <- function(key, value, level = -999) {
  stopifnot(is.numeric(level))
  list(key = key, value = value, level = level)
}

#' @rdname grib_opts
#' @export
use_grib_indicatorOfTypeOfLevel <- function(value, level = -999) {
  stopifnot(is.numeric(value))
  stopifnot(is.numeric(level))
  use_grib_key_level("indicatorOfTypeOfLevel", value, level)
}

#' @rdname grib_opts
#' @export
use_grib_typeOfLevel <- function(value, level = -999) {
  stopifnot(is.character(value))
  stopifnot(is.numeric(level))
  use_grib_key_level("typeOfLevel", value, level)
}

#' @rdname grib_opts
#' @export
use_grib_heightAboveGround <- function(level = -999) {
  stopifnot(is.numeric(level))
  use_grib_typeOfLevel("heightAboveGround", level)
}

#' @rdname grib_opts
#' @export
use_grib_pressure <- function(level = -999) {
  stopifnot(is.numeric(level))
  use_grib_typeOfLevel("isobaricInhPa", level)
}

#' @rdname grib_opts
#' @export
use_grib_hybrid <- function(level = -999) {
  stopifnot(is.numeric(level))
  use_grib_typeOfLevel("hybrid", level)
}

#' @rdname grib_opts
#' @export
use_grib_model <- function(level = -999) {
  stopifnot(is.numeric(level))
  use_grib_hybrid(level)
}

#' @rdname grib_opts
#' @export
use_grib_surface <- function() {
  use_grib_typeOfLevel("surface", -999)
}

#' @rdname grib_opts
#' @export
use_grib_meanSea <- function() {
  use_grib_key_level("typeOfLevel", "meanSea", -999)
}


