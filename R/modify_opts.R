#' Modify an options list
#'
#' harpIO includes a number of functions for creating lists of options for, for
#' example, file formats and transformations. \code{modify_opts} allows you to
#' change the values for given options in an already generated options list.
#'
#' \code{modify_opts} is designed to be used in a pipeline so that an options
#' list can easily be modified. It is particularly useful for modifying options
#' set via, for example, an options set in \code{\link{netcdf_opts}}.
#'
#' @param opts The options list to be modified.
#' @param ... Expressions that give new values for options in \code{opts}.
#' @param add_options If \code{add_options = TRUE}, options that are not already
#'   found in \code{opts} will be added to \code{opts}. The default is FALSE and
#'   will give a warning for each option not found in \code{opts}
#'
#' @return The input options list with modified options
#' @export
#'
#' @examples
#' # Default value for z_var is "height1" with
#' # options_set = "met_norway_eps"
#' netcdf_opts(options_set = "met_norway_eps")
#'
#' # Change z_var to "height0", keeping all other
#' # options from options_set = "met_norway_eps"
#' netcdf_opts(options_set = "met_norway_eps") %>%
#'   modify_opts(z_var = "height2")
modify_opts <- function(opts, ..., add_options = FALSE) {
  stopifnot(is.list(opts))
  dots <- list(...)
  for (i in seq_along(dots)) {
    opt <- names(dots)[i]
    if (!add_options)
      if (!is.element(opt, names(opts))) {
        warning(opt, " is not part of opts. To add as a new options set add_options = TRUE.")
        next()
      }
    opts[[names(dots)[i]]] <- dots[[i]]
  }
  opts
}

