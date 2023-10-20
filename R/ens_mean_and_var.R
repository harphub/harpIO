#' Compute the ensemble mean and variance
#'
#' `r lifecycle::badge("deprecated")`
#' This function is replaced by \code{\link[harpCore]{ens_stats}}
#'
#' The ensemble mean and variance are computed and added as columns to tables in
#' a \code{harp_fcst} object.
#'
#' @param .fcst A \code{harp_fcst} object, or a data frame containing columns
#'   with "_mbr" as part of the column names.
#' @param mean_name The output column name for the ensemble mean
#' @param var_name The output column name for the ensemble variance
#' @param sd_name The output column name for the ensemble spread (standard
#'   deviation)
#' @param var_drop_member Which members to drop for the calculation of the
#'   ensemble variance and standard deviation. For harp_fcst objects, this can
#'   be a numeric scalar - in which case it is recycled for all forecast models;
#'   a list or numeric vector of the same length as the harp_fcst object, or a
#'   named list with the names corresponding to names in the harp_fcst object.
#' @param ...
#'
#' @return A \code{harp_fcst} object with columns ens_mean and ens_var added to
#'   the forecast tables.
#' @export
#'
ens_mean_and_var <- function(
  .fcst, mean_name = "ens_mean", var_name = "ens_var", sd_name = "ens_spread",
  var_drop_member = NULL
) {
  lifecycle::deprecate_stop(
    "0.1.0",
    "ens_mean_and_var()",
    "ens_stats()"
  )
  UseMethod("ens_mean_and_var")
}

#' @export
ens_mean_and_var.default <- function(
  .fcst, mean_name = "ens_mean", var_name = "ens_var",
  sd_name = "ens_spread", var_drop_member = NULL
) {
  col_names <- colnames(.fcst)
  mean_name <- rlang::sym(mean_name)
  var_name  <- rlang::sym(var_name)
  sd_name   <- rlang::sym(sd_name)

  if (length(grep("_mbr", col_names)) < 1) {
    stop(".fcst column names must contain '_mbr' to indicate an ensemble", call. = FALSE)
  }

  drop_members <- "TheRegexShouldNeverExistInnit"

  if (!is.null(var_drop_member)) {
    if (!is.numeric(var_drop_member)) {
      stop("`var_drop_member` must be numeric.", call. = FALSE)
    }
    drop_members <- paste(
      paste0("_mbr", formatC(var_drop_member, width = 3, flag = "0"), "$"),
      sep = "|"
    )

  }

  member_data <- dplyr::select(.fcst, dplyr::contains("_mbr"))

  .fcst <- dplyr::mutate(
    .fcst,
    !!mean_name := rowMeans(member_data)#,
    #!!var_name  := matrixStats::rowVars(as.matrix(member_data)),
    #!!sd_name   := sqrt(!!var_name)
  )

  if (!is.null(var_drop_member)) {
    dm_var_name <- rlang::sym(paste0("dropped_members_", var_name))
    dm_sd_name <- rlang::sym(paste0("dropped_members_", sd_name))
    .fcst <- dplyr::mutate(
      .fcst#,
      #!!dm_var_name := matrixStats::rowVars(
      #  as.matrix(dplyr::select(member_data, -dplyr::matches(drop_members)))
      #),
      #!!dm_sd_name  := sqrt(!!dm_var_name)
    )
  }

  .fcst

}

#' @export
ens_mean_and_var.harp_spatial_fcst <- function(
  .fcst, mean_name = "ens_mean", var_name = "ens_var",
  sd_name = "ens_spread", var_drop_member = NULL
) {
  col_names <- colnames(.fcst)
  mean_name <- rlang::sym(mean_name)
  var_name  <- rlang::sym(var_name)
  sd_name   <- rlang::sym(sd_name)

  if (length(grep("_mbr", col_names)) < 1) {
    stop(".fcst column names must contain '_mbr' to indicate an ensemble", call. = FALSE)
  }

  member_data <- lapply(
    purrr::transpose(dplyr::select(.fcst, dplyr::contains("_mbr"))),
    harpCore::geolist
  )

  dplyr::mutate(
    .fcst,
    !!mean_name := harpCore::geolist(lapply(member_data, mean)),
    !!var_name  := harpCore::geolist(lapply(member_data, harpCore::variance)),
    !!sd_name   := harpCore::geolist(sqrt(!!var_name))
  )

}

#' @export
ens_mean_and_var.harp_fcst <- function(
  .fcst, mean_name = "ens_mean", var_name = "ens_var",
  sd_name = "ens_spread", var_drop_member = NULL
) {

  var_drop_member <- parse_member_drop(var_drop_member, names(.fcst))

  structure(
    purrr::map2(
      .fcst, var_drop_member,
      ~ens_mean_and_var(.x, mean_name, var_name, sd_name, var_drop_member = .y)
    ),
    class = "harp_fcst"
  )
}

parse_member_drop <- function(x, nm) {

  if (!is.null(names(x))) {
    x <- as.list(x)
  }

  if (!is.list(x)) {
    if (is.null(x)) {
      return(sapply(nm, function(x) NULL, simplify = FALSE))
    }
    if (length(x) == 1) {
      return(sapply(nm, function(.x) x, simplify = FALSE))
    }
    if (length(x) == length(nm)) {
      x <- as.list(x)
      names(x) <- nm
      return(x)
    }
    stop("Bad input for `spread_exclude_member`", call. = FALSE)
  }

  if (is.null(names(x))) {

    if (length(x) == length(nm)) {
      names(x) <- nm
      return(x)
    }

    stop(
      "If `spread_exclude_member` is a list ",
      "it must be the same length as `.fcst` or have names",
      call. = FALSE
    )

  }

  if (identical(sort(names(x)), sort(nm))) {
    return(x[nm])
  }

  if (length(intersect(names(x), nm)) < 1) {
    stop(
      "spread_exclude_member: ",
      paste(names(x), collapse = ", "),
      " not found in `.fcst`.",
      call. = FALSE
    )
  }

  if (length(setdiff(names(x), nm)) > 0) {
    stop(
      "spread_exclude_member: ",
      paste(setdiff(names(x), nm), collapse = ", "),
      " not found in `.fcst`.",
      call. = FALSE
    )
  }

  x <- c(x, sapply(setdiff(nm, names(x)), function(x) NULL, simplify = FALSE))

  x[nm]

}
