#' Get ensemble statistics
#'
#' @param .fcst A harp_fcst list or a harp_point_fcst or harp_spatial_fcst data
#'   frame.
#' @param mean Logical. Set to TRUE (the default) to compute the ensemble mean.
#' @param spread Logical. Set to TRUE (the default) to compute the ensemble
#'   spread.
#' @param var Logical. Set to TRUE to compute the ensemble variance.
#' @param min Logical. Set to TRUE to compute the ensemble minimum.
#' @param max Logical. Set to TRUE to compute the ensemble maximum.
#' @param prob_thresh A numeric threshold for which to compute the probability.
#'   If a length 2 vector is passed, the probability for between those two
#'   values (inclusive) will be computed.
#' @param prob_inequality The inequality to use in the calculation for the
#'   probability. Must be in back ticks. Can be any one of `>=` (the default),
#'   `>`, `<=`, `<`, `==`, or `!=`.
#' @param nbh_radius For spatial data the radius of neighbourhoods for which to
#'   compute the probability in grid lengths. The neigbourhood will be a square
#'   with sides equal 2 * nbh_radius.
#' @param keep_members Logical. Whether to keep the ensemble members after
#'   computing the statistics. Default is TRUE.
#' @param na.rm Logical. Whether to include missing values. Default is FALSE.
#'
#' @return An object of the same class as .fcst with new columns in the data
#'   frame(s) for the computed values.
#' @export
#'
#' @examples
ens_stats <- function(
  .fcst,
  mean            = TRUE,
  spread          = TRUE,
  var             = FALSE,
  min             = FALSE,
  max             = FALSE,
  prob_thresh     = NULL,
  prob_inequality = `>=`,
  nbh_radius      = 0,
  keep_members    = TRUE,
  na.rm           = FALSE
) {
  if (!is.null(prob_thresh)) {

    stopifnot(is.numeric(prob_thresh))
    stopifnot(length(prob_thresh) <= 2)

    if (length(prob_thresh) == 1) {

      stopifnot(is.function(prob_inequality))

      ineq <- as.character(enquote(prob_inequality))[[2]]

      err_string <- paste(
        "prob_inequality must be one of",
        "`<`, `<=`, `>`, `>=`, `==`, `!=`"
      )

      if (!grepl(".Primitive", ineq)) {
        stop(err_string)
      }

      ineq <- gsub("\"", "", regmatches(ineq, regexpr("\"[[:graph:]]+\"", ineq)))
      if (!ineq %in% c("<", "<=", ">", ">=", "==", "!=")) {
        stop(err_string)
      }

    }

  }

  UseMethod("ens_stats")

}

#' @export
ens_stats.default <- function(
  .fcst,
  mean            = TRUE,
  spread          = TRUE,
  var             = FALSE,
  min             = FALSE,
  max             = FALSE,
  prob_thresh     = NULL,
  prob_inequality = `>=`,
  keep_members    = TRUE,
  na.rm           = FALSE,
  ...
) {

  col_names <- colnames(.fcst)

  if (length(grep("_mbr[[:digit:]]+", col_names)) < 1) {
    stop("ens_stats only works with ensemble data.", call. = FALSE)
  }

  member_data <- dplyr::select(.fcst, dplyr::matches("_mbr[[:digit:]]+"))

  if (mean) {
    .fcst[["ens_mean"]] <- rowMeans(member_data, na.rm = na.rm)
  }

  if (spread) {
    .fcst[["ens_spread"]] <- matrixStats::rowSds(as.matrix(member_data), na.rm = na.rm)
  }

  if (var) {
    if (is.element("ens_spread", colnames(.fcst))) {
      .fcst[["ens_var"]] <- .fcst[["ens_spread"]] ^ 2
    } else {
      .fcst[["ens_var"]] <- matrixStats::rowVars(as.matrix(member_data), na.rm = na.rm)
    }
  }

  if (min) {
    .fcst[["ens_min"]] <- matrixStats::rowMins(as.matrix(member_data), na.rm = na.rm)
  }

  if (max) {
    .fcst[["ens_max"]] <- matrixStats::rowMaxs(as.matrix(member_data), na.rm = na.rm)
  }

  if (!is.null(prob_thresh)) {

    if (length(prob_thresh) == 1) {

      prob_col <- paste("prob", ineq2char(prob_inequality), prob_thresh, sep = "_")

      .fcst[[prob_col]] <- rowSums(prob_inequality(as.matrix(member_data), prob_thresh), na.rm = na.rm) /
        ncol(member_data)

    } else {

      prob_col <- paste("prob_bt", paste(prob_thresh, collapse = "_"), sep = "_")

      .fcst[[prob_col]] <- rowMeans(
        member_data >= min(prob_thresh) & member_data <= max(prob_thresh),
        na.rm = na.rm
      )

    }

  }

  if (!keep_members) {
    .fcst <- dplyr::select(
      .fcst, dplyr::everything(), -dplyr::matches("_mbr[[:digit:]]+")
    )
  }

  .fcst

}

#' @export
ens_stats.harp_spatial_fcst <- function(
  .fcst,
  mean            = TRUE,
  spread          = TRUE,
  var             = FALSE,
  min             = FALSE,
  max             = FALSE,
  prob_thresh     = NULL,
  prob_inequality = `>=`,
  nbh_radius      = 0,
  keep_members    = TRUE,
  na.rm           = FALSE
) {

  col_names <- colnames(.fcst)

  if (length(grep("_mbr[[:digit:]]+", col_names)) < 1) {
    stop("ens_stats only works with ensemble data.", call. = FALSE)
  }

  member_data <- lapply(
    purrr::transpose(dplyr::select(.fcst, dplyr::matches("_mbr[[:digit:]]+"))),
    as_geolist
  )

  if (mean) {
    .fcst[["ens_mean"]] <- as_geolist(lapply(member_data, mean, na.rm = na.rm))
  }

  if (spread) {
    .fcst[["ens_spread"]] <- as_geolist(lapply(member_data, std_dev, na.rm = na.rm))
  }

  if (var) {
    if (is.element("ens_spread", colnames(.fcst))) {
      .fcst[["ens_var"]] <- .fcst[["ens_spread"]] ^ 2
    } else {
      .fcst[["ens_var"]] <- as_geolist(lapply(member_data, variance, na.rm = na.rm))
    }
  }

  if (min) {
    .fcst[["ens_min"]] <- as_geolist(lapply(member_data, min, na.rm = na.rm))
  }

  if (max) {
    .fcst[["ens_max"]] <- as_geolist(lapply(member_data, max, na.rm = na.rm))
  }

  if (!is.null(prob_thresh)) {

    binary_probs <- function(geo, thresh, ineq) {
      geo_atts <- attributes(geo)
      if (length(thresh) == 1) {
        geo <- ineq(geo, thresh)
      } else {
        geo <- geo >= min(thresh) & geo <= max(thresh)
      }
      geo <- as.integer(geo)
      attributes(geo) <- geo_atts
      geo
    }

    member_data <- lapply(
      member_data,
      function(x) as_geolist(lapply(x, binary_probs, prob_thresh, prob_inequality))
    )

    if (nbh_radius > 0) {

      if (!requireNamespace("harpSpatial")) {
        stop(
          "harpSpatial must be installed to compute neighbourhood probabilites.\n",
          "Install with remotes::install_github(\"adeckmyn/harpSpatial\").",
          call. = FALSE
        )
      }

      prob_func <- function(geo, rad) {
        geo_atts <- attributes(geo)
        geo      <- harpSpatial:::windowMean(geo, rad)
        attributes(geo) <- geo_atts
        geo
      }

      member_data <- lapply(
        member_data,
        function(x) as_geolist(lapply(x, prob_func, nbh_radius))
      )

    }

    if (length(prob_thresh) == 1) {
      prob_col <- paste("prob", ineq2char(prob_inequality), prob_thresh, sep = "_")
    } else {
      prob_col <- paste("prob", "bt", paste(prob_thresh, collapse = "_"), sep = "_")
    }

    .fcst[[prob_col]] <- as_geolist(lapply(member_data, mean, na.rm = na.rm))

  }

  if (!keep_members) {
    .fcst <- dplyr::select(
      .fcst, dplyr::everything(), -dplyr::matches("_mbr[[:digit:]]+")
    )
  }

  .fcst

}

#' @export
ens_stats.harp_fcst <- function(
  .fcst,
  mean            = TRUE,
  spread          = TRUE,
  var             = FALSE,
  min             = FALSE,
  max             = FALSE,
  prob_thresh     = NULL,
  prob_inequality = `>=`,
  nbh_radius      = 0,
  keep_members    = TRUE,
  na.rm           = FALSE
) {

  structure(
    lapply(
      .fcst,
      ens_stats,
      mean            = mean,
      spread          = spread,
      var             = var,
      min             = min,
      max             = max,
      prob_thresh     = prob_thresh,
      prob_inequality = prob_inequality,
      nbh_radius      = nbh_radius,
      keep_members    = keep_members,
      na.rm           = na.rm
    ),
    class = "harp_fcst"
  )

}

ineq2char <- function(ineq) {
  ineq <- as.character(enquote(ineq))[[2]]
  ineq <- gsub("\"", "", regmatches(ineq, regexpr("\"[[:graph:]]+\"", ineq)))
  switch(
    ineq,
    ">"  = "gt",
    ">=" = "ge",
    "<"  = "lt",
    "<=" = "le",
    "==" = "eq",
    "!=" = "ne"
  )
}

