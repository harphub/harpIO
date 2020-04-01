xsection_rows_to_matrix <- function(xs_heights, xs_rows, horizontal_res, num_levels = 50, breaks = NULL) {
  stopifnot(is.list(xs_rows))
  xs_in <- Reduce(rbind, xs_rows)
  xs_cols <- lapply(seq(1:ncol(xs_in)), function(x) xs_in[, x])
  if (is.null(breaks)) {
    interp_cols <- lapply(xs_cols, function(x) approx(x = xs_heights, y = x, n = num_levels))
  } else {
    interp_cols <- lapply(xs_cols, function(x) approx(x = xs_heights, y = x, xout = breaks))
  }
  xs_out <- Reduce(rbind, lapply(interp_cols, function(x) x[["y"]]))
  attr(xs_out, "level") <- interp_cols[[1]][["x"]]
  attr(xs_out, "distance") <- seq(0, (length(xs_rows[[1]]) - 1) * horizontal_res, horizontal_res)
  xs_out
}
