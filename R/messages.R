# a simple way to supress /all/ messages in harpIO
#' @Turn off harp diagnostic messaging
#' @param x FALSE will turn off all messages.
#'    If missing, the current state is returned.
#' @export
harp_messages <- local({
  .harp_messages <- TRUE
  function(x) if (!missing(x)) .harp_messages <<- x else .harp_messages
})

message <- function(...) {
  if (harp_messages()) base::message(...)
}


