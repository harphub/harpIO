#' Options for writing OBSTABLE files
#'
#' OBSTABLE files are sqlite files containing four tables - SYNOP for "synop"
#' observations, TEMP for "temp" observations, synop_params for metadata about
#' the contents of SYNOP and temp_params for metadata about the contents of
#' TEMP. Note that data do not have to strictly be "synop" or "temp"
#' observations, but rather SYNOP contains near surface observations and TEMP
#' contains vertical profiles.
#'
#' @param path The path to which to write the files.
#' @param template The template from which to generate file names.
#' @param index_cols The columns to use for indexing in the output sqlite files.
#'   Set to "auto" (the default) to use automatically prescribed index columns.
#' @param synchronous The synchronus setting for sqlite files. The defualt is
#'   "off", but could also be "normal", "full", or "extra". See
#'   \url{https://www.sqlite.org/pragma.html#pragma_synchronous} for more
#'   information.
#' @param journal_mode The journal mode for the sqlite files. The default is
#'   "delete", but can also be "truncate", "persist", "memory", "wal", or "off".
#'   See \url{https://www.sqlite.org/pragma.html#pragma_journal_mode} for more
#'   information.
#'
#' @return
#' @export
#'
#' @examples
obstable_opts <- function(
  path         = NULL,
  template     = "obstable",
  index_cols   = "auto",
  synchronous  = c("off", "normal", "full", "extra"),
  journal_mode = c("delete", "truncate", "persist", "memory", "wal", "off")
) {

  synchronous  <- match.arg(synchronous)
  journal_mode <- match.arg(journal_mode)

  list(
    path         = path,
    template     = template,
    index_cols   = index_cols,
    synchronous  = synchronous,
    journal_mode = journal_mode
  )

}
