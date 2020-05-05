#' Options for Writing SQLite files
#'
#' @param path If not NULL, sqlite files are generated and written to the
#'   directory specified here.
#' @param template The template for the filenames of the sqlite files. See
#'   \code{\link{show_file_templates}} for available built in templates - for
#'   point forecast sqlite files, these are templates beginning "fctable_". The
#'   default is "fctable_det".
#' @param index_cols The columns to index by
#' @param synchronous The synchronus setting for sqlite files. The defualt is
#'   "off", but could also be "normal", "full", or "extra". See
#'   \url{https://www.sqlite.org/pragma.html#pragma_synchronous} for more
#'   information.
#' @param journal_mode The journal mode for the sqlite files. The default is
#'   "delete", but can also be "truncate", "persist", "memory", "wal", or "off".
#'   See \url{https://www.sqlite.org/pragma.html#pragma_journal_mode} for more
#'   information.
#' @param remove_model_elev Set to TRUE to not include model elevation in the
#'   sqlite output files. For multi model ensembles, members having different
#'   model elevations from each other will make it impossible to include all
#'   members in the same row and thus break unique constraints for the row
#'   indexing.
#'
#' @return A list with options for writing sqlite files
#' @export
#'
#' @examples
#' sqlite_opts()
#' sqlite_opts(path = tempdir())
#' sqlite_opts(path = tempdir(), template = "fctable_eps_all_leads")

sqlite_opts <- function(
  path              = NULL,
  template          = "fctable_det",
  index_cols        = c("fcdate", "leadtime", "SID"),
  synchronous       = c("off", "normal", "full", "extra"),
  journal_mode      = c("delete", "truncate", "persist", "memory", "wal", "off"),
  remove_model_elev = FALSE
) {

  synchronous  <- match.arg(synchronous)
  journal_mode <- match.arg(journal_mode)

  list(
    path              = path,
    template          = template,
    index_cols        = index_cols,
    synchronous       = synchronous,
    journal_mode      = journal_mode,
    remove_model_elev = remove_model_elev
  )

}
