#' Title
#'
#' @param start_date
#' @param end_date
#' @param eps_model
#' @param parameter
#' @param file_path
#' @param lead_time
#' @param members_in
#' @param by
#' @param file_format
#' @param file_template
#' @param sqlite_path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read_eps_interpolate <- function(
  start_date,
  end_date,
  eps_model,
  parameter,
  file_path,
  lead_time     = seq(0, 48, 3),
  members_in    = seq(0,9),
  by            = "6h",
  file_format   = "vfld",
  file_template = "vfld",
  sqlite_path   = NULL,
  ...
) {

# Sanity checks and organisation of members_in as a list

  if (is.list(eps_model)) {

    multimodel <- TRUE
    eps_models <- names(eps_model)

    if (!is.list(members_in) | !identical(eps_models, names(members_in))) {
      stop(
        paste(
          "For multimodel, members_in must be a list with the",
          "same names as in the eps argument.", sep = "\n  "
        )
      )
    }

    for (eps in eps_models) {
      if (!identical(eps_model[[eps]], names(members_in[[eps]]))) {
        stop(
          paste(
            "Model names specified in members_in do not match those in eps.",
            paste0("model = ", eps, ": ", paste0(eps_models[[eps]], collapse = ",")),
            paste0("members_in = ", eps, ": ", paste0(names(members_in[[eps]]), collapse = ",")),
            sep = "\n  "
          )
        )
      }
    }

  } else {

    multimodel <- FALSE
    eps_models <- eps_model

    if (length(eps_models) > 1) {

      if (!is.list(members_in) | !identical(eps_models, names(members_in))) {
        stop(
          paste(
            "If more than one eps_model is specified, the members must",
            "be passed as a named list with the names as those specified",
            "in eps",
            sep = "\n  "
          )
        )
      }
    } else {
      if (!is.list(members_in)) {
        members_temp <- list()
        members_temp[[eps_models]] <- members_in
        members_in <- members_temp
      }
      if (!identical(eps_models, names(members_in))) {
        stop(
          paste(
            "If specifying members as a named list for a single eps, the",
            "name in the list for members_in must match that specified",
            "in eps.",
            sep = "\n  "
          )
        )
      }
    }

    members_temp <- list()
    for (eps in eps_models) {
      members_temp[[eps]] <- list()
      members_temp[[eps]][[eps]] <- members_in[[eps]]
    }
    members_in <- members_temp

  }

# Convert members_in to a tibble for easier manipulation

  members_in <- tibble::tibble(
    eps_model = names(members_in)
  ) %>%
    dplyr::mutate(sub_model = purrr::map(members_in, names)) %>%
    dplyr::mutate(members   = purrr::modify_depth(members_in, 2, `[`)) %>%
    tidyr::unnest()

# Get the file names

  data_files <- members_in %>%
    dplyr::transmute(
      file_names = purrr::pmap(
        list(eps_model = eps_model, sub_model = sub_model, members = members),
        function(eps_model, sub_model, members) harp_get_filenames(
          file_path     = file_path,
          start_date    = start_date,
          end_date      = end_date,
          by            = by,
          parameter     = parameter,
          eps_model     = eps_model,
          sub_model     = sub_model,
          lead_time     = lead_time,
          members       = members,
          file_template = file_template
        )
      )
    ) %>% unnest()

  data_files
}
