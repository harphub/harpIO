# Internal function to check and process arguments to read_forecast.
# Returns a data frame with all arguments in a data
# frame ready to generate file names.

process_read_forecast_args <- function(
  fcst_model,
  file_path     = getwd(),
  file_format   = NULL,
  file_template = "vfld_det",
  members_in    = NULL,
  members_out   = NULL,
  lags          = NULL
) {

  if (is.null(file_format)) file_format <- NA_character_

  # If no members are passed, assume all models are deterministic

  if (is.null(members_in)) {

    # Make sure multimodel type input hasn't been used
    if (any(sapply(fcst_model, length)) > 1) {
      stop("Each element of 'fcst_model' must contain only one entry.", call. = FALSE)
    }

    fcst_model_df <- data.frame(fcst_model = fcst_model, stringsAsFactors = FALSE) %>%
      dplyr::inner_join(
        arg_to_dataframe("file_template", "fcst_model", "vfld_det"),
        by = "fcst_model"
      ) %>%
      dplyr::inner_join(
        arg_to_dataframe("file_format", "fcst_model", "vfld"),
        by = "fcst_model"
      ) %>%
      dplyr::inner_join(
        arg_to_dataframe("file_path", "fcst_model", "vfld_det"),
        by = "fcst_model"
      ) %>%
      cbind(members = NA_integer_) %>%
      cbind(members_out = NA_integer_) %>%
      cbind(lags = "0h", stringsAsFactors = FALSE) %>%
      cbind(sub_model = NA_character_, stringsAsFactors = FALSE)

  } else {

    # Get fcst_model in a list format
    if (!is.list(fcst_model)) fcst_model <- as.list(fcst_model)
    if (is.null(names(fcst_model))) names(fcst_model) <- fcst_model
    names(fcst_model)[names(fcst_model) == ""] <- fcst_model[names(fcst_model) == ""]

    # Get the other arguments as lists
    members_in_list <- arg_to_list("members_in", names(fcst_model))

    if (missing(members_out) || is.null(members_out)) members_out <- members_in_list
    members_out_list <- arg_to_list("members_out", names(members_in_list))

    if (is.null(lags)) {
      lags <- as.list(rep("0h", length(fcst_model)))
      names(lags) <- names(fcst_model)
    }
    lags_list <- arg_to_list("lags", names(fcst_model), default_val = "0h")

    if (length(file_template) == 1) file_template <- rep_args(file_template, fcst_model)
    file_template_list <- arg_to_list("file_template", names(fcst_model))

    if (length(file_path) == 1) file_path <- rep_args(file_path, fcst_model)
    file_path_list <- arg_to_list("file_path", names(fcst_model))

    if (length(file_format) == 1) file_format <- rep_args(file_format, fcst_model)
    file_format_list <- arg_to_list("file_format", names(fcst_model))

    # Loop over the lists and convert to data frames
    fcst_model_list <- fcst_model
    fcst_model_df   <- list()

    for (i in seq_along(fcst_model)) {

      sub_model     <- fcst_model_list[[i]]
      fcst_model    <- names(fcst_model_list)[i]
      members_in    <- members_in_list[[i]]
      members_out   <- members_out_list[[i]]
      lags          <- lags_list[[i]]
      file_template <- file_template_list[[i]]
      file_path     <- file_path_list[[i]]
      file_format   <- file_format_list[[i]]

      if (!is.list(sub_model)) sub_model <- as.list(sub_model)
      if (is.null(names(sub_model))) names(sub_model) <- sub_model
      names(sub_model)[names(sub_model) == ""] <- sub_model[names(sub_model) == ""]

      members_in_sublist <- arg_to_list("members_in", names(sub_model))

      if (missing(members_out) || is.null(members_out)) members_out <- members_in_sublist
      members_out_sublist <- arg_to_list("members_out", names(members_in_sublist))

      if (is.null(lags)) {
        lags <- as.list(rep("0h", length(sub_model)))
        names(lags) <- names(sub_model)
      }
      if (is.null(names(lags)) && length(lags) == 1) {
        lags <- as.list(rep(lags, length(sub_model)))
        names(lags) <- names(sub_model)
      }
      lags_sublist <- arg_to_list("lags", names(sub_model), default_val = "0h")

      if (length(file_template) == 1) file_template <- rep_args(file_template, sub_model)
      file_template_sublist <- arg_to_list("file_template", names(sub_model), default_val = "vfld_eps")

      if (length(file_path) == 1) file_path <- rep_args(file_path, sub_model)
      file_path_sublist <- arg_to_list("file_path", names(sub_model), default_val = ".")

      if (length(file_format) == 1) file_format <- rep_args(file_format, sub_model)
      file_format_sublist <- arg_to_list("file_format", names(sub_model), default_val = "vfld")

      # Do for sub_model as for fcst_model - this covers the multimodel possibility
      sub_model_list <- sub_model
      for (j in seq_along(sub_model)) {

        sub_model     <- sub_model_list[j]
        fcst_model    <- names(fcst_model_list)[i]
        members       <- members_in_sublist[[j]]
        members_out   <- members_out_sublist[[j]]
        lags          <- lags_sublist[[j]]
        file_template <- file_template_sublist[[j]]
        file_path     <- file_path_sublist[[j]]
        file_format   <- file_format_sublist[[j]]

        if (length(lags) > 1 && length(lags) != length(members)) {
          stop(
            length(lags), " lags specified for '",
            paste(union(fcst_model, sub_model), collapse = ":"), "'. ",
            length(members), ", 1, or NULL expected.",
            call. = FALSE
          )
        }
        if (is.numeric(lags)) lags <- paste0(lags, "h")

        if (length(members_out) != length(members)) {
          stop(length(members_out), " members_out specified for '",
            paste(union(fcst_model, sub_model), collapse = ":"), "'. ",
            length(members), " expected.",
            call. = FALSE
          )
        }

        fcst_model_df[[i + j - 1]] <- arg_to_dataframe("fcst_model", "sub_model") %>%
          dplyr::inner_join(
            arg_to_dataframe("sub_model", "members"),
            by = "sub_model"
          ) %>%
          cbind(
            members_out = arg_to_dataframe("sub_model", "members_out")[["members_out"]]
          ) %>%
          cbind(
            lags = arg_to_dataframe("sub_model", "lags")[["lags"]],
            stringsAsFactors = FALSE
          ) %>%
          dplyr::inner_join(
            arg_to_dataframe("sub_model", "file_template"),
            by = "sub_model"
          ) %>%
          dplyr::inner_join(
            arg_to_dataframe("sub_model", "file_path"),
            by = "sub_model"
          ) %>%
          dplyr::inner_join(
            arg_to_dataframe("sub_model", "file_format"),
            by = "sub_model"
          )

      }
    }

    fcst_model_df <- dplyr::bind_rows(fcst_model_df)

  }

  fcst_model_df

}

arg_to_dataframe <- function(arg, comparison, assumed_value) {

  arg_value        <- get(arg, envir = parent.frame())
  comparison_value <- get(comparison, envir = parent.frame())

  # Assign default value to arg_value if one is given
  if (length(arg_value) < 1) {

    if (is.null(assumed_value)) {
      stop("'", arg, "' not passed and no default value.", call. = FALSE)
    } else {
      warning("'", arg, "' not passed. Assuming '", assumed_value, "'.", call. = FALSE, immediate. = TRUE)
      assign(arg, assumed_value)
      arg_value <- assumed_value
    }

  }

  # If there is only one value for arg, recycle for all values of comparison
  if (length(arg_value) == 1) {

    arg_df <- data.frame(
      comparison       = unlist(comparison_value, use.names = FALSE),
      arg              = unlist(arg_value, use.names = FALSE),
      stringsAsFactors = FALSE
    )

  } else {

    # If more than one value for arg, it must be same length as comparison
    if (length(arg_value) != length(comparison_value)) {
      stop("'", arg, "' must be a character vector of the same length as '", comparison, "'.", call. = FALSE)
    }

    # If no names are supplied for arg, assume order is the same as comparison
    if (is.null(names(arg_value))) {
      warning("Assuming '", arg, "' is in the same order as '", comparison, "'.", call. = FALSE, immediate. = TRUE)
      arg_df <- data.frame(
        comparison       = unlist(comparison_value, use.names = FALSE),
        arg              = unlist(arg_value, use.names = FALSE),
        stringsAsFactors = FALSE
      )

    } else {

      # If names are given for arg, they must be the same as those for comparison
      if (!identical(sort(names(arg_value)), sort(comparison_value))) {
        stop("The names of '", arg, "' must be the same as those for '", comparison, "'.", call. = FALSE)
      }

      arg_df <- data.frame(
        comparison       = names(arg_value),
        arg              = unlist(arg_value, use.names = FALSE),
        stringsAsFactors = FALSE
      )

    }

  }

  # Set the column names for arg and comparison
  colnames(arg_df)[colnames(arg_df) == "comparison"] <- comparison
  colnames(arg_df)[colnames(arg_df) == "arg"]        <- arg

  arg_df

}

arg_to_list <- function(arg, list_names, default_val = NULL) {
  arg_name <- arg
  arg <- get(arg, envir = parent.frame())
  if (!is.list(arg)) arg <- list(arg)

  if (is.null(names(arg))) {
    if (length(arg) == length(list_names)) {
      if (length(arg) != 1) {
        warning(
          "Assuming elements in '", arg_name, "' are in the order:\n",
          "(", paste(list_names, collapse = ", "), ").",
          call. = FALSE, immediate. = TRUE
        )
      }
      names(arg) <- list_names
    } else {
      stop(
        "If '", arg_name, "' is not a named list it must be a list of length ", length(list_names),
        call. = FALSE
      )
    }

  } else {

    if (length(arg) == length(list_names)) {
      if (identical(sort(names(arg)), sort(list_names))) {
        arg <- arg[list_names]
      } else {
        stop(
          "Names of '", arg_name, "' are: ", paste(names(arg), collapse = ", "), ".\n",
          "Expected names: ", paste(list_names, collapse = ", "), ".",
          call. = FALSE
        )
      }
    } else {
      if (length(setdiff(names(arg), list_names)) == 0) {
        arg <- lapply(list_names, check_arg, arg, arg_name, default_val)
        names(arg) <- list_names
      } else {
        bad_names <- setdiff(names(arg), list_names)
        stop(
          "'", arg_name, "' includes unexpected named elements: ", paste(names(arg), collapse = ", "), ".\n",
          "Expected names: ", paste(list_names, collapse = ", "), ".",
          call. = FALSE
        )
      }
    }

  }
  arg
}

check_arg <- function(x, arg, arg_name, default_val) {
  if (is.null(arg[[x]])) {
    if (is.null(default_val)) {
      stop("'", arg_name, "' must be a named list that includes '", x, "'.", call. = FALSE)
    } else {
      return(default_val)
    }
  }
  arg[[x]]
}

rep_args <- function(arg, arg_names) {
  arg <- as.list(rep(arg, length(arg_names)))
  names(arg) <- names(arg_names)
  arg
}
