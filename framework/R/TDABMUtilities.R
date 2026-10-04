# ==============================================================================
# TDABMUtilities.R
# ==============================================================================
# Utility functions used by the TDABM project object and later execution modules.
#
# Stage 1 purpose:
#   - safe defaults and small assertion helpers;
#   - radius-grid construction;
#   - output-path construction;
#   - analysis and robustness specification normalisation;
#   - workflow-history helpers.
# ==============================================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

tdabm_is_scalar_character <- function(x, allow_na = FALSE) {
  length(x) == 1L &&
    is.character(x) &&
    (allow_na || !is.na(x)) &&
    (allow_na || nzchar(x))
}

tdabm_is_scalar_numeric <- function(x, allow_na = FALSE) {
  length(x) == 1L &&
    is.numeric(x) &&
    (allow_na || is.finite(x))
}

tdabm_is_scalar_logical <- function(x, allow_na = FALSE) {
  length(x) == 1L &&
    is.logical(x) &&
    (allow_na || !is.na(x))
}

tdabm_as_scalar_character <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0L) return(default)
  as.character(x[[1L]])
}

tdabm_as_scalar_integer <- function(x, default = NA_integer_) {
  if (is.null(x) || length(x) == 0L) return(default)
  as.integer(x[[1L]])
}

tdabm_as_scalar_numeric <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L) return(default)
  as.numeric(x[[1L]])
}

tdabm_as_scalar_logical <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0L) return(default)
  as.logical(x[[1L]])
}

tdabm_timestamp <- function(format = "%Y%m%d_%H%M%S") {
  format(Sys.time(), format = format)
}

tdabm_slug <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

tdabm_path <- function(..., must_work = FALSE) {
  normalizePath(file.path(...), winslash = "/", mustWork = must_work)
}

tdabm_check_file <- function(path, label = "file", required = TRUE) {
  ok <- !is.null(path) && length(path) == 1L && !is.na(path) && nzchar(path) && file.exists(path)
  if (!ok && isTRUE(required)) {
    return(list(ok = FALSE, message = paste0(label, " not found: ", path)))
  }
  list(ok = TRUE, message = paste0(label, " available"))
}

tdabm_check_directory <- function(path, label = "directory", required = TRUE) {
  ok <- !is.null(path) && length(path) == 1L && !is.na(path) && nzchar(path) && dir.exists(path)
  if (!ok && isTRUE(required)) {
    return(list(ok = FALSE, message = paste0(label, " not found: ", path)))
  }
  list(ok = TRUE, message = paste0(label, " available"))
}

tdabm_create_dir <- function(path, verbose = FALSE) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Invalid directory path.", call. = FALSE)
  }

  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(path)) {
      stop("Could not create directory: ", path, call. = FALSE)
    }
    if (isTRUE(verbose)) {
      message("Created directory: ", path)
    }
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}

tdabm_modify_list <- function(defaults, values) {
  if (is.null(values)) return(defaults)
  if (!is.list(values)) {
    stop("Expected a list of override values.", call. = FALSE)
  }
  utils::modifyList(defaults, values, keep.null = TRUE)
}

tdabm_create_radius_grid <- function(
  min,
  max,
  step,
  tolerance = 1e-8,
  digits = 10L
) {
  if (!is.numeric(min) || length(min) != 1L || !is.finite(min)) {
    stop("Radius minimum must be a finite numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(max) || length(max) != 1L || !is.finite(max)) {
    stop("Radius maximum must be a finite numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(step) || length(step) != 1L || !is.finite(step) || step <= 0) {
    stop("Radius step must be a positive finite numeric scalar.", call. = FALSE)
  }
  if (max < min) {
    stop("Radius maximum must be greater than or equal to the minimum.", call. = FALSE)
  }

  raw_intervals <- (max - min) / step
  n_intervals <- round(raw_intervals)

  if (abs(raw_intervals - n_intervals) > tolerance) {
    stop(
      "Radius grid is not an exact integer sequence: (max - min) / step = ",
      raw_intervals,
      call. = FALSE
    )
  }

  grid <- min + seq.int(0L, n_intervals) * step
  grid <- round(grid, digits = digits)
  x001 <- length(grid)

  if (abs(grid[1L] - min) > tolerance) {
    stop("Constructed radius grid does not start at the requested minimum.", call. = FALSE)
  }
  if (abs(grid[x001] - max) > tolerance) {
    stop("Constructed radius grid does not end at the requested maximum.", call. = FALSE)
  }

  structure(
    list(
      min = min,
      max = max,
      step = step,
      grid = grid,
      x001 = x001,
      intervals = n_intervals
    ),
    class = "TDABMRadiusGrid"
  )
}

print.TDABMRadiusGrid <- function(x, ...) {
  cat("TDABM radius grid\n")
  cat("  min:  ", x$min, "\n", sep = "")
  cat("  max:  ", x$max, "\n", sep = "")
  cat("  step: ", x$step, "\n", sep = "")
  cat("  x001: ", x$x001, "\n", sep = "")
  invisible(x)
}

tdabm_make_output_paths <- function(output_root, subdirs = NULL) {
  if (is.null(output_root) || length(output_root) != 1L || is.na(output_root) || !nzchar(output_root)) {
    stop("Provide a valid output root.", call. = FALSE)
  }

  default_subdirs <- if (exists("TDABMDefaultSettings", mode = "function")) {
    TDABMDefaultSettings()$outputs$subdirs
  } else {
    c(
      checkpoints = "checkpoints",
      verification = "verification",
      memberships = "memberships",
      summary_plots = "summary_plots",
      logs = "logs",
      tables = "tables",
      figures = "figures",
      cache = "cache"
    )
  }

  if (is.null(subdirs)) {
    subdirs <- default_subdirs
  } else {
    subdirs <- tdabm_modify_list(as.list(default_subdirs), as.list(subdirs))
    subdirs <- unlist(subdirs, use.names = TRUE)
  }

  output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)

  paths <- list(root = output_root)
  for (nm in names(subdirs)) {
    paths[[nm]] <- file.path(output_root, subdirs[[nm]])
  }

  paths
}

tdabm_create_output_directories <- function(output_paths, verbose = TRUE) {
  if (is.null(output_paths$root)) {
    stop("Output paths must include a root directory.", call. = FALSE)
  }

  created <- character(0)

  for (nm in names(output_paths)) {
    path <- output_paths[[nm]]
    if (is.character(path) && length(path) == 1L && nzchar(path)) {
      existed <- dir.exists(path)
      tdabm_create_dir(path, verbose = FALSE)
      if (!existed) created <- c(created, path)
    }
  }

  if (isTRUE(verbose)) {
    if (length(created) == 0L) {
      message("All TDABM output directories already exist.")
    } else {
      message("Created TDABM output directories:")
      message(paste0("  - ", created, collapse = "\n"))
    }
  }

  invisible(output_paths)
}

tdabm_history_event <- function(action, detail = "", time = Sys.time()) {
  data.frame(
    time = as.character(time),
    action = as.character(action),
    detail = as.character(detail %||% ""),
    stringsAsFactors = FALSE
  )
}

tdabm_append_history <- function(project, action, detail = "") {
  event <- tdabm_history_event(action = action, detail = detail)

  if (is.null(project$history)) {
    project$history <- event
  } else {
    project$history <- rbind(project$history, event)
  }

  project
}

tdabm_normalise_colourings <- function(colourings) {
  required <- c(
    "run_id",
    "variable",
    "label",
    "short_label",
    "enabled",
    "colour_type",
    "family",
    "priority",
    "run_primary",
    "run_robustness"
  )

  if (is.null(colourings) || length(colourings) == 0L) {
    out <- as.data.frame(setNames(replicate(length(required), character(0), simplify = FALSE), required))
    out$enabled <- logical(0)
    out$priority <- integer(0)
    out$run_primary <- logical(0)
    out$run_robustness <- logical(0)
    return(out)
  }

  if (is.data.frame(colourings)) {
    out <- colourings
  } else if (is.list(colourings)) {
    rows <- lapply(seq_along(colourings), function(i) {
      item <- colourings[[i]]
      nm <- names(colourings)[i]
      if (is.null(item) || !is.list(item)) {
        stop("Each colouring specification must be a list.", call. = FALSE)
      }

      data.frame(
        run_id = tdabm_as_scalar_character(item$run_id %||% nm),
        variable = tdabm_as_scalar_character(item$variable %||% NA_character_, default = NA_character_),
        label = tdabm_as_scalar_character(item$label %||% item$short_label %||% item$run_id %||% nm),
        short_label = tdabm_as_scalar_character(item$short_label %||% item$label %||% item$run_id %||% nm),
        enabled = tdabm_as_scalar_logical(item$enabled, default = TRUE),
        colour_type = tdabm_as_scalar_character(item$colour_type %||% "variable"),
        family = tdabm_as_scalar_character(item$family %||% "core"),
        priority = tdabm_as_scalar_integer(item$priority, default = i),
        run_primary = tdabm_as_scalar_logical(item$run_primary, default = TRUE),
        run_robustness = tdabm_as_scalar_logical(item$run_robustness, default = FALSE),
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, rows)
  } else {
    stop("Colourings must be supplied as a data frame or list of lists.", call. = FALSE)
  }

  for (nm in required) {
    if (!nm %in% names(out)) {
      if (nm %in% c("enabled", "run_primary")) {
        out[[nm]] <- TRUE
      } else if (nm == "run_robustness") {
        out[[nm]] <- FALSE
      } else if (nm == "priority") {
        out[[nm]] <- seq_len(nrow(out))
      } else if (nm == "colour_type") {
        out[[nm]] <- "variable"
      } else if (nm == "family") {
        out[[nm]] <- "core"
      } else {
        out[[nm]] <- NA_character_
      }
    }
  }

  out <- out[, required, drop = FALSE]
  out$run_id <- as.character(out$run_id)
  out$variable <- as.character(out$variable)
  out$label <- as.character(out$label)
  out$short_label <- as.character(out$short_label)
  out$enabled <- as.logical(out$enabled)
  out$colour_type <- as.character(out$colour_type)
  out$family <- as.character(out$family)
  out$priority <- as.integer(out$priority)
  out$run_primary <- as.logical(out$run_primary)
  out$run_robustness <- as.logical(out$run_robustness)

  out
}

tdabm_normalise_robustness_specs <- function(robustness) {
  empty_table <- function() {
    out <- data.frame(
      robustness_id = character(0),
      type = character(0),
      variable = character(0),
      exclude = character(0),
      method = character(0),
      enabled = logical(0),
      selection_mode = character(0),
      require_all_matches = logical(0),
      minimum_removed = integer(0),
      rationale = character(0),
      stringsAsFactors = FALSE
    )
    out$exclude_values <- I(list())
    out$analysis_run_ids <- I(list())
    out
  }

  normalise_values <- function(x) {
    if (is.null(x) || length(x) == 0L || all(is.na(x))) {
      return(character(0))
    }
    unique(as.character(x[!is.na(x)]))
  }

  if (is.null(robustness) || length(robustness) == 0L) {
    return(empty_table())
  }

  if (is.data.frame(robustness)) {
    out <- robustness

    if (!"robustness_id" %in% names(out)) out$robustness_id <- paste0("robustness_", seq_len(nrow(out)))
    if (!"type" %in% names(out)) out$type <- "original"
    if (!"variable" %in% names(out)) out$variable <- NA_character_
    if (!"exclude" %in% names(out)) out$exclude <- NA_character_
    if (!"method" %in% names(out)) out$method <- NA_character_
    if (!"enabled" %in% names(out)) out$enabled <- TRUE
    if (!"selection_mode" %in% names(out)) {
      out$selection_mode <- ifelse(
        out$type %in% c("filter", "exclude", "manual_exclusion"),
        "user_specified",
        "not_applicable"
      )
    }
    if (!"require_all_matches" %in% names(out)) out$require_all_matches <- TRUE
    if (!"minimum_removed" %in% names(out)) out$minimum_removed <- 1L
    if (!"rationale" %in% names(out)) out$rationale <- NA_character_

    if (!"exclude_values" %in% names(out)) {
      out$exclude_values <- I(lapply(out$exclude, normalise_values))
    } else {
      out$exclude_values <- I(lapply(out$exclude_values, normalise_values))
    }

    if (!"analysis_run_ids" %in% names(out)) {
      out$analysis_run_ids <- I(rep(list(character(0)), nrow(out)))
    } else {
      out$analysis_run_ids <- I(lapply(out$analysis_run_ids, normalise_values))
    }

    out$exclude <- vapply(
      out$exclude_values,
      function(x) if (length(x)) paste(x, collapse = " | ") else NA_character_,
      character(1)
    )
  } else {
    if (!is.list(robustness)) {
      stop("Robustness specifications must be a data frame or list of lists.", call. = FALSE)
    }

    rows <- lapply(seq_along(robustness), function(i) {
      item <- robustness[[i]]
      nm <- names(robustness)[i]
      if (is.null(item) || !is.list(item)) {
        stop("Each robustness specification must be a list.", call. = FALSE)
      }

      type <- tdabm_as_scalar_character(item$type %||% "original")
      values <- normalise_values(item$exclude_values %||% item$exclude %||% character(0))
      selection_mode <- tdabm_as_scalar_character(
        item$selection_mode %||% if (type %in% c("filter", "exclude", "manual_exclusion")) {
          "user_specified"
        } else {
          "not_applicable"
        }
      )

      data.frame(
        robustness_id = tdabm_as_scalar_character(item$robustness_id %||% nm),
        type = type,
        variable = tdabm_as_scalar_character(item$variable %||% NA_character_, default = NA_character_),
        exclude = if (length(values)) paste(values, collapse = " | ") else NA_character_,
        method = tdabm_as_scalar_character(item$method %||% NA_character_, default = NA_character_),
        enabled = tdabm_as_scalar_logical(item$enabled, default = TRUE),
        selection_mode = selection_mode,
        require_all_matches = tdabm_as_scalar_logical(item$require_all_matches, default = TRUE),
        minimum_removed = as.integer(item$minimum_removed %||% 1L),
        rationale = tdabm_as_scalar_character(item$rationale %||% NA_character_, default = NA_character_),
        stringsAsFactors = FALSE
      )
    })

    out <- do.call(rbind, rows)
    out$exclude_values <- I(lapply(seq_along(robustness), function(i) {
      item <- robustness[[i]]
      normalise_values(item$exclude_values %||% item$exclude %||% character(0))
    }))
    out$analysis_run_ids <- I(lapply(seq_along(robustness), function(i) {
      item <- robustness[[i]]
      normalise_values(item$analysis_run_ids %||% character(0))
    }))
  }

  out$robustness_id <- as.character(out$robustness_id)
  out$type <- as.character(out$type)
  out$variable <- as.character(out$variable)
  out$exclude <- as.character(out$exclude)
  out$method <- as.character(out$method)
  out$enabled <- as.logical(out$enabled)
  out$selection_mode <- as.character(out$selection_mode)
  out$require_all_matches <- as.logical(out$require_all_matches)
  out$minimum_removed <- as.integer(out$minimum_removed)
  out$rationale <- as.character(out$rationale)

  column_order <- c(
    "robustness_id", "type", "variable", "exclude", "method", "enabled",
    "selection_mode", "require_all_matches", "minimum_removed", "rationale",
    "exclude_values", "analysis_run_ids"
  )
  out[, column_order, drop = FALSE]
}

tdabm_required_columns_from_project <- function(project) {
  cols <- character(0)

  cols <- c(
    cols,
    project$data$id_col,
    project$data$label_col,
    project$data$region_col,
    project$topology$axes
  )

  if (!is.null(project$colourings) && nrow(project$colourings) > 0L) {
    variable_colourings <- project$colourings[
      !project$colourings$colour_type %in% c("constant", "computed") &
        !is.na(project$colourings$variable),
      ,
      drop = FALSE
    ]
    cols <- c(cols, variable_colourings$variable)
  }

  if (!is.null(project$robustness) && nrow(project$robustness) > 0L) {
    filter_rows <- project$robustness[
      project$robustness$type %in% c("filter", "exclude", "manual_exclusion"),
      ,
      drop = FALSE
    ]
    cols <- c(cols, filter_rows$variable)
  }

  unique(cols[!is.na(cols) & nzchar(cols)])
}
