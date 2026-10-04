# ==============================================================================
# TDABMRobustness.R
# ==============================================================================
# Robustness-execution layer for reusable TDABM projects.
#
# Stage 5 purpose:
#   This module consumes a validated TDABMProject object, creates robustness
#   variants of the project data, and reruns selected primary BMStats analyses.
#
# Supported robustness types:
#   - original  : no data transformation;
#   - filter/manual_exclusion: remove only values explicitly supplied by the user;
#   - exclude   : backward-compatible synonym for manual_exclusion;
#   - scaling   : rescale topology axes using mad, rank, z_score, standard,
#                 or as_supplied.
#
# Robustness identifiers and variables are supplied by each project adapter.
#
# The robustness module deliberately runs BMStats only. Membership robustness can
# be added later if needed.
#
# Source order:
#   source("R/TDABMFramework.R")
#   source("R/TDABMUtilities.R")
#   source("R/TDABMProject.R")
#   source("R/TDABMValidation.R")
#   source("R/TDABMExecution.R")
#   source("R/TDABMRobustness.R")
# ==============================================================================

if (!exists("is.TDABMProject", mode = "function")) {
  stop("Source TDABMProject.R before TDABMRobustness.R.", call. = FALSE)
}

if (!exists("RunTDABMPrimary", mode = "function")) {
  stop("Source TDABMExecution.R before TDABMRobustness.R.", call. = FALSE)
}

# ------------------------------------------------------------------------------
# Explicit user-controlled exclusion specification
# ------------------------------------------------------------------------------

TDABMManualExclusionSpec <- function(
  robustness_id,
  variable,
  values,
  analysis_run_ids = character(0),
  rationale = NA_character_,
  enabled = FALSE,
  require_all_matches = TRUE,
  minimum_removed = 1L
) {
  if (!tdabm_is_scalar_character(robustness_id)) {
    stop("robustness_id must be a non-empty character value.", call. = FALSE)
  }
  if (!tdabm_is_scalar_character(variable)) {
    stop("variable must be a non-empty character value.", call. = FALSE)
  }

  values <- unique(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0L) {
    stop("The user must supply at least one exact exclusion value.", call. = FALSE)
  }

  list(
    robustness_id = robustness_id,
    type = "manual_exclusion",
    variable = variable,
    exclude_values = values,
    selection_mode = "user_specified",
    require_all_matches = isTRUE(require_all_matches),
    minimum_removed = as.integer(minimum_removed),
    rationale = as.character(rationale)[1L],
    enabled = isTRUE(enabled),
    analysis_run_ids = as.character(analysis_run_ids)
  )
}

# ------------------------------------------------------------------------------
# Basic helpers
# ------------------------------------------------------------------------------

tdabm_clone_project <- function(project) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
  }

  unserialize(serialize(project, NULL))
}

tdabm_robustness_specs <- function(
  project,
  robustness_ids = NULL,
  enabled_only = TRUE,
  include_original = FALSE
) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
  }

  specs <- project$robustness

  if (!is.data.frame(specs) || nrow(specs) == 0L) {
    stop("No robustness specifications are defined in project$robustness.", call. = FALSE)
  }

  if (isTRUE(enabled_only)) {
    specs <- specs[specs$enabled %in% TRUE, , drop = FALSE]
  }

  if (!isTRUE(include_original)) {
    specs <- specs[!specs$type %in% "original", , drop = FALSE]
  }

  if (!is.null(robustness_ids)) {
    specs <- specs[specs$robustness_id %in% robustness_ids, , drop = FALSE]
  }

  row.names(specs) <- NULL
  specs
}

tdabm_robustness_run_ids <- function(project, spec, requested_run_ids = NULL) {
  spec <- as.data.frame(spec, stringsAsFactors = FALSE)

  if (nrow(spec) != 1L) {
    stop("Expected a single robustness specification row.", call. = FALSE)
  }

  spec_run_ids <- character(0)

  if ("analysis_run_ids" %in% names(spec)) {
    spec_run_ids <- unlist(spec$analysis_run_ids, use.names = FALSE)
  }

  if (length(spec_run_ids) == 0L) {
    spec_run_ids <- project$colourings$run_id[
      project$colourings$enabled %in% TRUE &
        project$colourings$run_robustness %in% TRUE
    ]
  }

  spec_run_ids <- unique(as.character(spec_run_ids))

  if (!is.null(requested_run_ids)) {
    spec_run_ids <- intersect(spec_run_ids, requested_run_ids)
  }

  spec_run_ids
}

tdabm_with_output_root <- function(project, output_root) {
  project$outputs$root <- output_root
  project$outputs$paths <- tdabm_make_output_paths(output_root)
  project
}

# ------------------------------------------------------------------------------
# Dataset transformations
# ------------------------------------------------------------------------------

tdabm_filter_data <- function(
  df,
  variable,
  exclude,
  robustness_id,
  require_all_matches = TRUE,
  minimum_removed = 1L,
  id_col = NULL,
  label_col = NULL
) {
  if (!tdabm_is_scalar_character(variable) || !variable %in% names(df)) {
    stop(
      "Robustness exclusion variable not found for ",
      robustness_id,
      ": ",
      variable,
      call. = FALSE
    )
  }

  exclude <- unique(as.character(exclude))
  exclude <- exclude[!is.na(exclude) & nzchar(exclude)]
  if (length(exclude) == 0L) {
    stop(
      "No explicit user-supplied exclusion values were provided for ",
      robustness_id,
      ".",
      call. = FALSE
    )
  }

  data_values <- as.character(df[[variable]])
  present <- exclude %in% unique(data_values)
  missing_values <- exclude[!present]

  if (isTRUE(require_all_matches) && length(missing_values) > 0L) {
    stop(
      "User-specified exclusion values were not found for ",
      robustness_id,
      ". Variable: ", variable,
      "; missing: ", paste(missing_values, collapse = ", "),
      call. = FALSE
    )
  }

  remove <- data_values %in% exclude
  n_before <- nrow(df)
  n_removed <- sum(remove)
  minimum_removed <- as.integer(minimum_removed)

  if (!is.finite(minimum_removed) || minimum_removed < 1L) {
    stop("minimum_removed must be at least one.", call. = FALSE)
  }

  if (n_removed < minimum_removed) {
    stop(
      "User-specified exclusion removed fewer observations than required for ",
      robustness_id,
      ". Required: ", minimum_removed,
      "; removed: ", n_removed,
      call. = FALSE
    )
  }

  if (n_removed >= n_before) {
    stop(
      "User-specified exclusion removed every observation for ",
      robustness_id,
      ".",
      call. = FALSE
    )
  }

  keep <- !remove
  out <- df[keep, , drop = FALSE]

  audit_summary <- data.frame(
    robustness_id = robustness_id,
    action = "user_specified_exclusion",
    selection_mode = "user_specified",
    variable = variable,
    requested_values = paste(exclude, collapse = " | "),
    matched_values = paste(exclude[present], collapse = " | "),
    missing_values = paste(missing_values, collapse = " | "),
    require_all_matches = isTRUE(require_all_matches),
    minimum_removed = minimum_removed,
    n_before = n_before,
    n_removed = n_removed,
    n_after = nrow(out),
    automatic_exclusion_performed = FALSE,
    stringsAsFactors = FALSE
  )

  audit_cols <- unique(c(id_col, label_col, variable))
  audit_cols <- audit_cols[!is.na(audit_cols) & nzchar(audit_cols) & audit_cols %in% names(df)]
  removed_rows <- df[remove, audit_cols, drop = FALSE]
  if (nrow(removed_rows) > 0L) {
    removed_rows$robustness_id <- robustness_id
    removed_rows$selection_mode <- "user_specified"
    removed_rows$automatic_exclusion_performed <- FALSE
    front <- c("robustness_id", "selection_mode", "automatic_exclusion_performed")
    removed_rows <- removed_rows[, c(front, setdiff(names(removed_rows), front)), drop = FALSE]
  }

  attr(out, "tdabm_robustness_transform_audit") <- list(
    summary = audit_summary,
    removed_observations = removed_rows
  )
  out
}

tdabm_scale_axis_vector <- function(x, method, axis_name) {
  if (!is.numeric(x)) {
    stop("Axis must be numeric before scaling: ", axis_name, call. = FALSE)
  }

  if (anyNA(x) || any(!is.finite(x))) {
    stop("Axis contains missing or non-finite values: ", axis_name, call. = FALSE)
  }

  if (method %in% c("as_supplied", "none")) {
    return(x)
  }

  if (method %in% c("z_score", "standard")) {
    s <- stats::sd(x)
    if (!is.finite(s) || s <= 0) {
      stop("Axis has no usable variation under z-score scaling: ", axis_name, call. = FALSE)
    }
    return(as.numeric((x - mean(x)) / s))
  }

  if (method == "mad") {
    centre <- stats::median(x)
    scale <- stats::mad(x, center = centre, constant = 1.4826, na.rm = FALSE)

    if (!is.finite(scale) || scale <= 0) {
      stop("Axis has no usable variation under MAD scaling: ", axis_name, call. = FALSE)
    }

    return(as.numeric((x - centre) / scale))
  }

  if (method == "rank") {
    r <- rank(x, ties.method = "average")
    s <- stats::sd(r)

    if (!is.finite(s) || s <= 0) {
      stop("Axis has no usable variation under rank scaling: ", axis_name, call. = FALSE)
    }

    return(as.numeric((r - mean(r)) / s))
  }

  stop("Unsupported scaling method: ", method, call. = FALSE)
}

tdabm_apply_axis_scaling <- function(df, axes, method, robustness_id) {
  missing_axes <- setdiff(axes, names(df))

  if (length(missing_axes) > 0L) {
    stop(
      "Cannot apply scaling for ",
      robustness_id,
      "; missing axes: ",
      paste(missing_axes, collapse = ", "),
      call. = FALSE
    )
  }

  out <- df

  for (axis in axes) {
    out[[axis]] <- tdabm_scale_axis_vector(out[[axis]], method = method, axis_name = axis)
  }

  out
}

tdabm_apply_robustness_transform <- function(project, spec) {
  spec <- as.data.frame(spec, stringsAsFactors = FALSE)

  if (nrow(spec) != 1L) {
    stop("Expected a single robustness specification row.", call. = FALSE)
  }

  robustness_id <- as.character(spec$robustness_id[[1L]])
  type <- as.character(spec$type[[1L]])

  data_result <- tdabm_project_get_dataset(project)
  df <- as.data.frame(data_result$data)
  axes <- project$topology$axes

  if (length(axes) == 0L && !is.null(data_result$axes)) {
    axes <- as.character(data_result$axes)
  }

  if (type == "original") {
    transformed <- df
  } else if (type %in% c("filter", "exclude", "manual_exclusion")) {
    exclude_values <- if ("exclude_values" %in% names(spec)) {
      unlist(spec$exclude_values[[1L]], use.names = FALSE)
    } else {
      spec$exclude[[1L]]
    }
    transformed <- tdabm_filter_data(
      df = df,
      variable = spec$variable[[1L]],
      exclude = exclude_values,
      robustness_id = robustness_id,
      require_all_matches = if ("require_all_matches" %in% names(spec)) isTRUE(spec$require_all_matches[[1L]]) else TRUE,
      minimum_removed = if ("minimum_removed" %in% names(spec)) as.integer(spec$minimum_removed[[1L]]) else 1L,
      id_col = project$data$id_col,
      label_col = project$data$label_col
    )
  } else if (type == "scaling") {
    transformed <- tdabm_apply_axis_scaling(
      df = df,
      axes = axes,
      method = spec$method[[1L]],
      robustness_id = robustness_id
    )
  } else {
    stop("Unsupported robustness type: ", type, call. = FALSE)
  }

  transformed
}

tdabm_make_robust_project <- function(project, spec) {
  spec <- as.data.frame(spec, stringsAsFactors = FALSE)

  if (nrow(spec) != 1L) {
    stop("Expected a single robustness specification row.", call. = FALSE)
  }

  robustness_id <- as.character(spec$robustness_id[[1L]])
  type <- as.character(spec$type[[1L]])

  robust_project <- tdabm_clone_project(project)
  transformed <- tdabm_apply_robustness_transform(project, spec)
  transform_audit <- attr(transformed, "tdabm_robustness_transform_audit")

  robust_project$metadata$project_id <- paste0(project$metadata$project_id, "__", robustness_id)
  robust_project$metadata$project_name <- paste0(project$metadata$project_name, " [", robustness_id, "]")

  robust_project$data$data_format <- "data_frame"
  robust_project$data$data_frame <- transformed
  robust_project$data$input_rds <- NA_character_
  robust_project$data$dataset_name <- paste0(project$data$dataset_name, "__", robustness_id)

  robust_project$robustness_active <- as.list(spec[1, , drop = FALSE])
  robust_project$robustness_transform_audit <- transform_audit

  robust_project <- tdabm_with_output_root(
    robust_project,
    file.path(project$outputs$paths$root, "robustness", robustness_id)
  )

  robust_project <- tdabm_append_history(
    robust_project,
    action = "robustness_transform",
    detail = paste0("Applied robustness specification: ", robustness_id, " (", type, ")")
  )

  robust_project
}

# ------------------------------------------------------------------------------
# Public robustness runner
# ------------------------------------------------------------------------------

RunTDABMRobustness <- function(
  project,
  robustness_ids = NULL,
  run_ids = NULL,
  include_original = FALSE,
  overwrite = FALSE,
  generate_plots = TRUE,
  validate_first = TRUE,
  stop_on_validation_error = TRUE,
  verbose = TRUE
) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
  }

  if (isTRUE(validate_first)) {
    if (!exists("validate_tdabm_project", mode = "function")) {
      stop("validate_tdabm_project() is not available. Source TDABMValidation.R first.", call. = FALSE)
    }

    validation <- validate_tdabm_project(
      project,
      check_files = TRUE,
      check_data = TRUE,
      create_dirs = TRUE,
      run_dependency_smoke_test = FALSE
    )

    project$validation <- validation

    if (!isTRUE(validation$ok) && isTRUE(stop_on_validation_error)) {
      print(validation)
      stop("Project validation failed. Robustness execution did not start.", call. = FALSE)
    }
  }

  tdabm_create_output_directories(project$outputs$paths, verbose = FALSE)

  specs <- tdabm_robustness_specs(
    project = project,
    robustness_ids = robustness_ids,
    enabled_only = TRUE,
    include_original = include_original
  )

  if (nrow(specs) == 0L) {
    stop("No robustness specifications selected.", call. = FALSE)
  }

  if (isTRUE(verbose)) {
    message("\nSelected robustness specifications:")
    print(specs[, c("robustness_id", "type", "variable", "exclude", "method"), drop = FALSE])
  }

  results <- list()
  radius_one_rows <- list()
  audit_rows <- list()
  transform_audit_rows <- list()
  removed_observation_rows <- list()

  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, , drop = FALSE]
    robustness_id <- as.character(spec$robustness_id[[1L]])

    selected_run_ids <- tdabm_robustness_run_ids(
      project = project,
      spec = spec,
      requested_run_ids = run_ids
    )

    if (length(selected_run_ids) == 0L) {
      warning(
        "Skipping robustness specification with no selected run IDs: ",
        robustness_id
      )
      next
    }

    if (isTRUE(verbose)) {
      message("\n============================================================")
      message("Starting robustness specification: ", robustness_id)
      message("Selected run IDs: ", paste(selected_run_ids, collapse = ", "))
      message("============================================================")
    }

    robust_project <- tdabm_make_robust_project(project, spec)

    if (!is.null(robust_project$robustness_transform_audit)) {
      ta <- robust_project$robustness_transform_audit
      if (!is.null(ta$summary) && nrow(ta$summary) > 0L) {
        transform_audit_rows[[robustness_id]] <- ta$summary
      }
      if (!is.null(ta$removed_observations) && nrow(ta$removed_observations) > 0L) {
        removed_observation_rows[[robustness_id]] <- ta$removed_observations
      }
    }

    # Keep only selected run IDs enabled inside the robustness project.
    robust_project$colourings$enabled <- robust_project$colourings$run_id %in% selected_run_ids
    robust_project$colourings$run_primary <- robust_project$colourings$run_id %in% selected_run_ids

    execution <- RunTDABMPrimary(
      project = robust_project,
      run_ids = selected_run_ids,
      overwrite = overwrite,
      generate_plots = generate_plots,
      validate_first = TRUE,
      stop_on_validation_error = stop_on_validation_error,
      verbose = verbose
    )

    results[[robustness_id]] <- execution

    if (!is.null(execution$radius_one) && nrow(execution$radius_one) > 0L) {
      x <- execution$radius_one
      x$robustness_id <- robustness_id
      x$robustness_type <- as.character(spec$type[[1L]])
      x$robustness_variable <- as.character(spec$variable[[1L]])
      x$robustness_exclude <- as.character(spec$exclude[[1L]])
      x$robustness_method <- as.character(spec$method[[1L]])

      front <- intersect(
        c(
          "robustness_id",
          "robustness_type",
          "robustness_variable",
          "robustness_exclude",
          "robustness_method",
          "run_id",
          "variable",
          "label",
          "eps"
        ),
        names(x)
      )

      x <- x[, c(front, setdiff(names(x), front)), drop = FALSE]
      radius_one_rows[[robustness_id]] <- x
    }

    if (!is.null(execution$audit) && nrow(execution$audit) > 0L) {
      a <- execution$audit
      a$robustness_id <- robustness_id
      a$robustness_type <- as.character(spec$type[[1L]])
      a$robustness_variable <- as.character(spec$variable[[1L]])
      a$robustness_exclude <- as.character(spec$exclude[[1L]])
      a$robustness_method <- as.character(spec$method[[1L]])

      front <- intersect(
        c(
          "robustness_id",
          "robustness_type",
          "robustness_variable",
          "robustness_exclude",
          "robustness_method",
          "run_id"
        ),
        names(a)
      )

      a <- a[, c(front, setdiff(names(a), front)), drop = FALSE]
      audit_rows[[robustness_id]] <- a
    }
  }

  combined_radius_one <- if (length(radius_one_rows) > 0L) {
    do.call(rbind, radius_one_rows)
  } else {
    data.frame()
  }

  combined_audit <- if (length(audit_rows) > 0L) {
    do.call(rbind, audit_rows)
  } else {
    data.frame()
  }

  combined_transform_audit <- if (length(transform_audit_rows) > 0L) {
    do.call(rbind, transform_audit_rows)
  } else {
    data.frame()
  }

  combined_removed_observations <- if (length(removed_observation_rows) > 0L) {
    do.call(rbind, removed_observation_rows)
  } else {
    data.frame()
  }

  # Write combined robustness outputs to the parent project's verification folder.
  tdabm_create_dir(project$outputs$paths$verification)

  if (nrow(combined_radius_one) > 0L) {
    tdabm_write_table(
      combined_radius_one,
      file.path(project$outputs$paths$verification, "robustness_radius_1_results.csv")
    )
  }

  if (nrow(combined_audit) > 0L) {
    tdabm_write_table(
      combined_audit,
      file.path(project$outputs$paths$verification, "robustness_execution_audit.csv")
    )
  }

  if (nrow(combined_transform_audit) > 0L) {
    tdabm_write_table(
      combined_transform_audit,
      file.path(project$outputs$paths$verification, "robustness_transform_audit.csv")
    )
  }

  if (nrow(combined_removed_observations) > 0L) {
    tdabm_write_table(
      combined_removed_observations,
      file.path(project$outputs$paths$verification, "robustness_removed_observations.csv")
    )
  }

  project <- tdabm_append_history(
    project,
    action = "robustness_execution",
    detail = paste0("Completed ", length(results), " robustness specification(s).")
  )

  saveRDS(
    list(
      project = project,
      results = results,
      radius_one = combined_radius_one,
      audit = combined_audit,
      transform_audit = combined_transform_audit,
      removed_observations = combined_removed_observations
    ),
    file.path(project$outputs$paths$logs, "tdabm_robustness_execution_latest.rds")
  )

  invisible(list(
    project = project,
    results = results,
    radius_one = combined_radius_one,
    audit = combined_audit,
    transform_audit = combined_transform_audit,
    removed_observations = combined_removed_observations
  ))
}
