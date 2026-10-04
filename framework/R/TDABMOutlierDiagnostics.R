# ==============================================================================
# TDABMOutlierDiagnostics.R
# ==============================================================================
# Diagnostic-only candidate identification for portable TDABM projects.
#
# This module never removes observations, never edits project$robustness, and
# never constructs an exclusion specification. It produces review tables that
# may help a user decide whether a separately declared sensitivity analysis is
# substantively justified.
#
# Variables can be drawn from:
#   - declared topology axes;
#   - enabled numeric colour variables.
#
# Supported diagnostic methods:
#   - mad: median-centred robust z scores;
#   - iqr: Tukey-style lower and upper fences.
# ============================================================================== 

if (!exists("is.TDABMProject", mode = "function")) {
  stop("Source TDABMProject.R before TDABMOutlierDiagnostics.R.", call. = FALSE)
}

# ------------------------------------------------------------------------------
# Variable registry
# ------------------------------------------------------------------------------

tdabm_outlier_variable_registry <- function(
  project,
  df,
  include_axes = TRUE,
  include_colourings = TRUE,
  enabled_colourings_only = TRUE
) {
  rows <- list()

  if (isTRUE(include_axes)) {
    axes <- unique(as.character(project$topology$axes %||% character(0)))
    axes <- axes[nzchar(axes)]
    for (variable in axes) {
      rows[[length(rows) + 1L]] <- data.frame(
        variable = variable,
        role = "axis",
        source_run_id = NA_character_,
        source_label = project$topology$description %||% "Topology axis",
        stringsAsFactors = FALSE
      )
    }
  }

  if (isTRUE(include_colourings) && is.data.frame(project$colourings)) {
    colourings <- project$colourings
    if (isTRUE(enabled_colourings_only)) {
      colourings <- colourings[colourings$enabled %in% TRUE, , drop = FALSE]
    }
    colourings <- colourings[
      !colourings$colour_type %in% c("constant", "computed") &
        !is.na(colourings$variable) & nzchar(colourings$variable),
      ,
      drop = FALSE
    ]

    if (nrow(colourings) > 0L) {
      for (i in seq_len(nrow(colourings))) {
        rows[[length(rows) + 1L]] <- data.frame(
          variable = as.character(colourings$variable[[i]]),
          role = "colour",
          source_run_id = as.character(colourings$run_id[[i]]),
          source_label = as.character(colourings$label[[i]]),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(rows) == 0L) {
    return(data.frame(
      variable = character(0), role = character(0),
      source_run_id = character(0), source_label = character(0),
      stringsAsFactors = FALSE
    ))
  }

  raw <- do.call(rbind, rows)
  raw <- raw[raw$variable %in% names(df), , drop = FALSE]
  variables <- unique(raw$variable)

  combined <- lapply(variables, function(variable) {
    x <- raw[raw$variable == variable, , drop = FALSE]
    data.frame(
      variable = variable,
      roles = paste(unique(x$role), collapse = " | "),
      source_run_ids = paste(unique(x$source_run_id[!is.na(x$source_run_id)]), collapse = " | "),
      source_labels = paste(unique(x$source_label[!is.na(x$source_label)]), collapse = " | "),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, combined)
}

# ------------------------------------------------------------------------------
# Single-variable diagnostics
# ------------------------------------------------------------------------------

tdabm_outlier_diagnose_vector <- function(
  x,
  method = c("mad", "iqr"),
  mad_threshold = 3.5,
  iqr_multiplier = 1.5
) {
  method <- match.arg(method)

  if (!is.numeric(x)) {
    return(list(
      ok = FALSE,
      status = "non_numeric",
      centre = NA_real_,
      scale = NA_real_,
      lower_bound = NA_real_,
      upper_bound = NA_real_,
      score = rep(NA_real_, length(x)),
      flag = rep(FALSE, length(x))
    ))
  }

  if (anyNA(x) || any(!is.finite(x))) {
    return(list(
      ok = FALSE,
      status = "missing_or_non_finite",
      centre = NA_real_,
      scale = NA_real_,
      lower_bound = NA_real_,
      upper_bound = NA_real_,
      score = rep(NA_real_, length(x)),
      flag = rep(FALSE, length(x))
    ))
  }

  if (method == "mad") {
    centre <- stats::median(x)
    scale <- stats::mad(x, center = centre, constant = 1.4826, na.rm = FALSE)

    if (!is.finite(scale) || scale <= 0) {
      return(list(
        ok = FALSE,
        status = "zero_robust_scale",
        centre = centre,
        scale = scale,
        lower_bound = centre,
        upper_bound = centre,
        score = rep(0, length(x)),
        flag = rep(FALSE, length(x))
      ))
    }

    score <- (x - centre) / scale
    lower <- centre - mad_threshold * scale
    upper <- centre + mad_threshold * scale
    flag <- abs(score) >= mad_threshold

    return(list(
      ok = TRUE,
      status = "analysed",
      centre = centre,
      scale = scale,
      lower_bound = lower,
      upper_bound = upper,
      score = score,
      flag = flag
    ))
  }

  q <- stats::quantile(x, probs = c(0.25, 0.50, 0.75), names = FALSE, type = 7)
  spread <- q[[3L]] - q[[1L]]

  if (!is.finite(spread) || spread <= 0) {
    return(list(
      ok = FALSE,
      status = "zero_iqr",
      centre = q[[2L]],
      scale = spread,
      lower_bound = q[[1L]],
      upper_bound = q[[3L]],
      score = rep(0, length(x)),
      flag = rep(FALSE, length(x))
    ))
  }

  lower <- q[[1L]] - iqr_multiplier * spread
  upper <- q[[3L]] + iqr_multiplier * spread
  score <- (x - q[[2L]]) / spread
  flag <- x < lower | x > upper

  list(
    ok = TRUE,
    status = "analysed",
    centre = q[[2L]],
    scale = spread,
    lower_bound = lower,
    upper_bound = upper,
    score = score,
    flag = flag
  )
}

# ------------------------------------------------------------------------------
# Public diagnostic runner
# ------------------------------------------------------------------------------

tdabm_outlier_suggestions <- function(
  project,
  method = c("mad", "iqr"),
  mad_threshold = 3.5,
  iqr_multiplier = 1.5,
  include_axes = TRUE,
  include_colourings = TRUE,
  enabled_colourings_only = TRUE,
  output_dir = NULL,
  write_outputs = !is.null(output_dir),
  verbose = TRUE
) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
  }

  method <- match.arg(method)

  if (!is.numeric(mad_threshold) || length(mad_threshold) != 1L ||
      !is.finite(mad_threshold) || mad_threshold <= 0) {
    stop("mad_threshold must be one positive finite number.", call. = FALSE)
  }

  if (!is.numeric(iqr_multiplier) || length(iqr_multiplier) != 1L ||
      !is.finite(iqr_multiplier) || iqr_multiplier <= 0) {
    stop("iqr_multiplier must be one positive finite number.", call. = FALSE)
  }

  data_result <- tdabm_project_get_dataset(project)
  df <- as.data.frame(data_result$data)
  id_col <- project$data$id_col
  label_col <- project$data$label_col

  if (!tdabm_is_scalar_character(id_col) || !id_col %in% names(df)) {
    stop("Project id column is required for diagnostic suggestions: ", id_col, call. = FALSE)
  }

  registry <- tdabm_outlier_variable_registry(
    project = project,
    df = df,
    include_axes = include_axes,
    include_colourings = include_colourings,
    enabled_colourings_only = enabled_colourings_only
  )

  if (nrow(registry) == 0L) {
    stop("No eligible axis or colour variables were found for diagnostics.", call. = FALSE)
  }

  long_rows <- list()
  audit_rows <- list()

  for (i in seq_len(nrow(registry))) {
    variable <- registry$variable[[i]]
    diagnosis <- tdabm_outlier_diagnose_vector(
      x = df[[variable]],
      method = method,
      mad_threshold = mad_threshold,
      iqr_multiplier = iqr_multiplier
    )

    audit_rows[[variable]] <- data.frame(
      variable = variable,
      roles = registry$roles[[i]],
      source_run_ids = registry$source_run_ids[[i]],
      source_labels = registry$source_labels[[i]],
      method = method,
      status = diagnosis$status,
      centre = diagnosis$centre,
      scale = diagnosis$scale,
      lower_bound = diagnosis$lower_bound,
      upper_bound = diagnosis$upper_bound,
      n_observations = nrow(df),
      n_flagged = sum(diagnosis$flag),
      stringsAsFactors = FALSE
    )

    flagged <- which(diagnosis$flag)
    if (length(flagged) > 0L) {
      labels <- if (tdabm_is_scalar_character(label_col) && label_col %in% names(df)) {
        as.character(df[[label_col]][flagged])
      } else {
        rep(NA_character_, length(flagged))
      }

      long_rows[[variable]] <- data.frame(
        observation_id = as.character(df[[id_col]][flagged]),
        observation_label = labels,
        variable = variable,
        roles = registry$roles[[i]],
        source_run_ids = registry$source_run_ids[[i]],
        source_labels = registry$source_labels[[i]],
        value = as.numeric(df[[variable]][flagged]),
        method = method,
        centre = diagnosis$centre,
        scale = diagnosis$scale,
        lower_bound = diagnosis$lower_bound,
        upper_bound = diagnosis$upper_bound,
        diagnostic_score = as.numeric(diagnosis$score[flagged]),
        absolute_diagnostic_score = abs(as.numeric(diagnosis$score[flagged])),
        review_status = "candidate_for_user_review_only",
        automatic_exclusion_performed = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }

  long <- if (length(long_rows)) do.call(rbind, long_rows) else data.frame(
    observation_id = character(0), observation_label = character(0),
    variable = character(0), roles = character(0), source_run_ids = character(0),
    source_labels = character(0), value = numeric(0), method = character(0),
    centre = numeric(0), scale = numeric(0), lower_bound = numeric(0),
    upper_bound = numeric(0), diagnostic_score = numeric(0),
    absolute_diagnostic_score = numeric(0), review_status = character(0),
    automatic_exclusion_performed = logical(0), stringsAsFactors = FALSE
  )

  variable_audit <- do.call(rbind, audit_rows)
  row.names(variable_audit) <- NULL

  if (nrow(long) > 0L) {
    split_rows <- split(long, long$observation_id)
    by_observation <- do.call(rbind, lapply(split_rows, function(x) {
      data.frame(
        observation_id = x$observation_id[[1L]],
        observation_label = x$observation_label[[1L]],
        n_flagged_variables = nrow(x),
        n_axis_flags = sum(grepl("axis", x$roles, fixed = TRUE)),
        n_colour_flags = sum(grepl("colour", x$roles, fixed = TRUE)),
        maximum_absolute_diagnostic_score = max(x$absolute_diagnostic_score, na.rm = TRUE),
        flagged_variables = paste(unique(x$variable), collapse = " | "),
        review_status = "candidate_for_user_review_only",
        automatic_exclusion_performed = FALSE,
        stringsAsFactors = FALSE
      )
    }))
    row.names(by_observation) <- NULL
    by_observation <- by_observation[
      order(-by_observation$n_flagged_variables,
            -by_observation$maximum_absolute_diagnostic_score,
            by_observation$observation_id),
      ,
      drop = FALSE
    ]
  } else {
    by_observation <- data.frame(
      observation_id = character(0), observation_label = character(0),
      n_flagged_variables = integer(0), n_axis_flags = integer(0),
      n_colour_flags = integer(0), maximum_absolute_diagnostic_score = numeric(0),
      flagged_variables = character(0), review_status = character(0),
      automatic_exclusion_performed = logical(0), stringsAsFactors = FALSE
    )
  }

  settings <- data.frame(
    project_id = project$metadata$project_id,
    framework_version = project$metadata$framework_version,
    method = method,
    mad_threshold = mad_threshold,
    iqr_multiplier = iqr_multiplier,
    include_axes = isTRUE(include_axes),
    include_colourings = isTRUE(include_colourings),
    enabled_colourings_only = isTRUE(enabled_colourings_only),
    n_variables_considered = nrow(registry),
    n_candidate_observations = nrow(by_observation),
    automatic_exclusion_performed = FALSE,
    user_decision_required = TRUE,
    generated_at = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  files <- list()
  if (isTRUE(write_outputs)) {
    if (is.null(output_dir) || length(output_dir) != 1L || !nzchar(output_dir)) {
      stop("Provide output_dir when write_outputs is TRUE.", call. = FALSE)
    }
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    files <- list(
      long = file.path(output_dir, "outlier_suggestions_long.csv"),
      by_observation = file.path(output_dir, "outlier_suggestions_by_observation.csv"),
      variable_audit = file.path(output_dir, "outlier_diagnostic_variable_audit.csv"),
      settings = file.path(output_dir, "outlier_diagnostic_settings.csv")
    )

    utils::write.csv(long, files$long, row.names = FALSE, na = "")
    utils::write.csv(by_observation, files$by_observation, row.names = FALSE, na = "")
    utils::write.csv(variable_audit, files$variable_audit, row.names = FALSE, na = "")
    utils::write.csv(settings, files$settings, row.names = FALSE, na = "")
  }

  if (isTRUE(verbose)) {
    cat("\nTDABM outlier diagnostic suggestions complete.\n")
    cat("Method: ", method, "\n", sep = "")
    cat("Variables considered: ", nrow(registry), "\n", sep = "")
    cat("Candidate observations: ", nrow(by_observation), "\n", sep = "")
    cat("Automatic exclusions performed: no\n")
    if (isTRUE(write_outputs)) cat("Output directory: ", output_dir, "\n", sep = "")
  }

  invisible(list(
    settings = settings,
    variable_registry = registry,
    variable_audit = variable_audit,
    suggestions_long = long,
    suggestions_by_observation = by_observation,
    files = files
  ))
}
