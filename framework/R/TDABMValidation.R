# ==============================================================================
# TDABMValidation.R
# ==============================================================================
# Formal validation layer for reusable TDABM projects.
#
# Stage 2 purpose:
#   This file upgrades validation from a small constructor check into a structured
#   diagnostic system. It validates project structure, paths, dependencies, data,
#   identifiers, topology axes, colourings, membership/focal observations,
#   robustness specifications, radius settings, and parallel settings.
#
# Source order:
#   source("R/TDABMFramework.R")
#   source("R/TDABMUtilities.R")
#   source("R/TDABMProject.R")
#   source("R/TDABMValidation.R")
#
# This file deliberately does not run BMStats(), BMStatsMembership(), or any
# Ball Mapper construction. It is a pre-flight check.
# ==============================================================================

if (!exists("is.TDABMProject", mode = "function")) {
  stop("Source TDABMProject.R before TDABMValidation.R.", call. = FALSE)
}

tdabm_validation_new <- function(project_id = NA_character_) {
  checks <- data.frame(
    time = character(0),
    section = character(0),
    item = character(0),
    severity = character(0),
    ok = logical(0),
    message = character(0),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      project_id = project_id,
      ok = TRUE,
      checks = checks,
      errors = character(0),
      warnings = character(0),
      notes = character(0),
      started_at = as.character(Sys.time()),
      completed_at = NA_character_
    ),
    class = "TDABMValidation"
  )
}

tdabm_validation_add <- function(
  validation,
  section,
  item,
  ok,
  message,
  severity = c("ERROR", "WARNING", "NOTE")
) {
  severity <- toupper(match.arg(severity))
  ok <- isTRUE(ok)

  validation$checks <- rbind(
    validation$checks,
    data.frame(
      time = as.character(Sys.time()),
      section = as.character(section),
      item = as.character(item),
      severity = severity,
      ok = ok,
      message = as.character(message),
      stringsAsFactors = FALSE
    )
  )

  if (!ok && identical(severity, "ERROR")) {
    validation$ok <- FALSE
    validation$errors <- c(validation$errors, message)
  } else if (!ok && identical(severity, "WARNING")) {
    validation$warnings <- c(validation$warnings, message)
  } else if (!ok && identical(severity, "NOTE")) {
    validation$notes <- c(validation$notes, message)
  }

  validation
}

tdabm_validation_finalize <- function(validation) {
  validation$completed_at <- as.character(Sys.time())
  validation
}

tdabm_validation_failed_checks <- function(validation) {
  validation$checks[!validation$checks$ok, , drop = FALSE]
}

tdabm_validation_section_summary <- function(validation) {
  checks <- validation$checks

  if (nrow(checks) == 0L) {
    return(data.frame(
      section = character(0),
      checks = integer(0),
      failed = integer(0),
      errors = integer(0),
      warnings = integer(0),
      notes = integer(0),
      status = character(0),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(unique(checks$section), function(sec) {
    x <- checks[checks$section == sec, , drop = FALSE]
    failed <- sum(!x$ok)
    errors <- sum(!x$ok & x$severity == "ERROR")
    warnings <- sum(!x$ok & x$severity == "WARNING")
    notes <- sum(!x$ok & x$severity == "NOTE")

    status <- if (errors > 0L) {
      "FAIL"
    } else if (warnings > 0L) {
      "WARNING"
    } else if (notes > 0L) {
      "NOTE"
    } else {
      "PASS"
    }

    data.frame(
      section = sec,
      checks = nrow(x),
      failed = failed,
      errors = errors,
      warnings = warnings,
      notes = notes,
      status = status,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

print.TDABMValidation <- function(x, ...) {
  cat("\n")
  cat("============================================================\n")
  cat("TDABM validation report\n")
  cat("============================================================\n")
  cat("Project ID: ", x$project_id, "\n", sep = "")
  cat("Status:     ", if (isTRUE(x$ok)) "PASS" else "FAIL", "\n", sep = "")
  cat("Checks:     ", nrow(x$checks), "\n", sep = "")
  cat("Errors:     ", length(x$errors), "\n", sep = "")
  cat("Warnings:   ", length(x$warnings), "\n", sep = "")
  cat("Notes:      ", length(x$notes), "\n", sep = "")

  summary <- tdabm_validation_section_summary(x)

  if (nrow(summary) > 0L) {
    cat("\nSection summary\n")
    print(summary, row.names = FALSE)
  }

  failed <- tdabm_validation_failed_checks(x)

  if (nrow(failed) > 0L) {
    cat("\nFailed checks\n")
    print(
      failed[, c("section", "item", "severity", "message"), drop = FALSE],
      row.names = FALSE
    )
  }

  cat("============================================================\n")
  invisible(x)
}

as.data.frame.TDABMValidation <- function(x, ...) {
  x$checks
}

write_tdabm_validation_report <- function(
  validation,
  output_dir,
  prefix = "tdabm_validation",
  include_timestamp = TRUE
) {
  if (!inherits(validation, "TDABMValidation")) {
    stop("Expected a TDABMValidation object.", call. = FALSE)
  }

  if (is.null(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir)) {
    stop("Provide a valid output directory.", call. = FALSE)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(output_dir)) {
    stop("Could not create validation output directory: ", output_dir, call. = FALSE)
  }

  suffix <- if (isTRUE(include_timestamp)) {
    paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  } else {
    ""
  }

  checks_file <- file.path(output_dir, paste0(prefix, "_checks", suffix, ".csv"))
  summary_file <- file.path(output_dir, paste0(prefix, "_section_summary", suffix, ".csv"))
  rds_file <- file.path(output_dir, paste0(prefix, suffix, ".rds"))

  utils::write.csv(validation$checks, checks_file, row.names = FALSE)
  utils::write.csv(tdabm_validation_section_summary(validation), summary_file, row.names = FALSE)
  saveRDS(validation, rds_file)

  invisible(list(
    checks_file = checks_file,
    summary_file = summary_file,
    rds_file = rds_file
  ))
}

tdabm_validate_project_class <- function(project, validation) {
  tdabm_validation_add(
    validation,
    "project",
    "class",
    is.TDABMProject(project),
    "Object must inherit from TDABMProject."
  )
}

tdabm_validate_required_sections <- function(project, validation) {
  required <- if (exists("TDABMRequiredProjectSections", mode = "function")) {
    TDABMRequiredProjectSections()
  } else {
    c(
      "metadata", "paths", "data", "topology", "colourings", "radius",
      "parallel", "outputs", "membership", "dependencies", "robustness",
      "history", "validation"
    )
  }

  for (section in required) {
    validation <- tdabm_validation_add(
      validation,
      "project",
      paste0("section_", section),
      section %in% names(project),
      paste0("Project object must contain section: ", section)
    )
  }

  validation
}

tdabm_validate_metadata <- function(project, validation) {
  md <- project$metadata

  validation <- tdabm_validation_add(
    validation,
    "metadata",
    "project_id",
    tdabm_is_scalar_character(md$project_id),
    "metadata$project_id must be a non-empty character scalar."
  )

  validation <- tdabm_validation_add(
    validation,
    "metadata",
    "project_name",
    tdabm_is_scalar_character(md$project_name),
    "metadata$project_name must be a non-empty character scalar."
  )

  validation <- tdabm_validation_add(
    validation,
    "metadata",
    "version",
    tdabm_is_scalar_character(md$version),
    "metadata$version must be a non-empty character scalar."
  )

  validation <- tdabm_validation_add(
    validation,
    "metadata",
    "framework_version",
    "framework_version" %in% names(md),
    "metadata$framework_version should be recorded.",
    severity = "WARNING"
  )

  validation
}

tdabm_validate_paths <- function(
  project,
  validation,
  check_files = TRUE,
  create_dirs = FALSE
) {
  project_root <- project$paths$project_root

  validation <- tdabm_validation_add(
    validation,
    "paths",
    "project_root_scalar",
    tdabm_is_scalar_character(project_root),
    "Project root must be a non-empty character scalar."
  )

  if (isTRUE(check_files)) {
    validation <- tdabm_validation_add(
      validation,
      "paths",
      "project_root_exists",
      dir.exists(project_root),
      paste0("Project root must exist: ", project_root)
    )
  }

  output_paths <- project$outputs$paths

  validation <- tdabm_validation_add(
    validation,
    "outputs",
    "output_paths_exist",
    is.list(output_paths) && "root" %in% names(output_paths),
    "Project outputs must contain a named list of output paths including root."
  )

  if (isTRUE(create_dirs) && is.list(output_paths)) {
    created_ok <- TRUE
    created_message <- "Output directories are available."

    tryCatch(
      tdabm_create_output_directories(output_paths, verbose = FALSE),
      error = function(e) {
        created_ok <<- FALSE
        created_message <<- paste0("Could not create output directories: ", conditionMessage(e))
      }
    )

    validation <- tdabm_validation_add(
      validation,
      "outputs",
      "create_output_directories",
      created_ok,
      created_message
    )
  }

  if (isTRUE(check_files) && is.list(output_paths)) {
    for (nm in names(output_paths)) {
      path <- output_paths[[nm]]

      if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
        validation <- tdabm_validation_add(
          validation,
          "outputs",
          paste0(nm, "_path_valid"),
          FALSE,
          paste0("Output path is not a valid character scalar: ", nm)
        )
      } else {
        validation <- tdabm_validation_add(
          validation,
          paste0("outputs"),
          paste0(nm, "_directory_exists"),
          dir.exists(path),
          paste0("Output directory should exist: ", path),
          severity = if (isTRUE(create_dirs)) "ERROR" else "WARNING"
        )
      }
    }
  }

  validation
}

tdabm_validate_dependencies <- function(
  project,
  validation,
  check_files = TRUE,
  run_dependency_smoke_test = FALSE
) {
  deps <- project$dependencies
  expected <- c("function_file", "bmcpp_path", "coref_path")

  for (nm in expected) {
    validation <- tdabm_validation_add(
      validation,
      "dependencies",
      paste0(nm, "_declared"),
      nm %in% names(deps) && tdabm_is_scalar_character(deps[[nm]]),
      paste0("Dependency path should be declared: ", nm),
      severity = "WARNING"
    )
  }

  if (isTRUE(check_files)) {
    for (nm in intersect(expected, names(deps))) {
      path <- deps[[nm]]

      if (!tdabm_is_scalar_character(path)) {
        validation <- tdabm_validation_add(
          validation,
          "dependencies",
          nm,
          FALSE,
          paste0("Dependency path is invalid: ", nm)
        )
        next
      }

      validation <- tdabm_validation_add(
        validation,
        "dependencies",
        nm,
        file.exists(path),
        paste0("Dependency file must exist: ", path)
      )
    }
  }

  if (isTRUE(run_dependency_smoke_test)) {
    for (nm in c("function_file", "coref_path")) {
      path <- deps[[nm]]

      if (tdabm_is_scalar_character(path) && file.exists(path)) {
        parse_ok <- TRUE
        parse_message <- paste0("R dependency parsed successfully: ", path)

        tryCatch(
          parse(file = path),
          error = function(e) {
            parse_ok <<- FALSE
            parse_message <<- paste0("Could not parse R dependency: ", conditionMessage(e))
          }
        )

        validation <- tdabm_validation_add(
          validation,
          "dependencies",
          paste0(nm, "_parse"),
          parse_ok,
          parse_message
        )
      }
    }
  }

  validation
}

tdabm_validate_radius <- function(project, validation) {
  radius <- project$radius

  validation <- tdabm_validation_add(
    validation,
    "radius",
    "grid_present",
    is.numeric(radius$grid) && length(radius$grid) >= 1L,
    "Radius grid must contain at least one numeric radius."
  )

  validation <- tdabm_validation_add(
    validation,
    "radius",
    "x001_consistent",
    is.numeric(radius$x001) && radius$x001 == length(radius$grid),
    "radius$x001 must equal length(radius$grid)."
  )

  validation <- tdabm_validation_add(
    validation,
    "radius",
    "step_positive",
    is.numeric(radius$step) && length(radius$step) == 1L && is.finite(radius$step) && radius$step > 0,
    "Radius step must be positive."
  )

  validation <- tdabm_validation_add(
    validation,
    "radius",
    "monotone",
    length(radius$grid) == 1L || all(diff(radius$grid) > 0),
    "Radius grid must be strictly increasing."
  )

  validation
}

tdabm_validate_parallel <- function(project, validation) {
  par <- project$parallel

  validation <- tdabm_validation_add(
    validation,
    "parallel",
    "ncores",
    is.numeric(par$ncores) && length(par$ncores) == 1L && is.finite(par$ncores) && par$ncores >= 1L,
    "parallel$ncores must be a positive integer."
  )

  available <- tryCatch(
    parallel::detectCores(logical = FALSE),
    error = function(e) NA_integer_
  )

  if (is.finite(available) && !is.na(available)) {
    validation <- tdabm_validation_add(
      validation,
      "parallel",
      "ncores_available",
      par$ncores <= available,
      paste0(
        "Requested ncores (", par$ncores,
        ") exceeds detected physical cores (", available, ")."
      ),
      severity = "WARNING"
    )
  }

  validation <- tdabm_validation_add(
    validation,
    "parallel",
    "repetitions",
    is.numeric(par$repetitions) && length(par$repetitions) == 1L && is.finite(par$repetitions) && par$repetitions >= 1L,
    "parallel$repetitions must be a positive integer."
  )

  validation <- tdabm_validation_add(
    validation,
    "parallel",
    "base_seed",
    is.numeric(par$base_seed) && length(par$base_seed) == 1L && is.finite(par$base_seed),
    "parallel$base_seed must be a finite integer."
  )

  validation <- tdabm_validation_add(
    validation,
    "parallel",
    "checkpoint_every",
    is.numeric(par$checkpoint_every) && length(par$checkpoint_every) == 1L && is.finite(par$checkpoint_every) && par$checkpoint_every >= 1L,
    "parallel$checkpoint_every must be a positive integer."
  )

  validation
}

tdabm_validate_spec_tables <- function(project, validation) {
  colourings <- project$colourings

  validation <- tdabm_validation_add(
    validation,
    "colourings",
    "table",
    is.data.frame(colourings),
    "project$colourings must be a data frame."
  )

  if (is.data.frame(colourings)) {
    required_colour_cols <- c(
      "run_id", "variable", "label", "short_label", "enabled",
      "colour_type", "family", "priority", "run_primary", "run_robustness"
    )

    for (nm in required_colour_cols) {
      validation <- tdabm_validation_add(
        validation,
        "colourings",
        paste0("column_", nm),
        nm %in% names(colourings),
        paste0("Colourings table must contain column: ", nm)
      )
    }

    if ("run_id" %in% names(colourings)) {
      validation <- tdabm_validation_add(
        validation,
        "colourings",
        "run_id_unique",
        !anyDuplicated(colourings$run_id),
        "Colouring run IDs must be unique."
      )

      validation <- tdabm_validation_add(
        validation,
        "colourings",
        "run_id_nonmissing",
        all(!is.na(colourings$run_id) & nzchar(colourings$run_id)),
        "Colouring run IDs must be non-missing."
      )
    }
  }

  robustness <- project$robustness

  validation <- tdabm_validation_add(
    validation,
    "robustness",
    "table",
    is.data.frame(robustness),
    "project$robustness must be a data frame."
  )

  if (is.data.frame(robustness)) {
    required_robust_cols <- c(
      "robustness_id", "type", "variable", "exclude",
      "method", "enabled", "selection_mode", "require_all_matches",
      "minimum_removed", "rationale", "exclude_values", "analysis_run_ids"
    )

    for (nm in required_robust_cols) {
      validation <- tdabm_validation_add(
        validation,
        "robustness",
        paste0("column_", nm),
        nm %in% names(robustness),
        paste0("Robustness table must contain column: ", nm)
      )
    }

    if ("robustness_id" %in% names(robustness)) {
      validation <- tdabm_validation_add(
        validation,
        "robustness",
        "robustness_id_unique",
        !anyDuplicated(robustness$robustness_id),
        "Robustness IDs must be unique."
      )
    }

    allowed_types <- c("original", "filter", "exclude", "manual_exclusion", "scaling")
    if ("type" %in% names(robustness)) {
      validation <- tdabm_validation_add(
        validation,
        "robustness",
        "allowed_types",
        all(robustness$type %in% allowed_types),
        paste0("Robustness types must be one of: ", paste(allowed_types, collapse = ", "))
      )
    }
  }

  validation
}

tdabm_validate_loaded_data <- function(project, validation) {
  data_result <- tryCatch(
    tdabm_project_get_dataset(project),
    error = function(e) e
  )

  if (inherits(data_result, "error")) {
    validation <- tdabm_validation_add(
      validation,
      "data",
      "load",
      FALSE,
      paste0("Could not load project data: ", conditionMessage(data_result))
    )
    return(list(validation = validation, data_result = NULL))
  }

  df <- as.data.frame(data_result$data)
  axes <- data_result$axes

  validation <- tdabm_validation_add(
    validation,
    "data",
    "load",
    TRUE,
    "Project data loaded successfully."
  )

  validation <- tdabm_validation_add(
    validation,
    "data",
    "nrow",
    nrow(df) > 0L,
    "Dataset must contain at least one row."
  )

  validation <- tdabm_validation_add(
    validation,
    "data",
    "ncol",
    ncol(df) > 0L,
    "Dataset must contain at least one column."
  )

  id_col <- project$data$id_col

  validation <- tdabm_validation_add(
    validation,
    "identifiers",
    "id_col_exists",
    id_col %in% names(df),
    paste0("ID column must exist: ", id_col)
  )

  if (id_col %in% names(df)) {
    validation <- tdabm_validation_add(
      validation,
      "identifiers",
      "id_col_nonmissing",
      !any(is.na(df[[id_col]])),
      "ID values must not be missing."
    )

    validation <- tdabm_validation_add(
      validation,
      "identifiers",
      "id_col_unique",
      !anyDuplicated(df[[id_col]]),
      "ID values must be unique."
    )
  }

  optional_id_cols <- c(project$data$label_col, project$data$region_col)
  optional_id_cols <- optional_id_cols[!is.na(optional_id_cols) & nzchar(optional_id_cols)]

  for (col in optional_id_cols) {
    validation <- tdabm_validation_add(
      validation,
      "identifiers",
      paste0(col, "_exists"),
      col %in% names(df),
      paste0("Identifier column should exist: ", col)
    )
  }

  validation <- tdabm_validation_add(
    validation,
    "topology",
    "axis_count",
    length(axes) >= 1L,
    "At least one topology axis must be supplied or recovered from the input object."
  )

  for (axis in axes) {
    exists_axis <- axis %in% names(df)

    validation <- tdabm_validation_add(
      validation,
      "topology",
      paste0(axis, "_exists"),
      exists_axis,
      paste0("Topology axis must exist: ", axis)
    )

    if (exists_axis) {
      x <- df[[axis]]

      validation <- tdabm_validation_add(
        validation,
        "topology",
        paste0(axis, "_numeric"),
        is.numeric(x),
        paste0("Topology axis must be numeric: ", axis)
      )

      if (is.numeric(x)) {
        validation <- tdabm_validation_add(
          validation,
          "topology",
          paste0(axis, "_finite"),
          all(is.finite(x)),
          paste0("Topology axis must contain finite values only: ", axis)
        )

        validation <- tdabm_validation_add(
          validation,
          "topology",
          paste0(axis, "_variation"),
          stats::sd(x) > 0,
          paste0("Topology axis must have positive variation: ", axis)
        )
      }
    }
  }

  colourings <- project$colourings

  if (is.data.frame(colourings) && nrow(colourings) > 0L) {
    for (i in seq_len(nrow(colourings))) {
      spec <- colourings[i, , drop = FALSE]

      if (!isTRUE(spec$enabled)) next

      if (spec$colour_type %in% c("constant", "computed")) {
        validation <- tdabm_validation_add(
          validation,
          "colourings",
          paste0(spec$run_id, "_", spec$colour_type),
          TRUE,
          paste0("Colouring does not require a data column: ", spec$run_id)
        )
        next
      }

      var <- spec$variable
      exists_var <- !is.na(var) && nzchar(var) && var %in% names(df)

      validation <- tdabm_validation_add(
        validation,
        "colourings",
        paste0(spec$run_id, "_exists"),
        exists_var,
        paste0("Colour variable must exist for ", spec$run_id, ": ", var)
      )

      if (exists_var) {
        y <- df[[var]]

        validation <- tdabm_validation_add(
          validation,
          "colourings",
          paste0(spec$run_id, "_numeric"),
          is.numeric(y),
          paste0("Colour variable must be numeric for ", spec$run_id, ": ", var)
        )

        if (is.numeric(y)) {
          validation <- tdabm_validation_add(
            validation,
            "colourings",
            paste0(spec$run_id, "_finite"),
            all(is.finite(y)),
            paste0("Colour variable must be finite for ", spec$run_id, ": ", var)
          )
        }
      }
    }
  }

  membership <- project$membership

  if (isTRUE(membership$enabled)) {
    validation <- tdabm_validation_add(
      validation,
      "membership",
      "pairwise_mode",
      membership$pairwise_mode %in% c("none", "focal", "all"),
      "membership$pairwise_mode must be one of: none, focal, all."
    )

    if (length(membership$focal_labels) > 0L) {
      label_col <- membership$focal_label_col %||% project$data$label_col

      validation <- tdabm_validation_add(
        validation,
        "membership",
        "focal_label_col_exists",
        label_col %in% names(df),
        paste0("Focal label column must exist: ", label_col)
      )

      if (label_col %in% names(df)) {
        missing_labels <- setdiff(membership$focal_labels, df[[label_col]])

        validation <- tdabm_validation_add(
          validation,
          "membership",
          "focal_labels_present",
          length(missing_labels) == 0L,
          paste0(
            "All focal labels must be present. Missing: ",
            paste(missing_labels, collapse = ", ")
          )
        )
      }
    }
  }

  robustness <- project$robustness

  if (is.data.frame(robustness) && nrow(robustness) > 0L) {
    enabled_robustness <- robustness[robustness$enabled %in% TRUE, , drop = FALSE]

    for (i in seq_len(nrow(enabled_robustness))) {
      spec <- enabled_robustness[i, , drop = FALSE]

      if (spec$type %in% c("filter", "exclude", "manual_exclusion")) {
        var <- as.character(spec$variable[[1L]])
        exists_var <- !is.na(var) && nzchar(var) && var %in% names(df)
        values <- if ("exclude_values" %in% names(spec)) {
          unique(as.character(unlist(spec$exclude_values[[1L]], use.names = FALSE)))
        } else {
          unique(as.character(spec$exclude[!is.na(spec$exclude)]))
        }
        values <- values[nzchar(values)]
        require_all <- if ("require_all_matches" %in% names(spec)) {
          isTRUE(spec$require_all_matches[[1L]])
        } else {
          TRUE
        }
        minimum_removed <- if ("minimum_removed" %in% names(spec)) {
          as.integer(spec$minimum_removed[[1L]])
        } else {
          1L
        }

        validation <- tdabm_validation_add(
          validation,
          "robustness",
          paste0(spec$robustness_id, "_filter_variable_exists"),
          exists_var,
          paste0("Robustness exclusion variable must exist: ", var)
        )

        validation <- tdabm_validation_add(
          validation,
          "robustness",
          paste0(spec$robustness_id, "_selection_mode"),
          identical(as.character(spec$selection_mode[[1L]]), "user_specified"),
          "Exclusion robustness specifications must be explicitly user-specified."
        )

        validation <- tdabm_validation_add(
          validation,
          "robustness",
          paste0(spec$robustness_id, "_values_supplied"),
          length(values) > 0L,
          "At least one explicit exclusion value must be supplied by the user."
        )

        validation <- tdabm_validation_add(
          validation,
          "robustness",
          paste0(spec$robustness_id, "_minimum_removed"),
          is.finite(minimum_removed) && minimum_removed >= 1L,
          "minimum_removed must be at least one."
        )

        if (exists_var && length(values) > 0L) {
          present <- values %in% as.character(unique(df[[var]]))
          match_ok <- if (require_all) all(present) else any(present)
          removal_count <- sum(as.character(df[[var]]) %in% values)

          validation <- tdabm_validation_add(
            validation,
            "robustness",
            paste0(spec$robustness_id, "_exclude_values_present"),
            match_ok,
            paste0(
              "Explicit exclusion values did not satisfy the matching contract in ",
              var,
              ". Missing: ",
              paste(values[!present], collapse = ", ")
            )
          )

          validation <- tdabm_validation_add(
            validation,
            "robustness",
            paste0(spec$robustness_id, "_removal_count"),
            removal_count >= minimum_removed && removal_count < nrow(df),
            paste0(
              "Exclusion must remove at least ", minimum_removed,
              " observation(s) and leave at least one observation. Matched: ",
              removal_count
            )
          )
        }
      }

      if (spec$type == "scaling") {
        allowed_methods <- c("mad", "rank", "z_score", "standard", "as_supplied")
        validation <- tdabm_validation_add(
          validation,
          "robustness",
          paste0(spec$robustness_id, "_scaling_method"),
          spec$method %in% allowed_methods,
          paste0("Scaling method should be one of: ", paste(allowed_methods, collapse = ", "))
        )
      }

      if ("analysis_run_ids" %in% names(spec)) {
        requested <- unlist(spec$analysis_run_ids, use.names = FALSE)
        if (length(requested) > 0L) {
          missing_runs <- setdiff(requested, project$colourings$run_id)

          validation <- tdabm_validation_add(
            validation,
            "robustness",
            paste0(spec$robustness_id, "_analysis_run_ids"),
            length(missing_runs) == 0L,
            paste0(
              "Robustness specification references unknown analysis run IDs: ",
              paste(missing_runs, collapse = ", ")
            )
          )
        }
      }
    }
  }

  list(validation = validation, data_result = data_result)
}

validate_tdabm_project <- function(
  project,
  check_files = TRUE,
  check_data = TRUE,
  create_dirs = FALSE,
  run_dependency_smoke_test = FALSE,
  attach_to_project = FALSE
) {
  project_id <- tryCatch(
    project$metadata$project_id,
    error = function(e) NA_character_
  )

  validation <- tdabm_validation_new(project_id = project_id)

  validation <- tdabm_validate_project_class(project, validation)

  if (!is.TDABMProject(project)) {
    validation <- tdabm_validation_finalize(validation)
    return(validation)
  }

  validation <- tdabm_validate_required_sections(project, validation)
  validation <- tdabm_validate_metadata(project, validation)
  validation <- tdabm_validate_paths(
    project,
    validation,
    check_files = check_files,
    create_dirs = create_dirs
  )
  validation <- tdabm_validate_dependencies(
    project,
    validation,
    check_files = check_files,
    run_dependency_smoke_test = run_dependency_smoke_test
  )
  validation <- tdabm_validate_radius(project, validation)
  validation <- tdabm_validate_parallel(project, validation)
  validation <- tdabm_validate_spec_tables(project, validation)

  if (isTRUE(check_data)) {
    data_validation <- tdabm_validate_loaded_data(project, validation)
    validation <- data_validation$validation
  }

  validation <- tdabm_validation_finalize(validation)

  if (isTRUE(attach_to_project)) {
    project$validation <- validation
    project <- tdabm_append_history(
      project,
      action = "validated",
      detail = if (isTRUE(validation$ok)) "Validation passed." else "Validation failed."
    )
    return(project)
  }

  validation
}
