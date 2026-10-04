# ==============================================================================
# TDABMProject.R
# ==============================================================================
# S3 project object for reusable TDABM workflows.
#
# Stage 1 purpose:
#   - construct a project object;
#   - print and summarise it;
#   - perform structural and optional data-level validation;
#   - provide a consistent contract for later execution modules.
#
# Required source order:
#   source("R/TDABMFramework.R")
#   source("R/TDABMUtilities.R")
#   source("R/TDABMProject.R")
# ==============================================================================

if (!exists("tdabm_create_radius_grid", mode = "function")) {
  stop("Source TDABMUtilities.R before TDABMProject.R.", call. = FALSE)
}

is.TDABMProject <- function(x) {
  inherits(x, "TDABMProject")
}

TDABMProject <- function(
  metadata = list(),
  paths = list(),
  data = list(),
  topology = list(),
  colourings = list(),
  robustness = list(),
  radius = list(),
  parallel = list(),
  outputs = list(),
  membership = list(),
  paper = list(),
  dependencies = list(),
  validate = TRUE,
  create_output_dirs = FALSE
) {
  defaults <- if (exists("TDABMDefaultSettings", mode = "function")) {
    TDABMDefaultSettings()
  } else {
    list(
      radius = list(min = 0.50, max = 1.50, step = 0.01),
      parallel = list(
        ncores = 1L,
        repetitions = 1000L,
        base_seed = 12345L,
        thresh2 = 4L,
        checkpoint_every = 500L
      )
    )
  }

  default_metadata <- list(
    project_id = "tdabm_project",
    project_name = "TDABM project",
    version = "0.1.0",
    author = Sys.info()[["user"]],
    created_at = as.character(Sys.time()),
    framework_version = if (exists("TDABMFrameworkVersion", mode = "function")) {
      TDABMFrameworkVersion()
    } else {
      NA_character_
    }
  )

  metadata <- tdabm_modify_list(default_metadata, metadata)

  default_paths <- list(
    project_root = getwd(),
    framework_root = NA_character_
  )

  paths <- tdabm_modify_list(default_paths, paths)
  paths$project_root <- normalizePath(paths$project_root, winslash = "/", mustWork = FALSE)

  default_data <- list(
    input_rds = NA_character_,
    dataset_name = NA_character_,
    data_format = "verification_rds",
    id_col = "id",
    label_col = NA_character_,
    region_col = NA_character_
  )

  data <- tdabm_modify_list(default_data, data)

  default_topology <- list(
    axes = character(0),
    scaling = "as_supplied",
    description = NA_character_
  )

  topology <- tdabm_modify_list(default_topology, topology)
  topology$axes <- as.character(topology$axes %||% character(0))

  default_radius <- defaults$radius
  radius <- tdabm_modify_list(default_radius, radius)
  radius_grid <- tdabm_create_radius_grid(
    min = radius$min,
    max = radius$max,
    step = radius$step
  )

  default_parallel <- defaults$parallel
  detected_cores <- tryCatch(
    parallel::detectCores(logical = FALSE),
    error = function(e) NA_integer_
  )
  if (is.finite(detected_cores) && !is.na(detected_cores)) {
    default_parallel$ncores <- max(1L, as.integer(detected_cores))
  }

  parallel <- tdabm_modify_list(default_parallel, parallel)
  parallel$ncores <- as.integer(parallel$ncores)
  parallel$repetitions <- as.integer(parallel$repetitions)
  parallel$base_seed <- as.integer(parallel$base_seed)
  parallel$thresh2 <- as.integer(parallel$thresh2)
  parallel$checkpoint_every <- as.integer(parallel$checkpoint_every)

  default_outputs <- list(
    root = file.path(paths$project_root, "outputs"),
    subdirs = NULL
  )
  outputs <- tdabm_modify_list(default_outputs, outputs)
  output_paths <- tdabm_make_output_paths(outputs$root, outputs$subdirs)
  outputs$paths <- output_paths

  default_membership <- list(
    enabled = FALSE,
    run_id = "P01_membership",
    pairwise_mode = "focal",
    focal_ids = character(0),
    focal_labels = character(0),
    focal_label_col = data$label_col
  )
  membership <- tdabm_modify_list(default_membership, membership)
  membership$enabled <- as.logical(membership$enabled)
  membership$focal_ids <- as.character(membership$focal_ids %||% character(0))
  membership$focal_labels <- as.character(membership$focal_labels %||% character(0))

  default_paper <- list(
    radius_contract = NULL,
    canonical_ballmapper_rds = NA_character_,
    require_frozen_ballmapper = TRUE
  )
  paper <- tdabm_modify_list(default_paper, paper)
  paper$require_frozen_ballmapper <- as.logical(paper$require_frozen_ballmapper)

  default_dependencies <- list(
    function_file = NA_character_,
    bmcpp_path = NA_character_,
    coref_path = NA_character_
  )
  dependencies <- tdabm_modify_list(default_dependencies, dependencies)

  colourings <- tdabm_normalise_colourings(colourings)
  robustness <- tdabm_normalise_robustness_specs(robustness)

  project <- structure(
    list(
      metadata = metadata,
      paths = paths,
      data = data,
      topology = topology,
      colourings = colourings,
      robustness = robustness,
      radius = radius_grid,
      parallel = parallel,
      outputs = outputs,
      membership = membership,
      paper = paper,
      dependencies = dependencies,
      history = tdabm_history_event(
        action = "created",
        detail = paste0("Project object created: ", metadata$project_id)
      ),
      validation = NULL
    ),
    class = c("TDABMProject", "list")
  )

  if (isTRUE(create_output_dirs)) {
    tdabm_create_output_directories(project$outputs$paths, verbose = TRUE)
    project <- tdabm_append_history(project, "created_output_directories", project$outputs$paths$root)
  }

  if (isTRUE(validate)) {
    project$validation <- validate_tdabm_project(
      project,
      check_files = FALSE,
      check_data = FALSE,
      create_dirs = FALSE
    )
  }

  project
}

print.TDABMProject <- function(x, ...) {
  cat("\n")
  cat("============================================================\n")
  cat("TDABM Project\n")
  cat("============================================================\n")
  cat("Project:       ", x$metadata$project_name, "\n", sep = "")
  cat("Project ID:    ", x$metadata$project_id, "\n", sep = "")
  cat("Version:       ", x$metadata$version, "\n", sep = "")
  cat("Framework:     ", x$metadata$framework_version, "\n", sep = "")
  cat("Project root:  ", x$paths$project_root, "\n", sep = "")
  cat("Output root:   ", x$outputs$paths$root, "\n", sep = "")
  cat("\n")

  cat("Data\n")
  cat("  format:      ", x$data$data_format, "\n", sep = "")
  cat("  dataset:     ", x$data$dataset_name, "\n", sep = "")
  cat("  id column:   ", x$data$id_col, "\n", sep = "")
  cat("  label column:", x$data$label_col, "\n", sep = "")
  cat("\n")

  cat("Topology\n")
  cat("  axes:        ", paste(x$topology$axes, collapse = ", "), "\n", sep = "")
  cat("  scaling:     ", x$topology$scaling, "\n", sep = "")
  cat("\n")

  enabled_colourings <- if (is.null(x$colourings)) {
    0L
  } else {
    sum(isTRUE(x$colourings$enabled) | x$colourings$enabled %in% TRUE, na.rm = TRUE)
  }

  cat("Analyses\n")
  cat("  colourings:  ", nrow(x$colourings), " total; ", enabled_colourings, " enabled\n", sep = "")
  cat("  robustness:  ", nrow(x$robustness), " specifications\n", sep = "")
  cat("  membership:  ", if (isTRUE(x$membership$enabled)) "enabled" else "disabled", "\n", sep = "")
  cat("\n")

  cat("Radius grid\n")
  cat("  min/max/step:", x$radius$min, " / ", x$radius$max, " / ", x$radius$step, "\n", sep = "")
  cat("  x001:        ", x$radius$x001, "\n", sep = "")
  cat("\n")

  cat("Parallel\n")
  cat("  cores:       ", x$parallel$ncores, "\n", sep = "")
  cat("  repetitions: ", x$parallel$repetitions, "\n", sep = "")
  cat("  base seed:   ", x$parallel$base_seed, "\n", sep = "")
  cat("\n")

  if (!is.null(x$validation)) {
    cat("Validation\n")
    cat("  status:      ", if (isTRUE(x$validation$ok)) "PASS" else "FAIL", "\n", sep = "")
    cat("  errors:      ", length(x$validation$errors), "\n", sep = "")
    cat("  warnings:    ", length(x$validation$warnings), "\n", sep = "")
  } else {
    cat("Validation\n")
    cat("  status:      not run\n")
  }

  cat("============================================================\n")
  invisible(x)
}

summary.TDABMProject <- function(object, ...) {
  out <- list(
    metadata = object$metadata,
    project_root = object$paths$project_root,
    output_root = object$outputs$paths$root,
    data = object$data,
    axes = object$topology$axes,
    colourings = object$colourings,
    robustness = object$robustness,
    radius = object$radius,
    parallel = object$parallel,
    membership = object$membership,
    paper = object$paper,
    validation = object$validation
  )

  class(out) <- "summary.TDABMProject"
  out
}

print.summary.TDABMProject <- function(x, ...) {
  cat("TDABM project summary\n")
  cat("Project: ", x$metadata$project_name, "\n", sep = "")
  cat("Axes: ", paste(x$axes, collapse = ", "), "\n", sep = "")
  cat("Colourings: ", nrow(x$colourings), "\n", sep = "")
  cat("Robustness specifications: ", nrow(x$robustness), "\n", sep = "")
  cat("Radius grid x001: ", x$radius$x001, "\n", sep = "")
  invisible(x)
}

tdabm_project_status <- function(project) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
  }

  if (is.null(project$validation)) {
    return("not_validated")
  }

  if (isTRUE(project$validation$ok)) "valid" else "invalid"
}

tdabm_project_colourings <- function(project, enabled_only = TRUE) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
  }

  colourings <- project$colourings
  if (isTRUE(enabled_only) && nrow(colourings) > 0L) {
    colourings <- colourings[colourings$enabled %in% TRUE, , drop = FALSE]
  }

  colourings
}

tdabm_project_output_path <- function(project, name) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
  }

  if (!name %in% names(project$outputs$paths)) {
    stop("Unknown output path name: ", name, call. = FALSE)
  }

  project$outputs$paths[[name]]
}

tdabm_project_get_dataset <- function(project) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
  }

  if (identical(project$data$data_format, "data_frame")) {
    if (is.null(project$data$data_frame)) {
      stop("data_format is 'data_frame' but project$data$data_frame is missing.", call. = FALSE)
    }
    data <- as.data.frame(project$data$data_frame)
    return(list(data = data, axes = project$topology$axes, source = "data_frame"))
  }

  input_rds <- project$data$input_rds

  if (is.null(input_rds) || length(input_rds) != 1L || is.na(input_rds) || !file.exists(input_rds)) {
    stop("Input RDS not found: ", input_rds, call. = FALSE)
  }

  obj <- readRDS(input_rds)

  if (identical(project$data$data_format, "verification_rds")) {
    dataset_name <- project$data$dataset_name

    if (!dataset_name %in% names(obj)) {
      stop("Dataset '", dataset_name, "' not found in input RDS.", call. = FALSE)
    }

    dataset_obj <- obj[[dataset_name]]

    if (is.null(dataset_obj$data)) {
      stop("Dataset object does not contain a $data component.", call. = FALSE)
    }

    data <- as.data.frame(dataset_obj$data)

    axes <- project$topology$axes
    if (length(axes) == 0L && !is.null(dataset_obj$axis_columns)) {
      axes <- as.character(dataset_obj$axis_columns)
    }

    return(list(data = data, axes = axes, source = dataset_obj, input_object = obj))
  }

  if (is.data.frame(obj)) {
    return(list(data = as.data.frame(obj), axes = project$topology$axes, source = obj))
  }

  stop(
    "Unsupported data_format: ",
    project$data$data_format,
    ". Use 'verification_rds', 'data_frame', or an RDS containing a data frame.",
    call. = FALSE
  )
}

tdabm_new_validation <- function() {
  checks <- data.frame(
    section = character(0),
    item = character(0),
    level = character(0),
    ok = logical(0),
    message = character(0),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      ok = TRUE,
      checks = checks,
      errors = character(0),
      warnings = character(0)
    ),
    class = "TDABMProjectValidation"
  )
}

tdabm_add_check <- function(validation, section, item, ok, message, level = "ERROR") {
  ok <- isTRUE(ok)
  level <- toupper(level)

  validation$checks <- rbind(
    validation$checks,
    data.frame(
      section = as.character(section),
      item = as.character(item),
      level = level,
      ok = ok,
      message = as.character(message),
      stringsAsFactors = FALSE
    )
  )

  if (!ok && identical(level, "ERROR")) {
    validation$ok <- FALSE
    validation$errors <- c(validation$errors, message)
  }

  if (!ok && identical(level, "WARNING")) {
    validation$warnings <- c(validation$warnings, message)
  }

  validation
}

validate_tdabm_project <- function(
  project,
  check_files = TRUE,
  check_data = FALSE,
  create_dirs = FALSE
) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
  }

  v <- tdabm_new_validation()

  v <- tdabm_add_check(
    v,
    "metadata",
    "project_id",
    tdabm_is_scalar_character(project$metadata$project_id),
    "Project ID must be a non-empty character scalar."
  )

  v <- tdabm_add_check(
    v,
    "paths",
    "project_root",
    tdabm_is_scalar_character(project$paths$project_root),
    "Project root must be a non-empty character scalar."
  )

  v <- tdabm_add_check(
    v,
    "data",
    "dataset_name",
    tdabm_is_scalar_character(project$data$dataset_name),
    "Dataset name must be a non-empty character scalar."
  )

  v <- tdabm_add_check(
    v,
    "data",
    "id_col",
    tdabm_is_scalar_character(project$data$id_col),
    "ID column must be a non-empty character scalar."
  )

  v <- tdabm_add_check(
    v,
    "topology",
    "axes",
    is.character(project$topology$axes),
    "Topology axes must be a character vector."
  )

  v <- tdabm_add_check(
    v,
    "radius",
    "x001",
    is.numeric(project$radius$x001) && project$radius$x001 >= 1L,
    "Radius grid must contain at least one radius."
  )

  v <- tdabm_add_check(
    v,
    "parallel",
    "ncores",
    is.numeric(project$parallel$ncores) && is.finite(project$parallel$ncores) && project$parallel$ncores >= 1L,
    "Number of cores must be a positive integer."
  )

  v <- tdabm_add_check(
    v,
    "parallel",
    "repetitions",
    is.numeric(project$parallel$repetitions) && is.finite(project$parallel$repetitions) && project$parallel$repetitions >= 1L,
    "Number of repetitions must be a positive integer."
  )

  if (!is.null(project$colourings) && nrow(project$colourings) > 0L) {
    v <- tdabm_add_check(
      v,
      "colourings",
      "run_id_unique",
      !anyDuplicated(project$colourings$run_id),
      "Colouring run IDs must be unique."
    )

    v <- tdabm_add_check(
      v,
      "colourings",
      "run_id_present",
      all(!is.na(project$colourings$run_id) & nzchar(project$colourings$run_id)),
      "Every colouring must have a non-empty run_id."
    )
  }

  if (!is.null(project$robustness) && nrow(project$robustness) > 0L) {
    v <- tdabm_add_check(
      v,
      "robustness",
      "robustness_id_unique",
      !anyDuplicated(project$robustness$robustness_id),
      "Robustness IDs must be unique."
    )
  }

  if (isTRUE(check_files)) {
    v <- tdabm_add_check(
      v,
      "paths",
      "project_root_exists",
      dir.exists(project$paths$project_root),
      paste0("Project root must exist: ", project$paths$project_root)
    )

    if (!is.na(project$data$input_rds)) {
      v <- tdabm_add_check(
        v,
        "data",
        "input_rds_exists",
        file.exists(project$data$input_rds),
        paste0("Input RDS must exist: ", project$data$input_rds)
      )
    }

    dependency_paths <- unlist(project$dependencies, use.names = TRUE)
    dependency_paths <- dependency_paths[!is.na(dependency_paths) & nzchar(dependency_paths)]

    if (length(dependency_paths) > 0L) {
      for (nm in names(dependency_paths)) {
        v <- tdabm_add_check(
          v,
          "dependencies",
          nm,
          file.exists(dependency_paths[[nm]]),
          paste0("Dependency file must exist: ", dependency_paths[[nm]])
        )
      }
    }
  }

  if (isTRUE(create_dirs)) {
    tryCatch(
      {
        tdabm_create_output_directories(project$outputs$paths, verbose = FALSE)
        v <- tdabm_add_check(
          v,
          "outputs",
          "create_directories",
          TRUE,
          "Output directories are available."
        )
      },
      error = function(e) {
        v <<- tdabm_add_check(
          v,
          "outputs",
          "create_directories",
          FALSE,
          paste0("Could not create output directories: ", conditionMessage(e))
        )
      }
    )
  }

  if (isTRUE(check_data)) {
    data_result <- tryCatch(
      tdabm_project_get_dataset(project),
      error = function(e) e
    )

    if (inherits(data_result, "error")) {
      v <- tdabm_add_check(
        v,
        "data",
        "load_dataset",
        FALSE,
        paste0("Could not load project dataset: ", conditionMessage(data_result))
      )
    } else {
      df <- data_result$data
      axes <- data_result$axes

      v <- tdabm_add_check(
        v,
        "data",
        "nrow",
        nrow(df) > 0L,
        "Dataset must contain at least one row."
      )

      id_col <- project$data$id_col
      v <- tdabm_add_check(
        v,
        "identifiers",
        id_col,
        id_col %in% names(df),
        paste0("ID column must exist: ", id_col)
      )

      if (id_col %in% names(df)) {
        v <- tdabm_add_check(
          v,
          "identifiers",
          "unique_ids",
          !anyDuplicated(df[[id_col]]) && !any(is.na(df[[id_col]])),
          "Observation IDs must be unique and non-missing."
        )
      }

      optional_id_cols <- c(project$data$label_col, project$data$region_col)
      optional_id_cols <- optional_id_cols[!is.na(optional_id_cols) & nzchar(optional_id_cols)]
      for (col in optional_id_cols) {
        v <- tdabm_add_check(
          v,
          "identifiers",
          col,
          col %in% names(df),
          paste0("Identifier column must exist: ", col)
        )
      }

      v <- tdabm_add_check(
        v,
        "topology",
        "axis_count",
        length(axes) >= 1L,
        "At least one topology axis must be supplied or recoverable from the input object."
      )

      for (axis in axes) {
        exists_axis <- axis %in% names(df)
        v <- tdabm_add_check(
          v,
          "topology",
          axis,
          exists_axis,
          paste0("Topology axis must exist: ", axis)
        )

        if (exists_axis) {
          value <- df[[axis]]
          v <- tdabm_add_check(
            v,
            "topology",
            paste0(axis, "_numeric"),
            is.numeric(value),
            paste0("Topology axis must be numeric: ", axis)
          )

          if (is.numeric(value)) {
            v <- tdabm_add_check(
              v,
              "topology",
              paste0(axis, "_usable"),
              !anyNA(value) && stats::sd(value) > 0,
              paste0("Topology axis must be non-missing and have positive variation: ", axis)
            )
          }
        }
      }

      if (!is.null(project$colourings) && nrow(project$colourings) > 0L) {
        for (i in seq_len(nrow(project$colourings))) {
          row <- project$colourings[i, , drop = FALSE]
          if (!isTRUE(row$enabled)) next
          if (row$colour_type %in% c("constant", "computed")) next

          var <- row$variable
          exists_var <- !is.na(var) && nzchar(var) && var %in% names(df)

          v <- tdabm_add_check(
            v,
            "colourings",
            row$run_id,
            exists_var,
            paste0("Colour variable must exist for ", row$run_id, ": ", var)
          )

          if (exists_var) {
            value <- df[[var]]
            v <- tdabm_add_check(
              v,
              "colourings",
              paste0(row$run_id, "_numeric"),
              is.numeric(value),
              paste0("Colour variable must be numeric for ", row$run_id, ": ", var)
            )

            if (is.numeric(value)) {
              v <- tdabm_add_check(
                v,
                "colourings",
                paste0(row$run_id, "_complete"),
                !anyNA(value),
                paste0("Colour variable must not contain missing values for ", row$run_id, ": ", var)
              )
            }
          }
        }
      }

      if (!is.null(project$robustness) && nrow(project$robustness) > 0L) {
        filter_specs <- project$robustness[
          project$robustness$type %in% c("filter", "exclude", "manual_exclusion") &
            project$robustness$enabled %in% TRUE,
          ,
          drop = FALSE
        ]

        if (nrow(filter_specs) > 0L) {
          for (i in seq_len(nrow(filter_specs))) {
            var <- filter_specs$variable[[i]]
            v <- tdabm_add_check(
              v,
              "robustness",
              filter_specs$robustness_id[[i]],
              !is.na(var) && nzchar(var) && var %in% names(df),
              paste0("Robustness filter variable must exist: ", var)
            )
          }
        }
      }
    }
  }

  v
}

print.TDABMProjectValidation <- function(x, ...) {
  cat("\n")
  cat("TDABM project validation\n")
  cat("Status: ", if (isTRUE(x$ok)) "PASS" else "FAIL", "\n", sep = "")
  cat("Checks: ", nrow(x$checks), "\n", sep = "")
  cat("Errors: ", length(x$errors), "\n", sep = "")
  cat("Warnings: ", length(x$warnings), "\n", sep = "")

  if (length(x$errors) > 0L) {
    cat("\nErrors\n")
    cat(paste0("  - ", x$errors, collapse = "\n"), "\n", sep = "")
  }

  if (length(x$warnings) > 0L) {
    cat("\nWarnings\n")
    cat(paste0("  - ", x$warnings, collapse = "\n"), "\n", sep = "")
  }

  invisible(x)
}
