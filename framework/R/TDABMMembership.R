# ==============================================================================
# TDABMMembership.R
# ==============================================================================
# Membership-stability execution layer for reusable TDABM projects.
#
# Stage 4 purpose:
#   This module consumes a validated TDABMProject object and runs
#   BMStatsMembership() using the project topology, observation IDs, focal
#   observations, radius grid, parallel settings, checkpointing, and output paths.
#
# What this file does:
#   - load BMCLAUDE3.R and check BMStatsMembership();
#   - load the project dataset through tdabm_project_get_dataset();
#   - prepare topology axes using tdabm_execution_data();
#   - resolve focal labels into focal IDs;
#   - run or load BMStatsMembership();
#   - export authority, isolation, focal, graph, point, pairwise, and error
#     summaries where available;
#   - save metadata and a membership execution audit.
#
# What this file does not do:
#   - robustness transformations;
#   - publication tables;
#   - final uncertainty/statistical framework.
#
# Source order:
#   source("R/TDABMFramework.R")
#   source("R/TDABMUtilities.R")
#   source("R/TDABMProject.R")
#   source("R/TDABMValidation.R")
#   source("R/TDABMExecution.R")
#   source("R/TDABMMembership.R")
# ==============================================================================

if (!exists("is.TDABMProject", mode = "function")) {
  stop("Source TDABMProject.R before TDABMMembership.R.", call. = FALSE)
}

if (!exists("tdabm_execution_data", mode = "function")) {
  stop("Source TDABMExecution.R before TDABMMembership.R.", call. = FALSE)
}

# ------------------------------------------------------------------------------
# Dependency loading
# ------------------------------------------------------------------------------

tdabm_membership_load_dependencies <- function(project, verbose = TRUE) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
  }

  function_file <- project$dependencies$function_file

  if (!tdabm_is_scalar_character(function_file) || !file.exists(function_file)) {
    stop("TDABM function file not found: ", function_file, call. = FALSE)
  }

  if (isTRUE(verbose)) {
    message("Sourcing TDABM function file: ", function_file)
  }

  source(function_file)

  required_functions <- c("BMStatsMembership")
  missing_functions <- required_functions[
    !vapply(required_functions, exists, logical(1), mode = "function")
  ]

  if (length(missing_functions) > 0L) {
    stop(
      "The sourced TDABM function file does not provide required functions: ",
      paste(missing_functions, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Focal-observation helpers
# ------------------------------------------------------------------------------

tdabm_resolve_focal_ids <- function(project, project_data) {
  membership <- project$membership

  if (length(membership$focal_ids) > 0L) {
    return(as.character(membership$focal_ids))
  }

  if (length(membership$focal_labels) == 0L) {
    return(character(0))
  }

  df <- project_data$data
  id_col <- project$data$id_col
  label_col <- membership$focal_label_col %||% project$data$label_col

  if (!label_col %in% names(df)) {
    stop("Focal label column not found: ", label_col, call. = FALSE)
  }

  if (!id_col %in% names(df)) {
    stop("ID column not found: ", id_col, call. = FALSE)
  }

  missing_labels <- setdiff(membership$focal_labels, df[[label_col]])

  if (length(missing_labels) > 0L) {
    stop(
      "Could not resolve focal labels: ",
      paste(missing_labels, collapse = ", "),
      call. = FALSE
    )
  }

  focal_ids <- df[df[[label_col]] %in% membership$focal_labels, id_col, drop = TRUE]
  as.character(focal_ids)
}

tdabm_focal_lookup_table <- function(project, project_data, focal_ids = NULL) {
  df <- project_data$data
  id_col <- project$data$id_col
  label_col <- project$data$label_col
  region_col <- project$data$region_col

  if (is.null(focal_ids)) {
    focal_ids <- tdabm_resolve_focal_ids(project, project_data)
  }

  if (length(focal_ids) == 0L) {
    return(data.frame())
  }

  keep_cols <- c(id_col, label_col, region_col)
  keep_cols <- keep_cols[!is.na(keep_cols) & nzchar(keep_cols) & keep_cols %in% names(df)]

  out <- df[df[[id_col]] %in% focal_ids, keep_cols, drop = FALSE]
  row.names(out) <- NULL
  out
}

# ------------------------------------------------------------------------------
# Output paths and exports
# ------------------------------------------------------------------------------

tdabm_membership_file_paths <- function(project, run_id = NULL) {
  if (is.null(run_id)) {
    run_id <- project$membership$run_id %||% "P01_membership"
  }

  output_paths <- project$outputs$paths
  result_dir <- output_paths$memberships
  checkpoint_dir <- output_paths$checkpoints %||% file.path(output_paths$root, "checkpoints")

  list(
    result_dir = result_dir,
    checkpoint_dir = checkpoint_dir,
    result_file = file.path(result_dir, paste0(run_id, "_result.rds")),
    metadata_file = file.path(result_dir, paste0(run_id, "_metadata.csv")),
    authority_summary_file = file.path(result_dir, paste0(run_id, "_authority_summary.csv")),
    isolation_summary_file = file.path(result_dir, paste0(run_id, "_isolation_summary.csv")),
    focal_authority_summary_file = file.path(result_dir, paste0(run_id, "_focal_authority_summary.csv")),
    focal_isolation_summary_file = file.path(result_dir, paste0(run_id, "_focal_isolation_summary.csv")),
    focal_lookup_file = file.path(result_dir, paste0(run_id, "_focal_lookup.csv")),
    pairwise_file = file.path(result_dir, paste0(run_id, "_pairwise_comembership.csv")),
    graph_repetitions_file = file.path(result_dir, paste0(run_id, "_graph_repetitions.csv")),
    point_repetitions_file = file.path(result_dir, paste0(run_id, "_point_repetitions.csv")),
    errors_file = file.path(result_dir, paste0(run_id, "_errors.csv")),
    checkpoint_file = file.path(checkpoint_dir, paste0(run_id, "_BMStatsMembership.rds"))
  )
}

tdabm_membership_metadata <- function(project, start_time, end_time, project_data, focal_ids) {
  data.frame(
    project_id = project$metadata$project_id,
    project_name = project$metadata$project_name,
    run_id = project$membership$run_id,
    dataset_name = project$data$dataset_name,
    n_observations = nrow(project_data$data),
    id_col = project$data$id_col,
    axes = paste(project_data$axes, collapse = ", "),
    radius_min = project$radius$min,
    radius_max = project$radius$max,
    radius_step = project$radius$step,
    x001 = project$radius$x001,
    repetitions = project$parallel$repetitions,
    jobs = project$radius$x001 * project$parallel$repetitions,
    ncores = project$parallel$ncores,
    base_seed = project$parallel$base_seed,
    pairwise_mode = project$membership$pairwise_mode,
    n_focal_ids = length(focal_ids),
    started = as.character(start_time),
    completed = as.character(end_time),
    elapsed_seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
    stringsAsFactors = FALSE
  )
}

tdabm_export_membership_component <- function(result, component_name, file) {
  component <- result[[component_name]]

  if (!is.null(component)) {
    tdabm_write_table(component, file)
    return(TRUE)
  }

  FALSE
}

tdabm_detect_id_column <- function(x, candidate_names = NULL) {
  if (is.null(x)) {
    return(NA_character_)
  }

  x <- as.data.frame(x)

  if (is.null(candidate_names)) {
    candidate_names <- c(
      "observation_id",
      "original_id",
      "authority_id",
      "id",
      "la_code",
      "point_id",
      "ID"
    )
  }

  hit <- intersect(candidate_names, names(x))

  if (length(hit) == 0L) {
    return(NA_character_)
  }

  hit[[1L]]
}

tdabm_export_focal_membership_tables <- function(project, result, project_data, focal_ids, files) {
  if (length(focal_ids) == 0L) {
    return(invisible(FALSE))
  }

  id_col <- project$data$id_col
  lookup <- tdabm_focal_lookup_table(project, project_data, focal_ids)

  if (nrow(lookup) > 0L) {
    tdabm_write_table(lookup, files$focal_lookup_file)
  }

  authority_summary <- result$authority_summary
  isolation_summary <- result$isolation_summary

  if (!is.null(authority_summary)) {
    x <- as.data.frame(authority_summary)
    x_id <- tdabm_detect_id_column(x)

    if (!is.na(x_id)) {
      focal <- x[x[[x_id]] %in% focal_ids, , drop = FALSE]
      if (nrow(lookup) > 0L && id_col %in% names(lookup)) {
        names(lookup)[names(lookup) == id_col] <- x_id
        focal <- merge(focal, lookup, by = x_id, all.x = TRUE, all.y = FALSE)
      }
      tdabm_write_table(focal, files$focal_authority_summary_file)
    } else {
      warning("Could not identify ID column in authority_summary.")
    }
  }

  if (!is.null(isolation_summary)) {
    x <- as.data.frame(isolation_summary)
    x_id <- tdabm_detect_id_column(x)

    if (!is.na(x_id)) {
      focal <- x[x[[x_id]] %in% focal_ids, , drop = FALSE]
      if (nrow(lookup) > 0L && id_col %in% names(lookup)) {
        names(lookup)[names(lookup) == id_col] <- x_id
        focal <- merge(focal, lookup, by = x_id, all.x = TRUE, all.y = FALSE)
      }
      tdabm_write_table(focal, files$focal_isolation_summary_file)
    } else {
      warning("Could not identify ID column in isolation_summary.")
    }
  }

  invisible(TRUE)
}

tdabm_export_membership_result <- function(project, result, project_data, focal_ids, metadata, files) {
  saveRDS(result, files$result_file)
  tdabm_write_table(metadata, files$metadata_file)

  tdabm_export_membership_component(
    result,
    "authority_summary",
    files$authority_summary_file
  )

  tdabm_export_membership_component(
    result,
    "isolation_summary",
    files$isolation_summary_file
  )

  tdabm_export_membership_component(
    result,
    "pairwise_comembership",
    files$pairwise_file
  )

  tdabm_export_membership_component(
    result,
    "graph_repetitions",
    files$graph_repetitions_file
  )

  tdabm_export_membership_component(
    result,
    "point_repetitions",
    files$point_repetitions_file
  )

  tdabm_export_membership_component(
    result,
    "errors",
    files$errors_file
  )

  tdabm_export_focal_membership_tables(
    project = project,
    result = result,
    project_data = project_data,
    focal_ids = focal_ids,
    files = files
  )

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# BMStatsMembership call
# ------------------------------------------------------------------------------

tdabm_call_BMStatsMembership <- function(project, project_data, focal_ids, files) {
  args <- list(
    xvars = project_data$axes_df,
    yvar = rep(1, nrow(project_data$data)),
    observation_ids = as.character(project_data$data[[project$data$id_col]]),
    epsf = project$radius$min,
    epsint = project$radius$step,
    x001 = project$radius$x001,
    rep = project$parallel$repetitions,
    ncores = project$parallel$ncores,
    bmcpp_path = project$dependencies$bmcpp_path,
    coref_path = project$dependencies$coref_path,
    pairwise = project$membership$pairwise_mode,
    focal_ids = focal_ids,
    checkpoint_file = files$checkpoint_file,
    checkpoint_every = project$parallel$checkpoint_every,
    base_seed = project$parallel$base_seed
  )

  formals_names <- names(formals(BMStatsMembership))
  supported_args <- intersect(names(args), formals_names)
  unsupported_args <- setdiff(names(args), formals_names)

  if (length(unsupported_args) > 0L) {
    message(
      "BMStatsMembership() does not expose these arguments; they will be omitted: ",
      paste(unsupported_args, collapse = ", ")
    )
  }

  do.call(BMStatsMembership, args[supported_args])
}

# ------------------------------------------------------------------------------
# Public membership runner
# ------------------------------------------------------------------------------

RunTDABMMembership <- function(
  project,
  overwrite = FALSE,
  validate_first = TRUE,
  stop_on_validation_error = TRUE,
  verbose = TRUE
) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
  }

  if (!isTRUE(project$membership$enabled)) {
    stop("Membership analysis is disabled in project$membership.", call. = FALSE)
  }

  tdabm_execution_require_packages()

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
      stop("Project validation failed. Membership execution did not start.", call. = FALSE)
    }
  }

  tdabm_create_output_directories(project$outputs$paths, verbose = FALSE)
  tdabm_membership_load_dependencies(project, verbose = verbose)

  project_data <- tdabm_execution_data(project)

  run_id <- project$membership$run_id %||% "P01_membership"
  files <- tdabm_membership_file_paths(project, run_id = run_id)

  tdabm_create_dir(files$result_dir)
  tdabm_create_dir(files$checkpoint_dir)

  if (file.exists(files$result_file) && !isTRUE(overwrite)) {
    if (isTRUE(verbose)) {
      message("Loading existing membership result: ", files$result_file)
    }

    result <- readRDS(files$result_file)

    return(invisible(list(
      project = project,
      result = result,
      files = files
    )))
  }

  focal_ids <- tdabm_resolve_focal_ids(project, project_data)

  if (isTRUE(verbose)) {
    message("\n============================================================")
    message("Starting BMStatsMembership run: ", run_id)
    message("Pairwise mode: ", project$membership$pairwise_mode)
    message("Focal IDs: ", if (length(focal_ids) == 0L) "none" else paste(focal_ids, collapse = ", "))
    message("Radii: ", project$radius$x001)
    message("Repetitions: ", project$parallel$repetitions)
    message("Constructions: ", format(project$radius$x001 * project$parallel$repetitions, big.mark = ","))
    message("============================================================")
  }

  start_time <- Sys.time()

  result <- tdabm_call_BMStatsMembership(
    project = project,
    project_data = project_data,
    focal_ids = focal_ids,
    files = files
  )

  end_time <- Sys.time()

  metadata <- tdabm_membership_metadata(
    project = project,
    start_time = start_time,
    end_time = end_time,
    project_data = project_data,
    focal_ids = focal_ids
  )

  result$tdabm_project_run <- metadata

  tdabm_export_membership_result(
    project = project,
    result = result,
    project_data = project_data,
    focal_ids = focal_ids,
    metadata = metadata,
    files = files
  )

  project <- tdabm_append_history(
    project,
    action = "membership_execution",
    detail = paste0("Completed membership run: ", run_id)
  )

  saveRDS(
    list(project = project, result = result, files = files),
    file.path(project$outputs$paths$logs, "tdabm_membership_execution_latest.rds")
  )

  if (isTRUE(verbose)) {
    message("Completed BMStatsMembership run: ", run_id)
    message("Saved result: ", files$result_file)
  }

  invisible(list(
    project = project,
    result = result,
    files = files,
    metadata = metadata,
    focal_ids = focal_ids
  ))
}
