# ==============================================================================
# TDABMOrchestrator.R
# ==============================================================================
# Project-level orchestration for the reusable TDABM framework.
#
# Stage 6 purpose:
#   This module gives the framework a single controlled entry point.  It runs:
#     1. validation;
#     2. primary BMStats analyses;
#     3. membership stability;
#     4. robustness BMStats analyses;
#     5. final audit/session outputs.
#
# It does not introduce new statistical logic.  It coordinates the modules already
# built in Stages 1--5.
#
# Source order:
#   source("R/TDABMFramework.R")
#   source("R/TDABMUtilities.R")
#   source("R/TDABMProject.R")
#   source("R/TDABMValidation.R")
#   source("R/TDABMExecution.R")
#   source("R/TDABMMembership.R")
#   source("R/TDABMOutlierDiagnostics.R")
#   source("R/TDABMRobustness.R")
#   source("R/TDABMOrchestrator.R")
# ==============================================================================

if (!exists("is.TDABMProject", mode = "function")) {
  stop("Source TDABMProject.R before TDABMOrchestrator.R.", call. = FALSE)
}

if (!exists("RunTDABMPrimary", mode = "function")) {
  stop("Source TDABMExecution.R before TDABMOrchestrator.R.", call. = FALSE)
}

if (!exists("RunTDABMMembership", mode = "function")) {
  stop("Source TDABMMembership.R before TDABMOrchestrator.R.", call. = FALSE)
}

if (!exists("RunTDABMRobustness", mode = "function")) {
  stop("Source TDABMRobustness.R before TDABMOrchestrator.R.", call. = FALSE)
}

# ------------------------------------------------------------------------------
# Plan object
# ------------------------------------------------------------------------------

TDABMRunPlan <- function(
  run_primary = TRUE,
  run_membership = TRUE,
  run_robustness = TRUE,
  primary_run_ids = NULL,
  robustness_ids = NULL,
  robustness_run_ids = NULL,
  overwrite = FALSE,
  generate_plots = TRUE,
  validate_first = TRUE,
  run_dependency_smoke_test = FALSE,
  stop_on_validation_error = TRUE
) {
  structure(
    list(
      run_primary = isTRUE(run_primary),
      run_membership = isTRUE(run_membership),
      run_robustness = isTRUE(run_robustness),
      primary_run_ids = primary_run_ids,
      robustness_ids = robustness_ids,
      robustness_run_ids = robustness_run_ids,
      overwrite = isTRUE(overwrite),
      generate_plots = isTRUE(generate_plots),
      validate_first = isTRUE(validate_first),
      run_dependency_smoke_test = isTRUE(run_dependency_smoke_test),
      stop_on_validation_error = isTRUE(stop_on_validation_error)
    ),
    class = "TDABMRunPlan"
  )
}

print.TDABMRunPlan <- function(x, ...) {
  cat("\n")
  cat("============================================================\n")
  cat("TDABM run plan\n")
  cat("============================================================\n")
  cat("Primary:       ", if (x$run_primary) "yes" else "no", "\n", sep = "")
  cat("Membership:    ", if (x$run_membership) "yes" else "no", "\n", sep = "")
  cat("Robustness:    ", if (x$run_robustness) "yes" else "no", "\n", sep = "")
  cat("Overwrite:     ", if (x$overwrite) "yes" else "no", "\n", sep = "")
  cat("Plots:         ", if (x$generate_plots) "yes" else "no", "\n", sep = "")
  cat("Validate first:", if (x$validate_first) "yes" else "no", "\n", sep = "")
  cat("Primary IDs:   ", paste(x$primary_run_ids %||% "all enabled primary", collapse = ", "), "\n", sep = "")
  cat("Robust IDs:    ", paste(x$robustness_ids %||% "all enabled robustness", collapse = ", "), "\n", sep = "")
  cat("Robust runs:   ", paste(x$robustness_run_ids %||% "specified by robustness table", collapse = ", "), "\n", sep = "")
  cat("============================================================\n")
  invisible(x)
}

TDABMProductionPlan <- function(
  primary_run_ids = NULL,
  robustness_ids = NULL,
  robustness_run_ids = NULL,
  overwrite = FALSE,
  generate_plots = TRUE
) {
  TDABMRunPlan(
    run_primary = TRUE,
    run_membership = TRUE,
    run_robustness = TRUE,
    primary_run_ids = primary_run_ids,
    robustness_ids = robustness_ids,
    robustness_run_ids = robustness_run_ids,
    overwrite = overwrite,
    generate_plots = generate_plots,
    validate_first = TRUE,
    run_dependency_smoke_test = FALSE,
    stop_on_validation_error = TRUE
  )
}

TDABMSmokePlan <- function(
  primary_run_ids = NULL,
  robustness_ids = NULL,
  robustness_run_ids = NULL,
  overwrite = TRUE,
  generate_plots = FALSE
) {
  TDABMRunPlan(
    run_primary = TRUE,
    run_membership = TRUE,
    run_robustness = TRUE,
    primary_run_ids = primary_run_ids,
    robustness_ids = robustness_ids,
    robustness_run_ids = robustness_run_ids,
    overwrite = overwrite,
    generate_plots = generate_plots,
    validate_first = TRUE,
    run_dependency_smoke_test = TRUE,
    stop_on_validation_error = TRUE
  )
}

# ------------------------------------------------------------------------------
# Source helper
# ------------------------------------------------------------------------------

source_tdabm_full_framework <- function(
  tdabm_root = if (exists("tdabm_find_framework_root", mode = "function")) tdabm_find_framework_root() else Sys.getenv("TDABM_FRAMEWORK_ROOT", unset = getwd()),
  verbose = TRUE
) {
  r_dir <- file.path(tdabm_root, "R")

  files <- c(
    "TDABMFramework.R",
    "TDABMUtilities.R",
    "TDABMProject.R",
    "TDABMValidation.R",
    "TDABMExecution.R",
    "TDABMMembership.R",
    "TDABMOutlierDiagnostics.R",
    "TDABMRobustness.R",
    "TDABMOrchestrator.R"
  )

  for (f in files) {
    fpath <- file.path(r_dir, f)
    if (!file.exists(fpath)) {
      stop("Required TDABM framework source file not found: ", fpath, call. = FALSE)
    }
    if (isTRUE(verbose)) {
      message("Sourcing ", fpath)
    }
    source(fpath)
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Audit helpers
# ------------------------------------------------------------------------------

tdabm_orchestration_start_audit <- function(project, plan) {
  data.frame(
    project_id = project$metadata$project_id,
    project_name = project$metadata$project_name,
    framework_version = project$metadata$framework_version,
    run_primary = plan$run_primary,
    run_membership = plan$run_membership,
    run_robustness = plan$run_robustness,
    primary_run_ids = paste(plan$primary_run_ids %||% "all", collapse = ", "),
    robustness_ids = paste(plan$robustness_ids %||% "all", collapse = ", "),
    robustness_run_ids = paste(plan$robustness_run_ids %||% "table_defaults", collapse = ", "),
    overwrite = plan$overwrite,
    generate_plots = plan$generate_plots,
    radius_min = project$radius$min,
    radius_max = project$radius$max,
    radius_step = project$radius$step,
    x001 = project$radius$x001,
    repetitions = project$parallel$repetitions,
    ncores = project$parallel$ncores,
    started = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
}

tdabm_write_session_info <- function(project, suffix = "orchestrator") {
  logs_dir <- project$outputs$paths$logs %||% file.path(project$outputs$paths$root, "logs")
  tdabm_create_dir(logs_dir)

  file <- file.path(logs_dir, paste0("sessionInfo_", suffix, ".txt"))
  writeLines(capture.output(sessionInfo()), file)
  invisible(file)
}

tdabm_collect_output_audit <- function(project, suffix = "orchestrator") {
  root <- project$outputs$paths$root
  logs_dir <- project$outputs$paths$logs %||% file.path(root, "logs")
  tdabm_create_dir(logs_dir)

  files <- list.files(root, recursive = TRUE, full.names = TRUE)

  if (length(files) == 0L) {
    audit <- data.frame(
      file = character(0),
      size_bytes = numeric(0),
      modified = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    info <- file.info(files)
    audit <- data.frame(
      file = files,
      size_bytes = as.numeric(info$size),
      modified = as.character(info$mtime),
      stringsAsFactors = FALSE
    )
  }

  out_file <- file.path(logs_dir, paste0("output_audit_", suffix, ".csv"))
  tdabm_write_table(audit, out_file)

  invisible(audit)
}

tdabm_orchestration_result_summary <- function(result) {
  data.frame(
    component = c("validation", "primary", "membership", "robustness"),
    available = c(
      !is.null(result$validation),
      !is.null(result$primary),
      !is.null(result$membership),
      !is.null(result$robustness)
    ),
    status = c(
      if (!is.null(result$validation) && isTRUE(result$validation$ok)) "PASS" else if (!is.null(result$validation)) "FAIL" else "not_run",
      if (!is.null(result$primary)) "available" else "not_run",
      if (!is.null(result$membership)) "available" else "not_run",
      if (!is.null(result$robustness)) "available" else "not_run"
    ),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# Main orchestrator
# ------------------------------------------------------------------------------

RunTDABMProject <- function(
  project,
  plan = TDABMRunPlan(),
  verbose = TRUE
) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
  }

  if (!inherits(plan, "TDABMRunPlan")) {
    stop("Expected a TDABMRunPlan object.", call. = FALSE)
  }

  tdabm_create_output_directories(project$outputs$paths, verbose = FALSE)

  if (isTRUE(verbose)) {
    print(project)
    print(plan)
  }

  start_audit <- tdabm_orchestration_start_audit(project, plan)
  tdabm_write_table(
    start_audit,
    file.path(project$outputs$paths$logs, "tdabm_orchestration_start_audit.csv")
  )

  validation <- NULL

  if (isTRUE(plan$validate_first)) {
    validation <- validate_tdabm_project(
      project,
      check_files = TRUE,
      check_data = TRUE,
      create_dirs = TRUE,
      run_dependency_smoke_test = plan$run_dependency_smoke_test
    )

    project$validation <- validation

    if (isTRUE(verbose)) {
      print(validation)
    }

    write_tdabm_validation_report(
      validation = validation,
      output_dir = project$outputs$paths$logs,
      prefix = "tdabm_orchestration_validation",
      include_timestamp = TRUE
    )

    if (!isTRUE(validation$ok) && isTRUE(plan$stop_on_validation_error)) {
      stop("Project validation failed. Orchestration stopped.", call. = FALSE)
    }
  }

  primary <- NULL
  membership <- NULL
  robustness <- NULL

  if (isTRUE(plan$run_primary)) {
    primary <- RunTDABMPrimary(
      project = project,
      run_ids = plan$primary_run_ids,
      overwrite = plan$overwrite,
      generate_plots = plan$generate_plots,
      validate_first = FALSE,
      stop_on_validation_error = plan$stop_on_validation_error,
      verbose = verbose
    )
    project <- primary$project
  }

  if (isTRUE(plan$run_membership)) {
    membership <- RunTDABMMembership(
      project = project,
      overwrite = plan$overwrite,
      validate_first = FALSE,
      stop_on_validation_error = plan$stop_on_validation_error,
      verbose = verbose
    )
    project <- membership$project
  }

  if (isTRUE(plan$run_robustness)) {
    robustness <- RunTDABMRobustness(
      project = project,
      robustness_ids = plan$robustness_ids,
      run_ids = plan$robustness_run_ids,
      include_original = FALSE,
      overwrite = plan$overwrite,
      generate_plots = plan$generate_plots,
      validate_first = FALSE,
      stop_on_validation_error = plan$stop_on_validation_error,
      verbose = verbose
    )
    project <- robustness$project
  }

  project <- tdabm_append_history(
    project,
    action = "orchestration_complete",
    detail = "RunTDABMProject completed."
  )

  session_file <- tdabm_write_session_info(project, suffix = "orchestrator")
  output_audit <- tdabm_collect_output_audit(project, suffix = "orchestrator")

  result <- list(
    project = project,
    plan = plan,
    validation = validation,
    primary = primary,
    membership = membership,
    robustness = robustness,
    session_file = session_file,
    output_audit = output_audit
  )

  summary <- tdabm_orchestration_result_summary(result)
  tdabm_write_table(
    summary,
    file.path(project$outputs$paths$logs, "tdabm_orchestration_component_summary.csv")
  )

  saveRDS(
    result,
    file.path(project$outputs$paths$logs, "tdabm_orchestration_latest.rds")
  )

  if (isTRUE(verbose)) {
    cat("\n")
    cat("============================================================\n")
    cat("TDABM ORCHESTRATION COMPLETE\n")
    cat("============================================================\n")
    print(summary, row.names = FALSE)
    cat("Output root:\n  ", project$outputs$paths$root, "\n", sep = "")
    cat("Saved latest orchestration object:\n  ",
        file.path(project$outputs$paths$logs, "tdabm_orchestration_latest.rds"),
        "\n", sep = "")
    cat("============================================================\n")
  }

  invisible(result)
}
