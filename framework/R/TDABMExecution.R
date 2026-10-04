# ==============================================================================
# TDABMExecution.R
# ==============================================================================
# Primary BMStats execution layer for reusable TDABM projects.
#
# Stage 3 purpose:
#   This module consumes a validated TDABMProject object and runs primary
#   BMStats() analyses from the project$colourings specification table.
#
# What this file does:
#   - load BMCLAUDE3.R and check BMStats() / bmsum();
#   - load the project dataset through tdabm_project_get_dataset();
#   - prepare topology axes;
#   - prepare constant and variable colourings;
#   - run or load BMStats() results;
#   - export repetitions, radius summaries, axis ranges, metadata, and radius-1
#     summaries;
#   - optionally generate bmsum() plots.
#
# What this file does not do yet:
#   - BMStatsMembership();
#   - robustness data transformations;
#   - publication tables;
#   - final statistical stability framework.
#
# Source order:
#   source("R/TDABMFramework.R")
#   source("R/TDABMUtilities.R")
#   source("R/TDABMProject.R")
#   source("R/TDABMValidation.R")
#   source("R/TDABMExecution.R")
# ==============================================================================

if (!exists("is.TDABMProject", mode = "function")) {
  stop("Source TDABMProject.R before TDABMExecution.R.", call. = FALSE)
}

# ------------------------------------------------------------------------------
# Small package helpers
# ------------------------------------------------------------------------------

tdabm_execution_require_packages <- function(
  packages = c("data.table", "ggplot2", "parallel", "igraph", "Rcpp", "BallMapper")
) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) > 0L) {
    stop(
      "Install the missing packages before running TDABM execution:\n  install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "),
      "))",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

tdabm_execution_load_dependencies <- function(project, verbose = TRUE) {
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

  required_functions <- c("BMStats", "bmsum")
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
# Data preparation
# ------------------------------------------------------------------------------

tdabm_execution_data <- function(project) {
  data_result <- tdabm_project_get_dataset(project)

  data <- as.data.frame(data_result$data)

  axes <- project$topology$axes
  if (length(axes) == 0L && !is.null(data_result$axes)) {
    axes <- as.character(data_result$axes)
  }

  if (length(axes) == 0L) {
    stop("No topology axes are available for this project.", call. = FALSE)
  }

  missing_axes <- setdiff(axes, names(data))
  if (length(missing_axes) > 0L) {
    stop(
      "Missing topology axes: ",
      paste(missing_axes, collapse = ", "),
      call. = FALSE
    )
  }

  axes_df <- as.data.frame(data[, axes, drop = FALSE])

  if (anyNA(axes_df)) {
    stop("Topology axes contain missing values.", call. = FALSE)
  }

  if (!all(vapply(axes_df, is.numeric, logical(1)))) {
    stop("All topology axes must be numeric.", call. = FALSE)
  }

  axis_sd <- vapply(axes_df, stats::sd, numeric(1))
  if (any(!is.finite(axis_sd)) || any(axis_sd <= 0)) {
    stop("One or more topology axes have no usable variation.", call. = FALSE)
  }

  list(
    data = data,
    axes = axes,
    axes_df = axes_df,
    input = data_result
  )
}

tdabm_prepare_colour_vector <- function(project, project_data, spec) {
  n <- nrow(project_data$data)

  if (spec$colour_type %in% c("constant")) {
    return(rep(1, n))
  }

  if (spec$colour_type %in% c("computed")) {
    stop(
      "Computed colourings are not implemented in Stage 3. Run ID: ",
      spec$run_id,
      call. = FALSE
    )
  }

  variable <- spec$variable

  if (!tdabm_is_scalar_character(variable) || !variable %in% names(project_data$data)) {
    stop(
      "Colour variable not found for ",
      spec$run_id,
      ": ",
      variable,
      call. = FALSE
    )
  }

  y <- project_data$data[[variable]]

  if (!is.numeric(y)) {
    y <- suppressWarnings(as.numeric(y))
  }

  if (!is.numeric(y) || length(y) != n) {
    stop(
      "Colour variable could not be converted to a numeric vector for ",
      spec$run_id,
      ": ",
      variable,
      call. = FALSE
    )
  }

  if (anyNA(y) || any(!is.finite(y))) {
    stop(
      "Colour variable contains missing or non-finite values for ",
      spec$run_id,
      ": ",
      variable,
      call. = FALSE
    )
  }

  y
}

# ------------------------------------------------------------------------------
# Output helpers
# ------------------------------------------------------------------------------

tdabm_run_file_paths <- function(project, run_id, result_subdir = "verification") {
  output_paths <- project$outputs$paths

  if (!result_subdir %in% names(output_paths)) {
    stop("Unknown output subdirectory: ", result_subdir, call. = FALSE)
  }

  result_dir <- output_paths[[result_subdir]]
  checkpoint_dir <- output_paths$checkpoints %||% file.path(output_paths$root, "checkpoints")

  list(
    result_dir = result_dir,
    checkpoint_dir = checkpoint_dir,
    result_file = file.path(result_dir, paste0(run_id, "_result.rds")),
    repetitions_file = file.path(result_dir, paste0(run_id, "_repetitions.csv")),
    radius_summary_file = file.path(result_dir, paste0(run_id, "_radius_summary.csv")),
    axis_ranges_file = file.path(result_dir, paste0(run_id, "_axis_ranges.csv")),
    metadata_file = file.path(result_dir, paste0(run_id, "_metadata.csv")),
    checkpoint_file = file.path(checkpoint_dir, paste0(run_id, "_BMStats.rds"))
  )
}

tdabm_write_table <- function(x, file) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fwrite(data.table::as.data.table(x), file)
  } else {
    utils::write.csv(as.data.frame(x), file, row.names = FALSE)
  }
  invisible(file)
}

tdabm_result_metadata <- function(project, spec, start_time, end_time, project_data) {
  data.frame(
    project_id = project$metadata$project_id,
    project_name = project$metadata$project_name,
    run_id = spec$run_id,
    variable = spec$variable,
    label = spec$label,
    short_label = spec$short_label,
    colour_type = spec$colour_type,
    family = spec$family,
    dataset_name = project$data$dataset_name,
    n_observations = nrow(project_data$data),
    axes = paste(project_data$axes, collapse = ", "),
    radius_min = project$radius$min,
    radius_max = project$radius$max,
    radius_step = project$radius$step,
    x001 = project$radius$x001,
    repetitions = project$parallel$repetitions,
    jobs = project$radius$x001 * project$parallel$repetitions,
    ncores = project$parallel$ncores,
    base_seed = project$parallel$base_seed,
    started = as.character(start_time),
    completed = as.character(end_time),
    elapsed_seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# Run one BMStats analysis
# ------------------------------------------------------------------------------

tdabm_run_or_load_bmstats <- function(
  project,
  spec,
  project_data = NULL,
  overwrite = FALSE,
  generate_plots = TRUE,
  verbose = TRUE
) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
  }

  if (is.null(project_data)) {
    project_data <- tdabm_execution_data(project)
  }

  spec <- as.data.frame(spec, stringsAsFactors = FALSE)

  if (nrow(spec) != 1L) {
    stop("tdabm_run_or_load_bmstats() expects exactly one analysis specification row.", call. = FALSE)
  }

  run_id <- as.character(spec$run_id[[1L]])
  files <- tdabm_run_file_paths(project, run_id, result_subdir = "verification")

  tdabm_create_dir(files$result_dir)
  tdabm_create_dir(files$checkpoint_dir)

  if (file.exists(files$result_file) && !isTRUE(overwrite)) {
    if (isTRUE(verbose)) {
      message("Loading existing BMStats result: ", files$result_file)
    }
    result <- readRDS(files$result_file)
    return(result)
  }

  yvar <- tdabm_prepare_colour_vector(project, project_data, spec)

  if (length(yvar) != nrow(project_data$axes_df)) {
    stop(
      run_id,
      ": colour vector length does not match topology data.",
      call. = FALSE
    )
  }

  if (isTRUE(verbose)) {
    message("\n============================================================")
    message("Starting BMStats run: ", run_id)
    message("Colouring: ", spec$variable)
    message("Label: ", spec$label)
    message("Radii: ", project$radius$x001)
    message("Repetitions: ", project$parallel$repetitions)
    message("Constructions: ", format(project$radius$x001 * project$parallel$repetitions, big.mark = ","))
    message("============================================================")
  }

  start_time <- Sys.time()

  result <- BMStats(
    xvars = project_data$axes_df,
    yvar = yvar,
    epsf = project$radius$min,
    epsint = project$radius$step,
    x001 = project$radius$x001,
    rep = project$parallel$repetitions,
    thresh2 = project$parallel$thresh2,
    xvar = TRUE,
    ncores = project$parallel$ncores,
    bmcpp_path = project$dependencies$bmcpp_path,
    coref_path = project$dependencies$coref_path,
    checkpoint_file = files$checkpoint_file,
    checkpoint_every = project$parallel$checkpoint_every,
    base_seed = project$parallel$base_seed
  )

  end_time <- Sys.time()

  metadata <- tdabm_result_metadata(
    project = project,
    spec = spec,
    start_time = start_time,
    end_time = end_time,
    project_data = project_data
  )

  result$tdabm_project_run <- metadata

  saveRDS(result, files$result_file)

  if (!is.null(result$m001)) {
    tdabm_write_table(result$m001, files$repetitions_file)
  }

  if (!is.null(result$m001s)) {
    tdabm_write_table(result$m001s, files$radius_summary_file)
  }

  if (!is.null(result$xv01)) {
    tdabm_write_table(result$xv01, files$axis_ranges_file)
  }

  tdabm_write_table(metadata, files$metadata_file)

  if (isTRUE(generate_plots) && exists("bmsum", mode = "function") && !is.null(result$m001s)) {
    tdabm_generate_bmsum_plots(
      project = project,
      result = result,
      spec = spec,
      verbose = verbose
    )
  }

  if (isTRUE(verbose)) {
    message("Completed BMStats run: ", run_id)
    message("Saved result: ", files$result_file)
  }

  result
}

# ------------------------------------------------------------------------------
# bmsum plot wrapper
# ------------------------------------------------------------------------------

tdabm_generate_bmsum_plots <- function(project, result, spec, verbose = TRUE) {
  if (is.null(result) || is.null(result$m001s)) {
    warning("No m001s object available for ", spec$run_id)
    return(invisible(FALSE))
  }

  if (!exists("bmsum", mode = "function")) {
    warning("bmsum() is not available.")
    return(invisible(FALSE))
  }

  plot_dir <- project$outputs$paths$summary_plots %||% file.path(project$outputs$paths$root, "summary_plots")
  tdabm_create_dir(plot_dir)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  setwd(plot_dir)

  if (isTRUE(verbose)) {
    message("Generating bmsum plots for ", spec$run_id, " in ", plot_dir)
  }

  bmsum(
    m001s = result$m001s,
    epstab = result$m001s$eps,
    suff = paste0("_", spec$run_id),
    dp = 2,
    col1 = spec$short_label,
    col2 = paste0(spec$short_label, " SD"),
    col3 = paste0(spec$short_label, " range")
  )

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Run primary colourings
# ------------------------------------------------------------------------------

tdabm_primary_analysis_specs <- function(
  project,
  run_ids = NULL,
  include_disabled = FALSE
) {
  specs <- project$colourings

  if (!is.data.frame(specs) || nrow(specs) == 0L) {
    stop("No colourings are defined in project$colourings.", call. = FALSE)
  }

  if (!isTRUE(include_disabled)) {
    specs <- specs[specs$enabled %in% TRUE & specs$run_primary %in% TRUE, , drop = FALSE]
  }

  if (!is.null(run_ids)) {
    specs <- specs[specs$run_id %in% run_ids, , drop = FALSE]
  }

  if (nrow(specs) > 0L && "priority" %in% names(specs)) {
    specs <- specs[order(specs$priority, specs$run_id), , drop = FALSE]
  }

  specs
}

RunTDABMPrimary <- function(
  project,
  run_ids = NULL,
  overwrite = FALSE,
  generate_plots = TRUE,
  validate_first = TRUE,
  stop_on_validation_error = TRUE,
  verbose = TRUE
) {
  if (!is.TDABMProject(project)) {
    stop("Expected a TDABMProject object.", call. = FALSE)
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
      stop("Project validation failed. BMStats execution did not start.", call. = FALSE)
    }
  }

  tdabm_create_output_directories(project$outputs$paths, verbose = FALSE)
  tdabm_execution_load_dependencies(project, verbose = verbose)

  project_data <- tdabm_execution_data(project)

  specs <- tdabm_primary_analysis_specs(
    project = project,
    run_ids = run_ids,
    include_disabled = FALSE
  )

  if (nrow(specs) == 0L) {
    stop("No enabled primary analysis specifications were selected.", call. = FALSE)
  }

  if (isTRUE(verbose)) {
    message("\nSelected primary BMStats analyses:")
    print(specs[, c("run_id", "variable", "short_label", "priority"), drop = FALSE])
  }

  results <- list()

  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, , drop = FALSE]

    results[[spec$run_id]] <- tdabm_run_or_load_bmstats(
      project = project,
      spec = spec,
      project_data = project_data,
      overwrite = overwrite,
      generate_plots = generate_plots,
      verbose = verbose
    )
  }

  radius_one <- tdabm_extract_radius_summaries(
    results = results,
    radius_value = 1.00
  )

  if (nrow(radius_one) > 0L) {
    tdabm_write_table(
      radius_one,
      file.path(project$outputs$paths$verification, "radius_1_primary_results.csv")
    )
  }

  run_audit <- tdabm_primary_run_audit(project, results)
  tdabm_write_table(
    run_audit,
    file.path(project$outputs$paths$verification, "primary_execution_audit.csv")
  )

  project <- tdabm_append_history(
    project,
    action = "primary_execution",
    detail = paste0("Completed ", length(results), " primary BMStats run(s).")
  )

  saveRDS(
    list(project = project, results = results),
    file.path(project$outputs$paths$logs, "tdabm_primary_execution_latest.rds")
  )

  invisible(list(
    project = project,
    results = results,
    radius_one = radius_one,
    audit = run_audit
  ))
}

# ------------------------------------------------------------------------------
# Summary extraction
# ------------------------------------------------------------------------------

tdabm_extract_radius_summaries <- function(results, radius_value = 1.00, tolerance = 1e-8) {
  if (is.null(results) || length(results) == 0L) {
    return(data.frame())
  }

  rows <- lapply(names(results), function(run_id) {
    result <- results[[run_id]]

    if (is.null(result) || is.null(result$m001s)) {
      return(NULL)
    }

    x <- as.data.frame(result$m001s)

    if (!"eps" %in% names(x)) {
      return(NULL)
    }

    row <- x[abs(x$eps - radius_value) < tolerance, , drop = FALSE]

    if (nrow(row) == 0L) {
      return(NULL)
    }

    row$run_id <- run_id

    if (!is.null(result$tdabm_project_run)) {
      row$variable <- result$tdabm_project_run$variable[[1L]]
      row$label <- result$tdabm_project_run$label[[1L]]
    }

    row
  })

  rows <- Filter(Negate(is.null), rows)

  if (length(rows) == 0L) {
    return(data.frame())
  }

  out <- do.call(rbind, rows)
  first_cols <- intersect(c("run_id", "variable", "label", "eps"), names(out))
  out <- out[, c(first_cols, setdiff(names(out), first_cols)), drop = FALSE]
  row.names(out) <- NULL
  out
}

tdabm_primary_run_audit <- function(project, results) {
  rows <- lapply(names(results), function(run_id) {
    result <- results[[run_id]]

    metadata <- result$tdabm_project_run

    if (is.null(metadata)) {
      return(data.frame(
        run_id = run_id,
        result_available = !is.null(result),
        stringsAsFactors = FALSE
      ))
    }

    metadata$result_available <- !is.null(result)
    metadata
  })

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}
