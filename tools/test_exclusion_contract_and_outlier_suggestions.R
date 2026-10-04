#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1L) {
  repo_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
} else {
  file_args <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_args) == 0L) {
    stop("Could not determine repository root. Pass it as the first argument.", call. = FALSE)
  }
  script_file <- sub("^--file=", "", file_args[[1L]])
  repo_root <- normalizePath(file.path(dirname(script_file), ".."), winslash = "/", mustWork = TRUE)
}

r_dir <- file.path(repo_root, "framework", "R")
source(file.path(r_dir, "TDABMFramework.R"))
source(file.path(r_dir, "TDABMUtilities.R"))
source(file.path(r_dir, "TDABMProject.R"))
source(file.path(r_dir, "TDABMValidation.R"))
source(file.path(r_dir, "TDABMExecution.R"))
source(file.path(r_dir, "TDABMOutlierDiagnostics.R"))
source(file.path(r_dir, "TDABMRobustness.R"))

# Clean-baseline self-tests must not write into the portability source tree.
# Optional second argument = external retained test-output directory.
if (length(args) >= 2L) {
  output_dir <- normalizePath(args[[2L]], winslash = "/", mustWork = FALSE)
  cleanup_output <- FALSE
} else {
  output_dir <- tempfile("tdabm_p1_1_exclusion_contract_test_")
  cleanup_output <- TRUE
}
if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE, force = TRUE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
if (cleanup_output) on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

set.seed(12345)
synthetic <- data.frame(
  observation_id = sprintf("O%02d", 1:16),
  observation_label = paste("Observation", 1:16),
  axis_1 = c(rnorm(15, 0, 0.5), 9),
  axis_2 = c(rnorm(15, 0, 0.5), -8),
  colour_1 = c(rnorm(15, 5, 0.7), 20),
  stringsAsFactors = FALSE
)

project <- TDABMProject(
  metadata = list(
    project_id = "p1_1_synthetic_test",
    project_name = "P1.1 synthetic exclusion-contract test"
  ),
  paths = list(project_root = output_dir, framework_root = file.path(repo_root, "framework")),
  data = list(
    data_format = "data_frame",
    data_frame = synthetic,
    dataset_name = "synthetic",
    id_col = "observation_id",
    label_col = "observation_label"
  ),
  topology = list(axes = c("axis_1", "axis_2"), scaling = "as_supplied"),
  colourings = list(
    P01_topology = list(
      run_id = "P01_topology", variable = "__constant__", label = "Topology",
      short_label = "Topology", colour_type = "constant", family = "topology",
      enabled = TRUE, priority = 1L, run_primary = TRUE, run_robustness = TRUE
    ),
    P02_colour = list(
      run_id = "P02_colour", variable = "colour_1", label = "Synthetic colour",
      short_label = "Colour", colour_type = "variable", family = "outcome",
      enabled = TRUE, priority = 2L, run_primary = TRUE, run_robustness = TRUE
    )
  ),
  robustness = list(
    primary = list(
      robustness_id = "primary", type = "original", enabled = TRUE,
      analysis_run_ids = c("P01_topology", "P02_colour")
    )
  ),
  radius = list(min = 0.5, max = 0.6, step = 0.1),
  parallel = list(ncores = 1L, repetitions = 2L),
  outputs = list(root = output_dir),
  membership = list(enabled = FALSE),
  dependencies = list(),
  validate = FALSE
)

project_before <- serialize(project, NULL)
rows_before <- nrow(tdabm_project_get_dataset(project)$data)

diagnostic <- tdabm_outlier_suggestions(
  project = project,
  method = "mad",
  mad_threshold = 3.5,
  output_dir = file.path(output_dir, "diagnostics"),
  write_outputs = TRUE,
  verbose = TRUE
)

stopifnot("O16" %in% diagnostic$suggestions_by_observation$observation_id)
stopifnot(all(diagnostic$suggestions_long$automatic_exclusion_performed %in% FALSE))
stopifnot(identical(project_before, serialize(project, NULL)))
stopifnot(rows_before == nrow(tdabm_project_get_dataset(project)$data))

manual <- TDABMManualExclusionSpec(
  robustness_id = "user_selected_O16",
  variable = "observation_id",
  values = "O16",
  analysis_run_ids = c("P01_topology", "P02_colour"),
  rationale = "Synthetic user decision after reviewing diagnostic tables.",
  enabled = TRUE
)

project_manual <- project
project_manual$robustness <- tdabm_normalise_robustness_specs(list(user_selected_O16 = manual))
spec <- project_manual$robustness[1, , drop = FALSE]
filtered <- tdabm_apply_robustness_transform(project_manual, spec)
filter_audit <- attr(filtered, "tdabm_robustness_transform_audit")

stopifnot(nrow(filtered) == rows_before - 1L)
stopifnot(!"O16" %in% filtered$observation_id)
stopifnot(filter_audit$summary$n_removed[[1L]] == 1L)
stopifnot(isFALSE(filter_audit$summary$automatic_exclusion_performed[[1L]]))

bad <- TDABMManualExclusionSpec(
  robustness_id = "bad_zero_match",
  variable = "observation_id",
  values = "NOT_PRESENT",
  enabled = TRUE
)
project_bad <- project
project_bad$robustness <- tdabm_normalise_robustness_specs(list(bad_zero_match = bad))
bad_error <- tryCatch(
  {
    tdabm_apply_robustness_transform(project_bad, project_bad$robustness[1, , drop = FALSE])
    NULL
  },
  error = function(e) e
)
stopifnot(inherits(bad_error, "error"))

acceptance <- c(
  "TDABM P1.1 exclusion contract and outlier diagnostic synthetic test",
  paste0("Framework version: ", TDABMFrameworkVersion()),
  "PASS: diagnostic suggestions identified the planted candidate",
  "PASS: diagnostic suggestions did not alter project data",
  "PASS: diagnostic suggestions did not alter project robustness specifications",
  "PASS: explicit user-specified exclusion removed exactly one requested observation",
  "PASS: exclusion audit recorded automatic_exclusion_performed = FALSE",
  "PASS: zero-match exclusion failed before analysis",
  paste0("Output directory: ", output_dir)
)
writeLines(acceptance, file.path(output_dir, "P1_1_SYNTHETIC_ACCEPTANCE.txt"))
cat(paste(acceptance, collapse = "\n"), "\n")
