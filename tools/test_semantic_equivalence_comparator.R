#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_file <- if (length(file_arg)) sub("^--file=", "", file_arg[[1L]]) else ""
script_dir <- if (nzchar(script_file)) dirname(normalizePath(script_file, mustWork = TRUE)) else getwd()
repo_root <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
source(file.path(repo_root, "framework", "R", "TDABMSemanticEquivalence.R"))

root <- tempfile("tdabm_p12_test_")
reference_root <- file.path(root, "reference")
candidate_root <- file.path(root, "candidate")
output_root <- file.path(root, "audit")

dir.create(file.path(reference_root, "verification"), recursive = TRUE)
dir.create(file.path(candidate_root, "verification"), recursive = TRUE)
dir.create(file.path(reference_root, "robustness", "manual_exclusion", "verification"), recursive = TRUE)
dir.create(file.path(candidate_root, "robustness", "manual_exclusion", "verification"), recursive = TRUE)
dir.create(file.path(reference_root, "memberships"), recursive = TRUE)
dir.create(file.path(candidate_root, "memberships"), recursive = TRUE)

metadata <- function(run_id, ncores, started) {
  data.frame(
    project_id = "synthetic",
    project_name = "Synthetic equivalence test",
    run_id = run_id,
    variable = "y",
    label = "Synthetic",
    short_label = "Synthetic",
    colour_type = "continuous",
    family = "test",
    dataset_name = "synthetic_data",
    n_observations = 4L,
    axes = "x1, x2",
    radius_min = 0.5,
    radius_max = 0.6,
    radius_step = 0.1,
    x001 = 2L,
    repetitions = 2L,
    jobs = 4L,
    ncores = ncores,
    base_seed = 12345L,
    started = started,
    completed = started,
    elapsed_seconds = ncores,
    stringsAsFactors = FALSE
  )
}

bmstats_object <- function(run_id, ncores = 2L, tolerance_shift = 0) {
  m001 <- data.frame(
    eps = c(0.5, 0.5, 0.6, 0.6),
    rep = c(1L, 2L, 1L, 2L),
    nballs = c(2, 3, 2, 3),
    sdc = c(0.1, 0.2, 0.3, 0.4),
    stringsAsFactors = FALSE
  )
  m001$sdc[[4L]] <- m001$sdc[[4L]] + tolerance_shift

  list(
    m001 = m001,
    xv01 = data.frame(
      mean_xvar1 = c(1, 2, 3, 4),
      min_xvar1 = c(0, 1, 2, 3),
      max_xvar1 = c(2, 3, 4, 5),
      rep = c(1L, 2L, 1L, 2L),
      eps = c(0.5, 0.5, 0.6, 0.6),
      stringsAsFactors = FALSE
    ),
    m001s = data.frame(
      eps = c(0.5, 0.6),
      mn = c(2.5, 2.5),
      msdc = c(0.15, 0.35 + tolerance_shift / 2),
      stringsAsFactors = FALSE
    ),
    settings = list(
      eps_values = c(0.5, 0.6),
      repetitions = 2L,
      threshold = 5,
      base_seed = 12345L,
      ncores = ncores,
      checkpoint_every = 10L
    ),
    tdabm_project_run = metadata(
      run_id,
      ncores,
      if (ncores == 2L) "2026-01-01 00:00:00" else "2026-01-02 00:00:00"
    )
  )
}

membership_object <- function(ncores = 2L) {
  meta <- metadata("P01_membership", ncores, if (ncores == 2L) "2026-01-01 00:00:00" else "2026-01-02 00:00:00")
  meta$pairwise_mode <- "focal"
  meta$n_focal_ids <- 1L

  list(
    membership_long = data.frame(
      eps = c(0.5, 0.5), rep = c(1L, 1L), ball = c(1L, 2L),
      original_id = c("A", "B"), is_isolated_ball = c(FALSE, TRUE),
      stringsAsFactors = FALSE
    ),
    point_repetitions = data.frame(
      eps = c(0.5, 0.5), rep = c(1L, 1L), original_id = c("A", "B"),
      n_ball_memberships = c(1, 1), n_isolated_memberships = c(0, 1),
      only_isolated = c(FALSE, TRUE), stringsAsFactors = FALSE
    ),
    authority_summary = data.frame(
      eps = c(0.5, 0.5), original_id = c("A", "B"),
      n_ball_memberships = c(1, 1), n_isolated_memberships = c(0, 1),
      isolation_frequency = c(0, 1), stringsAsFactors = FALSE
    ),
    isolation_summary = data.frame(
      eps = c(0.5, 0.5), original_id = c("A", "B"),
      isolation_frequency = c(0, 1), n_ball_memberships = c(1, 1),
      n_isolated_memberships = c(0, 1), stringsAsFactors = FALSE
    ),
    pairwise_comembership = data.frame(
      eps = 0.5, id1 = "A", id2 = "B", present = 1,
      completed_repetitions = 2, comembership_frequency = 0.5,
      stringsAsFactors = FALSE
    ),
    graph_repetitions = data.frame(
      eps = c(0.5, 0.5), rep = c(1L, 2L), nballs = c(2L, 2L),
      nedges = c(1L, 1L), ncomponents = c(1L, 1L), nisolated_balls = c(0L, 0L),
      stringsAsFactors = FALSE
    ),
    errors = NULL,
    settings = list(
      eps_values = c(0.5), repetitions = 2L, pairwise = "focal",
      focal_ids = "A", base_seed = 12345L, ncores = ncores,
      checkpoint_every = 10L
    ),
    tdabm_project_run = meta
  )
}

saveRDS(bmstats_object("P01_exact", 2L), file.path(reference_root, "verification", "P01_exact_result.rds"))
saveRDS(bmstats_object("P01_exact", 5L), file.path(candidate_root, "verification", "P01_exact_result.rds"))

saveRDS(bmstats_object("P02_tolerance", 2L), file.path(reference_root, "verification", "P02_tolerance_result.rds"))
saveRDS(bmstats_object("P02_tolerance", 5L, tolerance_shift = 5e-13), file.path(candidate_root, "verification", "P02_tolerance_result.rds"))

saveRDS(bmstats_object("P03_excluded", 2L), file.path(reference_root, "robustness", "manual_exclusion", "verification", "P03_excluded_result.rds"))
saveRDS(bmstats_object("P03_excluded", 5L, tolerance_shift = 1), file.path(candidate_root, "robustness", "manual_exclusion", "verification", "P03_excluded_result.rds"))

saveRDS(membership_object(2L), file.path(reference_root, "memberships", "P01_membership_result.rds"))
saveRDS(membership_object(5L), file.path(candidate_root, "memberships", "P01_membership_result.rds"))

result <- tdabm_compare_production_roots(
  reference_root = reference_root,
  candidate_root = candidate_root,
  output_root = output_root,
  exclusion_regex = "^robustness/manual_exclusion/",
  exclusion_reason = "Synthetic excluded run.",
  abs_tol = 1e-12,
  rel_tol = 1e-10,
  membership_mode = "summary",
  expected_bmstats = 2L,
  expected_membership = 1L,
  verbose = FALSE
)

stopifnot(isTRUE(result$gate_pass))
status <- setNames(result$run_comparison$overall_status, result$run_comparison$relative_path)
stopifnot(status[["verification/P01_exact_result.rds"]] == "METADATA_ONLY_DIFFERENCE")
stopifnot(status[["verification/P02_tolerance_result.rds"]] == "TOLERANCE_EQUIVALENT")
stopifnot(status[["robustness/manual_exclusion/verification/P03_excluded_result.rds"]] == "NOT_COMPARABLE")
stopifnot(status[["memberships/P01_membership_result.rds"]] == "METADATA_ONLY_DIFFERENCE")

cat("PASS: exact analytical values with runtime differences were classified as METADATA_ONLY_DIFFERENCE\n")
cat("PASS: small numerical differences were classified as TOLERANCE_EQUIVALENT\n")
cat("PASS: project-specified exclusions were classified as NOT_COMPARABLE\n")
cat("PASS: membership summaries passed full semantic comparison\n")
cat("PASS: large raw membership components received structure-only checks in summary mode\n")
cat("PASS: the P1.2 synthetic acceptance gate passed\n")
cat("Synthetic audit root:\n  ", output_root, "\n", sep = "")
