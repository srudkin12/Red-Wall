#!/usr/bin/env Rscript
options(warn = 2)

# Derive the release root from the script path supplied by Rscript.
file_arg <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", file_arg[grepl("^--file=", file_arg)][1L])
repo <- normalizePath(file.path(dirname(file_arg), ".."), winslash = "/", mustWork = TRUE)
module_path <- file.path(repo, "framework", "R", "TDABMRadiusDiagnostics.R")
cpp_path <- file.path(repo, "framework", "BallMapper.cpp")
source(module_path, local = FALSE)

# Packed signature storage must be valid for arbitrary logical lengths, not
# only lengths divisible by eight. The regression set explicitly covers
# non-byte-aligned landmark and cover-relation signature lengths.
signature_lengths <- c(1L, 2L, 7L, 8L, 9L, 24L, 100L, 276L, 4950L)
for (n_bits in signature_lengths) {
  bits <- rep(FALSE, n_bits)
  bits[unique(c(1L, max(1L, n_bits %/% 2L), n_bits))] <- TRUE
  packed <- tdabm_rd_pack_signature(bits)
  stopifnot(is.raw(packed))
  stopifnot(length(packed) == ceiling(n_bits / 8))
  stopifnot(identical(tdabm_rd_unpack_signature(packed, n_bits), bits))
}

# Malformed signatures fail closed rather than being silently truncated or
# expanded. All-zero signatures have Jaccard similarity one by convention.
malformed <- tdabm_rd_pack_signature(rep(FALSE, 9L))
stopifnot(inherits(try(tdabm_rd_unpack_signature(malformed, 8L), silent = TRUE), "try-error"))
zero7 <- tdabm_rd_pack_signature(rep(FALSE, 7L))
stopifnot(identical(tdabm_rd_jaccard_signature(zero7, zero7, 7L), 1))

# Direct cover/landmark helpers must support sample sizes whose corresponding
# bit counts are not byte-aligned.
cover24 <- lapply(seq_len(24L), function(i) i)
stopifnot(length(tdabm_rd_cover_signature(cover24, 24L)) == ceiling(choose(24L, 2L) / 8))
stopifnot(length(tdabm_rd_landmark_signature(seq_len(24L), 24L)) == ceiling(24L / 8))
cover100 <- lapply(seq_len(100L), function(i) i)
stopifnot(length(tdabm_rd_cover_signature(cover100, 100L)) == ceiling(choose(100L, 2L) / 8))
stopifnot(length(tdabm_rd_landmark_signature(seq_len(100L), 100L)) == ceiling(100L / 8))

set.seed(901)
n <- 24L
synthetic <- data.frame(
  unit_id = sprintf("u%03d", seq_len(n)),
  x1 = c(stats::rnorm(n / 2L, -2, 0.45), stats::rnorm(n / 2L, 2, 0.45)),
  x2 = c(stats::rnorm(n / 2L, -1, 0.55), stats::rnorm(n / 2L, 1, 0.55)),
  x3 = stats::rnorm(n),
  stringsAsFactors = FALSE
)

validation <- tdabm_rd_validate_topology_frame(synthetic, "unit_id", c("x1", "x2", "x3"), scaling = "as_supplied", metric = "euclidean", expected_rows = n)
stopifnot(validation$pass)
prepared <- tdabm_rd_prepare_topology(synthetic, "unit_id", c("x1", "x2", "x3"), scaling = "as_supplied", metric = "euclidean")
grid <- tdabm_rd_make_radius_grid(prepared$axes, grid_points = 11L, lower_multiplier = 0.75, upper_multiplier = 1.000000001)
stopifnot(nrow(grid) == 11L, all(diff(grid$radius) > 0))

run1 <- tdabm_rd_run_replicates(
  prepared$axes, prepared$ids, grid,
  repetitions = 6L, base_seed = 44001L, ncores = 1L,
  module_path = module_path, cpp_path = cpp_path
)
run2 <- tdabm_rd_run_replicates(
  prepared$axes, prepared$ids, grid,
  repetitions = 6L, base_seed = 44001L, ncores = 2L,
  module_path = module_path, cpp_path = cpp_path
)
worker <- tdabm_rd_compare_runs(run1$diagnostics, run2$diagnostics, tolerance = 0)
stopifnot(all(worker$pass))

# The broad grid must deliberately include both degenerate anchors so the user
# can see the complete topology transition rather than only a preselected band.
first <- run1$diagnostics[run1$diagnostics$radius_index == 1L, , drop = FALSE]
last <- run1$diagnostics[run1$diagnostics$radius_index == nrow(grid), , drop = FALSE]
stopifnot(all(first$n_balls == n), all(first$n_edges == 0L), all(last$n_balls == 1L))

stability <- tdabm_rd_pairwise_stability(run1$replicates, grid, n)
summary <- tdabm_rd_summarise(run1$diagnostics, stability)
stopifnot(nrow(stability) == nrow(grid), nrow(summary) == nrow(grid))
stopifnot(all(stability$pairwise_comparisons == choose(6L, 2L)))
stopifnot(all(stability$mean_pairwise_cover_jaccard >= 0 & stability$mean_pairwise_cover_jaccard <= 1))

resolution <- tdabm_rd_resolution_transition_report(
  summary = summary,
  n_observations = n,
  n_dimensions = ncol(prepared$axes),
  dominance_targets = c(0.40, 0.50, 0.60, 0.75)
)
stopifnot(nrow(resolution$reference) == 1L)
stopifnot(identical(resolution$reference$method[[1L]], "majority_cover_transition"))
stopifnot(is.finite(resolution$reference$reference_radius[[1L]]))
stopifnot(!isTRUE(resolution$reference$automatic_approval[[1L]]))
stopifnot(identical(resolution$reference$optimality_claim[[1L]], "NONE"))
stopifnot(all(c("median_effective_n_balls", "median_max_ball_share_observations", "majority_ball_rate") %in% names(summary)))
stopifnot(all(is.finite(summary$median_effective_n_balls)))
stopifnot(all(summary$median_max_ball_share_observations >= 0 & summary$median_max_ball_share_observations <= 1))

# Connectivity is descriptive only. Changing connected/no-isolate rates while
# preserving resolution diagnostics must leave the pipeline reference unchanged.
summary_connectivity_changed <- summary
summary_connectivity_changed$connected_rate <- rev(summary_connectivity_changed$connected_rate)
summary_connectivity_changed$no_isolate_rate <- rev(summary_connectivity_changed$no_isolate_rate)
resolution_connectivity_changed <- tdabm_rd_resolution_transition_report(
  summary = summary_connectivity_changed,
  n_observations = n,
  n_dimensions = ncol(prepared$axes),
  dominance_targets = c(0.40, 0.50, 0.60, 0.75)
)
stopifnot(isTRUE(all.equal(
  resolution$reference$reference_radius,
  resolution_connectivity_changed$reference$reference_radius,
  tolerance = 0
)))

inside_row <- summary[which.min(abs(log(summary$radius)-log(resolution$reference$reference_radius[[1L]]))),,drop=FALSE]
review <- tdabm_rd_user_radius_review(summary,inside_row$radius[[1L]],resolution$reference,ncol(prepared$axes))
stopifnot(!isTRUE(review$automatic_approval[[1L]]))
stopifnot(identical(review$approval_status[[1L]],"AWAITING_EXACT_RADIUS_EVIDENCE_AND_EXPLICIT_USER_DECISION"))
technical <- tdabm_rd_technical_user_radius_review(inside_row,review,severe_dominance_threshold=0.99)
stopifnot(is.list(technical),nrow(technical$integrity_checks)>=8L,nrow(technical$diagnostic_flags)>=5L,isTRUE(technical$technical_evidence_complete),identical(technical$status,"TECHNICAL_EVIDENCE_COMPLETE_USER_DECISION_EXTERNAL"),!isTRUE(technical$automatic_approval),identical(technical$optimality_claim,"NONE"))
below_candidates<-which(summary$radius<resolution$reference$review_band_lower[[1L]])
above_candidates<-which(summary$radius>resolution$reference$review_band_upper[[1L]])
stopifnot(length(below_candidates)>=1L,length(above_candidates)>=1L)
below_row<-summary[below_candidates[[1L]],,drop=FALSE]
above_row<-summary[tail(above_candidates,1L),,drop=FALSE]
below_review<-tdabm_rd_user_radius_review(summary,below_row$radius[[1L]],resolution$reference,ncol(prepared$axes))
above_review<-tdabm_rd_user_radius_review(summary,above_row$radius[[1L]],resolution$reference,ncol(prepared$axes))
below_technical<-tdabm_rd_technical_user_radius_review(below_row,below_review)
above_technical<-tdabm_rd_technical_user_radius_review(above_row,above_review)
stopifnot(isTRUE(below_technical$technical_evidence_complete),isTRUE(above_technical$technical_evidence_complete))
stopifnot(below_technical$diagnostic_flags$triggered[below_technical$diagnostic_flags$diagnostic=="outside_resolution_review_band"],above_technical$diagnostic_flags$triggered[above_technical$diagnostic_flags$diagnostic=="outside_resolution_review_band"])
stopifnot(all(below_technical$diagnostic_flags$blocking==FALSE),all(above_technical$diagnostic_flags$blocking==FALSE))
bad_exact<-above_row
bad_exact$mean_pairwise_cover_jaccard[[1L]]<-NA_real_
stopifnot(!tdabm_rd_technical_user_radius_review(bad_exact,above_review)$technical_evidence_complete)
mismatch<-above_row
mismatch$radius[[1L]]<-mismatch$radius[[1L]]*0.99
stopifnot(!tdabm_rd_technical_user_radius_review(mismatch,above_review)$technical_evidence_complete)

# The superseded connectedness-window API must fail loudly rather than silently
# recreating the P1.4-B v1.0 behaviour.
stopifnot(inherits(try(tdabm_rd_transition_report(summary), silent = TRUE), "try-error"))
stopifnot(inherits(try(tdabm_rd_candidate_shortlist(summary), silent = TRUE), "try-error"))

# Input row order must not change the deterministic diagnostic result because
# the generic module canonicalises by stable unit identifier before shuffling.
reordered <- synthetic[rev(seq_len(nrow(synthetic))), , drop = FALSE]
prepared_reordered <- tdabm_rd_prepare_topology(reordered, "unit_id", c("x1", "x2", "x3"), scaling = "as_supplied", metric = "euclidean")
stopifnot(identical(prepared$ids, prepared_reordered$ids))
stopifnot(isTRUE(all.equal(prepared$axes, prepared_reordered$axes, tolerance = 0, check.attributes = TRUE)))
run_reordered <- tdabm_rd_run_replicates(
  prepared_reordered$axes, prepared_reordered$ids, grid,
  repetitions = 6L, base_seed = 44001L, ncores = 1L,
  module_path = module_path, cpp_path = cpp_path
)
row_order_check <- tdabm_rd_compare_runs(run1$diagnostics, run_reordered$diagnostics, tolerance = 0)
stopifnot(all(row_order_check$pass))

# The public diagnostic runner has no outcome or colouring argument. Topology is
# the only analytical payload passed to the repeated-order engine.
formal_names <- names(formals(tdabm_rd_run_replicates))
stopifnot(!any(tolower(formal_names) %in% c("y", "yvar", "outcome", "colour", "color", "colour_variables")))

# Malformed topology must fail before any Ball Mapper execution.
bad <- synthetic
bad$x2[[1L]] <- NA_real_
failed_bad <- inherits(try(tdabm_rd_validate_topology_frame(bad, "unit_id", c("x1", "x2", "x3"), stop_on_failure = TRUE), silent = TRUE), "try-error")
stopifnot(failed_bad)

cat("PASS: packed topology signatures round-trip exactly for byte-aligned and non-byte-aligned bit lengths.\n")
cat("PASS: broad geometry-derived radius grid contains both sparse and one-ball collapse anchors.\n")
cat("PASS: repeated-order topology diagnostics are deterministic across one and two workers.\n")
cat("PASS: input row reordering does not alter diagnostics when stable unit identifiers are unchanged.\n")
cat("PASS: cover-relation and landmark stability are label-invariant and bounded in [0,1].\n")
cat("PASS: majority-cover transition provides a non-binding resolution reference without using connectivity as an admissibility rule.\n")
cat("PASS: 40%-60% review region, severe dominance and low-resolution states are diagnostic warnings rather than automatic radius gates.\n")
cat("PASS: exact-radius mismatch and malformed/non-finite technical evidence still fail closed.\n")
cat("PASS: radius-diagnostic API has no outcome/colour argument and malformed topology fails before execution.\n")
cat("PASS: P1.4-B topology-only radius diagnostic regression test complete.\n")
