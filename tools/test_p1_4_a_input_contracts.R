#!/usr/bin/env Rscript
options(warn = 2)

# Resolve repository robustly from --file when invoked with Rscript.
all_args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", all_args[grep("^--file=", all_args)])
if (length(file_arg) != 1L) stop("Could not resolve test script path.", call. = FALSE)
repo <- normalizePath(file.path(dirname(file_arg), ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo, "framework", "R", "TDABMArtifactProvenance.R"))
source(file.path(repo, "framework", "R", "TDABMValueContracts.R"))
source(file.path(repo, "framework", "R", "TDABMInputFreeze.R"))

expect_error <- function(expr, pattern = NULL) {
  caught <- NULL
  tryCatch(force(expr), error = function(e) caught <<- conditionMessage(e))
  if (is.null(caught)) stop("Expected an error but none was raised.", call. = FALSE)
  if (!is.null(pattern) && !grepl(pattern, caught, perl = TRUE)) {
    stop("Error did not match expected pattern. Error: ", caught, call. = FALSE)
  }
  invisible(caught)
}

tmp <- tempfile("TDABM P1.4-A generic contract test ")
dir.create(tmp, recursive = TRUE)
on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

x <- data.frame(
  unit_id = c("A-01", "B-02", "C-03", "D-04", "E-05"),
  unit_label = c("Alpha", "München", "Łódź", "Zürich", "Göteborg"),
  x1 = c(10, 20, 30, 40, 50),
  x2 = c(15, 25, 35, 45, 55),
  colour_a = c(60, 61, 62, 63, 64),
  colour_b = c(70, 71, 72, 73, 74),
  stringsAsFactors = FALSE
)

contract <- tdabm_input_contract(
  application_id = "synthetic_portability_test",
  id_col = "unit_id",
  label_col = "unit_label",
  topology_variables = c("x1", "x2"),
  colour_variables = c("colour_a", "colour_b"),
  scaling = "as_supplied",
  metric = "euclidean",
  expected_rows = 5L,
  numeric_lower = 0,
  numeric_upper = 100,
  require_nonconstant_topology = TRUE,
  approved_exclusions = "NONE",
  radius_status = "PENDING_USER_DECISION"
)

valid <- tdabm_validate_input_frame(x, contract, stop_on_failure = TRUE)
stopifnot(isTRUE(valid$pass), all(valid$report$pass %in% TRUE))
cat("PASS: generic input contract accepts string IDs, Unicode labels, finite topology and colour columns.\n")

candidate <- file.path(tmp, "candidate.csv")
utils::write.csv(x, candidate, row.names = FALSE, fileEncoding = "UTF-8")
frozen <- file.path(tmp, "freeze with spaces", "input.csv")
first <- tdabm_freeze_input_csv(candidate, frozen, contract, allow_existing = TRUE)
stopifnot(isTRUE(first$pass), identical(first$manifest$freeze_status[[1L]], "CREATED_NEW_FREEZE"))
second <- tdabm_freeze_input_csv(candidate, frozen, contract, allow_existing = TRUE)
stopifnot(isTRUE(second$pass), second$manifest$freeze_status[[1L]] %in% c("EXISTING_BYTE_IDENTICAL", "EXISTING_SEMANTICALLY_IDENTICAL"))
cat("PASS: canonical input freeze is atomic, non-overwriting, and idempotent for an existing valid freeze.\n")

reordered <- x[c(5, 1, 4, 2, 3), ]
strict_order <- tdabm_compare_input_frames(x, reordered, "unit_id", require_id_order = TRUE, stop_on_failure = FALSE)
order_invariant <- tdabm_compare_input_frames(x, reordered, "unit_id", require_id_order = FALSE, stop_on_failure = FALSE)
stopifnot(!isTRUE(strict_order$pass), isTRUE(order_invariant$pass))
cat("PASS: semantic comparison distinguishes strict row-order identity from key-aligned equality.\n")

bad <- x
bad$unit_id[[2L]] <- bad$unit_id[[1L]]
expect_error(tdabm_validate_input_frame(bad, contract, stop_on_failure = TRUE), "id_unique")
bad <- x
bad$unit_label[[2L]] <- ""
expect_error(tdabm_validate_input_frame(bad, contract, stop_on_failure = TRUE), "label_nonmissing_nonblank")
bad <- x
bad$x1[[2L]] <- Inf
expect_error(tdabm_validate_input_frame(bad, contract, stop_on_failure = TRUE), "finite:x1")
bad <- x
bad$x1[[2L]] <- 101
expect_error(tdabm_validate_input_frame(bad, contract, stop_on_failure = TRUE), "upper_bound:x1")
bad <- x
bad$x1 <- 1
expect_error(tdabm_validate_input_frame(bad, contract, stop_on_failure = TRUE), "topology_nonconstant:x1")
cat("PASS: duplicate IDs, blank labels, non-finite values, bounds violations and constant topology fail closed.\n")

source_file <- file.path(tmp, "source.txt")
writeLines(c("one", "two"), source_file, useBytes = TRUE)
manifest <- data.frame(
  relative_path = "source.txt",
  sha256 = tdabm_if_sha256(source_file),
  role = "canonical_source",
  immutable = "YES",
  stringsAsFactors = FALSE
)
verified <- tdabm_if_verify_manifest(manifest, tmp, require_immutable = TRUE, stop_on_failure = TRUE)
stopifnot(isTRUE(verified$pass))
write("three", source_file, append = TRUE)
expect_error(tdabm_if_verify_manifest(manifest, tmp, require_immutable = TRUE, stop_on_failure = TRUE), "failed verification")
cat("PASS: immutable source-manifest hashes are verified and post-manifest mutation is detected.\n")

stopifnot(identical(contract$radius_status, "PENDING_USER_DECISION"))
stopifnot(!exists("BallMapper", mode = "function"), !exists("BMStats", mode = "function"), !exists("BMStatsMembership", mode = "function"))
cat("PASS: P1.4-A generic dependency closure has no radius approval or analytical construction side effect.\n")
cat("PASS: P1.4-A generic input-freeze contract test complete.\n")
