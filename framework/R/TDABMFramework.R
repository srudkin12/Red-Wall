# ==============================================================================
# TDABMFramework.R
# ==============================================================================
# Core framework metadata and loading helpers for reusable TDABM projects.
#
# Stage 1 purpose:
#   This file deliberately contains no Ball Mapper execution code.  It defines
#   framework metadata, defaults, and helper functions used by the project object,
#   validation layer, and later execution modules.
#
# Recommended source order:
#   source("R/TDABMFramework.R")
#   source("R/TDABMUtilities.R")
#   source("R/TDABMProject.R")
# ==============================================================================

TDABMFrameworkVersion <- function() {
  "0.3.0-p1.4a"
}

TDABMFrameworkInfo <- function() {
  list(
    name = "TDABM reusable research framework",
    short_name = "TDABMFramework",
    version = TDABMFrameworkVersion(),
    stage = "Portability Stage P1.4-A: application-neutral source reconstruction and canonical input freeze",
    created_for = "Reusable Topological Data Analysis Ball Mapper workflows",
    execution_engine_available = TRUE
  )
}

TDABMDefaultSettings <- function() {
  list(
    radius = list(
      min = 0.50,
      max = 1.50,
      step = 0.01
    ),
    parallel = list(
      ncores = 1L,
      repetitions = 1000L,
      base_seed = 12345L,
      thresh2 = 4L,
      checkpoint_every = 500L
    ),
    outputs = list(
      subdirs = c(
        checkpoints = "checkpoints",
        verification = "verification",
        memberships = "memberships",
        summary_plots = "summary_plots",
        logs = "logs",
        tables = "tables",
        figures = "figures",
        cache = "cache"
      )
    )
  )
}

TDABMRequiredProjectSections <- function() {
  c(
    "metadata",
    "paths",
    "data",
    "topology",
    "colourings",
    "radius",
    "parallel",
    "outputs",
    "membership",
    "paper",
    "dependencies",
    "robustness",
    "history",
    "validation"
  )
}

TDABMStartupMessage <- function() {
  info <- TDABMFrameworkInfo()
  message(info$name)
  message("Version: ", info$version)
  message("Stage: ", info$stage)
  invisible(info)
}

tdabm_framework_file_order <- function() {
  c(
    "TDABMFramework.R",
    "TDABMUtilities.R",
    "TDABMValueContracts.R",
    "TDABMInputFreeze.R",
    "TDABMProject.R"
  )
}

source_tdabm_framework <- function(
  r_dir,
  files = tdabm_framework_file_order(),
  verbose = TRUE
) {
  if (missing(r_dir) || is.null(r_dir) || length(r_dir) != 1L) {
    stop("Provide the directory containing the TDABM R source files.", call. = FALSE)
  }

  r_dir <- normalizePath(r_dir, winslash = "/", mustWork = FALSE)

  if (!dir.exists(r_dir)) {
    stop("TDABM R source directory does not exist: ", r_dir, call. = FALSE)
  }

  for (f in files) {
    fpath <- file.path(r_dir, f)
    if (!file.exists(fpath)) {
      stop("TDABM source file not found: ", fpath, call. = FALSE)
    }
    if (isTRUE(verbose)) {
      message("Sourcing ", fpath)
    }
    source(fpath)
  }

  invisible(TRUE)
}

tdabm_framework_loaded <- function() {
  all(vapply(
    c("TDABMFrameworkVersion", "TDABMDefaultSettings"),
    exists,
    logical(1),
    mode = "function"
  ))
}
