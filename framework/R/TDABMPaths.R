# ==============================================================================
# TDABMPaths.R — portable path discovery (P1)
# ==============================================================================

tdabm_normalize_path <- function(path, must_work = FALSE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

tdabm_parent_paths <- function(path) {
  path <- tdabm_normalize_path(path, must_work = FALSE)
  out <- character(0)
  repeat {
    out <- c(out, path)
    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }
  unique(out)
}

tdabm_find_framework_root <- function(start = getwd(), required = TRUE) {
  env_root <- Sys.getenv("TDABM_FRAMEWORK_ROOT", unset = "")
  candidates <- c(env_root, tdabm_parent_paths(start))
  candidates <- unique(candidates[nzchar(candidates)])
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "R", "TDABMFramework.R"))) {
      return(tdabm_normalize_path(candidate, must_work = TRUE))
    }
    nested <- file.path(candidate, "framework")
    if (file.exists(file.path(nested, "R", "TDABMFramework.R"))) {
      return(tdabm_normalize_path(nested, must_work = TRUE))
    }
  }
  if (isTRUE(required)) {
    stop("Could not locate the TDABM framework. Set TDABM_FRAMEWORK_ROOT.", call. = FALSE)
  }
  NULL
}

tdabm_core_files <- function(include_summaries = TRUE) {
  files <- c(
    "TDABMFramework.R", "TDABMUtilities.R", "TDABMProject.R",
    "TDABMValidation.R", "TDABMExecution.R", "TDABMMembership.R",
    "TDABMRobustness.R", "TDABMOrchestrator.R"
  )
  if (isTRUE(include_summaries)) files <- c(files, "TDABMSummaries.R")
  files
}

tdabm_source_core <- function(framework_root = tdabm_find_framework_root(), include_summaries = TRUE, verbose = TRUE) {
  for (file in tdabm_core_files(include_summaries)) {
    path <- file.path(framework_root, "R", file)
    if (!file.exists(path)) stop("Missing framework source: ", path, call. = FALSE)
    if (isTRUE(verbose)) message("Sourcing ", path)
    source(path)
  }
  invisible(framework_root)
}
