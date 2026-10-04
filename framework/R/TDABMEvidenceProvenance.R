# ==============================================================================
# TDABMEvidenceProvenance.R
# ==============================================================================
# Application-neutral evidence registry and handover-copy contracts.
# ==============================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

tdabm_ep_sha256 <- function(path) {
  if (!file.exists(path) || dir.exists(path)) stop("Cannot hash missing/non-file path: ", path, call. = FALSE)
  if (requireNamespace("digest", quietly = TRUE)) return(tolower(unname(digest::digest(path, algo = "sha256", file = TRUE))))
  exe <- Sys.which("sha256sum")
  if (!nzchar(exe)) stop("SHA-256 requires digest or sha256sum.", call. = FALSE)
  out <- system2(exe, shQuote(path), stdout = TRUE, stderr = TRUE)
  if (length(out) != 1L) stop("Could not compute SHA-256.", call. = FALSE)
  tolower(strsplit(out, "[[:space:]]+")[[1L]][[1L]])
}

tdabm_ep_write_csv_atomic <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  utils::write.csv(x, tmp, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  if (!file.rename(tmp, path)) { unlink(tmp); stop("Could not atomically write ", path, call. = FALSE) }
  invisible(path)
}

tdabm_ep_write_lines_atomic <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  writeLines(enc2utf8(as.character(x)), tmp, useBytes = TRUE)
  if (!file.rename(tmp, path)) { unlink(tmp); stop("Could not atomically write ", path, call. = FALSE) }
  invisible(path)
}

tdabm_ep_registry <- function(paths, artifact_role, source_stage) {
  paths <- as.character(paths)
  artifact_role <- rep(as.character(artifact_role), length.out = length(paths))
  source_stage <- rep(as.character(source_stage), length.out = length(paths))
  exists <- file.exists(paths) & !dir.exists(paths)
  if (!all(exists)) stop("Evidence registry contains missing files.", call. = FALSE)
  data.frame(
    artifact = basename(paths),
    artifact_role = artifact_role,
    source_stage = source_stage,
    path = normalizePath(paths, winslash = "/", mustWork = TRUE),
    bytes = as.numeric(file.info(paths)$size),
    sha256 = vapply(paths, tdabm_ep_sha256, character(1)),
    stringsAsFactors = FALSE
  )
}

tdabm_ep_copy_verified <- function(source, destination, source_stage, artifact_role) {
  source <- normalizePath(source, winslash = "/", mustWork = TRUE)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(file.copy(source, destination, overwrite = TRUE))) stop("Could not copy evidence artifact.", call. = FALSE)
  src_sha <- tdabm_ep_sha256(source)
  dst_sha <- tdabm_ep_sha256(destination)
  if (!identical(src_sha, dst_sha)) stop("Evidence copy SHA-256 mismatch.", call. = FALSE)
  data.frame(
    source_file = source,
    source_sha256 = src_sha,
    destination_file = normalizePath(destination, winslash = "/", mustWork = TRUE),
    destination_sha256 = dst_sha,
    source_stage = source_stage,
    artifact_role = artifact_role,
    identity_verified = TRUE,
    stringsAsFactors = FALSE
  )
}
