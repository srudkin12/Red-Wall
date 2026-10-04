# ==============================================================================
# TDABMArtifactProvenance.R
# ==============================================================================
# Application-neutral file-copy provenance contracts for paper-output stages.
#
# Principles:
#   * copied artifacts remain attributable to one authoritative source;
#   * SHA-256 identity, not directory location, establishes copy continuity;
#   * every copy boundary may be validated independently and end-to-end;
#   * missing, altered, or ambiguously recorded files fail explicitly.
# ==============================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

tdabm_artifact_path <- function(path, must_work = FALSE) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(as.character(path))) {
    return(NA_character_)
  }
  expanded <- path.expand(as.character(path))
  if (file.exists(expanded) || dir.exists(expanded)) {
    return(normalizePath(expanded, winslash = "/", mustWork = must_work))
  }
  if (isTRUE(must_work)) {
    stop("Artifact path does not exist: ", expanded, call. = FALSE)
  }
  absolute <- grepl("^/", expanded) || grepl("^[A-Za-z]:", expanded)
  if (!absolute) expanded <- file.path(normalizePath(getwd(), winslash = "/", mustWork = TRUE), expanded)
  gsub("\\", "/", expanded, fixed = TRUE)
}

tdabm_artifact_sha256 <- function(file, require_file = TRUE) {
  file <- tdabm_artifact_path(file, must_work = FALSE)
  exists <- length(file) == 1L && !is.na(file) && file.exists(file) && !dir.exists(file)
  if (!exists) {
    if (isTRUE(require_file)) stop("Artifact file does not exist: ", file, call. = FALSE)
    return(NA_character_)
  }
  file <- tdabm_artifact_path(file, must_work = TRUE)

  if (requireNamespace("digest", quietly = TRUE)) {
    return(unname(digest::digest(file, algo = "sha256", file = TRUE)))
  }

  sha256sum <- Sys.which("sha256sum")
  if (nzchar(sha256sum)) {
    out <- system2(sha256sum, shQuote(file), stdout = TRUE, stderr = TRUE)
    status <- attr(out, "status")
    if (!is.null(status) && !identical(as.integer(status), 0L)) {
      stop("sha256sum failed for artifact: ", file, call. = FALSE)
    }
    if (!length(out)) stop("sha256sum returned no result for artifact: ", file, call. = FALSE)
    return(strsplit(out[[1L]], "[[:space:]]+")[[1L]][[1L]])
  }

  shasum <- Sys.which("shasum")
  if (nzchar(shasum)) {
    out <- system2(shasum, c("-a", "256", shQuote(file)), stdout = TRUE, stderr = TRUE)
    status <- attr(out, "status")
    if (!is.null(status) && !identical(as.integer(status), 0L)) {
      stop("shasum -a 256 failed for artifact: ", file, call. = FALSE)
    }
    if (!length(out)) stop("shasum returned no result for artifact: ", file, call. = FALSE)
    return(strsplit(out[[1L]], "[[:space:]]+")[[1L]][[1L]])
  }

  stop("No SHA-256 implementation is available. Install package 'digest' or provide sha256sum/shasum.", call. = FALSE)
}

tdabm_artifact_same_sha256 <- function(a, b) {
  length(a) == 1L && length(b) == 1L && !is.na(a) && !is.na(b) && nzchar(a) && nzchar(b) && identical(as.character(a), as.character(b))
}

tdabm_artifact_verify_chain <- function(
  authoritative_source_file,
  source_file = authoritative_source_file,
  destination_file = NULL,
  recorded_authoritative_sha256 = NULL,
  recorded_source_sha256 = NULL,
  recorded_destination_sha256 = NULL,
  require_destination = !is.null(destination_file),
  stop_on_failure = FALSE
) {
  auth_path <- tdabm_artifact_path(authoritative_source_file, must_work = FALSE)
  src_path <- tdabm_artifact_path(source_file, must_work = FALSE)
  dst_path <- if (is.null(destination_file)) NA_character_ else tdabm_artifact_path(destination_file, must_work = FALSE)

  auth_exists <- !is.na(auth_path) && file.exists(auth_path) && !dir.exists(auth_path)
  src_exists <- !is.na(src_path) && file.exists(src_path) && !dir.exists(src_path)
  dst_exists <- !is.na(dst_path) && file.exists(dst_path) && !dir.exists(dst_path)

  auth_sha <- if (auth_exists) tdabm_artifact_sha256(auth_path) else NA_character_
  src_sha <- if (src_exists) tdabm_artifact_sha256(src_path) else NA_character_
  dst_sha <- if (dst_exists) tdabm_artifact_sha256(dst_path) else NA_character_

  recorded_auth_ok <- is.null(recorded_authoritative_sha256) || tdabm_artifact_same_sha256(auth_sha, as.character(recorded_authoritative_sha256))
  recorded_src_ok <- is.null(recorded_source_sha256) || tdabm_artifact_same_sha256(src_sha, as.character(recorded_source_sha256))
  recorded_dst_ok <- is.null(recorded_destination_sha256) || tdabm_artifact_same_sha256(dst_sha, as.character(recorded_destination_sha256))
  source_matches_authoritative <- auth_exists && src_exists && tdabm_artifact_same_sha256(auth_sha, src_sha)
  destination_matches_source <- if (isTRUE(require_destination)) {
    src_exists && dst_exists && tdabm_artifact_same_sha256(src_sha, dst_sha)
  } else {
    TRUE
  }
  end_to_end <- auth_exists && src_exists && recorded_auth_ok && recorded_src_ok && recorded_dst_ok &&
    source_matches_authoritative && destination_matches_source && (!isTRUE(require_destination) || dst_exists)

  out <- data.frame(
    authoritative_source_file = auth_path,
    authoritative_source_exists = auth_exists,
    authoritative_source_sha256 = auth_sha,
    source_file = src_path,
    source_exists = src_exists,
    source_sha256 = src_sha,
    destination_file = dst_path,
    destination_exists = dst_exists,
    destination_sha256 = dst_sha,
    recorded_authoritative_sha256_matches = recorded_auth_ok,
    recorded_source_sha256_matches = recorded_src_ok,
    recorded_destination_sha256_matches = recorded_dst_ok,
    source_matches_authoritative = source_matches_authoritative,
    destination_matches_source = destination_matches_source,
    end_to_end_identity_verified = end_to_end,
    stringsAsFactors = FALSE
  )

  if (isTRUE(stop_on_failure) && !isTRUE(out$end_to_end_identity_verified[[1L]])) {
    failed <- names(out)[vapply(out, function(z) length(z) == 1L && is.logical(z) && !isTRUE(z), logical(1))]
    stop(
      "Artifact provenance chain failed: ",
      if (length(failed)) paste(failed, collapse = "; ") else "unknown identity failure",
      call. = FALSE
    )
  }

  out
}

tdabm_artifact_copy_with_provenance <- function(
  source_file,
  destination_file,
  authoritative_source_file = source_file,
  recorded_authoritative_sha256 = NULL,
  recorded_source_sha256 = NULL,
  overwrite = TRUE,
  source_stage = NA_character_,
  source_artifact_id = NA_character_,
  stop_on_failure = TRUE
) {
  source_file <- tdabm_artifact_path(source_file, must_work = TRUE)
  authoritative_source_file <- tdabm_artifact_path(authoritative_source_file, must_work = TRUE)
  destination_file <- tdabm_artifact_path(destination_file, must_work = FALSE)

  dir.create(dirname(destination_file), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(destination_file))) {
    stop("Could not create artifact destination directory: ", dirname(destination_file), call. = FALSE)
  }

  pre <- tdabm_artifact_verify_chain(
    authoritative_source_file = authoritative_source_file,
    source_file = source_file,
    recorded_authoritative_sha256 = recorded_authoritative_sha256,
    recorded_source_sha256 = recorded_source_sha256,
    require_destination = FALSE,
    stop_on_failure = stop_on_failure
  )

  copied <- isTRUE(file.copy(source_file, destination_file, overwrite = overwrite))
  if (!copied || !file.exists(destination_file)) {
    stop("Artifact copy failed: ", source_file, " -> ", destination_file, call. = FALSE)
  }

  post <- tdabm_artifact_verify_chain(
    authoritative_source_file = authoritative_source_file,
    source_file = source_file,
    destination_file = destination_file,
    recorded_authoritative_sha256 = pre$authoritative_source_sha256[[1L]],
    recorded_source_sha256 = pre$source_sha256[[1L]],
    require_destination = TRUE,
    stop_on_failure = stop_on_failure
  )

  post$copied <- copied
  post$source_stage <- as.character(source_stage)
  post$source_artifact_id <- as.character(source_artifact_id)
  post
}

tdabm_artifact_assert_provenance_table <- function(
  x,
  identity_column = "end_to_end_identity_verified",
  require_rows = TRUE,
  table_name = "artifact provenance table"
) {
  if (!is.data.frame(x)) stop(table_name, " must be a data.frame-like object.", call. = FALSE)
  if (isTRUE(require_rows) && nrow(x) == 0L) stop(table_name, " contains no rows.", call. = FALSE)
  if (!identity_column %in% names(x)) stop(table_name, " is missing column: ", identity_column, call. = FALSE)
  ok <- as.logical(x[[identity_column]])
  if (length(ok) != nrow(x) || anyNA(ok) || !all(ok)) {
    stop(table_name, " contains an unverified artifact copy.", call. = FALSE)
  }
  invisible(TRUE)
}
