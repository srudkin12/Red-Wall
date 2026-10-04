# ==============================================================================
# TDABMInputFreeze.R
# ==============================================================================
# Application-neutral source/input validation and canonical rectangular-input
# freeze contracts introduced for P1.4-A.
#
# Scope:
#   * verify immutable source manifests;
#   * validate reconstructed rectangular inputs;
#   * compare independent reconstructions semantically;
#   * freeze one validated input without overwriting a valid existing freeze;
#   * record cryptographic provenance and explicit analytical contracts.
#
# Explicit non-scope:
#   * no BallMapper construction;
#   * no BMStats/BMStatsMembership execution;
#   * no radius selection or approval;
#   * no observation exclusion beyond an application-declared reconstruction rule;
#   * no paper/manuscript production.
#
# No application-specific variable, label, sample size, radius, exclusion, or
# project path belongs in this module.
# ==============================================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) return(y)
  if (tryCatch(all(is.na(x)), error = function(e) FALSE)) return(y)
  x
}

# ------------------------------------------------------------------------------
# Scalar/config helpers
# ------------------------------------------------------------------------------

tdabm_if_assert_scalar_character <- function(x, name, allow_blank = FALSE) {
  if (length(x) != 1L || is.na(x)) {
    stop(name, " must be one non-missing character value.", call. = FALSE)
  }
  value <- as.character(x)
  if (!isTRUE(allow_blank) && !nzchar(trimws(value))) {
    stop(name, " must not be blank.", call. = FALSE)
  }
  value
}

tdabm_if_assert_scalar_logical <- function(x, name) {
  if (length(x) != 1L || is.na(x) || !is.logical(x)) {
    stop(name, " must be one non-missing logical value.", call. = FALSE)
  }
  isTRUE(x)
}

tdabm_if_assert_scalar_numeric <- function(x, name, minimum = NULL, maximum = NULL) {
  if (length(x) != 1L || is.na(x) || !is.numeric(x) || !is.finite(x)) {
    stop(name, " must be one finite numeric value.", call. = FALSE)
  }
  value <- as.numeric(x)
  if (!is.null(minimum) && value < minimum) stop(name, " is below its minimum.", call. = FALSE)
  if (!is.null(maximum) && value > maximum) stop(name, " is above its maximum.", call. = FALSE)
  value
}

tdabm_if_split_semicolon <- function(x, name = "value", allow_empty = FALSE) {
  value <- tdabm_if_assert_scalar_character(x, name, allow_blank = allow_empty)
  if (!nzchar(trimws(value))) return(character(0))
  out <- trimws(strsplit(value, ";", fixed = TRUE)[[1L]])
  if (any(!nzchar(out))) stop(name, " contains an empty semicolon-delimited element.", call. = FALSE)
  if (anyDuplicated(out)) stop(name, " contains duplicate elements.", call. = FALSE)
  out
}

tdabm_if_read_kv_config <- function(file) {
  file <- tdabm_if_assert_scalar_character(file, "file")
  if (!file.exists(file) || dir.exists(file)) stop("Config file not found: ", file, call. = FALSE)
  x <- utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  required <- c("key", "value")
  if (!all(required %in% names(x))) stop("Config must contain key and value columns.", call. = FALSE)
  if (nrow(x) == 0L) stop("Config contains no rows.", call. = FALSE)
  keys <- trimws(as.character(x$key))
  if (any(is.na(keys) | !nzchar(keys))) stop("Config contains a missing or blank key.", call. = FALSE)
  if (anyDuplicated(keys)) stop("Config contains duplicate keys: ", paste(unique(keys[duplicated(keys)]), collapse = "; "), call. = FALSE)
  values <- as.character(x$value)
  names(values) <- keys
  as.list(values)
}

tdabm_if_config_get <- function(config, key, allow_blank = FALSE) {
  if (!is.list(config)) stop("config must be a named list.", call. = FALSE)
  key <- tdabm_if_assert_scalar_character(key, "key")
  if (!key %in% names(config)) stop("Missing config key: ", key, call. = FALSE)
  tdabm_if_assert_scalar_character(config[[key]], paste0("config$", key), allow_blank = allow_blank)
}

# ------------------------------------------------------------------------------
# Path/hash helpers
# ------------------------------------------------------------------------------

tdabm_if_normalize_path <- function(path, root = NULL, must_exist = FALSE) {
  path <- tdabm_if_assert_scalar_character(path, "path")
  expanded <- path.expand(path)
  absolute <- grepl("^/", expanded) || grepl("^[A-Za-z]:", expanded)
  if (!absolute) {
    root <- root %||% getwd()
    root <- normalizePath(path.expand(root), winslash = "/", mustWork = TRUE)
    expanded <- file.path(root, expanded)
  }
  if (isTRUE(must_exist) && !file.exists(expanded) && !dir.exists(expanded)) {
    stop("Path does not exist: ", expanded, call. = FALSE)
  }
  normalizePath(expanded, winslash = "/", mustWork = FALSE)
}

tdabm_if_sha256 <- function(file) {
  file <- tdabm_if_normalize_path(file, must_exist = TRUE)
  if (dir.exists(file)) stop("SHA-256 requires a file, not a directory: ", file, call. = FALSE)

  if (exists("tdabm_artifact_sha256", mode = "function")) {
    return(tdabm_artifact_sha256(file))
  }

  if (requireNamespace("digest", quietly = TRUE)) {
    return(unname(digest::digest(file, algo = "sha256", file = TRUE)))
  }

  command <- Sys.which("sha256sum")
  if (nzchar(command)) {
    out <- system2(command, shQuote(file), stdout = TRUE, stderr = TRUE)
    status <- attr(out, "status")
    if (!is.null(status) && as.integer(status) != 0L) stop("sha256sum failed for: ", file, call. = FALSE)
    if (length(out) != 1L) stop("sha256sum returned an unexpected result for: ", file, call. = FALSE)
    return(strsplit(out[[1L]], "[[:space:]]+")[[1L]][[1L]])
  }

  command <- Sys.which("shasum")
  if (nzchar(command)) {
    out <- system2(command, c("-a", "256", shQuote(file)), stdout = TRUE, stderr = TRUE)
    status <- attr(out, "status")
    if (!is.null(status) && as.integer(status) != 0L) stop("shasum failed for: ", file, call. = FALSE)
    if (length(out) != 1L) stop("shasum returned an unexpected result for: ", file, call. = FALSE)
    return(strsplit(out[[1L]], "[[:space:]]+")[[1L]][[1L]])
  }

  stop("No SHA-256 implementation is available.", call. = FALSE)
}

tdabm_if_write_csv_atomic <- function(x, file) {
  if (!is.data.frame(x)) stop("x must be data.frame-compatible.", call. = FALSE)
  file <- tdabm_if_normalize_path(file, must_exist = FALSE)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(file))) stop("Could not create output directory: ", dirname(file), call. = FALSE)
  tmp <- tempfile(pattern = paste0(".", basename(file), "."), tmpdir = dirname(file))
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  utils::write.csv(x, tmp, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  if (file.exists(file)) unlink(file)
  if (!file.rename(tmp, file)) stop("Atomic CSV rename failed: ", file, call. = FALSE)
  invisible(file)
}

tdabm_if_write_lines_atomic <- function(lines, file) {
  file <- tdabm_if_normalize_path(file, must_exist = FALSE)
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(file))) stop("Could not create output directory: ", dirname(file), call. = FALSE)
  tmp <- tempfile(pattern = paste0(".", basename(file), "."), tmpdir = dirname(file))
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  writeLines(enc2utf8(as.character(lines)), con = tmp, useBytes = TRUE)
  if (file.exists(file)) unlink(file)
  if (!file.rename(tmp, file)) stop("Atomic text rename failed: ", file, call. = FALSE)
  invisible(file)
}

# ------------------------------------------------------------------------------
# Manifest verification
# ------------------------------------------------------------------------------

tdabm_if_verify_manifest <- function(
  manifest,
  root,
  required_roles = NULL,
  require_immutable = FALSE,
  stop_on_failure = TRUE,
  manifest_name = "file manifest"
) {
  if (is.character(manifest) && length(manifest) == 1L) {
    if (!file.exists(manifest)) stop("Manifest file not found: ", manifest, call. = FALSE)
    manifest <- utils::read.csv(manifest, stringsAsFactors = FALSE, check.names = FALSE)
  }
  if (!is.data.frame(manifest)) stop(manifest_name, " must be a data frame or CSV path.", call. = FALSE)
  x <- as.data.frame(manifest, stringsAsFactors = FALSE, check.names = FALSE)
  needed <- c("relative_path", "sha256")
  missing <- setdiff(needed, names(x))
  if (length(missing)) stop(manifest_name, " is missing columns: ", paste(missing, collapse = "; "), call. = FALSE)
  if (!is.null(required_roles)) {
    if (!"role" %in% names(x)) stop(manifest_name, " lacks role column required for role filtering.", call. = FALSE)
    x <- x[x$role %in% required_roles, , drop = FALSE]
  }
  if (nrow(x) == 0L) stop(manifest_name, " contains no rows to verify.", call. = FALSE)
  if (anyDuplicated(x$relative_path)) stop(manifest_name, " contains duplicate relative paths.", call. = FALSE)
  root <- tdabm_if_normalize_path(root, must_exist = TRUE)

  rows <- lapply(seq_len(nrow(x)), function(i) {
    rel <- as.character(x$relative_path[[i]])
    expected <- tolower(trimws(as.character(x$sha256[[i]])))
    path <- tdabm_if_normalize_path(rel, root = root, must_exist = FALSE)
    exists <- file.exists(path) && !dir.exists(path)
    observed <- if (exists) tdabm_if_sha256(path) else NA_character_
    immutable_ok <- TRUE
    if (isTRUE(require_immutable)) {
      if (!"immutable" %in% names(x)) {
        immutable_ok <- FALSE
      } else {
        flag <- toupper(trimws(as.character(x$immutable[[i]])))
        immutable_ok <- identical(flag, "YES") || identical(flag, "TRUE")
      }
    }
    data.frame(
      relative_path = rel,
      role = if ("role" %in% names(x)) as.character(x$role[[i]]) else NA_character_,
      exists = exists,
      expected_sha256 = expected,
      observed_sha256 = observed,
      sha256_match = exists && !is.na(observed) && identical(tolower(observed), expected),
      immutable_contract = immutable_ok,
      pass = exists && !is.na(observed) && identical(tolower(observed), expected) && immutable_ok,
      stringsAsFactors = FALSE
    )
  })
  report <- do.call(rbind, rows)
  pass <- nrow(report) > 0L && all(report$pass %in% TRUE)
  if (!pass && isTRUE(stop_on_failure)) {
    failed <- report[!report$pass, c("relative_path", "exists", "sha256_match", "immutable_contract"), drop = FALSE]
    stop(manifest_name, " failed verification: ", paste(capture.output(print(failed, row.names = FALSE)), collapse = " | "), call. = FALSE)
  }
  list(pass = pass, report = report)
}

# ------------------------------------------------------------------------------
# Input contract and validation
# ------------------------------------------------------------------------------

tdabm_input_contract <- function(
  application_id,
  id_col,
  label_col = NULL,
  topology_variables,
  colour_variables = character(0),
  scaling,
  metric,
  expected_rows = NULL,
  numeric_lower = NULL,
  numeric_upper = NULL,
  require_nonconstant_topology = TRUE,
  approved_exclusions = "NONE",
  radius_status = "PENDING_USER_DECISION"
) {
  application_id <- tdabm_if_assert_scalar_character(application_id, "application_id")
  id_col <- tdabm_if_assert_scalar_character(id_col, "id_col")
  if (!is.null(label_col)) label_col <- tdabm_if_assert_scalar_character(label_col, "label_col")
  topology_variables <- as.character(topology_variables)
  colour_variables <- as.character(colour_variables)
  if (!length(topology_variables) || any(is.na(topology_variables) | !nzchar(topology_variables))) {
    stop("topology_variables must contain at least one nonblank variable.", call. = FALSE)
  }
  if (anyDuplicated(topology_variables)) stop("topology_variables contains duplicates.", call. = FALSE)
  if (any(is.na(colour_variables) | !nzchar(colour_variables))) stop("colour_variables contains missing/blank values.", call. = FALSE)
  if (anyDuplicated(colour_variables)) stop("colour_variables contains duplicates.", call. = FALSE)
  if (length(intersect(topology_variables, colour_variables))) {
    stop("Topology and colour variables must be explicitly distinct in the input contract.", call. = FALSE)
  }
  scaling <- tdabm_if_assert_scalar_character(scaling, "scaling")
  metric <- tdabm_if_assert_scalar_character(metric, "metric")
  if (!is.null(expected_rows)) {
    expected_rows <- tdabm_if_assert_scalar_numeric(as.numeric(expected_rows), "expected_rows", minimum = 1)
    if (abs(expected_rows - round(expected_rows)) > 1e-8) stop("expected_rows must be integer-like.", call. = FALSE)
    expected_rows <- as.integer(round(expected_rows))
  }
  if (!is.null(numeric_lower)) numeric_lower <- tdabm_if_assert_scalar_numeric(as.numeric(numeric_lower), "numeric_lower")
  if (!is.null(numeric_upper)) numeric_upper <- tdabm_if_assert_scalar_numeric(as.numeric(numeric_upper), "numeric_upper")
  if (!is.null(numeric_lower) && !is.null(numeric_upper) && numeric_lower > numeric_upper) {
    stop("numeric_lower must not exceed numeric_upper.", call. = FALSE)
  }
  require_nonconstant_topology <- tdabm_if_assert_scalar_logical(as.logical(require_nonconstant_topology), "require_nonconstant_topology")
  approved_exclusions <- tdabm_if_assert_scalar_character(approved_exclusions, "approved_exclusions")
  radius_status <- tdabm_if_assert_scalar_character(radius_status, "radius_status")

  structure(list(
    application_id = application_id,
    id_col = id_col,
    label_col = label_col,
    topology_variables = topology_variables,
    colour_variables = colour_variables,
    scaling = scaling,
    metric = metric,
    expected_rows = expected_rows,
    numeric_lower = numeric_lower,
    numeric_upper = numeric_upper,
    require_nonconstant_topology = require_nonconstant_topology,
    approved_exclusions = approved_exclusions,
    radius_status = radius_status
  ), class = c("TDABMInputContract", "list"))
}

tdabm_validate_input_frame <- function(data, contract, stop_on_failure = TRUE) {
  if (!inherits(contract, "TDABMInputContract")) stop("contract must be a TDABMInputContract.", call. = FALSE)
  if (!is.data.frame(data)) stop("data must be data.frame-compatible.", call. = FALSE)
  x <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  rows <- list()
  add <- function(check, pass, detail) {
    rows[[length(rows) + 1L]] <<- data.frame(
      check = as.character(check),
      pass = isTRUE(pass),
      detail = as.character(detail),
      stringsAsFactors = FALSE
    )
  }

  required <- unique(c(contract$id_col, contract$label_col %||% character(0), contract$topology_variables, contract$colour_variables))
  missing <- setdiff(required, names(x))
  add("required_columns", length(missing) == 0L, if (length(missing)) paste(missing, collapse = "; ") else "all present")

  if (!is.null(contract$expected_rows)) {
    add("expected_rows", nrow(x) == contract$expected_rows, paste0("observed=", nrow(x), "; expected=", contract$expected_rows))
  } else {
    add("positive_rows", nrow(x) > 0L, paste0("observed=", nrow(x)))
  }

  if (contract$id_col %in% names(x)) {
    id <- as.character(x[[contract$id_col]])
    bad <- is.na(id) | !nzchar(trimws(id))
    add("id_nonmissing_nonblank", !any(bad), paste0("bad_rows=", sum(bad)))
    add("id_unique", !anyDuplicated(id), paste0("duplicate_rows=", sum(duplicated(id) | duplicated(id, fromLast = TRUE))))
    add("id_string_roundtrip", identical(id, as.character(id)), "identifiers evaluated as strings")
  }

  if (!is.null(contract$label_col) && contract$label_col %in% names(x)) {
    label <- as.character(x[[contract$label_col]])
    bad <- is.na(label) | !nzchar(trimws(label))
    add("label_nonmissing_nonblank", !any(bad), paste0("bad_rows=", sum(bad)))
    converted <- suppressWarnings(iconv(label, from = "UTF-8", to = "UTF-8", sub = NA_character_))
    add("label_utf8_convertible", !anyNA(converted), paste0("invalid_rows=", sum(is.na(converted))))
  }

  numeric_variables <- unique(c(contract$topology_variables, contract$colour_variables))
  for (nm in numeric_variables) {
    if (!nm %in% names(x)) next
    value <- x[[nm]]
    numeric_class <- is.numeric(value) || is.integer(value)
    add(paste0("numeric:", nm), numeric_class, paste0("class=", paste(class(value), collapse = "/")))
    if (!numeric_class) next
    finite <- is.finite(value)
    add(paste0("finite:", nm), all(finite), paste0("nonfinite=", sum(!finite)))
    if (!is.null(contract$numeric_lower)) {
      bad <- !finite | value < contract$numeric_lower
      add(paste0("lower_bound:", nm), !any(bad), paste0("below_or_nonfinite=", sum(bad), "; lower=", contract$numeric_lower))
    }
    if (!is.null(contract$numeric_upper)) {
      bad <- !finite | value > contract$numeric_upper
      add(paste0("upper_bound:", nm), !any(bad), paste0("above_or_nonfinite=", sum(bad), "; upper=", contract$numeric_upper))
    }
  }

  if (isTRUE(contract$require_nonconstant_topology)) {
    for (nm in contract$topology_variables) {
      if (!nm %in% names(x) || !(is.numeric(x[[nm]]) || is.integer(x[[nm]]))) next
      finite <- x[[nm]][is.finite(x[[nm]])]
      varying <- length(finite) > 1L && length(unique(finite)) > 1L
      add(paste0("topology_nonconstant:", nm), varying, paste0("unique_finite=", length(unique(finite))))
    }
  }

  add("approved_exclusions_recorded", nzchar(contract$approved_exclusions), contract$approved_exclusions)
  add("radius_status_recorded", nzchar(contract$radius_status), contract$radius_status)

  report <- if (length(rows)) do.call(rbind, rows) else data.frame(check = character(), pass = logical(), detail = character())
  pass <- nrow(report) > 0L && all(report$pass %in% TRUE)
  if (!pass && isTRUE(stop_on_failure)) {
    failed <- report[!report$pass, , drop = FALSE]
    stop("Input frame failed validation: ", paste0(failed$check, " [", failed$detail, "]", collapse = " | "), call. = FALSE)
  }
  list(pass = pass, report = report, data = x)
}

# ------------------------------------------------------------------------------
# Semantic comparison
# ------------------------------------------------------------------------------

tdabm_compare_input_frames <- function(
  reference,
  candidate,
  id_col,
  numeric_tolerance = 1e-8,
  require_column_order = TRUE,
  require_id_order = TRUE,
  stop_on_failure = FALSE
) {
  if (!is.data.frame(reference) || !is.data.frame(candidate)) stop("reference and candidate must be data frames.", call. = FALSE)
  ref <- as.data.frame(reference, stringsAsFactors = FALSE, check.names = FALSE)
  cand <- as.data.frame(candidate, stringsAsFactors = FALSE, check.names = FALSE)
  id_col <- tdabm_if_assert_scalar_character(id_col, "id_col")
  numeric_tolerance <- tdabm_if_assert_scalar_numeric(as.numeric(numeric_tolerance), "numeric_tolerance", minimum = 0)
  if (!id_col %in% names(ref) || !id_col %in% names(cand)) stop("id_col is absent from one comparison frame.", call. = FALSE)

  ref_id <- as.character(ref[[id_col]])
  cand_id <- as.character(cand[[id_col]])
  id_set_equal <- setequal(ref_id, cand_id) && length(ref_id) == length(cand_id)
  id_order_equal <- identical(ref_id, cand_id)
  column_set_equal <- setequal(names(ref), names(cand)) && length(names(ref)) == length(names(cand))
  column_order_equal <- identical(names(ref), names(cand))

  rows <- list(
    data.frame(check = "row_count", pass = nrow(ref) == nrow(cand), detail = paste0("reference=", nrow(ref), "; candidate=", nrow(cand)), stringsAsFactors = FALSE),
    data.frame(check = "column_set", pass = column_set_equal, detail = if (column_set_equal) "equal" else "different", stringsAsFactors = FALSE),
    data.frame(check = "column_order", pass = !isTRUE(require_column_order) || column_order_equal, detail = if (column_order_equal) "equal" else "different", stringsAsFactors = FALSE),
    data.frame(check = "id_set", pass = id_set_equal, detail = if (id_set_equal) "equal" else "different", stringsAsFactors = FALSE),
    data.frame(check = "id_order", pass = !isTRUE(require_id_order) || id_order_equal, detail = if (id_order_equal) "equal" else "different", stringsAsFactors = FALSE)
  )

  column_report <- data.frame(
    column = character(0),
    type = character(0),
    compared_rows = integer(0),
    mismatch_count = integer(0),
    max_abs_difference = numeric(0),
    pass = logical(0),
    stringsAsFactors = FALSE
  )

  if (column_set_equal && id_set_equal && nrow(ref) == nrow(cand)) {
    cand_index <- match(ref_id, cand_id)
    aligned <- cand[cand_index, names(ref), drop = FALSE]
    for (nm in names(ref)) {
      a <- ref[[nm]]
      b <- aligned[[nm]]
      numeric_pair <- (is.numeric(a) || is.integer(a)) && (is.numeric(b) || is.integer(b))
      if (numeric_pair) {
        same_missing <- identical(is.na(a), is.na(b))
        finite_pair <- is.finite(a) & is.finite(b)
        diffs <- abs(as.numeric(a[finite_pair]) - as.numeric(b[finite_pair]))
        maxdiff <- if (length(diffs)) max(diffs) else 0
        nonfinite_equal <- identical(a[!finite_pair], b[!finite_pair])
        mismatch <- if (!same_missing || !nonfinite_equal) length(a) else sum(diffs > numeric_tolerance)
        pass_col <- same_missing && nonfinite_equal && mismatch == 0L
        column_report <- rbind(column_report, data.frame(
          column = nm,
          type = "numeric",
          compared_rows = length(a),
          mismatch_count = as.integer(mismatch),
          max_abs_difference = as.numeric(maxdiff),
          pass = pass_col,
          stringsAsFactors = FALSE
        ))
      } else {
        aa <- as.character(a)
        bb <- as.character(b)
        same <- (is.na(aa) & is.na(bb)) | (!is.na(aa) & !is.na(bb) & aa == bb)
        mismatch <- sum(!same)
        column_report <- rbind(column_report, data.frame(
          column = nm,
          type = "character",
          compared_rows = length(a),
          mismatch_count = as.integer(mismatch),
          max_abs_difference = NA_real_,
          pass = mismatch == 0L,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  column_pass <- nrow(column_report) == ncol(ref) && all(column_report$pass %in% TRUE)
  structural_pass <- all(vapply(rows, function(z) isTRUE(z$pass[[1L]]), logical(1)))
  pass <- structural_pass && column_pass
  report <- do.call(rbind, rows)
  if (!pass && isTRUE(stop_on_failure)) {
    bad_cols <- column_report[!column_report$pass, , drop = FALSE]
    stop(
      "Input semantic comparison failed. Structural failures: ",
      paste(report$check[!report$pass], collapse = "; "),
      if (nrow(bad_cols)) paste0("; column failures: ", paste(bad_cols$column, collapse = "; ")) else "",
      call. = FALSE
    )
  }
  list(pass = pass, report = report, column_report = column_report)
}

# ------------------------------------------------------------------------------
# Canonical freeze
# ------------------------------------------------------------------------------

tdabm_freeze_input_csv <- function(
  candidate_file,
  frozen_file,
  contract,
  metadata = list(),
  numeric_tolerance = 1e-8,
  allow_existing = TRUE
) {
  candidate_file <- tdabm_if_normalize_path(candidate_file, must_exist = TRUE)
  frozen_file <- tdabm_if_normalize_path(frozen_file, must_exist = FALSE)
  candidate <- utils::read.csv(candidate_file, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  validation <- tdabm_validate_input_frame(candidate, contract, stop_on_failure = TRUE)
  candidate_sha <- tdabm_if_sha256(candidate_file)

  dir.create(dirname(frozen_file), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(frozen_file))) stop("Could not create freeze directory: ", dirname(frozen_file), call. = FALSE)

  freeze_status <- NA_character_
  if (file.exists(frozen_file)) {
    if (!isTRUE(allow_existing)) stop("Frozen input already exists and overwrite is prohibited: ", frozen_file, call. = FALSE)
    existing <- utils::read.csv(frozen_file, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
    comparison <- tdabm_compare_input_frames(existing, candidate, contract$id_col, numeric_tolerance = numeric_tolerance, stop_on_failure = TRUE)
    freeze_status <- if (identical(tdabm_if_sha256(frozen_file), candidate_sha)) "EXISTING_BYTE_IDENTICAL" else "EXISTING_SEMANTICALLY_IDENTICAL"
  } else {
    tmp <- tempfile(pattern = paste0(".", basename(frozen_file), "."), tmpdir = dirname(frozen_file))
    on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
    if (!file.copy(candidate_file, tmp, overwrite = FALSE, copy.mode = TRUE, copy.date = FALSE)) {
      stop("Could not stage candidate input for freezing.", call. = FALSE)
    }
    staged_sha <- tdabm_if_sha256(tmp)
    if (!identical(staged_sha, candidate_sha)) stop("Staged input hash differs from candidate hash.", call. = FALSE)
    if (!file.rename(tmp, frozen_file)) stop("Atomic freeze rename failed: ", frozen_file, call. = FALSE)
    freeze_status <- "CREATED_NEW_FREEZE"
  }

  frozen_sha <- tdabm_if_sha256(frozen_file)
  frozen <- utils::read.csv(frozen_file, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  frozen_validation <- tdabm_validate_input_frame(frozen, contract, stop_on_failure = TRUE)
  comparison <- tdabm_compare_input_frames(frozen, candidate, contract$id_col, numeric_tolerance = numeric_tolerance, stop_on_failure = TRUE)

  meta_value <- function(name, default = NA_character_) {
    value <- metadata[[name]] %||% default
    if (length(value) == 0L) default else as.character(value[[1L]])
  }

  manifest <- data.frame(
    application_id = contract$application_id,
    freeze_status = freeze_status,
    candidate_file = candidate_file,
    candidate_sha256 = candidate_sha,
    frozen_file = frozen_file,
    frozen_sha256 = frozen_sha,
    byte_identity = identical(candidate_sha, frozen_sha),
    semantic_identity = comparison$pass,
    validation_pass = frozen_validation$pass,
    rows = nrow(frozen),
    columns = ncol(frozen),
    id_col = contract$id_col,
    label_col = contract$label_col %||% NA_character_,
    topology_variables = paste(contract$topology_variables, collapse = ";"),
    colour_variables = paste(contract$colour_variables, collapse = ";"),
    scaling = contract$scaling,
    metric = contract$metric,
    approved_exclusions = contract$approved_exclusions,
    radius_status = contract$radius_status,
    source_pack_sha256 = meta_value("source_pack_sha256"),
    parent_framework_sha256 = meta_value("parent_framework_sha256"),
    created_at = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  list(
    pass = isTRUE(manifest$semantic_identity[[1L]]) && isTRUE(manifest$validation_pass[[1L]]),
    manifest = manifest,
    validation = frozen_validation$report,
    comparison = comparison$report,
    column_comparison = comparison$column_report,
    frozen_data = frozen
  )
}

# ------------------------------------------------------------------------------
# Session provenance
# ------------------------------------------------------------------------------

tdabm_if_session_lines <- function() {
  c(
    paste0("R.version.string: ", R.version.string),
    paste0("Platform: ", R.version$platform),
    paste0("Working directory: ", normalizePath(getwd(), winslash = "/", mustWork = TRUE)),
    "",
    capture.output(utils::sessionInfo())
  )
}
