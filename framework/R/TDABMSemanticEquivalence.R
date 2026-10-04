# TDABMSemanticEquivalence.R
#
# Semantic comparison utilities for TDABM production roots.
#
# Design principles:
#   * compare analytical content rather than raw RDS byte identity;
#   * ignore declared runtime-only fields such as worker count and timestamps;
#   * preserve an explicit audit trail for every compared run and component;
#   * never modify either production root;
#   * allow project adapters to mark invalid or intentionally non-comparable runs.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

tdabm_p12_timestamp <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

tdabm_p12_normalize_path <- function(path, must_work = TRUE) {
  normalizePath(path.expand(path), winslash = "/", mustWork = must_work)
}

tdabm_p12_relative_path <- function(path, root) {
  path <- tdabm_p12_normalize_path(path, must_work = TRUE)
  root <- tdabm_p12_normalize_path(root, must_work = TRUE)
  prefix <- paste0(root, "/")
  if (!startsWith(path, prefix)) {
    stop("Path is not inside root: ", path, call. = FALSE)
  }
  substring(path, nchar(prefix) + 1L)
}

tdabm_p12_hash_file <- function(path) {
  path <- tdabm_p12_normalize_path(path, must_work = TRUE)

  if (requireNamespace("digest", quietly = TRUE)) {
    return(data.frame(
      algorithm = "sha256",
      hash = digest::digest(file = path, algo = "sha256"),
      stringsAsFactors = FALSE
    ))
  }

  sha_cmd <- Sys.which("sha256sum")
  if (nzchar(sha_cmd)) {
    out <- system2(sha_cmd, shQuote(path), stdout = TRUE, stderr = TRUE)
    status <- attr(out, "status") %||% 0L
    if (identical(as.integer(status), 0L) && length(out) > 0L) {
      return(data.frame(
        algorithm = "sha256",
        hash = strsplit(out[[1L]], "[[:space:]]+")[[1L]][[1L]],
        stringsAsFactors = FALSE
      ))
    }
  }

  data.frame(
    algorithm = "md5",
    hash = unname(tools::md5sum(path)),
    stringsAsFactors = FALSE
  )
}

tdabm_p12_write_csv <- function(x, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(as.data.frame(x), file, row.names = FALSE, na = "")
  invisible(file)
}

tdabm_p12_write_lines <- function(x, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(x), con = file, useBytes = TRUE)
  invisible(file)
}

tdabm_p12_empty_detail <- function() {
  data.frame(
    run_path = character(),
    run_type = character(),
    component = character(),
    column = character(),
    status = character(),
    reference_type = character(),
    candidate_type = character(),
    n_reference = integer(),
    n_candidate = integer(),
    differing_cells = numeric(),
    max_abs_diff = numeric(),
    max_rel_diff = numeric(),
    note = character(),
    stringsAsFactors = FALSE
  )
}

tdabm_p12_atomic_type <- function(x) {
  if (is.factor(x)) return("factor")
  if (inherits(x, "POSIXt")) return("datetime")
  if (inherits(x, "Date")) return("date")
  if (is.integer(x)) return("integer")
  if (is.numeric(x)) return("numeric")
  if (is.logical(x)) return("logical")
  if (is.character(x)) return("character")
  if (is.list(x)) return("list")
  class(x)[[1L]] %||% typeof(x)
}

tdabm_p12_as_character_safe <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (inherits(x, "POSIXt")) return(format(x, tz = "UTC", usetz = TRUE))
  if (inherits(x, "Date")) return(as.character(x))
  if (is.list(x)) {
    return(vapply(x, function(z) paste(capture.output(dput(z)), collapse = ""), character(1)))
  }
  as.character(x)
}

tdabm_p12_compare_vector <- function(
  reference,
  candidate,
  abs_tol = 1e-12,
  rel_tol = 1e-10
) {
  n_ref <- length(reference)
  n_can <- length(candidate)
  ref_type <- tdabm_p12_atomic_type(reference)
  can_type <- tdabm_p12_atomic_type(candidate)

  if (n_ref != n_can) {
    return(list(
      status = "FAIL",
      exact = FALSE,
      within_tolerance = FALSE,
      reference_type = ref_type,
      candidate_type = can_type,
      n_reference = n_ref,
      n_candidate = n_can,
      differing_cells = abs(n_ref - n_can),
      max_abs_diff = NA_real_,
      max_rel_diff = NA_real_,
      note = "Vector lengths differ."
    ))
  }

  numeric_like <- (is.numeric(reference) || is.integer(reference)) &&
    (is.numeric(candidate) || is.integer(candidate))

  if (numeric_like) {
    a <- as.numeric(reference)
    b <- as.numeric(candidate)

    same_na <- identical(is.na(a), is.na(b))
    same_nan <- identical(is.nan(a), is.nan(b))
    same_inf <- identical(is.infinite(a), is.infinite(b)) &&
      identical(sign(a[is.infinite(a)]), sign(b[is.infinite(b)]))

    finite <- is.finite(a) & is.finite(b)
    abs_diff <- rep(NA_real_, length(a))
    rel_diff <- rep(NA_real_, length(a))
    abs_diff[finite] <- abs(a[finite] - b[finite])
    scale <- pmax(abs(a[finite]), abs(b[finite]), 1)
    rel_diff[finite] <- abs_diff[finite] / scale

    mismatch_special <- (!same_na) || (!same_nan) || (!same_inf)
    tolerance_pass <- rep(TRUE, length(a))
    tolerance_pass[finite] <- abs_diff[finite] <=
      (abs_tol + rel_tol * pmax(abs(a[finite]), abs(b[finite])))

    differing <- sum(!tolerance_pass, na.rm = TRUE)
    if (mismatch_special) differing <- differing + 1L

    max_abs <- if (any(finite)) max(abs_diff[finite], na.rm = TRUE) else 0
    max_rel <- if (any(finite)) max(rel_diff[finite], na.rm = TRUE) else 0

    exact <- !mismatch_special && all(abs_diff[finite] == 0)
    within <- !mismatch_special && all(tolerance_pass)
    status <- if (exact) "EXACT" else if (within) "TOLERANCE_EQUIVALENT" else "FAIL"

    return(list(
      status = status,
      exact = exact,
      within_tolerance = within,
      reference_type = ref_type,
      candidate_type = can_type,
      n_reference = n_ref,
      n_candidate = n_can,
      differing_cells = differing,
      max_abs_diff = max_abs,
      max_rel_diff = max_rel,
      note = if (ref_type == can_type) "" else "Storage types differ; numeric values were compared."
    ))
  }

  a <- tdabm_p12_as_character_safe(reference)
  b <- tdabm_p12_as_character_safe(candidate)
  same_na <- identical(is.na(reference), is.na(candidate))
  equal <- same_na && identical(a, b)

  list(
    status = if (equal) "EXACT" else "FAIL",
    exact = equal,
    within_tolerance = equal,
    reference_type = ref_type,
    candidate_type = can_type,
    n_reference = n_ref,
    n_candidate = n_can,
    differing_cells = if (equal) 0 else sum((a != b) | xor(is.na(reference), is.na(candidate)), na.rm = TRUE),
    max_abs_diff = NA_real_,
    max_rel_diff = NA_real_,
    note = if (ref_type == can_type) "" else "Values were compared after safe character conversion."
  )
}

tdabm_p12_sort_data_frame <- function(x, keys) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  row.names(x) <- NULL
  keys <- intersect(keys, names(x))

  if (nrow(x) <= 1L || length(keys) == 0L) return(x)

  order_args <- lapply(keys, function(key) {
    value <- x[[key]]
    if (is.factor(value)) value <- as.character(value)
    value
  })
  order_args$na.last <- TRUE
  idx <- do.call(order, order_args)
  x[idx, , drop = FALSE]
}

tdabm_p12_component_keys <- function(component) {
  switch(
    component,
    m001 = c("eps", "rep"),
    xv01 = c("eps", "rep"),
    m001s = c("eps"),
    membership_long = c("eps", "rep", "original_id", "ball"),
    point_repetitions = c("eps", "rep", "original_id"),
    authority_summary = c("eps", "original_id"),
    isolation_summary = c("eps", "original_id"),
    pairwise_comembership = c("eps", "id1", "id2"),
    graph_repetitions = c("eps", "rep"),
    errors = c("eps", "rep", "message"),
    character(0)
  )
}

tdabm_p12_compare_data_frame <- function(
  reference,
  candidate,
  component,
  run_path,
  run_type,
  abs_tol = 1e-12,
  rel_tol = 1e-10,
  structure_only = FALSE
) {
  if (is.null(reference) && is.null(candidate)) {
    summary <- data.frame(
      run_path = run_path,
      run_type = run_type,
      component = component,
      status = "EXACT",
      nrow_reference = 0L,
      nrow_candidate = 0L,
      ncol_reference = 0L,
      ncol_candidate = 0L,
      differing_cells = 0,
      max_abs_diff = 0,
      max_rel_diff = 0,
      note = "Both components are NULL.",
      stringsAsFactors = FALSE
    )
    return(list(summary = summary, details = tdabm_p12_empty_detail()))
  }

  if (is.null(reference) || is.null(candidate)) {
    summary <- data.frame(
      run_path = run_path,
      run_type = run_type,
      component = component,
      status = "FAIL",
      nrow_reference = if (is.null(reference)) 0L else nrow(as.data.frame(reference)),
      nrow_candidate = if (is.null(candidate)) 0L else nrow(as.data.frame(candidate)),
      ncol_reference = if (is.null(reference)) 0L else ncol(as.data.frame(reference)),
      ncol_candidate = if (is.null(candidate)) 0L else ncol(as.data.frame(candidate)),
      differing_cells = NA_real_,
      max_abs_diff = NA_real_,
      max_rel_diff = NA_real_,
      note = "One component is NULL and the other is not.",
      stringsAsFactors = FALSE
    )
    return(list(summary = summary, details = tdabm_p12_empty_detail()))
  }

  ref <- as.data.frame(reference, stringsAsFactors = FALSE)
  can <- as.data.frame(candidate, stringsAsFactors = FALSE)
  row.names(ref) <- NULL
  row.names(can) <- NULL

  ref_names <- names(ref)
  can_names <- names(can)
  same_columns <- setequal(ref_names, can_names)

  if (!same_columns) {
    missing_in_candidate <- setdiff(ref_names, can_names)
    extra_in_candidate <- setdiff(can_names, ref_names)
    note <- paste0(
      "Column sets differ. Missing in candidate: ",
      paste(missing_in_candidate, collapse = ", "),
      "; extra in candidate: ",
      paste(extra_in_candidate, collapse = ", "),
      "."
    )
    summary <- data.frame(
      run_path = run_path,
      run_type = run_type,
      component = component,
      status = "FAIL",
      nrow_reference = nrow(ref),
      nrow_candidate = nrow(can),
      ncol_reference = ncol(ref),
      ncol_candidate = ncol(can),
      differing_cells = NA_real_,
      max_abs_diff = NA_real_,
      max_rel_diff = NA_real_,
      note = note,
      stringsAsFactors = FALSE
    )
    return(list(summary = summary, details = tdabm_p12_empty_detail()))
  }

  can <- can[, ref_names, drop = FALSE]
  keys <- tdabm_p12_component_keys(component)
  ref <- tdabm_p12_sort_data_frame(ref, keys)
  can <- tdabm_p12_sort_data_frame(can, keys)

  if (nrow(ref) != nrow(can)) {
    summary <- data.frame(
      run_path = run_path,
      run_type = run_type,
      component = component,
      status = "FAIL",
      nrow_reference = nrow(ref),
      nrow_candidate = nrow(can),
      ncol_reference = ncol(ref),
      ncol_candidate = ncol(can),
      differing_cells = abs(nrow(ref) - nrow(can)),
      max_abs_diff = NA_real_,
      max_rel_diff = NA_real_,
      note = "Row counts differ.",
      stringsAsFactors = FALSE
    )
    return(list(summary = summary, details = tdabm_p12_empty_detail()))
  }

  if (isTRUE(structure_only)) {
    type_equal <- identical(
      vapply(ref, tdabm_p12_atomic_type, character(1)),
      vapply(can, tdabm_p12_atomic_type, character(1))
    )
    status <- if (type_equal) "STRUCTURE_ONLY" else "FAIL"
    summary <- data.frame(
      run_path = run_path,
      run_type = run_type,
      component = component,
      status = status,
      nrow_reference = nrow(ref),
      nrow_candidate = nrow(can),
      ncol_reference = ncol(ref),
      ncol_candidate = ncol(can),
      differing_cells = NA_real_,
      max_abs_diff = NA_real_,
      max_rel_diff = NA_real_,
      note = "Large raw membership component: dimensions, columns and storage types only.",
      stringsAsFactors = FALSE
    )
    return(list(summary = summary, details = tdabm_p12_empty_detail()))
  }

  details <- lapply(ref_names, function(column) {
    comparison <- tdabm_p12_compare_vector(
      ref[[column]],
      can[[column]],
      abs_tol = abs_tol,
      rel_tol = rel_tol
    )

    data.frame(
      run_path = run_path,
      run_type = run_type,
      component = component,
      column = column,
      status = comparison$status,
      reference_type = comparison$reference_type,
      candidate_type = comparison$candidate_type,
      n_reference = comparison$n_reference,
      n_candidate = comparison$n_candidate,
      differing_cells = comparison$differing_cells,
      max_abs_diff = comparison$max_abs_diff,
      max_rel_diff = comparison$max_rel_diff,
      note = comparison$note,
      stringsAsFactors = FALSE
    )
  })
  details <- do.call(rbind, details)

  status <- if (any(details$status == "FAIL")) {
    "FAIL"
  } else if (any(details$status == "TOLERANCE_EQUIVALENT")) {
    "TOLERANCE_EQUIVALENT"
  } else {
    "EXACT"
  }

  finite_abs <- details$max_abs_diff[is.finite(details$max_abs_diff)]
  finite_rel <- details$max_rel_diff[is.finite(details$max_rel_diff)]

  summary <- data.frame(
    run_path = run_path,
    run_type = run_type,
    component = component,
    status = status,
    nrow_reference = nrow(ref),
    nrow_candidate = nrow(can),
    ncol_reference = ncol(ref),
    ncol_candidate = ncol(can),
    differing_cells = sum(details$differing_cells, na.rm = TRUE),
    max_abs_diff = if (length(finite_abs)) max(finite_abs) else NA_real_,
    max_rel_diff = if (length(finite_rel)) max(finite_rel) else NA_real_,
    note = if (length(keys) && all(keys %in% ref_names)) {
      paste0("Rows aligned by: ", paste(keys, collapse = ", "), ".")
    } else {
      "Rows compared in stored order."
    },
    stringsAsFactors = FALSE
  )

  list(summary = summary, details = details)
}

tdabm_p12_compare_named_list <- function(
  reference,
  candidate,
  component,
  run_path,
  run_type,
  ignore_fields = character(0),
  abs_tol = 1e-12,
  rel_tol = 1e-10
) {
  reference <- reference %||% list()
  candidate <- candidate %||% list()

  fields <- sort(union(names(reference), names(candidate)))
  compared_fields <- setdiff(fields, ignore_fields)
  ignored_fields <- intersect(fields, ignore_fields)

  details <- lapply(compared_fields, function(field) {
    if (!field %in% names(reference) || !field %in% names(candidate)) {
      return(data.frame(
        run_path = run_path,
        run_type = run_type,
        component = component,
        column = field,
        status = "FAIL",
        reference_type = if (field %in% names(reference)) tdabm_p12_atomic_type(reference[[field]]) else "missing",
        candidate_type = if (field %in% names(candidate)) tdabm_p12_atomic_type(candidate[[field]]) else "missing",
        n_reference = if (field %in% names(reference)) length(reference[[field]]) else 0L,
        n_candidate = if (field %in% names(candidate)) length(candidate[[field]]) else 0L,
        differing_cells = 1,
        max_abs_diff = NA_real_,
        max_rel_diff = NA_real_,
        note = "Field is missing from one object.",
        stringsAsFactors = FALSE
      ))
    }

    comparison <- tdabm_p12_compare_vector(
      reference[[field]],
      candidate[[field]],
      abs_tol = abs_tol,
      rel_tol = rel_tol
    )

    data.frame(
      run_path = run_path,
      run_type = run_type,
      component = component,
      column = field,
      status = comparison$status,
      reference_type = comparison$reference_type,
      candidate_type = comparison$candidate_type,
      n_reference = comparison$n_reference,
      n_candidate = comparison$n_candidate,
      differing_cells = comparison$differing_cells,
      max_abs_diff = comparison$max_abs_diff,
      max_rel_diff = comparison$max_rel_diff,
      note = comparison$note,
      stringsAsFactors = FALSE
    )
  })

  details <- if (length(details)) do.call(rbind, details) else tdabm_p12_empty_detail()

  ignored <- lapply(ignored_fields, function(field) {
    ref_value <- if (field %in% names(reference)) reference[[field]] else NULL
    can_value <- if (field %in% names(candidate)) candidate[[field]] else NULL
    comparison <- tdabm_p12_compare_vector(ref_value, can_value, abs_tol, rel_tol)
    data.frame(
      run_path = run_path,
      run_type = run_type,
      field = field,
      differs = comparison$status != "EXACT",
      reference_value = paste(tdabm_p12_as_character_safe(ref_value), collapse = " | "),
      candidate_value = paste(tdabm_p12_as_character_safe(can_value), collapse = " | "),
      reason = "Declared runtime-only field.",
      stringsAsFactors = FALSE
    )
  })
  ignored <- if (length(ignored)) do.call(rbind, ignored) else data.frame(
    run_path = character(), run_type = character(), field = character(),
    differs = logical(), reference_value = character(), candidate_value = character(),
    reason = character(), stringsAsFactors = FALSE
  )

  status <- if (nrow(details) == 0L) {
    "EXACT"
  } else if (any(details$status == "FAIL")) {
    "FAIL"
  } else if (any(details$status == "TOLERANCE_EQUIVALENT")) {
    "TOLERANCE_EQUIVALENT"
  } else {
    "EXACT"
  }

  finite_abs <- details$max_abs_diff[is.finite(details$max_abs_diff)]
  finite_rel <- details$max_rel_diff[is.finite(details$max_rel_diff)]

  summary <- data.frame(
    run_path = run_path,
    run_type = run_type,
    component = component,
    status = status,
    nrow_reference = 1L,
    nrow_candidate = 1L,
    ncol_reference = length(names(reference)),
    ncol_candidate = length(names(candidate)),
    differing_cells = sum(details$differing_cells, na.rm = TRUE),
    max_abs_diff = if (length(finite_abs)) max(finite_abs) else NA_real_,
    max_rel_diff = if (length(finite_rel)) max(finite_rel) else NA_real_,
    note = if (length(ignored_fields)) {
      paste0("Ignored runtime fields: ", paste(ignored_fields, collapse = ", "), ".")
    } else {
      "No runtime fields ignored."
    },
    stringsAsFactors = FALSE
  )

  list(summary = summary, details = details, ignored = ignored)
}

tdabm_p12_metadata_to_list <- function(metadata) {
  if (is.null(metadata)) return(list())
  metadata <- as.data.frame(metadata, stringsAsFactors = FALSE)
  if (nrow(metadata) == 0L) return(list())
  as.list(metadata[1L, , drop = FALSE])
}

tdabm_p12_identify_run_type <- function(relative_path) {
  if (grepl("(^|/)memberships/[^/]+_result\\.rds$", relative_path)) {
    return("membership")
  }
  if (grepl("(^|/)verification/[^/]+_result\\.rds$", relative_path)) {
    return("bmstats")
  }
  "other"
}

tdabm_p12_inventory_results <- function(root) {
  root <- tdabm_p12_normalize_path(root, must_work = TRUE)
  files <- list.files(
    root,
    pattern = "_result\\.rds$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(files) == 0L) {
    return(data.frame(
      relative_path = character(),
      run_type = character(),
      absolute_path = character(),
      size_bytes = numeric(),
      modified = character(),
      stringsAsFactors = FALSE
    ))
  }

  info <- file.info(files)
  relative <- vapply(files, tdabm_p12_relative_path, character(1), root = root)
  run_type <- vapply(relative, tdabm_p12_identify_run_type, character(1))
  keep <- run_type %in% c("bmstats", "membership")

  data.frame(
    relative_path = relative[keep],
    run_type = run_type[keep],
    absolute_path = files[keep],
    size_bytes = info$size[keep],
    modified = format(info$mtime[keep], "%Y-%m-%dT%H:%M:%S%z"),
    stringsAsFactors = FALSE
  )
}

tdabm_p12_status_from_components <- function(statuses) {
  statuses <- as.character(statuses)
  if (length(statuses) == 0L) return("FAIL")
  if (any(statuses == "FAIL")) return("FAIL")
  if (any(statuses == "TOLERANCE_EQUIVALENT")) return("TOLERANCE_EQUIVALENT")
  if (all(statuses %in% c("EXACT", "STRUCTURE_ONLY"))) return("EXACT")
  "FAIL"
}

tdabm_p12_compare_one_run <- function(
  run_path,
  run_type,
  reference_file,
  candidate_file,
  abs_tol,
  rel_tol,
  membership_mode = c("summary", "deep")
) {
  membership_mode <- match.arg(membership_mode)

  ref <- tryCatch(readRDS(reference_file), error = function(e) e)
  can <- tryCatch(readRDS(candidate_file), error = function(e) e)

  if (inherits(ref, "error") || inherits(can, "error")) {
    note <- paste(
      if (inherits(ref, "error")) paste0("Reference read error: ", conditionMessage(ref)) else "",
      if (inherits(can, "error")) paste0("Candidate read error: ", conditionMessage(can)) else ""
    )
    run <- data.frame(
      relative_path = run_path,
      run_type = run_type,
      analytical_status = "FAIL",
      overall_status = "FAIL",
      metadata_only_difference = FALSE,
      structure_only_components = 0L,
      note = trimws(note),
      stringsAsFactors = FALSE
    )
    return(list(
      run = run,
      components = data.frame(),
      details = tdabm_p12_empty_detail(),
      ignored_metadata = data.frame()
    ))
  }

  comparisons <- list()
  ignored_metadata <- list()

  if (run_type == "bmstats") {
    components <- c("m001", "xv01", "m001s")
    for (component in components) {
      comparisons[[component]] <- tdabm_p12_compare_data_frame(
        ref[[component]], can[[component]], component,
        run_path, run_type, abs_tol, rel_tol
      )
    }
  } else if (run_type == "membership") {
    summary_components <- c(
      "authority_summary",
      "isolation_summary",
      "pairwise_comembership",
      "graph_repetitions",
      "errors"
    )
    for (component in summary_components) {
      comparisons[[component]] <- tdabm_p12_compare_data_frame(
        ref[[component]], can[[component]], component,
        run_path, run_type, abs_tol, rel_tol
      )
    }

    raw_components <- c("membership_long", "point_repetitions")
    for (component in raw_components) {
      comparisons[[component]] <- tdabm_p12_compare_data_frame(
        ref[[component]], can[[component]], component,
        run_path, run_type, abs_tol, rel_tol,
        structure_only = membership_mode == "summary"
      )
    }
  }

  settings <- tdabm_p12_compare_named_list(
    ref$settings,
    can$settings,
    component = "settings",
    run_path = run_path,
    run_type = run_type,
    ignore_fields = c("ncores"),
    abs_tol = abs_tol,
    rel_tol = rel_tol
  )
  comparisons$settings <- settings
  ignored_metadata[["settings"]] <- settings$ignored

  metadata <- tdabm_p12_compare_named_list(
    tdabm_p12_metadata_to_list(ref$tdabm_project_run),
    tdabm_p12_metadata_to_list(can$tdabm_project_run),
    component = "tdabm_project_run",
    run_path = run_path,
    run_type = run_type,
    ignore_fields = c("ncores", "started", "completed", "elapsed_seconds"),
    abs_tol = abs_tol,
    rel_tol = rel_tol
  )
  comparisons$tdabm_project_run <- metadata
  ignored_metadata[["tdabm_project_run"]] <- metadata$ignored

  component_summary <- do.call(rbind, lapply(comparisons, `[[`, "summary"))
  column_details <- do.call(rbind, lapply(comparisons, function(x) x$details %||% tdabm_p12_empty_detail()))
  ignored_metadata <- do.call(rbind, ignored_metadata)

  analytical_status <- tdabm_p12_status_from_components(component_summary$status)
  metadata_diff <- nrow(ignored_metadata) > 0L && any(ignored_metadata$differs)

  overall_status <- if (analytical_status == "FAIL") {
    "FAIL"
  } else if (analytical_status == "TOLERANCE_EQUIVALENT") {
    "TOLERANCE_EQUIVALENT"
  } else if (metadata_diff) {
    "METADATA_ONLY_DIFFERENCE"
  } else {
    "EXACT"
  }

  run <- data.frame(
    relative_path = run_path,
    run_type = run_type,
    analytical_status = analytical_status,
    overall_status = overall_status,
    metadata_only_difference = metadata_diff,
    structure_only_components = sum(component_summary$status == "STRUCTURE_ONLY"),
    note = if (membership_mode == "summary" && run_type == "membership") {
      "Large raw membership components received structure-only comparison; paper-facing summaries received full semantic comparison."
    } else {
      ""
    },
    stringsAsFactors = FALSE
  )

  list(
    run = run,
    components = component_summary,
    details = column_details,
    ignored_metadata = ignored_metadata
  )
}

tdabm_p12_build_acceptance_text <- function(
  reference_root,
  candidate_root,
  output_root,
  run_comparison,
  abs_tol,
  rel_tol,
  membership_mode,
  expected_bmstats,
  expected_membership,
  gate_pass
) {
  substantive <- run_comparison[run_comparison$included_in_gate, , drop = FALSE]
  excluded <- run_comparison[!run_comparison$included_in_gate, , drop = FALSE]

  counts <- table(factor(
    substantive$overall_status,
    levels = c("EXACT", "METADATA_ONLY_DIFFERENCE", "TOLERANCE_EQUIVALENT", "FAIL")
  ))

  c(
    "TDABM P1.2 semantic equivalence acceptance",
    "",
    paste0("Created: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    paste0("Reference root: ", reference_root),
    paste0("Candidate root: ", candidate_root),
    paste0("Audit output: ", output_root),
    paste0("Absolute tolerance: ", format(abs_tol, scientific = TRUE)),
    paste0("Relative tolerance: ", format(rel_tol, scientific = TRUE)),
    paste0("Membership comparison mode: ", membership_mode),
    "",
    "Substantive run counts",
    paste0("- expected BMStats runs: ", expected_bmstats),
    paste0("- observed BMStats runs in gate: ", sum(substantive$run_type == "bmstats")),
    paste0("- expected membership runs: ", expected_membership),
    paste0("- observed membership runs in gate: ", sum(substantive$run_type == "membership")),
    paste0("- excluded/not comparable runs: ", nrow(excluded)),
    "",
    "Semantic classifications",
    paste0("- EXACT: ", counts[["EXACT"]]),
    paste0("- METADATA_ONLY_DIFFERENCE: ", counts[["METADATA_ONLY_DIFFERENCE"]]),
    paste0("- TOLERANCE_EQUIVALENT: ", counts[["TOLERANCE_EQUIVALENT"]]),
    paste0("- FAIL: ", counts[["FAIL"]]),
    "",
    paste0("Acceptance gate: ", if (gate_pass) "PASS" else "FAIL"),
    "",
    "Interpretation",
    "- EXACT means analytical values matched exactly after key-based row alignment.",
    "- METADATA_ONLY_DIFFERENCE means analytical values matched exactly and only declared runtime fields differed.",
    "- TOLERANCE_EQUIVALENT means all analytical differences were within the declared tolerances.",
    "- NOT_COMPARABLE runs are reported but excluded from the substantive gate by an explicit project-level rule.",
    "- In summary membership mode, raw membership_long and point_repetitions objects receive structure-only checks; paper-facing membership summaries receive full value comparisons.",
    "",
    "No production files were modified by this comparator."
  )
}

tdabm_compare_production_roots <- function(
  reference_root,
  candidate_root,
  output_root,
  exclusion_regex = "^$",
  exclusion_reason = "Explicitly excluded by project adapter.",
  abs_tol = 1e-12,
  rel_tol = 1e-10,
  membership_mode = c("summary", "deep"),
  expected_bmstats = NA_integer_,
  expected_membership = NA_integer_,
  input_file = NULL,
  verbose = TRUE
) {
  membership_mode <- match.arg(membership_mode)
  reference_root <- tdabm_p12_normalize_path(reference_root, must_work = TRUE)
  candidate_root <- tdabm_p12_normalize_path(candidate_root, must_work = TRUE)
  output_root <- tdabm_p12_normalize_path(output_root, must_work = FALSE)

  if (identical(reference_root, candidate_root)) {
    stop("Reference and candidate roots must differ.", call. = FALSE)
  }
  if (dir.exists(output_root) || file.exists(output_root)) {
    stop("Output root already exists: ", output_root, call. = FALSE)
  }

  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

  if (isTRUE(verbose)) {
    cat("============================================================\n")
    cat("TDABM P1.2 SEMANTIC EQUIVALENCE COMPARATOR\n")
    cat("============================================================\n")
    cat("Reference: ", reference_root, "\n", sep = "")
    cat("Candidate: ", candidate_root, "\n", sep = "")
    cat("Output:    ", output_root, "\n", sep = "")
    cat("Abs tol:   ", abs_tol, "\n", sep = "")
    cat("Rel tol:   ", rel_tol, "\n", sep = "")
    cat("Membership:", membership_mode, "\n", sep = "")
    cat("============================================================\n\n")
  }

  ref_inventory <- tdabm_p12_inventory_results(reference_root)
  can_inventory <- tdabm_p12_inventory_results(candidate_root)
  all_paths <- sort(union(ref_inventory$relative_path, can_inventory$relative_path))

  if (length(all_paths) == 0L) {
    stop("No BMStats or membership result files were found in either production root.", call. = FALSE)
  }

  inventory <- lapply(all_paths, function(path) {
    ref_row <- ref_inventory[ref_inventory$relative_path == path, , drop = FALSE]
    can_row <- can_inventory[can_inventory$relative_path == path, , drop = FALSE]
    run_type <- if (nrow(ref_row)) ref_row$run_type[[1L]] else can_row$run_type[[1L]]
    excluded <- nzchar(exclusion_regex) && grepl(exclusion_regex, path, perl = TRUE)

    data.frame(
      relative_path = path,
      run_type = run_type,
      reference_exists = nrow(ref_row) == 1L,
      candidate_exists = nrow(can_row) == 1L,
      reference_file = if (nrow(ref_row)) ref_row$absolute_path[[1L]] else "",
      candidate_file = if (nrow(can_row)) can_row$absolute_path[[1L]] else "",
      reference_size_bytes = if (nrow(ref_row)) ref_row$size_bytes[[1L]] else NA_real_,
      candidate_size_bytes = if (nrow(can_row)) can_row$size_bytes[[1L]] else NA_real_,
      included_in_gate = !excluded,
      exclusion_reason = if (excluded) exclusion_reason else "",
      stringsAsFactors = FALSE
    )
  })
  inventory <- do.call(rbind, inventory)

  run_rows <- list()
  component_rows <- list()
  detail_rows <- list()
  ignored_rows <- list()

  for (i in seq_len(nrow(inventory))) {
    row <- inventory[i, , drop = FALSE]
    path <- row$relative_path[[1L]]

    if (isTRUE(verbose)) {
      cat(sprintf("[%d/%d] %s\n", i, nrow(inventory), path))
    }

    if (!row$included_in_gate[[1L]]) {
      run_rows[[length(run_rows) + 1L]] <- data.frame(
        relative_path = path,
        run_type = row$run_type[[1L]],
        analytical_status = "NOT_COMPARABLE",
        overall_status = "NOT_COMPARABLE",
        metadata_only_difference = FALSE,
        structure_only_components = 0L,
        note = row$exclusion_reason[[1L]],
        included_in_gate = FALSE,
        stringsAsFactors = FALSE
      )
      next
    }

    if (!row$reference_exists[[1L]] || !row$candidate_exists[[1L]]) {
      note <- if (!row$reference_exists[[1L]]) {
        "Reference result is missing."
      } else {
        "Candidate result is missing."
      }
      run_rows[[length(run_rows) + 1L]] <- data.frame(
        relative_path = path,
        run_type = row$run_type[[1L]],
        analytical_status = "FAIL",
        overall_status = "FAIL",
        metadata_only_difference = FALSE,
        structure_only_components = 0L,
        note = note,
        included_in_gate = TRUE,
        stringsAsFactors = FALSE
      )
      next
    }

    comparison <- tdabm_p12_compare_one_run(
      run_path = path,
      run_type = row$run_type[[1L]],
      reference_file = row$reference_file[[1L]],
      candidate_file = row$candidate_file[[1L]],
      abs_tol = abs_tol,
      rel_tol = rel_tol,
      membership_mode = membership_mode
    )

    comparison$run$included_in_gate <- TRUE
    run_rows[[length(run_rows) + 1L]] <- comparison$run
    component_rows[[length(component_rows) + 1L]] <- comparison$components
    detail_rows[[length(detail_rows) + 1L]] <- comparison$details
    ignored_rows[[length(ignored_rows) + 1L]] <- comparison$ignored_metadata

    rm(comparison)
    invisible(gc(verbose = FALSE))
  }

  run_comparison <- do.call(rbind, run_rows)
  component_comparison <- if (length(component_rows)) do.call(rbind, component_rows) else data.frame()
  column_comparison <- if (length(detail_rows)) do.call(rbind, detail_rows) else tdabm_p12_empty_detail()
  ignored_metadata <- if (length(ignored_rows)) do.call(rbind, ignored_rows) else data.frame()

  substantive <- run_comparison[run_comparison$included_in_gate, , drop = FALSE]
  observed_bmstats <- sum(substantive$run_type == "bmstats")
  observed_membership <- sum(substantive$run_type == "membership")
  expected_counts_ok <- (is.na(expected_bmstats) || observed_bmstats == expected_bmstats) &&
    (is.na(expected_membership) || observed_membership == expected_membership)
  statuses_ok <- nrow(substantive) > 0L && all(substantive$overall_status %in% c(
    "EXACT", "METADATA_ONLY_DIFFERENCE", "TOLERANCE_EQUIVALENT"
  ))
  inventory_ok <- all(inventory$reference_exists[inventory$included_in_gate]) &&
    all(inventory$candidate_exists[inventory$included_in_gate])
  gate_pass <- expected_counts_ok && statuses_ok && inventory_ok

  summary <- data.frame(
    metric = c(
      "reference_result_files",
      "candidate_result_files",
      "substantive_bmstats_runs",
      "substantive_membership_runs",
      "excluded_not_comparable_runs",
      "exact_runs",
      "metadata_only_difference_runs",
      "tolerance_equivalent_runs",
      "failed_runs",
      "acceptance_gate_pass"
    ),
    value = c(
      nrow(ref_inventory),
      nrow(can_inventory),
      observed_bmstats,
      observed_membership,
      sum(!run_comparison$included_in_gate),
      sum(substantive$overall_status == "EXACT"),
      sum(substantive$overall_status == "METADATA_ONLY_DIFFERENCE"),
      sum(substantive$overall_status == "TOLERANCE_EQUIVALENT"),
      sum(substantive$overall_status == "FAIL"),
      as.integer(gate_pass)
    ),
    stringsAsFactors = FALSE
  )

  hashes <- lapply(seq_len(nrow(inventory)), function(i) {
    row <- inventory[i, , drop = FALSE]
    out <- list()
    if (row$reference_exists[[1L]]) {
      hash <- tdabm_p12_hash_file(row$reference_file[[1L]])
      out[[length(out) + 1L]] <- data.frame(
        relative_path = row$relative_path[[1L]],
        side = "reference",
        algorithm = hash$algorithm,
        hash = hash$hash,
        stringsAsFactors = FALSE
      )
    }
    if (row$candidate_exists[[1L]]) {
      hash <- tdabm_p12_hash_file(row$candidate_file[[1L]])
      out[[length(out) + 1L]] <- data.frame(
        relative_path = row$relative_path[[1L]],
        side = "candidate",
        algorithm = hash$algorithm,
        hash = hash$hash,
        stringsAsFactors = FALSE
      )
    }
    do.call(rbind, out)
  })
  hashes <- do.call(rbind, hashes)

  input_hash <- data.frame()
  if (!is.null(input_file) && nzchar(input_file) && file.exists(input_file)) {
    hash <- tdabm_p12_hash_file(input_file)
    input_hash <- data.frame(
      input_file = tdabm_p12_normalize_path(input_file, must_work = TRUE),
      algorithm = hash$algorithm,
      hash = hash$hash,
      size_bytes = file.info(input_file)$size,
      stringsAsFactors = FALSE
    )
  }

  tdabm_p12_write_csv(inventory, file.path(output_root, "run_inventory.csv"))
  tdabm_p12_write_csv(run_comparison, file.path(output_root, "run_comparison.csv"))
  tdabm_p12_write_csv(component_comparison, file.path(output_root, "component_comparison.csv"))
  tdabm_p12_write_csv(column_comparison, file.path(output_root, "column_comparison.csv"))
  tdabm_p12_write_csv(ignored_metadata, file.path(output_root, "ignored_runtime_metadata.csv"))
  tdabm_p12_write_csv(summary, file.path(output_root, "acceptance_summary.csv"))
  tdabm_p12_write_csv(hashes, file.path(output_root, "result_file_hashes.csv"))
  if (nrow(input_hash)) {
    tdabm_p12_write_csv(input_hash, file.path(output_root, "input_file_hash.csv"))
  }

  config <- data.frame(
    parameter = c(
      "reference_root", "candidate_root", "output_root", "exclusion_regex",
      "exclusion_reason", "abs_tol", "rel_tol", "membership_mode",
      "expected_bmstats", "expected_membership", "input_file"
    ),
    value = c(
      reference_root, candidate_root, output_root, exclusion_regex,
      exclusion_reason, format(abs_tol, scientific = TRUE),
      format(rel_tol, scientific = TRUE), membership_mode,
      as.character(expected_bmstats), as.character(expected_membership),
      input_file %||% ""
    ),
    stringsAsFactors = FALSE
  )
  tdabm_p12_write_csv(config, file.path(output_root, "comparison_configuration.csv"))

  acceptance_text <- tdabm_p12_build_acceptance_text(
    reference_root, candidate_root, output_root, run_comparison,
    abs_tol, rel_tol, membership_mode,
    expected_bmstats, expected_membership, gate_pass
  )
  tdabm_p12_write_lines(
    acceptance_text,
    file.path(output_root, "P1_2_SEMANTIC_EQUIVALENCE_ACCEPTANCE.txt")
  )

  session <- capture.output(sessionInfo())
  tdabm_p12_write_lines(session, file.path(output_root, "sessionInfo.txt"))

  saveRDS(
    list(
      configuration = config,
      inventory = inventory,
      run_comparison = run_comparison,
      component_comparison = component_comparison,
      column_comparison = column_comparison,
      ignored_metadata = ignored_metadata,
      acceptance_summary = summary,
      gate_pass = gate_pass
    ),
    file.path(output_root, "p1_2_semantic_equivalence_audit.rds")
  )

  manifest_files <- list.files(output_root, recursive = TRUE, full.names = TRUE)
  manifest_info <- file.info(manifest_files)
  manifest <- data.frame(
    relative_path = vapply(manifest_files, tdabm_p12_relative_path, character(1), root = output_root),
    size_bytes = manifest_info$size,
    modified = format(manifest_info$mtime, "%Y-%m-%dT%H:%M:%S%z"),
    stringsAsFactors = FALSE
  )
  tdabm_p12_write_csv(manifest, file.path(output_root, "audit_manifest.csv"))

  if (isTRUE(verbose)) {
    cat("\n============================================================\n")
    cat("P1.2 SEMANTIC EQUIVALENCE COMPLETE\n")
    cat("============================================================\n")
    print(summary, row.names = FALSE)
    cat("Acceptance gate: ", if (gate_pass) "PASS" else "FAIL", "\n", sep = "")
    cat("Output root: ", output_root, "\n", sep = "")
    cat("============================================================\n")
  }

  invisible(list(
    gate_pass = gate_pass,
    output_root = output_root,
    inventory = inventory,
    run_comparison = run_comparison,
    component_comparison = component_comparison,
    column_comparison = column_comparison,
    ignored_metadata = ignored_metadata,
    acceptance_summary = summary
  ))
}
