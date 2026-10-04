# ==============================================================================
# TDABMRadiusContract.R
# ==============================================================================
# Explicit, user-controlled contract for paper-facing radius choices.
#
# A criterion may suggest a radius.  It cannot silently choose the paper radius.
# Paper evidence requires a declared radius, a substantive interpretation, and
# an auditable user decision.  Construction and paper-output stages remain
# separate.
# ==============================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

tdabm_radius_contract <- function(
  target_radius,
  tolerance = 1e-8,
  selection_basis,
  interpretation,
  decision_rationale,
  topology_axes,
  scaling,
  distance_description,
  conditioning_statement,
  precision_policy,
  optimality_claim = "none",
  criterion_candidate_radius = NA_real_,
  criterion_description = NA_character_,
  approved_by,
  approved_at = as.character(Sys.time()),
  construction_seed = 123L,
  automatic_selection_performed = FALSE
) {
  list(
    target_radius = as.numeric(target_radius),
    tolerance = as.numeric(tolerance),
    selection_basis = as.character(selection_basis),
    interpretation = as.character(interpretation),
    decision_rationale = as.character(decision_rationale),
    topology_axes = as.character(topology_axes),
    scaling = as.character(scaling),
    distance_description = as.character(distance_description),
    conditioning_statement = as.character(conditioning_statement),
    precision_policy = as.character(precision_policy),
    optimality_claim = as.character(optimality_claim),
    criterion_candidate_radius = suppressWarnings(as.numeric(criterion_candidate_radius)),
    criterion_description = as.character(criterion_description),
    approved_by = as.character(approved_by),
    approved_at = as.character(approved_at),
    construction_seed = as.integer(construction_seed),
    automatic_selection_performed = isTRUE(automatic_selection_performed)
  )
}

tdabm_validate_radius_contract <- function(contract, stop_on_error = FALSE) {
  allowed_basis <- c(
    "substantively_interpretable_fixed",
    "criterion_selected",
    "criterion_rounded_with_interpretation",
    "externally_defined",
    "user_declared"
  )
  allowed_precision_policy <- c(
    "substantive_rounding", "criterion_precision", "externally_fixed", "user_declared"
  )
  allowed_optimality_claim <- c(
    "none", "criterion_only", "formal_objective", "external_rule"
  )
  checks <- data.frame(
    check = character(), pass = logical(), message = character(),
    stringsAsFactors = FALSE
  )
  add <- function(name, pass, message) {
    checks <<- rbind(checks, data.frame(
      check = name, pass = isTRUE(pass), message = message,
      stringsAsFactors = FALSE
    ))
  }

  add("contract_is_list", is.list(contract), "Radius contract must be a list.")
  if (!is.list(contract)) {
    if (isTRUE(stop_on_error)) stop("Radius contract is not a list.", call. = FALSE)
    return(checks)
  }

  radius <- suppressWarnings(as.numeric(contract$target_radius %||% NA_real_))
  tolerance <- suppressWarnings(as.numeric(contract$tolerance %||% NA_real_))
  basis <- as.character(contract$selection_basis %||% "")
  interpretation <- trimws(as.character(contract$interpretation %||% ""))
  rationale <- trimws(as.character(contract$decision_rationale %||% ""))
  axes <- as.character(contract$topology_axes %||% character())
  scaling <- trimws(as.character(contract$scaling %||% ""))
  distance <- trimws(as.character(contract$distance_description %||% ""))
  conditioning <- trimws(as.character(contract$conditioning_statement %||% ""))
  precision_policy <- trimws(as.character(contract$precision_policy %||% ""))
  optimality_claim <- trimws(as.character(contract$optimality_claim %||% ""))
  criterion_candidate <- suppressWarnings(as.numeric(contract$criterion_candidate_radius %||% NA_real_))
  criterion_description <- trimws(as.character(contract$criterion_description %||% ""))
  approver <- trimws(as.character(contract$approved_by %||% ""))

  add("target_radius", length(radius) == 1L && is.finite(radius) && radius > 0,
      "target_radius must be one finite positive number.")
  add("tolerance", length(tolerance) == 1L && is.finite(tolerance) && tolerance >= 0,
      "tolerance must be one finite non-negative number.")
  add("selection_basis", length(basis) == 1L && basis %in% allowed_basis,
      paste0("selection_basis must be one of: ", paste(allowed_basis, collapse = ", "), "."))
  add("interpretation", length(interpretation) == 1L && nchar(interpretation) >= 60L,
      "A substantive radius interpretation of at least 60 characters is required.")
  add("decision_rationale", length(rationale) == 1L && nchar(rationale) >= 60L,
      "A decision rationale of at least 60 characters is required.")
  add("topology_axes", length(axes) >= 1L && all(!is.na(axes) & nzchar(axes)),
      "At least one declared topology axis is required.")
  add("scaling", length(scaling) == 1L && nzchar(scaling), "Scaling must be declared.")
  add("distance_description", length(distance) == 1L && nchar(distance) >= 30L,
      "The distance/neighbourhood interpretation must be declared in at least 30 characters.")
  add("conditioning_statement", length(conditioning) == 1L && nchar(conditioning) >= 40L,
      "The interpretation must be stated as conditional on the selected variables, scaling and metric.")
  add("precision_policy", length(precision_policy) == 1L && precision_policy %in% allowed_precision_policy,
      paste0("precision_policy must be one of: ", paste(allowed_precision_policy, collapse = ", "), "."))
  add("optimality_claim", length(optimality_claim) == 1L && optimality_claim %in% allowed_optimality_claim,
      paste0("optimality_claim must be one of: ", paste(allowed_optimality_claim, collapse = ", "), "."))
  criterion_basis <- basis %in% c("criterion_selected", "criterion_rounded_with_interpretation")
  candidate_recorded <- length(criterion_candidate) == 1L && is.finite(criterion_candidate)
  criterion_provenance_ok <- !(criterion_basis || candidate_recorded) ||
    (candidate_recorded && nchar(criterion_description) >= 30L)
  add("criterion_provenance_when_used", criterion_provenance_ok,
      "A recorded or criterion-based candidate radius requires a criterion description of at least 30 characters.")
  add("approved_by", length(approver) == 1L && nzchar(approver),
      "The user or analyst approving the paper radius must be recorded.")
  add("no_automatic_selection", identical(contract$automatic_selection_performed, FALSE),
      "The paper-radius contract must record automatic_selection_performed = FALSE.")

  if (isTRUE(stop_on_error) && !all(checks$pass)) {
    failed <- checks[!checks$pass, , drop = FALSE]
    stop(
      paste(c("Radius contract failed:", paste0("- ", failed$check, ": ", failed$message)), collapse = "\n"),
      call. = FALSE
    )
  }
  checks
}

tdabm_radius_contract_table <- function(contract) {
  fields <- c(
    "target_radius", "tolerance", "selection_basis", "interpretation",
    "decision_rationale", "topology_axes", "scaling", "distance_description",
    "conditioning_statement", "precision_policy", "optimality_claim",
    "criterion_candidate_radius", "criterion_description", "approved_by",
    "approved_at", "construction_seed", "automatic_selection_performed"
  )
  value <- vapply(fields, function(name) {
    x <- contract[[name]]
    if (is.null(x) || length(x) == 0L) return(NA_character_)
    paste(as.character(x), collapse = "; ")
  }, character(1))
  data.frame(field = fields, value = unname(value), stringsAsFactors = FALSE)
}

tdabm_radius_file_sha256 <- function(file) {
  if (is.null(file) || length(file) != 1L || is.na(file[[1L]]) || !file.exists(file[[1L]])) return(NA_character_)
  file <- as.character(file[[1L]])
  if (requireNamespace("digest", quietly = TRUE)) {
    return(unname(digest::digest(file, algo = "sha256", file = TRUE)))
  }
  cmd <- Sys.which("sha256sum")
  if (nzchar(cmd)) {
    out <- system2(cmd, shQuote(normalizePath(file, mustWork = TRUE)), stdout = TRUE, stderr = TRUE)
    if (length(out) && is.null(attr(out, "status"))) return(strsplit(out[[1L]], "[[:space:]]+")[[1L]][[1L]])
  }
  cmd <- Sys.which("shasum")
  if (nzchar(cmd)) {
    out <- system2(cmd, c("-a", "256", shQuote(normalizePath(file, mustWork = TRUE))), stdout = TRUE, stderr = TRUE)
    if (length(out) && is.null(attr(out, "status"))) return(strsplit(out[[1L]], "[[:space:]]+")[[1L]][[1L]])
  }
  NA_character_
}

tdabm_is_ballmapper_object <- function(obj) {
  is.list(obj) &&
    all(c("points_covered_by_landmarks", "coverage", "landmarks", "coloring") %in% names(obj)) &&
    is.list(obj$points_covered_by_landmarks)
}

tdabm_ballmapper_radius <- function(obj, object_file = NULL) {
  for (slot in c("epsilon", "eps", "radius", "epsf")) {
    if (is.list(obj) && slot %in% names(obj)) {
      value <- suppressWarnings(as.numeric(obj[[slot]][1L]))
      if (length(value) == 1L && is.finite(value)) return(value)
    }
  }
  meta <- obj$tdabm_paper_metadata %||% list()
  value <- suppressWarnings(as.numeric(meta$target_radius %||% NA_real_))
  if (length(value) == 1L && is.finite(value)) return(value)
  NA_real_
}

tdabm_assert_ballmapper_radius_contract <- function(
  object_file,
  contract,
  require_metadata = TRUE,
  expected_n_points = NA_integer_
) {
  tdabm_validate_radius_contract(contract, stop_on_error = TRUE)
  if (!file.exists(object_file)) stop("Frozen BallMapper object not found: ", object_file, call. = FALSE)
  obj <- readRDS(object_file)
  if (!tdabm_is_ballmapper_object(obj)) stop("Frozen RDS is not a recognised BallMapper object.", call. = FALSE)

  radius <- tdabm_ballmapper_radius(obj, object_file)
  target <- as.numeric(contract$target_radius)
  tolerance <- as.numeric(contract$tolerance)
  if (!is.finite(radius) || abs(radius - target) > tolerance) {
    stop("Frozen BallMapper radius does not match the declared paper radius. Declared: ",
         target, "; detected: ", radius, call. = FALSE)
  }

  meta <- obj$tdabm_paper_metadata %||% NULL
  if (isTRUE(require_metadata) && is.null(meta)) {
    stop("Frozen BallMapper object lacks tdabm_paper_metadata.", call. = FALSE)
  }
  if (!is.null(meta)) {
    meta_axes <- as.character(meta$point_columns %||% character())
    contract_axes <- as.character(contract$topology_axes)
    if (length(meta_axes) == 0L || !identical(meta_axes, contract_axes)) {
      stop("Frozen BallMapper topology axes do not match the declared paper-radius contract.", call. = FALSE)
    }
    meta_auto <- meta$automatic_selection_performed %||% NA
    if (!identical(meta_auto, FALSE)) {
      stop("Frozen object does not record automatic_selection_performed = FALSE.", call. = FALSE)
    }
    meta_target <- suppressWarnings(as.numeric(meta$target_radius %||% NA_real_))
    if (length(meta_target) != 1L || !is.finite(meta_target) || abs(meta_target - target) > tolerance) {
      stop("Frozen object metadata target radius does not match the declared contract.", call. = FALSE)
    }
    fields_exact <- c("selection_basis", "scaling", "conditioning_statement", "precision_policy", "optimality_claim", "approved_by")
    for (field in fields_exact) {
      observed <- as.character(meta[[field]] %||% "")
      expected <- as.character(contract[[field]] %||% "")
      if (length(observed) != 1L || !identical(observed, expected)) {
        stop("Frozen object metadata does not match the radius contract for field: ", field, call. = FALSE)
      }
    }
    if (is.finite(expected_n_points)) {
      n_meta <- suppressWarnings(as.integer(meta$n_points %||% NA_integer_))
      if (!identical(n_meta, as.integer(expected_n_points))) {
        stop("Frozen BallMapper observation count does not match the current project data.", call. = FALSE)
      }
    }
  }
  invisible(list(object = obj, radius = radius, metadata = meta))
}
