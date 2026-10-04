# ==============================================================================
# TDABMUserRadiusContract.R
# ==============================================================================
# Application-neutral user radius approval contract.
#
# Diagnostic pipelines may report a non-binding topology reference. They may
# not silently approve a radius. A fixed topology requires an explicit user
# radius, an interpretation, provenance for the diagnostic reference, and no
# automatic-selection or optimality claim. The review band is diagnostic only;
# an explicitly approved radius may lie below, inside, or above it.
# ==============================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

tdabm_urc_review_band_position <- function(target_radius, review_band_lower, review_band_upper) {
  radius <- suppressWarnings(as.numeric(target_radius))
  lo <- suppressWarnings(as.numeric(review_band_lower))
  hi <- suppressWarnings(as.numeric(review_band_upper))
  if (length(radius)!=1L || !is.finite(radius) || radius<=0 ||
      length(lo)!=1L || length(hi)!=1L || !is.finite(lo) || !is.finite(hi) ||
      lo<=0 || hi<=lo) return(NA_character_)
  if (radius < lo) return("BELOW_REVIEW_BAND")
  if (radius > hi) return("ABOVE_REVIEW_BAND")
  "INSIDE_REVIEW_BAND"
}

tdabm_urc_contract <- function(
  application,
  target_radius,
  tolerance = 1e-8,
  selection_basis,
  interpretation,
  decision_rationale,
  topology_axes,
  scaling,
  metric,
  precision_policy,
  pipeline_reference_radius = NA_real_,
  pipeline_reference_method = NA_character_,
  review_band_lower = NA_real_,
  review_band_upper = NA_real_,
  diagnostic_audit_sha256 = NA_character_,
  approved_by,
  approved_at,
  automatic_selection_performed = FALSE,
  optimality_claim = "NONE"
) {
  list(
    application = as.character(application),
    target_radius = as.numeric(target_radius),
    tolerance = as.numeric(tolerance),
    selection_basis = as.character(selection_basis),
    interpretation = as.character(interpretation),
    decision_rationale = as.character(decision_rationale),
    topology_axes = as.character(topology_axes),
    scaling = as.character(scaling),
    metric = as.character(metric),
    precision_policy = as.character(precision_policy),
    pipeline_reference_radius = suppressWarnings(as.numeric(pipeline_reference_radius)),
    pipeline_reference_method = as.character(pipeline_reference_method),
    review_band_lower = suppressWarnings(as.numeric(review_band_lower)),
    review_band_upper = suppressWarnings(as.numeric(review_band_upper)),
    review_band_position = tdabm_urc_review_band_position(target_radius, review_band_lower, review_band_upper),
    diagnostic_audit_sha256 = as.character(diagnostic_audit_sha256),
    approved_by = as.character(approved_by),
    approved_at = as.character(approved_at),
    automatic_selection_performed = isTRUE(automatic_selection_performed),
    optimality_claim = as.character(optimality_claim)
  )
}

tdabm_urc_validate <- function(contract, stop_on_error = FALSE) {
  rows <- data.frame(check=character(), pass=logical(), detail=character(), stringsAsFactors=FALSE)
  add <- function(check, pass, detail) rows <<- rbind(rows, data.frame(check=check, pass=isTRUE(pass), detail=detail, stringsAsFactors=FALSE))
  add("contract_is_list", is.list(contract), "contract must be a list")
  if (!is.list(contract)) {
    if (isTRUE(stop_on_error)) stop("User radius contract is not a list.", call.=FALSE)
    return(rows)
  }
  radius <- suppressWarnings(as.numeric(contract$target_radius %||% NA_real_))
  tol <- suppressWarnings(as.numeric(contract$tolerance %||% NA_real_))
  axes <- as.character(contract$topology_axes %||% character())
  pref <- suppressWarnings(as.numeric(contract$pipeline_reference_radius %||% NA_real_))
  lo <- suppressWarnings(as.numeric(contract$review_band_lower %||% NA_real_))
  hi <- suppressWarnings(as.numeric(contract$review_band_upper %||% NA_real_))
  add("application", length(contract$application)==1L && nzchar(trimws(contract$application)), "application must be declared")
  add("target_radius", length(radius)==1L && is.finite(radius) && radius>0, "target radius must be one positive finite number")
  add("tolerance", length(tol)==1L && is.finite(tol) && tol>=0, "tolerance must be non-negative")
  add("selection_basis", identical(as.character(contract$selection_basis), "substantively_interpretable_fixed"), "P1.4 fixed topology requires substantively_interpretable_fixed")
  add("interpretation", length(contract$interpretation)==1L && nchar(trimws(contract$interpretation))>=60L, "interpretation must contain at least 60 characters")
  add("decision_rationale", length(contract$decision_rationale)==1L && nchar(trimws(contract$decision_rationale))>=60L, "decision rationale must contain at least 60 characters")
  add("topology_axes", length(axes)>=1L && !anyNA(axes) && all(nzchar(axes)) && !anyDuplicated(axes), "topology axes must be nonblank and unique")
  add("scaling", length(contract$scaling)==1L && nzchar(trimws(contract$scaling)), "scaling must be declared")
  add("metric", length(contract$metric)==1L && nzchar(trimws(contract$metric)), "metric must be declared")
  add("precision_policy", identical(as.character(contract$precision_policy), "substantive_rounding"), "precision policy must record substantive_rounding")
  add("pipeline_reference", length(pref)==1L && is.finite(pref) && pref>0, "a finite non-binding pipeline reference must be recorded")
  add("pipeline_reference_method", length(contract$pipeline_reference_method)==1L && nchar(trimws(contract$pipeline_reference_method))>=20L, "pipeline reference method must be described")
  review_band_valid <- length(lo)==1L && length(hi)==1L && is.finite(lo) && is.finite(hi) && lo>0 && hi>lo
  add("review_band_valid", review_band_valid, "review band must contain two positive finite ordered bounds; the band is diagnostic and non-binding")
  expected_position <- tdabm_urc_review_band_position(radius, lo, hi)
  recorded_position <- as.character(contract$review_band_position %||% NA_character_)
  add("review_band_position_recorded", review_band_valid && length(recorded_position)==1L && !is.na(recorded_position) && identical(recorded_position, expected_position), paste0("approved-radius position relative to the non-binding review band must be recorded as ", expected_position %||% "UNAVAILABLE", "; being outside the band does not invalidate explicit user approval"))
  sha <- tolower(trimws(as.character(contract$diagnostic_audit_sha256 %||% "")))
  add("diagnostic_audit_sha256", length(sha)==1L && grepl("^[0-9a-f]{64}$", sha), "diagnostic audit SHA-256 must be recorded")
  add("approved_by", length(contract$approved_by)==1L && nzchar(trimws(contract$approved_by)), "approver must be recorded")
  add("approved_at", length(contract$approved_at)==1L && nzchar(trimws(contract$approved_at)), "approval date/time must be recorded")
  add("automatic_selection_false", identical(contract$automatic_selection_performed, FALSE), "automatic radius selection must be FALSE")
  add("optimality_claim_none", toupper(as.character(contract$optimality_claim)) == "NONE", "optimality claim must be NONE")
  if (isTRUE(stop_on_error) && !all(rows$pass)) {
    bad <- rows[!rows$pass,,drop=FALSE]
    stop(paste(c("User radius contract failed:", paste0("- ",bad$check,": ",bad$detail)),collapse="\n"), call.=FALSE)
  }
  rows
}

tdabm_urc_table <- function(contract) {
  fields <- names(contract)
  data.frame(field=fields, value=vapply(fields,function(f) paste(as.character(contract[[f]]),collapse=";"),character(1)), stringsAsFactors=FALSE)
}
