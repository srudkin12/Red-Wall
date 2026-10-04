# ==============================================================================
# TDABMClaimContracts.R
# ==============================================================================
# Application-neutral contracts for checking that prose claims mention complete
# sets of positive integer identifiers. These helpers deliberately avoid passing
# vector-valued regular-expression patterns to grep()/grepl().
# ==============================================================================

tdabm_normalize_positive_integer_ids <- function(
  ids,
  name = "integer IDs",
  allow_empty = FALSE
) {
  if (is.null(ids) || length(ids) == 0L) {
    if (isTRUE(allow_empty)) return(integer(0))
    stop(name, " must contain at least one ID.", call. = FALSE)
  }

  if (is.factor(ids)) ids <- as.character(ids)
  if (is.logical(ids) || is.list(ids) || is.complex(ids)) {
    stop(name, " must be supplied as integer, numeric, factor or digit-only character values.", call. = FALSE)
  }

  if (is.character(ids)) {
    raw_text <- trimws(ids)
    if (anyNA(raw_text) || any(!nzchar(raw_text)) || any(!grepl("^[0-9]+$", raw_text))) {
      stop(name, " character values must contain positive integer digits only.", call. = FALSE)
    }
    numeric_ids <- suppressWarnings(as.numeric(raw_text))
  } else if (is.numeric(ids) || is.integer(ids)) {
    numeric_ids <- as.numeric(ids)
  } else {
    stop(name, " has an unsupported type: ", typeof(ids), call. = FALSE)
  }

  if (anyNA(numeric_ids) || any(!is.finite(numeric_ids))) {
    stop(name, " must not contain missing or non-finite values.", call. = FALSE)
  }
  if (any(abs(numeric_ids - round(numeric_ids)) > sqrt(.Machine$double.eps))) {
    stop(name, " must contain integer-like values only.", call. = FALSE)
  }
  if (any(numeric_ids <= 0) || any(numeric_ids > .Machine$integer.max)) {
    stop(name, " must contain positive integers within R's integer range.", call. = FALSE)
  }

  integer_ids <- as.integer(round(numeric_ids))
  if (anyDuplicated(integer_ids)) {
    duplicates <- sort(unique(integer_ids[duplicated(integer_ids)]))
    stop(
      name,
      " must be unique; duplicated IDs: ",
      paste(duplicates, collapse = ", "),
      call. = FALSE
    )
  }

  sort(integer_ids)
}

tdabm_claim_integer_id_membership <- function(
  claim,
  expected_ids,
  ids_name = "expected integer IDs"
) {
  ids <- tdabm_normalize_positive_integer_ids(
    expected_ids,
    name = ids_name,
    allow_empty = FALSE
  )

  if (is.null(claim) || length(claim) == 0L) {
    text <- NA_character_
  } else {
    if (length(claim) != 1L) {
      stop("claim must be a scalar character value.", call. = FALSE)
    }
    text <- as.character(claim[[1L]])
  }

  if (is.na(text) || !nzchar(trimws(text))) {
    out <- rep(FALSE, length(ids))
    names(out) <- as.character(ids)
    return(out)
  }

  out <- vapply(
    ids,
    function(id) {
      # Numeric-token look-arounds are stricter than \b for identifiers:
      # ID 1 does not match 10, 11, 21, 01, 1.0, -1, +1 or alphanumeric B1.
      # Ranges such as 1-3 are deliberately not treated as explicit mentions.
      pattern <- paste0("(?<![A-Za-z0-9.+-])", id, "(?![A-Za-z0-9.+-])")
      grepl(pattern, text, perl = TRUE)
    },
    logical(1)
  )
  names(out) <- as.character(ids)
  out
}

tdabm_claim_contains_all_integer_ids <- function(
  claim,
  expected_ids,
  ids_name = "expected integer IDs"
) {
  membership <- tdabm_claim_integer_id_membership(
    claim = claim,
    expected_ids = expected_ids,
    ids_name = ids_name
  )
  isTRUE(all(membership))
}

tdabm_claim_missing_integer_ids <- function(
  claim,
  expected_ids,
  ids_name = "expected integer IDs"
) {
  membership <- tdabm_claim_integer_id_membership(
    claim = claim,
    expected_ids = expected_ids,
    ids_name = ids_name
  )
  as.integer(names(membership)[!membership])
}
