# ==============================================================================
# TDABMValueContracts.R
# ==============================================================================
# Generic value, vector, table, and prose-construction contracts.
#
# Purpose:
#   * distinguish scalar operations from vector operations explicitly;
#   * preserve vector length and missing-value position during formatting;
#   * prevent silent R recycling in paper-facing text;
#   * avoid Inf/-Inf summaries when a group contains no finite observations;
#   * provide reusable table preflights for every TDABM application adapter.
#
# No application-specific variable, observation, radius, or exclusion choice
# belongs in this module.
# ============================================================================== 

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) return(y)
  if (tryCatch(all(is.na(x)), error = function(e) FALSE)) return(y)
  x
}

tdabm_vc_assert_scalar_integer <- function(x, name, minimum = NULL, maximum = NULL) {
  if (length(x) != 1L || is.na(x) || !is.finite(x) || abs(x - round(x)) > 1e-8) {
    stop(name, " must be one finite integer-like value.", call. = FALSE)
  }
  value <- as.integer(round(x))
  if (!is.null(minimum) && value < minimum) {
    stop(name, " must be at least ", minimum, ".", call. = FALSE)
  }
  if (!is.null(maximum) && value > maximum) {
    stop(name, " must be at most ", maximum, ".", call. = FALSE)
  }
  value
}

tdabm_vc_assert_scalar_logical <- function(x, name) {
  if (length(x) != 1L || !is.logical(x) || is.na(x)) {
    stop(name, " must be TRUE or FALSE.", call. = FALSE)
  }
  isTRUE(x)
}

tdabm_vc_as_numeric <- function(x, name = "x", allow_coercion = FALSE) {
  if (is.null(x)) return(numeric(0))
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))
  if (!isTRUE(allow_coercion)) {
    stop(name, " must be numeric or integer.", call. = FALSE)
  }
  out <- suppressWarnings(as.numeric(x))
  introduced <- !is.na(x) & is.na(out)
  if (any(introduced)) {
    stop(name, " contains values that cannot be converted to numeric.", call. = FALSE)
  }
  out
}

tdabm_format_number_vector <- function(
  x,
  digits = 2L,
  na_label = "NA",
  integer_tolerance = 1e-8,
  big_mark = ",",
  decimal_mark = ".",
  allow_coercion = FALSE
) {
  digits <- tdabm_vc_assert_scalar_integer(digits, "digits", minimum = 0L, maximum = 15L)
  if (length(na_label) != 1L || is.na(na_label)) {
    stop("na_label must be one non-missing character value.", call. = FALSE)
  }
  if (length(integer_tolerance) != 1L || !is.finite(integer_tolerance) || integer_tolerance < 0) {
    stop("integer_tolerance must be one non-negative finite number.", call. = FALSE)
  }

  original_names <- names(x)
  values <- tdabm_vc_as_numeric(x, name = "x", allow_coercion = allow_coercion)
  if (length(values) == 0L) return(character(0))

  # Prevent negative zero after rounding.
  values[is.finite(values) & abs(values) <= integer_tolerance] <- 0
  result <- rep(as.character(na_label), length(values))
  finite <- is.finite(values)
  integer_like <- finite & abs(values - round(values)) <= integer_tolerance
  decimal_like <- finite & !integer_like

  if (any(integer_like)) {
    result[integer_like] <- formatC(
      round(values[integer_like]),
      format = "f",
      digits = 0L,
      big.mark = big_mark,
      decimal.mark = decimal_mark
    )
  }
  if (any(decimal_like)) {
    result[decimal_like] <- formatC(
      round(values[decimal_like], digits = digits),
      format = "f",
      digits = digits,
      big.mark = big_mark,
      decimal.mark = decimal_mark
    )
  }

  if (!is.null(original_names)) names(result) <- original_names
  result
}

tdabm_format_number_scalar <- function(
  x,
  digits = 2L,
  na_label = "NA",
  name = "x",
  integer_tolerance = 1e-8,
  big_mark = ",",
  decimal_mark = ".",
  allow_coercion = FALSE
) {
  if (is.null(x) || length(x) == 0L) return(as.character(na_label))
  if (length(x) != 1L) {
    stop(
      name, " must be scalar for scalar formatting; received length ", length(x),
      ". Use tdabm_format_number_vector() for a column.",
      call. = FALSE
    )
  }
  unname(tdabm_format_number_vector(
    x,
    digits = digits,
    na_label = na_label,
    integer_tolerance = integer_tolerance,
    big_mark = big_mark,
    decimal_mark = decimal_mark,
    allow_coercion = allow_coercion
  ))[[1L]]
}

tdabm_first_nonmissing <- function(x, default = NA, drop_blank = FALSE) {
  if (is.null(x) || length(x) == 0L) return(default)
  keep <- !is.na(x)
  if (isTRUE(drop_blank)) keep <- keep & nzchar(trimws(as.character(x)))
  index <- which(keep)
  if (!length(index)) return(default)
  x[[index[[1L]]]]
}

tdabm_first_nonblank_character <- function(x, default = NA_character_) {
  value <- tdabm_first_nonmissing(x, default = default, drop_blank = TRUE)
  as.character(value)
}

tdabm_finite_numeric <- function(x, name = "x", allow_coercion = FALSE) {
  values <- tdabm_vc_as_numeric(x, name = name, allow_coercion = allow_coercion)
  values[is.finite(values)]
}

tdabm_safe_mean <- function(x, empty = NA_real_, allow_coercion = FALSE) {
  values <- tdabm_finite_numeric(x, allow_coercion = allow_coercion)
  if (!length(values)) return(as.numeric(empty))
  mean(values)
}

tdabm_safe_min <- function(x, empty = NA_real_, allow_coercion = FALSE) {
  values <- tdabm_finite_numeric(x, allow_coercion = allow_coercion)
  if (!length(values)) return(as.numeric(empty))
  min(values)
}

tdabm_safe_max <- function(x, empty = NA_real_, allow_coercion = FALSE) {
  values <- tdabm_finite_numeric(x, allow_coercion = allow_coercion)
  if (!length(values)) return(as.numeric(empty))
  max(values)
}

tdabm_safe_sd <- function(x, empty = NA_real_, allow_coercion = FALSE) {
  values <- tdabm_finite_numeric(x, allow_coercion = allow_coercion)
  if (length(values) < 2L) return(as.numeric(empty))
  stats::sd(values)
}

tdabm_safe_mode <- function(
  x,
  empty = NA_character_,
  drop_na = TRUE,
  drop_blank = TRUE,
  ties = c("sorted_first", "first_observed")
) {
  ties <- match.arg(ties)
  values <- as.character(x)
  if (isTRUE(drop_na)) values <- values[!is.na(values)]
  if (isTRUE(drop_blank)) values <- values[nzchar(trimws(values))]
  if (!length(values)) return(as.character(empty))
  counts <- table(values, useNA = "no")
  winners <- names(counts)[counts == max(counts)]
  if (length(winners) == 1L) return(winners)
  if (ties == "sorted_first") return(sort(winners, method = "radix")[[1L]])
  values[match(TRUE, values %in% winners)]
}

tdabm_paste0_strict <- function(..., sep = "") {
  pieces <- list(...)
  if (!length(pieces)) return(character(0))
  lengths <- vapply(pieces, length, integer(1))
  target <- max(lengths)
  if (target == 0L) return(character(0))
  invalid <- lengths != 1L & lengths != target
  if (any(invalid)) {
    stop(
      "Strict text construction rejected incompatible vector lengths: ",
      paste(lengths, collapse = ", "),
      ". Only scalars and vectors of the common target length are allowed.",
      call. = FALSE
    )
  }
  recycled <- lapply(pieces, function(x) {
    if (length(x) == target) return(as.character(x))
    rep(as.character(x), target)
  })
  do.call(paste, c(recycled, list(sep = sep)))
}

tdabm_assert_equal_lengths <- function(..., names = NULL, allow_scalar = FALSE) {
  values <- list(...)
  lengths <- vapply(values, length, integer(1))
  if (is.null(names)) names <- paste0("value", seq_along(values))
  if (length(names) != length(values)) stop("names must match the number of values.", call. = FALSE)
  target <- if (length(lengths)) max(lengths) else 0L
  pass <- if (isTRUE(allow_scalar)) all(lengths %in% c(1L, target)) else length(unique(lengths)) <= 1L
  if (!pass) {
    stop(
      "Length contract failed: ",
      paste0(names, "=", lengths, collapse = "; "),
      call. = FALSE
    )
  }
  invisible(target)
}

tdabm_table_contract <- function(
  data,
  table_name = "table",
  required_columns = character(0),
  minimum_rows = 0L,
  exact_rows = NULL,
  unique_key = character(0),
  nonmissing_columns = character(0),
  finite_columns = character(0),
  positive_columns = character(0),
  nonnegative_columns = character(0),
  integer_like_columns = character(0),
  stop_on_error = TRUE
) {
  if (!is.data.frame(data)) {
    stop(table_name, " must be a data.frame-compatible object.", call. = FALSE)
  }
  x <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  rows <- list()
  add <- function(check, pass, detail) {
    rows[[length(rows) + 1L]] <<- data.frame(
      table = table_name,
      check = as.character(check),
      pass = isTRUE(pass),
      detail = as.character(detail),
      stringsAsFactors = FALSE
    )
  }

  minimum_rows <- tdabm_vc_assert_scalar_integer(minimum_rows, "minimum_rows", minimum = 0L)
  add("minimum_rows", nrow(x) >= minimum_rows, paste0("observed=", nrow(x), "; required>=", minimum_rows))
  if (!is.null(exact_rows)) {
    exact_rows <- tdabm_vc_assert_scalar_integer(exact_rows, "exact_rows", minimum = 0L)
    add("exact_rows", nrow(x) == exact_rows, paste0("observed=", nrow(x), "; required=", exact_rows))
  }

  missing_required <- setdiff(required_columns, names(x))
  add(
    "required_columns",
    length(missing_required) == 0L,
    if (length(missing_required)) paste(missing_required, collapse = "; ") else "all present"
  )

  available <- function(cols) intersect(cols, names(x))
  unavailable <- function(cols) setdiff(cols, names(x))

  if (length(unique_key)) {
    missing_key <- unavailable(unique_key)
    if (length(missing_key)) {
      add("unique_key_columns", FALSE, paste("missing", paste(missing_key, collapse = "; ")))
    } else {
      key_frame <- x[, unique_key, drop = FALSE]
      key_missing <- !stats::complete.cases(key_frame)
      key_text <- do.call(paste, c(lapply(key_frame, as.character), sep = "\r"))
      duplicate_key <- duplicated(key_text) | duplicated(key_text, fromLast = TRUE)
      add("unique_key_nonmissing", !any(key_missing), paste0("missing_rows=", sum(key_missing)))
      add("unique_key_unique", !any(duplicate_key), paste0("duplicate_rows=", sum(duplicate_key)))
    }
  }

  for (column in nonmissing_columns) {
    if (!column %in% names(x)) {
      add(paste0("nonmissing:", column), FALSE, "column missing")
    } else {
      value <- x[[column]]
      missing <- is.na(value)
      if (is.character(value)) missing <- missing | !nzchar(trimws(value))
      add(paste0("nonmissing:", column), !any(missing), paste0("missing_or_blank=", sum(missing)))
    }
  }

  numeric_check <- function(columns, kind) {
    for (column in columns) {
      if (!column %in% names(x)) {
        add(paste0(kind, ":", column), FALSE, "column missing")
        next
      }
      value <- x[[column]]
      if (!(is.numeric(value) || is.integer(value))) {
        add(paste0(kind, ":", column), FALSE, paste0("class=", paste(class(value), collapse = "/")))
        next
      }
      finite <- is.finite(value)
      pass <- switch(
        kind,
        finite = all(finite),
        positive = all(finite & value > 0),
        nonnegative = all(finite & value >= 0),
        integer_like = all(finite & abs(value - round(value)) <= 1e-8),
        FALSE
      )
      add(
        paste0(kind, ":", column),
        pass,
        paste0("n=", length(value), "; nonfinite=", sum(!finite))
      )
    }
  }
  numeric_check(finite_columns, "finite")
  numeric_check(positive_columns, "positive")
  numeric_check(nonnegative_columns, "nonnegative")
  numeric_check(integer_like_columns, "integer_like")

  report <- do.call(rbind, rows)
  pass <- nrow(report) > 0L && all(report$pass %in% TRUE)
  if (!pass && isTRUE(stop_on_error)) {
    failed <- report[!report$pass, , drop = FALSE]
    stop(
      table_name, " failed its value contract: ",
      paste0(failed$check, " [", failed$detail, "]", collapse = " | "),
      call. = FALSE
    )
  }
  list(pass = pass, report = report, data = x)
}
