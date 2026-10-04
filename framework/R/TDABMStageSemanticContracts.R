# ==============================================================================
# TDABMStageSemanticContracts.R
# ==============================================================================
# Generic semantic-output gate helpers.
#
# A process exit status is necessary but not sufficient evidence that a stage
# produced the outputs it promised.  Applications declare their own expected
# files, counts and status-table rules using these helpers.  No application-
# specific filenames or substantive choices belong in this framework module.
# ==============================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

tdabm_stage_files <- function(root, pattern) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  files <- files[file.exists(files) & !dir.exists(files)]
  if (!length(files)) return(character())
  normalized <- normalizePath(files, winslash = "/", mustWork = TRUE)
  rel <- substring(normalized, nchar(root) + 2L)
  files[grepl(pattern, rel, perl = TRUE)]
}

tdabm_stage_required_file_check <- function(root, relative_path, minimum_bytes = 1L) {
  file <- file.path(root, relative_path)
  pass <- file.exists(file) && !dir.exists(file) && isTRUE(file.info(file)$size >= minimum_bytes)
  data.frame(
    check = paste0("required_file:", relative_path),
    pass = pass,
    detail = normalizePath(file, winslash = "/", mustWork = FALSE),
    stringsAsFactors = FALSE
  )
}

tdabm_stage_minimum_count_check <- function(root, pattern, minimum_count, label) {
  files <- tdabm_stage_files(root, pattern)
  data.frame(
    check = as.character(label),
    pass = length(files) >= as.integer(minimum_count),
    detail = paste0(length(files), " files matched; minimum ", as.integer(minimum_count)),
    stringsAsFactors = FALSE
  )
}

tdabm_stage_read_csv <- function(file) {
  if (!file.exists(file)) return(NULL)
  tryCatch(
    utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
}

tdabm_stage_scan_status_table <- function(
  file,
  failure_pattern = "(^|_)(fail|error|missing|not_created|not_found|invalid)($|_)",
  status_column_pattern = "status|error|diagnostic"
) {
  x <- tdabm_stage_read_csv(file)
  if (is.null(x)) return(list(pass = FALSE, message = "status file missing or unreadable", data = NULL))
  if (!nrow(x)) return(list(pass = FALSE, message = "status file is empty", data = x))
  status_cols <- grep(status_column_pattern, names(x), ignore.case = TRUE, value = TRUE)
  bad <- FALSE
  if (length(status_cols)) {
    vals <- tolower(trimws(as.character(unlist(x[status_cols], use.names = FALSE))))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    bad <- any(grepl(failure_pattern, vals, perl = TRUE))
  }
  list(
    pass = !bad,
    message = if (bad) "status table contains a declared failure value" else "status table contains no declared failure value",
    data = x
  )
}

tdabm_stage_log_absence_check <- function(log_file, forbidden_pattern, label) {
  if (is.null(log_file) || !file.exists(log_file)) {
    return(data.frame(check = label, pass = FALSE, detail = "log file missing", stringsAsFactors = FALSE))
  }
  lines <- readLines(log_file, warn = FALSE)
  hits <- grep(forbidden_pattern, lines, ignore.case = TRUE, perl = TRUE, value = TRUE)
  data.frame(
    check = as.character(label),
    pass = length(hits) == 0L,
    detail = if (length(hits)) paste(utils::head(hits, 5L), collapse = " | ") else "no forbidden marker found",
    stringsAsFactors = FALSE
  )
}

tdabm_stage_contract_result <- function(stage, checks) {
  if (is.null(checks) || !nrow(checks)) {
    checks <- data.frame(
      check = "contract_has_checks",
      pass = FALSE,
      detail = "Application supplied no semantic checks.",
      stringsAsFactors = FALSE
    )
  }
  checks$stage <- as.character(stage)
  checks <- checks[, c("stage", "check", "pass", "detail"), drop = FALSE]
  checks$pass <- as.logical(checks$pass)
  list(pass = nrow(checks) > 0L && all(checks$pass %in% TRUE), report = checks)
}
