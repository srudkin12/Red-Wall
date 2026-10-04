# Otway-Rudkin 2024 application adapter utilities.
# Application-specific logic only. Generic framework source is never modified.

or24_read_contract <- function(path) {
  x <- utils::read.csv(path, stringsAsFactors=FALSE, check.names=FALSE)
  if (!identical(names(x), c("field","value","status")) || anyDuplicated(x$field)) {
    stop("Malformed application_contract.csv", call.=FALSE)
  }
  stats::setNames(as.list(x$value), x$field)
}

or24_get <- function(config, key) {
  x <- config[[key]]
  if (is.null(x) || length(x)!=1L || is.na(x)) stop("Missing contract field: ", key, call.=FALSE)
  as.character(x)
}

or24_split <- function(x) {
  out <- trimws(strsplit(as.character(x), ";", fixed=TRUE)[[1L]])
  out[nzchar(out)]
}

or24_sha256 <- function(path) {
  if (requireNamespace("digest", quietly=TRUE)) {
    return(tolower(unname(digest::digest(path, algo="sha256", file=TRUE))))
  }
  exe <- Sys.which("sha256sum")
  if (!nzchar(exe)) stop("Need digest package or sha256sum.", call.=FALSE)
  out <- system2(exe, shQuote(path), stdout=TRUE, stderr=TRUE)
  tolower(strsplit(out[[1L]], "[[:space:]]+")[[1L]][[1L]])
}

or24_read_source <- function(path) {
  # File is known CP1252. Read bytes under that declared encoding.
  x <- utils::read.csv(
    path, stringsAsFactors=FALSE, check.names=FALSE,
    fileEncoding="CP1252", na.strings=c("NA","")
  )
  x
}

or24_assert_source <- function(x, expected_sha, source_path) {
  stopifnot(is.data.frame(x))
  observed_sha <- or24_sha256(source_path)
  exact <- identical(observed_sha, tolower(expected_sha))
  source_mode <- tolower(trimws(Sys.getenv("OTWAY_SOURCE_MODE", unset="strict")))
  if (!exact && source_mode != "reconstructed") {
    stop(
      "merged_data.csv SHA-256 mismatch. Exact historical replication requires the ",
      "archived source hash. For a user-reconstructed source that satisfies the ",
      "documented schema, set OTWAY_SOURCE_MODE=reconstructed; the canonical ",
      "topology fingerprint will still be checked downstream.",
      call.=FALSE
    )
  }
  if (!exact && source_mode == "reconstructed") {
    warning(
      "Running with a user-reconstructed merged_data.csv. Byte-level source identity ",
      "is not established; downstream canonical-topology validation remains binding.",
      call.=FALSE
    )
  }
  if (nrow(x)!=573L || ncol(x)!=84L) stop("Source shape mismatch: expected 573 x 84.", call.=FALSE)
  if (!"ONSConstID" %in% names(x) || anyNA(x$ONSConstID) || anyDuplicated(as.character(x$ONSConstID))) {
    stop("ONSConstID uniqueness/nonmissing contract failed.", call.=FALSE)
  }
  invisible(TRUE)
}

or24_apply_sample <- function(x) {
  expected <- c(E14000637="Chorley", E14000608="Buckingham")
  got <- x[x$ONSConstID %in% names(expected), c("ONSConstID","ConstituencyName"), drop=FALSE]
  if (nrow(got)!=2L) stop("Expected Speaker/former-Speaker rows not found.", call.=FALSE)
  got <- got[match(names(expected), got$ONSConstID), , drop=FALSE]
  if (!identical(as.character(got$ConstituencyName), unname(expected))) {
    stop("Speaker exclusion IDs do not match expected constituency labels.", call.=FALSE)
  }
  y <- x[!x$ONSConstID %in% names(expected), , drop=FALSE]
  if (nrow(y)!=571L) stop("Final sample is not N=571.", call.=FALSE)
  y
}

or24_minmax <- function(x) {
  x <- as.numeric(x)
  if (any(!is.finite(x))) stop("Min-max input is non-finite.", call.=FALSE)
  r <- range(x)
  if (!is.finite(diff(r)) || diff(r)<=0) stop("Min-max input is constant.", call.=FALSE)
  (x-r[[1L]])/diff(r)
}

or24_historical_left_behind <- function(x) {
  pd <- as.numeric(x$population_density)
  npd <- as.numeric(-scale(pd))
  q02 <- as.numeric(x$qualifications_none) + as.numeric(x$qualifications_level2)
  raw <- rowMeans(cbind(
    npd,
    as.numeric(x$ethnic_group_white_british),
    q02,
    as.numeric(x$NSSEC_routine),
    as.numeric(x$industry_manufacturing)
  ))
  or24_minmax(raw)
}

or24_corrected_left_behind <- function(x) {
  density_inverse_01 <- 1 - or24_minmax(as.numeric(x$population_density))
  below_level3 <- as.numeric(x$qualifications_none) +
    as.numeric(x$qualifications_level1) +
    as.numeric(x$qualifications_level2)
  raw <- rowMeans(cbind(
    density_inverse_01,
    as.numeric(x$ethnic_group_white_british),
    below_level3,
    as.numeric(x$NSSEC_routine),
    as.numeric(x$industry_manufacturing)
  ))
  or24_minmax(raw)
}

or24_make_candidate <- function(x, topology_vars, index_name, index_kind) {
  needed <- unique(c(
    "ONSConstID","ConstituencyName","Country","Region","Winner19",
    topology_vars,
    "Con15","Con17","Con19","Con1517","Con1719",
    "Lab15","Lab17","Lab19","Lab1517","Lab1719","leave"
  ))
  missing <- setdiff(needed, names(x))
  if (length(missing)) stop("Source is missing required columns: ", paste(missing,collapse=";"), call.=FALSE)
  y <- x[,needed,drop=FALSE]
  if (identical(index_kind,"historical")) {
    y[[index_name]] <- or24_historical_left_behind(x)
  } else if (identical(index_kind,"corrected")) {
    y[[index_name]] <- or24_corrected_left_behind(x)
  } else stop("Unknown index_kind.", call.=FALSE)

  # Input freeze is deterministic by stable ID. P1.4-C later reasserts the same
  # stable point-order rule.
  ord <- order(enc2utf8(as.character(y$ONSConstID)), method="radix")
  y <- y[ord,,drop=FALSE]
  rownames(y) <- NULL
  y
}

or24_check_corrected_index <- function(x) {
  explicit <- as.numeric(x$qualifications_none) +
    as.numeric(x$qualifications_level1) +
    as.numeric(x$qualifications_level2)
  if (!"qualifications_below_a_level" %in% names(x)) stop("Source validation aggregate missing.", call.=FALSE)
  diff <- max(abs(explicit-as.numeric(x$qualifications_below_a_level)))
  if (!is.finite(diff) || diff>1e-12) stop("Corrected below-Level-3 validation failed.", call.=FALSE)
  diff
}

or24_write_csv <- function(x,path) {
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  utils::write.csv(x,path,row.names=FALSE,na="",fileEncoding="UTF-8")
  invisible(path)
}
