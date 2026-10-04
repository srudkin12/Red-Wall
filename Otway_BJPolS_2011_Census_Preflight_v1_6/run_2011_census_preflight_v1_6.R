#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, warn = 1)

# =============================================================================
# Otway–Rudkin BJPolS: 2011 Census preflight / harmonisation v1.6
# =============================================================================
# Preflight only. No 2011 Ball Mapper topology is constructed here.
#
# Hard principles:
#   * use the frozen Corrected31D frame as the 2021 reference;
#   * obtain 2011 Census data from official Nomis standard tables;
#   * match the 571 frozen constituencies by GSS code;
#   * do not silently substitute categories;
#   * expose unresolved definition/denominator issues as AMBER gates;
#   * write enough raw material and provenance for a later forensic audit.
# =============================================================================

fail <- function(...) stop(paste0(...), call. = FALSE)
msg  <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(...), "\n", sep = "")

script_path <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", a, value = TRUE)
  if (length(f)) return(normalizePath(sub("^--file=", "", f[1]), mustWork = TRUE))
  normalizePath("run_2011_census_preflight_v1_6.R", mustWork = TRUE)
}
script_dir <- dirname(script_path())
out_dir <- file.path(script_dir, "outputs")
raw_dir <- file.path(out_dir, "raw_nomis")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

safe_read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
}

norm_text <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("[[:space:]]+", " ", x))
}

char_code <- function(x) {
  if (is.numeric(x)) return(format(x, scientific = FALSE, trim = TRUE, digits = 22))
  trimws(as.character(x))
}

num_env <- function(name, default) {
  z <- Sys.getenv(name, unset = "")
  if (!nzchar(z)) return(as.integer(default))
  v <- suppressWarnings(as.integer(z))
  if (!is.finite(v) || v < 1L) fail("Invalid positive integer in ", name, ": ", z)
  v
}

hash_file <- function(path) {
  exe <- Sys.which("sha256sum")
  if (nzchar(exe)) {
    z <- suppressWarnings(system2(exe, shQuote(path), stdout = TRUE, stderr = FALSE))
    if (length(z)) return(strsplit(z[1], "[[:space:]]+")[[1]][1])
  }
  unname(tools::md5sum(path))
}

find_repo_root <- function(start_dir) {
  p <- normalizePath(start_dir, mustWork = TRUE)
  for (i in 0:8) {
    fw <- file.path(p, "framework", "R", "TDABMRadiusDiagnostics.R")
    app <- file.path(p, "applications", "Otway_Rudkin_2024_Corrected31D")
    if (file.exists(fw) && dir.exists(app)) return(p)
    q <- dirname(p)
    if (identical(q, p)) break
    p <- q
  }
  fail("Could not discover TDABM_Portability root from ", start_dir)
}


# Locate the authoritative frozen P1.4-A input without assuming that one
# particular retained sub-version exists.
discover_frozen_input <- function(app_dir, app_id, topology_vars) {
  override <- Sys.getenv("OTWAY_FROZEN_INPUT", unset = "")
  candidates <- character()

  if (nzchar(override)) candidates <- c(candidates, override)

  candidates <- c(
    candidates,
    file.path(app_dir, "frozen_input", "p1_4_a_v1_1",
              paste0(app_id, "_input_frozen_P1_4_A_v1_1.csv")),
    file.path(app_dir, "frozen_input", "p1_4_a_v1_0",
              paste0(app_id, "_input_frozen_P1_4_A_v1_0.csv"))
  )

  fi_root <- file.path(app_dir, "frozen_input")
  if (dir.exists(fi_root)) {
    candidates <- c(
      candidates,
      list.files(fi_root,
                 pattern = "_input_frozen_P1_4_A_v1_[0-9]+[.]csv$",
                 recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    )
  }

  candidates <- unique(candidates[file.exists(candidates)])
  if (!length(candidates)) {
    fail("No P1.4-A frozen input CSV found under ", fi_root,
         ". You may set OTWAY_FROZEN_INPUT to an explicit authoritative CSV path.")
  }

  audit <- vector("list", length(candidates))
  valid <- logical(length(candidates))
  for (i in seq_along(candidates)) {
    p <- normalizePath(candidates[i], mustWork = TRUE)
    d <- tryCatch(safe_read_csv(p), error = function(e) NULL)
    nr <- if (is.null(d)) NA_integer_ else nrow(d)
    has_id <- !is.null(d) && "ONSConstID" %in% names(d)
    nvars <- if (is.null(d)) 0L else sum(topology_vars %in% names(d))
    ok <- !is.null(d) && nr == 571L && has_id &&
          length(topology_vars) == 31L && nvars == 31L
    valid[i] <- ok
    audit[[i]] <- data.frame(
      candidate = p,
      bytes = file.info(p)$size,
      rows = nr,
      has_ONSConstID = has_id,
      topology_variables_found = nvars,
      valid = ok,
      sha256_or_md5 = hash_file(p),
      stringsAsFactors = FALSE
    )
  }
  audit <- do.call(rbind, audit)

  if (!any(valid)) {
    write_csv(audit, file.path(out_dir, "00a_frozen_input_discovery_audit.csv"))
    fail("Frozen-input candidates were found, but none passed the ",
         "571-row / ONSConstID / 31-variable contract. ",
         "See 00a_frozen_input_discovery_audit.csv.")
  }

  vi <- which(valid)
  if (nzchar(override)) {
    ov <- normalizePath(override, mustWork = TRUE)
    oi <- match(ov, audit$candidate)
    if (!is.na(oi) && valid[oi]) {
      chosen_i <- oi
    } else {
      fail("OTWAY_FROZEN_INPUT was supplied but did not pass the frozen-input contract.")
    }
  } else {
    v10 <- vi[grepl("p1_4_a_v1_0|P1_4_A_v1_0",
                    audit$candidate[vi], ignore.case = TRUE)]
    chosen_i <- if (length(v10)) v10[1] else vi[1]
  }

  audit$selected <- seq_len(nrow(audit)) == chosen_i
  write_csv(audit, file.path(out_dir, "00a_frozen_input_discovery_audit.csv"))
  list(path = audit$candidate[chosen_i], audit = audit)
}

urlencode <- function(x) utils::URLencode(as.character(x), reserved = TRUE)

redact_url <- function(url) {
  gsub("([?&]uid=)[^&]+", "\\1REDACTED", url, ignore.case = TRUE)
}

build_nomis_url <- function(dataset, params) {
  base <- paste0("https://www.nomisweb.co.uk/api/v01/dataset/", dataset, ".data.csv")
  uid <- Sys.getenv("NOMIS_UID", unset = "")
  if (nzchar(uid)) params$uid <- uid
  kv <- vapply(names(params), function(k) paste0(k, "=", urlencode(params[[k]])), character(1))
  paste0(base, "?", paste(kv, collapse = "&"))
}

fetch_url <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) return(invisible(TRUE))
  tmp <- paste0(dest, ".part")
  unlink(tmp)
  curl <- Sys.which("curl")
  rc <- 1L
  if (nzchar(curl)) {
    args <- c("-L", "--fail", "--silent", "--show-error", "--retry", "3",
              "--retry-delay", "2", "--connect-timeout", "30", "--max-time", "240",
              "-o", shQuote(tmp), shQuote(url))
    rc <- suppressWarnings(system2(curl, args = args))
  } else {
    rc <- tryCatch({
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE, method = "libcurl")
      0L
    }, error = function(e) 1L)
  }
  if (!identical(as.integer(rc), 0L) || !file.exists(tmp) || file.info(tmp)$size <= 0) {
    unlink(tmp)
    fail("Nomis download failed: ", redact_url(url))
  }
  file.rename(tmp, dest)
  invisible(TRUE)
}

read_nomis_csv <- function(path) {
  txt <- readLines(path, n = 8L, warn = FALSE)
  first_nonblank <- txt[nzchar(trimws(txt))][1]
  if (length(first_nonblank) && !is.na(first_nonblank) &&
      grepl("^\\s*<(?:!DOCTYPE|html|head|body)", first_nonblank, ignore.case = TRUE, perl = TRUE)) {
    fail("Nomis returned HTML rather than CSV in ", basename(path),
         ". The raw response has been retained in outputs/raw_nomis for diagnosis.")
  }
  if (length(txt) && any(grepl("^Error:", txt, ignore.case = TRUE))) {
    fail("Nomis returned an error in ", basename(path), ": ", paste(txt, collapse = " | "))
  }
  d <- tryCatch(safe_read_csv(path), error = function(e) fail("Cannot parse Nomis CSV ", basename(path), ": ", conditionMessage(e)))
  if (!nrow(d)) fail("Nomis CSV has no rows: ", basename(path))
  names(d) <- toupper(names(d))
  d
}

pick_col <- function(d, choices, required = TRUE) {
  hit <- choices[choices %in% names(d)]
  if (length(hit)) return(hit[1])
  if (required) fail("Missing required column; tried: ", paste(choices, collapse = ", "))
  NA_character_
}

filter_total_sex <- function(d, dataset) {
  code_col <- c("C_SEX", "SEX")
  code_col <- code_col[code_col %in% names(d)]
  name_col <- c("C_SEX_NAME", "SEX_NAME")
  name_col <- name_col[name_col %in% names(d)]

  if (!length(code_col) && !length(name_col)) return(d)

  # If the API request already constrained sex to total, only one category
  # should remain. Keep it without making assumptions about its internal code.
  if (length(code_col)) {
    vals <- unique(char_code(d[[code_col[1]]]))
    vals <- vals[nzchar(vals)]
    if (length(vals) == 1L) return(d)
    if (any(vals == "0")) return(d[char_code(d[[code_col[1]]]) == "0", , drop = FALSE])
  }

  if (length(name_col)) {
    nm <- norm_text(d[[name_col[1]]])
    ok <- grepl("all categories.*sex|all persons|both sexes|^total$|^all$", nm, perl = TRUE)
    if (any(ok)) return(d[ok, , drop = FALSE])
  }

  vals <- if (length(name_col)) unique(d[[name_col[1]]]) else unique(d[[code_col[1]]])
  fail("Could not identify all-persons sex category in ", dataset,
       ". Values: ", paste(vals, collapse = "; "))
}

make_manifest_row <- function(dataset, batch, url, path, d) {
  data.frame(
    dataset = dataset,
    batch = batch,
    url_redacted = redact_url(url),
    file = substring(normalizePath(path, mustWork = TRUE), nchar(normalizePath(out_dir, mustWork = TRUE)) + 2L),
    bytes = file.info(path)$size,
    sha256_or_md5 = hash_file(path),
    rows = nrow(d),
    stringsAsFactors = FALSE
  )
}

nomis_cache_candidates <- function(dataset, batch_no) {
  fname <- sprintf("%s_batch_%03d.csv", dataset, batch_no)
  explicit <- Sys.getenv("OTWAY_NOMIS_CACHE", unset = "")
  sibling_v15 <- file.path(dirname(script_dir),
                           "Otway_BJPolS_2011_Census_Preflight_v1_5",
                           "outputs", "raw_nomis")
  sibling_v14 <- file.path(dirname(script_dir),
                           "Otway_BJPolS_2011_Census_Preflight_v1_4",
                           "outputs", "raw_nomis")
  sibling_v13 <- file.path(dirname(script_dir),
                           "Otway_BJPolS_2011_Census_Preflight_v1_3",
                           "outputs", "raw_nomis")
  roots <- unique(c(if (nzchar(explicit)) explicit else character(),
                    sibling_v15, sibling_v14, sibling_v13))
  file.path(roots, fname)
}

fetch_dataset <- function(dataset, geography_codes, batch_size,
                          filters = list(RURAL_URBAN = "0", MEASURES = "20100", date = "latest")) {
  geos <- char_code(geography_codes)
  idx <- split(seq_along(geos), ceiling(seq_along(geos) / batch_size))
  tabs <- vector("list", length(idx))
  mans <- vector("list", length(idx))
  j <- 0L
  for (ii in idx) {
    j <- j + 1L
    params <- c(list(geography = paste(geos[ii], collapse = ",")), filters)
    url <- build_nomis_url(dataset, params)
    dest <- file.path(raw_dir, sprintf("%s_batch_%03d.csv", dataset, j))

    cache <- nomis_cache_candidates(dataset, j)
    cache <- cache[file.exists(cache)]
    used_cache <- FALSE

    if (length(cache)) {
      # Validate the cached response before reusing it.
      cached_data <- tryCatch(read_nomis_csv(cache[1]), error = function(e) NULL)
      if (!is.null(cached_data)) {
        ok_copy <- file.copy(cache[1], dest, overwrite = TRUE)
        if (!ok_copy) fail("Could not copy cached Nomis response: ", cache[1])
        msg("Reusing cached ", dataset, " batch ", j, "/", length(idx),
            " from ", cache[1])
        used_cache <- TRUE
      }
    }

    if (!used_cache) {
      msg("Downloading ", dataset, " batch ", j, "/", length(idx),
          " (", length(ii), " constituencies)")
      fetch_url(url, dest)
    }

    dd <- filter_total_sex(read_nomis_csv(dest), dataset)
    tabs[[j]] <- dd
    mr <- make_manifest_row(dataset, j, url, dest, dd)
    mr$source_mode <- if (used_cache) "reused_validated_cache" else "downloaded"
    mr$cache_source <- if (used_cache) normalizePath(cache[1], mustWork = TRUE) else ""
    mans[[j]] <- mr
  }
  list(data = do.call(rbind, tabs), manifest = do.call(rbind, mans))
}

classification_cols <- function(d, dataset) {
  # Dataset-specific content dimensions take precedence over age.  LC6110EW
  # and LC6114EW retain C_AGE=total in the returned data, so C_AGE must not
  # be mistaken for the substantive classification.
  priority <- c("CELL", "C_INDGPUK11", "C_NSSEC", "C_AGE")
  cc <- priority[priority %in% names(d)]
  if (!length(cc)) {
    fail("Could not identify a supported classification dimension in ", dataset,
         ". Expected one of ", paste(priority, collapse = ", "), ".")
  }
  code <- cc[1]
  cn <- paste0(code, "_NAME")
  if (!cn %in% names(d)) fail("Classification-name column missing in ", dataset, ": ", cn)
  list(code = code, name = cn)
}

ensure_unique_geo_cell <- function(d, dataset) {
  gc <- pick_col(d, c("GEOGRAPHY_CODE"))
  cls <- classification_cols(d, dataset)
  key <- paste(char_code(d[[gc]]), char_code(d[[cls$code]]), sep = "::")
  if (anyDuplicated(key)) {
    dup <- unique(key[duplicated(key)])
    fail(dataset, " retains duplicate geography-classification rows after total filters. First duplicates: ",
         paste(head(dup, 10), collapse = "; "))
  }
  d
}

cell_catalogue <- function(d, dataset) {
  cls <- classification_cols(d, dataset)
  x <- unique(data.frame(
    dataset = dataset,
    source_dimension = cls$code,
    CELL = char_code(d[[cls$code]]),
    CELL_NAME = as.character(d[[cls$name]]),
    stringsAsFactors = FALSE))
  x <- x[order(suppressWarnings(as.numeric(x$CELL)), x$CELL), , drop = FALSE]
  rownames(x) <- NULL
  x
}

find_cells <- function(catalog, include, exclude = NULL, expected = NULL, label = "mapping") {
  n <- norm_text(catalog$CELL_NAME)
  ok <- rep(TRUE, nrow(catalog))
  for (p in include) ok <- ok & grepl(p, n, perl = TRUE)
  if (length(exclude)) for (p in exclude) ok <- ok & !grepl(p, n, perl = TRUE)
  hit <- catalog[ok, , drop = FALSE]
  status <- if (is.null(expected) || nrow(hit) == expected) "PASS" else "FAIL"
  list(cells = hit$CELL, rows = hit,
       audit = data.frame(item = label, expected_matches = if (is.null(expected)) NA_integer_ else expected,
                          observed_matches = nrow(hit), status = status,
                          matched_labels = paste(hit$CELL_NAME, collapse = " || "), stringsAsFactors = FALSE))
}

obs_value <- function(d) {
  oc <- pick_col(d, c("OBS_VALUE", "OBSVALUE"))
  suppressWarnings(as.numeric(d[[oc]]))
}

geo_code <- function(d) char_code(d[[pick_col(d, c("GEOGRAPHY_CODE"))]])
cell_code <- function(d) {
  cls <- classification_cols(d, "aggregation input")
  char_code(d[[cls$code]])
}

aggregate_pct <- function(d, numerator_cells, denominator_cells, target) {
  g <- geo_code(d); c <- cell_code(d); v <- obs_value(d)
  num <- tapply(v[c %in% numerator_cells], g[c %in% numerator_cells], sum, na.rm = FALSE)
  den <- tapply(v[c %in% denominator_cells], g[c %in% denominator_cells], sum, na.rm = FALSE)
  ids <- sort(unique(g))
  out <- data.frame(GEOGRAPHY_CODE = ids, value = NA_real_, stringsAsFactors = FALSE)
  out$value <- 100 * as.numeric(num[ids]) / as.numeric(den[ids])
  names(out)[2] <- target
  out
}

aggregate_value <- function(d, cells, target) {
  g <- geo_code(d); c <- cell_code(d); v <- obs_value(d)
  num <- tapply(v[c %in% cells], g[c %in% cells], sum, na.rm = FALSE)
  ids <- sort(unique(g))
  out <- data.frame(GEOGRAPHY_CODE = ids, value = as.numeric(num[ids]), stringsAsFactors = FALSE)
  names(out)[2] <- target
  out
}

infer_share_scale <- function(x) {
  q <- suppressWarnings(stats::quantile(abs(as.numeric(x)), 0.95, na.rm = TRUE, names = FALSE))
  if (!is.finite(q)) return("unknown")
  if (q <= 1.5) "fraction" else "percent"
}

match_2021_scale <- function(pct, frozen_x) {
  sc <- infer_share_scale(frozen_x)
  if (identical(sc, "fraction")) pct / 100 else pct
}

pct_for_diag <- function(x) {
  if (identical(infer_share_scale(x), "fraction")) 100 * as.numeric(x) else as.numeric(x)
}

block_diag <- function(dat, vars, block, census) {
  if (!all(vars %in% names(dat))) return(data.frame(census=census, block=block, n_vars=length(vars), available=FALSE))
  z <- vapply(vars, function(v) pct_for_diag(dat[[v]]), numeric(nrow(dat)))
  if (is.vector(z)) z <- matrix(z, ncol = 1)
  rs <- rowSums(z)
  data.frame(census=census, block=block, n_vars=length(vars), available=TRUE,
             min_sum=min(rs,na.rm=TRUE), q05=as.numeric(quantile(rs,.05,na.rm=TRUE)),
             median_sum=median(rs,na.rm=TRUE), mean_sum=mean(rs,na.rm=TRUE),
             q95=as.numeric(quantile(rs,.95,na.rm=TRUE)), max_sum=max(rs,na.rm=TRUE),
             n_missing=sum(!is.finite(rs)), stringsAsFactors=FALSE)
}

scan_local_provenance <- function(app_dir) {
  ff <- list.files(app_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  okext <- grepl("\\.(R|r|md|txt|csv|tsv|json|ya?ml)$", ff)
  ff <- ff[okext & file.exists(ff)]
  ff <- ff[file.info(ff)$size <= 3e6]
  pats <- c("qualifications_level1", "NSSEC_higher_managerial", "ethnic_group_white_british", "population_density", "C2021", "census 2021")
  ans <- list(); k <- 0L
  for (f in ff) {
    x <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
    if (!length(x)) next
    hit <- which(vapply(x, function(s) any(grepl(paste(pats, collapse="|"), s, ignore.case=TRUE, perl=TRUE)), logical(1)))
    if (!length(hit)) next
    for (h in head(hit, 30)) {
      k <- k + 1L
      ans[[k]] <- data.frame(file=substring(normalizePath(f,mustWork=TRUE),nchar(normalizePath(app_dir,mustWork=TRUE))+2L),
                             line=h, text=substr(x[h],1,500), stringsAsFactors=FALSE)
    }
  }
  if (!length(ans)) data.frame(file=character(),line=integer(),text=character()) else do.call(rbind, ans)
}

# -----------------------------------------------------------------------------
# Locate frozen 2021 project input
# -----------------------------------------------------------------------------
repo <- find_repo_root(script_dir)
app_dir <- file.path(repo, "applications", "Otway_Rudkin_2024_Corrected31D")
adapter <- file.path(app_dir, "R", "OtwayRudkin2024Adapter.R")
contract_file <- file.path(app_dir, "config", "application_contract.csv")
if (!file.exists(adapter) || !file.exists(contract_file)) fail("Corrected31D application files missing.")
source(adapter)
config <- or24_read_contract(contract_file)
app_id <- or24_get(config, "application_id")
topology_vars <- or24_split(or24_get(config, "topology_variables"))
if (length(topology_vars) != 31L) fail("Application contract does not define exactly 31 topology variables.")
frozen_info <- discover_frozen_input(app_dir, app_id, topology_vars)
frozen <- frozen_info$path
msg("Selected authoritative frozen input: ", frozen)
dat21 <- safe_read_csv(frozen)
if (nrow(dat21) != 571L) fail("Expected 571 frozen constituencies, found ", nrow(dat21))
if (!all(topology_vars %in% names(dat21))) fail("Frozen topology-variable contract is not 31 complete columns.")
if (!"ONSConstID" %in% names(dat21)) fail("Frozen input lacks ONSConstID.")

batch_size <- num_env("OTWAY_NOMIS_BATCH", 40L)

write_csv(data.frame(
  field=c("repo_root","app_dir","application_id","frozen_input","frozen_input_sha256_or_md5","frozen_input_discovery_rule","frozen_rows","topology_coordinates","nomis_batch_size","nomis_uid_supplied","run_started"),
  value=c(repo,app_dir,app_id,frozen,hash_file(frozen),"validated discovery; explicit override if supplied, otherwise retained p1_4_a_v1_0 preferred",nrow(dat21),length(topology_vars),batch_size,nzchar(Sys.getenv("NOMIS_UID",unset="")),format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
  stringsAsFactors=FALSE), file.path(out_dir,"00_environment_and_provenance.csv"))

write_csv(data.frame(variable=topology_vars,
                     class=vapply(dat21[topology_vars],function(x)paste(class(x),collapse="/"),character(1)),
                     nonmissing=vapply(dat21[topology_vars],function(x)sum(!is.na(x)),integer(1)),
                     unique_nonmissing=vapply(dat21[topology_vars],function(x)length(unique(x[!is.na(x)])),integer(1)),
                     p05=vapply(dat21[topology_vars],function(x)as.numeric(quantile(x,.05,na.rm=TRUE)),numeric(1)),
                     median=vapply(dat21[topology_vars],function(x)median(x,na.rm=TRUE),numeric(1)),
                     p95=vapply(dat21[topology_vars],function(x)as.numeric(quantile(x,.95,na.rm=TRUE)),numeric(1)),
                     inferred_share_scale=vapply(dat21[topology_vars],infer_share_scale,character(1)),
                     stringsAsFactors=FALSE), file.path(out_dir,"01_2021_frozen_variable_inventory.csv"))

blocks <- list(
  age=grep("^age_",topology_vars,value=TRUE),
  education=grep("^qualifications_",topology_vars,value=TRUE),
  industry=grep("^industry_",topology_vars,value=TRUE),
  nssec=grep("^NSSEC_",topology_vars,value=TRUE)
)
block21 <- do.call(rbind, lapply(names(blocks), function(b) block_diag(dat21, blocks[[b]], b, "2021_frozen")))
write_csv(block21, file.path(out_dir,"02_2021_block_sum_diagnostics.csv"))
write_csv(scan_local_provenance(app_dir), file.path(out_dir,"03_local_2021_provenance_trace.csv"))

# -----------------------------------------------------------------------------
# Official 2011 Nomis data acquisition
# -----------------------------------------------------------------------------
# v1.1 change:
# Query the frozen 571 constituencies directly by their official GSS codes.
# Nomis supports geographic queries using official GSS codes.  This is both
# simpler and safer than relying on an internal TYPE selector to reconstruct
# the full parliamentary-constituency universe.
frozen_ids <- char_code(dat21$ONSConstID)
if (length(frozen_ids) != 571L || anyDuplicated(frozen_ids)) {
  fail("Frozen constituency identifier frame is not 571 unique ONS/GSS codes.")
}
if (!all(grepl("^[EW][0-9]{8}$", frozen_ids))) {
  bad <- frozen_ids[!grepl("^[EW][0-9]{8}$", frozen_ids)]
  fail("Frozen ONSConstID contains non England/Wales GSS-style codes. First: ",
       paste(head(bad, 10), collapse = "; "))
}

# Nomis REST API uses NM_* dataset identifiers. The Census table labels are
# retained as analysis-facing keys so manuscript provenance remains readable.
# IDs verified against the Nomis dataset catalogue on 2026-10-02.
specs <- list(
  C2011QS102EW=list(api_id="NM_143_1", table="QS102EW - Population density",
                    filters=list(RURAL_URBAN="total",MEASURES="20100",time="latest")),
  C2011QS103EW=list(api_id="NM_503_1", table="QS103EW - Age by single year",
                    filters=list(RURAL_URBAN="total",MEASURES="20100",time="latest")),
  C2011KS201EW=list(api_id="NM_608_1", table="KS201EW - Ethnic group",
                    filters=list(RURAL_URBAN="total",MEASURES="20100",time="latest")),
  C2011KS501EW=list(api_id="NM_623_1", table="KS501EW - Qualifications and students",
                    filters=list(RURAL_URBAN="total",MEASURES="20100",time="latest")),
  C2011LC6110EW=list(api_id="NM_1047_1", table="LC6110EW - Industry by age",
                    filters=list(C_AGE="total",MEASURES="20100",time="latest")),
  C2011LC6114EW=list(api_id="NM_1051_1", table="LC6114EW - NS-SeC by age",
                    filters=list(C_AGE="total",MEASURES="20100",time="latest"))
)

dataset_contract <- do.call(rbind, lapply(names(specs), function(k) data.frame(
  analysis_key=k,
  census_table=specs[[k]]$table,
  nomis_api_id=specs[[k]]$api_id,
  filters=paste(names(specs[[k]]$filters), unlist(specs[[k]]$filters),
                sep="=", collapse="&"),
  stringsAsFactors=FALSE)))
write_csv(dataset_contract,file.path(out_dir,"03a_nomis_dataset_contract.csv"))

tables <- list(); manifests <- list(); catalogs <- list(); coverage_rows <- list()
for (ds in names(specs)) {
  api_id <- specs[[ds]]$api_id
  msg("Retrieving ", ds, " [", api_id, "] directly for frozen 571 GSS constituency codes...")
  z <- fetch_dataset(api_id, frozen_ids, batch_size, specs[[ds]]$filters)
  z$data <- ensure_unique_geo_cell(z$data,ds)
  tables[[ds]] <- z$data
  z$manifest$analysis_key <- ds
  z$manifest$census_table <- specs[[ds]]$table
  z$manifest$nomis_api_id <- api_id
  manifests[[length(manifests)+1L]] <- z$manifest
  catalogs[[ds]] <- cell_catalogue(z$data,ds)

  gc <- pick_col(z$data, c("GEOGRAPHY_CODE"))
  got <- sort(unique(char_code(z$data[[gc]])))
  missing <- setdiff(frozen_ids, got)
  extra <- setdiff(got, frozen_ids)
  coverage_rows[[length(coverage_rows)+1L]] <- data.frame(
    dataset=ds,
    requested_n=length(frozen_ids),
    returned_unique_geographies=length(got),
    matched_frozen_n=sum(frozen_ids %in% got),
    missing_frozen_n=length(missing),
    extra_geographies_n=length(extra),
    missing_codes=paste(missing,collapse=";"),
    extra_codes=paste(extra,collapse=";"),
    status=if(length(missing)==0L && length(extra)==0L)"PASS" else "FAIL",
    stringsAsFactors=FALSE)
}
manifest <- do.call(rbind,manifests)
write_csv(manifest,file.path(out_dir,"04_nomis_download_manifest.csv"))
catalog <- do.call(rbind,catalogs)
write_csv(catalog,file.path(out_dir,"05_nomis_cell_catalogue.csv"))
coverage <- do.call(rbind,coverage_rows)
write_csv(coverage,file.path(out_dir,"06_dataset_geography_coverage.csv"))

if (any(coverage$status != "PASS")) {
  bad <- coverage[coverage$status != "PASS",,drop=FALSE]
  fail("One or more Nomis tables failed the frozen-571 geography coverage gate: ",
       paste(bad$dataset, collapse=", "),
       ". See 06_dataset_geography_coverage.csv.")
}

# Constituency-name audit comes from QS102EW but GSS code is authoritative.
seed <- tables[["C2011QS102EW"]]
gc <- pick_col(seed,c("GEOGRAPHY_CODE"))
gn <- pick_col(seed,c("GEOGRAPHY_NAME"),required=FALSE)
gi <- pick_col(seed,c("GEOGRAPHY"),required=FALSE)
geo_frame <- unique(data.frame(
  GEOGRAPHY=if(is.na(gi)) NA_character_ else char_code(seed[[gi]]),
  GEOGRAPHY_CODE=char_code(seed[[gc]]),
  GEOGRAPHY_NAME=if(is.na(gn)) NA_character_ else as.character(seed[[gn]]),
  stringsAsFactors=FALSE))
geo_frame <- geo_frame[geo_frame$GEOGRAPHY_CODE %in% frozen_ids,,drop=FALSE]
geo_frame <- geo_frame[!duplicated(geo_frame$GEOGRAPHY_CODE),,drop=FALSE]
geo_frame <- geo_frame[order(match(geo_frame$GEOGRAPHY_CODE,frozen_ids)),,drop=FALSE]
write_csv(geo_frame,file.path(out_dir,"07_2011_frozen_constituency_frame_from_nomis.csv"))

mi <- match(frozen_ids, geo_frame$GEOGRAPHY_CODE)
match_audit <- data.frame(
  ONSConstID=frozen_ids,
  frozen_name=if ("ConstituencyName" %in% names(dat21)) as.character(dat21$ConstituencyName) else NA_character_,
  matched=!is.na(mi),
  nomis_internal_geography=ifelse(is.na(mi),NA_character_,geo_frame$GEOGRAPHY[mi]),
  nomis_2011_name=ifelse(is.na(mi),NA_character_,geo_frame$GEOGRAPHY_NAME[mi]),
  normalised_name_equal=ifelse(
    is.na(mi) | !("ConstituencyName" %in% names(dat21)) | is.na(geo_frame$GEOGRAPHY_NAME[mi]),
    NA,
    norm_text(dat21$ConstituencyName) == norm_text(geo_frame$GEOGRAPHY_NAME[mi])),
  stringsAsFactors=FALSE)
write_csv(match_audit,file.path(out_dir,"08_constituency_match_audit.csv"))
if (any(!match_audit$matched)) {
  fail("Only ",sum(match_audit$matched),"/571 frozen constituencies matched Nomis by GSS code.")
}

# -----------------------------------------------------------------------------
# Mapping helpers and explicit category audits
# -----------------------------------------------------------------------------
mapping_audits <- list(); ma <- 0L
add_audit <- function(x) { ma <<- ma + 1L; mapping_audits[[ma]] <<- x }

cat_for <- function(ds) catalogs[[ds]]

# Density
# v1.3 catalogue label is exactly:
# "Density (number of persons per hectare)".
# Match the normalised published label explicitly; do not infer from area.
m_density <- find_cells(cat_for("C2011QS102EW"),
                        c("^density number of persons per hectare$"),
                        expected=1L, label="population_density")
add_audit(m_density$audit)

# Age: require single-year categories.
agecat <- cat_for("C2011QS103EW")
age_norm <- norm_text(agecat$CELL_NAME)
is_total_age <- grepl("all categories|all usual residents|total",age_norm)
age_value <- rep(NA_integer_,nrow(agecat))
age_value[grepl("under 1",age_norm)] <- 0L
for (i in seq_len(nrow(agecat))) {
  if (is_total_age[i] || is.finite(age_value[i])) next
  s <- age_norm[i]
  mm <- regmatches(s,regexpr("(^| )([0-9]{1,3})( |$)",s,perl=TRUE))
  if (length(mm) && nzchar(mm)) age_value[i] <- as.integer(gsub("[^0-9]","",mm))
}
age_single_cells <- agecat[!is_total_age & is.finite(age_value),,drop=FALSE]
age_single_cells$AGE <- age_value[!is_total_age & is.finite(age_value)]
age_exact_ok <- all(0:99 %in% age_single_cells$AGE) && any(age_single_cells$AGE >= 100) && !any(grepl(" to ",age_norm[!is_total_age]))
add_audit(data.frame(item="age_single_year_coverage",expected_matches=101L,observed_matches=length(unique(age_single_cells$AGE)),
                     status=if(age_exact_ok)"PASS" else "FAIL",matched_labels=paste(head(age_single_cells$CELL_NAME,10),collapse=" || "),stringsAsFactors=FALSE))
age_total <- find_cells(agecat,c("all"),exclude=c("single year"),expected=1L,label="age_denominator_total")
if (nrow(age_total$rows)!=1L) {
  # Some QS103 layouts name the denominator simply "All usual residents".
  age_total <- find_cells(agecat,c("all usual residents"),expected=1L,label="age_denominator_total")
}
add_audit(age_total$audit)

# Ethnicity
ethcat <- cat_for("C2011KS201EW")
# v1.3 catalogue shows the denominator as exactly "All usual residents".
# Use that published label explicitly rather than requiring the words
# "ethnic" or "all categories".
eth_total <- find_cells(ethcat,c("^all usual residents$"),
                        expected=1L,label="ethnicity_denominator_total")
add_audit(eth_total$audit)
eth_white <- find_cells(ethcat,c("white","english","welsh","scottish","northern","irish","british"),expected=1L,label="ethnic_group_white_british")
add_audit(eth_white$audit)

# Qualifications
qcat <- cat_for("C2011KS501EW")
q_total <- find_cells(qcat,c("all categories","highest level of qualification"),expected=1L,label="qualifications_denominator_total")
add_audit(q_total$audit)
q_specs <- list(
  qualifications_none=list(inc=c("no qualifications"),exc=NULL),
  qualifications_level1=list(inc=c("level 1 qualifications"),exc=NULL),
  qualifications_level2=list(inc=c("level 2 qualifications"),exc=NULL),
  qualifications_level3=list(inc=c("level 3 qualifications"),exc=NULL),
  qualifications_level4=list(inc=c("level 4 qualifications"),exc=NULL),
  qualifications_other=list(inc=c("other qualifications"),exc=NULL)
)
q_map <- list()
for (nm in names(q_specs)) {
  z <- find_cells(qcat,q_specs[[nm]]$inc,q_specs[[nm]]$exc,expected=1L,label=nm)
  q_map[[nm]] <- z$cells; add_audit(z$audit)
}
q_apprentice <- find_cells(qcat,c("apprenticeship"),expected=1L,label="qualifications_apprenticeship_source")
add_audit(q_apprentice$audit)
add_audit(data.frame(
  item="qualifications_other_harmonised_components",
  expected_matches=2L,
  observed_matches=length(c(q_map[["qualifications_other"]], q_apprentice$cells)),
  status=if(length(q_map[["qualifications_other"]])==1L && length(q_apprentice$cells)==1L)"PASS" else "FAIL",
  matched_labels=paste(qcat$CELL_NAME[qcat$CELL %in% c(q_map[["qualifications_other"]],q_apprentice$cells)],collapse=" || "),
  stringsAsFactors=FALSE))

# Industry: use LC6110EW, whose population base is usual residents aged 16+
# in employment, matching the Census 2021 industry population base.
icat <- cat_for("C2011LC6110EW")
inorm <- norm_text(icat$CELL_NAME)
i_total_idx <- grepl("all categories|all usual residents|all persons|total",inorm)
industry_assign <- rep(NA_character_,nrow(icat))
industry_targets <- c("industry_agriculture_energy_water","industry_manufacturing","industry_construction",
                      "industry_distribution_hotels_restaurants","industry_transport_communication",
                      "industry_financial_realestate_professional_administrative",
                      "industry_public_administration_education_health","industry_other")
for (i in which(!i_total_idx)) {
  s <- inorm[i]
  hits <- character()
  if (grepl("agriculture|forestry|fishing|energy and water|mining|quarrying|electricity|gas|water supply",s)) hits <- c(hits,industry_targets[1])
  if (grepl("manufacturing",s)) hits <- c(hits,industry_targets[2])
  if (grepl("construction",s)) hits <- c(hits,industry_targets[3])
  if (grepl("distribution hotels and restaurants|wholesale|retail|motor vehicles|accommodation|food service",s)) hits <- c(hits,industry_targets[4])
  if (grepl("transport and communication|transport|storage|information and communication",s)) hits <- c(hits,industry_targets[5])
  if (grepl("financial real estate professional and administrative|financial|insurance|real estate|professional scientific|technical activities|administrative and support",s)) hits <- c(hits,industry_targets[6])
  if (grepl("public administration education and health|public administration|defence|education|human health|social work",s)) hits <- c(hits,industry_targets[7])
  if (grepl("arts entertainment|recreation|other service|activities of households|extraterritorial| other$",s)) hits <- c(hits,industry_targets[8])
  hits <- unique(hits)
  if (length(hits)==1L) industry_assign[i] <- hits
  if (length(hits)>1L) industry_assign[i] <- paste(hits,collapse="||")
}
industry_category_audit <- data.frame(dataset="C2011LC6110EW",CELL=icat$CELL,CELL_NAME=icat$CELL_NAME,
                                      is_total=i_total_idx,assigned_target=industry_assign,
                                      assignment_status=ifelse(i_total_idx,"TOTAL",ifelse(is.na(industry_assign),"UNMAPPED",ifelse(grepl("\\|\\|",industry_assign),"AMBIGUOUS","MAPPED"))),stringsAsFactors=FALSE)
write_csv(industry_category_audit,file.path(out_dir,"08a_industry_category_mapping.csv"))
industry_map <- lapply(industry_targets,function(t) icat$CELL[industry_assign==t]); names(industry_map)<-industry_targets
for (nm in names(industry_map)) add_audit(data.frame(item=nm,expected_matches=NA_integer_,observed_matches=length(industry_map[[nm]]),
                                                     status=if(length(industry_map[[nm]])>=1L)"PASS" else "FAIL",
                                                     matched_labels=paste(icat$CELL_NAME[icat$CELL%in%industry_map[[nm]]],collapse=" || "),stringsAsFactors=FALSE))
i_total <- find_cells(icat,c("^all categories industry$|^all categories$|^all industries$"),
                      expected=1L,label="industry_denominator_total_age16plus")
if (nrow(i_total$rows)!=1L) i_total <- find_cells(icat,c("all"),expected=1L,label="industry_denominator_total_age16plus")
add_audit(i_total$audit)
industry_mapping_ok <- !any(industry_category_audit$assignment_status %in% c("UNMAPPED","AMBIGUOUS")) && all(vapply(industry_map,length,integer(1))>=1L)

# NS-SEC: only the nine top-level frozen categories, excluding nested subcategories.
ncat <- cat_for("C2011LC6114EW")
nnorm <- norm_text(ncat$CELL_NAME)
ns_patterns <- list(
  NSSEC_higher_managerial_admin_professional="^1 higher managerial administrative and professional occupations$",
  NSSEC_lower_managerial_admin_professional="^2 lower managerial administrative and professional occupations$",
  NSSEC_intermediate_occupations="^3 intermediate occupations$",
  NSSEC_small_employers_own_account_workers="^4 small employers and own account workers$",
  NSSEC_lower_supervisory_technical="^5 lower supervisory and technical occupations$",
  NSSEC_semiroutine="^6 semi routine occupations$",
  NSSEC_routine="^7 routine occupations$",
  NSSEC_never_worked_long_term_unemployed="^8 never worked and long term unemployed$",
  NSSEC_full_time_students="full time students$"
)
ns_map <- list()
for (nm in names(ns_patterns)) {
  ok <- grepl(ns_patterns[[nm]],nnorm,perl=TRUE)
  # full-time students must not be an umbrella containing an unrelated phrase.
  if (nm=="NSSEC_full_time_students") ok <- ok & !grepl("schoolchildren",nnorm)
  hit <- ncat[ok,,drop=FALSE]
  ns_map[[nm]] <- hit$CELL
  add_audit(data.frame(item=nm,expected_matches=1L,observed_matches=nrow(hit),status=if(nrow(hit)==1L)"PASS" else "FAIL",
                       matched_labels=paste(hit$CELL_NAME,collapse=" || "),stringsAsFactors=FALSE))
}
n_total <- find_cells(ncat,c("^all categories ns sec$|^all categories nssec$|^all categories$"),
                      expected=1L,label="nssec_denominator_total_age16plus")
if (nrow(n_total$rows)!=1L) n_total <- find_cells(ncat,c("all"),expected=1L,label="nssec_denominator_total_age16plus")
add_audit(n_total$audit)

mapping_audit <- do.call(rbind,mapping_audits)
write_csv(mapping_audit,file.path(out_dir,"09_category_mapping_audit.csv"))

# -----------------------------------------------------------------------------
# Build 2011 candidate frame
# -----------------------------------------------------------------------------
parts <- list()

# density
# QS102EW reports persons per hectare. The frozen 2021 variable register
# records population_density as residents per square kilometre.
# Convert explicitly before freezing: 1 square kilometre = 100 hectares.
if (length(m_density$cells)==1L) {
  density_raw <- aggregate_value(
    tables$C2011QS102EW,m_density$cells,
    "__population_density_persons_per_hectare")
  density_km2 <- density_raw
  density_km2$population_density <-
    100 * density_km2$`__population_density_persons_per_hectare`
  density_km2$`__population_density_persons_per_hectare` <- NULL
  parts[["population_density"]] <- density_km2
  parts[["__population_density_persons_per_hectare"]] <- density_raw
}

# age exact groups
if (age_exact_ok && length(age_total$cells)==1L) {
  a_ranges <- list(age_0_15=c(0,15),age_16_24=c(16,24),age_25_34=c(25,34),age_35_49=c(35,49),age_50_64=c(50,64),age_65_over=c(65,Inf))
  for (nm in names(a_ranges)) {
    lo <- a_ranges[[nm]][1]; hi <- a_ranges[[nm]][2]
    cells <- age_single_cells$CELL[age_single_cells$AGE>=lo & age_single_cells$AGE<=hi]
    parts[[nm]] <- aggregate_pct(tables$C2011QS103EW,cells,age_total$cells,nm)
  }
}

if (length(eth_white$cells)==1L && length(eth_total$cells)==1L) parts[["ethnic_group_white_british"]] <- aggregate_pct(tables$C2011KS201EW,eth_white$cells,eth_total$cells,"ethnic_group_white_british")

if (length(q_total$cells)==1L) {
  for (nm in names(q_map)) {
    if (nm=="qualifications_other") next
    if (length(q_map[[nm]])==1L) {
      parts[[nm]] <- aggregate_pct(tables$C2011KS501EW,q_map[[nm]],q_total$cells,nm)
    }
  }
  if (length(q_map[["qualifications_other"]])==1L && length(q_apprentice$cells)==1L) {
    harmonised_other_cells <- c(q_map[["qualifications_other"]], q_apprentice$cells)
    parts[["qualifications_other"]] <- aggregate_pct(
      tables$C2011KS501EW,harmonised_other_cells,q_total$cells,"qualifications_other")
    parts[["__qualifications_apprenticeship_pct"]] <- aggregate_pct(
      tables$C2011KS501EW,q_apprentice$cells,q_total$cells,"__qualifications_apprenticeship_pct")
    parts[["__qualifications_raw_other_pct"]] <- aggregate_pct(
      tables$C2011KS501EW,q_map[["qualifications_other"]],q_total$cells,"__qualifications_raw_other_pct")
  }
}

if (industry_mapping_ok && length(i_total$cells)==1L) {
  for (nm in names(industry_map)) parts[[nm]] <- aggregate_pct(
    tables$C2011LC6110EW,industry_map[[nm]],i_total$cells,nm)
}

if (length(n_total$cells)==1L) {
  for (nm in names(ns_map)) if (length(ns_map[[nm]])==1L) parts[[nm]] <- aggregate_pct(
    tables$C2011LC6114EW,ns_map[[nm]],n_total$cells,nm)
}

cand <- data.frame(ONSConstID=frozen_ids,stringsAsFactors=FALSE)
cand$ConstituencyName_2021_frame <- if("ConstituencyName"%in%names(dat21)) as.character(dat21$ConstituencyName) else NA_character_
cand$ConstituencyName_2011_Nomis <- geo_frame$GEOGRAPHY_NAME[match(frozen_ids,geo_frame$GEOGRAPHY_CODE)]
for (nm in names(parts)) {
  p <- parts[[nm]]
  cand[[nm]] <- p[[nm]][match(cand$ONSConstID,p$GEOGRAPHY_CODE)]
}

# Convert percentage candidates to the same numerical scale as frozen variables.
share_targets <- setdiff(intersect(names(cand),topology_vars),"population_density")
for (nm in share_targets) cand[[nm]] <- match_2021_scale(cand[[nm]],dat21[[nm]])

# Keep target columns in frozen order; absent targets are explicit NA columns.
for (nm in setdiff(topology_vars,names(cand))) cand[[nm]] <- NA_real_
extra_cols <- intersect(c("__population_density_persons_per_hectare",
                          "__qualifications_apprenticeship_pct",
                          "__qualifications_raw_other_pct"),names(cand))
cand <- cand[,c("ONSConstID","ConstituencyName_2021_frame","ConstituencyName_2011_Nomis",topology_vars,extra_cols),drop=FALSE]
write_csv(cand,file.path(out_dir,"10_2011_harmonised_candidate.csv"))

# -----------------------------------------------------------------------------
# Crosswalk and diagnostics
# -----------------------------------------------------------------------------
crosswalk <- data.frame(target_variable=topology_vars,block=NA_character_,dataset=NA_character_,method=NA_character_,provisional_comparability=NA_character_,note=NA_character_,stringsAsFactors=FALSE)
for(i in seq_len(nrow(crosswalk))) {
  v <- crosswalk$target_variable[i]
  if(v=="population_density") {crosswalk$block[i]<-"density";crosswalk$dataset[i]<-"QS102EW";crosswalk$method[i]<-"direct density cell converted persons/hectare to residents/km^2 by x100";crosswalk$provisional_comparability[i]<-"PASS_HARMONISED";crosswalk$note[i]<-"QS102EW publishes persons per hectare; frozen 2021 variable_register.csv records residents_per_square_km. Raw 2011 density is retained in a provenance helper column."}
  else if(grepl("^age_",v)) {crosswalk$block[i]<-"age";crosswalk$dataset[i]<-"QS103EW";crosswalk$method[i]<-"exact aggregation of single-year age cells";crosswalk$provisional_comparability[i]<-if(age_exact_ok)"PASS" else "FAIL";crosswalk$note[i]<-"Target cut-points reproduced exactly from single-year ages."}
  else if(v=="ethnic_group_white_british") {crosswalk$block[i]<-"ethnicity";crosswalk$dataset[i]<-"KS201EW";crosswalk$method[i]<-"direct White British category share";crosswalk$provisional_comparability[i]<-"PASS_CONCEPTUALLY";crosswalk$note[i]<-"Verify frozen 2021 source label in final provenance crosswalk."}
  else if(grepl("^qualifications_",v)) {crosswalk$block[i]<-"education";crosswalk$dataset[i]<-"KS501EW";crosswalk$method[i]<-if(v=="qualifications_other")"official harmonised grouping: apprenticeship + other qualifications" else "direct highest-qualification category share";crosswalk$provisional_comparability[i]<-"PASS_HARMONISED";crosswalk$note[i]<-if(v=="qualifications_other")"Matches the official Census 2021 7a grouping in which apprenticeships are included with Other; 2011 categories are unchanged but combined for cross-census comparability." else "Directly comparable named highest-qualification category; both censuses apply to usual residents aged 16+."}
  else if(grepl("^industry_",v)) {crosswalk$block[i]<-"industry";crosswalk$dataset[i]<-"LC6110EW";crosswalk$method[i]<-"aggregation of published SIC-based industry categories using All categories: Age 16 and over";crosswalk$provisional_comparability[i]<-if(industry_mapping_ok)"PASS_HARMONISED" else "FAIL";crosswalk$note[i]<-"2011 LC6110EW and Census 2021 TS060 both use residents aged 16+ in employment; audit source cells in 08a_industry_category_mapping.csv."}
  else if(grepl("^NSSEC_",v)) {crosswalk$block[i]<-"nssec";crosswalk$dataset[i]<-"LC6114EW";crosswalk$method[i]<-"direct top-level NS-SEC category share using All categories: Age 16 and over";crosswalk$provisional_comparability[i]<-"PASS_HARMONISED";crosswalk$note[i]<-"2011 LC6114EW and Census 2021 TS062 both use usual residents aged 16+."}
}
write_csv(crosswalk,file.path(out_dir,"11_harmonisation_crosswalk.csv"))

block11 <- do.call(rbind,lapply(names(blocks),function(b) block_diag(cand,blocks[[b]],b,"2011_candidate")))
write_csv(rbind(block21,block11),file.path(out_dir,"12_block_sum_diagnostics_2011_2021.csv"))

comp <- list(); kk <- 0L
for(v in topology_vars) {
  kk<-kk+1L
  x<-as.numeric(dat21[[v]]); y<-as.numeric(cand[[v]])
  ok<-is.finite(x)&is.finite(y)
  comp[[kk]]<-data.frame(variable=v,n_complete=sum(ok),
    mean_2011=if(any(ok))mean(y[ok]) else NA_real_,mean_2021=if(any(ok))mean(x[ok]) else NA_real_,
    sd_2011=if(sum(ok)>1)sd(y[ok]) else NA_real_,sd_2021=if(sum(ok)>1)sd(x[ok]) else NA_real_,
    pearson=if(sum(ok)>2)suppressWarnings(cor(x[ok],y[ok],method="pearson")) else NA_real_,
    spearman=if(sum(ok)>2)suppressWarnings(cor(x[ok],y[ok],method="spearman")) else NA_real_,
    note="Diagnostic only: temporal change is expected; correlation is not a harmonisation pass/fail criterion.",stringsAsFactors=FALSE)
}
write_csv(do.call(rbind,comp),file.path(out_dir,"13_2011_2021_variable_diagnostics.csv"))

# Density-unit diagnostic.
raw_density11 <- if("__population_density_persons_per_hectare" %in% names(cand)) {
  as.numeric(cand$`__population_density_persons_per_hectare`)
} else rep(NA_real_, nrow(cand))
density11_km2 <- as.numeric(cand$population_density)
density21_km2 <- as.numeric(dat21$population_density)
density_conversion_error <- max(
  abs(density11_km2 - 100 * raw_density11), na.rm=TRUE)
density_unit_ok <- all(is.finite(raw_density11)) &&
                   all(is.finite(density11_km2)) &&
                   is.finite(density_conversion_error) &&
                   density_conversion_error < 1e-10
density_diag <- data.frame(
  metric=c("2011_raw_persons_per_hectare_mean",
           "2011_harmonised_residents_per_square_km_mean",
           "2021_frozen_residents_per_square_km_mean",
           "2011_conversion_max_absolute_error",
           "2011_2021_density_pearson"),
  value=c(mean(raw_density11,na.rm=TRUE),
          mean(density11_km2,na.rm=TRUE),
          mean(density21_km2,na.rm=TRUE),
          density_conversion_error,
          suppressWarnings(cor(density11_km2,density21_km2,use="complete.obs"))),
  stringsAsFactors=FALSE)
write_csv(density_diag,file.path(out_dir,"14a_density_unit_diagnostics.csv"))

# Qualification omission diagnostic.
qvars <- blocks$education
q21_sum <- rowSums(vapply(qvars,function(v)pct_for_diag(dat21[[v]]),numeric(nrow(dat21))))
q11_sum <- rowSums(vapply(qvars,function(v)pct_for_diag(cand[[v]]),numeric(nrow(cand))))
app11 <- if("__qualifications_apprenticeship_pct"%in%names(cand)) cand$`__qualifications_apprenticeship_pct` else rep(NA_real_,nrow(cand))
rawother11 <- if("__qualifications_raw_other_pct"%in%names(cand)) cand$`__qualifications_raw_other_pct` else rep(NA_real_,nrow(cand))
qual_diag <- data.frame(
  metric=c("2021_harmonised_six_category_sum_median","2021_harmonised_six_category_sum_q05","2021_harmonised_six_category_sum_q95",
           "2011_harmonised_six_category_sum_median","2011_harmonised_six_category_sum_q05","2011_harmonised_six_category_sum_q95",
           "2011_apprenticeship_pct_median","2011_raw_other_pct_median","2011_harmonised_other_pct_median"),
  value=c(median(q21_sum,na.rm=TRUE),quantile(q21_sum,.05,na.rm=TRUE),quantile(q21_sum,.95,na.rm=TRUE),
          median(q11_sum,na.rm=TRUE),quantile(q11_sum,.05,na.rm=TRUE),quantile(q11_sum,.95,na.rm=TRUE),
          median(app11,na.rm=TRUE),median(rawother11,na.rm=TRUE),median(pct_for_diag(cand$qualifications_other),na.rm=TRUE)),
  stringsAsFactors=FALSE)
write_csv(qual_diag,file.path(out_dir,"14_qualification_definition_diagnostics.csv"))

# -----------------------------------------------------------------------------
# Gates
# -----------------------------------------------------------------------------
hard_mapping_fail <- any(mapping_audit$status=="FAIL") || !industry_mapping_ok
all31 <- all(vapply(topology_vars,function(v)any(is.finite(cand[[v]])),logical(1)))
no_missing <- all(vapply(topology_vars,function(v)all(is.finite(cand[[v]])),logical(1)))
frame_ok <- all(match_audit$matched)
dataset_frame_ok <- all(coverage$status=="PASS")
education_sum_ok <- all(abs(q11_sum-100) < 1e-6, na.rm=TRUE) && all(abs(q21_sum-100) < 1e-6, na.rm=TRUE)
industry_sum <- rowSums(vapply(blocks$industry,function(v)pct_for_diag(cand[[v]]),numeric(nrow(cand))))
nssec_sum <- rowSums(vapply(blocks$nssec,function(v)pct_for_diag(cand[[v]]),numeric(nrow(cand))))
industry_denominator_ok <- all(abs(industry_sum-100) < 1e-6, na.rm=TRUE)
nssec_denominator_ok <- all(abs(nssec_sum-100) < 1e-6, na.rm=TRUE)

gates <- data.frame(
  gate=c("nomis_frozen_571_retrieval","frozen_571_code_match","category_mapping_unambiguous","all_31_coordinates_constructed","all_31_coordinates_complete",
         "density_unit_harmonisation","qualifications_definition","industry_denominator_definition","nssec_denominator_definition","overall_preflight"),
  status=c(if(dataset_frame_ok)"PASS" else "FAIL",if(frame_ok)"PASS" else "FAIL",if(!hard_mapping_fail)"PASS" else "FAIL",
           if(all31)"PASS" else "FAIL",if(no_missing)"PASS" else "FAIL",
           if(density_unit_ok)"PASS" else "FAIL",
           if(education_sum_ok)"PASS" else "FAIL",
           if(industry_denominator_ok)"PASS" else "FAIL",
           if(nssec_denominator_ok)"PASS" else "FAIL",
           if(dataset_frame_ok&&frame_ok&&!hard_mapping_fail&&all31&&no_missing&&density_unit_ok&&education_sum_ok&&industry_denominator_ok&&nssec_denominator_ok)
             "PASS_HARMONISATION_READY_FOR_FREEZE" else "FAIL"),
  detail=c(
    paste0("All six Nomis tables returned the exact frozen 571 GSS-code frame=",dataset_frame_ok,"."),
    paste0(sum(match_audit$matched),"/571 frozen ONS constituency IDs matched QS102EW by GSS code."),
    paste0("Mapping FAIL rows=",sum(mapping_audit$status=="FAIL"),"; industry_mapping_ok=",industry_mapping_ok,"."),
    paste0(sum(vapply(topology_vars,function(v)any(is.finite(cand[[v]])),logical(1))),"/31 coordinates have finite values."),
    paste0(sum(vapply(topology_vars,function(v)all(is.finite(cand[[v]])),logical(1))),"/31 coordinates are complete across all 571 constituencies."),
    paste0("QS102EW persons/hectare converted to residents/km^2 by x100; conversion max absolute error=",format(density_conversion_error,scientific=TRUE),"."),
    paste0("2011 apprenticeship is combined with raw Other to implement the official cross-census grouped category; 2011 six-coordinate sum median=",sprintf("%.3f",median(q11_sum,na.rm=TRUE)),"; 2021=",sprintf("%.3f",median(q21_sum,na.rm=TRUE)),"."),
    paste0("2011 LC6110EW uses All categories: Age 16 and over in employment; eight industry shares sum median=",sprintf("%.3f",median(industry_sum,na.rm=TRUE)),"."),
    paste0("2011 LC6114EW uses All categories: Age 16 and over; nine NS-SEC shares sum median=",sprintf("%.3f",median(nssec_sum,na.rm=TRUE)),"."),
    "PASS authorises freezing the harmonised 2011 input for a separately governed 2011 topology/radius stage; it does not import the 2021 radius."),
  stringsAsFactors=FALSE)
write_csv(gates,file.path(out_dir,"15_preflight_gates.csv"))

# Source reference table (public URLs only; no secret UID)
source_refs <- data.frame(
  table=c("QS102EW","QS103EW","KS201EW","KS501EW","LC6110EW","LC6114EW"),
  topic=c("Population density","Age by single year","Ethnic group","Qualifications and students","Industry by age","NS-SEC by age"),
  nomis_api_id=c("NM_143_1","NM_503_1","NM_608_1","NM_623_1","NM_1047_1","NM_1051_1"),
  public_metadata_url=paste0("https://www.nomisweb.co.uk/census/2011/",tolower(c("qs102ew","qs103ew","ks201ew","ks501ew","lc6110ew","lc6114ew"))),
  intended_role=c("Direct density coordinate converted persons/hectare to residents/km^2","Exact aggregation to six age coordinates","White British share",
                  "Six harmonised qualification shares; apprenticeship combined with Other",
                  "Eight industry coordinates using the age-16+ total",
                  "Nine top-level NS-SEC shares using the age-16+ total"),
  stringsAsFactors=FALSE)
write_csv(source_refs,file.path(out_dir,"16_source_reference_table.csv"))

decision_record <- data.frame(
  issue=c("density_unit","qualifications_apprenticeship","industry_age_denominator","nssec_age_denominator"),
  prior_status=c("PASS_WITH_UNIT_AUDIT","AMBER","UNFLAGGED_MISMATCH","AMBER"),
  v1_6_resolution=c(
    "Convert QS102EW persons per hectare to residents per square kilometre by multiplying by 100.",
    "Combine 2011 Apprenticeship with 2011 Other qualifications in qualifications_other.",
    "Replace KS605EW (age 16-74) with LC6110EW and select All categories: Age 16 and over.",
    "Replace KS611EW (age 16-74) with LC6114EW and select All categories: Age 16 and over."
  ),
  basis=c(
    "QS102EW source label is Density (number of persons per hectare); frozen 2021 variable_register.csv records population_density as residents_per_square_km; 1 km^2 = 100 hectares.",
    "ONS Census 2021 grouped highest-qualification classification combines apprenticeships with Other.",
    "Census 2021 industry uses residents aged 16+ in employment; LC6110EW supplies the corresponding 2011 age-16+ base.",
    "Census 2021 NS-SEC uses usual residents aged 16+; LC6114EW supplies the corresponding 2011 age-16+ base."
  ),
  stringsAsFactors=FALSE)
write_csv(decision_record,file.path(out_dir,"17_harmonisation_decision_record.csv"))

# Manuscript-facing summary
get_gate <- function(n) gates$status[gates$gate==n][1]
summary_lines <- c(
  "# 2011 Census preflight / harmonisation summary",
  "",
  paste0("Generated: ",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Scope",
  "This run is a feasibility and harmonisation preflight only. It does not construct a 2011 Ball Mapper topology or select a 2011 radius.",
  "",
  "## Computational gates",
  paste0("- Direct Nomis retrieval of the frozen 571 GSS-code frame across all six tables: **",get_gate("nomis_frozen_571_retrieval"),"**."),
  paste0("- Frozen 571 constituency GSS-code match in QS102EW: **",get_gate("frozen_571_code_match"),"** (",sum(match_audit$matched),"/571)."),
  paste0("- Category mapping: **",get_gate("category_mapping_unambiguous"),"**."),
  paste0("- 31-coordinate construction: **",get_gate("all_31_coordinates_constructed"),"**."),
  paste0("- Complete 571 x 31 candidate frame: **",get_gate("all_31_coordinates_complete"),"**."),
  "",
  "## Resolved harmonisation gates",
  paste0("- Density units: **",get_gate("density_unit_harmonisation"),"**. QS102EW persons/hectare is converted to residents/km^2 by x100; 2011 harmonised mean ",sprintf("%.3f",mean(density11_km2,na.rm=TRUE))," versus frozen 2021 mean ",sprintf("%.3f",mean(density21_km2,na.rm=TRUE)),"."),
  paste0("- Qualifications: **",get_gate("qualifications_definition"),"**. 2011 Apprenticeship is combined with raw Other to match the official grouped cross-census definition. Harmonised six-coordinate sums: 2011 median ",sprintf("%.3f",median(q11_sum,na.rm=TRUE)),"; 2021 median ",sprintf("%.3f",median(q21_sum,na.rm=TRUE)),"."),
  paste0("- Industry denominator: **",get_gate("industry_denominator_definition"),"**. LC6110EW supplies the 2011 age-16+ employed population base."),
  paste0("- NS-SEC denominator: **",get_gate("nssec_denominator_definition"),"**. LC6114EW supplies the 2011 age-16+ population base."),
  "",
  "## Overall",
  paste0("Overall preflight status: **",get_gate("overall_preflight"),"**."),
  "",
  "If PASS, the harmonised 2011 571 x 31 frame is approved for a separate freeze and topology-governance stage. The 2011 radius must still be selected independently from topology-only diagnostics.",
  "",
  "## Production rule if the preflight is approved later",
  "A subsequent 2011 Ball Mapper analysis must govern its radius independently using topology-only diagnostics. It must not import epsilon = 5.50 from the 2021 Corrected31D topology, and graph identity must not be used as the success criterion."
)
writeLines(summary_lines,file.path(out_dir,"PREFLIGHT_SUMMARY.md"),useBytes=TRUE)

# Session + artifact manifest
capture.output(sessionInfo(),file=file.path(out_dir,"sessionInfo.txt"))
outs <- list.files(out_dir,recursive=TRUE,full.names=TRUE)
outs <- outs[file.exists(outs) & !grepl("artifact_manifest\\.csv$",outs)]
artifact <- data.frame(file=substring(normalizePath(outs,mustWork=TRUE),nchar(normalizePath(out_dir,mustWork=TRUE))+2L),
                       bytes=file.info(outs)$size,
                       sha256_or_md5=vapply(outs,hash_file,character(1)),stringsAsFactors=FALSE)
write_csv(artifact,file.path(out_dir,"artifact_manifest.csv"))

msg("2011 Census preflight completed. Overall status: ",get_gate("overall_preflight"))
