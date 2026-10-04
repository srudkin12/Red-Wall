options(stringsAsFactors = FALSE)
options(width = 160)

cat("OTWAY_BJPOLS_TARGETED_ADDENDUM_V1_2\n")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

fail <- function(...) stop(paste0(...), call. = FALSE)

script_path <- function() {
  x <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", x[grep("^--file=", x)])
  if (!length(f)) return(normalizePath(".", winslash = "/", mustWork = TRUE))
  normalizePath(f[[1L]], winslash = "/", mustWork = TRUE)
}

find_repo_root <- function(start_dir) {
  p <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
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

num_env <- function(name, default, integer = TRUE, minimum = 1) {
  z <- Sys.getenv(name, "")
  if (!nzchar(z)) z <- as.character(default)
  out <- suppressWarnings(as.numeric(z))
  if (!is.finite(out) || out < minimum) fail("Invalid environment variable ", name, "=", z)
  if (integer) out <- as.integer(out)
  out
}

write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE, na = "")
}

safe_read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
}

norm_name <- function(x) tolower(gsub("[^a-z0-9]+", "", x))

detect_id_col <- function(d) {
  n <- names(d)
  preferred <- c("ONSConstID", "ons_const_id", "constituency_id", "ConstituencyID", "point_id", "id")
  for (p in preferred) if (p %in% n) return(p)
  nn <- norm_name(n)
  hit <- which(nn %in% c("onsconstid", "constituencyid", "pointid"))
  if (length(hit)) return(n[[hit[[1L]]]])
  hit <- grep("ons.*const|const.*id|point.*id", nn)
  if (length(hit)) return(n[[hit[[1L]]]])
  NA_character_
}

as_binary_redwall <- function(x) {
  if (is.logical(x)) return(as.integer(x))
  if (is.numeric(x)) {
    u <- unique(x[is.finite(x)])
    if (all(u %in% c(0, 1))) return(as.integer(x))
  }
  y <- tolower(trimws(as.character(x)))
  yes <- y %in% c("1","true","t","yes","y","red wall","redwall","rw")
  no  <- y %in% c("0","false","f","no","n","","na","none","not red wall","non-red-wall","non red wall")
  if (all(yes | no | is.na(x))) return(as.integer(yes))
  NULL
}

redwall_diag_from_balls <- function(balls, redwall, edges = NULL) {
  if (!length(balls)) fail("Ball list is empty.")
  if (length(redwall) < max(unlist(balls))) fail("Red Wall vector is shorter than ball membership indices.")

  tab <- do.call(rbind, lapply(seq_along(balls), function(i) {
    idx <- balls[[i]]
    nrw <- sum(redwall[idx] == 1L)
    data.frame(
      ball = i,
      size = length(idx),
      redwall = nrw,
      nonredwall = length(idx) - nrw,
      redwall_share = if (length(idx)) nrw / length(idx) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))

  n <- length(redwall)
  mem_count <- integer(n)
  mixed_touch <- logical(n)
  for (b in seq_along(balls)) {
    idx <- balls[[b]]
    mem_count[idx] <- mem_count[idx] + 1L
    if (any(redwall[idx] == 1L) && any(redwall[idx] == 0L)) {
      mixed_touch[idx[redwall[idx] == 1L]] <- TRUE
    }
  }

  rw_idx <- which(redwall == 1L)
  occupied <- which(tab$redwall > 0L)
  rw_memberships <- tab$redwall[occupied]
  p <- rw_memberships / sum(rw_memberships)
  entropy <- if (length(p) <= 1L) 0 else -sum(p * log(p)) / log(length(p))

  comp_count <- NA_integer_
  rw_components <- NA_integer_
  rw_in_largest_component <- NA_real_
  if (!is.null(edges)) {
    cc <- connected_components(length(balls), edges)
    comp_count <- max(cc)
    comp_sizes <- tabulate(cc, nbins = comp_count)
    largest <- which.max(comp_sizes)
    rw_ball_comp <- unique(cc[occupied])
    rw_components <- length(rw_ball_comp)
    rw_membership_by_ball <- tab$redwall
    denom <- sum(rw_membership_by_ball)
    rw_in_largest_component <- if (denom > 0) sum(rw_membership_by_ball[cc == largest]) / denom else NA_real_
  }

  data.frame(
    n_redwall = length(rw_idx),
    balls_total = length(balls),
    balls_with_redwall = length(occupied),
    max_redwall_share = max(tab$redwall_share, na.rm = TRUE),
    majority_redwall_ball = any(tab$redwall_share > 0.5, na.rm = TRUE),
    max_redwall_count = max(tab$redwall),
    redwall_multimembership_n = sum(mem_count[rw_idx] >= 2L),
    redwall_multimembership_share = mean(mem_count[rw_idx] >= 2L),
    redwall_touch_mixed_ball_share = mean(mixed_touch[rw_idx]),
    redwall_membership_entropy = entropy,
    redwall_components = rw_components,
    redwall_memberships_in_largest_component_share = rw_in_largest_component,
    stringsAsFactors = FALSE
  )
}

connected_components <- function(nballs, edges) {
  if (!nballs) return(integer())
  adj <- vector("list", nballs)
  if (!is.null(edges) && nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      a <- edges[i, 1L]; b <- edges[i, 2L]
      adj[[a]] <- unique(c(adj[[a]], b))
      adj[[b]] <- unique(c(adj[[b]], a))
    }
  }
  comp <- integer(nballs)
  cid <- 0L
  for (s in seq_len(nballs)) {
    if (comp[s] != 0L) next
    cid <- cid + 1L
    q <- s
    comp[s] <- cid
    while (length(q)) {
      v <- q[[1L]]
      q <- q[-1L]
      nb <- adj[[v]]
      if (length(nb)) {
        fresh <- nb[comp[nb] == 0L]
        if (length(fresh)) {
          comp[fresh] <- cid
          q <- c(q, fresh)
        }
      }
    }
  }
  comp
}

build_edges <- function(balls) {
  nb <- length(balls)
  if (nb < 2L) return(data.frame(ball1 = integer(), ball2 = integer()))
  out <- vector("list", nb * (nb - 1L) / 2L)
  k <- 0L
  for (i in seq_len(nb - 1L)) {
    ai <- balls[[i]]
    for (j in (i + 1L):nb) {
      if (length(intersect(ai, balls[[j]]))) {
        k <- k + 1L
        out[[k]] <- c(i, j)
      }
    }
  }
  if (!k) return(data.frame(ball1 = integer(), ball2 = integer()))
  z <- do.call(rbind, out[seq_len(k)])
  data.frame(ball1 = z[,1L], ball2 = z[,2L])
}

build_cover <- function(distance_matrix, radius, order_idx) {
  n <- nrow(distance_matrix)
  if (length(order_idx) != n || !setequal(order_idx, seq_len(n))) fail("Invalid landmark order.")
  covered <- rep(FALSE, n)
  balls <- list()
  landmarks <- integer()
  eps <- 1e-12
  for (idx in order_idx) {
    if (covered[idx]) next
    members <- which(distance_matrix[idx, ] <= radius + eps)
    balls[[length(balls) + 1L]] <- members
    landmarks <- c(landmarks, idx)
    covered[members] <- TRUE
    if (all(covered)) break
  }
  if (!all(covered)) fail("Reference Ball Mapper failed to cover all points.")
  edges <- build_edges(balls)
  cc <- connected_components(length(balls), edges)
  memberships <- integer(n)
  for (b in balls) memberships[b] <- memberships[b] + 1L
  list(
    balls = balls,
    landmarks = landmarks,
    edges = edges,
    components = cc,
    summary = data.frame(
      balls = length(balls),
      edges = nrow(edges),
      components = max(cc),
      isolates = sum(tabulate(cc, nbins = max(cc)) == 1L),
      multicoverage = mean(memberships >= 2L),
      stringsAsFactors = FALSE
    )
  )
}

extract_ids_from_order_file <- function(path, prepared_ids) {
  if (!file.exists(path)) return(seq_along(prepared_ids))
  d <- safe_read_csv(path)
  idc <- detect_id_col(d)
  if (!is.na(idc)) {
    ids <- as.character(d[[idc]])
    m <- match(ids, prepared_ids)
    m <- m[!is.na(m)]
    if (length(m) == length(prepared_ids) && length(unique(m)) == length(m)) return(m)
  }
  # fallback: integer order column
  for (nm in names(d)) {
    v <- suppressWarnings(as.integer(d[[nm]]))
    if (length(v) == length(prepared_ids) && setequal(v, seq_along(prepared_ids))) return(v)
  }
  seq_along(prepared_ids)
}

find_approved_radius <- function(app_dir, expected = 5.50) {
  candidates <- c(
    file.path(app_dir, "config", "p1_4_c_radius_decision.csv"),
    file.path(app_dir, "frozen_topology", "p1_4_c_v1_0", "P1_4_C_radius_decision.csv"),
    file.path(app_dir, "frozen_topology", "p1_4_c_v1_0", "P1_4_C_user_radius_contract.csv")
  )
  vals <- numeric()
  for (p in candidates[file.exists(candidates)]) {
    d <- safe_read_csv(p)
    for (nm in names(d)) {
      if (!grepl("radius", nm, ignore.case = TRUE) && !"field" %in% names(d)) next
      x <- suppressWarnings(as.numeric(d[[nm]]))
      vals <- c(vals, x[is.finite(x) & x > 0 & x < 50])
    }
    if (all(c("field","value") %in% names(d))) {
      hit <- grepl("radius", d$field, ignore.case = TRUE)
      x <- suppressWarnings(as.numeric(d$value[hit]))
      vals <- c(vals, x[is.finite(x) & x > 0 & x < 50])
    }
  }
  vals <- unique(round(vals, 10))
  if (any(abs(vals - expected) < 1e-8)) return(expected)
  if (length(vals) == 1L) return(vals[[1L]])
  expected
}

latest_production_audit <- function(app_dir) {
  d <- list.dirs(file.path(app_dir, "runs"), recursive = FALSE, full.names = TRUE)
  d <- d[grepl("p1_4_b_production_", basename(d))]
  if (!length(d)) return(NA_character_)
  d[order(basename(d), decreasing = TRUE)][[1L]]
}

discover_radius_grid <- function(app_dir, approved) {
  paths <- c(
    file.path(app_dir, "frozen_topology", "p1_4_c_v1_0", "P1_4_C_selected_radius_B_diagnostics.csv")
  )
  prod <- latest_production_audit(app_dir)
  if (!is.na(prod)) {
    aud <- list.dirs(prod, recursive = FALSE, full.names = TRUE)
    if (length(aud)) {
      paths <- c(paths,
        file.path(aud, "radius_review_rows.csv"),
        file.path(aud, "radius_summary.csv"))
    }
  }
  vals <- numeric()
  for (p in unique(paths[file.exists(paths)])) {
    d <- safe_read_csv(p)
    rc <- names(d)[grepl("^radius$|radius_value|radius", names(d), ignore.case = TRUE)]
    for (nm in rc) {
      x <- suppressWarnings(as.numeric(d[[nm]]))
      vals <- c(vals, x[is.finite(x)])
    }
  }
  vals <- sort(unique(round(vals, 2L)))
  vals <- vals[vals >= approved - 0.15 & vals <= approved + 0.15]
  if (length(vals) >= 3L) {
    # at most five closest values, always include approved
    vals <- vals[order(abs(vals - approved))]
    vals <- sort(unique(c(approved, head(vals, 5L))))
    return(vals)
  }
  round(seq(approved - 0.10, approved + 0.10, by = 0.05), 2L)
}

discover_redwall <- function(repo, frozen_data, ids) {
  source_file <- Sys.getenv("OTWAY_RED_WALL_SOURCE",
                            unset=file.path(repo,"data","user","redwall_source.csv"))
  if (!file.exists(source_file)) fail("Missing user-supplied Red Wall source file: ",source_file)
  srcdat <- utils::read.csv(source_file,stringsAsFactors=FALSE,check.names=FALSE)
  if (!"source_constituency" %in% names(srcdat)) fail("Red Wall source must contain source_constituency.")
  source_names <- trimws(as.character(srcdat$source_constituency))
  if (nrow(srcdat)!=42L || anyDuplicated(source_names)) fail("Red Wall source must contain exactly 42 unique constituencies.")
  if (!"ConstituencyName" %in% names(frozen_data)) fail("Frozen input lacks ConstituencyName.")
  normalise_const_name <- function(x) {
    y <- iconv(as.character(x),from="",to="ASCII//TRANSLIT",sub="")
    y <- tolower(y); y <- gsub("&","and",y,fixed=TRUE); gsub("[^a-z0-9]+","",y)
  }
  aliases <- c(penistoneandstockbridge="penistoneandstocksbridge",
               newcastleupontynenorth="newcastleupontynenorth")
  src_norm <- normalise_const_name(source_names)
  src_norm[src_norm %in% names(aliases)] <- unname(aliases[src_norm[src_norm %in% names(aliases)]])
  dat_names <- as.character(frozen_data$ConstituencyName)
  dat_norm <- normalise_const_name(dat_names)
  m <- match(src_norm,dat_norm); matched <- !is.na(m)
  match_audit <- data.frame(
    source_constituency=source_names,source_normalised=src_norm,matched=matched,
    matched_row=ifelse(matched,m,NA_integer_),
    matched_ons_id=ifelse(matched,as.character(frozen_data$ONSConstID[m]),NA_character_),
    matched_constituency_name=ifelse(matched,dat_names[m],NA_character_),stringsAsFactors=FALSE)
  missing_names <- source_names[!matched]
  if (length(missing_names)!=1L || normalise_const_name(missing_names)!=normalise_const_name("Chorley"))
    fail("Published Red Wall list did not reconcile to frozen sample; expected Chorley as sole missing seat.")
  bb <- integer(nrow(frozen_data)); bb[m[matched]] <- 1L
  if (sum(bb)!=41L) fail("Published Red Wall lookup produced ",sum(bb)," in-sample seats; expected 41.")
  list(redwall=bb,
       source="user_supplied_Kanagasooriam_Simon_2021_42_seat_definition_minus_excluded_Chorley",
       raw_count=42L,source_42=source_names,match_audit=match_audit)
}

read_frozen_memberships <- function(path, ids) {
  if (!file.exists(path)) return(NULL)
  d <- safe_read_csv(path)
  idc <- detect_id_col(d)
  bc <- names(d)[grepl("^ball$|ball_id|ballid|landmark", names(d), ignore.case = TRUE)]
  if (is.na(idc) || !length(bc)) return(NULL)
  bcol <- bc[[1L]]
  d <- d[as.character(d[[idc]]) %in% ids, , drop = FALSE]
  split(match(as.character(d[[idc]]), ids), d[[bcol]])
}

same_ball_sets <- function(a, b) {
  if (is.null(a) || is.null(b) || length(a) != length(b)) return(NA)
  canon <- function(z) sort(vapply(z, function(x) paste(sort(unique(x)), collapse=","), character(1)))
  identical(canon(a), canon(b))
}

summarise_numeric <- function(d, group_cols = character()) {
  num <- names(d)[vapply(d, is.numeric, logical(1))]
  num <- setdiff(num, c(group_cols, "replicate", "seed"))
  if (!length(group_cols)) {
    groups <- list(all = seq_len(nrow(d)))
  } else {
    key <- interaction(d[group_cols], drop = TRUE, lex.order = TRUE)
    groups <- split(seq_len(nrow(d)), key)
  }
  out <- lapply(groups, function(ix) {
    z <- d[ix, , drop = FALSE]
    base <- if (length(group_cols)) z[1L, group_cols, drop = FALSE] else data.frame(scope = "all")
    for (nm in num) {
      x <- z[[nm]]
      base[[paste0(nm, "_median")]] <- median(x, na.rm = TRUE)
      base[[paste0(nm, "_q10")]] <- as.numeric(quantile(x, 0.10, na.rm = TRUE, names = FALSE))
      base[[paste0(nm, "_q90")]] <- as.numeric(quantile(x, 0.90, na.rm = TRUE, names = FALSE))
      base[[paste0(nm, "_min")]] <- min(x, na.rm = TRUE)
      base[[paste0(nm, "_max")]] <- max(x, na.rm = TRUE)
    }
    base
  })
  do.call(rbind, out)
}

run_replicates <- function(D, radius, redwall, reps, base_seed, workers) {
  fun <- function(i) {
    set.seed(base_seed + i - 1L)
    ord <- sample.int(nrow(D))
    tp <- build_cover(D, radius, ord)
    rd <- redwall_diag_from_balls(tp$balls, redwall, tp$edges)
    cbind(
      data.frame(
        replicate = i,
        seed = base_seed + i - 1L,
        radius = radius,
        balls = tp$summary$balls,
        edges = tp$summary$edges,
        components = tp$summary$components,
        isolates = tp$summary$isolates,
        multicoverage = tp$summary$multicoverage,
        stringsAsFactors = FALSE
      ),
      rd
    )
  }
  if (.Platform$OS.type == "unix" && workers > 1L) {
    z <- parallel::mclapply(seq_len(reps), fun, mc.cores = workers, mc.preschedule = TRUE)
  } else {
    z <- lapply(seq_len(reps), fun)
  }
  do.call(rbind, z)
}

topology_loss <- function(summary_row, target) {
  vals <- c(
    balls = summary_row$balls_median,
    edges = summary_row$edges_median,
    components = summary_row$components_median,
    isolates = summary_row$isolates_median,
    multicoverage = summary_row$multicoverage_median
  )
  sc <- c(
    balls = max(1, target["balls"]),
    edges = max(1, target["edges"]),
    components = max(1, target["components"]),
    isolates = 1,
    multicoverage = 1
  )
  sum(abs(vals - target) / sc)
}

hash_file <- function(path) {
  if (nzchar(Sys.which("sha256sum"))) {
    out <- system2("sha256sum", shQuote(path), stdout = TRUE)
    return(strsplit(out[[1L]], "\\s+")[[1L]][[1L]])
  }
  NA_character_
}

# ---------------------------------------------------------------------------
# Environment discovery
# ---------------------------------------------------------------------------

sp <- script_path()
script_dir <- if (dir.exists(sp)) sp else dirname(sp)
repo <- find_repo_root(script_dir)
app_dir <- file.path(repo, "applications", "Otway_Rudkin_2024_Corrected31D")
out_dir <- file.path(script_dir, "outputs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

workers <- num_env("OTWAY_ADDENDUM_WORKERS", min(20L, max(1L, parallel::detectCores() - 1L)))
order_reps <- num_env("OTWAY_ORDER_REPS", 1000L)
radius_reps <- num_env("OTWAY_RADIUS_REPS", 1000L)
label_reps <- num_env("OTWAY_RANDOM_LABEL_REPS", 10000L)
block_grid_reps <- num_env("OTWAY_BLOCK_GRID_REPS", 100L)
block_final_reps <- num_env("OTWAY_BLOCK_FINAL_REPS", 1000L)

cat("Repository root: ", repo, "\n", sep="")
cat("Corrected31D app: ", app_dir, "\n", sep="")
cat("Workers: ", workers, "\n", sep="")

source(file.path(repo, "framework", "R", "TDABMRadiusDiagnostics.R"))
source(file.path(app_dir, "R", "OtwayRudkin2024Adapter.R"))

config <- or24_read_contract(file.path(app_dir, "config", "application_contract.csv"))
app_id <- or24_get(config, "application_id")
topology_vars <- or24_split(or24_get(config, "topology_variables"))

frozen <- file.path(
  app_dir, "frozen_input", "p1_4_a_v1_0",
  paste0(app_id, "_input_frozen_P1_4_A_v1_0.csv")
)
if (!file.exists(frozen)) fail("Frozen P1.4-A input missing: ", frozen)

dat <- safe_read_csv(frozen)
tdabm_rd_validate_topology_frame(
  dat, "ONSConstID", topology_vars,
  scaling = "z_score", metric = "euclidean",
  expected_rows = 571L, stop_on_failure = TRUE
)
prepared <- tdabm_rd_prepare_topology(
  dat, "ONSConstID", topology_vars,
  scaling = "z_score", metric = "euclidean"
)

if (nrow(prepared$axes) != 571L || ncol(prepared$axes) != 31L) {
  fail("Prepared Corrected31D dimensions do not equal 571 x 31.")
}

approved_radius <- find_approved_radius(app_dir, expected = 5.50)
if (abs(approved_radius - 5.50) > 1e-8) {
  fail("Approved radius recovered as ", approved_radius, ", expected frozen Corrected31D radius 5.50.")
}

canonical_dir <- file.path(app_dir, "frozen_topology", "p1_4_c_v1_0")
order_file <- file.path(canonical_dir, "canonical_point_order.csv")
membership_file <- file.path(canonical_dir, "canonical_memberships.csv")
canonical_order <- extract_ids_from_order_file(order_file, prepared$ids)

rw <- discover_redwall(repo, dat, prepared$ids)
if (!is.null(rw$match_audit)) {
  write_csv(rw$match_audit, file.path(out_dir, "00_redwall_source_match_audit.csv"))
}
if (!is.null(rw$source_42)) {
  write_csv(
    data.frame(source_order = seq_along(rw$source_42),
               source_constituency = rw$source_42,
               stringsAsFactors = FALSE),
    file.path(out_dir, "00_redwall_source_42_constituencies.csv")
  )
}
redwall <- as.integer(rw$redwall)
if (sum(redwall) != 41L) fail("Recovered Red Wall count is ", sum(redwall), "; expected 41 in analytical sample.")

env <- data.frame(
  field = c(
    "repo_root","app_dir","application_id","frozen_input","frozen_input_sha256",
    "n","p","approved_radius","redwall_count","redwall_source",
    "order_reps","radius_reps","random_label_reps","block_grid_reps",
    "block_final_reps","workers"
  ),
  value = c(
    repo, app_dir, app_id, frozen, hash_file(frozen),
    nrow(prepared$axes), ncol(prepared$axes), approved_radius,
    sum(redwall), rw$source, order_reps, radius_reps, label_reps,
    block_grid_reps, block_final_reps, workers
  ),
  stringsAsFactors = FALSE
)
write_csv(env, file.path(out_dir, "00_environment_and_provenance.csv"))

# ---------------------------------------------------------------------------
# Canonical validation
# ---------------------------------------------------------------------------

D <- as.matrix(stats::dist(prepared$axes))
canonical <- build_cover(D, approved_radius, canonical_order)

expected <- c(balls = 28L, edges = 96L, components = 2L, isolates = 1L)
got <- c(
  balls = canonical$summary$balls,
  edges = canonical$summary$edges,
  components = canonical$summary$components,
  isolates = canonical$summary$isolates
)
headline_pass <- identical(as.integer(got), as.integer(expected))
if (!headline_pass) {
  write_csv(
    data.frame(metric=names(expected), expected=expected, observed=got),
    file.path(out_dir, "CANONICAL_VALIDATION_FAILED.csv")
  )
  fail("Reference Ball Mapper does not reproduce frozen canonical headline topology. Failing closed.")
}

frozen_balls <- read_frozen_memberships(membership_file, prepared$ids)
membership_exact <- same_ball_sets(canonical$balls, frozen_balls)

validation <- data.frame(
  metric = c("balls","edges","components","isolates","membership_sets_exact_if_parseable"),
  expected = c(28,96,2,1,TRUE),
  observed = c(got, membership_exact),
  pass = c(got == expected, is.na(membership_exact) | isTRUE(membership_exact)),
  stringsAsFactors = FALSE
)
write_csv(validation, file.path(out_dir, "01_canonical_validation.csv"))
if (!all(validation$pass)) fail("Canonical validation did not pass.")

canonical_rw <- redwall_diag_from_balls(canonical$balls, redwall, canonical$edges)
write_csv(canonical_rw, file.path(out_dir, "02_canonical_redwall_diagnostics.csv"))

# ---------------------------------------------------------------------------
# Input-order robustness at approved radius
# ---------------------------------------------------------------------------

cat("Running Red Wall input-order robustness at radius ", approved_radius, " ...\n", sep="")
order_raw <- run_replicates(
  D, approved_radius, redwall,
  order_reps, base_seed = 202610020L, workers = workers
)
write_csv(order_raw, file.path(out_dir, "03_redwall_order_robustness_replicates.csv"))
order_sum <- summarise_numeric(order_raw)
order_sum$majority_redwall_ball_frequency <- mean(order_raw$majority_redwall_ball)
write_csv(order_sum, file.path(out_dir, "04_redwall_order_robustness_summary.csv"))

# ---------------------------------------------------------------------------
# Targeted radius robustness around approved radius using prior evidence
# ---------------------------------------------------------------------------

radii <- discover_radius_grid(app_dir, approved_radius)
cat("Targeted radii: ", paste(radii, collapse=", "), "\n", sep="")

radius_raw_list <- vector("list", length(radii))
for (k in seq_along(radii)) {
  cat("Radius robustness ", k, "/", length(radii), ": ", radii[[k]], "\n", sep="")
  radius_raw_list[[k]] <- run_replicates(
    D, radii[[k]], redwall,
    radius_reps, base_seed = 202620000L + k * 100000L,
    workers = workers
  )
}
radius_raw <- do.call(rbind, radius_raw_list)
write_csv(radius_raw, file.path(out_dir, "05_redwall_radius_robustness_replicates.csv"))
radius_sum <- summarise_numeric(radius_raw, group_cols = "radius")
maj_by_r <- aggregate(majority_redwall_ball ~ radius, radius_raw, mean)
names(maj_by_r)[2L] <- "majority_redwall_ball_frequency"
radius_sum <- merge(radius_sum, maj_by_r, by = "radius", all.x = TRUE, sort = TRUE)
write_csv(radius_sum, file.path(out_dir, "06_redwall_radius_robustness_summary.csv"))

# ---------------------------------------------------------------------------
# Random-label concentration benchmark on frozen canonical topology
# ---------------------------------------------------------------------------

cat("Running random-label benchmark (", label_reps, " draws) ...\n", sep="")
set.seed(202630001L)
n <- length(redwall)
k_rw <- sum(redwall)
bench <- vector("list", label_reps)
for (i in seq_len(label_reps)) {
  lab <- integer(n)
  lab[sample.int(n, k_rw, replace = FALSE)] <- 1L
  d <- redwall_diag_from_balls(canonical$balls, lab, canonical$edges)
  bench[[i]] <- cbind(data.frame(draw=i), d)
}
bench <- do.call(rbind, bench)
write_csv(bench, file.path(out_dir, "07_random_label_benchmark_replicates.csv"))

metrics <- c(
  "balls_with_redwall","max_redwall_share","max_redwall_count",
  "redwall_multimembership_share","redwall_touch_mixed_ball_share",
  "redwall_membership_entropy"
)
bench_summary <- do.call(rbind, lapply(metrics, function(m) {
  obs <- canonical_rw[[m]]
  x <- bench[[m]]
  data.frame(
    metric = m,
    observed = obs,
    random_median = median(x),
    random_q05 = as.numeric(quantile(x,0.05,names=FALSE)),
    random_q95 = as.numeric(quantile(x,0.95,names=FALSE)),
    lower_tail_p = (1 + sum(x <= obs)) / (1 + length(x)),
    upper_tail_p = (1 + sum(x >= obs)) / (1 + length(x)),
    stringsAsFactors = FALSE
  )
}))
write_csv(bench_summary, file.path(out_dir, "08_random_label_benchmark_summary.csv"))

# ---------------------------------------------------------------------------
# Conceptual-block weighting sensitivity
# ---------------------------------------------------------------------------

tv <- topology_vars
groups <- list(
  density = grep("^population_density$", tv, ignore.case = TRUE),
  age = grep("^age_", tv, ignore.case = TRUE),
  ethnicity = grep("^ethnic_group_", tv, ignore.case = TRUE),
  education = grep("^qualifications_", tv, ignore.case = TRUE),
  industry = grep("^industry_", tv, ignore.case = TRUE),
  nssec = grep("^NSSEC_", tv, ignore.case = TRUE)
)
expected_counts <- c(density=1L, age=6L, ethnicity=1L, education=6L, industry=8L, nssec=9L)
actual_counts <- vapply(groups, length, integer(1))
if (!identical(actual_counts[names(expected_counts)], expected_counts)) {
  write_csv(
    data.frame(block=names(expected_counts), expected=expected_counts, observed=actual_counts[names(expected_counts)]),
    file.path(out_dir, "BLOCK_DEFINITION_FAILED.csv")
  )
  fail("Topology variables could not be assigned to expected conceptual blocks.")
}

Xw <- prepared$axes
for (nm in names(groups)) {
  idx <- groups[[nm]]
  Xw[, idx] <- Xw[, idx, drop = FALSE] / sqrt(length(idx))
}
Dw <- as.matrix(stats::dist(Xw))

upper <- upper.tri(D)
scale_ratio <- median(Dw[upper]) / median(D[upper])
scaled_center <- approved_radius * scale_ratio
grid <- seq(scaled_center * 0.90, scaled_center * 1.10, length.out = 31L)

target <- c(
  balls = median(order_raw$balls),
  edges = median(order_raw$edges),
  components = median(order_raw$components),
  isolates = median(order_raw$isolates),
  multicoverage = median(order_raw$multicoverage)
)

cat("Running block-weighting topology-only resolution match ...\n")
grid_rows <- vector("list", length(grid))
for (g in seq_along(grid)) {
  rr <- run_replicates(
    Dw, grid[[g]], redwall,
    block_grid_reps, base_seed = 202640000L + g * 10000L,
    workers = workers
  )
  s <- summarise_numeric(rr)
  s$radius <- grid[[g]]
  s$loss <- topology_loss(s, target)
  grid_rows[[g]] <- s
}
grid_tab <- do.call(rbind, grid_rows)
grid_tab <- grid_tab[order(grid_tab$radius), ]
write_csv(grid_tab, file.path(out_dir, "09_block_weighting_topology_selection.csv"))

best_loss <- min(grid_tab$loss)
cand <- grid_tab[abs(grid_tab$loss - best_loss) < 1e-12, , drop = FALSE]
weighted_radius <- cand$radius[which.min(abs(cand$radius - scaled_center))][[1L]]

cat("Block-weighted sensitivity radius: ", format(weighted_radius,digits=12), "\n", sep="")
block_raw <- run_replicates(
  Dw, weighted_radius, redwall,
  block_final_reps, base_seed = 202650001L, workers = workers
)
write_csv(block_raw, file.path(out_dir, "10_block_weighting_redwall_replicates.csv"))
block_sum <- summarise_numeric(block_raw)
block_sum$weighted_radius <- weighted_radius
block_sum$distance_scale_ratio <- scale_ratio
block_sum$majority_redwall_ball_frequency <- mean(block_raw$majority_redwall_ball)
write_csv(block_sum, file.path(out_dir, "11_block_weighting_redwall_summary.csv"))

block_def <- do.call(rbind, lapply(names(groups), function(nm) {
  data.frame(
    block=nm,
    n_variables=length(groups[[nm]]),
    weight_per_coordinate=1/sqrt(length(groups[[nm]])),
    variables=paste(tv[groups[[nm]]], collapse=";"),
    stringsAsFactors=FALSE
  )
}))
write_csv(block_def, file.path(out_dir, "12_block_weighting_definition.csv"))

# ---------------------------------------------------------------------------
# Manuscript-facing summary and PASS gates
# ---------------------------------------------------------------------------

order_majority_freq <- mean(order_raw$majority_redwall_ball)
radius_majority_max <- max(maj_by_r$majority_redwall_ball_frequency)
block_majority_freq <- mean(block_raw$majority_redwall_ball)

pass_order <- order_majority_freq <= 0.05
pass_radius <- radius_majority_max <= 0.05
pass_block <- block_majority_freq <= 0.05
pass_all <- pass_order && pass_radius && pass_block

summary_md <- c(
  "# Otway–Rudkin BJPolS Targeted Empirical Addendum v1.2",
  "",
  "## Provenance",
  paste0("- Repository: `", repo, "`"),
  paste0("- Corrected31D application: `", app_dir, "`"),
  paste0("- Approved primary radius: ", approved_radius),
  paste0("- Red Wall source: `", rw$source, "`"),
  paste0("- In-sample Red Wall constituencies: ", sum(redwall)),
  "",
  "## Canonical validation",
  paste0("- Balls: ", got["balls"], " (expected 28)"),
  paste0("- Edges: ", got["edges"], " (expected 96)"),
  paste0("- Components: ", got["components"], " (expected 2)"),
  paste0("- Isolates: ", got["isolates"], " (expected 1)"),
  paste0("- Frozen membership sets exactly reproduced when parseable: ", membership_exact),
  "",
  "## Canonical Red Wall diagnostics",
  paste0("- Balls containing Red Wall seats: ", canonical_rw$balls_with_redwall),
  paste0("- Maximum within-ball Red Wall share: ", sprintf("%.4f", canonical_rw$max_redwall_share)),
  paste0("- Any majority-Red-Wall ball: ", canonical_rw$majority_redwall_ball),
  paste0("- Red Wall multi-membership share: ", sprintf("%.4f", canonical_rw$redwall_multimembership_share)),
  paste0("- Red Wall seats touching at least one mixed ball: ", sprintf("%.4f", canonical_rw$redwall_touch_mixed_ball_share)),
  "",
  "## Robustness headline",
  paste0("- Majority-Red-Wall-ball frequency across approved-radius input orders: ", sprintf("%.4f", order_majority_freq)),
  paste0("- Maximum majority-Red-Wall-ball frequency across targeted radii: ", sprintf("%.4f", radius_majority_max)),
  paste0("- Majority-Red-Wall-ball frequency under block-weighted sensitivity: ", sprintf("%.4f", block_majority_freq)),
  "",
  "## Random-label benchmark",
  "See `08_random_label_benchmark_summary.csv` for concentration comparisons against 10,000 random 41-seat labels.",
  "",
  "## Gate",
  paste0("- ORDER_ROBUSTNESS_PASS: ", pass_order),
  paste0("- RADIUS_ROBUSTNESS_PASS: ", pass_radius),
  paste0("- BLOCK_WEIGHTING_PASS: ", pass_block),
  paste0("- OVERALL_PASS: ", pass_all)
)
writeLines(summary_md, file.path(out_dir, "MANUSCRIPT_ADDENDUM_SUMMARY.md"), useBytes = TRUE)

writeLines(
  c(
    paste0("OTWAY_BJPOLS_TARGETED_ADDENDUM_V1_2=", if (pass_all) "PASS" else "REVIEW_REQUIRED"),
    paste0("canonical_validation=", if (all(validation$pass)) "PASS" else "FAIL"),
    paste0("redwall_count=", sum(redwall)),
    paste0("approved_radius=", approved_radius),
    paste0("order_majority_frequency=", format(order_majority_freq,digits=10)),
    paste0("radius_majority_max_frequency=", format(radius_majority_max,digits=10)),
    paste0("block_majority_frequency=", format(block_majority_freq,digits=10))
  ),
  file.path(out_dir, "ACCEPTANCE.txt"),
  useBytes = TRUE
)

capture.output(sessionInfo(), file = file.path(out_dir, "sessionInfo.txt"))

# Hash outputs
outs <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
outs <- outs[file.info(outs)$isdir %in% FALSE]
manifest <- data.frame(
  relative_path = sub(paste0("^", gsub("([][{}()+*^$|\\\\?.])","\\\\\\1",out_dir), "/?"), "", outs),
  bytes = file.info(outs)$size,
  sha256 = vapply(outs, hash_file, character(1)),
  stringsAsFactors = FALSE
)
write_csv(manifest, file.path(out_dir, "artifact_manifest.csv"))

# Handback ZIP
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
handdir <- file.path(out_dir, "handback")
dir.create(handdir, recursive = TRUE, showWarnings = FALSE)
zipname <- file.path(handdir, paste0("Otway_BJPolS_Targeted_Addendum_Handback_", stamp, ".zip"))

to_zip <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
to_zip <- to_zip[file.info(to_zip)$isdir %in% FALSE]
to_zip <- to_zip[!grepl("/handback/", to_zip, fixed = TRUE)]
old <- getwd()
setwd(out_dir)
on.exit(setwd(old), add = TRUE)
rel <- sub(paste0("^", normalizePath(out_dir,winslash="/"), "/"), "", normalizePath(to_zip,winslash="/",mustWork=TRUE))
utils::zip(zipfile = zipname, files = rel, flags = "-q")
setwd(old)

cat("\n")
cat(if (pass_all) "PASS_TARGETED_EMPIRICAL_ADDENDUM\n" else "REVIEW_REQUIRED_TARGETED_EMPIRICAL_ADDENDUM\n")
cat("HAND BACK: ", zipname, "\n", sep="")
