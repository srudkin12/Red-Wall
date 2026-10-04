options(stringsAsFactors = FALSE)
options(width = 180)
cat("OTWAY_BJPOLS_2021_ROUNDED_RADIUS_ROBUSTNESS_V1_1\n")

# -----------------------------------------------------------------------------
# Core helpers
# -----------------------------------------------------------------------------
fail <- function(...) stop(paste0(...), call. = FALSE)

script_path <- function() {
  x <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", x[grep("^--file=", x)])
  if (!length(f)) return(normalizePath(".", winslash = "/", mustWork = TRUE))
  normalizePath(f[[1L]], winslash = "/", mustWork = TRUE)
}

find_repo_root <- function(start_dir) {
  p <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  for (i in 0:10) {
    fw <- file.path(p, "framework", "R", "TDABMRadiusDiagnostics.R")
    app <- file.path(p, "applications", "Otway_Rudkin_2024_Corrected31D")
    if (file.exists(fw) && dir.exists(app)) return(p)
    q <- dirname(p)
    if (identical(q, p)) break
    p <- q
  }
  fail("Could not discover TDABM_Portability root from ", start_dir)
}

num_env <- function(name, default, minimum = 1L) {
  z <- Sys.getenv(name, "")
  if (!nzchar(z)) z <- as.character(default)
  v <- suppressWarnings(as.integer(z))
  if (is.na(v) || v < minimum) fail("Invalid environment variable ", name, "=", z)
  v
}

write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE, na = "")
}

safe_read_csv <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
}

norm_name <- function(x) gsub("[^a-z0-9]+", "", tolower(x))

zscore_matrix <- function(X) {
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  mu <- colMeans(X, na.rm = TRUE)
  sdv <- apply(X, 2L, stats::sd, na.rm = TRUE)
  if (any(!is.finite(sdv) | sdv <= 0)) {
    bad <- colnames(X)[!is.finite(sdv) | sdv <= 0]
    fail("Cannot z-standardise zero/nonfinite-variance columns: ", paste(bad, collapse = "; "))
  }
  sweep(sweep(X, 2L, mu, "-"), 2L, sdv, "/")
}

hash_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  if (nzchar(Sys.which("sha256sum"))) {
    out <- system2("sha256sum", shQuote(path), stdout = TRUE)
    return(strsplit(out[[1L]], "\\s+")[[1L]][[1L]])
  }
  NA_character_
}

extract_ids_from_order_file <- function(path, prepared_ids) {
  if (!file.exists(path)) return(seq_along(prepared_ids))
  d <- safe_read_csv(path)
  for (nm in c("unit_id", "ONSConstID", "ons_const_id", "constituency_id", "point_id", "id")) {
    if (nm %in% names(d)) {
      m <- match(as.character(d[[nm]]), prepared_ids)
      m <- m[!is.na(m)]
      if (length(m) == length(prepared_ids) && !anyDuplicated(m)) return(m)
    }
  }
  for (nm in names(d)) {
    v <- suppressWarnings(as.integer(d[[nm]]))
    if (length(v) == length(prepared_ids) && setequal(v, seq_along(prepared_ids))) return(v)
  }
  seq_along(prepared_ids)
}

# -----------------------------------------------------------------------------
# Reference Ball Mapper implementation used only for sensitivity reconstruction
# -----------------------------------------------------------------------------
build_edges <- function(balls) {
  nb <- length(balls)
  if (nb < 2L) return(data.frame(from = integer(), to = integer(), strength = integer()))
  out <- vector("list", nb * (nb - 1L) / 2L)
  k <- 0L
  for (i in seq_len(nb - 1L)) {
    ai <- balls[[i]]
    for (j in (i + 1L):nb) {
      s <- length(intersect(ai, balls[[j]]))
      if (s > 0L) {
        k <- k + 1L
        out[[k]] <- c(i, j, s)
      }
    }
  }
  if (!k) return(data.frame(from = integer(), to = integer(), strength = integer()))
  z <- do.call(rbind, out[seq_len(k)])
  data.frame(from = as.integer(z[,1L]), to = as.integer(z[,2L]), strength = as.integer(z[,3L]))
}

connected_components <- function(nballs, edges) {
  if (!nballs) return(integer())
  adj <- vector("list", nballs)
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      a <- edges$from[i]; b <- edges$to[i]
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

build_cover <- function(D, radius, order_idx) {
  n <- nrow(D)
  if (length(order_idx) != n || !setequal(order_idx, seq_len(n))) fail("Invalid landmark order.")
  covered <- rep(FALSE, n)
  balls <- list()
  landmarks <- integer()
  eps <- 1e-12
  for (idx in order_idx) {
    if (covered[idx]) next
    members <- which(D[idx, ] <= radius + eps)
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
      components = if (length(cc)) max(cc) else 0L,
      isolates = if (length(cc)) sum(tabulate(cc, nbins = max(cc)) == 1L) else 0L,
      multicoverage = mean(memberships >= 2L),
      stringsAsFactors = FALSE
    )
  )
}

# -----------------------------------------------------------------------------
# Red Wall source definition
# -----------------------------------------------------------------------------
redwall_source <- function(dat) {
  source_file <- Sys.getenv("OTWAY_RED_WALL_SOURCE",
                            unset=file.path(repo,"data","user","redwall_source.csv"))
  if (!file.exists(source_file)) fail("Missing user-supplied Red Wall source file: ",source_file)
  srcdat <- utils::read.csv(source_file,stringsAsFactors=FALSE,check.names=FALSE)
  req <- c("source_constituency","source_region")
  if (!all(req %in% names(srcdat))) fail("Red Wall source must contain source_constituency and source_region.")
  source_names <- trimws(as.character(srcdat$source_constituency))
  source_region <- trimws(as.character(srcdat$source_region))
  if (nrow(srcdat)!=42L || anyDuplicated(source_names)) fail("Red Wall source must contain exactly 42 unique constituencies.")
  if (!"ConstituencyName" %in% names(dat)) fail("Frozen input lacks ConstituencyName.")
  nn <- function(x) {
    y <- iconv(as.character(x),from="",to="ASCII//TRANSLIT",sub="")
    y <- tolower(y); y <- gsub("&","and",y,fixed=TRUE); gsub("[^a-z0-9]+","",y)
  }
  src <- nn(source_names)
  src[src=="penistoneandstockbridge"] <- "penistoneandstocksbridge"
  dn <- nn(dat$ConstituencyName)
  m <- match(src,dn)
  audit <- data.frame(
    source_order=seq_along(source_names),source_constituency=source_names,
    source_region=source_region,matched=!is.na(m),matched_row=m,
    matched_ons_id=ifelse(!is.na(m) & "ONSConstID" %in% names(dat),as.character(dat$ONSConstID[m]),NA_character_),
    matched_constituency_name=ifelse(!is.na(m),as.character(dat$ConstituencyName[m]),NA_character_),
    stringsAsFactors=FALSE)
  missing <- source_names[is.na(m)]
  if (length(missing)!=1L || nn(missing)!=nn("Chorley"))
    return(list(ok=FALSE,audit=audit,redwall=NULL,source_region=source_region,source_names=source_names,
                message=paste("Expected Chorley as sole missing source seat; observed:",paste(missing,collapse="; "))))
  lab <- integer(nrow(dat)); lab[m[!is.na(m)]] <- 1L
  if (sum(lab)!=41L) fail("Red Wall reconstruction produced ",sum(lab)," seats, expected 41.")
  list(ok=TRUE,audit=audit,redwall=lab,source_region=source_region,source_names=source_names,message="PASS")
}

# -----------------------------------------------------------------------------
# Diagnostics: concentration, prevalence and stronger mixing/separation
# -----------------------------------------------------------------------------
label_diagnostics <- function(balls, label, edges) {
  n <- length(label)
  nb <- length(balls)
  ball <- do.call(rbind, lapply(seq_along(balls), function(i) {
    idx <- balls[[i]]
    nl <- sum(label[idx] == 1L)
    data.frame(ball_id=i, ball_size=length(idx), labelled=nl,
               unlabelled=length(idx)-nl,
               labelled_share=if(length(idx)) nl/length(idx) else NA_real_)
  }))
  occupied <- which(ball$labelled > 0L)
  overall <- mean(label == 1L)
  memberships_label <- sum(ball$labelled[occupied])
  memberships_all <- sum(ball$ball_size[occupied])
  p <- ball$labelled[occupied]
  p <- p / sum(p)
  ent <- if (length(p) <= 1L) 0 else -sum(p * log(p)) / log(length(p))

  # Constituency-level co-membership mixing: unique neighbours sharing >=1 ball.
  point_balls <- vector("list", n)
  for (b in seq_along(balls)) for (idx in balls[[b]]) point_balls[[idx]] <- c(point_balls[[idx]], b)
  labelled_idx <- which(label == 1L)
  co_nonlabel <- rep(NA_real_, length(labelled_idx))
  co_n <- integer(length(labelled_idx))
  for (ii in seq_along(labelled_idx)) {
    i <- labelled_idx[ii]
    bs <- point_balls[[i]]
    neigh <- unique(unlist(balls[bs], use.names = FALSE))
    neigh <- setdiff(neigh, i)
    co_n[ii] <- length(neigh)
    if (length(neigh)) co_nonlabel[ii] <- mean(label[neigh] == 0L)
  }

  # Ball-graph boundary / conductance for the set of balls containing >=1 labelled seat.
  boundary_edges <- incident_edges <- internal_edges <- 0L
  boundary_weight <- incident_weight <- internal_weight <- 0
  conductance <- NA_real_
  if (nrow(edges)) {
    inS <- seq_len(nb) %in% occupied
    a <- edges$from; b <- edges$to; w <- edges$strength
    inc <- inS[a] | inS[b]
    cross <- xor(inS[a], inS[b])
    internal <- inS[a] & inS[b]
    incident_edges <- sum(inc)
    boundary_edges <- sum(cross)
    internal_edges <- sum(internal)
    incident_weight <- sum(w[inc])
    boundary_weight <- sum(w[cross])
    internal_weight <- sum(w[internal])
    deg <- numeric(nb)
    for (i in seq_len(nrow(edges))) {
      deg[a[i]] <- deg[a[i]] + w[i]
      deg[b[i]] <- deg[b[i]] + w[i]
    }
    volS <- sum(deg[inS]); volT <- sum(deg[!inS])
    den <- min(volS, volT)
    if (den > 0) conductance <- boundary_weight / den
  }

  data.frame(
    n_labelled = sum(label), balls_total = nb, balls_with_label = length(occupied),
    overall_label_prevalence = overall,
    max_label_share = max(ball$labelled_share, na.rm = TRUE),
    majority_label_ball = any(ball$labelled_share > 0.5, na.rm = TRUE),
    exact_half_label_ball = any(abs(ball$labelled_share - 0.5) < 1e-12, na.rm = TRUE),
    exclusive_label_ball = any(ball$labelled_share >= 1 - 1e-12, na.rm = TRUE),
    membership_entropy = ent,
    membership_weighted_local_prevalence = if (memberships_all > 0) memberships_label / memberships_all else NA_real_,
    occupied_balls_above_global_prevalence_share = mean(ball$labelled_share[occupied] > overall),
    occupied_balls_above_twice_global_prevalence_share = mean(ball$labelled_share[occupied] > 2 * overall),
    mean_unique_comembers = mean(co_n),
    mean_nonlabel_share_among_unique_comembers = mean(co_nonlabel, na.rm = TRUE),
    min_nonlabel_share_among_unique_comembers = min(co_nonlabel, na.rm = TRUE),
    boundary_edge_share_of_incident = if (incident_edges > 0) boundary_edges / incident_edges else NA_real_,
    boundary_overlap_weight_share_of_incident = if (incident_weight > 0) boundary_weight / incident_weight else NA_real_,
    occupied_ball_conductance_weighted = conductance,
    internal_edge_share_of_incident = if (incident_edges > 0) internal_edges / incident_edges else NA_real_,
    stringsAsFactors = FALSE
  )
}

ball_table <- function(balls, label) {
  do.call(rbind, lapply(seq_along(balls), function(i) {
    idx <- balls[[i]]
    nl <- sum(label[idx] == 1L)
    data.frame(ball_id=i, ball_size=length(idx), labelled=nl,
               unlabelled=length(idx)-nl,
               labelled_share=nl/length(idx), stringsAsFactors = FALSE)
  }))
}

edge_table <- function(edges, balls, label) {
  bt <- ball_table(balls, label)
  if (!nrow(edges)) return(data.frame())
  data.frame(
    from=edges$from, to=edges$to, strength=edges$strength,
    from_labelled=bt$labelled[edges$from], to_labelled=bt$labelled[edges$to],
    from_share=bt$labelled_share[edges$from], to_share=bt$labelled_share[edges$to],
    crosses_occupied_boundary=xor(bt$labelled[edges$from] > 0, bt$labelled[edges$to] > 0),
    both_occupied=(bt$labelled[edges$from] > 0 & bt$labelled[edges$to] > 0),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Repeated topology helpers
# -----------------------------------------------------------------------------
run_topology_reps <- function(D, radius, reps, base_seed, workers, label = NULL) {
  fun <- function(i) {
    set.seed(base_seed + i - 1L)
    ord <- sample.int(nrow(D))
    tp <- build_cover(D, radius, ord)
    base <- data.frame(replicate=i, seed=base_seed+i-1L, radius=radius,
                       balls=tp$summary$balls, edges=tp$summary$edges,
                       components=tp$summary$components, isolates=tp$summary$isolates,
                       multicoverage=tp$summary$multicoverage,
                       stringsAsFactors=FALSE)
    if (!is.null(label)) cbind(base, label_diagnostics(tp$balls, label, tp$edges)) else base
  }
  if (.Platform$OS.type == "unix" && workers > 1L) {
    z <- parallel::mclapply(seq_len(reps), fun, mc.cores = workers, mc.preschedule = TRUE)
  } else z <- lapply(seq_len(reps), fun)
  do.call(rbind, z)
}

summary_numeric <- function(d, by = character()) {
  num <- names(d)[vapply(d, is.numeric, logical(1))]
  num <- setdiff(num, c(by, "replicate", "seed"))
  if (!length(by)) groups <- list(all=seq_len(nrow(d))) else groups <- split(seq_len(nrow(d)), interaction(d[by], drop=TRUE, lex.order=TRUE))
  out <- lapply(groups, function(ix) {
    z <- d[ix,,drop=FALSE]
    b <- if (length(by)) z[1L,by,drop=FALSE] else data.frame(scope="all")
    for (nm in num) {
      x <- z[[nm]]
      b[[paste0(nm,"_median")]] <- median(x, na.rm=TRUE)
      b[[paste0(nm,"_q05")]] <- as.numeric(stats::quantile(x,0.05,na.rm=TRUE,names=FALSE))
      b[[paste0(nm,"_q95")]] <- as.numeric(stats::quantile(x,0.95,na.rm=TRUE,names=FALSE))
      b[[paste0(nm,"_min")]] <- min(x,na.rm=TRUE)
      b[[paste0(nm,"_max")]] <- max(x,na.rm=TRUE)
    }
    b
  })
  do.call(rbind,out)
}

topology_target <- function(d) {
  c(balls=median(d$balls), edges=median(d$edges), components=median(d$components),
    isolates=median(d$isolates), multicoverage=median(d$multicoverage))
}

topology_loss_row <- function(row, target) {
  vals <- c(balls=row$balls_median, edges=row$edges_median, components=row$components_median,
            isolates=row$isolates_median, multicoverage=row$multicoverage_median)
  sc <- c(balls=max(1,target["balls"]), edges=max(1,target["edges"]), components=max(1,target["components"]), isolates=1, multicoverage=1)
  sum(abs(vals-target)/sc)
}

common_rounded_radius_grid <- function() {
  g <- seq(75L, 2000L, by=5L) / 100
  if (length(g) != 386L || g[1L] != 0.75 || tail(g,1L) != 20.00 ||
      g[96L] != 5.50 || any(abs(g*20 - round(g*20)) > 1e-12)) {
    fail("Rounded common radius-grid contract failed.")
  }
  g
}

select_radius_topology_only <- function(Dnew, Dref, ref_radius, target, reps, workers,
                                        base_seed, grid_n=NULL, span=NULL) {
  ratio <- stats::median(Dnew[upper.tri(Dnew)]) /
           stats::median(Dref[upper.tri(Dref)])
  center <- ref_radius * ratio
  grid <- common_rounded_radius_grid()
  rows <- vector("list", length(grid))
  for (g in seq_along(grid)) {
    cat("  rounded topology-only radius grid ", g, "/", length(grid),
        " radius=", sprintf("%.2f",grid[g]), "\n", sep="")
    rr <- run_topology_reps(Dnew, grid[g], reps,
                            base_seed + g*10000L, workers, label=NULL)
    ss <- summary_numeric(rr)
    ss$radius <- grid[g]
    ss$distance_scale_ratio <- ratio
    ss$scaled_center <- center
    ss$loss <- topology_loss_row(ss,target)
    rows[[g]] <- ss
  }
  tab <- do.call(rbind,rows)
  best <- min(tab$loss,na.rm=TRUE)
  cand <- tab[abs(tab$loss-best)<1e-12,,drop=FALSE]
  chosen <- cand$radius[which.min(abs(cand$radius-center))][1L]
  if (abs(chosen*20-round(chosen*20)) > 1e-12)
    fail("Selected sensitivity radius is not on the 0.05 grid.")
  list(grid=tab,radius=chosen,ratio=ratio,center=center,
       grid_contract="0.75_to_20.00_by_0.05_two_decimal")
}


# -----------------------------------------------------------------------------
# ILR composition-aware geometry
# -----------------------------------------------------------------------------
ilr_block <- function(M, block_name) {
  X <- as.matrix(M); storage.mode(X) <- "double"
  if (any(!is.finite(X))) fail("Non-finite values in composition block ", block_name)
  if (any(X < 0)) fail("Negative values in composition block ", block_name)
  rs <- rowSums(X)
  if (any(rs <= 0)) fail("Non-positive row total in composition block ", block_name)
  C <- X/rs
  positives <- C[C>0]
  if (!length(positives)) fail("No positive values in composition block ", block_name)
  delta <- min(positives)/2
  zero_count <- sum(C==0)
  if (zero_count > 0) {
    C[C==0] <- delta
    C <- C/rowSums(C)
  }
  k <- ncol(C)
  H <- stats::contr.helmert(k)
  V <- qr.Q(qr(H))
  Z <- log(C) %*% V
  colnames(Z) <- paste0(block_name,"_ilr",seq_len(ncol(Z)))
  list(coords=Z, zero_count=zero_count, delta=delta, original_parts=k, ilr_dims=ncol(Z))
}

# -----------------------------------------------------------------------------
# Benchmark stratum detection
# -----------------------------------------------------------------------------
detect_region <- function(dat) {
  n <- names(dat); nn <- norm_name(n)
  exact <- c("region","regionname","governmentofficeregion","gor","gorname","englishregion")
  hit <- which(nn %in% exact)
  if (!length(hit)) hit <- grep("region|governmentofficeregion", nn)
  if (!length(hit)) return(list(ok=FALSE,col=NA_character_,values=NULL,message="No region column identified."))
  # prefer a character/factor column with 2-20 levels
  for (i in hit) {
    v <- trimws(as.character(dat[[i]])); u <- unique(v[!is.na(v) & nzchar(v)])
    if (length(u)>=2 && length(u)<=20) return(list(ok=TRUE,col=n[i],values=v,message="PASS"))
  }
  list(ok=FALSE,col=NA_character_,values=NULL,message=paste("Region-like columns found but not safely usable:",paste(n[hit],collapse="; ")))
}

detect_labour2017 <- function(dat) {
  n <- names(dat); nn <- norm_name(n)
  # Direct binary Labour-held/won indicators.
  hit_bin <- which(grepl("2017",nn) & grepl("labour|lab",nn) & grepl("held|won|winner|win",nn))
  for (i in hit_bin) {
    x <- dat[[i]]
    if (is.logical(x)) return(list(ok=TRUE,col=n[i],values=as.integer(x),message="binary logical"))
    if (is.numeric(x)) {
      u <- unique(x[is.finite(x)])
      if (length(u) && all(u %in% c(0,1))) return(list(ok=TRUE,col=n[i],values=as.integer(x),message="binary numeric"))
    }
    y <- tolower(trimws(as.character(x)))
    yes <- y %in% c("1","true","t","yes","y","labour","lab")
    no <- y %in% c("0","false","f","no","n","conservative","con","other")
    if (all(yes|no|is.na(x)|!nzchar(y))) return(list(ok=TRUE,col=n[i],values=as.integer(yes),message="binary text"))
  }
  # Winner/party column for 2017.
  hit_party <- which(grepl("2017",nn) & grepl("winner|winningparty|party",nn))
  for (i in hit_party) {
    y <- tolower(trimws(as.character(dat[[i]])))
    u <- unique(y[!is.na(y)&nzchar(y)])
    if (length(u)>=2 && length(u)<=20 && any(grepl("labour|^lab$",u))) {
      val <- as.integer(grepl("labour|^lab$", y))
      return(list(ok=TRUE,col=n[i],values=val,message="winner/party text"))
    }
  }
  list(ok=FALSE,col=NA_character_,values=NULL,message="No safely identifiable 2017 Labour-held/winner field.")
}

stratified_draw <- function(strata, observed_label, eligible=NULL) {
  if (is.null(eligible)) eligible <- rep(TRUE,length(observed_label))
  if (length(strata)!=length(observed_label) || length(eligible)!=length(observed_label)) fail("Stratified benchmark length mismatch.")
  strata <- as.character(strata)
  target <- table(strata[observed_label==1L], useNA="no")
  out <- integer(length(observed_label))
  for (s in names(target)) {
    need <- as.integer(target[[s]])
    pool <- which(strata==s & eligible)
    if (length(pool)<need) return(NULL)
    out[sample(pool,need,replace=FALSE)] <- 1L
  }
  if (sum(out)!=sum(observed_label)) return(NULL)
  out
}

benchmark_run <- function(name, balls, edges, observed_label, reps, seed, strata=NULL, eligible=NULL) {
  set.seed(seed)
  out <- vector("list",reps)
  n <- length(observed_label); k <- sum(observed_label)
  for (i in seq_len(reps)) {
    if (is.null(strata)) {
      lab <- integer(n); pool <- if (is.null(eligible)) seq_len(n) else which(eligible)
      if (length(pool)<k) return(NULL)
      lab[sample(pool,k,replace=FALSE)] <- 1L
    } else {
      lab <- stratified_draw(strata,observed_label,eligible)
      if (is.null(lab)) return(NULL)
    }
    out[[i]] <- cbind(data.frame(benchmark=name,draw=i), label_diagnostics(balls,lab,edges))
  }
  do.call(rbind,out)
}

benchmark_summary <- function(bench, obs) {
  metrics <- intersect(names(obs), names(bench))
  metrics <- metrics[vapply(bench[metrics],is.numeric,logical(1))]
  metrics <- setdiff(metrics,c("n_labelled","balls_total","draw"))
  do.call(rbind,lapply(metrics,function(m){
    x <- bench[[m]]; o <- obs[[m]]
    data.frame(benchmark=bench$benchmark[1L],metric=m,observed=o,
               benchmark_median=median(x,na.rm=TRUE),
               benchmark_q05=as.numeric(stats::quantile(x,0.05,na.rm=TRUE,names=FALSE)),
               benchmark_q95=as.numeric(stats::quantile(x,0.95,na.rm=TRUE,names=FALSE)),
               lower_tail_p=(1+sum(x<=o,na.rm=TRUE))/(1+sum(is.finite(x))),
               upper_tail_p=(1+sum(x>=o,na.rm=TRUE))/(1+sum(is.finite(x))),
               stringsAsFactors=FALSE)
  }))
}


# -----------------------------------------------------------------------------
# Accepted 2021 P1.4-B rounded-grid verification
# -----------------------------------------------------------------------------
verify_p14b_grid <- function(app_dir,out_dir) {
  expected_name <- "Otway_Rudkin_2024_Corrected31D_P1_4_B_production_Audit_20260926_223222.zip"
  expected_sha <- "b2e77fcbd78eb048cfccec4f1939787e7b86bfab566b9d14c8fea3a237c58670"
  expected_grid <- common_rounded_radius_grid()
  hits <- list.files(app_dir,pattern=paste0("^",expected_name,"$"),
                     recursive=TRUE,full.names=TRUE)
  status <- "SOURCE_NOT_FOUND"
  detail <- "Accepted production ZIP not located under application directory."
  located <- ""; observed_sha <- ""; matched_csv <- ""
  if (length(hits)) {
    located <- normalizePath(hits[1L],mustWork=TRUE)
    observed_sha <- hash_file(located)
    if (tolower(observed_sha) != tolower(expected_sha)) {
      status <- "SHA256_MISMATCH"
      detail <- paste0("Expected ",expected_sha,"; observed ",observed_sha)
    } else {
      td <- tempfile("p14b_"); dir.create(td)
      utils::unzip(located,exdir=td)
      csvs <- list.files(td,pattern="[.]csv$",recursive=TRUE,full.names=TRUE)
      for (p in csvs) {
        d <- tryCatch(safe_read_csv(p),error=function(e)NULL)
        if (is.null(d)) next
        rc <- names(d)[tolower(names(d))=="radius"]
        if (!length(rc)) next
        vals <- sort(unique(suppressWarnings(as.numeric(d[[rc[1L]]]))))
        vals <- vals[is.finite(vals)]
        if (length(vals)==386L && max(abs(vals-expected_grid))<1e-10) {
          status <- "PASS"
          detail <- "Accepted P1.4-B audit contains exact 386-radius grid 0.75:20.00 by 0.05."
          matched_csv <- p
          break
        }
      }
      if (status != "PASS") {
        status <- "GRID_NOT_VERIFIED"
        detail <- "Accepted ZIP hash matches, but exact grid CSV was not located inside archive."
      }
    }
  }
  write_csv(data.frame(
    check=c("grid_count","grid_first","grid_last","grid_row96","production_audit"),
    status=c("PASS","PASS","PASS","PASS",status),
    detail=c("386","0.75","20.00","5.50",detail),
    stringsAsFactors=FALSE),
    file.path(out_dir,"00a_2021_p14b_rounded_grid_verification.csv"))
  write_csv(data.frame(
    field=c("expected_zip","expected_sha256","located_zip","located_sha256","matched_grid_csv"),
    value=c(expected_name,expected_sha,located,observed_sha,matched_csv),
    stringsAsFactors=FALSE),
    file.path(out_dir,"00b_2021_p14b_grid_provenance.csv"))
}

# -----------------------------------------------------------------------------
# Environment and frozen application discovery
# -----------------------------------------------------------------------------
sp <- script_path(); script_dir <- if (dir.exists(sp)) sp else dirname(sp)
repo <- find_repo_root(script_dir)
app_dir <- file.path(repo,"applications","Otway_Rudkin_2024_Corrected31D")
out_dir <- file.path(script_dir,"outputs")
dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)
verify_p14b_grid(app_dir,out_dir)

workers <- num_env("OTWAY_FOLLOWUP_WORKERS", min(50L,max(1L,parallel::detectCores()-2L)))
target_reps <- num_env("OTWAY_TARGET_REPS",200L)
grid_reps <- num_env("OTWAY_GRID_REPS",60L)
final_reps <- num_env("OTWAY_FINAL_REPS",1000L)
lobo_grid_reps <- num_env("OTWAY_LOBO_GRID_REPS",40L)
lobo_final_reps <- num_env("OTWAY_LOBO_FINAL_REPS",1000L)
benchmark_reps <- num_env("OTWAY_BENCHMARK_REPS",10000L)
approved_radius <- 5.50

source(file.path(repo,"framework","R","TDABMRadiusDiagnostics.R"))
source(file.path(app_dir,"R","OtwayRudkin2024Adapter.R"))
config <- or24_read_contract(file.path(app_dir,"config","application_contract.csv"))
app_id <- or24_get(config,"application_id")
topology_vars <- or24_split(or24_get(config,"topology_variables"))
frozen <- file.path(app_dir,"frozen_input","p1_4_a_v1_0",paste0(app_id,"_input_frozen_P1_4_A_v1_0.csv"))
if (!file.exists(frozen)) fail("Frozen input missing: ",frozen)
dat <- safe_read_csv(frozen)

tdabm_rd_validate_topology_frame(dat,"ONSConstID",topology_vars,scaling="z_score",metric="euclidean",expected_rows=571L,stop_on_failure=TRUE)
prepared <- tdabm_rd_prepare_topology(dat,"ONSConstID",topology_vars,scaling="z_score",metric="euclidean")
if (nrow(prepared$axes)!=571L || ncol(prepared$axes)!=31L) fail("Expected 571x31 prepared topology.")

canonical_dir <- file.path(app_dir,"frozen_topology","p1_4_c_v1_0")
order_file <- file.path(canonical_dir,"canonical_point_order.csv")
canonical_order <- extract_ids_from_order_file(order_file,prepared$ids)
D <- as.matrix(stats::dist(prepared$axes))
canonical <- build_cover(D,approved_radius,canonical_order)
expected <- c(balls=28L,edges=96L,components=2L,isolates=1L)
got <- c(balls=canonical$summary$balls,edges=canonical$summary$edges,components=canonical$summary$components,isolates=canonical$summary$isolates)
if (!identical(as.integer(got),as.integer(expected))) fail("Canonical headline topology failed reproduction: ",paste(names(got),got,collapse="; "))

rw <- redwall_source(dat)
write_csv(rw$audit,file.path(out_dir,"03_redwall_source_match_audit.csv"))
if (!isTRUE(rw$ok)) fail(rw$message)
redwall <- rw$redwall

write_csv(data.frame(field=c("repo_root","app_dir","application_id","frozen_input","frozen_input_sha256","n","p","approved_radius","workers","target_reps","grid_reps","final_reps","lobo_grid_reps","lobo_final_reps","benchmark_reps"),
                     value=c(repo,app_dir,app_id,frozen,hash_file(frozen),nrow(prepared$axes),ncol(prepared$axes),approved_radius,workers,target_reps,grid_reps,final_reps,lobo_grid_reps,lobo_final_reps,benchmark_reps)),
          file.path(out_dir,"00_environment_and_provenance.csv"))
write_csv(data.frame(metric=names(expected),expected=expected,observed=got,pass=got==expected),file.path(out_dir,"01_canonical_validation.csv"))
write_csv(data.frame(column=names(dat),class=vapply(dat,function(x)paste(class(x),collapse="/"),character(1)),
                     nonmissing=vapply(dat,function(x)sum(!is.na(x)),integer(1)),
                     unique_nonmissing=vapply(dat,function(x)length(unique(x[!is.na(x)])),integer(1))),
          file.path(out_dir,"02_input_column_inventory.csv"))

canonical_diag <- label_diagnostics(canonical$balls,redwall,canonical$edges)
write_csv(canonical_diag,file.path(out_dir,"04_canonical_extended_diagnostics.csv"))
write_csv(ball_table(canonical$balls,redwall),file.path(out_dir,"17_canonical_ball_diagnostics.csv"))
write_csv(edge_table(canonical$edges,canonical$balls,redwall),file.path(out_dir,"18_canonical_edge_diagnostics.csv"))

# -----------------------------------------------------------------------------
# Topology-only target from the accepted original geometry
# -----------------------------------------------------------------------------
cat("Building topology-only resolution target from original Corrected31D geometry...\n")
target_raw <- run_topology_reps(D,approved_radius,target_reps,202610200L,workers,label=NULL)
target <- topology_target(target_raw)
write_csv(data.frame(metric=names(target),target=as.numeric(target)),file.path(out_dir,"05_topology_only_resolution_target.csv"))

# -----------------------------------------------------------------------------
# Conceptual block definitions
# -----------------------------------------------------------------------------
tv <- topology_vars
groups <- list(
  density=grep("^population_density$",tv,ignore.case=TRUE),
  age=grep("^age_",tv,ignore.case=TRUE),
  ethnicity=grep("^ethnic_group_",tv,ignore.case=TRUE),
  education=grep("^qualifications_",tv,ignore.case=TRUE),
  industry=grep("^industry_",tv,ignore.case=TRUE),
  nssec=grep("^NSSEC_",tv,ignore.case=TRUE)
)
expected_counts <- c(density=1L,age=6L,ethnicity=1L,education=6L,industry=8L,nssec=9L)
actual_counts <- vapply(groups,length,integer(1))
if (!identical(actual_counts[names(expected_counts)],expected_counts)) fail("Conceptual block definition mismatch: ",paste(names(actual_counts),actual_counts,collapse="; "))

# -----------------------------------------------------------------------------
# ILR sensitivity
# -----------------------------------------------------------------------------
cat("Constructing composition-aware ILR sensitivity geometry...\n")
ilr_parts <- list()
ilr_defs <- list()
for (nm in c("age","education","industry","nssec")) {
  vars <- tv[groups[[nm]]]
  z <- ilr_block(dat[,vars,drop=FALSE],nm)
  ilr_parts[[nm]] <- z$coords
  ilr_defs[[nm]] <- data.frame(block=nm,original_parts=z$original_parts,ilr_dimensions=z$ilr_dims,
                               zero_values_replaced=z$zero_count,zero_replacement_delta=z$delta,
                               variables=paste(vars,collapse=";"),stringsAsFactors=FALSE)
}
scalar_vars <- c(tv[groups$density],tv[groups$ethnicity])
scalar <- zscore_matrix(dat[,scalar_vars,drop=FALSE])
X_ilr <- do.call(cbind,c(list(scalar),ilr_parts))
X_ilr <- zscore_matrix(X_ilr)
D_ilr <- as.matrix(stats::dist(X_ilr))
write_csv(do.call(rbind,ilr_defs),file.path(out_dir,"06_ilr_block_definition.csv"))

cat("Selecting ILR sensitivity radius using topology-only matching...\n")
ilr_sel <- select_radius_topology_only(D_ilr,D,approved_radius,target,grid_reps,workers,202611000L,grid_n=41L,span=0.20)
write_csv(ilr_sel$grid,file.path(out_dir,"07_ilr_radius_grid.csv"))
write_csv(data.frame(selected_radius=ilr_sel$radius,distance_scale_ratio=ilr_sel$ratio,scaled_center=ilr_sel$center,
                     selection_rule="minimum topology-only resolution loss; tie closest to scaled centre"),
          file.path(out_dir,"08_ilr_selected_radius.csv"))
cat("Running ILR Red Wall robustness at selected radius ",format(ilr_sel$radius,digits=10),"...\n",sep="")
ilr_raw <- run_topology_reps(D_ilr,ilr_sel$radius,final_reps,202612000L,workers,label=redwall)
write_csv(ilr_raw,file.path(out_dir,"09_ilr_redwall_replicates.csv"))
ilr_sum <- summary_numeric(ilr_raw)
ilr_sum$majority_label_ball_frequency <- mean(ilr_raw$majority_label_ball)
ilr_sum$exact_half_label_ball_frequency <- mean(ilr_raw$exact_half_label_ball)
ilr_sum$exclusive_label_ball_frequency <- mean(ilr_raw$exclusive_label_ball)
write_csv(ilr_sum,file.path(out_dir,"10_ilr_redwall_summary.csv"))
write_csv(data.frame(
  selected_radius=ilr_sel$radius,
  on_0_05_grid=abs(ilr_sel$radius*20-round(ilr_sel$radius*20))<1e-12,
  two_decimal=abs(ilr_sel$radius-round(ilr_sel$radius,2))<1e-12,
  grid_contract="0.75_to_20.00_by_0.05_two_decimal",
  stringsAsFactors=FALSE),
  file.path(out_dir,"10a_ilr_rounded_radius_gate.csv"))

# -----------------------------------------------------------------------------
# Leave-one-conceptual-block-out sensitivity
# -----------------------------------------------------------------------------
cat("Running leave-one-block-out sensitivities...\n")
lobo_select <- list(); lobo_raw <- list(); li <- 0L
for (nm in names(groups)) {
  cat("LOBO specification: omit ",nm,"\n",sep="")
  keep <- setdiff(seq_along(tv),groups[[nm]])
  X <- prepared$axes[,keep,drop=FALSE]
  Dn <- as.matrix(stats::dist(X))
  ss <- select_radius_topology_only(Dn,D,approved_radius,target,lobo_grid_reps,workers,202620000L+li*1000000L,grid_n=31L,span=0.20)
  g <- ss$grid; g$omitted_block <- nm; lobo_select[[nm]] <- g
  rr <- run_topology_reps(Dn,ss$radius,lobo_final_reps,202625000L+li*1000000L,workers,label=redwall)
  rr$omitted_block <- nm; rr$selected_radius <- ss$radius; rr$distance_scale_ratio <- ss$ratio
  lobo_raw[[nm]] <- rr
  li <- li + 1L
}
lobo_select_tab <- do.call(rbind,lobo_select)
write_csv(lobo_select_tab,file.path(out_dir,"11_lobo_radius_selection.csv"))
lobo_raw_tab <- do.call(rbind,lobo_raw)
lobo_rounding_gate <- unique(lobo_raw_tab[,c("omitted_block","selected_radius"),drop=FALSE])
lobo_rounding_gate$on_0_05_grid <- abs(lobo_rounding_gate$selected_radius*20 -
                                        round(lobo_rounding_gate$selected_radius*20)) < 1e-12
lobo_rounding_gate$two_decimal <- abs(lobo_rounding_gate$selected_radius -
                                       round(lobo_rounding_gate$selected_radius,2)) < 1e-12
write_csv(lobo_rounding_gate,file.path(out_dir,"11a_lobo_rounded_radius_gate.csv"))
write_csv(lobo_raw_tab,file.path(out_dir,"12_lobo_redwall_replicates.csv"))
lobo_sum <- summary_numeric(lobo_raw_tab,by="omitted_block")
maj <- aggregate(as.integer(lobo_raw_tab$majority_label_ball), by=list(omitted_block=lobo_raw_tab$omitted_block), FUN=mean); names(maj)[2] <- "majority_label_ball_frequency"
half <- aggregate(as.integer(lobo_raw_tab$exact_half_label_ball), by=list(omitted_block=lobo_raw_tab$omitted_block), FUN=mean); names(half)[2] <- "exact_half_label_ball_frequency"
excl <- aggregate(as.integer(lobo_raw_tab$exclusive_label_ball), by=list(omitted_block=lobo_raw_tab$omitted_block), FUN=mean); names(excl)[2] <- "exclusive_label_ball_frequency"
lobo_sum <- Reduce(function(x,y) merge(x,y,by="omitted_block",all.x=TRUE,sort=FALSE),list(lobo_sum,maj,half,excl))
write_csv(lobo_sum,file.path(out_dir,"13_lobo_redwall_summary.csv"))

# -----------------------------------------------------------------------------
# Harder benchmarks on frozen canonical topology
# -----------------------------------------------------------------------------
reg <- detect_region(dat)
lab17 <- detect_labour2017(dat)

# Direct frozen-input electoral-history proxy.  This is deliberately NOT called
# a 2017 Labour-winner indicator: the frozen file contains Labour and Conservative
# vote shares, not a complete 2017 winner field.  It therefore supports the
# narrower and auditable condition Labour vote share > Conservative vote share.
labcon_ok <- all(c("Lab17","Con17") %in% names(dat))
if (labcon_ok) {
  lab17_share <- suppressWarnings(as.numeric(dat$Lab17))
  con17_share <- suppressWarnings(as.numeric(dat$Con17))
  labcon_ok <- length(lab17_share)==nrow(dat) && length(con17_share)==nrow(dat) &&
               all(is.finite(lab17_share)) && all(is.finite(con17_share))
}
if (labcon_ok) {
  lab_over_con17 <- as.integer(lab17_share > con17_share)
  labcon_margin17 <- lab17_share - con17_share
  rw_lab_over_con_share <- mean(lab_over_con17[redwall==1L])
  qbreaks <- unique(as.numeric(stats::quantile(labcon_margin17, probs=seq(0,1,0.25), na.rm=TRUE, names=FALSE, type=7)))
  if (length(qbreaks)>=3L) {
    margin_quartile <- cut(labcon_margin17, breaks=qbreaks, include.lowest=TRUE, labels=FALSE)
  } else {
    rr <- rank(labcon_margin17, ties.method="average")
    margin_quartile <- pmin(4L, pmax(1L, ceiling(4*rr/length(rr))))
  }
} else {
  lab_over_con17 <- NULL
  labcon_margin17 <- NULL
  margin_quartile <- NULL
  rw_lab_over_con_share <- NA_real_
}

strata_detection <- data.frame(
  item=c("region","labour_2017_winner","lab17_con17_proxy"),
  ok=c(reg$ok,lab17$ok,labcon_ok),
  column=c(reg$col,lab17$col,if (labcon_ok) "Lab17;Con17" else NA_character_),
  message=c(reg$message,lab17$message,
            if (labcon_ok) paste0("PASS: direct vote-share proxy available; observed Red Wall share with Lab17>Con17=",sprintf("%.4f",rw_lab_over_con_share))
            else "Lab17 and Con17 were not both safely available."),
  stringsAsFactors=FALSE)
write_csv(strata_detection,file.path(out_dir,"14_benchmark_strata_detection.csv"))

# Audit the region and electoral-history constraints used by the harder benchmarks.
if (isTRUE(reg$ok)) {
  regv <- as.character(reg$values)
  region_contract <- data.frame(region_code=sort(unique(regv)),
                                pool_n=as.integer(table(factor(regv,levels=sort(unique(regv))))),
                                redwall_n=as.integer(table(factor(regv[redwall==1L],levels=sort(unique(regv))))),
                                stringsAsFactors=FALSE)
  write_csv(region_contract,file.path(out_dir,"14a_region_benchmark_contract.csv"))
}
if (labcon_ok) {
  electoral_contract <- data.frame(
    ONSConstID=if ("ONSConstID" %in% names(dat)) as.character(dat$ONSConstID) else seq_len(nrow(dat)),
    ConstituencyName=if ("ConstituencyName" %in% names(dat)) as.character(dat$ConstituencyName) else NA_character_,
    redwall=redwall,
    region=if (isTRUE(reg$ok)) as.character(reg$values) else NA_character_,
    Lab17=lab17_share, Con17=con17_share, LabConMargin17=labcon_margin17,
    LabourOverConservative17=lab_over_con17, MarginQuartile=margin_quartile,
    stringsAsFactors=FALSE)
  write_csv(electoral_contract,file.path(out_dir,"14b_electoral_history_proxy_audit.csv"))
}

bench_list <- list()
cat("Running unrestricted random-label benchmark...\n")
bench_list[["unrestricted_41_seat"]] <- benchmark_run("unrestricted_41_seat",canonical$balls,canonical$edges,redwall,benchmark_reps,202630001L)

if (isTRUE(reg$ok)) {
  cat("Running region-stratified benchmark using column ",reg$col,"...\n",sep="")
  br <- benchmark_run("region_stratified",canonical$balls,canonical$edges,redwall,benchmark_reps,202631001L,strata=reg$values)
  if (!is.null(br)) bench_list[["region_stratified"]] <- br
}

if (isTRUE(reg$ok) && isTRUE(lab17$ok)) {
  # Before using this benchmark, verify every observed Red Wall seat is classified Labour-held in 2017.
  rw_lab_share <- mean(lab17$values[redwall==1L]==1L,na.rm=TRUE)
  if (is.finite(rw_lab_share) && rw_lab_share==1) {
    cat("Running region + 2017 Labour-held benchmark using columns ",reg$col," and ",lab17$col,"...\n",sep="")
    br <- benchmark_run("region_plus_labour2017",canonical$balls,canonical$edges,redwall,benchmark_reps,202632001L,
                        strata=reg$values,eligible=(lab17$values==1L))
    if (!is.null(br)) bench_list[["region_plus_labour2017"]] <- br
  } else {
    strata_detection <- rbind(strata_detection,data.frame(item="region_plus_labour2017_contract",ok=FALSE,column=lab17$col,
      message=paste0("Not run: only ",sprintf("%.4f",rw_lab_share)," of observed Red Wall seats classified Labour-held in 2017; requires review.")))
    write_csv(strata_detection,file.path(out_dir,"14_benchmark_strata_detection.csv"))
  }
}

# Region + direct Labour-over-Conservative 2017 proxy.  This is an electoral-history
# sensitivity, not a claim that Lab17>Con17 proves the formal 2017 winner.
if (isTRUE(reg$ok) && isTRUE(labcon_ok)) {
  if (is.finite(rw_lab_over_con_share) && abs(rw_lab_over_con_share-1) < 1e-12) {
    cat("Running region + Labour-over-Conservative 2017 proxy benchmark...\n")
    br <- benchmark_run("region_plus_labour_over_conservative_2017",
                        canonical$balls,canonical$edges,redwall,benchmark_reps,202633001L,
                        strata=reg$values,eligible=(lab_over_con17==1L))
    if (!is.null(br)) bench_list[["region_plus_labour_over_conservative_2017"]] <- br
  } else {
    strata_detection <- rbind(strata_detection,data.frame(
      item="region_plus_labour_over_conservative_2017_contract",ok=FALSE,column="Lab17;Con17",
      message=paste0("Not run: observed Red Wall share with Lab17>Con17=",sprintf("%.4f",rw_lab_over_con_share),
                     "; benchmark requires all observed Red Wall seats to satisfy the proxy."),
      stringsAsFactors=FALSE))
  }

  # Preserve region and a broad quartile of the 2017 Labour-Conservative margin.
  # Quartiles are computed on the full frozen 571-seat frame and are used only as
  # benchmark strata; the canonical topology remains unchanged.
  rq <- interaction(as.character(reg$values),as.character(margin_quartile),drop=TRUE,lex.order=TRUE)
  cat("Running region + 2017 Labour-Conservative margin-quartile benchmark...\n")
  br <- benchmark_run("region_plus_2017_labcon_margin_quartile",
                      canonical$balls,canonical$edges,redwall,benchmark_reps,202634001L,
                      strata=rq)
  if (!is.null(br)) bench_list[["region_plus_2017_labcon_margin_quartile"]] <- br

  # Record target and pool sizes for every combined stratum, so reviewers can see
  # whether the conditioning becomes nearly deterministic in any cell.
  target_tab <- table(rq[redwall==1L])
  pool_tab <- table(rq)
  contract_levels <- union(names(pool_tab),names(target_tab))
  conditional_contract <- data.frame(
    stratum=contract_levels,
    pool_n=as.integer(pool_tab[contract_levels]),
    redwall_target_n=as.integer(target_tab[contract_levels]),
    stringsAsFactors=FALSE)
  conditional_contract$pool_n[is.na(conditional_contract$pool_n)] <- 0L
  conditional_contract$redwall_target_n[is.na(conditional_contract$redwall_target_n)] <- 0L
  conditional_contract$pool_to_target_ratio <- ifelse(conditional_contract$redwall_target_n>0,
                                                       conditional_contract$pool_n/conditional_contract$redwall_target_n,NA_real_)
  write_csv(conditional_contract,file.path(out_dir,"14c_region_margin_quartile_contract.csv"))
  write_csv(strata_detection,file.path(out_dir,"14_benchmark_strata_detection.csv"))
}

bench_tab <- do.call(rbind,bench_list)
write_csv(bench_tab,file.path(out_dir,"15_benchmark_replicates.csv"))
bench_sum <- do.call(rbind,lapply(bench_list,benchmark_summary,obs=canonical_diag))
write_csv(bench_sum,file.path(out_dir,"16_benchmark_summary.csv"))

# -----------------------------------------------------------------------------
# Interpretive gates: flags for review, not automated substantive acceptance
# -----------------------------------------------------------------------------
get_med <- function(sumtab,name) {
  nm <- paste0(name,"_median"); if (nm %in% names(sumtab)) sumtab[[nm]][1L] else NA_real_
}
gates <- data.frame(
  gate=c(
    "canonical_reproduced",
    "ilr_no_majority_in_95pct_or_more_runs",
    "ilr_no_exclusive_ball_in_95pct_or_more_runs",
    "lobo_no_spec_has_majority_in_more_than_5pct_runs",
    "lobo_no_spec_has_exclusive_ball_in_more_than_5pct_runs",
    "region_benchmark_available",
    "region_plus_labour2017_winner_benchmark_available",
    "region_plus_labour_over_conservative_2017_benchmark_available",
    "region_plus_2017_labcon_margin_quartile_benchmark_available"
  ),
  pass_or_available=c(
    all(got==expected),
    mean(ilr_raw$majority_label_ball)<=0.05,
    mean(ilr_raw$exclusive_label_ball)<=0.05,
    max(lobo_sum$majority_label_ball_frequency,na.rm=TRUE)<=0.05,
    max(lobo_sum$exclusive_label_ball_frequency,na.rm=TRUE)<=0.05,
    "region_stratified" %in% names(bench_list),
    "region_plus_labour2017" %in% names(bench_list),
    "region_plus_labour_over_conservative_2017" %in% names(bench_list),
    "region_plus_2017_labcon_margin_quartile" %in% names(bench_list)
  ),
  note=c(
    "Technical validation against frozen canonical headline topology.",
    "Sensitivity flag only; interpret prevalence distribution as well as majority threshold.",
    "Sensitivity flag only.",
    "Checks whether any single conceptual domain is carrying majority-ball separation.",
    "Checks whether any single conceptual domain is carrying strict exclusivity.",
    if (isTRUE(reg$ok)) paste("Uses",reg$col) else reg$message,
    if (isTRUE(reg$ok)&&isTRUE(lab17$ok)) paste("Candidate columns:",reg$col,"+",lab17$col) else "No direct 2017 winner field was safely detected.",
    if (isTRUE(reg$ok)&&isTRUE(labcon_ok)) paste0("Vote-share proxy; observed Red Wall Lab17>Con17 share=",sprintf("%.4f",rw_lab_over_con_share)) else "Lab17/Con17 proxy unavailable.",
    if (isTRUE(reg$ok)&&isTRUE(labcon_ok)) "Preserves region and broad 2017 Labour-Conservative margin quartile." else "Required fields unavailable."
  ),
  stringsAsFactors=FALSE
)
write_csv(gates,file.path(out_dir,"19_interpretive_gates.csv"))

# -----------------------------------------------------------------------------
# Manuscript-facing summary
# -----------------------------------------------------------------------------
bs <- bench_sum
bench_line <- function(bname,metric) {
  z <- bs[bs$benchmark==bname & bs$metric==metric,,drop=FALSE]
  if (!nrow(z)) return(paste0("- `",bname,"`: not available."))
  paste0("- `",bname,"`, ",metric,": observed=",format(z$observed[1],digits=5),
         "; median=",format(z$benchmark_median[1],digits=5),
         "; 5th–95th=",format(z$benchmark_q05[1],digits=5),"–",format(z$benchmark_q95[1],digits=5),
         "; lower-tail p=",format(z$lower_tail_p[1],digits=5),
         "; upper-tail p=",format(z$upper_tail_p[1],digits=5),".")
}

summary_md <- c(
  "# Otway–Rudkin BJPolS adversarial empirical follow-up v1.1",
  "",
  "## Status",
  "This run is a sensitivity/diagnostic package. It does not replace or mutate the frozen P1.4-C canonical topology.",
  "",
  "## Canonical extended diagnostics",
  paste0("- Red Wall balls: ",canonical_diag$balls_with_label," of ",canonical_diag$balls_total,"."),
  paste0("- Maximum Red Wall share: ",sprintf("%.4f",canonical_diag$max_label_share),"."),
  paste0("- Membership-weighted local Red Wall prevalence across occupied balls: ",sprintf("%.4f",canonical_diag$membership_weighted_local_prevalence),"."),
  paste0("- Mean non-Red-Wall share among each Red Wall seat's unique Ball-Mapper co-members: ",sprintf("%.4f",canonical_diag$mean_nonlabel_share_among_unique_comembers),"."),
  paste0("- Weighted conductance of the Red-Wall-occupied ball set: ",sprintf("%.4f",canonical_diag$occupied_ball_conductance_weighted),"."),
  paste0("- Boundary overlap-weight share among edges incident to Red-Wall-occupied balls: ",sprintf("%.4f",canonical_diag$boundary_overlap_weight_share_of_incident),"."),
  "",
  "## Composition-aware ILR sensitivity",
  paste0("- Selected topology-matched radius: ",format(ilr_sel$radius,digits=8),"."),
  paste0("- Majority-Red-Wall-ball frequency: ",sprintf("%.4f",mean(ilr_raw$majority_label_ball)),"."),
  paste0("- Exact-50%-Red-Wall-ball frequency: ",sprintf("%.4f",mean(ilr_raw$exact_half_label_ball)),"."),
  paste0("- Exclusive-Red-Wall-ball frequency: ",sprintf("%.4f",mean(ilr_raw$exclusive_label_ball)),"."),
  paste0("- Median maximum within-ball Red Wall share: ",sprintf("%.4f",get_med(ilr_sum,"max_label_share")),"."),
  paste0("- Median weighted conductance: ",sprintf("%.4f",get_med(ilr_sum,"occupied_ball_conductance_weighted")),"."),
  "",
  "## Leave-one-block-out sensitivity",
  paste0("- Highest majority-ball frequency across omitted blocks: ",sprintf("%.4f",max(lobo_sum$majority_label_ball_frequency,na.rm=TRUE)),"."),
  paste0("- Highest exclusive-ball frequency across omitted blocks: ",sprintf("%.4f",max(lobo_sum$exclusive_label_ball_frequency,na.rm=TRUE)),"."),
  "See `13_lobo_redwall_summary.csv` for the domain-specific results.",
  "",
  "## Benchmarks: occupied-ball concentration",
  bench_line("unrestricted_41_seat","balls_with_label"),
  bench_line("region_stratified","balls_with_label"),
  bench_line("region_plus_labour2017","balls_with_label"),
  bench_line("region_plus_labour_over_conservative_2017","balls_with_label"),
  bench_line("region_plus_2017_labcon_margin_quartile","balls_with_label"),
  "",
  "## Benchmarks: graph mixing",
  bench_line("unrestricted_41_seat","occupied_ball_conductance_weighted"),
  bench_line("region_stratified","occupied_ball_conductance_weighted"),
  bench_line("region_plus_labour2017","occupied_ball_conductance_weighted"),
  bench_line("region_plus_labour_over_conservative_2017","occupied_ball_conductance_weighted"),
  bench_line("region_plus_2017_labcon_margin_quartile","occupied_ball_conductance_weighted"),
  bench_line("unrestricted_41_seat","mean_nonlabel_share_among_unique_comembers"),
  bench_line("region_stratified","mean_nonlabel_share_among_unique_comembers"),
  bench_line("region_plus_labour2017","mean_nonlabel_share_among_unique_comembers"),
  bench_line("region_plus_labour_over_conservative_2017","mean_nonlabel_share_among_unique_comembers"),
  bench_line("region_plus_2017_labcon_margin_quartile","mean_nonlabel_share_among_unique_comembers"),
  "",
  "## Interpretation rule",
  "Do not treat any single majority-ball threshold as decisive. Read concentration, local prevalence, boundary/conductance and co-membership mixing together. Benchmark availability depends on frozen-input strata and is recorded explicitly in `14_benchmark_strata_detection.csv`. The Labour-over-Conservative test is explicitly a vote-share proxy, not a reconstructed 2017 winner field.",
  "",
  "## Next decision",
  "If the concentration-without-discreteness result survives the ILR and LOBO sensitivities and remains non-trivial against the region/electoral-history benchmarks, the next empirical decision is whether to build a harmonised 2011 Census sensitivity to retain the temporal consolidation account. If it does not survive, repair the core specification before any 2011 work."
)
writeLines(summary_md,file.path(out_dir,"MANUSCRIPT_FACING_SUMMARY.md"),useBytes=TRUE)

capture.output(sessionInfo(),file=file.path(out_dir,"sessionInfo.txt"))

outs <- list.files(out_dir,recursive=TRUE,full.names=TRUE)
outs <- outs[file.info(outs)$isdir %in% FALSE]
manifest <- data.frame(relative_path=sub(paste0("^",normalizePath(out_dir,winslash="/"),"/"),"",normalizePath(outs,winslash="/",mustWork=TRUE)),
                       bytes=file.info(outs)$size,
                       sha256=vapply(outs,hash_file,character(1)),stringsAsFactors=FALSE)
write_csv(manifest,file.path(out_dir,"artifact_manifest.csv"))

cat("\nPASS_TECHNICAL_RUN\n")
cat("Review outputs/MANUSCRIPT_FACING_SUMMARY.md and return the handback ZIP created by run_all.sh.\n")
