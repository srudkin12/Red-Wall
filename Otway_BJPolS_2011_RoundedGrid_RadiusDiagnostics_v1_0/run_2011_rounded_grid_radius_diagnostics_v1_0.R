options(stringsAsFactors = FALSE)
options(width = 200)
cat("OTWAY_BJPOLS_2011_ROUNDED_GRID_RADIUS_DIAGNOSTICS_V1_0\n")

fail <- function(...) stop(paste0(...), call. = FALSE)

script_path <- function() {
  x <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", x[grep("^--file=", x)])
  if (!length(f)) return(normalizePath(".", winslash="/", mustWork=TRUE))
  normalizePath(f[[1L]], winslash="/", mustWork=TRUE)
}

find_repo_root <- function(start_dir) {
  p <- normalizePath(start_dir, winslash="/", mustWork=TRUE)
  for (i in 0:12) {
    fw <- file.path(p,"framework","R","TDABMRadiusDiagnostics.R")
    if (file.exists(fw)) return(p)
    q <- dirname(p)
    if (identical(q,p)) break
    p <- q
  }
  fail("Could not discover TDABM_Portability root from ", start_dir)
}

num_env <- function(name, default, minimum=1L) {
  z <- Sys.getenv(name, "")
  if (!nzchar(z)) z <- as.character(default)
  v <- suppressWarnings(as.integer(z))
  if (is.na(v) || v < minimum) fail("Invalid environment variable ",name,"=",z)
  v
}

write_csv <- function(x,path) {
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  utils::write.csv(x,path,row.names=FALSE,na="")
}

safe_read_csv <- function(path) {
  utils::read.csv(path,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8")
}

hash_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  if (nzchar(Sys.which("sha256sum"))) {
    x <- system2("sha256sum", shQuote(path), stdout=TRUE)
    return(strsplit(x[[1L]],"\\s+")[[1L]][[1L]])
  }
  unname(tools::md5sum(path))
}

jaccard_set <- function(a,b) {
  a <- unique(a); b <- unique(b)
  u <- union(a,b)
  if (!length(u)) return(1)
  length(intersect(a,b))/length(u)
}

build_edges_fast <- function(balls,n) {
  if (length(balls) < 2L) {
    return(data.frame(from=integer(),to=integer(),strength=integer()))
  }
  pb <- vector("list",n)
  for (b in seq_along(balls)) {
    for (i in balls[[b]]) pb[[i]] <- c(pb[[i]],b)
  }
  keys <- character()
  for (x in pb) {
    x <- unique(x)
    if (length(x)>=2L) {
      cm <- utils::combn(sort(x),2L)
      keys <- c(keys,paste(cm[1L,],cm[2L,],sep=":"))
    }
  }
  if (!length(keys)) return(data.frame(from=integer(),to=integer(),strength=integer()))
  tt <- table(keys)
  kk <- names(tt)
  sp <- strsplit(kk,":",fixed=TRUE)
  data.frame(
    from=as.integer(vapply(sp,`[[`,character(1),1L)),
    to=as.integer(vapply(sp,`[[`,character(1),2L)),
    strength=as.integer(tt),
    stringsAsFactors=FALSE
  )
}

connected_components <- function(nballs,edges) {
  if (!nballs) return(integer())
  adj <- vector("list",nballs)
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      a <- edges$from[i]; b <- edges$to[i]
      adj[[a]] <- c(adj[[a]],b)
      adj[[b]] <- c(adj[[b]],a)
    }
  }
  comp <- integer(nballs); cid <- 0L
  for (s in seq_len(nballs)) {
    if (comp[s] != 0L) next
    cid <- cid + 1L
    q <- s; comp[s] <- cid
    while (length(q)) {
      v <- q[[1L]]; q <- q[-1L]
      nb <- unique(adj[[v]])
      fresh <- nb[comp[nb]==0L]
      if (length(fresh)) {
        comp[fresh] <- cid
        q <- c(q,fresh)
      }
    }
  }
  comp
}

build_cover <- function(D,radius,order_idx) {
  n <- nrow(D)
  if (length(order_idx)!=n || !setequal(order_idx,seq_len(n))) fail("Invalid landmark order.")
  covered <- rep(FALSE,n)
  balls <- list(); landmarks <- integer()
  eps <- 1e-12
  for (idx in order_idx) {
    if (covered[idx]) next
    members <- which(D[idx,] <= radius + eps)
    balls[[length(balls)+1L]] <- members
    landmarks <- c(landmarks,idx)
    covered[members] <- TRUE
    if (all(covered)) break
  }
  if (!all(covered)) fail("Ball Mapper failed to cover all observations.")
  edges <- build_edges_fast(balls,n)
  cc <- connected_components(length(balls),edges)
  memberships <- integer(n)
  for (b in balls) memberships[b] <- memberships[b] + 1L
  ball_sizes <- vapply(balls,length,integer(1))
  comp_sizes <- if (length(cc)) tabulate(cc,nbins=max(cc)) else integer()
  list(
    balls=balls,landmarks=landmarks,edges=edges,components=cc,
    summary=data.frame(
      balls=length(balls),
      edges=nrow(edges),
      components=if(length(cc))max(cc) else 0L,
      isolates=if(length(comp_sizes))sum(comp_sizes==1L) else 0L,
      largest_component_ball_share=if(length(comp_sizes))max(comp_sizes)/length(balls) else NA_real_,
      multicoverage=mean(memberships>=2L),
      mean_memberships=mean(memberships),
      ball_size_median=stats::median(ball_sizes),
      ball_size_mean=mean(ball_sizes),
      ball_size_max=max(ball_sizes),
      stringsAsFactors=FALSE
    )
  )
}

comembership_signature <- function(balls,n) {
  M <- matrix(FALSE,n,n)
  for (b in balls) {
    if (length(b)>=2L) M[b,b] <- TRUE
  }
  diag(M) <- FALSE
  M[upper.tri(M)]
}

jaccard_logical <- function(a,b) {
  u <- a | b
  if (!any(u)) return(1)
  sum(a & b)/sum(u)
}

run_reps <- function(D,radius,reps,base_seed,workers) {
  f <- function(i) {
    set.seed(base_seed + i - 1L)
    ord <- sample.int(nrow(D))
    tp <- build_cover(D,radius,ord)
    cbind(
      data.frame(replicate=i,seed=base_seed+i-1L,radius=radius,stringsAsFactors=FALSE),
      tp$summary
    )
  }
  if (.Platform$OS.type=="unix" && workers>1L) {
    z <- parallel::mclapply(seq_len(reps),f,mc.cores=workers,mc.preschedule=TRUE)
  } else z <- lapply(seq_len(reps),f)
  do.call(rbind,z)
}

summarise_reps <- function(d) {
  metrics <- c("balls","edges","components","isolates","largest_component_ball_share",
               "multicoverage","mean_memberships","ball_size_median","ball_size_mean","ball_size_max")
  out <- data.frame(radius=d$radius[1L],reps=nrow(d),stringsAsFactors=FALSE)
  for (m in metrics) {
    x <- d[[m]]
    out[[paste0(m,"_median")]] <- stats::median(x,na.rm=TRUE)
    out[[paste0(m,"_q05")]] <- as.numeric(stats::quantile(x,.05,na.rm=TRUE,names=FALSE))
    out[[paste0(m,"_q95")]] <- as.numeric(stats::quantile(x,.95,na.rm=TRUE,names=FALSE))
    out[[paste0(m,"_min")]] <- min(x,na.rm=TRUE)
    out[[paste0(m,"_max")]] <- max(x,na.rm=TRUE)
  }
  out
}

stability_radius <- function(D,radius,reps,base_seed) {
  covers <- vector("list",reps)
  sigs <- vector("list",reps)
  landmarks <- vector("list",reps)
  for (i in seq_len(reps)) {
    set.seed(base_seed+i-1L)
    ord <- sample.int(nrow(D))
    tp <- build_cover(D,radius,ord)
    covers[[i]] <- tp
    landmarks[[i]] <- tp$landmarks
    sigs[[i]] <- comembership_signature(tp$balls,nrow(D))
  }
  if (reps<2L) {
    return(data.frame(radius=radius,stability_reps=reps,
                      mean_pairwise_landmark_jaccard=NA_real_,
                      median_pairwise_landmark_jaccard=NA_real_,
                      mean_pairwise_comembership_jaccard=NA_real_,
                      median_pairwise_comembership_jaccard=NA_real_))
  }
  lj <- numeric(reps*(reps-1L)/2L)
  cj <- numeric(length(lj)); k <- 0L
  for (i in seq_len(reps-1L)) {
    for (j in (i+1L):reps) {
      k <- k+1L
      lj[k] <- jaccard_set(landmarks[[i]],landmarks[[j]])
      cj[k] <- jaccard_logical(sigs[[i]],sigs[[j]])
    }
  }
  data.frame(
    radius=radius,stability_reps=reps,
    mean_pairwise_landmark_jaccard=mean(lj),
    median_pairwise_landmark_jaccard=stats::median(lj),
    q05_pairwise_landmark_jaccard=as.numeric(stats::quantile(lj,.05,names=FALSE)),
    mean_pairwise_comembership_jaccard=mean(cj),
    median_pairwise_comembership_jaccard=stats::median(cj),
    q05_pairwise_comembership_jaccard=as.numeric(stats::quantile(cj,.05,names=FALSE)),
    stringsAsFactors=FALSE
  )
}

# -------------------------------------------------------------------
# Paths and contract
# -------------------------------------------------------------------
sp <- script_path()
script_dir <- if (dir.exists(sp)) sp else dirname(sp)
repo <- find_repo_root(script_dir)
out_dir <- file.path(script_dir,"outputs")
freeze_dir <- file.path(out_dir,"frozen_input","s2011_a_v1_0")
dir.create(freeze_dir,recursive=TRUE,showWarnings=FALSE)

workers <- num_env("OTWAY_2011_RADIUS_WORKERS",min(50L,max(1L,parallel::detectCores()-2L)))
radius_reps <- num_env("OTWAY_2011_RADIUS_REPS",1000L)
stability_reps <- num_env("OTWAY_2011_STABILITY_REPS",20L)

input_path <- file.path(script_dir,"input","Otway_Rudkin_2011_Harmonised31D_input_freeze_candidate_v1_0.csv")
input_sha_path <- paste0(input_path,".sha256")
vars_path <- file.path(script_dir,"input","topology_variables.csv")
gate_path <- file.path(script_dir,"reference","v1_6_preflight_gates.csv")
for (p in c(input_path,input_sha_path,vars_path,gate_path)) if (!file.exists(p)) fail("Required package input missing: ",p)

expected_sha <- strsplit(readLines(input_sha_path,warn=FALSE)[1L],"\\s+")[[1L]][1L]
observed_sha <- hash_file(input_path)
if (!identical(tolower(expected_sha),tolower(observed_sha))) {
  fail("Embedded freeze-candidate checksum mismatch.")
}

gates <- safe_read_csv(gate_path)
og <- gates$status[gates$gate=="overall_preflight"]
if (length(og)!=1L || og!="PASS_HARMONISATION_READY_FOR_FREEZE") {
  fail("Embedded v1.6 preflight gate is not PASS_HARMONISATION_READY_FOR_FREEZE.")
}

vars <- safe_read_csv(vars_path)
topology_vars <- as.character(vars$topology_variable)
if (length(topology_vars)!=31L || anyDuplicated(topology_vars)) fail("Topology variable contract is not 31 unique variables.")

dat <- safe_read_csv(input_path)
if (nrow(dat)!=571L || anyDuplicated(dat$ONSConstID)) fail("Freeze candidate must be 571 unique ONSConstID rows.")
if (!all(topology_vars %in% names(dat))) fail("Freeze candidate lacks topology variables.")
if (any(!is.finite(as.matrix(dat[,topology_vars,drop=FALSE])))) fail("Freeze candidate has non-finite topology values.")

# -------------------------------------------------------------------
# Freeze authoritative 2011 input
# -------------------------------------------------------------------
frozen_name <- "Otway_Rudkin_2011_Harmonised31D_input_frozen_S2011_A_v1_0.csv"
frozen_path <- file.path(freeze_dir,frozen_name)
write_csv(dat[,c("ONSConstID","ConstituencyName_2021_frame","ConstituencyName_2011_Nomis",topology_vars),drop=FALSE],frozen_path)
frozen_sha <- hash_file(frozen_path)
writeLines(paste(frozen_sha,basename(frozen_path)),paste0(frozen_path,".sha256"))

# -------------------------------------------------------------------
# Same topology preparation framework as the accepted 2021 project
# -------------------------------------------------------------------
source(file.path(repo,"framework","R","TDABMRadiusDiagnostics.R"))
tdabm_rd_validate_topology_frame(
  dat,"ONSConstID",topology_vars,
  scaling="z_score",metric="euclidean",
  expected_rows=571L,stop_on_failure=TRUE
)
prepared <- tdabm_rd_prepare_topology(
  dat,"ONSConstID",topology_vars,
  scaling="z_score",metric="euclidean"
)
if (nrow(prepared$axes)!=571L || ncol(prepared$axes)!=31L) fail("Prepared topology is not 571x31.")

# Verify framework scaling against explicit sample-z-score reconstruction.
rawX <- as.matrix(dat[,topology_vars,drop=FALSE]); storage.mode(rawX) <- "double"
mu <- colMeans(rawX)
sdv <- apply(rawX,2L,stats::sd)
explicit_axes <- sweep(sweep(rawX,2L,mu,"-"),2L,sdv,"/")
scale_error <- max(abs(explicit_axes-prepared$axes))
if (!is.finite(scale_error) || scale_error > 1e-10) fail("Framework z-score validation failed: max error=",scale_error)

write_csv(data.frame(variable=topology_vars,mean_2011=mu,sd_2011=sdv,stringsAsFactors=FALSE),
          file.path(out_dir,"03_scaling_parameters.csv"))
axes_out <- data.frame(ONSConstID=prepared$ids,prepared$axes,check.names=FALSE)
write_csv(axes_out,file.path(out_dir,"04_scaled_axes.csv"))

# -------------------------------------------------------------------
# Researcher-specified rounded common radius grid
# -------------------------------------------------------------------
D <- as.matrix(stats::dist(prepared$axes))
dv <- D[upper.tri(D)]
Dnn <- D
diag(Dnn) <- Inf
nn <- apply(Dnn,1L,min)

distance_summary <- data.frame(
  quantity=c("nearest_neighbour","pairwise"),
  q00=c(min(nn),min(dv)),
  q01=c(quantile(nn,.01),quantile(dv,.01)),
  q05=c(quantile(nn,.05),quantile(dv,.05)),
  q10=c(quantile(nn,.10),quantile(dv,.10)),
  q25=c(quantile(nn,.25),quantile(dv,.25)),
  q50=c(quantile(nn,.50),quantile(dv,.50)),
  q75=c(quantile(nn,.75),quantile(dv,.75)),
  q90=c(quantile(nn,.90),quantile(dv,.90)),
  q95=c(quantile(nn,.95),quantile(dv,.95)),
  q99=c(quantile(nn,.99),quantile(dv,.99)),
  q100=c(max(nn),max(dv)),
  stringsAsFactors=FALSE
)
write_csv(distance_summary,file.path(out_dir,"05_geometry_distance_summary.csv"))

radii <- seq(75L,2000L,by=5L)/100
if (length(radii)!=386L || radii[1L]!=0.75 || tail(radii,1L)!=20.00 ||
    radii[96L]!=5.50 || any(abs(radii*20-round(radii*20))>1e-12)) {
  fail("Rounded common radius-grid contract failed.")
}
grid <- data.frame(
  radius_index=seq_along(radii),
  radius=radii,
  radius_display=sprintf("%.2f",radii),
  grid_rule="researcher-specified common grid 0.75 to 20.00 inclusive by 0.05",
  precision_policy="substantive_rounding_two_decimal",
  stringsAsFactors=FALSE
)
write_csv(grid,file.path(out_dir,"06_radius_grid.csv"))

# -------------------------------------------------------------------
# Baseline topology and repeated landmark-order screening
# -------------------------------------------------------------------
all_reps <- vector("list",length(radii))
all_sum <- vector("list",length(radii))
all_stab <- vector("list",length(radii))
baseline_rows <- vector("list",length(radii))

base_seed <- 201110000L
stab_seed <- 201120000L

for (g in seq_along(radii)) {
  rr <- radii[g]
  cat("Radius ",g,"/",length(radii)," = ",format(rr,digits=10),"\n",sep="")
  b <- build_cover(D,rr,seq_len(nrow(D)))
  baseline_rows[[g]] <- cbind(
    data.frame(radius_index=g,radius=rr,order="frozen_row_order",stringsAsFactors=FALSE),
    b$summary
  )
  reps <- run_reps(D,rr,radius_reps,base_seed+g*100000L,workers)
  reps$radius_index <- g
  all_reps[[g]] <- reps
  sm <- summarise_reps(reps)
  sm$radius_index <- g
  all_sum[[g]] <- sm
  all_stab[[g]] <- stability_radius(D,rr,stability_reps,stab_seed+g*100000L)
}

baseline_tab <- do.call(rbind,baseline_rows)
reps_tab <- do.call(rbind,all_reps)
sum_tab <- do.call(rbind,all_sum)
stab_tab <- do.call(rbind,all_stab)

write_csv(baseline_tab,file.path(out_dir,"07_frozen_order_radius_topologies.csv"))
write_csv(reps_tab,file.path(out_dir,"08_radius_landmark_order_replicates.csv"))
write_csv(sum_tab,file.path(out_dir,"09_radius_summary.csv"))
write_csv(stab_tab,file.path(out_dir,"10_radius_stability_summary.csv"))

joined <- merge(grid,sum_tab,by=c("radius_index","radius"),all.x=TRUE,sort=TRUE)
joined <- merge(joined,stab_tab,by="radius",all.x=TRUE,sort=FALSE)
joined <- joined[order(joined$radius_index),,drop=FALSE]

# Adjacent-radius transition diagnostics. These are descriptive only.
joined$delta_balls_median <- c(NA,diff(joined$balls_median))
joined$delta_edges_median <- c(NA,diff(joined$edges_median))
joined$delta_components_median <- c(NA,diff(joined$components_median))
joined$delta_isolates_median <- c(NA,diff(joined$isolates_median))
joined$delta_multicoverage_median <- c(NA,diff(joined$multicoverage_median))
joined$relative_ball_spread <- (joined$balls_q95-joined$balls_q05)/pmax(1,joined$balls_median)
write_csv(joined,file.path(out_dir,"11_radius_diagnostic_table.csv"))

# Simple visual audit.
png(file.path(out_dir,"12_radius_diagnostics.png"),width=1600,height=1200,res=150)
op <- par(mfrow=c(2,2),mar=c(4,4,2,1))
plot(joined$radius,joined$balls_median,type="l",xlab="Radius",ylab="Median balls")
lines(joined$radius,joined$balls_q05,lty=2); lines(joined$radius,joined$balls_q95,lty=2)
plot(joined$radius,joined$components_median,type="l",xlab="Radius",ylab="Median components")
lines(joined$radius,joined$isolates_median,lty=2)
plot(joined$radius,joined$multicoverage_median,type="l",xlab="Radius",ylab="Median multicoverage")
plot(joined$radius,joined$mean_pairwise_comembership_jaccard,type="l",
     xlab="Radius",ylab="Mean pairwise co-membership Jaccard")
lines(joined$radius,joined$mean_pairwise_landmark_jaccard,lty=2)
legend("bottomright",legend=c("co-membership","landmarks"),lty=c(1,2),bty="n")
par(op)
dev.off()

# -------------------------------------------------------------------
# Freeze and decision governance
# -------------------------------------------------------------------
freeze_validation <- data.frame(
  gate=c("v1_6_harmonisation_pass","embedded_candidate_sha256","rows_571","unique_ons_ids","topology_variables_31","complete_numeric_topology","framework_scaling_match","electoral_fields_absent"),
  status=c("PASS","PASS","PASS","PASS","PASS","PASS","PASS",
           if(any(grepl("RedWall|Lab17|Con17|vote|swing|turnout",names(dat),ignore.case=TRUE)))"FAIL" else "PASS"),
  detail=c(
    "Embedded preflight overall status is PASS_HARMONISATION_READY_FOR_FREEZE.",
    paste0("SHA256=",observed_sha),
    paste0("rows=",nrow(dat)),
    paste0("unique ONSConstID=",length(unique(dat$ONSConstID))),
    paste0("topology variables=",length(topology_vars)),
    "All 31 topology variables finite for all 571 rows.",
    paste0("Maximum explicit-vs-framework z-score difference=",format(scale_error,scientific=TRUE)),
    "Freeze candidate contains only IDs/names plus 31 socio-demographic variables."
  ),
  stringsAsFactors=FALSE
)
if (any(freeze_validation$status!="PASS")) {
  write_csv(freeze_validation,file.path(out_dir,"01_freeze_validation.csv"))
  fail("Freeze validation failed. See 01_freeze_validation.csv.")
}
write_csv(freeze_validation,file.path(out_dir,"01_freeze_validation.csv"))

write_csv(data.frame(
  field=c("repo_root","source_candidate","source_candidate_sha256","frozen_input","frozen_input_sha256",
          "rows","topology_coordinates","scaling","distance_metric","radius_reps","stability_reps",
          "workers","radius_grid_contract","radius_grid_count","radius_approval"),
  value=c(repo,input_path,observed_sha,frozen_path,frozen_sha,nrow(dat),length(topology_vars),
          "z_score","euclidean",radius_reps,stability_reps,workers,
          "0.75_to_20.00_by_0.05_two_decimal",length(radii),
          "NONE_AUTOMATIC_MANUAL_REVIEW_REQUIRED"),
  stringsAsFactors=FALSE
),file.path(out_dir,"00_environment_and_provenance.csv"))

write_csv(data.frame(
  file=c(frozen_path,paste0(frozen_path,".sha256"),file.path(out_dir,"04_scaled_axes.csv")),
  sha256=c(hash_file(frozen_path),hash_file(paste0(frozen_path,".sha256")),hash_file(file.path(out_dir,"04_scaled_axes.csv"))),
  stringsAsFactors=FALSE
),file.path(out_dir,"02_freeze_manifest.csv"))

summary_lines <- c(
  "# 2011 freeze and radius-diagnostics summary",
  "",
  paste0("Generated: ",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Freeze",
  paste0("- Frozen rows: **",nrow(dat),"**."),
  paste0("- Topology coordinates: **",length(topology_vars),"**."),
  paste0("- Frozen input SHA-256: `",frozen_sha,"`."),
  paste0("- Framework scaling cross-check max difference: **",format(scale_error,scientific=TRUE),"**."),
  "",
  "## Radius governance",
  "- Radius grid follows the researcher-specified common rounded production contract.",
  "- Lower bound: 0.75.",
  "- Upper bound: 20.00; increment: 0.05.",
  paste0("- Grid points: **",length(radii),"**."),
  paste0("- Landmark-order replicates per radius: **",radius_reps,"**."),
  paste0("- Stability covers per radius: **",stability_reps,"**."),
  "- No Red Wall labels or electoral outcomes are present in the frozen input or used by this script.",
  "- **No radius has been approved or selected automatically.**",
  "",
  "## Next decision",
  "Review `11_radius_diagnostic_table.csv` and `12_radius_diagnostics.png`.",
  "A later stage may approve one radius based on topology-only stability and resolution.",
  "Only after approval should a canonical 2011 topology be frozen and the Red Wall label introduced."
)
writeLines(summary_lines,file.path(out_dir,"RADIUS_DIAGNOSTICS_SUMMARY.md"))

writeLines(c(
  "# Radius decision required",
  "",
  "This package deliberately does not select a 2011 canonical radius.",
  "",
  "The reviewer should inspect:",
  "",
  "- topology resolution: balls, edges, components and isolates;",
  "- overlap: multicoverage and membership counts;",
  "- landmark-order variability: q05/q95 bands;",
  "- mean/median pairwise landmark Jaccard;",
  "- mean/median pairwise co-membership Jaccard;",
  "- adjacent-radius transitions.",
  "",
  "No Red Wall prevalence, party vote, turnout or electoral outcome may be used to choose the radius.",
  "",
  "A chosen radius must be recorded explicitly in the next stage as a user-approved, topology-only decision."
),file.path(out_dir,"RADIUS_DECISION_REQUIRED.md"))

# Artifact manifest at end.
files <- list.files(out_dir,recursive=TRUE,full.names=TRUE)
files <- files[file.info(files)$isdir %in% FALSE]
manifest <- data.frame(
  relative_path=sub(paste0("^",normalizePath(out_dir,winslash="/"),"/?"),"",
                    normalizePath(files,winslash="/")),
  bytes=file.info(files)$size,
  sha256=vapply(files,hash_file,character(1)),
  stringsAsFactors=FALSE
)
write_csv(manifest,file.path(out_dir,"13_artifact_manifest.csv"))

writeLines(c(
  "status=PASS",
  paste0("generated=",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
  "stage=S2011_A_FREEZE_PLUS_B_RADIUS_DIAGNOSTICS",
  "radius_approval=NONE_AUTOMATIC_MANUAL_REVIEW_REQUIRED",
  paste0("frozen_sha256=",frozen_sha)
),file.path(out_dir,"RUN_STATUS.txt"))

cat("\nPASS: 2011 input frozen and topology-only radius diagnostics completed.\n")
cat("NO RADIUS APPROVED: manual review of the handback is required.\n")
