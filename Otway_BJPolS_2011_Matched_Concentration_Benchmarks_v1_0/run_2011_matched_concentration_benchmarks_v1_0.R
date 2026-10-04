options(stringsAsFactors=FALSE)
options(width=220)
cat("OTWAY_BJPOLS_2011_MATCHED_CONCENTRATION_BENCHMARKS_V1_0\n")

fail <- function(...) stop(paste0(...),call.=FALSE)
safe_read_csv <- function(p) utils::read.csv(p,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8")
write_csv <- function(x,p) {
  dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE)
  utils::write.csv(x,p,row.names=FALSE,na="")
}
script_path <- function() {
  x<-commandArgs(trailingOnly=FALSE)
  f<-sub("^--file=","",x[grep("^--file=",x)])
  if(!length(f)) return(normalizePath(".",winslash="/",mustWork=TRUE))
  normalizePath(f[[1]],winslash="/",mustWork=TRUE)
}
num_env <- function(name,default,minimum=1L) {
  z<-Sys.getenv(name,""); if(!nzchar(z)) z<-as.character(default)
  v<-suppressWarnings(as.integer(z))
  if(is.na(v)||v<minimum) fail("Invalid ",name,"=",z)
  v
}

label_diagnostics <- function(balls,label,edges) {
  n<-length(label); nb<-length(balls)
  ball<-do.call(rbind,lapply(seq_along(balls),function(i) {
    idx<-balls[[i]]; nl<-sum(label[idx]==1L)
    data.frame(ball_id=i,ball_size=length(idx),labelled=nl,
      unlabelled=length(idx)-nl,labelled_share=nl/length(idx))
  }))
  occupied<-which(ball$labelled>0L)
  overall<-mean(label==1L)
  memberships_label<-sum(ball$labelled[occupied])
  memberships_all<-sum(ball$ball_size[occupied])
  p<-ball$labelled[occupied]; p<-p/sum(p)
  ent<-if(length(p)<=1L)0 else -sum(p*log(p))/log(length(p))

  point_balls<-vector("list",n)
  for(b in seq_along(balls)) for(idx in balls[[b]]) point_balls[[idx]]<-c(point_balls[[idx]],b)
  labelled_idx<-which(label==1L)
  co_nonlabel<-rep(NA_real_,length(labelled_idx)); co_n<-integer(length(labelled_idx))
  for(ii in seq_along(labelled_idx)) {
    i<-labelled_idx[ii]; bs<-point_balls[[i]]
    neigh<-unique(unlist(balls[bs],use.names=FALSE)); neigh<-setdiff(neigh,i)
    co_n[ii]<-length(neigh)
    if(length(neigh)) co_nonlabel[ii]<-mean(label[neigh]==0L)
  }

  boundary_edges<-incident_edges<-internal_edges<-0L
  boundary_weight<-incident_weight<-internal_weight<-0
  conductance<-NA_real_
  if(nrow(edges)) {
    inS<-seq_len(nb)%in%occupied; a<-edges$from; b<-edges$to; w<-edges$strength
    inc<-inS[a]|inS[b]; cross<-xor(inS[a],inS[b]); internal<-inS[a]&inS[b]
    incident_edges<-sum(inc); boundary_edges<-sum(cross); internal_edges<-sum(internal)
    incident_weight<-sum(w[inc]); boundary_weight<-sum(w[cross]); internal_weight<-sum(w[internal])
    deg<-numeric(nb)
    for(i in seq_len(nrow(edges))) {
      deg[a[i]]<-deg[a[i]]+w[i]; deg[b[i]]<-deg[b[i]]+w[i]
    }
    den<-min(sum(deg[inS]),sum(deg[!inS]))
    if(den>0) conductance<-boundary_weight/den
  }

  data.frame(
    n_labelled=sum(label),balls_total=nb,balls_with_label=length(occupied),
    overall_label_prevalence=overall,
    max_label_share=max(ball$labelled_share,na.rm=TRUE),
    majority_label_ball=any(ball$labelled_share>0.5,na.rm=TRUE),
    exact_half_label_ball=any(abs(ball$labelled_share-0.5)<1e-12,na.rm=TRUE),
    exclusive_label_ball=any(ball$labelled_share>=1-1e-12,na.rm=TRUE),
    membership_entropy=ent,
    membership_weighted_local_prevalence=if(memberships_all>0)memberships_label/memberships_all else NA_real_,
    occupied_balls_above_global_prevalence_share=mean(ball$labelled_share[occupied]>overall),
    occupied_balls_above_twice_global_prevalence_share=mean(ball$labelled_share[occupied]>2*overall),
    mean_unique_comembers=mean(co_n),
    mean_nonlabel_share_among_unique_comembers=mean(co_nonlabel,na.rm=TRUE),
    min_nonlabel_share_among_unique_comembers=min(co_nonlabel,na.rm=TRUE),
    boundary_edge_share_of_incident=if(incident_edges>0)boundary_edges/incident_edges else NA_real_,
    boundary_overlap_weight_share_of_incident=if(incident_weight>0)boundary_weight/incident_weight else NA_real_,
    occupied_ball_conductance_weighted=conductance,
    internal_edge_share_of_incident=if(incident_edges>0)internal_edges/incident_edges else NA_real_,
    stringsAsFactors=FALSE)
}

stratified_draw <- function(strata, observed_label, eligible=NULL) {
  if(is.null(eligible)) eligible<-rep(TRUE,length(observed_label))
  if(length(strata)!=length(observed_label)||length(eligible)!=length(observed_label))
    fail("Stratified benchmark length mismatch.")
  strata<-as.character(strata)
  target<-table(strata[observed_label==1L],useNA="no")
  out<-integer(length(observed_label))
  for(s in names(target)) {
    need<-as.integer(target[[s]])
    pool<-which(strata==s & eligible)
    if(length(pool)<need) return(NULL)
    out[sample(pool,need,replace=FALSE)]<-1L
  }
  if(sum(out)!=sum(observed_label)) return(NULL)
  out
}

benchmark_run <- function(name,balls,edges,observed_label,reps,seed,strata=NULL,eligible=NULL) {
  set.seed(seed)
  out<-vector("list",reps)
  n<-length(observed_label); k<-sum(observed_label)
  for(i in seq_len(reps)) {
    if(is.null(strata)) {
      lab<-integer(n); pool<-if(is.null(eligible))seq_len(n) else which(eligible)
      if(length(pool)<k) return(NULL)
      lab[sample(pool,k,replace=FALSE)]<-1L
    } else {
      lab<-stratified_draw(strata,observed_label,eligible)
      if(is.null(lab)) return(NULL)
    }
    out[[i]]<-cbind(data.frame(benchmark=name,draw=i),label_diagnostics(balls,lab,edges))
  }
  do.call(rbind,out)
}

benchmark_summary <- function(bench,obs) {
  metrics<-intersect(names(obs),names(bench))
  metrics<-metrics[vapply(bench[metrics],is.numeric,logical(1))]
  metrics<-setdiff(metrics,c("n_labelled","balls_total","draw"))
  do.call(rbind,lapply(metrics,function(m) {
    x<-bench[[m]]; o<-obs[[m]]
    data.frame(benchmark=bench$benchmark[1L],metric=m,observed=o,
      benchmark_median=median(x,na.rm=TRUE),
      benchmark_q05=as.numeric(stats::quantile(x,.05,na.rm=TRUE,names=FALSE)),
      benchmark_q95=as.numeric(stats::quantile(x,.95,na.rm=TRUE,names=FALSE)),
      lower_tail_p=(1+sum(x<=o,na.rm=TRUE))/(1+sum(is.finite(x))),
      upper_tail_p=(1+sum(x>=o,na.rm=TRUE))/(1+sum(is.finite(x))),
      stringsAsFactors=FALSE)
  }))
}

sp<-script_path(); script_dir<-if(dir.exists(sp))sp else dirname(sp)
out_dir<-file.path(script_dir,"outputs")
dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)
reps<-num_env("OTWAY_2011_BENCHMARK_REPS",10000L)

balls_tab<-safe_read_csv(file.path(script_dir,"input","2011_canonical_balls.csv"))
members<-safe_read_csv(file.path(script_dir,"input","2011_canonical_memberships.csv"))
edges<-safe_read_csv(file.path(script_dir,"input","2011_canonical_edges.csv"))
labels<-safe_read_csv(file.path(script_dir,"input","2011_redwall_label_audit.csv"))
elec<-safe_read_csv(file.path(script_dir,"input","2021_electoral_history_proxy_audit.csv"))
obs_ref<-safe_read_csv(file.path(script_dir,"reference","2011_canonical_redwall_diagnostics.csv"))
bench21_rep<-safe_read_csv(file.path(script_dir,"reference","2021_benchmark_replicates.csv"))
bench21_sum<-safe_read_csv(file.path(script_dir,"reference","2021_benchmark_summary.csv"))

# Contract gates
same_order <- nrow(labels)==571L && nrow(elec)==571L &&
              identical(as.character(labels$ONSConstID),as.character(elec$ONSConstID))
redwall<-as.integer(labels$redwall)
if(!same_order) fail("2011 and 2021 benchmark-contract ONS row order differs.")
if(sum(redwall)!=41L) fail("Expected 41 Red Wall labels.")

nb <- max(members$ball)
balls <- lapply(seq_len(nb),function(b) as.integer(members$row[members$ball==b]))
if(length(balls)!=26L) fail("Expected 26 canonical 2011 balls.")
if(nrow(edges)!=94L) fail("Expected 94 canonical 2011 edges.")

observed<-label_diagnostics(balls,redwall,edges)
core_check <- c("n_labelled","balls_total","balls_with_label","max_label_share",
                "membership_entropy","membership_weighted_local_prevalence",
                "mean_nonlabel_share_among_unique_comembers",
                "occupied_ball_conductance_weighted")
obs_match <- all(vapply(core_check,function(nm)
  isTRUE(all.equal(as.numeric(observed[[nm]]),as.numeric(obs_ref[[nm]]),tolerance=1e-12)),
  logical(1)))
if(!obs_match) fail("Recomputed 2011 canonical diagnostics do not match frozen reference.")

reg <- as.character(elec$region)
lab_over <- as.integer(elec$LabourOverConservative17)
margin_q <- as.integer(elec$MarginQuartile)
if(any(is.na(reg))||any(is.na(lab_over))||any(is.na(margin_q)))
  fail("2021 electoral benchmark contract contains missing strata.")
if(mean(lab_over[redwall==1L])!=1) fail("Not all Red Wall seats satisfy Lab17>Con17 proxy.")
rq <- interaction(reg,as.character(margin_q),drop=TRUE,lex.order=TRUE)

gates<-data.frame(
  gate=c("same_571_ons_row_order","redwall_n_41","canonical_26_balls",
         "canonical_94_edges","canonical_observed_diagnostics_reproduced",
         "redwall_all_lab17_gt_con17","benchmark_reps_10000"),
  status=c(if(same_order)"PASS"else"FAIL",
           if(sum(redwall)==41L)"PASS"else"FAIL",
           if(length(balls)==26L)"PASS"else"FAIL",
           if(nrow(edges)==94L)"PASS"else"FAIL",
           if(obs_match)"PASS"else"FAIL",
           if(mean(lab_over[redwall==1L])==1)"PASS"else"FAIL",
           if(reps==10000L)"PASS"else"REVIEW"),
  detail=c("2011 labels and 2021 electoral contract have identical ONS row order.",
           paste0("n=",sum(redwall)),paste0("balls=",length(balls)),
           paste0("edges=",nrow(edges)),
           "Selected canonical diagnostics agree within 1e-12.",
           paste0("share=",mean(lab_over[redwall==1L])),
           paste0("reps=",reps)),
  stringsAsFactors=FALSE)
write_csv(gates,file.path(out_dir,"00_benchmark_contract_gates.csv"))

cat("Running 2011 unrestricted 41-seat benchmark...\n")
b1<-benchmark_run("unrestricted_41_seat",balls,edges,redwall,reps,202630001L)

cat("Running 2011 region-stratified benchmark...\n")
b2<-benchmark_run("region_stratified",balls,edges,redwall,reps,202631001L,strata=reg)

cat("Running 2011 region + Labour-over-Conservative 2017 benchmark...\n")
b3<-benchmark_run("region_plus_labour_over_conservative_2017",
                  balls,edges,redwall,reps,202633001L,
                  strata=reg,eligible=(lab_over==1L))

cat("Running 2011 region + 2017 Labour-Conservative margin-quartile benchmark...\n")
b4<-benchmark_run("region_plus_2017_labcon_margin_quartile",
                  balls,edges,redwall,reps,202634001L,strata=rq)

if(any(vapply(list(b1,b2,b3,b4),is.null,logical(1))))
  fail("At least one 2011 benchmark could not satisfy its sampling contract.")

bench11<-do.call(rbind,list(b1,b2,b3,b4))
write_csv(bench11,file.path(out_dir,"01_2011_benchmark_replicates.csv"))

sum11<-do.call(rbind,lapply(split(bench11,bench11$benchmark),benchmark_summary,obs=observed))
write_csv(sum11,file.path(out_dir,"02_2011_benchmark_summary.csv"))

# Verify that using the same seeds and same 571-seat order reproduces the exact
# 2021 random-label draw sequence indirectly. For topology-invariant fields,
# every draw must match the accepted 2021 replicate file.
keycols <- c("benchmark","draw")
m <- merge(bench11,bench21_rep,by=keycols,suffixes=c("_2011","_2021"),sort=FALSE)
expected_rows <- 4L*reps
if(nrow(m)!=expected_rows) fail("Could not pair all 2011/2021 benchmark draws.")

pair_gates <- data.frame(
  check=c("paired_rows","n_labelled_identical","overall_prevalence_identical"),
  status=c(if(nrow(m)==expected_rows)"PASS"else"FAIL",
           if(all(m$n_labelled_2011==m$n_labelled_2021))"PASS"else"FAIL",
           if(max(abs(m$overall_label_prevalence_2011-m$overall_label_prevalence_2021))<1e-15)"PASS"else"FAIL"),
  detail=c(paste0("paired rows=",nrow(m)),
           "Each paired draw has 41 labels.",
           "Paired draws have identical label prevalence."),
  stringsAsFactors=FALSE)
write_csv(pair_gates,file.path(out_dir,"03_paired_draw_contract.csv"))

# Direct benchmark-summary comparison.
metrics_focus <- c("balls_with_label","membership_entropy",
                   "membership_weighted_local_prevalence",
                   "mean_unique_comembers",
                   "mean_nonlabel_share_among_unique_comembers",
                   "boundary_edge_share_of_incident",
                   "boundary_overlap_weight_share_of_incident",
                   "occupied_ball_conductance_weighted",
                   "internal_edge_share_of_incident")
s11<-sum11[sum11$metric%in%metrics_focus,,drop=FALSE]
s21<-bench21_sum[bench21_sum$metric%in%metrics_focus,,drop=FALSE]
cmp<-merge(s11,s21,by=c("benchmark","metric"),suffixes=c("_2011","_2021"),all=TRUE,sort=FALSE)
write_csv(cmp,file.path(out_dir,"04_2011_2021_benchmark_summary_comparison.csv"))

# Paired draw differences isolate topology effects because labels are held fixed.
paired_metrics <- intersect(metrics_focus,
  intersect(sub("_2011$","",grep("_2011$",names(m),value=TRUE)),
            sub("_2021$","",grep("_2021$",names(m),value=TRUE))))
paired_out<-do.call(rbind,lapply(unique(m$benchmark),function(bn) {
  d<-m[m$benchmark==bn,,drop=FALSE]
  do.call(rbind,lapply(paired_metrics,function(metric) {
    delta<-d[[paste0(metric,"_2011")]]-d[[paste0(metric,"_2021")]]
    data.frame(
      benchmark=bn,metric=metric,
      paired_delta_median=median(delta,na.rm=TRUE),
      paired_delta_q05=as.numeric(quantile(delta,.05,na.rm=TRUE,names=FALSE)),
      paired_delta_q95=as.numeric(quantile(delta,.95,na.rm=TRUE,names=FALSE)),
      mean_delta=mean(delta,na.rm=TRUE),
      share_delta_positive=mean(delta>0,na.rm=TRUE),
      stringsAsFactors=FALSE)
  }))
}))
write_csv(paired_out,file.path(out_dir,"05_paired_topology_effects.csv"))

# Compact manuscript-facing concentration table.
concentration<-cmp[cmp$metric=="balls_with_label",
 c("benchmark","observed_2011","benchmark_median_2011","benchmark_q05_2011",
   "benchmark_q95_2011","lower_tail_p_2011",
   "observed_2021","benchmark_median_2021","benchmark_q05_2021",
   "benchmark_q95_2021","lower_tail_p_2021"),drop=FALSE]
write_csv(concentration,file.path(out_dir,"06_concentration_benchmark_comparison.csv"))

lines<-c(
  "# 2011 matched concentration benchmark summary",
  "",
  paste0("Generated: ",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
  "",
  "All four 2011 benchmarks use the exact same 571-seat row order, sampling contracts,",
  "seeds and sequential randomisation algorithm as the accepted 2021 benchmark run.",
  "Only the canonical socio-demographic topology differs.",
  "",
  "## Occupied-ball concentration",
  unlist(lapply(seq_len(nrow(concentration)),function(i) {
    x<-concentration[i,]
    paste0("- `",x$benchmark,"`: 2011 observed=",x$observed_2011,
           ", median=",x$benchmark_median_2011,
           ", q05-q95=",x$benchmark_q05_2011,"-",x$benchmark_q95_2011,
           ", lower-tail p=",format(x$lower_tail_p_2011,digits=5),
           "; 2021 observed=",x$observed_2021,
           ", median=",x$benchmark_median_2021,
           ", q05-q95=",x$benchmark_q05_2021,"-",x$benchmark_q95_2021,
           ", lower-tail p=",format(x$lower_tail_p_2021,digits=5),".")
  })),
  "",
  "## Governance",
  "No radius is selected or changed here.",
  "No manuscript conclusion is automatically accepted by this script.",
  "Interpret the 2011 and 2021 benchmark profiles jointly after handback review."
)
writeLines(lines,file.path(out_dir,"MATCHED_BENCHMARK_SUMMARY.md"))

writeLines(c(
  "status=PASS",
  paste0("generated=",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
  "stage=2011_MATCHED_CONCENTRATION_BENCHMARKS",
  paste0("benchmark_reps_each=",reps),
  "benchmarks=4",
  "canonical_2011_radius=5.75",
  "canonical_2021_radius=5.50"
),file.path(out_dir,"RUN_STATUS.txt"))

cat("\nPASS: matched 2011 concentration benchmarks completed.\n")
