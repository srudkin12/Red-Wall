#!/usr/bin/env Rscript
options(warn=2);options(stringsAsFactors=FALSE)

parse_args<-function(x){
  out<-list()
  for(a in x){z<-strsplit(sub("^--","",a),"=",fixed=TRUE)[[1L]];out[[z[[1L]]]]<-paste(z[-1L],collapse="=")}
  out
}
a<-parse_args(commandArgs(trailingOnly=TRUE))
repo<-normalizePath(a[["repo"]],winslash="/",mustWork=TRUE)
app_dir<-normalizePath(a[["app-dir"]],winslash="/",mustWork=TRUE)
audit<-normalizePath(a[["audit-dir"]],winslash="/",mustWork=FALSE)
mode<-a[["mode"]]
dir.create(audit,recursive=TRUE,showWarnings=FALSE)

source(file.path(repo,"framework/R/TDABMRadiusDiagnostics.R"))
source(file.path(app_dir,"R/OtwayRudkin2024Adapter.R"))
source(file.path(app_dir,"R/OtwayRudkinExactStability.R"))
or24_compile_exact_stability(file.path(app_dir,"cpp/OtwayRudkinPackedJaccard.cpp"))

config<-or24_read_contract(file.path(app_dir,"config/application_contract.csv"))
app_id<-or24_get(config,"application_id")
topology_vars<-or24_split(or24_get(config,"topology_variables"))
frozen<-file.path(app_dir,"frozen_input","p1_4_a_v1_0",paste0(app_id,"_input_frozen_P1_4_A_v1_0.csv"))
if(!file.exists(frozen)) stop("Accepted P1.4-A frozen input is missing. Run P1.4-A first.",call.=FALSE)
input_sha_before<-or24_sha256(frozen)

data<-utils::read.csv(frozen,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8")
validation<-tdabm_rd_validate_topology_frame(
  data,"ONSConstID",topology_vars,
  scaling="z_score",metric="euclidean",expected_rows=571L,stop_on_failure=TRUE
)
prepared<-tdabm_rd_prepare_topology(data,"ONSConstID",topology_vars,scaling="z_score",metric="euclidean")

if(identical(mode,"preflight")){
  grid_points<-as.integer(Sys.getenv("TDABM_P1_4_B_GRID_POINTS","11"))
  reps<-as.integer(Sys.getenv("TDABM_P1_4_B_REPETITIONS","4"))
  workers<-as.integer(Sys.getenv("TDABM_P1_4_B_WORKERS","2"))
  radius_min<-NA_real_
  radius_max<-NA_real_
  radius_step<-NA_real_
  grid_design<-"GEOMETRY_DERIVED_PREFLIGHT"
} else {
  radius_min<-0.75
  radius_max<-20.00
  radius_step<-0.05
  radii<-round(seq(radius_min,radius_max,by=radius_step),2L)
  if(length(radii)!=386L || radii[[1L]]!=radius_min || tail(radii,1L)!=radius_max){
    stop("Fixed production radius grid failed its internal contract.",call.=FALSE)
  }
  grid_points<-length(radii)
  reps<-as.integer(Sys.getenv("TDABM_P1_4_B_REPETITIONS","1000"))
  workers<-as.integer(Sys.getenv("TDABM_P1_4_B_WORKERS","50"))
  grid_design<-"USER_SPECIFIED_LINEAR_INTERVAL"
}
if(grid_points<7L || reps<2L || workers<1L) stop("Invalid P1.4-B execution controls.",call.=FALSE)

if(identical(mode,"preflight")){
  grid<-tdabm_rd_make_radius_grid(
    prepared$axes,grid_points=grid_points,
    lower_multiplier=0.75,upper_multiplier=1.000000001
  )
} else {
  grid<-data.frame(
    radius_index=seq_along(radii),radius=as.numeric(radii),
    grid_role=rep("user_fixed_interval",length(radii)),
    stringsAsFactors=FALSE
  )
}
module_path<-file.path(repo,"framework/R/TDABMRadiusDiagnostics.R")
cpp_path<-file.path(repo,"framework/BallMapper.cpp")
run<-tdabm_rd_run_replicates(
  prepared$axes,prepared$ids,grid,
  repetitions=reps,base_seed=20260810L,ncores=workers,
  module_path=module_path,cpp_path=cpp_path
)
stability_validation<-or24_validate_exact_stability(
  run$replicates,grid,nrow(prepared$axes),nthreads=workers,tolerance=1e-14
)
or24_write_csv(stability_validation,file.path(audit,"P1_4_B_exact_stability_validation.csv"))
stability<-or24_pairwise_stability_exact_fast(
  run$replicates,grid,nrow(prepared$axes),
  nthreads=workers,
  progress_path=file.path(audit,"P1_4_B_stability_progress.txt")
)
summary<-tdabm_rd_summarise(run$diagnostics,stability)
resolution<-tdabm_rd_resolution_transition_report(
  summary,n_observations=nrow(prepared$axes),n_dimensions=ncol(prepared$axes),
  dominance_targets=c(0.40,0.50,0.60,0.75)
)
review_rows<-tdabm_rd_resolution_review_rows(
  resolution$summary,resolution$reference$reference_radius[[1L]],points_each_side=2L
)

or24_write_csv(grid,file.path(audit,"radius_grid.csv"))
or24_write_csv(run$diagnostics,file.path(audit,"radius_diagnostics_replicates.csv"))
or24_write_csv(stability,file.path(audit,"radius_order_stability.csv"))
or24_write_csv(resolution$summary,file.path(audit,"radius_summary.csv"))
or24_write_csv(resolution$transitions,file.path(audit,"radius_topology_transitions.csv"))
or24_write_csv(resolution$dominance_crossings,file.path(audit,"radius_dominance_crossings.csv"))
or24_write_csv(resolution$reference,file.path(audit,"radius_pipeline_reference.csv"))
or24_write_csv(review_rows,file.path(audit,"radius_review_rows.csv"))
tdabm_rd_write_diagnostic_plots(resolution$summary,file.path(audit,"plots"))

# Bounded deterministic worker check for production/preflight.
check_grid<-grid[unique(round(seq(1,nrow(grid),length.out=min(5L,nrow(grid))))),,drop=FALSE]
wreps<-min(4L,reps)
one<-tdabm_rd_run_replicates(
  prepared$axes,prepared$ids,check_grid,
  repetitions=wreps,base_seed=902024L,ncores=1L,module_path=module_path,cpp_path=cpp_path
)
two<-tdabm_rd_run_replicates(
  prepared$axes,prepared$ids,check_grid,
  repetitions=wreps,base_seed=902024L,ncores=min(2L,workers),module_path=module_path,cpp_path=cpp_path
)
worker_check<-tdabm_rd_compare_runs(one$diagnostics,two$diagnostics,tolerance=0)
if(!all(worker_check$pass)) stop("P1.4-B worker invariance failed.",call.=FALSE)
or24_write_csv(worker_check,file.path(audit,"P1_4_B_worker_invariance.csv"))

input_sha_after<-or24_sha256(frozen)
if(!identical(input_sha_before,input_sha_after)) stop("Frozen P1.4-A input changed during P1.4-B.",call.=FALSE)

execution<-data.frame(
  field=c(
    "mode","grid_design","radius_min","radius_max","radius_step","grid_points",
    "repetitions","workers","stability_engine","stability_threads","base_seed",
    "n_observations","n_dimensions","scaling","metric",
    "automatic_approval","optimality_claim"
  ),
  value=c(
    mode,grid_design,
    if(identical(mode,"production"))format(radius_min,nsmall=2L) else "NA",
    if(identical(mode,"production"))format(radius_max,nsmall=2L) else "NA",
    if(identical(mode,"production"))format(radius_step,nsmall=2L) else "NA",
    grid_points,reps,workers,"EXACT_PACKED_CPP_OPENMP",workers,20260810,
    nrow(prepared$axes),ncol(prepared$axes),"z_score","euclidean",
    "FALSE","NONE"
  ),
  stringsAsFactors=FALSE
)
or24_write_csv(execution,file.path(audit,"P1_4_B_execution_contract.csv"))

# Historical information is provenance only and never enters selection.
historical<-data.frame(
  field=c("historical_main_plot_radius","historical_sensitivity_range","historical_sensitivity_repetitions","role"),
  value=c("UNRESOLVED","3 to 8 by 0.10","10000","HISTORICAL_REFERENCE_ONLY"),
  stringsAsFactors=FALSE
)
or24_write_csv(historical,file.path(audit,"historical_radius_context.csv"))

ref<-resolution$reference
acceptance<-c(
  paste0(app_id," P1.4-B ",toupper(mode),": PASS"),
  paste0("pipeline_reference_radius=",format(ref$reference_radius[[1L]],digits=16)),
  paste0("review_band_lower=",format(ref$review_band_lower[[1L]],digits=16)),
  paste0("review_band_upper=",format(ref$review_band_upper[[1L]],digits=16)),
  "selection_status=NON_BINDING_RESOLUTION_REFERENCE",
  "automatic_approval=FALSE",
  "optimality_claim=NONE",
  "connectedness_is_descriptive_only=TRUE",
  "user_radius_decision=PENDING",
  paste0("frozen_input_sha256=",input_sha_after),
  if(identical(mode,"production"))"READY_FOR_USER_RADIUS_DECISION=YES" else "READY_FOR_PRODUCTION_P1_4_B=YES"
)
writeLines(acceptance,file.path(audit,"P1_4_B_ACCEPTANCE.txt"),useBytes=TRUE)
cat(paste(acceptance,collapse="\n"),"\n")
