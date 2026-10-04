options(stringsAsFactors=FALSE)
options(width=220)
cat("OTWAY_BJPOLS_2011_ROUNDED_RADIUS_CANONICAL_RED_WALL_V1_1\n")

fail <- function(...) stop(paste0(...),call.=FALSE)
safe_read_csv <- function(p) utils::read.csv(p,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8")
write_csv <- function(x,p) {dir.create(dirname(p),recursive=TRUE,showWarnings=FALSE);utils::write.csv(x,p,row.names=FALSE,na="")}
hash_file <- function(path) {
  if(nzchar(Sys.which("sha256sum"))) {
    x<-system2("sha256sum",shQuote(path),stdout=TRUE)
    return(strsplit(x[[1]],"\\s+")[[1]][1])
  }
  unname(tools::md5sum(path))
}
script_path <- function() {
  x<-commandArgs(trailingOnly=FALSE)
  f<-sub("^--file=","",x[grep("^--file=",x)])
  if(!length(f)) return(normalizePath(".",winslash="/",mustWork=TRUE))
  normalizePath(f[[1]],winslash="/",mustWork=TRUE)
}
find_repo_root <- function(start_dir) {
  p<-normalizePath(start_dir,winslash="/",mustWork=TRUE)
  for(i in 0:12) {
    fw<-file.path(p,"framework","R","TDABMRadiusDiagnostics.R")
    if(file.exists(fw)) return(p)
    q<-dirname(p); if(identical(q,p)) break; p<-q
  }
  fail("Could not discover TDABM_Portability root.")
}
num_env <- function(name,default,minimum=1L) {
  z<-Sys.getenv(name,""); if(!nzchar(z)) z<-as.character(default)
  v<-suppressWarnings(as.integer(z))
  if(is.na(v)||v<minimum) fail("Invalid ",name,"=",z)
  v
}

build_edges <- function(balls,n) {
  if(length(balls)<2L) return(data.frame(from=integer(),to=integer(),strength=integer()))
  pb<-vector("list",n)
  for(b in seq_along(balls)) for(i in balls[[b]]) pb[[i]]<-c(pb[[i]],b)
  keys<-character()
  for(x in pb) {
    x<-unique(x)
    if(length(x)>=2L) {
      cm<-utils::combn(sort(x),2L)
      keys<-c(keys,paste(cm[1,],cm[2,],sep=":"))
    }
  }
  if(!length(keys)) return(data.frame(from=integer(),to=integer(),strength=integer()))
  tt<-table(keys); sp<-strsplit(names(tt),":",fixed=TRUE)
  data.frame(from=as.integer(vapply(sp,`[[`,character(1),1L)),
             to=as.integer(vapply(sp,`[[`,character(1),2L)),
             strength=as.integer(tt),stringsAsFactors=FALSE)
}
connected_components <- function(nballs,edges) {
  if(!nballs) return(integer())
  adj<-vector("list",nballs)
  if(nrow(edges)) for(i in seq_len(nrow(edges))) {
    a<-edges$from[i]; b<-edges$to[i]
    adj[[a]]<-c(adj[[a]],b); adj[[b]]<-c(adj[[b]],a)
  }
  comp<-integer(nballs); cid<-0L
  for(s in seq_len(nballs)) {
    if(comp[s]!=0L) next
    cid<-cid+1L; q<-s; comp[s]<-cid
    while(length(q)) {
      v<-q[[1]]; q<-q[-1]
      nb<-unique(adj[[v]])
      fresh<-nb[comp[nb]==0L]
      if(length(fresh)) {comp[fresh]<-cid;q<-c(q,fresh)}
    }
  }
  comp
}
build_cover <- function(D,radius,order_idx) {
  n<-nrow(D)
  if(length(order_idx)!=n||!setequal(order_idx,seq_len(n))) fail("Invalid landmark order.")
  covered<-rep(FALSE,n);balls<-list();landmarks<-integer();eps0<-1e-12
  for(idx in order_idx) {
    if(covered[idx]) next
    members<-which(D[idx,] <= radius+eps0)
    balls[[length(balls)+1L]]<-members
    landmarks<-c(landmarks,idx)
    covered[members]<-TRUE
    if(all(covered)) break
  }
  if(!all(covered)) fail("Cover failure.")
  edges<-build_edges(balls,n)
  cc<-connected_components(length(balls),edges)
  memberships<-integer(n);for(b in balls)memberships[b]<-memberships[b]+1L
  list(balls=balls,landmarks=landmarks,edges=edges,components=cc,
       summary=data.frame(balls=length(balls),edges=nrow(edges),
         components=if(length(cc))max(cc)else 0L,
         isolates=if(length(cc))sum(tabulate(cc,nbins=max(cc))==1L)else 0L,
         multicoverage=mean(memberships>=2L),stringsAsFactors=FALSE))
}

label_diagnostics <- function(balls,label,edges) {
  n<-length(label);nb<-length(balls)
  ball<-do.call(rbind,lapply(seq_along(balls),function(i) {
    idx<-balls[[i]];nl<-sum(label[idx]==1L)
    data.frame(ball_id=i,ball_size=length(idx),labelled=nl,
      unlabelled=length(idx)-nl,labelled_share=nl/length(idx))
  }))
  occupied<-which(ball$labelled>0L)
  overall<-mean(label==1L)
  memberships_label<-sum(ball$labelled[occupied])
  memberships_all<-sum(ball$ball_size[occupied])
  p<-ball$labelled[occupied];p<-p/sum(p)
  ent<-if(length(p)<=1L)0 else -sum(p*log(p))/log(length(p))

  point_balls<-vector("list",n)
  for(b in seq_along(balls)) for(idx in balls[[b]]) point_balls[[idx]]<-c(point_balls[[idx]],b)
  labelled_idx<-which(label==1L)
  co_nonlabel<-rep(NA_real_,length(labelled_idx));co_n<-integer(length(labelled_idx))
  for(ii in seq_along(labelled_idx)) {
    i<-labelled_idx[ii];bs<-point_balls[[i]]
    neigh<-unique(unlist(balls[bs],use.names=FALSE));neigh<-setdiff(neigh,i)
    co_n[ii]<-length(neigh)
    if(length(neigh))co_nonlabel[ii]<-mean(label[neigh]==0L)
  }

  boundary_edges<-incident_edges<-internal_edges<-0L
  boundary_weight<-incident_weight<-internal_weight<-0
  conductance<-NA_real_
  if(nrow(edges)) {
    inS<-seq_len(nb)%in%occupied;a<-edges$from;b<-edges$to;w<-edges$strength
    inc<-inS[a]|inS[b];cross<-xor(inS[a],inS[b]);internal<-inS[a]&inS[b]
    incident_edges<-sum(inc);boundary_edges<-sum(cross);internal_edges<-sum(internal)
    incident_weight<-sum(w[inc]);boundary_weight<-sum(w[cross]);internal_weight<-sum(w[internal])
    deg<-numeric(nb)
    for(i in seq_len(nrow(edges))) {deg[a[i]]<-deg[a[i]]+w[i];deg[b[i]]<-deg[b[i]]+w[i]}
    den<-min(sum(deg[inS]),sum(deg[!inS]))
    if(den>0)conductance<-boundary_weight/den
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
ball_table <- function(balls,label) {
  do.call(rbind,lapply(seq_along(balls),function(i) {
    idx<-balls[[i]];nl<-sum(label[idx]==1L)
    data.frame(ball_id=i,ball_size=length(idx),labelled=nl,
      unlabelled=length(idx)-nl,labelled_share=nl/length(idx),stringsAsFactors=FALSE)
  }))
}
edge_table <- function(edges,balls,label) {
  bt<-ball_table(balls,label)
  if(!nrow(edges))return(data.frame())
  data.frame(from=edges$from,to=edges$to,strength=edges$strength,
    from_labelled=bt$labelled[edges$from],to_labelled=bt$labelled[edges$to],
    from_share=bt$labelled_share[edges$from],to_share=bt$labelled_share[edges$to],
    crosses_occupied_boundary=xor(bt$labelled[edges$from]>0,bt$labelled[edges$to]>0),
    both_occupied=(bt$labelled[edges$from]>0&bt$labelled[edges$to]>0),
    stringsAsFactors=FALSE)
}
run_reps <- function(D,radius,reps,seed0,workers,label) {
  f<-function(i) {
    set.seed(seed0+i-1L);tp<-build_cover(D,radius,sample.int(nrow(D)))
    cbind(data.frame(replicate=i,seed=seed0+i-1L,radius=radius,
      balls=tp$summary$balls,edges=tp$summary$edges,
      components=tp$summary$components,isolates=tp$summary$isolates,
      multicoverage=tp$summary$multicoverage,stringsAsFactors=FALSE),
      label_diagnostics(tp$balls,label,tp$edges))
  }
  zz<-if(.Platform$OS.type=="unix"&&workers>1L)
    parallel::mclapply(seq_len(reps),f,mc.cores=workers,mc.preschedule=TRUE)
  else lapply(seq_len(reps),f)
  do.call(rbind,zz)
}
summary_reps <- function(d) {
  metrics<-names(d)[vapply(d,is.numeric,logical(1))]
  metrics<-setdiff(metrics,c("replicate","seed","radius"))
  o<-data.frame(radius=d$radius[1],reps=nrow(d),stringsAsFactors=FALSE)
  for(m in metrics) {
    x<-d[[m]]
    o[[paste0(m,"_median")]]<-median(x,na.rm=TRUE)
    o[[paste0(m,"_q05")]]<-as.numeric(quantile(x,.05,na.rm=TRUE,names=FALSE))
    o[[paste0(m,"_q95")]]<-as.numeric(quantile(x,.95,na.rm=TRUE,names=FALSE))
    o[[paste0(m,"_min")]]<-min(x,na.rm=TRUE)
    o[[paste0(m,"_max")]]<-max(x,na.rm=TRUE)
  }
  o$majority_ball_frequency<-mean(d$majority_label_ball)
  o$exact_half_ball_frequency<-mean(d$exact_half_label_ball)
  o$exclusive_ball_frequency<-mean(d$exclusive_label_ball)
  o$exact_2comp_1isolate_frequency<-mean(d$components==2L & d$isolates==1L)
  o
}

sp<-script_path();script_dir<-if(dir.exists(sp))sp else dirname(sp)
repo<-find_repo_root(script_dir);out_dir<-file.path(script_dir,"outputs")
dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)
workers<-num_env("OTWAY_2011_RW_WORKERS",min(50L,max(1L,parallel::detectCores()-2L)))
reps<-num_env("OTWAY_2011_RW_REPS",1000L)

frozen<-file.path(script_dir,"input","Otway_Rudkin_2011_Harmonised31D_input_frozen_S2011_A_v1_0.csv")
sha_file<-paste0(frozen,".sha256")
vars_file<-file.path(script_dir,"input","topology_variables.csv")
rw_file<-Sys.getenv("OTWAY_RED_WALL_AUDIT",unset=file.path(repo,"Otway_BJPolS_2021_RoundedRadius_Robustness_v1_1","outputs","03_redwall_source_match_audit.csv"))
for(p in c(frozen,sha_file,vars_file,rw_file))if(!file.exists(p))fail("Missing input: ",p)

expected<-strsplit(readLines(sha_file,warn=FALSE)[1],"\\s+")[[1]][1]
observed<-hash_file(frozen)
if(tolower(expected)!=tolower(observed))fail("Frozen input checksum mismatch.")

dat<-safe_read_csv(frozen);vars<-safe_read_csv(vars_file)$topology_variable
if(nrow(dat)!=571L||length(vars)!=31L||anyDuplicated(dat$ONSConstID))fail("Frozen input contract failure.")
if(any(!is.finite(as.matrix(dat[,vars,drop=FALSE]))))fail("Non-finite topology values.")
bad<-grep("RedWall|Lab17|Con17|vote|swing|turnout",names(dat),ignore.case=TRUE,value=TRUE)
if(length(bad))fail("Electoral fields found in frozen topology input: ",paste(bad,collapse=", "))

rwsrc<-safe_read_csv(rw_file)
if(nrow(rwsrc)!=42L)fail("Red Wall source audit must contain 42 rows.")

# The accepted source audit serialises its Boolean flag as title-case
# True/False.  R may therefore retain the column as character rather than
# logical.  Normalise explicitly instead of relying on type inference.
matched_raw <- trimws(tolower(as.character(rwsrc$matched)))
matched_flag <- matched_raw %in% c("true","t","1","yes","y")

write_csv(data.frame(
  source_order=rwsrc$source_order,
  source_constituency=rwsrc$source_constituency,
  raw_matched_value=as.character(rwsrc$matched),
  normalised_matched_value=matched_raw,
  parsed_matched=matched_flag,
  matched_ons_id=as.character(rwsrc$matched_ons_id),
  stringsAsFactors=FALSE),
  file.path(out_dir,"00a_redwall_source_flag_parse_audit.csv"))

matched_ids<-trimws(as.character(rwsrc$matched_ons_id[matched_flag]))
matched_ids<-matched_ids[!is.na(matched_ids)&nzchar(matched_ids)]
missing_src<-as.character(rwsrc$source_constituency[!matched_flag])

source_contract <- data.frame(
  check=c("source_rows_42",
          "matched_rows_41",
          "matched_unique_ons_ids_41",
          "unmatched_rows_1",
          "sole_unmatched_is_chorley",
          "all_matched_ids_in_2011_frame"),
  status=c(
    if(nrow(rwsrc)==42L)"PASS"else"FAIL",
    if(sum(matched_flag)==41L)"PASS"else"FAIL",
    if(length(unique(matched_ids))==41L)"PASS"else"FAIL",
    if(sum(!matched_flag)==1L)"PASS"else"FAIL",
    if(length(missing_src)==1L && identical(missing_src,"Chorley"))"PASS"else"FAIL",
    if(length(matched_ids)==41L && all(matched_ids%in%dat$ONSConstID))"PASS"else"FAIL"),
  detail=c(
    paste0("rows=",nrow(rwsrc)),
    paste0("matched rows=",sum(matched_flag)),
    paste0("unique matched ONS IDs=",length(unique(matched_ids))),
    paste0("unmatched rows=",sum(!matched_flag)),
    paste0("unmatched source=",paste(missing_src,collapse=";")),
    paste0("matched IDs present in frozen 2011 frame=",
           sum(unique(matched_ids)%in%dat$ONSConstID),"/",
           length(unique(matched_ids)))),
  stringsAsFactors=FALSE)
write_csv(source_contract,file.path(out_dir,"00b_redwall_source_contract.csv"))

if(any(source_contract$status!="PASS"))
  fail("Red Wall source contract failed. See 00a/00b source audit files.")

redwall<-as.integer(dat$ONSConstID%in%matched_ids)
if(sum(redwall)!=41L)fail("2011 Red Wall label count is not 41 after validated source parsing.")

source(file.path(repo,"framework","R","TDABMRadiusDiagnostics.R"))
prepared<-tdabm_rd_prepare_topology(dat,"ONSConstID",vars,scaling="z_score",metric="euclidean")
D<-as.matrix(stats::dist(prepared$axes))
approved_radius<-5.75

canonical<-build_cover(D,approved_radius,seq_len(nrow(D)))

# Validate the deterministic frozen-row-order canonical topology against the
# corrected 386-radius production handback at the approved rounded radius.
expected_balls <- 26L
expected_edges <- 94L
expected_components <- 2L
expected_isolates <- 1L

balls_out<-do.call(rbind,lapply(seq_along(canonical$balls),function(b)
  data.frame(ball=b,landmark_row=canonical$landmarks[b],
    landmark_ONSConstID=dat$ONSConstID[canonical$landmarks[b]],
    ball_size=length(canonical$balls[[b]]),stringsAsFactors=FALSE)))
members_out<-do.call(rbind,lapply(seq_along(canonical$balls),function(b)
  data.frame(ball=b,row=canonical$balls[[b]],
    ONSConstID=dat$ONSConstID[canonical$balls[[b]]],stringsAsFactors=FALSE)))
edges_out<-canonical$edges
comps_out<-data.frame(ball=seq_along(canonical$components),
                      component=canonical$components,stringsAsFactors=FALSE)

observed_isolates <- sum(tabulate(canonical$components,
                                  nbins=max(canonical$components))==1L)
summary_match <- canonical$summary$balls==expected_balls &&
                 canonical$summary$edges==expected_edges &&
                 canonical$summary$components==expected_components &&
                 observed_isolates==expected_isolates
if(!summary_match)
  fail("Canonical topology does not reproduce the corrected rounded-grid frozen-order summary at epsilon 5.75.")

write_csv(data.frame(
  gate=c("approved_radius_on_common_grid","frozen_input_sha256","redwall_source_contract",
         "canonical_frozen_order_summary_reproduced"),
  status=c(
    if(abs(approved_radius*20-round(approved_radius*20))<1e-12)"PASS"else"FAIL",
    "PASS","PASS",if(summary_match)"PASS"else"FAIL"),
  detail=c(
    "epsilon=5.75; common grid 0.75-20.00 by 0.05",
    paste0("SHA256=",observed),
    "42-seat source audit; 41 matched ONS IDs; Chorley sole missing source seat.",
    paste0("balls=",canonical$summary$balls,
           "; edges=",canonical$summary$edges,
           "; components=",canonical$summary$components,
           "; isolates=",observed_isolates)
  ),stringsAsFactors=FALSE
),file.path(out_dir,"00_canonical_freeze_gates.csv"))

# Freeze canonical topology.
write_csv(balls_out,file.path(out_dir,"01_canonical_balls.csv"))
write_csv(members_out,file.path(out_dir,"02_canonical_memberships.csv"))
write_csv(edges_out,file.path(out_dir,"03_canonical_edges.csv"))
write_csv(comps_out,file.path(out_dir,"04_canonical_components.csv"))
write_csv(data.frame(ONSConstID=dat$ONSConstID,
  ConstituencyName_2021_frame=dat$ConstituencyName_2021_frame,
  redwall=redwall,stringsAsFactors=FALSE),
  file.path(out_dir,"05_redwall_label_audit.csv"))

canon_diag<-label_diagnostics(canonical$balls,redwall,canonical$edges)
canon_diag$radius<-approved_radius
canon_diag$components<-canonical$summary$components
canon_diag$isolates<-canonical$summary$isolates
canon_diag$edges<-canonical$summary$edges
canon_diag$multicoverage<-canonical$summary$multicoverage
write_csv(canon_diag,file.path(out_dir,"06_canonical_redwall_diagnostics.csv"))
write_csv(ball_table(canonical$balls,redwall),file.path(out_dir,"07_canonical_redwall_ball_diagnostics.csv"))
write_csv(edge_table(canonical$edges,canonical$balls,redwall),file.path(out_dir,"08_canonical_redwall_edge_diagnostics.csv"))

cat("Running ",reps," labelled landmark-order repetitions at approved radius...\\n",sep="")
rep_tab<-run_reps(D,approved_radius,reps,410000000L,workers,redwall)
write_csv(rep_tab,file.path(out_dir,"09_redwall_landmark_order_replicates.csv"))
rep_sum<-summary_reps(rep_tab)
write_csv(rep_sum,file.path(out_dir,"10_redwall_landmark_order_summary.csv"))

# Invariant-level comparison with accepted 2021 canonical diagnostics.
d21<-safe_read_csv(file.path(script_dir,"reference","2021_canonical_extended_diagnostics.csv"))
common<-intersect(names(canon_diag),names(d21))
keep<-intersect(common,c(
  "n_labelled","balls_total","balls_with_label","overall_label_prevalence",
  "max_label_share","membership_entropy","membership_weighted_local_prevalence",
  "mean_unique_comembers","mean_nonlabel_share_among_unique_comembers",
  "min_nonlabel_share_among_unique_comembers","boundary_edge_share_of_incident",
  "boundary_overlap_weight_share_of_incident","occupied_ball_conductance_weighted",
  "internal_edge_share_of_incident"))
cmp<-rbind(
  cbind(year="2011",canon_diag[,keep,drop=FALSE]),
  cbind(year="2021",d21[,keep,drop=FALSE]))
write_csv(cmp,file.path(out_dir,"11_2011_2021_canonical_diagnostic_comparison.csv"))

inv<-data.frame(
  invariant=c("canonical_no_majority_redwall_ball",
              "canonical_no_exact_half_redwall_ball",
              "canonical_no_exclusive_redwall_ball",
              "canonical_redwall_mixed_with_nonredwall",
              "order_robust_no_majority_frequency",
              "order_robust_no_exclusive_frequency",
              "order_robust_macro_topology_frequency"),
  value=c(
    as.character(!canon_diag$majority_label_ball),
    as.character(!canon_diag$exact_half_label_ball),
    as.character(!canon_diag$exclusive_label_ball),
    as.character(canon_diag$mean_nonlabel_share_among_unique_comembers>0.5),
    sprintf("%.6f",1-rep_sum$majority_ball_frequency),
    sprintf("%.6f",1-rep_sum$exclusive_ball_frequency),
    sprintf("%.6f",rep_sum$exact_2comp_1isolate_frequency)),
  stringsAsFactors=FALSE)
write_csv(inv,file.path(out_dir,"12_historical_sensitivity_invariants.csv"))

summary_lines<-c(
  "# 2011 Red Wall historical sensitivity",
  "",
  paste0("Generated: ",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Canonical 2011 topology",
  paste0("- Approved radius: **",format(approved_radius,digits=15),"**."),
  paste0("- Balls: **",canonical$summary$balls,"**."),
  paste0("- Edges: **",canonical$summary$edges,"**."),
  paste0("- Components: **",canonical$summary$components,"**; isolates: **",canonical$summary$isolates,"**."),
  "",
  "## Canonical Red Wall diagnostics",
  paste0("- Matched Red Wall constituencies: **",canon_diag$n_labelled,"**."),
  paste0("- Occupied balls: **",canon_diag$balls_with_label,"/",canon_diag$balls_total,"**."),
  paste0("- Maximum within-ball Red Wall share: **",sprintf("%.4f",canon_diag$max_label_share),"**."),
  paste0("- Majority Red Wall ball: **",canon_diag$majority_label_ball,"**."),
  paste0("- Exclusive Red Wall ball: **",canon_diag$exclusive_label_ball,"**."),
  paste0("- Membership-weighted local Red Wall prevalence: **",sprintf("%.4f",canon_diag$membership_weighted_local_prevalence),"**."),
  paste0("- Mean non-Red-Wall share among unique Red Wall co-members: **",sprintf("%.4f",canon_diag$mean_nonlabel_share_among_unique_comembers),"**."),
  "",
  "## Landmark-order robustness",
  paste0("- Repetitions: **",reps,"**."),
  paste0("- Runs with no majority Red Wall ball: **",sprintf("%.4f",1-rep_sum$majority_ball_frequency),"**."),
  paste0("- Runs with no exclusive Red Wall ball: **",sprintf("%.4f",1-rep_sum$exclusive_ball_frequency),"**."),
  paste0("- Runs with 2 components and 1 isolate: **",sprintf("%.4f",rep_sum$exact_2comp_1isolate_frequency),"**."),
  "",
  "## Interpretation governance",
  "No random-label benchmark or manuscript claim is hard-coded by this package.",
  "The handback should be reviewed against the 2021 canonical diagnostics before wording the historical sensitivity result."
)
writeLines(summary_lines,file.path(out_dir,"HISTORICAL_SENSITIVITY_SUMMARY.md"))

# Topology artifact manifest and canonical hashes.
files0<-c("01_canonical_balls.csv","02_canonical_memberships.csv","03_canonical_edges.csv",
          "04_canonical_components.csv","06_canonical_redwall_diagnostics.csv")
write_csv(data.frame(file=files0,
  sha256=vapply(file.path(out_dir,files0),hash_file,character(1)),
  stringsAsFactors=FALSE),
  file.path(out_dir,"13_canonical_artifact_hashes.csv"))

files<-list.files(out_dir,recursive=TRUE,full.names=TRUE)
files<-files[file.info(files)$isdir%in%FALSE]
write_csv(data.frame(
  relative_path=sub(paste0("^",normalizePath(out_dir,winslash="/"),"/?"),"",
                    normalizePath(files,winslash="/")),
  bytes=file.info(files)$size,
  sha256=vapply(files,hash_file,character(1)),
  stringsAsFactors=FALSE),
  file.path(out_dir,"14_artifact_manifest.csv"))

writeLines(c(
  "status=PASS",
  paste0("generated=",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
  "stage=S2011_D_CANONICAL_TOPOLOGY_AND_RED_WALL_SENSITIVITY",
  paste0("approved_radius=",format(approved_radius,digits=15)),
  paste0("redwall_n=",sum(redwall)),
  paste0("canonical_balls=",canonical$summary$balls),
  paste0("canonical_edges=",canonical$summary$edges),
  paste0("canonical_components=",canonical$summary$components),
  paste0("canonical_isolates=",canonical$summary$isolates)
),file.path(out_dir,"RUN_STATUS.txt"))

cat("\\nPASS: canonical 2011 topology frozen and Red Wall historical sensitivity completed.\\n")
cat("Upload the handback for interpretation before manuscript integration.\\n")
