options(stringsAsFactors=FALSE)
options(width=220)
cat("OTWAY_BJPOLS_2011_RED_WALL_SERIAL_REPRODUCIBILITY_AUDIT_V1_0\n")

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
find_repo_root <- function(start_dir) {
  p<-normalizePath(start_dir,winslash="/",mustWork=TRUE)
  for(i in 0:12) {
    fw<-file.path(p,"framework","R","TDABMRadiusDiagnostics.R")
    if(file.exists(fw)) return(p)
    q<-dirname(p); if(identical(q,p)) break; p<-q
  }
  fail("Could not discover TDABM_Portability root.")
}

sp<-script_path()
script_dir<-if(dir.exists(sp))sp else dirname(sp)
repo<-find_repo_root(script_dir)
out_dir<-file.path(script_dir,"outputs")
dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)

# Recover the exact function definitions used by the original parallel run,
# while deliberately not executing its top-level workflow.
orig_script <- file.path(script_dir,"reference","original_parallel_run_script.R")
exprs <- parse(orig_script)
loaded <- character()
for (e in exprs) {
  if (is.call(e) && identical(e[[1]],as.name("<-")) &&
      length(e)>=3L && is.call(e[[3]]) &&
      identical(e[[3]][[1]],as.name("function"))) {
    eval(e,envir=.GlobalEnv)
    loaded <- c(loaded,as.character(e[[2]]))
  }
}
required <- c("build_cover","label_diagnostics","run_reps")
if(!all(required %in% loaded))
  fail("Could not recover required original functions: ",
       paste(setdiff(required,loaded),collapse=", "))

frozen<-file.path(script_dir,"input",
  "Otway_Rudkin_2011_Harmonised31D_input_frozen_S2011_A_v1_0.csv")
vars_file<-file.path(script_dir,"input","topology_variables.csv")
rw_file<-file.path(script_dir,"input","redwall_source_42_with_ons_ids.csv")
ref_file<-file.path(script_dir,"reference","parallel_1000_replicates.csv")

dat<-safe_read_csv(frozen)
vars<-safe_read_csv(vars_file)$topology_variable
rwsrc<-safe_read_csv(rw_file)
reference<-safe_read_csv(ref_file)

if(nrow(reference)!=1000L ||
   length(unique(reference$replicate))!=1000L ||
   length(unique(reference$seed))!=1000L)
  fail("Parallel reference does not contain 1,000 unique replicate/seed rows.")

matched_flag <- trimws(tolower(as.character(rwsrc$matched))) %in%
                c("true","t","1","yes","y")
matched_ids <- trimws(as.character(rwsrc$matched_ons_id[matched_flag]))
matched_ids <- matched_ids[!is.na(matched_ids)&nzchar(matched_ids)]
if(length(unique(matched_ids))!=41L) fail("Red Wall audit does not yield 41 IDs.")
redwall<-as.integer(dat$ONSConstID%in%matched_ids)
if(sum(redwall)!=41L) fail("Frozen frame does not yield 41 Red Wall rows.")

source(file.path(repo,"framework","R","TDABMRadiusDiagnostics.R"))
prepared<-tdabm_rd_prepare_topology(dat,"ONSConstID",vars,
                                    scaling="z_score",metric="euclidean")
D<-as.matrix(stats::dist(prepared$axes))

cat("Recomputing all 1,000 original seeds serially on one worker...\n")
serial <- run_reps(D,5.75,1000L,410000000L,1L,redwall)
write_csv(serial,file.path(out_dir,"01_serial_1000_replicates.csv"))

# Align exactly to the reference columns.
missing_cols <- setdiff(names(reference),names(serial))
extra_cols <- setdiff(names(serial),names(reference))
if(length(missing_cols) || length(extra_cols))
  fail("Column contract differs. Missing: ",paste(missing_cols,collapse=","),
       "; extra: ",paste(extra_cols,collapse=","))

serial <- serial[,names(reference),drop=FALSE]

# Compare character/integer/logical exactly; numeric with machine-level tolerance.
cmp_rows <- vector("list",length(names(reference)))
all_pass <- TRUE
for (j in seq_along(names(reference))) {
  nm <- names(reference)[j]
  a <- reference[[nm]]
  b <- serial[[nm]]
  if (is.numeric(a) || is.integer(a)) {
    aa <- as.numeric(a); bb <- as.numeric(b)
    delta <- abs(aa-bb)
    delta[is.na(delta) & is.na(aa) & is.na(bb)] <- 0
    max_delta <- if(length(delta)) max(delta,na.rm=TRUE) else 0
    pass <- all((delta <= 1e-12) | (is.na(aa)&is.na(bb)))
    detail <- paste0("max_abs_difference=",format(max_delta,scientific=TRUE))
  } else {
    pass <- identical(as.character(a),as.character(b))
    detail <- paste0("exact_character_match=",pass)
  }
  all_pass <- all_pass && pass
  cmp_rows[[j]] <- data.frame(column=nm,status=if(pass)"PASS"else"FAIL",
                              detail=detail,stringsAsFactors=FALSE)
}
cmp <- do.call(rbind,cmp_rows)
write_csv(cmp,file.path(out_dir,"02_column_comparison.csv"))

# Row-level maximum numeric differences for forensic inspection.
num_cols <- names(reference)[vapply(reference,is.numeric,logical(1))]
row_delta <- rep(0,nrow(reference))
for(nm in num_cols) {
  d <- abs(as.numeric(reference[[nm]])-as.numeric(serial[[nm]]))
  d[is.na(d)] <- 0
  row_delta <- pmax(row_delta,d)
}
row_audit <- data.frame(
  replicate=reference$replicate,
  seed=reference$seed,
  max_numeric_abs_difference=row_delta,
  stringsAsFactors=FALSE)
write_csv(row_audit,file.path(out_dir,"03_row_comparison.csv"))

# Diversity checks on both output sets.
metric_cols <- setdiff(names(reference),c("replicate","seed","radius"))
ref_key <- apply(reference[,metric_cols,drop=FALSE],1,paste,collapse="|")
ser_key <- apply(serial[,metric_cols,drop=FALSE],1,paste,collapse="|")
diversity <- data.frame(
  dataset=c("parallel_reference","serial_recomputation"),
  rows=c(nrow(reference),nrow(serial)),
  unique_replicates=c(length(unique(reference$replicate)),length(unique(serial$replicate))),
  unique_seeds=c(length(unique(reference$seed)),length(unique(serial$seed))),
  unique_metric_rows=c(length(unique(ref_key)),length(unique(ser_key))),
  stringsAsFactors=FALSE)
write_csv(diversity,file.path(out_dir,"04_diversity_audit.csv"))

gates <- data.frame(
  gate=c("parallel_reference_1000_rows",
         "serial_recomputation_1000_rows",
         "parallel_unique_seeds_1000",
         "serial_unique_seeds_1000",
         "parallel_unique_metric_rows_1000",
         "serial_unique_metric_rows_1000",
         "all_columns_reproduce",
         "all_rows_numeric_tolerance"),
  status=c(
    if(nrow(reference)==1000L)"PASS"else"FAIL",
    if(nrow(serial)==1000L)"PASS"else"FAIL",
    if(length(unique(reference$seed))==1000L)"PASS"else"FAIL",
    if(length(unique(serial$seed))==1000L)"PASS"else"FAIL",
    if(length(unique(ref_key))==1000L)"PASS"else"FAIL",
    if(length(unique(ser_key))==1000L)"PASS"else"FAIL",
    if(all(cmp$status=="PASS"))"PASS"else"FAIL",
    if(max(row_delta,na.rm=TRUE)<=1e-12)"PASS"else"FAIL"),
  detail=c(
    paste0("rows=",nrow(reference)),
    paste0("rows=",nrow(serial)),
    paste0("unique seeds=",length(unique(reference$seed))),
    paste0("unique seeds=",length(unique(serial$seed))),
    paste0("unique metric rows=",length(unique(ref_key))),
    paste0("unique metric rows=",length(unique(ser_key))),
    paste0("failing columns=",sum(cmp$status!="PASS")),
    paste0("max row numeric difference=",format(max(row_delta,na.rm=TRUE),scientific=TRUE))),
  stringsAsFactors=FALSE)
write_csv(gates,file.path(out_dir,"00_reproducibility_gates.csv"))

if(any(gates$status!="PASS"))
  fail("Serial reproducibility audit failed. See 00_reproducibility_gates.csv.")

writeLines(c(
  "status=PASS",
  paste0("generated=",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
  "audit=serial_vs_parallel_exact_reproducibility",
  "parallel_replicates=1000",
  "serial_replicates=1000",
  "serial_workers=1",
  "numeric_tolerance=1e-12"
),file.path(out_dir,"RUN_STATUS.txt"))

cat("\nPASS: all 1,000 one-core serial repetitions reproduce the original parallel handback.\n")
