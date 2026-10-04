#!/usr/bin/env Rscript
options(warn=2)
options(stringsAsFactors=FALSE)

parse_args <- function(x) {
  out <- list()
  for (a in x) {
    z <- strsplit(sub("^--","",a),"=",fixed=TRUE)[[1L]]
    out[[z[[1L]]]] <- paste(z[-1L],collapse="=")
  }
  out
}

a <- parse_args(commandArgs(trailingOnly=TRUE))
repo <- normalizePath(a[["repo"]], winslash="/", mustWork=TRUE)
app_dir <- normalizePath(a[["app-dir"]], winslash="/", mustWork=TRUE)
audit <- normalizePath(a[["audit-dir"]], winslash="/", mustWork=FALSE)
freeze_dir <- normalizePath(a[["freeze-dir"]], winslash="/", mustWork=FALSE)
b_audit_zip <- normalizePath(a[["b-audit-zip"]], winslash="/", mustWork=TRUE)

dir.create(audit, recursive=TRUE, showWarnings=FALSE)
if (dir.exists(freeze_dir) || file.exists(freeze_dir)) {
  stop("Canonical P1.4-C freeze already exists; refusing to overwrite: ", freeze_dir, call.=FALSE)
}

source(file.path(repo,"framework/R/TDABMUserRadiusContract.R"))
source(file.path(repo,"framework/R/TDABMTopologyFreeze.R"))
source(file.path(repo,"framework/R/TDABMFrozenTopology.R"))
source(file.path(repo,"framework/R/TDABMRadiusDiagnostics.R"))
source(file.path(app_dir,"R/OtwayRudkin2024Adapter.R"))
source(file.path(app_dir,"R/OtwayRudkinP1_4_C.R"))

config <- or24_read_contract(file.path(app_dir,"config/application_contract.csv"))
decision <- or24_read_contract(file.path(app_dir,"config/p1_4_c_radius_decision.csv"))

app_id <- or24_get(config,"application_id")
id_col <- or24_get(config,"id_col")
label_col <- or24_get(config,"label_col")
topology_vars <- or24_split(or24_get(config,"topology_variables"))
scaling <- or24_get(config,"scaling")
metric <- or24_get(config,"metric")
expected_n <- as.integer(or24_get(config,"final_rows"))
expected_dim <- as.integer(or24_get(config,"topology_dimension"))

approved_radius <- as.numeric(or24_get(decision,"approved_radius"))
expected_b_sha <- tolower(or24_get(decision,"diagnostic_audit_sha256"))
expected_frozen_sha <- tolower(or24_get(decision,"frozen_input_sha256_expected"))

# ---------------------------------------------------------------------------
# Verify the accepted B-stage evidence package used by the human radius decision.
# ---------------------------------------------------------------------------
observed_b_sha <- tolower(tdabm_tf_sha256(b_audit_zip))
if (!identical(observed_b_sha, expected_b_sha)) {
  stop("P1.4-B production audit SHA-256 does not match the approved radius decision.", call.=FALSE)
}

b_list <- utils::unzip(b_audit_zip, list=TRUE)$Name
pick_one <- function(pattern) {
  z <- grep(pattern, b_list, value=TRUE)
  if (length(z)!=1L) stop("Expected exactly one P1.4-B artifact matching: ",pattern,call.=FALSE)
  z
}
b_accept_name <- pick_one("P1_4_B_ACCEPTANCE\\.txt$")
b_summary_name <- pick_one("radius_summary\\.csv$")
b_exec_name <- pick_one("P1_4_B_execution_contract\\.csv$")

b_tmp <- tempfile("or24-p1-4-c-b-")
dir.create(b_tmp, recursive=TRUE)
on.exit(unlink(b_tmp, recursive=TRUE, force=TRUE), add=TRUE)
utils::unzip(b_audit_zip, files=c(b_accept_name,b_summary_name,b_exec_name), exdir=b_tmp)

b_accept <- readLines(file.path(b_tmp,b_accept_name), warn=FALSE, encoding="UTF-8")
if (!any(grepl(paste0("^",app_id," P1\\.4-B PRODUCTION: PASS$"), b_accept))) {
  stop("The supplied P1.4-B package is not an accepted production audit for this application.",call.=FALSE)
}
if (!any(b_accept=="READY_FOR_USER_RADIUS_DECISION=YES")) {
  stop("P1.4-B package was not ready for a user radius decision.",call.=FALSE)
}

get_accept_num <- function(key) {
  line <- grep(paste0("^",key,"="),b_accept,value=TRUE)
  if(length(line)!=1L) stop("Missing P1.4-B acceptance field: ",key,call.=FALSE)
  as.numeric(sub(paste0("^",key,"="),"",line))
}
get_accept_chr <- function(key) {
  line <- grep(paste0("^",key,"="),b_accept,value=TRUE)
  if(length(line)!=1L) stop("Missing P1.4-B acceptance field: ",key,call.=FALSE)
  sub(paste0("^",key,"="),"",line)
}

b_ref <- get_accept_num("pipeline_reference_radius")
b_lo <- get_accept_num("review_band_lower")
b_hi <- get_accept_num("review_band_upper")
b_frozen_sha <- tolower(get_accept_chr("frozen_input_sha256"))

tol <- 1e-12
if (abs(b_ref-as.numeric(or24_get(decision,"pipeline_reference_radius")))>tol ||
    abs(b_lo-as.numeric(or24_get(decision,"review_band_lower")))>tol ||
    abs(b_hi-as.numeric(or24_get(decision,"review_band_upper")))>tol) {
  stop("Recorded radius-decision diagnostics do not match the accepted P1.4-B audit.",call.=FALSE)
}
if (!identical(b_frozen_sha, expected_frozen_sha)) {
  stop("P1.4-B frozen-input SHA does not match the C-stage radius decision.",call.=FALSE)
}

b_exec <- utils::read.csv(file.path(b_tmp,b_exec_name),stringsAsFactors=FALSE,check.names=FALSE)
if (!identical(names(b_exec),c("field","value")) || anyDuplicated(b_exec$field)) {
  stop("Malformed P1.4-B execution contract.",call.=FALSE)
}
b_exec_map <- stats::setNames(as.list(as.character(b_exec$value)),as.character(b_exec$field))
req_exec <- c(
  mode="production",
  grid_design="USER_SPECIFIED_LINEAR_INTERVAL",
  radius_min="0.75",
  radius_max="20.00",
  radius_step="0.05",
  grid_points="386",
  repetitions="1000",
  workers="50",
  stability_engine="EXACT_PACKED_CPP_OPENMP",
  stability_threads="50",
  automatic_approval="FALSE",
  optimality_claim="NONE"
)
for (nm in names(req_exec)) {
  got <- b_exec_map[[nm]]
  if (is.null(got) || !identical(as.character(got),as.character(req_exec[[nm]]))) {
    stop("Unexpected P1.4-B execution contract field ",nm,": ",got %||% "<missing>",call.=FALSE)
  }
}

b_summary <- utils::read.csv(file.path(b_tmp,b_summary_name),stringsAsFactors=FALSE,check.names=FALSE)
sel <- b_summary[abs(as.numeric(b_summary$radius)-approved_radius)<=1e-12,,drop=FALSE]
if (nrow(sel)!=1L) {
  stop("Approved radius is not represented exactly once in the accepted P1.4-B production grid.",call.=FALSE)
}
tdabm_tf_write_csv_atomic(sel,file.path(audit,"P1_4_C_selected_radius_B_diagnostics.csv"))

# ---------------------------------------------------------------------------
# Validate and materialise the explicit human radius contract.
# ---------------------------------------------------------------------------
urc <- tdabm_urc_contract(
  application=app_id,
  target_radius=approved_radius,
  selection_basis=or24_get(decision,"selection_basis"),
  interpretation=or24_get(decision,"interpretation"),
  decision_rationale=or24_get(decision,"decision_rationale"),
  topology_axes=topology_vars,
  scaling=scaling,
  metric=metric,
  precision_policy=or24_get(decision,"precision_policy"),
  pipeline_reference_radius=as.numeric(or24_get(decision,"pipeline_reference_radius")),
  pipeline_reference_method=or24_get(decision,"pipeline_reference_method"),
  review_band_lower=as.numeric(or24_get(decision,"review_band_lower")),
  review_band_upper=as.numeric(or24_get(decision,"review_band_upper")),
  diagnostic_audit_sha256=expected_b_sha,
  approved_by=or24_get(decision,"decision_maker"),
  approved_at=or24_get(decision,"decision_date"),
  automatic_selection_performed=FALSE,
  optimality_claim="NONE"
)
urc_check <- tdabm_urc_validate(urc, stop_on_error=TRUE)
tdabm_tf_write_csv_atomic(tdabm_urc_table(urc),file.path(audit,"P1_4_C_user_radius_contract.csv"))
tdabm_tf_write_csv_atomic(urc_check,file.path(audit,"P1_4_C_user_radius_contract_validation.csv"))

# Preserve the fuller decision record, including historical-project context.
decision_file <- file.path(app_dir,"config/p1_4_c_radius_decision.csv")
file.copy(decision_file,file.path(audit,"P1_4_C_radius_decision.csv"),overwrite=TRUE)

# ---------------------------------------------------------------------------
# Consume the immutable P1.4-A input; canonicalise order and reproduce B scaling.
# ---------------------------------------------------------------------------
frozen <- file.path(
  app_dir,"frozen_input","p1_4_a_v1_0",
  paste0(app_id,"_input_frozen_P1_4_A_v1_0.csv")
)
if (!file.exists(frozen)) stop("Accepted P1.4-A frozen input is missing.",call.=FALSE)
input_sha_before <- tolower(tdabm_tf_sha256(frozen))
if (!identical(input_sha_before,expected_frozen_sha)) {
  stop("Accepted P1.4-A frozen input SHA-256 mismatch.",call.=FALSE)
}

data <- utils::read.csv(
  frozen,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8"
)
if (nrow(data)!=expected_n) stop("Frozen input row count mismatch.",call.=FALSE)

canonical <- tdabm_tf_canonicalize(data,id_col,topology_vars)
if (length(canonical$ids)!=expected_n || ncol(canonical$axes)!=expected_dim) {
  stop("Canonical point preparation dimensions are incorrect.",call.=FALSE)
}
scaled_axes <- or24_c_scale_axes(canonical$axes,scaling)

# Independent equality check against the exact P1.4-B topology-preparation helper.
b_prepared <- tdabm_rd_prepare_topology(
  data,id_col,topology_vars,scaling=scaling,metric=metric
)
if (!identical(canonical$ids,b_prepared$ids)) {
  stop("P1.4-C stable-ID point order differs from P1.4-B preparation.",call.=FALSE)
}
scale_diff <- max(abs(as.matrix(scaled_axes)-as.matrix(b_prepared$axes)))
if (!is.finite(scale_diff) || scale_diff>1e-14) {
  stop("P1.4-C scaled topology axes differ from P1.4-B preparation.",call.=FALSE)
}

unit_labels <- enc2utf8(as.character(canonical$data[[label_col]]))
point_order <- data.frame(
  point_index=seq_len(expected_n),
  source_row_index=as.integer(canonical$source_row_index),
  unit_id=canonical$ids,
  unit_label=unit_labels,
  stringsAsFactors=FALSE
)
scaled_sidecar <- cbind(
  point_order[,c("point_index","unit_id","unit_label"),drop=FALSE],
  scaled_axes
)

# ---------------------------------------------------------------------------
# Construction boundary: compile once; construct Ball Mapper exactly once.
# ---------------------------------------------------------------------------
tdabm_tf_compile_cpp(file.path(repo,"framework/BallMapper.cpp"))
bm <- tdabm_tf_build(scaled_axes,approved_radius)

object_check <- tdabm_tf_validate_object(
  bm,n_points=expected_n,expected_radius=approved_radius
)
if (!all(object_check$pass)) {
  bad <- object_check[!object_check$pass,,drop=FALSE]
  stop("Canonical topology validation failed: ",
       paste(paste0(bad$check,"=",bad$detail),collapse=" | "),call.=FALSE)
}

membership <- tdabm_tf_membership_table(bm,canonical$ids,unit_labels)
landmarks <- tdabm_tf_landmark_table(bm,canonical$ids,unit_labels)
edges <- tdabm_tf_edge_table(bm)
component_tables <- or24_c_component_tables(bm,membership,edges,expected_n)

topology_lines <- tdabm_tf_topology_lines(bm,canonical$ids)
fingerprint <- tdabm_tf_topology_fingerprint(bm,canonical$ids)

topology_summary <- or24_c_topology_summary(
  bm,membership,edges,component_tables,expected_n,expected_dim,fingerprint
)

# ---------------------------------------------------------------------------
# Additional fail-closed application validations.
# ---------------------------------------------------------------------------
checks <- data.frame(check=character(),pass=logical(),detail=character(),stringsAsFactors=FALSE)
checks <- or24_c_add_check(checks,"radius_contract_all_pass",all(urc_check$pass),paste(sum(urc_check$pass),"/",nrow(urc_check)))
checks <- or24_c_add_check(checks,"topology_object_all_pass",all(object_check$pass),paste(sum(object_check$pass),"/",nrow(object_check)))
checks <- or24_c_add_check(checks,"point_order_sorted",identical(canonical$ids,sort(canonical$ids,method="radix")),"stable IDs sorted by radix order")
checks <- or24_c_add_check(checks,"point_order_unique",!anyDuplicated(canonical$ids),paste(length(unique(canonical$ids)),"unique IDs"))
checks <- or24_c_add_check(checks,"scaling_matches_p1_4_b",scale_diff<=1e-14,format(scale_diff,digits=17))
checks <- or24_c_add_check(checks,"landmark_identity",all(vapply(seq_len(nrow(landmarks)),function(i) landmarks$point_index[[i]] %in% bm$points_covered_by_landmarks[[i]],logical(1))),"each landmark point belongs to its own ball")
checks <- or24_c_add_check(checks,"membership_coverage",identical(sort(unique(membership$point_index)),seq_len(expected_n)),"all canonical points are covered")
checks <- or24_c_add_check(checks,"membership_coverage_duality",or24_c_validate_duality(bm,membership,expected_n),"point coverage agrees exactly with ball membership table")
checks <- or24_c_add_check(checks,"edge_overlap_strength",or24_c_validate_edges(bm,edges),"all edges and overlap strengths agree with memberships")
checks <- or24_c_add_check(checks,"observation_identity",identical(as.character(point_order$unit_id[membership$point_index]),as.character(membership$unit_id)),"membership IDs agree with canonical point order")
checks <- or24_c_add_check(checks,"component_assignment_complete",all(component_tables$ball$component_id>=1L),"every ball assigned to one component")
checks <- or24_c_add_check(checks,"fingerprint_format",grepl("^[0-9a-f]{64}$",fingerprint),"SHA-256 topology fingerprint")
checks <- or24_c_add_check(checks,"approved_radius_in_B_grid",nrow(sel)==1L,paste0("radius=",approved_radius))
checks <- or24_c_add_check(checks,"B_audit_sha256",identical(observed_b_sha,expected_b_sha),observed_b_sha)
checks <- or24_c_add_check(checks,"A_frozen_input_sha256",identical(input_sha_before,expected_frozen_sha),input_sha_before)

if (!all(checks$pass)) {
  bad <- checks[!checks$pass,,drop=FALSE]
  stop("P1.4-C fail-closed validation failed: ",
       paste(paste0(bad$check,"=",bad$detail),collapse=" | "),call.=FALSE)
}

# ---------------------------------------------------------------------------
# Write canonical audit artifacts.
# ---------------------------------------------------------------------------
canonical_rds <- file.path(audit,paste0(app_id,"_canonical_topology_P1_4_C_v1_0.rds"))
tdabm_tf_save_rds_atomic(bm,canonical_rds)
tdabm_tf_write_csv_atomic(point_order,file.path(audit,"canonical_point_order.csv"))
tdabm_tf_write_csv_atomic(scaled_sidecar,file.path(audit,"canonical_scaled_axes.csv"))
tdabm_tf_write_csv_atomic(landmarks,file.path(audit,"canonical_landmarks.csv"))
tdabm_tf_write_csv_atomic(membership,file.path(audit,"canonical_memberships.csv"))
tdabm_tf_write_csv_atomic(edges,file.path(audit,"canonical_overlap_edges.csv"))
tdabm_tf_write_csv_atomic(component_tables$ball,file.path(audit,"canonical_ball_components.csv"))
tdabm_tf_write_csv_atomic(component_tables$component,file.path(audit,"canonical_component_summary.csv"))
tdabm_tf_write_csv_atomic(topology_summary,file.path(audit,"canonical_topology_summary.csv"))
tdabm_tf_write_lines_atomic(topology_lines,file.path(audit,"canonical_topology_fingerprint_source.txt"))
tdabm_tf_write_lines_atomic(fingerprint,file.path(audit,"canonical_topology_fingerprint.sha256"))
tdabm_tf_write_csv_atomic(object_check,file.path(audit,"P1_4_C_generic_topology_validation.csv"))
tdabm_tf_write_csv_atomic(checks,file.path(audit,"P1_4_C_application_validation.csv"))

b_provenance <- data.frame(
  field=c(
    "diagnostic_audit_zip","diagnostic_audit_sha256",
    "pipeline_reference_radius","review_band_lower","review_band_upper",
    "approved_radius","approved_radius_grid_row"
  ),
  value=c(
    basename(b_audit_zip),observed_b_sha,
    format(b_ref,digits=17),format(b_lo,digits=17),format(b_hi,digits=17),
    format(approved_radius,nsmall=2L),as.character(sel$radius_index[[1L]])
  ),
  stringsAsFactors=FALSE
)
tdabm_tf_write_csv_atomic(b_provenance,file.path(audit,"P1_4_C_B_stage_provenance.csv"))

# Reload test: the serialized canonical object must preserve topology exactly.
bm_reload <- readRDS(canonical_rds)
reload_check <- data.frame(
  check=c("rds_topology_identical","rds_fingerprint_identical","rds_radius_identical"),
  pass=c(
    tdabm_ft_topology_identical(bm,bm_reload),
    identical(fingerprint,tdabm_tf_topology_fingerprint(bm_reload,canonical$ids)),
    isTRUE(all.equal(as.numeric(bm_reload$epsilon),approved_radius,tolerance=0))
  ),
  stringsAsFactors=FALSE
)
if (!all(reload_check$pass)) stop("Serialized canonical topology failed reload validation.",call.=FALSE)
tdabm_tf_write_csv_atomic(reload_check,file.path(audit,"P1_4_C_serialization_validation.csv"))

input_sha_after <- tolower(tdabm_tf_sha256(frozen))
if (!identical(input_sha_before,input_sha_after)) {
  stop("Frozen P1.4-A input changed during P1.4-C.",call.=FALSE)
}

# ---------------------------------------------------------------------------
# Freeze accepted canonical artifacts into a stable read-only stage directory.
# A pre-existing canonical freeze is never overwritten.
# ---------------------------------------------------------------------------
freeze_parent <- dirname(freeze_dir)
dir.create(freeze_parent,recursive=TRUE,showWarnings=FALSE)
tmp_freeze <- paste0(freeze_dir,".tmp-",Sys.getpid())
if (dir.exists(tmp_freeze)) unlink(tmp_freeze,recursive=TRUE,force=TRUE)
dir.create(tmp_freeze,recursive=TRUE,showWarnings=FALSE)
on.exit(if(dir.exists(tmp_freeze))unlink(tmp_freeze,recursive=TRUE,force=TRUE),add=TRUE)

freeze_names <- c(
  basename(canonical_rds),
  "canonical_point_order.csv",
  "canonical_scaled_axes.csv",
  "canonical_landmarks.csv",
  "canonical_memberships.csv",
  "canonical_overlap_edges.csv",
  "canonical_ball_components.csv",
  "canonical_component_summary.csv",
  "canonical_topology_summary.csv",
  "canonical_topology_fingerprint_source.txt",
  "canonical_topology_fingerprint.sha256",
  "P1_4_C_user_radius_contract.csv",
  "P1_4_C_radius_decision.csv",
  "P1_4_C_B_stage_provenance.csv",
  "P1_4_C_generic_topology_validation.csv",
  "P1_4_C_application_validation.csv",
  "P1_4_C_serialization_validation.csv",
  "P1_4_C_selected_radius_B_diagnostics.csv"
)

for (nm in freeze_names) {
  src <- file.path(audit,nm)
  if (!file.exists(src)) stop("Required C-stage freeze artifact is missing: ",nm,call.=FALSE)
  if (!file.copy(src,file.path(tmp_freeze,nm),overwrite=FALSE,copy.mode=TRUE,copy.date=TRUE)) {
    stop("Could not copy canonical freeze artifact: ",nm,call.=FALSE)
  }
  if (!identical(tdabm_tf_sha256(src),tdabm_tf_sha256(file.path(tmp_freeze,nm)))) {
    stop("Canonical freeze copy hash mismatch: ",nm,call.=FALSE)
  }
}

if (!file.rename(tmp_freeze,freeze_dir)) {
  stop("Could not atomically promote P1.4-C canonical freeze directory.",call.=FALSE)
}

freeze_files <- list.files(freeze_dir,full.names=TRUE,recursive=FALSE)
freeze_manifest <- data.frame(
  artifact=basename(freeze_files),
  sha256=vapply(freeze_files,tdabm_tf_sha256,character(1)),
  bytes=as.numeric(file.info(freeze_files)$size),
  stringsAsFactors=FALSE
)
tdabm_tf_write_csv_atomic(freeze_manifest,file.path(audit,"P1_4_C_freeze_manifest.csv"))

canonical_sha <- tdabm_tf_sha256(canonical_rds)

acceptance <- c(
  paste0(app_id," P1.4-C: PASS"),
  paste0("approved_radius=",format(approved_radius,nsmall=2L)),
  paste0("review_band_position=",urc$review_band_position),
  paste0("final_rows=",expected_n),
  paste0("topology_dimension=",expected_dim),
  paste0("n_balls=",topology_summary$n_balls[[1L]]),
  paste0("n_edges=",topology_summary$n_edges[[1L]]),
  paste0("n_components=",topology_summary$n_components[[1L]]),
  paste0("n_isolated_balls=",topology_summary$n_isolated_balls[[1L]]),
  paste0("topology_fingerprint_sha256=",fingerprint),
  paste0("canonical_object_sha256=",canonical_sha),
  paste0("frozen_input_sha256=",input_sha_after),
  paste0("diagnostic_audit_sha256=",observed_b_sha),
  "automatic_selection=FALSE",
  "optimality_claim=NONE",
  "canonical_topology_immutable=YES",
  "READY_FOR_P1_4_D=YES"
)
tdabm_tf_write_lines_atomic(acceptance,file.path(audit,"P1_4_C_ACCEPTANCE.txt"))

# Audit hashes are written last and exclude themselves.
audit_files <- list.files(audit,full.names=TRUE,recursive=FALSE)
audit_files <- audit_files[!basename(audit_files) %in% c("P1_4_C_artifact_hashes.csv","P1_4_C_run.log")]
or24_c_write_hash_manifest(audit_files,audit,file.path(audit,"P1_4_C_artifact_hashes.csv"))

cat(paste(acceptance,collapse="\n"),"\n")
