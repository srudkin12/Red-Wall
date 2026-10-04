#!/usr/bin/env Rscript
options(warn=2)
options(stringsAsFactors=FALSE)

args <- commandArgs(trailingOnly=TRUE)
repo <- if (length(args)>=1L) normalizePath(args[[1L]],winslash="/",mustWork=TRUE) else normalizePath(".",winslash="/",mustWork=TRUE)
app_dir <- file.path(repo,"applications","Otway_Rudkin_2024_Corrected31D")
freeze_dir <- file.path(app_dir,"frozen_topology","p1_4_c_v1_0")

source(file.path(repo,"framework","R","TDABMTopologyFreeze.R"))
source(file.path(repo,"framework","R","TDABMFrozenTopology.R"))
source(file.path(repo,"framework","R","TDABMRadiusDiagnostics.R"))
source(file.path(app_dir,"R","OtwayRudkin2024Adapter.R"))
source(file.path(app_dir,"R","OtwayRudkinP1_4_C.R"))

config <- or24_read_contract(file.path(app_dir,"config","application_contract.csv"))
decision <- or24_read_contract(file.path(app_dir,"config","p1_4_c_radius_decision.csv"))

app_id <- or24_get(config,"application_id")
id_col <- or24_get(config,"id_col")
label_col <- or24_get(config,"label_col")
topology_vars <- or24_split(or24_get(config,"topology_variables"))
scaling <- or24_get(config,"scaling")
metric <- or24_get(config,"metric")
expected_n <- as.integer(or24_get(config,"final_rows"))
expected_dim <- as.integer(or24_get(config,"topology_dimension"))
approved_radius <- as.numeric(or24_get(decision,"approved_radius"))

if (abs(approved_radius-5.50)>1e-12) stop("Public canonical builder expects approved radius 5.50.",call.=FALSE)
if (dir.exists(freeze_dir)) {
  stop("Canonical freeze already exists. Remove it deliberately before rebuilding: ",freeze_dir,call.=FALSE)
}

frozen <- file.path(app_dir,"frozen_input","p1_4_a_v1_0",
                    paste0(app_id,"_input_frozen_P1_4_A_v1_0.csv"))
if (!file.exists(frozen)) stop("P1.4-A frozen input missing. Run P1.4-A first.",call.=FALSE)

data <- utils::read.csv(frozen,stringsAsFactors=FALSE,check.names=FALSE,fileEncoding="UTF-8")
if (nrow(data)!=expected_n) stop("Frozen input row count mismatch.",call.=FALSE)

canonical <- tdabm_tf_canonicalize(data,id_col,topology_vars)
if (length(canonical$ids)!=expected_n || ncol(canonical$axes)!=expected_dim) {
  stop("Canonical point preparation dimensions are incorrect.",call.=FALSE)
}
scaled_axes <- or24_c_scale_axes(canonical$axes,scaling)

b_prepared <- tdabm_rd_prepare_topology(data,id_col,topology_vars,scaling=scaling,metric=metric)
if (!identical(canonical$ids,b_prepared$ids)) stop("Stable-ID order differs from radius-diagnostics preparation.",call.=FALSE)
scale_diff <- max(abs(as.matrix(scaled_axes)-as.matrix(b_prepared$axes)))
if (!is.finite(scale_diff) || scale_diff>1e-14) stop("Scaled axes differ from radius-diagnostics preparation.",call.=FALSE)

unit_labels <- enc2utf8(as.character(canonical$data[[label_col]]))
point_order <- data.frame(
  point_index=seq_len(expected_n),
  source_row_index=as.integer(canonical$source_row_index),
  unit_id=canonical$ids,
  unit_label=unit_labels,
  stringsAsFactors=FALSE
)
scaled_sidecar <- cbind(point_order[,c("point_index","unit_id","unit_label"),drop=FALSE],scaled_axes)

tdabm_tf_compile_cpp(file.path(repo,"framework","BallMapper.cpp"))
bm <- tdabm_tf_build(scaled_axes,approved_radius)
object_check <- tdabm_tf_validate_object(bm,n_points=expected_n,expected_radius=approved_radius)
if (!all(object_check$pass)) stop("Canonical topology object validation failed.",call.=FALSE)

membership <- tdabm_tf_membership_table(bm,canonical$ids,unit_labels)
landmarks <- tdabm_tf_landmark_table(bm,canonical$ids,unit_labels)
edges <- tdabm_tf_edge_table(bm)
component_tables <- or24_c_component_tables(bm,membership,edges,expected_n)
fingerprint <- tdabm_tf_topology_fingerprint(bm,canonical$ids)
topology_lines <- tdabm_tf_topology_lines(bm,canonical$ids)
topology_summary <- or24_c_topology_summary(
  bm,membership,edges,component_tables,expected_n,expected_dim,fingerprint
)

expected_fp <- "2a7fae54601a98262d7196c0f3ad8c56d43bde998638d95ca24adfffdba4c539"
allow_variation <- identical(Sys.getenv("OTWAY_ALLOW_TOPOLOGY_VARIATION",unset="0"),"1")
if (!identical(fingerprint,expected_fp) && !allow_variation) {
  stop(
    "Canonical topology fingerprint differs from the accepted paper topology. ",
    "Check data construction. Set OTWAY_ALLOW_TOPOLOGY_VARIATION=1 only for ",
    "explicit exploratory use; such output is not an exact paper replication.",
    call.=FALSE
  )
}

headline <- c(
  balls=nrow(component_tables$ball),
  edges=nrow(edges),
  components=nrow(component_tables$component),
  isolates=sum(component_tables$ball$isolated_ball)
)
expected_headline <- c(balls=28L,edges=96L,components=2L,isolates=1L)
if (!allow_variation && !identical(as.integer(headline),as.integer(expected_headline))) {
  stop("Canonical headline topology differs from accepted paper topology.",call.=FALSE)
}

dir.create(freeze_dir,recursive=TRUE,showWarnings=FALSE)
tdabm_tf_write_csv_atomic(point_order,file.path(freeze_dir,"canonical_point_order.csv"))
tdabm_tf_write_csv_atomic(scaled_sidecar,file.path(freeze_dir,"canonical_scaled_axes.csv"))
tdabm_tf_write_csv_atomic(landmarks,file.path(freeze_dir,"canonical_landmarks.csv"))
tdabm_tf_write_csv_atomic(membership,file.path(freeze_dir,"canonical_memberships.csv"))
tdabm_tf_write_csv_atomic(edges,file.path(freeze_dir,"canonical_overlap_edges.csv"))
tdabm_tf_write_csv_atomic(component_tables$ball,file.path(freeze_dir,"canonical_ball_components.csv"))
tdabm_tf_write_csv_atomic(component_tables$component,file.path(freeze_dir,"canonical_component_summary.csv"))
tdabm_tf_write_csv_atomic(topology_summary,file.path(freeze_dir,"canonical_topology_summary.csv"))
tdabm_tf_write_lines_atomic(topology_lines,file.path(freeze_dir,"canonical_topology_fingerprint_source.txt"))
tdabm_tf_write_lines_atomic(fingerprint,file.path(freeze_dir,"canonical_topology_fingerprint.sha256"))
tdabm_tf_write_csv_atomic(object_check,file.path(freeze_dir,"PUBLIC_CANONICAL_TOPOLOGY_VALIDATION.csv"))
file.copy(file.path(app_dir,"config","p1_4_c_radius_decision.csv"),
          file.path(freeze_dir,"P1_4_C_radius_decision.csv"),overwrite=TRUE)

status <- c(
  "PUBLIC_CANONICAL_BUILD=PASS",
  paste0("source_mode=",Sys.getenv("OTWAY_SOURCE_MODE",unset="strict")),
  paste0("approved_radius=",format(approved_radius,nsmall=2L)),
  paste0("topology_fingerprint=",fingerprint),
  paste0("accepted_fingerprint_match=",identical(fingerprint,expected_fp)),
  paste0("balls=",headline[["balls"]]),
  paste0("edges=",headline[["edges"]]),
  paste0("components=",headline[["components"]]),
  paste0("isolates=",headline[["isolates"]])
)
writeLines(status,file.path(freeze_dir,"PUBLIC_CANONICAL_BUILD_STATUS.txt"),useBytes=TRUE)
cat(paste(status,collapse="\n"),"\n")
