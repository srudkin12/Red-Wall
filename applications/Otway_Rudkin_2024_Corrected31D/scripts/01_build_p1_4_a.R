#!/usr/bin/env Rscript
options(warn=2);options(stringsAsFactors=FALSE)

parse_args <- function(x) {
  out<-list()
  for(a in x) {
    z<-strsplit(sub("^--","",a),"=",fixed=TRUE)[[1L]]
    out[[z[[1L]]]]<-paste(z[-1L],collapse="=")
  }
  out
}
a<-parse_args(commandArgs(trailingOnly=TRUE))
repo<-normalizePath(a[["repo"]],winslash="/",mustWork=TRUE)
app_dir<-normalizePath(a[["app-dir"]],winslash="/",mustWork=TRUE)
audit<-normalizePath(a[["audit-dir"]],winslash="/",mustWork=FALSE)
dir.create(audit,recursive=TRUE,showWarnings=FALSE)

source(file.path(repo,"framework/R/TDABMArtifactProvenance.R"))
source(file.path(repo,"framework/R/TDABMValueContracts.R"))
source(file.path(repo,"framework/R/TDABMInputFreeze.R"))
source(file.path(app_dir,"R/OtwayRudkin2024Adapter.R"))

config<-or24_read_contract(file.path(app_dir,"config/application_contract.csv"))
app_id<-or24_get(config,"application_id")
evidence<-file.path(repo,or24_get(config,"shared_evidence_relative_path"))
source_file<-file.path(evidence,"source/merged_data.csv")
expected_sha<-or24_get(config,"authoritative_source_sha256")

raw<-or24_read_source(source_file)
or24_assert_source(raw,expected_sha,source_file)
sample_source_order<-or24_apply_sample(raw)
topology_vars<-or24_split(or24_get(config,"topology_variables"))
colour_vars<-or24_split(or24_get(config,"colour_variables"))
index_name<-or24_get(config,"left_behind_index_variable")
index_kind<-if(grepl("Historical30D$",app_id))"historical" else "corrected"

if (anyNA(sample_source_order[,topology_vars,drop=FALSE])) {
  stop("Topology missingness remains after the declared sample rule.",call.=FALSE)
}

validation_rows<-list()
add<-function(check,pass,detail) {
  validation_rows[[length(validation_rows)+1L]]<<-data.frame(
    check=check,pass=isTRUE(pass),detail=as.character(detail),stringsAsFactors=FALSE
  )
}

add("source_sha256",identical(or24_sha256(source_file),tolower(expected_sha)),or24_sha256(source_file))
add("source_shape",nrow(raw)==573L && ncol(raw)==84L,paste(nrow(raw),ncol(raw),sep="x"))
add("source_id_unique",!anyDuplicated(as.character(raw$ONSConstID)),length(unique(raw$ONSConstID)))
add("final_sample_n",nrow(sample_source_order)==571L,nrow(sample_source_order))
add("topology_dimension",length(topology_vars)==as.integer(or24_get(config,"topology_dimension")),length(topology_vars))
add("topology_complete",!anyNA(sample_source_order[,topology_vars,drop=FALSE]),sum(is.na(sample_source_order[,topology_vars,drop=FALSE])))

if (identical(index_kind,"historical")) {
  target_raw<-utils::read.csv(
    file.path(evidence,"evidence/portable_validation_targets/historical_ev1_unstandardised_target.csv"),
    stringsAsFactors=FALSE,check.names=FALSE
  )
  target_z<-utils::read.csv(
    file.path(evidence,"evidence/portable_validation_targets/historical_ev2_zscore_target.csv"),
    stringsAsFactors=FALSE,check.names=FALSE
  )
  raw_matrix<-as.matrix(sample_source_order[,topology_vars,drop=FALSE])
  storage.mode(raw_matrix)<-"double"
  target_raw_matrix<-as.matrix(target_raw);storage.mode(target_raw_matrix)<-"double"
  target_z_matrix<-as.matrix(target_z);storage.mode(target_z_matrix)<-"double"
  if (!identical(dim(raw_matrix),dim(target_raw_matrix))) stop("Historical raw target dimensions differ.",call.=FALSE)
  raw_diff<-max(abs(raw_matrix-target_raw_matrix))
  z_matrix<-scale(raw_matrix)
  z_diff<-max(abs(z_matrix-target_z_matrix))
  add("historical_ev1_raw_reproduction",is.finite(raw_diff) && raw_diff<=1e-12,format(raw_diff,digits=17))
  add("historical_ev2_zscore_reproduction",is.finite(z_diff) && z_diff<=1e-12,format(z_diff,digits=17))
} else {
  qdiff<-or24_check_corrected_index(sample_source_order)
  add("corrected_below_level3_source_validation",is.finite(qdiff) && qdiff<=1e-12,format(qdiff,digits=17))
  add("qualifications_level1_present","qualifications_level1" %in% topology_vars,paste(topology_vars,collapse=";"))
  add("corrected_dimension_31",length(topology_vars)==31L,length(topology_vars))
}

candidate<-or24_make_candidate(sample_source_order,topology_vars,index_name,index_kind)

contract<-tdabm_input_contract(
  application_id=app_id,
  id_col="ONSConstID",
  label_col="ConstituencyName",
  topology_variables=topology_vars,
  colour_variables=colour_vars,
  scaling="z_score",
  metric="euclidean",
  expected_rows=571L,
  numeric_lower=NULL,
  numeric_upper=NULL,
  require_nonconstant_topology=TRUE,
  approved_exclusions="E14000637 Chorley; E14000608 Buckingham",
  radius_status="PENDING_USER_DECISION"
)
valid<-tdabm_validate_input_frame(candidate,contract,stop_on_failure=TRUE)
add("generic_input_contract",isTRUE(valid$pass),paste(sum(valid$report$pass),"/",nrow(valid$report)))

if(!all(vapply(validation_rows,function(x)isTRUE(x$pass[[1L]]),logical(1)))) {
  stop("Application P1.4-A validation failed before freeze.",call.=FALSE)
}

candidate_file<-file.path(audit,paste0(app_id,"_P1_4_A_candidate.csv"))
or24_write_csv(candidate,candidate_file)

freeze_dir<-file.path(app_dir,"frozen_input","p1_4_a_v1_0")
frozen_file<-file.path(freeze_dir,paste0(app_id,"_input_frozen_P1_4_A_v1_0.csv"))
freeze<-tdabm_freeze_input_csv(
  candidate_file,frozen_file,contract,
  metadata=list(
    source_sha256=or24_sha256(source_file),
    source_encoding="CP1252",
    source_rows=573L,
    final_rows=571L,
    topology_dimension=length(topology_vars),
    radius_status="PENDING_USER_DECISION",
    automatic_selection="FALSE",
    optimality_claim="NONE"
  ),
  allow_existing=TRUE
)

checks<-do.call(rbind,validation_rows)
or24_write_csv(checks,file.path(audit,"P1_4_A_application_checks.csv"))
or24_write_csv(valid$report,file.path(audit,"P1_4_A_generic_input_contract_checks.csv"))
or24_write_csv(freeze$manifest,file.path(audit,"P1_4_A_freeze_manifest.csv"))

# Machine-readable topology and colour contracts.
or24_write_csv(
  data.frame(order=seq_along(topology_vars),variable=topology_vars,role="TOPOLOGY",stringsAsFactors=FALSE),
  file.path(audit,"P1_4_A_topology_variable_contract.csv")
)
or24_write_csv(
  data.frame(variable=colour_vars,role="POST_TOPOLOGY_COLOUR",stringsAsFactors=FALSE),
  file.path(audit,"P1_4_A_colour_variable_contract.csv")
)

source_lineage<-data.frame(
  artifact=c("original_handover_zip","authoritative_merged_data","frozen_input"),
  path=c(
    "external/original handover zip",
    normalizePath(source_file,winslash="/",mustWork=TRUE),
    normalizePath(frozen_file,winslash="/",mustWork=TRUE)
  ),
  sha256=c(
    "94224475e92b4ad92543190bf87b708b26158531698bcdf01e3c2737c678680f",
    or24_sha256(source_file),
    or24_sha256(frozen_file)
  ),
  stringsAsFactors=FALSE
)
or24_write_csv(source_lineage,file.path(audit,"P1_4_A_source_lineage.csv"))

acceptance<-c(
  paste0(app_id," P1.4-A: PASS"),
  paste0("final_rows=571"),
  paste0("topology_dimension=",length(topology_vars)),
  "scaling=z_score",
  "metric=euclidean",
  "radius_status=PENDING_USER_DECISION",
  "automatic_radius_selection=FALSE",
  "optimality_claim=NONE",
  paste0("frozen_input=",normalizePath(frozen_file,winslash="/",mustWork=TRUE)),
  paste0("frozen_input_sha256=",or24_sha256(frozen_file)),
  "READY_FOR_P1_4_B=YES"
)
writeLines(acceptance,file.path(audit,"P1_4_A_ACCEPTANCE.txt"),useBytes=TRUE)
cat(paste(acceptance,collapse="\n"),"\n")
