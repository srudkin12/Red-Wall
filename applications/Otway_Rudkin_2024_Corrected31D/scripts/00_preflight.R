#!/usr/bin/env Rscript
options(warn=2);options(stringsAsFactors=FALSE)
args<-commandArgs(trailingOnly=TRUE)
app_dir<-normalizePath(args[[1L]],winslash="/",mustWork=TRUE)
repo<-normalizePath(file.path(app_dir,"../.."),winslash="/",mustWork=TRUE)
source(file.path(app_dir,"R/OtwayRudkin2024Adapter.R"))
config<-or24_read_contract(file.path(app_dir,"config/application_contract.csv"))
evidence<-file.path(repo,or24_get(config,"shared_evidence_relative_path"))
source_file<-file.path(evidence,"source/merged_data.csv")
x<-or24_read_source(source_file)
or24_assert_source(x,or24_get(config,"authoritative_source_sha256"),source_file)
s<-or24_apply_sample(x)
vars<-or24_split(or24_get(config,"topology_variables"))
stopifnot(nrow(s)==571L,length(vars)==as.integer(or24_get(config,"topology_dimension")))
stopifnot(all(vars %in% names(s)),!anyNA(s[,vars,drop=FALSE]))
if(grepl("Corrected31D$",or24_get(config,"application_id"))){
  stopifnot("qualifications_level1" %in% vars)
  stopifnot(or24_check_corrected_index(s)<=1e-12)
}else{
  stopifnot(!"qualifications_level1" %in% vars)
}
cat("PASS:",or24_get(config,"application_id"),"source/application contract preflight.\n")
