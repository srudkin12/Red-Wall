#!/usr/bin/env Rscript
options(warn=2)
args<-commandArgs(trailingOnly=TRUE)
repo<-if(length(args))normalizePath(args[[1L]],winslash="/",mustWork=TRUE) else normalizePath(".",winslash="/",mustWork=TRUE)
rdir<-file.path(repo,"framework","R")
files<-c(
"TDABMFramework.R","TDABMUtilities.R","TDABMPaths.R","TDABMValueContracts.R",
"TDABMProject.R","TDABMValidation.R","TDABMExecution.R","TDABMMembership.R",
"TDABMOutlierDiagnostics.R","TDABMRobustness.R","TDABMOrchestrator.R",
"TDABMArtifactProvenance.R","TDABMRadiusContract.R","TDABMUserRadiusContract.R",
"TDABMInputFreeze.R","TDABMRadiusDiagnostics.R","TDABMTopologyFreeze.R",
"TDABMFrozenTopology.R","TDABMEvidenceProvenance.R","TDABMPaperEvidence.R",
"TDABMInterpretationRobustness.R","TDABMSemanticEquivalence.R",
"TDABMStageSemanticContracts.R","TDABMClaimContracts.R"
)
for(f in files){
  p<-file.path(rdir,f)
  stopifnot(file.exists(p))
  source(p,local=.GlobalEnv)
}
cat("PASS: clean generic framework modules source successfully in a fresh R session.\n")
