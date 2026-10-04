#!/usr/bin/env Rscript
options(warn=2);options(stringsAsFactors=FALSE)
args<-commandArgs(trailingOnly=TRUE)
repo<-if(length(args))normalizePath(args[[1L]],winslash="/",mustWork=TRUE) else normalizePath(".",winslash="/",mustWork=TRUE)
source(file.path(repo,"framework/R/TDABMPaperEvidence.R"))

bm<-list(
  points_covered_by_landmarks=list(c(1L,2L,3L),c(3L,4L,5L),c(5L,6L)),
  landmarks=c(1L,4L,6L),
  edges=matrix(c(1L,2L,2L,3L),ncol=2L,byrow=TRUE),
  strength_of_edges=c(1L,1L),
  vertices=matrix(c(1,5,2,5,3,4),ncol=2L,byrow=TRUE),
  coverage=list(c(1L),c(1L),c(1L,2L),c(2L),c(2L,3L),c(3L)),
  coloring=c(0,0,0),
  epsilon=1
)
ids<-paste0("u",1:6);labels<-paste0("Unit ",1:6)
d<-data.frame(x=c(0,1,2,3,4,5),z=c(5,4,3,2,1,0),y=c(10,12,14,20,22,30))
s<-tdabm_pe_topology_summary(bm,6,"abc")
stopifnot(s$n_balls==3,s$n_edges==2,s$n_components==1,abs(s$fraction_multicovered-2/6)<1e-12)
o<-tdabm_pe_overlap_tables(bm,ids,labels)
stopifnot(o$summary$n_multicovered==2)
p<-tdabm_pe_ball_profile_wide(bm,d,ids,labels,c("x","z"),"y")
stopifnot(nrow(p)==3,all(c("axis_mean__x","axis_z__z","colour_mean__y")%in%names(p)))
lc<-tdabm_pe_local_colour(bm,d$y,ids,labels)
# ball means: 12, 18.666..., 26; u3 belongs to first two
stopifnot(abs(lc$local_colour[3]-mean(c(12,mean(c(14,20,22)))))<1e-12)
core_before<-list(bm$vertices,bm$edges,bm$points_covered_by_landmarks,bm$landmarks,bm$epsilon)
tdabm_pe_profile_long(bm,d,c("x","z"),"topology_axis",ids)
core_after<-list(bm$vertices,bm$edges,bm$points_covered_by_landmarks,bm$landmarks,bm$epsilon)
stopifnot(identical(core_before,core_after))
cat("PASS: generic canonical topology evidence is observation/ball consistent.\n")
cat("PASS: point-indexed local-colour aggregation is exact on a known overlap case.\n")
cat("PASS: paper-evidence utilities do not modify frozen topology.\n")
