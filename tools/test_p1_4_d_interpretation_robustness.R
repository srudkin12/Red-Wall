#!/usr/bin/env Rscript
options(warn=2);options(stringsAsFactors=FALSE)
args<-commandArgs(trailingOnly=TRUE)
repo<-if(length(args))normalizePath(args[[1L]],winslash="/",mustWork=TRUE) else normalizePath(".",winslash="/",mustWork=TRUE)
source(file.path(repo,"framework/R/TDABMInterpretationRobustness.R"))

set.seed(44)
n<-18L
axes<-data.frame(x=seq(0,17),z=sin(seq(0,17)/3)*3)
ids<-sprintf("id%02d",seq_len(n))
colours<-data.frame(y=10+axes$x+axes$z,w=50-0.5*axes$x)
# A deliberately simple canonical comparison vector; the robustness engine only
# requires a fixed point-indexed benchmark and never uses colours to choose radii.
canonical<-list(y=colours$y,w=colours$w)
radii<-c(2.5,4,6)
cpp<-file.path(repo,"framework/BallMapper.cpp")
module<-file.path(repo,"framework/R/TDABMInterpretationRobustness.R")

a<-tdabm_ir_run(axes,ids,colours,canonical,radii,4,6,20260807,1,"equal_ball_mean",module,cpp)
b<-tdabm_ir_run(axes,ids,colours,canonical,radii,4,6,20260807,2,"equal_ball_mean",module,cpp)
chk<-tdabm_ir_compare_runs_exact(a,b)
stopifnot(all(chk$pass))
stopifnot(nrow(a$topology)==18,nrow(a$comparison)==36,nrow(a$points)==36*n)
stopifnot(all(is.finite(a$points$local_colour)))
stopifnot(all(a$topology$n_balls>=1))
sumr<-tdabm_ir_summarise_radius(a)
pm<-tdabm_ir_radius_point_mean_summary(a)
pat<-tdabm_ir_radius_mean_pattern_summary(pm,4)
stopifnot(setequal(unique(sumr$radius),radii),setequal(unique(sumr$colour_variable),c("y","w")))
stopifnot(nrow(pm)==length(radii)*length(canonical)*n,nrow(pat)==length(radii)*length(canonical))
stopifnot(all(abs(pat$rmse_to_approved_repeated_mean[pat$is_approved_radius])<1e-12))
cat("PASS: generic post-approval robustness engine is exactly worker invariant.\n")
cat("PASS: arbitrary ball labels are bypassed by point-indexed local-colour comparison.\n")
cat("PASS: robustness radii are supplied externally and include the approved radius.\n")
