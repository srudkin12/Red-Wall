options(stringsAsFactors=FALSE)
options(width=220)
cat("OTWAY_BJPOLS_2011_2021_TEMPORAL_CONCENTRATION_CONTRAST_V1_0\n")

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

sp<-script_path()
script_dir<-if(dir.exists(sp))sp else dirname(sp)
out_dir<-file.path(script_dir,"outputs")
dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)

b11<-safe_read_csv(file.path(script_dir,"input","2011_benchmark_replicates.csv"))
b21<-safe_read_csv(file.path(script_dir,"input","2021_benchmark_replicates.csv"))
d11<-safe_read_csv(file.path(script_dir,"input","2011_canonical_redwall_diagnostics.csv"))
d21<-safe_read_csv(file.path(script_dir,"input","2021_canonical_extended_diagnostics.csv"))
pc<-safe_read_csv(file.path(script_dir,"input","paired_draw_contract.csv"))

m<-merge(b11,b21,by=c("benchmark","draw"),suffixes=c("_2011","_2021"),sort=FALSE)
if(nrow(m)!=40000L) fail("Expected 40,000 paired benchmark draws.")
if(!all(pc$status=="PASS")) fail("Inherited paired-draw contract is not all PASS.")

# Primary temporal concentration measure is the manuscript's concentration
# definition: number of occupied socio-demographic neighbourhoods.
# Positive gain means greater concentration in 2021 (fewer occupied balls).
primary_obs <- as.numeric(d11$balls_with_label[1L]-d21$balls_with_label[1L])

# Secondary descriptive concentration measures are prespecified for
# triangulation, not as substitute primary outcomes.
secondary <- data.frame(
  metric=c("membership_entropy","max_label_share"),
  direction=c("2011_minus_2021","2021_minus_2011"),
  observed_gain=c(
    as.numeric(d11$membership_entropy[1L]-d21$membership_entropy[1L]),
    as.numeric(d21$max_label_share[1L]-d11$max_label_share[1L])
  ),
  stringsAsFactors=FALSE)

benchmarks<-unique(m$benchmark)
primary_rows<-vector("list",length(benchmarks))
for(i in seq_along(benchmarks)) {
  bn<-benchmarks[i]
  x<-m[m$benchmark==bn,,drop=FALSE]
  delta<-x$balls_with_label_2011-x$balls_with_label_2021
  primary_rows[[i]]<-data.frame(
    benchmark=bn,
    observed_concentration_gain_2021=primary_obs,
    paired_null_median=median(delta),
    paired_null_q05=as.numeric(quantile(delta,.05,names=FALSE)),
    paired_null_q95=as.numeric(quantile(delta,.95,names=FALSE)),
    paired_null_mean=mean(delta),
    upper_tail_p=(1+sum(delta>=primary_obs))/(1+length(delta)),
    percentile_rank=(sum(delta<primary_obs)+0.5*sum(delta==primary_obs))/length(delta),
    stringsAsFactors=FALSE)
}
primary<-do.call(rbind,primary_rows)
write_csv(primary,file.path(out_dir,"01_primary_occupied_ball_temporal_contrast.csv"))

secondary_rows<-list(); k<-0L
for(i in seq_len(nrow(secondary))) {
  metric<-secondary$metric[i]
  direction<-secondary$direction[i]
  obs<-secondary$observed_gain[i]
  for(bn in benchmarks) {
    x<-m[m$benchmark==bn,,drop=FALSE]
    if(direction=="2011_minus_2021") {
      delta<-x[[paste0(metric,"_2011")]]-x[[paste0(metric,"_2021")]]
    } else {
      delta<-x[[paste0(metric,"_2021")]]-x[[paste0(metric,"_2011")]]
    }
    k<-k+1L
    secondary_rows[[k]]<-data.frame(
      metric=metric,benchmark=bn,direction=direction,
      observed_concentration_gain_2021=obs,
      paired_null_median=median(delta,na.rm=TRUE),
      paired_null_q05=as.numeric(quantile(delta,.05,na.rm=TRUE,names=FALSE)),
      paired_null_q95=as.numeric(quantile(delta,.95,na.rm=TRUE,names=FALSE)),
      paired_null_mean=mean(delta,na.rm=TRUE),
      upper_tail_p=(1+sum(delta>=obs,na.rm=TRUE))/(1+sum(is.finite(delta))),
      stringsAsFactors=FALSE)
  }
}
secondary_out<-do.call(rbind,secondary_rows)
write_csv(secondary_out,file.path(out_dir,"02_secondary_temporal_contrasts.csv"))

# Descriptive before/after table.
descriptive<-data.frame(
  metric=c("balls_with_label","max_label_share","membership_entropy",
           "mean_nonlabel_share_among_unique_comembers",
           "occupied_ball_conductance_weighted"),
  value_2011=c(d11$balls_with_label,d11$max_label_share,d11$membership_entropy,
               d11$mean_nonlabel_share_among_unique_comembers,
               d11$occupied_ball_conductance_weighted),
  value_2021=c(d21$balls_with_label,d21$max_label_share,d21$membership_entropy,
               d21$mean_nonlabel_share_among_unique_comembers,
               d21$occupied_ball_conductance_weighted),
  stringsAsFactors=FALSE)
write_csv(descriptive,file.path(out_dir,"03_2011_2021_descriptive_change.csv"))

# Decision-oriented summary without automatically making a manuscript claim.
lines<-c(
  "# 2011–2021 temporal concentration contrast",
  "",
  "Primary measure: occupied Red Wall balls. Positive observed gain means that",
  "the same 41-seat label occupies fewer socio-demographic neighbourhoods in",
  "2021 than in 2011.",
  "",
  paste0("Observed occupied-ball change: 2011=",d11$balls_with_label[1L],
         ", 2021=",d21$balls_with_label[1L],
         ", concentration gain=",primary_obs," fewer occupied balls."),
  "",
  "## Paired benchmark tests",
  unlist(lapply(seq_len(nrow(primary)),function(i) {
    x<-primary[i,]
    paste0("- `",x$benchmark,"`: paired-null median=",x$paired_null_median,
           ", q05-q95=",x$paired_null_q05,"-",x$paired_null_q95,
           ", upper-tail p=",format(x$upper_tail_p,digits=5),".")
  })),
  "",
  "## Secondary measures",
  "Membership entropy and maximum within-ball prevalence are reported as",
  "triangulation checks. They are not substituted for the pre-specified",
  "occupied-ball concentration measure.",
  "",
  "## Interpretation rule",
  "Evidence for temporal concentration strengthening should be described as",
  "benchmark-dependent if the strict region × 2017 margin-quartile contrast",
  "does not place the observed change in an unusual tail."
)
writeLines(lines,file.path(out_dir,"TEMPORAL_CONTRAST_SUMMARY.md"))

gates<-data.frame(
  gate=c("paired_draws_40000","four_benchmarks","primary_metric_prespecified",
         "observed_2011_balls_14","observed_2021_balls_11"),
  status=c(if(nrow(m)==40000L)"PASS"else"FAIL",
           if(length(benchmarks)==4L)"PASS"else"FAIL",
           "PASS",
           if(d11$balls_with_label[1L]==14L)"PASS"else"FAIL",
           if(d21$balls_with_label[1L]==11L)"PASS"else"FAIL"),
  detail=c(paste0("paired rows=",nrow(m)),
           paste0("benchmarks=",length(benchmarks)),
           "Primary concentration metric is occupied balls.",
           paste0("2011=",d11$balls_with_label[1L]),
           paste0("2021=",d21$balls_with_label[1L])),
  stringsAsFactors=FALSE)
write_csv(gates,file.path(out_dir,"00_temporal_contrast_gates.csv"))

if(any(gates$status!="PASS")) fail("Temporal contrast gates failed.")

writeLines(c(
  "status=PASS",
  paste0("generated=",format(Sys.time(),"%Y-%m-%d %H:%M:%S %Z")),
  "stage=2011_2021_TEMPORAL_CONCENTRATION_CONTRAST",
  "paired_draws=40000",
  "primary_metric=balls_with_label",
  paste0("observed_gain=",primary_obs)
),file.path(out_dir,"RUN_STATUS.txt"))

cat("\nPASS: temporal concentration contrast completed.\n")
