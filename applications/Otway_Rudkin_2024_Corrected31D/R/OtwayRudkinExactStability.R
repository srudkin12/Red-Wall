# Exact accelerated stability calculation for Otway-Rudkin P1.4-B.
# Scientific definition is unchanged: all pairwise Jaccards are retained.

or24_compile_exact_stability <- function(cpp_path) {
  if (!requireNamespace("Rcpp", quietly=TRUE)) {
    stop("Package 'Rcpp' is required for exact accelerated stability.", call.=FALSE)
  }
  cpp_path<-normalizePath(cpp_path,winslash="/",mustWork=TRUE)
  Rcpp::sourceCpp(cpp_path,rebuild=FALSE,verbose=FALSE,showOutput=FALSE)
  if (!exists("or24_exact_stability_openmp_enabled",mode="function",inherits=TRUE) ||
      !isTRUE(or24_exact_stability_openmp_enabled())) {
    stop("Exact stability helper compiled without OpenMP; refusing unintended serial production.",call.=FALSE)
  }
  invisible(TRUE)
}

or24_pairwise_stability_exact_fast <- function(
  replicates,radius_grid,n_observations,nthreads=1L,progress_path=NULL
) {
  if(!is.list(replicates) || length(replicates)<2L) stop("At least two replicate results are required.",call.=FALSE)
  nthreads<-tdabm_rd_assert_scalar_integer(nthreads,"nthreads",minimum=1L)
  n_observations<-tdabm_rd_assert_scalar_integer(n_observations,"n_observations",minimum=2L)

  cover_bits<-choose(n_observations,2L)
  landmark_bits<-n_observations
  rows<-vector("list",nrow(radius_grid))
  nreps<-length(replicates)
  expected_pairs<-choose(nreps,2L)
  started<-Sys.time()

  for(k in seq_len(nrow(radius_grid))){
    cover_signatures<-lapply(replicates,function(x) x$cover_signatures[[k]])
    landmark_signatures<-lapply(replicates,function(x) x$landmark_signatures[[k]])

    scores<-or24_exact_packed_pairwise_scores(
      cover_signatures,landmark_signatures,
      as.integer(cover_bits),as.integer(landmark_bits),as.integer(nthreads)
    )
    if(length(scores$cover)!=expected_pairs || length(scores$landmark)!=expected_pairs){
      stop("Exact stability engine returned the wrong number of pairwise scores.",call.=FALSE)
    }

    rows[[k]]<-data.frame(
      radius_index=as.integer(radius_grid$radius_index[[k]]),
      radius=as.numeric(radius_grid$radius[[k]]),
      pairwise_comparisons=length(scores$cover),
      mean_pairwise_cover_jaccard=mean(scores$cover),
      q10_pairwise_cover_jaccard=as.numeric(stats::quantile(scores$cover,0.10,names=FALSE,type=7)),
      median_pairwise_cover_jaccard=stats::median(scores$cover),
      min_pairwise_cover_jaccard=min(scores$cover),
      mean_pairwise_landmark_jaccard=mean(scores$landmark),
      median_pairwise_landmark_jaccard=stats::median(scores$landmark),
      min_pairwise_landmark_jaccard=min(scores$landmark),
      stringsAsFactors=FALSE
    )

    if(!is.null(progress_path)){
      elapsed<-as.numeric(difftime(Sys.time(),started,units="mins"))
      writeLines(c(
        "stage=EXACT_PARALLEL_PAIRWISE_STABILITY",
        paste0("completed_radius=",k),
        paste0("total_radii=",nrow(radius_grid)),
        paste0("radius=",format(radius_grid$radius[[k]],nsmall=2L)),
        paste0("pairwise_comparisons_per_radius=",expected_pairs),
        paste0("openmp_threads=",nthreads),
        paste0("elapsed_minutes=",format(elapsed,digits=8)),
        paste0("updated=",format(Sys.time(),"%Y-%m-%d %H:%M:%S %z"))
      ),progress_path,useBytes=TRUE)
    }

    rm(scores,cover_signatures,landmark_signatures)
    if((k %% 10L)==0L) gc(verbose=FALSE)
  }

  out<-do.call(rbind,rows)
  row.names(out)<-NULL
  out
}

or24_validate_exact_stability <- function(
  replicates,radius_grid,n_observations,nthreads=1L,tolerance=1e-14
) {
  if(length(replicates)<4L) stop("Need at least four replicates for stability validation.",call.=FALSE)
  idx<-unique(as.integer(c(1L,max(1L,round(nrow(radius_grid)/2)),nrow(radius_grid))))

  test_reps<-lapply(replicates[seq_len(4L)],function(x){
    y<-x
    y$cover_signatures<-x$cover_signatures[idx]
    y$landmark_signatures<-x$landmark_signatures[idx]
    y
  })
  test_grid<-radius_grid[idx,,drop=FALSE]

  reference<-tdabm_rd_pairwise_stability(test_reps,test_grid,n_observations)
  accelerated<-or24_pairwise_stability_exact_fast(
    test_reps,test_grid,n_observations,
    nthreads=min(as.integer(nthreads),4L),progress_path=NULL
  )

  numeric_cols<-names(reference)[vapply(reference,is.numeric,logical(1))]
  audit<-do.call(rbind,lapply(numeric_cols,function(nm){
    d<-max(abs(as.numeric(reference[[nm]])-as.numeric(accelerated[[nm]])))
    data.frame(field=nm,max_abs_difference=d,tolerance=tolerance,
               pass=is.finite(d) && d<=tolerance,stringsAsFactors=FALSE)
  }))
  if(!all(audit$pass)){
    stop("Exact accelerated stability failed regression validation against original implementation.",call.=FALSE)
  }
  audit
}
