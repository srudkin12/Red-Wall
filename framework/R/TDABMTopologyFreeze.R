# ==============================================================================
# TDABMTopologyFreeze.R
# ==============================================================================
# Explicit construction boundary for canonical fixed TDABM topology objects.
# Downstream recolouring/output modules must never source or call this builder.
# ==============================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

tdabm_tf_sha256 <- function(path) {
  if (!file.exists(path) || dir.exists(path)) stop("Cannot hash missing/non-file path: ", path, call.=FALSE)
  if (requireNamespace("digest", quietly=TRUE)) return(tolower(unname(digest::digest(path, algo="sha256", file=TRUE))))
  exe <- Sys.which("sha256sum")
  if (!nzchar(exe)) stop("SHA-256 requires digest or sha256sum.", call.=FALSE)
  out <- system2(exe, shQuote(path), stdout=TRUE, stderr=TRUE)
  if (length(out)!=1L) stop("Could not compute SHA-256: ", path, call.=FALSE)
  tolower(strsplit(out, "[[:space:]]+")[[1L]][[1L]])
}

tdabm_tf_write_csv_atomic <- function(x, path) {
  dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
  tmp <- paste0(path,".tmp-",Sys.getpid())
  utils::write.csv(x,tmp,row.names=FALSE,na="",fileEncoding="UTF-8")
  if (!file.rename(tmp,path)) { unlink(tmp); stop("Could not atomically write ",path,call.=FALSE) }
  invisible(path)
}

tdabm_tf_write_lines_atomic <- function(x, path) {
  dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
  tmp <- paste0(path,".tmp-",Sys.getpid())
  writeLines(enc2utf8(as.character(x)),tmp,useBytes=TRUE)
  if (!file.rename(tmp,path)) { unlink(tmp); stop("Could not atomically write ",path,call.=FALSE) }
  invisible(path)
}

tdabm_tf_save_rds_atomic <- function(x, path) {
  dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
  tmp <- paste0(path,".tmp-",Sys.getpid())
  saveRDS(x,tmp,version=3)
  if (!file.rename(tmp,path)) { unlink(tmp); stop("Could not atomically write ",path,call.=FALSE) }
  invisible(path)
}

tdabm_tf_compile_cpp <- function(cpp_path) {
  if (!requireNamespace("Rcpp", quietly=TRUE)) stop("Package 'Rcpp' is required.", call.=FALSE)
  if (!file.exists(cpp_path)) stop("BallMapper.cpp not found: ",cpp_path,call.=FALSE)
  Rcpp::sourceCpp(cpp_path, rebuild=FALSE, showOutput=FALSE, verbose=FALSE)
  if (!exists("SimplifiedBallMapperCppInterface", mode="function", inherits=TRUE)) stop("BallMapper C++ interface was not loaded.",call.=FALSE)
  invisible(TRUE)
}

tdabm_tf_canonicalize <- function(data, id_col, topology_axes) {
  data <- as.data.frame(data, stringsAsFactors=FALSE, check.names=FALSE)
  if (!id_col %in% names(data)) stop("Missing unit identifier: ",id_col,call.=FALSE)
  if (!all(topology_axes %in% names(data))) stop("Missing topology axis/axes.",call.=FALSE)
  ids <- enc2utf8(as.character(data[[id_col]]))
  if (length(ids)!=nrow(data) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) stop("Unit identifiers must be unique, nonmissing and nonblank.",call.=FALSE)
  ord <- order(ids, method="radix")
  out <- data[ord,,drop=FALSE]
  rownames(out) <- NULL
  axes <- out[,topology_axes,drop=FALSE]
  for (nm in topology_axes) axes[[nm]] <- suppressWarnings(as.numeric(axes[[nm]]))
  if (any(!is.finite(as.matrix(axes)))) stop("Canonical topology contains non-finite values.",call.=FALSE)
  list(data=out, ids=enc2utf8(as.character(out[[id_col]])), axes=axes, source_row_index=ord)
}

tdabm_tf_cpp_landmarks_one_based <- function(raw_landmarks, n_points) {
  n_points <- suppressWarnings(as.integer(n_points))
  if (length(n_points)!=1L || is.na(n_points) || n_points<1L) {
    stop("n_points must be one positive integer.", call.=FALSE)
  }
  raw <- suppressWarnings(as.integer(raw_landmarks))
  if (!length(raw) || anyNA(raw)) stop("Raw Ball Mapper landmarks are empty or missing.",call.=FALSE)
  # SimplifiedBallMapperCppInterface exposes C++ row positions directly for
  # landmarks (zero-based), unlike its one-based cover/edge outputs.
  if (raw[[1L]]!=0L || any(raw<0L | raw>=n_points)) {
    stop("Unexpected raw Ball Mapper landmark-index convention.",call.=FALSE)
  }
  out <- raw + 1L
  if (any(out<1L | out>n_points)) stop("Normalized landmark indices are invalid.",call.=FALSE)
  out
}

tdabm_tf_build <- function(axes, radius) {
  if (!exists("SimplifiedBallMapperCppInterface", mode="function", inherits=TRUE)) stop("Compile BallMapper.cpp before topology construction.",call.=FALSE)
  axes <- as.data.frame(axes,stringsAsFactors=FALSE)
  radius <- suppressWarnings(as.numeric(radius))
  if (length(radius)!=1L || !is.finite(radius) || radius<=0) stop("radius must be one positive finite value.",call.=FALSE)
  if (!nrow(axes) || !ncol(axes) || any(!is.finite(as.matrix(axes)))) stop("axes must be a non-empty finite rectangular table.",call.=FALSE)
  constant_values <- data.frame(interface_value=rep(1,nrow(axes)),stringsAsFactors=FALSE)
  bm <- SimplifiedBallMapperCppInterface(axes,constant_values,radius)
  if (!is.list(bm) || !is.list(bm$points_covered_by_landmarks) || !length(bm$points_covered_by_landmarks)) stop("Topology construction returned an invalid object.",call.=FALSE)
  bm$landmarks <- tdabm_tf_cpp_landmarks_one_based(bm$landmarks, nrow(axes))
  bm$tdabm_landmark_index_contract <- "R_ONE_BASED_NORMALIZED_FROM_CPP_ZERO_BASED"
  bm$epsilon <- radius
  bm
}

tdabm_tf_edge_table <- function(bm) {
  e <- as.matrix(bm$edges)
  if (length(e)==0L) return(data.frame(from=integer(),to=integer(),strength=integer(),stringsAsFactors=FALSE))
  if (ncol(e)!=2L) stop("Edge matrix must have two columns.",call.=FALSE)
  strength <- suppressWarnings(as.integer(bm$strength_of_edges))
  if (length(strength)!=nrow(e)) stop("Edge-strength vector is inconsistent with edges.",call.=FALSE)
  out <- data.frame(from=as.integer(e[,1]),to=as.integer(e[,2]),strength=strength,stringsAsFactors=FALSE)
  out <- out[order(out$from,out$to,method="radix"),,drop=FALSE]; rownames(out)<-NULL; out
}

tdabm_tf_membership_table <- function(bm, unit_ids, unit_labels=NULL) {
  unit_ids <- enc2utf8(as.character(unit_ids))
  if (is.null(unit_labels)) unit_labels <- unit_ids
  unit_labels <- enc2utf8(as.character(unit_labels))
  if (length(unit_ids)!=length(unit_labels)) stop("unit_ids and unit_labels must have equal length.",call.=FALSE)
  rows <- lapply(seq_along(bm$points_covered_by_landmarks), function(ball_id) {
    idx <- sort(unique(as.integer(bm$points_covered_by_landmarks[[ball_id]])))
    if (!length(idx) || any(idx<1L | idx>length(unit_ids))) stop("Membership index outside point order.",call.=FALSE)
    data.frame(ball_id=as.integer(ball_id),point_index=idx,unit_id=unit_ids[idx],unit_label=unit_labels[idx],stringsAsFactors=FALSE)
  })
  out <- do.call(rbind,rows); rownames(out)<-NULL; out
}

tdabm_tf_landmark_table <- function(bm, unit_ids, unit_labels=NULL) {
  if (is.null(unit_labels)) unit_labels <- unit_ids
  idx <- as.integer(bm$landmarks)
  if (length(idx)!=length(bm$points_covered_by_landmarks) || any(idx<1L | idx>length(unit_ids))) stop("Invalid landmark indices.",call.=FALSE)
  data.frame(ball_id=seq_along(idx),point_index=idx,unit_id=as.character(unit_ids[idx]),unit_label=as.character(unit_labels[idx]),stringsAsFactors=FALSE)
}

tdabm_tf_vertex_table <- function(bm) {
  v <- as.matrix(bm$vertices)
  if (length(v)==0L || ncol(v)<2L) stop("Invalid vertices table.",call.=FALSE)
  data.frame(ball_id=as.integer(v[,1]),ball_size=as.integer(v[,2])-2L,stringsAsFactors=FALSE)
}

tdabm_tf_validate_object <- function(bm, n_points, expected_radius=NULL) {
  rows <- data.frame(check=character(),pass=logical(),detail=character(),stringsAsFactors=FALSE)
  add <- function(c,p,d) rows <<- rbind(rows,data.frame(check=c,pass=isTRUE(p),detail=d,stringsAsFactors=FALSE))
  ok_list <- is.list(bm)
  add("object_is_list",ok_list,"object must be a list")
  if (!ok_list) return(rows)
  cover <- bm$points_covered_by_landmarks %||% NULL
  add("cover_nonempty",is.list(cover) && length(cover)>0L,"points_covered_by_landmarks must be a nonempty list")
  if (!is.list(cover) || !length(cover)) return(rows)
  idx <- suppressWarnings(as.integer(unlist(cover,use.names=FALSE)))
  add("membership_indices_valid",length(idx)>0L && all(is.finite(idx)) && all(idx>=1L & idx<=n_points),"all membership indices must reference the canonical point order")
  covered <- sort(unique(idx))
  add("all_points_covered",identical(covered,seq_len(as.integer(n_points))),"every point must belong to at least one ball")
  landmarks <- suppressWarnings(as.integer(bm$landmarks %||% integer()))
  add("landmark_count",length(landmarks)==length(cover),"one landmark is required for each ball")
  add("landmark_indices_valid",length(landmarks)==length(cover) && !anyNA(landmarks) && all(landmarks>=1L & landmarks<=n_points),"landmarks must use the normalized one-based canonical point order")
  add("landmark_index_contract",identical(bm$tdabm_landmark_index_contract %||% NA_character_,"R_ONE_BASED_NORMALIZED_FROM_CPP_ZERO_BASED"),"explicit zero-based C++ to one-based R normalization must be recorded")
  add("coloring_length",length(bm$coloring)==length(cover),"colouring length must equal number of balls")
  if (!is.null(expected_radius)) {
    observed <- suppressWarnings(as.numeric(bm$epsilon %||% NA_real_)); expected <- as.numeric(expected_radius)
    add("radius",length(observed)==1L && is.finite(observed) && abs(observed-expected)<=1e-8,paste0("observed=",observed," expected=",expected))
  }
  rows
}

tdabm_tf_topology_lines <- function(bm, unit_ids) {
  unit_ids <- as.character(unit_ids)
  lm <- tdabm_tf_landmark_table(bm,unit_ids,unit_ids)
  mem <- tdabm_tf_membership_table(bm,unit_ids,unit_ids)
  edges <- tdabm_tf_edge_table(bm)
  lines <- c(
    paste0("RADIUS\t",format(as.numeric(bm$epsilon),digits=17)),
    "LANDMARK_INDEX_BASE\t1",
    "LANDMARK_SOURCE_INDEX_BASE\t0"
  )
  for (i in seq_len(nrow(lm))) lines <- c(lines,paste0("LANDMARK\t",lm$ball_id[i],"\t",lm$unit_id[i]))
  split_ids <- split(mem$unit_id,mem$ball_id)
  for (i in seq_along(split_ids)) lines <- c(lines,paste0("BALL\t",i,"\t",paste(sort(split_ids[[i]],method="radix"),collapse=";")))
  if (nrow(edges)) for (i in seq_len(nrow(edges))) lines <- c(lines,paste0("EDGE\t",edges$from[i],"\t",edges$to[i],"\t",edges$strength[i]))
  enc2utf8(lines)
}

tdabm_tf_topology_fingerprint <- function(bm, unit_ids) {
  tf <- tempfile(fileext=".txt")
  on.exit(unlink(tf),add=TRUE)
  writeLines(tdabm_tf_topology_lines(bm,unit_ids),tf,useBytes=TRUE)
  tdabm_tf_sha256(tf)
}
