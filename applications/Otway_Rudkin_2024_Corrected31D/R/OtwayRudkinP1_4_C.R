# Otway-Rudkin P1.4-C application-level helper.
# Uses only an already constructed canonical BM object; it never reconstructs topology.

or24_c_add_check <- function(rows, check, pass, detail) {
  rbind(rows, data.frame(
    check=as.character(check),
    pass=isTRUE(pass),
    detail=as.character(detail),
    stringsAsFactors=FALSE
  ))
}

or24_c_scale_axes <- function(axes, scaling) {
  out <- as.data.frame(axes, stringsAsFactors=FALSE, check.names=FALSE)
  if (scaling %in% c("z_score","standard")) {
    for (nm in names(out)) {
      x <- as.numeric(out[[nm]])
      s <- stats::sd(x)
      if (!is.finite(s) || s <= 0) {
        stop("Topology variable has unusable variation under z-score scaling: ", nm, call.=FALSE)
      }
      out[[nm]] <- as.numeric((x - mean(x)) / s)
    }
  } else if (!scaling %in% c("as_supplied","none","raw_percentage")) {
    stop("Unsupported P1.4-C scaling: ", scaling, call.=FALSE)
  }
  out
}

or24_c_graph_components <- function(n_balls, edges) {
  n_balls <- as.integer(n_balls)
  if (length(n_balls)!=1L || is.na(n_balls) || n_balls < 1L) {
    stop("n_balls must be a positive integer.", call.=FALSE)
  }
  adj <- vector("list", n_balls)
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      a <- as.integer(edges$from[[i]])
      b <- as.integer(edges$to[[i]])
      adj[[a]] <- c(adj[[a]], b)
      adj[[b]] <- c(adj[[b]], a)
    }
  }

  component <- integer(n_balls)
  cid <- 0L
  for (start in seq_len(n_balls)) {
    if (component[[start]] != 0L) next
    cid <- cid + 1L
    queue <- start
    component[[start]] <- cid
    while (length(queue)) {
      v <- queue[[1L]]
      queue <- queue[-1L]
      for (w in adj[[v]]) {
        if (component[[w]] == 0L) {
          component[[w]] <- cid
          queue <- c(queue, w)
        }
      }
    }
  }
  component
}

or24_c_component_tables <- function(bm, membership, edges, n_points) {
  n_balls <- length(bm$points_covered_by_landmarks)
  comp <- or24_c_graph_components(n_balls, edges)
  degree <- integer(n_balls)
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      degree[[edges$from[[i]]]] <- degree[[edges$from[[i]]]] + 1L
      degree[[edges$to[[i]]]] <- degree[[edges$to[[i]]]] + 1L
    }
  }

  ball_sizes <- tabulate(membership$ball_id, nbins=n_balls)
  ball <- data.frame(
    ball_id=seq_len(n_balls),
    component_id=comp,
    ball_size=as.integer(ball_sizes),
    degree=as.integer(degree),
    isolated_ball=degree==0L,
    stringsAsFactors=FALSE
  )

  component_ids <- sort(unique(comp))
  comp_rows <- lapply(component_ids, function(k) {
    balls <- ball$ball_id[ball$component_id == k]
    pts <- sort(unique(membership$point_index[membership$ball_id %in% balls]))
    data.frame(
      component_id=as.integer(k),
      n_balls=length(balls),
      n_unique_points=length(pts),
      share_points=length(pts)/n_points,
      isolated_component=(length(balls)==1L && ball$isolated_ball[ball$ball_id==balls[[1L]]]),
      stringsAsFactors=FALSE
    )
  })
  list(ball=ball, component=do.call(rbind, comp_rows))
}

or24_c_topology_summary <- function(
  bm, membership, edges, component_tables, n_points, n_dimensions,
  topology_fingerprint
) {
  ball_sizes <- tabulate(membership$ball_id, nbins=length(bm$points_covered_by_landmarks))
  point_counts <- tabulate(membership$point_index, nbins=n_points)
  data.frame(
    radius=as.numeric(bm$epsilon),
    n_points=as.integer(n_points),
    n_dimensions=as.integer(n_dimensions),
    n_balls=length(ball_sizes),
    n_edges=nrow(edges),
    n_components=nrow(component_tables$component),
    n_isolated_balls=sum(component_tables$ball$isolated_ball),
    largest_ball_size=max(ball_sizes),
    largest_ball_share_observations=max(ball_sizes)/n_points,
    mean_ball_size=mean(ball_sizes),
    mean_memberships_per_observation=mean(point_counts),
    fraction_multicovered=mean(point_counts > 1L),
    topology_fingerprint_sha256=as.character(topology_fingerprint),
    landmark_index_contract=as.character(bm$tdabm_landmark_index_contract),
    stringsAsFactors=FALSE
  )
}

or24_c_validate_duality <- function(bm, membership, n_points) {
  n_balls <- length(bm$points_covered_by_landmarks)
  from_membership <- vector("list", n_points)
  for (i in seq_len(n_points)) {
    from_membership[[i]] <- sort(unique(as.integer(
      membership$ball_id[membership$point_index == i]
    )))
  }
  coverage <- lapply(bm$coverage, function(x) sort(unique(as.integer(x))))
  identical(coverage, from_membership) &&
    all(vapply(coverage, function(x) length(x)>0L && all(x>=1L & x<=n_balls), logical(1)))
}

or24_c_validate_edges <- function(bm, edges) {
  n_balls <- length(bm$points_covered_by_landmarks)
  if (!nrow(edges)) return(TRUE)
  endpoints <- all(edges$from>=1L & edges$from<=n_balls & edges$to>=1L & edges$to<=n_balls)
  no_self <- all(edges$from != edges$to)
  no_dup <- !anyDuplicated(edges[,c("from","to"),drop=FALSE])
  positive <- all(edges$strength > 0L)
  overlap_ok <- all(vapply(seq_len(nrow(edges)), function(i) {
    a <- edges$from[[i]]
    b <- edges$to[[i]]
    overlap <- length(intersect(
      as.integer(bm$points_covered_by_landmarks[[a]]),
      as.integer(bm$points_covered_by_landmarks[[b]])
    ))
    identical(as.integer(overlap), as.integer(edges$strength[[i]]))
  }, logical(1)))
  endpoints && no_self && no_dup && positive && overlap_ok
}

or24_c_write_hash_manifest <- function(files, root, output_file) {
  files <- normalizePath(files, winslash="/", mustWork=TRUE)
  root <- normalizePath(root, winslash="/", mustWork=TRUE)
  tab <- data.frame(
    artifact=basename(files),
    relative_path=substring(files,nchar(root)+2L),
    sha256=vapply(files, tdabm_tf_sha256, character(1)),
    bytes=as.numeric(file.info(files)$size),
    stringsAsFactors=FALSE
  )
  tdabm_tf_write_csv_atomic(tab, output_file)
  tab
}
