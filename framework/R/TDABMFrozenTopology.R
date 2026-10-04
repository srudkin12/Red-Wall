# ==============================================================================
# TDABMFrozenTopology.R
# ==============================================================================
# Read-only utilities for memberships, recolouring and downstream validation of
# an already frozen topology. This module contains no topology construction.
# ==============================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

tdabm_ft_edge_table <- function(bm) {
  e <- as.matrix(bm$edges)
  if (length(e)==0L) return(data.frame(from=integer(),to=integer(),strength=integer(),stringsAsFactors=FALSE))
  s <- suppressWarnings(as.integer(bm$strength_of_edges))
  data.frame(from=as.integer(e[,1]),to=as.integer(e[,2]),strength=s,stringsAsFactors=FALSE)
}

tdabm_ft_topology_core <- function(bm) {
  list(
    vertices=bm$vertices,
    edges=bm$edges,
    strength_of_edges=bm$strength_of_edges,
    points_covered_by_landmarks=bm$points_covered_by_landmarks,
    landmarks=bm$landmarks,
    coverage=bm$coverage,
    epsilon=bm$epsilon
  )
}

tdabm_ft_topology_identical <- function(a,b) identical(tdabm_ft_topology_core(a),tdabm_ft_topology_core(b))

tdabm_ft_recolour <- function(bm, values) {
  values <- suppressWarnings(as.numeric(values))
  n_points <- length(bm$coverage)
  if (length(values)!=n_points || any(!is.finite(values))) stop("Recolouring values must be finite and match point count.",call.=FALSE)
  means <- vapply(bm$points_covered_by_landmarks,function(idx) mean(values[as.integer(idx)]),numeric(1))
  out <- bm
  out$coloring <- means
  if (!tdabm_ft_topology_identical(bm,out)) stop("Recolouring altered frozen topology.",call.=FALSE)
  list(object=out,ball_means=means)
}

tdabm_ft_validate_recoloured <- function(canonical, recoloured) {
  data.frame(
    check=c("topology_identical","radius_identical","ball_count_identical","colouring_length"),
    pass=c(
      tdabm_ft_topology_identical(canonical,recoloured),
      identical(canonical$epsilon,recoloured$epsilon),
      identical(length(canonical$points_covered_by_landmarks),length(recoloured$points_covered_by_landmarks)),
      length(recoloured$coloring)==length(canonical$points_covered_by_landmarks)
    ),
    stringsAsFactors=FALSE
  )
}
