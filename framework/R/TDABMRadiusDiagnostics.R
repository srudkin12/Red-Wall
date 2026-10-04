# ==============================================================================
# TDABMRadiusDiagnostics.R
# ==============================================================================
# Application-neutral topology-only radius diagnostics introduced for P1.4-B.
#
# Scope:
#   * validate one frozen rectangular topology input;
#   * derive a broad radius grid from Euclidean geometry only;
#   * run repeated landmark-order Ball Mapper diagnostics with deterministic seeds;
#   * summarise graph, cover, membership, and repeated-order stability diagnostics;
#   * identify descriptive topology transitions and a non-binding resolution reference;
#   * separate blocking technical/integrity failures from non-binding diagnostic warnings;
#   * never use outcome/colour variables to define a radius;
#   * never use connectedness as an admissibility rule for radius choice;
#   * never approve, freeze, or label a radius as optimal.
#
# Explicit non-scope:
#   * no BMStats/BMStatsMembership execution;
#   * no application-specific labels or thresholds;
#   * no substantive observation exclusion;
#   * no canonical BallMapper-object freeze;
#   * no paper/manuscript production.
#
# The C++ interface requires a one-column value frame. A constant vector of ones
# is used solely to satisfy that interface; it cannot affect topology.
# ============================================================================== 

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

tdabm_rd_assert_scalar_character <- function(x, name, allow_blank = FALSE) {
  if (length(x) != 1L || is.na(x) || !is.character(x)) {
    stop(name, " must be one non-missing character value.", call. = FALSE)
  }
  value <- as.character(x)
  if (!isTRUE(allow_blank) && !nzchar(trimws(value))) {
    stop(name, " must not be blank.", call. = FALSE)
  }
  value
}

tdabm_rd_assert_scalar_integer <- function(x, name, minimum = NULL, maximum = NULL) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || is.na(value) || !is.finite(value) || abs(value - round(value)) > 1e-8) {
    stop(name, " must be one finite integer-like value.", call. = FALSE)
  }
  value <- as.integer(round(value))
  if (!is.null(minimum) && value < minimum) stop(name, " is below its minimum.", call. = FALSE)
  if (!is.null(maximum) && value > maximum) stop(name, " is above its maximum.", call. = FALSE)
  value
}

tdabm_rd_assert_scalar_numeric <- function(x, name, minimum = NULL, maximum = NULL) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || is.na(value) || !is.finite(value)) {
    stop(name, " must be one finite numeric value.", call. = FALSE)
  }
  value <- as.numeric(value)
  if (!is.null(minimum) && value < minimum) stop(name, " is below its minimum.", call. = FALSE)
  if (!is.null(maximum) && value > maximum) stop(name, " is above its maximum.", call. = FALSE)
  value
}

tdabm_rd_validate_topology_frame <- function(
  data,
  id_col,
  topology_variables,
  scaling = "as_supplied",
  metric = "euclidean",
  expected_rows = NULL,
  stop_on_failure = TRUE
) {
  if (!is.data.frame(data)) stop("data must be a data.frame-like object.", call. = FALSE)
  id_col <- tdabm_rd_assert_scalar_character(id_col, "id_col")
  topology_variables <- as.character(topology_variables)
  if (!length(topology_variables) || any(is.na(topology_variables) | !nzchar(topology_variables))) {
    stop("topology_variables must contain at least one nonblank variable name.", call. = FALSE)
  }
  if (anyDuplicated(topology_variables)) stop("topology_variables contains duplicates.", call. = FALSE)
  scaling <- tdabm_rd_assert_scalar_character(scaling, "scaling")
  metric <- tolower(tdabm_rd_assert_scalar_character(metric, "metric"))

  missing <- setdiff(c(id_col, topology_variables), names(data))
  checks <- data.frame(
    check = character(0),
    pass = logical(0),
    detail = character(0),
    stringsAsFactors = FALSE
  )
  add <- function(check, pass, detail) {
    checks <<- rbind(checks, data.frame(check = check, pass = isTRUE(pass), detail = as.character(detail), stringsAsFactors = FALSE))
  }

  add("required_columns", length(missing) == 0L, if (length(missing)) paste(missing, collapse = "; ") else "all present")
  if (length(missing) == 0L) {
    ids <- as.character(data[[id_col]])
    add("id_nonmissing_nonblank", !any(is.na(ids) | !nzchar(trimws(ids))), "stable identifiers required")
    add("id_unique", !anyDuplicated(ids), "stable identifiers must be unique")
  }

  if (!is.null(expected_rows)) {
    expected_rows <- tdabm_rd_assert_scalar_integer(expected_rows, "expected_rows", minimum = 2L)
    add("expected_row_count", nrow(data) == expected_rows, paste0(nrow(data), " observed; ", expected_rows, " expected"))
  } else {
    add("minimum_row_count", nrow(data) >= 2L, paste0(nrow(data), " observed"))
  }

  if (length(missing) == 0L) {
    numeric_ok <- vapply(data[topology_variables], is.numeric, logical(1))
    add("topology_numeric", all(numeric_ok), paste(names(numeric_ok)[!numeric_ok], collapse = "; "))
    if (all(numeric_ok)) {
      finite_ok <- vapply(data[topology_variables], function(x) !anyNA(x) && all(is.finite(x)), logical(1))
      add("topology_finite", all(finite_ok), paste(names(finite_ok)[!finite_ok], collapse = "; "))
      varying <- vapply(data[topology_variables], function(x) length(unique(as.numeric(x))) > 1L, logical(1))
      add("topology_nonconstant", all(varying), paste(names(varying)[!varying], collapse = "; "))
    }
  }

  supported_scaling <- scaling %in% c("as_supplied", "none", "raw_percentage", "z_score", "standard")
  add("supported_scaling", supported_scaling, scaling)
  add("supported_metric", identical(metric, "euclidean"), metric)

  pass <- nrow(checks) > 0L && all(checks$pass %in% TRUE)
  if (!pass && isTRUE(stop_on_failure)) {
    failed <- checks[!checks$pass, , drop = FALSE]
    stop("Topology-input contract failed: ", paste(paste0(failed$check, "=", failed$detail), collapse = " | "), call. = FALSE)
  }

  list(pass = pass, report = checks)
}

tdabm_rd_prepare_topology <- function(data, id_col, topology_variables, scaling = "as_supplied", metric = "euclidean") {
  validation <- tdabm_rd_validate_topology_frame(
    data = data,
    id_col = id_col,
    topology_variables = topology_variables,
    scaling = scaling,
    metric = metric,
    stop_on_failure = TRUE
  )
  ids <- enc2utf8(as.character(data[[id_col]]))
  ord <- order(ids, method = "radix")
  ids <- ids[ord]
  axes <- as.data.frame(data[ord, topology_variables, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE)

  if (scaling %in% c("z_score", "standard")) {
    for (nm in topology_variables) {
      s <- stats::sd(axes[[nm]])
      if (!is.finite(s) || s <= 0) stop("Topology variable has no usable variation under z-score scaling: ", nm, call. = FALSE)
      axes[[nm]] <- as.numeric((axes[[nm]] - mean(axes[[nm]])) / s)
    }
  }

  list(
    ids = ids,
    axes = axes,
    order = ord,
    validation = validation,
    scaling = scaling,
    metric = tolower(metric)
  )
}

tdabm_rd_distance_diagnostics <- function(axes) {
  if (!is.data.frame(axes) || nrow(axes) < 2L || ncol(axes) < 1L) {
    stop("axes must contain at least two observations and one topology dimension.", call. = FALSE)
  }
  dmat <- as.matrix(stats::dist(axes, method = "euclidean", upper = TRUE, diag = TRUE))
  n <- nrow(dmat)
  diag(dmat) <- Inf
  nearest <- apply(dmat, 1L, min)
  diag(dmat) <- 0
  pairwise <- dmat[upper.tri(dmat)]
  positive <- pairwise[is.finite(pairwise) & pairwise > 0]
  if (!length(positive)) stop("All topology observations are coincident; a radius grid cannot be derived.", call. = FALSE)
  nn_positive <- nearest[is.finite(nearest) & nearest > 0]
  if (!length(nn_positive)) nn_positive <- positive

  q <- function(x, p) as.numeric(stats::quantile(x, probs = p, names = FALSE, type = 7, na.rm = FALSE))
  summary <- data.frame(
    diagnostic = c(
      "n_observations", "n_dimensions", "duplicate_pair_count",
      "minimum_positive_pairwise_distance", "maximum_pairwise_distance",
      "nearest_neighbor_min", "nearest_neighbor_q05", "nearest_neighbor_q25",
      "nearest_neighbor_q50", "nearest_neighbor_q75", "nearest_neighbor_q95", "nearest_neighbor_max",
      "pairwise_q01", "pairwise_q05", "pairwise_q10", "pairwise_q25", "pairwise_q50",
      "pairwise_q75", "pairwise_q90", "pairwise_q95", "pairwise_q99"
    ),
    value = c(
      nrow(axes), ncol(axes), sum(pairwise == 0),
      min(positive), max(pairwise),
      min(nn_positive), q(nn_positive, 0.05), q(nn_positive, 0.25),
      q(nn_positive, 0.50), q(nn_positive, 0.75), q(nn_positive, 0.95), max(nn_positive),
      q(pairwise, 0.01), q(pairwise, 0.05), q(pairwise, 0.10), q(pairwise, 0.25), q(pairwise, 0.50),
      q(pairwise, 0.75), q(pairwise, 0.90), q(pairwise, 0.95), q(pairwise, 0.99)
    ),
    stringsAsFactors = FALSE
  )

  list(distance_matrix = dmat, nearest_neighbor = nearest, pairwise = pairwise, summary = summary)
}

tdabm_rd_make_radius_grid <- function(axes, grid_points = 61L, lower_multiplier = 0.75, upper_multiplier = 1.000000001) {
  grid_points <- tdabm_rd_assert_scalar_integer(grid_points, "grid_points", minimum = 7L)
  lower_multiplier <- tdabm_rd_assert_scalar_numeric(lower_multiplier, "lower_multiplier", minimum = .Machine$double.eps)
  upper_multiplier <- tdabm_rd_assert_scalar_numeric(upper_multiplier, "upper_multiplier", minimum = 1)
  dd <- tdabm_rd_distance_diagnostics(axes)
  positive <- dd$pairwise[is.finite(dd$pairwise) & dd$pairwise > 0]
  lower <- lower_multiplier * min(positive)
  upper <- upper_multiplier * max(dd$pairwise)
  if (!is.finite(lower) || lower <= 0 || !is.finite(upper) || upper <= lower) {
    stop("Could not construct a positive nondegenerate radius span.", call. = FALSE)
  }
  radii <- exp(seq(log(lower), log(upper), length.out = grid_points))
  radii[[1L]] <- lower
  radii[[length(radii)]] <- upper
  data.frame(
    radius_index = seq_along(radii),
    radius = as.numeric(radii),
    grid_role = c("sparse_anchor", rep("geometric_span", max(0L, length(radii) - 2L)), "collapse_anchor"),
    stringsAsFactors = FALSE
  )
}

tdabm_rd_compile_cpp <- function(cpp_path) {
  cpp_path <- normalizePath(cpp_path, winslash = "/", mustWork = TRUE)
  if (!requireNamespace("Rcpp", quietly = TRUE)) {
    stop("Package 'Rcpp' is required for topology-only Ball Mapper diagnostics.", call. = FALSE)
  }
  Rcpp::sourceCpp(cpp_path, rebuild = FALSE, verbose = FALSE, showOutput = FALSE)
  if (!exists("SimplifiedBallMapperCppInterface", mode = "function", inherits = TRUE)) {
    stop("Compiled BallMapper.cpp does not expose SimplifiedBallMapperCppInterface().", call. = FALSE)
  }
  invisible(TRUE)
}

tdabm_rd_graph_components <- function(n_vertices, edges) {
  n_vertices <- tdabm_rd_assert_scalar_integer(n_vertices, "n_vertices", minimum = 1L)
  if (is.null(edges)) edges <- matrix(numeric(0), ncol = 2L)
  edges <- as.matrix(edges)
  if (length(edges) == 0L) edges <- matrix(numeric(0), ncol = 2L)
  if (ncol(edges) != 2L) stop("Ball Mapper edge table must have exactly two columns.", call. = FALSE)

  adjacency <- vector("list", n_vertices)
  degree <- integer(n_vertices)
  if (nrow(edges) > 0L) {
    from <- as.integer(round(edges[, 1L]))
    to <- as.integer(round(edges[, 2L]))
    if (any(!is.finite(edges)) || any(from < 1L | from > n_vertices | to < 1L | to > n_vertices)) {
      stop("Ball Mapper edge table contains an invalid vertex identifier.", call. = FALSE)
    }
    if (any(from == to)) stop("Ball Mapper edge table contains a self-edge.", call. = FALSE)
    for (i in seq_len(nrow(edges))) {
      a <- from[[i]]
      b <- to[[i]]
      adjacency[[a]] <- c(adjacency[[a]], b)
      adjacency[[b]] <- c(adjacency[[b]], a)
      degree[[a]] <- degree[[a]] + 1L
      degree[[b]] <- degree[[b]] + 1L
    }
  }

  visited <- rep(FALSE, n_vertices)
  component_sizes <- integer(0)
  for (v in seq_len(n_vertices)) {
    if (visited[[v]]) next
    queue <- v
    visited[[v]] <- TRUE
    size <- 0L
    while (length(queue)) {
      current <- queue[[1L]]
      if (length(queue) == 1L) queue <- integer(0) else queue <- queue[-1L]
      size <- size + 1L
      neighbours <- adjacency[[current]]
      if (length(neighbours)) {
        new <- neighbours[!visited[neighbours]]
        if (length(new)) {
          visited[new] <- TRUE
          queue <- c(queue, new)
        }
      }
    }
    component_sizes <- c(component_sizes, size)
  }

  list(
    degree = degree,
    component_sizes = component_sizes,
    n_components = length(component_sizes),
    n_nontrivial_components = sum(component_sizes > 1L),
    largest_component_size = max(component_sizes),
    n_isolated_vertices = sum(degree == 0L)
  )
}

tdabm_rd_ball_resolution_metrics <- function(cover, n_observations) {
  n_observations <- tdabm_rd_assert_scalar_integer(n_observations, "n_observations", minimum = 2L)
  if (!is.list(cover) || length(cover) < 1L) stop("cover must contain at least one nonempty ball.", call. = FALSE)
  sizes <- vapply(cover, length, integer(1))
  if (any(sizes < 1L)) stop("cover contains an empty ball.", call. = FALSE)
  if (any(sizes > n_observations)) stop("A ball cannot contain more observations than the sample.", call. = FALSE)
  mass <- sum(sizes)
  if (!is.finite(mass) || mass <= 0) stop("Ball membership mass is invalid.", call. = FALSE)
  weights <- sizes / mass
  entropy <- -sum(weights * log(weights))
  effective <- exp(entropy)
  n_balls <- length(sizes)
  normalized_entropy <- if (n_balls > 1L) entropy / log(n_balls) else 0
  if (!is.finite(normalized_entropy)) normalized_entropy <- 0
  list(
    effective_n_balls = as.numeric(effective),
    effective_ball_fraction = as.numeric(effective / n_balls),
    normalized_ball_size_entropy = as.numeric(normalized_entropy),
    max_ball_share_observations = as.numeric(max(sizes) / n_observations),
    majority_ball = max(sizes) / n_observations >= 0.50,
    severe_dominance_ball = max(sizes) / n_observations >= 0.75
  )
}

tdabm_rd_pack_signature <- function(bits) {
  if (!is.logical(bits)) stop("bits must be a logical vector.", call. = FALSE)
  if (length(bits) < 1L) stop("bits must contain at least one logical value.", call. = FALSE)
  if (anyNA(bits)) stop("bits must not contain missing values.", call. = FALSE)

  # base::packBits(..., type = "raw") requires a bit vector whose length is
  # an exact multiple of eight. Topology signatures generally are not: for
  # example choose(100, 2) = 4950 cover bits and 100 landmark bits. Pad only
  # the storage representation with FALSE bits; the declared n_bits controls
  # unpacking and therefore preserves the exact logical signature.
  padding <- (8L - (length(bits) %% 8L)) %% 8L
  if (padding > 0L) bits <- c(bits, rep(FALSE, padding))

  base::packBits(as.raw(as.integer(bits)), type = "raw")
}

tdabm_rd_cover_signature <- function(cover, n_observations) {
  n_observations <- tdabm_rd_assert_scalar_integer(n_observations, "n_observations", minimum = 2L)
  shared <- matrix(FALSE, nrow = n_observations, ncol = n_observations)
  for (idx in cover) {
    idx <- unique(as.integer(idx))
    if (length(idx) >= 2L) shared[idx, idx] <- TRUE
  }
  bits <- shared[upper.tri(shared)]
  tdabm_rd_pack_signature(bits)
}

tdabm_rd_landmark_signature <- function(landmarks, n_observations) {
  flags <- rep(FALSE, n_observations)
  landmarks <- unique(as.integer(landmarks))
  if (length(landmarks)) flags[landmarks] <- TRUE
  tdabm_rd_pack_signature(flags)
}

tdabm_rd_unpack_signature <- function(x, n_bits) {
  n_bits <- tdabm_rd_assert_scalar_integer(n_bits, "n_bits", minimum = 1L)
  if (!is.raw(x)) stop("Packed signature must be a raw vector.", call. = FALSE)
  expected_bytes <- as.integer(ceiling(n_bits / 8))
  if (length(x) != expected_bytes) {
    stop(
      "Packed signature byte length does not match n_bits: expected ", expected_bytes,
      ", observed ", length(x), ".",
      call. = FALSE
    )
  }
  bits <- as.integer(base::rawToBits(x))
  as.logical(bits[seq_len(n_bits)])
}

tdabm_rd_jaccard_signature <- function(a, b, n_bits) {
  aa <- tdabm_rd_unpack_signature(a, n_bits)
  bb <- tdabm_rd_unpack_signature(b, n_bits)
  union_n <- sum(aa | bb)
  if (union_n == 0L) return(1)
  sum(aa & bb) / union_n
}

tdabm_rd_cpp_landmark_positions_one_based <- function(raw_landmarks, n_observations) {
  n_observations <- tdabm_rd_assert_scalar_integer(
    n_observations, "n_observations", minimum = 1L
  )
  raw <- suppressWarnings(as.integer(raw_landmarks))
  if (length(raw) < 1L || anyNA(raw)) {
    stop("Ball Mapper returned missing or empty raw landmark indices.", call. = FALSE)
  }
  # SimplifiedBallMapperCppInterface returns C++ row positions directly:
  # landmarks.push_back(current_point). Those positions are zero-based even
  # though points_covered_by_landmarks and graph indices returned by the same
  # interface are one-based. Normalize once, explicitly, at the R/C++ boundary.
  if (raw[[1L]] != 0L || any(raw < 0L | raw >= n_observations)) {
    stop(
      "Unexpected Ball Mapper landmark-index convention: expected zero-based ",
      "positions in [0, n-1] with the first landmark at raw position 0.",
      call. = FALSE
    )
  }
  one_based <- raw + 1L
  if (any(one_based < 1L | one_based > n_observations)) {
    stop("Normalized landmark positions fall outside the R point order.", call. = FALSE)
  }
  one_based
}

tdabm_rd_run_one_order <- function(axes, unit_ids, radius_grid, replicate_index, base_seed) {
  replicate_index <- tdabm_rd_assert_scalar_integer(replicate_index, "replicate_index", minimum = 1L)
  base_seed <- tdabm_rd_assert_scalar_integer(base_seed, "base_seed", minimum = 1L, maximum = .Machine$integer.max - 1000000L)
  if (!is.data.frame(axes)) stop("axes must be a data.frame.", call. = FALSE)
  if (length(unit_ids) != nrow(axes)) stop("unit_ids length must equal topology row count.", call. = FALSE)
  if (!is.data.frame(radius_grid) || !all(c("radius_index", "radius") %in% names(radius_grid))) {
    stop("radius_grid must contain radius_index and radius.", call. = FALSE)
  }
  if (!exists("SimplifiedBallMapperCppInterface", mode = "function", inherits = TRUE)) {
    stop("Compile BallMapper.cpp before running radius diagnostics.", call. = FALSE)
  }

  seed <- as.integer(base_seed + replicate_index * 1009L)
  set.seed(seed)
  n <- nrow(axes)
  permutation <- sample.int(n, size = n, replace = FALSE)
  shuffled <- axes[permutation, , drop = FALSE]
  constant_values <- data.frame(interface_value = rep(1, n), stringsAsFactors = FALSE)

  rows <- vector("list", nrow(radius_grid))
  cover_signatures <- vector("list", nrow(radius_grid))
  landmark_signatures <- vector("list", nrow(radius_grid))
  n_pair_bits <- choose(n, 2L)

  for (k in seq_len(nrow(radius_grid))) {
    radius <- as.numeric(radius_grid$radius[[k]])
    bm <- SimplifiedBallMapperCppInterface(shuffled, constant_values, radius)
    cover_shuffled <- bm$points_covered_by_landmarks
    if (!is.list(cover_shuffled) || length(cover_shuffled) < 1L) stop("Ball Mapper returned no cover.", call. = FALSE)
    cover <- lapply(cover_shuffled, function(idx) permutation[as.integer(idx)])
    landmark_positions <- tdabm_rd_cpp_landmark_positions_one_based(bm$landmarks, n)
    landmarks <- permutation[landmark_positions]
    n_balls <- length(cover)
    true_ball_sizes <- vapply(cover, length, integer(1))
    if (any(true_ball_sizes < 1L)) stop("Ball Mapper returned an empty ball.", call. = FALSE)
    resolution <- tdabm_rd_ball_resolution_metrics(cover, n)

    edges <- as.matrix(bm$edges)
    if (length(edges) == 0L) edges <- matrix(numeric(0), ncol = 2L)
    graph <- tdabm_rd_graph_components(n_balls, edges)

    membership_counts_shuffled <- vapply(bm$coverage, length, integer(1))
    if (length(membership_counts_shuffled) != n || any(membership_counts_shuffled < 1L)) {
      stop("Every observation must belong to at least one ball.", call. = FALSE)
    }
    membership_counts <- integer(n)
    membership_counts[permutation] <- membership_counts_shuffled

    cover_signatures[[k]] <- tdabm_rd_cover_signature(cover, n)
    landmark_signatures[[k]] <- tdabm_rd_landmark_signature(landmarks, n)

    rows[[k]] <- data.frame(
      radius_index = as.integer(radius_grid$radius_index[[k]]),
      radius = radius,
      replicate = replicate_index,
      replicate_seed = seed,
      n_observations = n,
      n_balls = n_balls,
      n_edges = nrow(edges),
      n_components = graph$n_components,
      n_nontrivial_components = graph$n_nontrivial_components,
      n_isolated_balls = graph$n_isolated_vertices,
      largest_component_share_balls = graph$largest_component_size / n_balls,
      connected = graph$n_components == 1L,
      no_isolated_balls = graph$n_isolated_vertices == 0L,
      one_ball = n_balls == 1L,
      all_points_landmarks = n_balls == n,
      min_ball_size = min(true_ball_sizes),
      median_ball_size = stats::median(true_ball_sizes),
      mean_ball_size = mean(true_ball_sizes),
      max_ball_size = max(true_ball_sizes),
      effective_n_balls = resolution$effective_n_balls,
      effective_ball_fraction = resolution$effective_ball_fraction,
      normalized_ball_size_entropy = resolution$normalized_ball_size_entropy,
      max_ball_share_observations = resolution$max_ball_share_observations,
      majority_ball = resolution$majority_ball,
      severe_dominance_ball = resolution$severe_dominance_ball,
      mean_memberships_per_observation = mean(membership_counts),
      max_memberships_per_observation = max(membership_counts),
      fraction_multicovered = mean(membership_counts > 1L),
      mean_degree = mean(graph$degree),
      max_degree = max(graph$degree),
      sd_degree = if (length(graph$degree) > 1L) stats::sd(graph$degree) else 0,
      edge_density = if (n_balls > 1L) nrow(edges) / choose(n_balls, 2L) else 0,
      cycle_rank = nrow(edges) - n_balls + graph$n_components,
      stringsAsFactors = FALSE
    )
  }

  list(
    diagnostics = do.call(rbind, rows),
    cover_signatures = cover_signatures,
    landmark_signatures = landmark_signatures,
    n_pair_bits = n_pair_bits,
    n_landmark_bits = n,
    permutation_first = paste(unit_ids[permutation][seq_len(min(5L, n))], collapse = ";"),
    permutation_last = paste(utils::tail(unit_ids[permutation], min(5L, n)), collapse = ";")
  )
}

tdabm_rd_worker_initialize <- function(module_path, cpp_path) {
  source(module_path, local = .GlobalEnv)
  tdabm_rd_compile_cpp(cpp_path)
  TRUE
}

tdabm_rd_run_replicates <- function(
  axes,
  unit_ids,
  radius_grid,
  repetitions = 100L,
  base_seed = 20260807L,
  ncores = 1L,
  module_path = NULL,
  cpp_path
) {
  repetitions <- tdabm_rd_assert_scalar_integer(repetitions, "repetitions", minimum = 2L)
  base_seed <- tdabm_rd_assert_scalar_integer(base_seed, "base_seed", minimum = 1L, maximum = .Machine$integer.max - repetitions * 1009L - 1L)
  ncores <- tdabm_rd_assert_scalar_integer(ncores, "ncores", minimum = 1L)
  ncores <- min(ncores, repetitions)
  cpp_path <- normalizePath(cpp_path, winslash = "/", mustWork = TRUE)
  reps <- seq_len(repetitions)

  if (ncores == 1L) {
    tdabm_rd_compile_cpp(cpp_path)
    out <- lapply(reps, function(r) tdabm_rd_run_one_order(axes, unit_ids, radius_grid, r, base_seed))
  } else {
    module_path <- normalizePath(module_path %||% stop("module_path is required when ncores > 1.", call. = FALSE), winslash = "/", mustWork = TRUE)
    cl <- parallel::makePSOCKcluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    init <- parallel::clusterCall(cl, tdabm_rd_worker_initialize, module_path = module_path, cpp_path = cpp_path)
    if (!all(vapply(init, isTRUE, logical(1)))) stop("A radius-diagnostic worker failed to initialise.", call. = FALSE)
    out <- parallel::parLapply(
      cl,
      reps,
      function(r, axes, unit_ids, radius_grid, base_seed) {
        tdabm_rd_run_one_order(axes, unit_ids, radius_grid, r, base_seed)
      },
      axes = axes,
      unit_ids = unit_ids,
      radius_grid = radius_grid,
      base_seed = base_seed
    )
  }

  diagnostics <- do.call(rbind, lapply(out, `[[`, "diagnostics"))
  diagnostics <- diagnostics[order(diagnostics$radius_index, diagnostics$replicate), , drop = FALSE]
  row.names(diagnostics) <- NULL
  list(replicates = out, diagnostics = diagnostics)
}

tdabm_rd_pairwise_stability <- function(replicates, radius_grid, n_observations) {
  if (!is.list(replicates) || length(replicates) < 2L) stop("At least two replicate results are required for stability diagnostics.", call. = FALSE)
  n_observations <- tdabm_rd_assert_scalar_integer(n_observations, "n_observations", minimum = 2L)
  cover_bits <- choose(n_observations, 2L)
  landmark_bits <- n_observations
  rows <- vector("list", nrow(radius_grid))

  for (k in seq_len(nrow(radius_grid))) {
    cover_scores <- numeric(0)
    landmark_scores <- numeric(0)
    for (i in seq_len(length(replicates) - 1L)) {
      for (j in seq.int(i + 1L, length(replicates))) {
        cover_scores <- c(cover_scores, tdabm_rd_jaccard_signature(
          replicates[[i]]$cover_signatures[[k]], replicates[[j]]$cover_signatures[[k]], cover_bits
        ))
        landmark_scores <- c(landmark_scores, tdabm_rd_jaccard_signature(
          replicates[[i]]$landmark_signatures[[k]], replicates[[j]]$landmark_signatures[[k]], landmark_bits
        ))
      }
    }
    rows[[k]] <- data.frame(
      radius_index = as.integer(radius_grid$radius_index[[k]]),
      radius = as.numeric(radius_grid$radius[[k]]),
      pairwise_comparisons = length(cover_scores),
      mean_pairwise_cover_jaccard = mean(cover_scores),
      q10_pairwise_cover_jaccard = as.numeric(stats::quantile(cover_scores, 0.10, names = FALSE, type = 7)),
      median_pairwise_cover_jaccard = stats::median(cover_scores),
      min_pairwise_cover_jaccard = min(cover_scores),
      mean_pairwise_landmark_jaccard = mean(landmark_scores),
      median_pairwise_landmark_jaccard = stats::median(landmark_scores),
      min_pairwise_landmark_jaccard = min(landmark_scores),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

tdabm_rd_q <- function(x, p) as.numeric(stats::quantile(x, probs = p, names = FALSE, type = 7, na.rm = FALSE))

tdabm_rd_summarise <- function(diagnostics, stability) {
  if (!is.data.frame(diagnostics) || !nrow(diagnostics)) stop("diagnostics contains no rows.", call. = FALSE)
  indices <- sort(unique(as.integer(diagnostics$radius_index)))
  rows <- lapply(indices, function(idx) {
    x <- diagnostics[diagnostics$radius_index == idx, , drop = FALSE]
    data.frame(
      radius_index = idx,
      radius = unique(x$radius)[[1L]],
      repetitions = nrow(x),
      median_n_balls = stats::median(x$n_balls),
      q10_n_balls = tdabm_rd_q(x$n_balls, 0.10),
      q90_n_balls = tdabm_rd_q(x$n_balls, 0.90),
      sd_n_balls = stats::sd(x$n_balls),
      median_n_edges = stats::median(x$n_edges),
      q10_n_edges = tdabm_rd_q(x$n_edges, 0.10),
      q90_n_edges = tdabm_rd_q(x$n_edges, 0.90),
      median_n_components = stats::median(x$n_components),
      median_n_isolated_balls = stats::median(x$n_isolated_balls),
      connected_rate = mean(x$connected),
      no_isolate_rate = mean(x$no_isolated_balls),
      one_ball_rate = mean(x$one_ball),
      all_points_landmarks_rate = mean(x$all_points_landmarks),
      median_largest_component_share_balls = stats::median(x$largest_component_share_balls),
      median_mean_ball_size = stats::median(x$mean_ball_size),
      median_max_ball_size = stats::median(x$max_ball_size),
      median_effective_n_balls = stats::median(x$effective_n_balls),
      median_effective_ball_fraction = stats::median(x$effective_ball_fraction),
      median_normalized_ball_size_entropy = stats::median(x$normalized_ball_size_entropy),
      median_max_ball_share_observations = stats::median(x$max_ball_share_observations),
      majority_ball_rate = mean(x$majority_ball),
      severe_dominance_ball_rate = mean(x$severe_dominance_ball),
      median_mean_memberships_per_observation = stats::median(x$mean_memberships_per_observation),
      median_fraction_multicovered = stats::median(x$fraction_multicovered),
      median_mean_degree = stats::median(x$mean_degree),
      median_max_degree = stats::median(x$max_degree),
      median_edge_density = stats::median(x$edge_density),
      median_cycle_rank = stats::median(x$cycle_rank),
      stringsAsFactors = FALSE
    )
  })
  summary <- do.call(rbind, rows)
  merge(summary, stability, by = c("radius_index", "radius"), all.x = TRUE, sort = TRUE)
}

tdabm_rd_interpolate_radius_at_share <- function(summary, target_share = 0.50) {
  if (!is.data.frame(summary) || !nrow(summary)) stop("summary contains no rows.", call. = FALSE)
  target_share <- tdabm_rd_assert_scalar_numeric(target_share, "target_share", minimum = 0, maximum = 1)
  if (!"median_max_ball_share_observations" %in% names(summary)) {
    stop("summary lacks median_max_ball_share_observations.", call. = FALSE)
  }
  x <- summary[order(summary$radius), , drop = FALSE]
  share <- as.numeric(x$median_max_ball_share_observations)
  if (any(!is.finite(share))) stop("median_max_ball_share_observations must be finite.", call. = FALSE)
  idx <- which(share >= target_share)
  if (!length(idx)) {
    return(data.frame(
      target_share = target_share, reference_radius = NA_real_, lower_radius = NA_real_, upper_radius = NA_real_,
      lower_share = NA_real_, upper_share = NA_real_, status = "NO_CROSSING_IN_GRID", stringsAsFactors = FALSE
    ))
  }
  i <- idx[[1L]]
  if (i == 1L) {
    return(data.frame(
      target_share = target_share, reference_radius = x$radius[[1L]], lower_radius = NA_real_, upper_radius = x$radius[[1L]],
      lower_share = NA_real_, upper_share = share[[1L]], status = "CROSSING_AT_OR_BELOW_GRID_MINIMUM", stringsAsFactors = FALSE
    ))
  }
  lo_r <- x$radius[[i - 1L]]
  hi_r <- x$radius[[i]]
  lo_s <- share[[i - 1L]]
  hi_s <- share[[i]]
  if (hi_s <= lo_s) {
    ref <- hi_r
    status <- "NONMONOTONE_LOCAL_SHARE_USED_UPPER_BRACKET"
  } else {
    w <- (target_share - lo_s) / (hi_s - lo_s)
    ref <- exp(log(lo_r) + w * (log(hi_r) - log(lo_r)))
    status <- "LOG_RADIUS_INTERPOLATED"
  }
  data.frame(
    target_share = target_share, reference_radius = as.numeric(ref), lower_radius = lo_r, upper_radius = hi_r,
    lower_share = lo_s, upper_share = hi_s, status = status, stringsAsFactors = FALSE
  )
}

tdabm_rd_resolution_transition_report <- function(summary, n_observations, n_dimensions, dominance_targets = c(0.40, 0.50, 0.60, 0.75)) {
  if (!is.data.frame(summary) || !nrow(summary)) stop("summary contains no rows.", call. = FALSE)
  n_observations <- tdabm_rd_assert_scalar_integer(n_observations, "n_observations", minimum = 2L)
  n_dimensions <- tdabm_rd_assert_scalar_integer(n_dimensions, "n_dimensions", minimum = 1L)
  dominance_targets <- as.numeric(dominance_targets)
  if (!length(dominance_targets) || any(!is.finite(dominance_targets)) || any(dominance_targets <= 0 | dominance_targets >= 1)) {
    stop("dominance_targets must contain finite values strictly between zero and one.", call. = FALSE)
  }
  if (anyDuplicated(dominance_targets)) stop("dominance_targets contains duplicates.", call. = FALSE)
  x <- summary[order(summary$radius), , drop = FALSE]
  x$radius_per_sqrt_dimension <- x$radius / sqrt(n_dimensions)
  x$median_ball_count_share_observations <- x$median_n_balls / n_observations
  x$median_effective_ball_count_share_observations <- x$median_effective_n_balls / n_observations

  first_radius <- function(condition) {
    idx <- which(condition)
    if (!length(idx)) NA_real_ else x$radius[[idx[[1L]]]]
  }
  first_nontrivial <- first_radius(x$median_fraction_multicovered > 0 | x$median_n_edges > 0 | x$median_n_balls < n_observations)
  nontrivial <- x[x$radius >= first_nontrivial & x$one_ball_rate < 0.50, , drop = FALSE]
  stability_trough <- if (nrow(nontrivial)) nontrivial$radius[[which.min(nontrivial$mean_pairwise_cover_jaccard)]] else NA_real_
  peak_multicover <- if (nrow(nontrivial)) nontrivial$radius[[which.max(nontrivial$median_fraction_multicovered)]] else NA_real_

  transitions <- data.frame(
    transition = c(
      "first_nontrivial_topology", "cover_stability_trough", "peak_multicover",
      "connected_rate_ge_0_50_descriptive", "connected_rate_ge_0_95_descriptive",
      "no_isolate_rate_ge_0_50_descriptive", "no_isolate_rate_ge_0_95_descriptive",
      "median_balls_le_20_descriptive", "median_balls_le_10_descriptive", "median_balls_le_5_descriptive",
      "one_ball_rate_ge_0_50_collapse"
    ),
    radius = c(
      first_nontrivial, stability_trough, peak_multicover,
      first_radius(x$connected_rate >= 0.50), first_radius(x$connected_rate >= 0.95),
      first_radius(x$no_isolate_rate >= 0.50), first_radius(x$no_isolate_rate >= 0.95),
      first_radius(x$median_n_balls <= 20), first_radius(x$median_n_balls <= 10), first_radius(x$median_n_balls <= 5),
      first_radius(x$one_ball_rate >= 0.50)
    ),
    role = c(
      "topology_transition", "order_sensitivity_transition", "overlap_transition",
      "descriptive_only", "descriptive_only", "descriptive_only", "descriptive_only",
      "descriptive_only", "descriptive_only", "descriptive_only", "collapse_transition"
    ),
    stringsAsFactors = FALSE
  )

  crossings <- do.call(rbind, lapply(dominance_targets, function(z) tdabm_rd_interpolate_radius_at_share(x, z)))
  crossings$radius_per_sqrt_dimension <- crossings$reference_radius / sqrt(n_dimensions)
  crossings$reference_role <- ifelse(
    abs(crossings$target_share - 0.50) < 1e-12,
    "PRIMARY_MAJORITY_COVER_TRANSITION_REFERENCE",
    "SENSITIVITY_DOMINANCE_TRANSITION"
  )

  primary <- crossings[abs(crossings$target_share - 0.50) < 1e-12, , drop = FALSE]
  if (nrow(primary) != 1L) stop("Exactly one 50% dominance reference is required.", call. = FALSE)
  band <- crossings[crossings$target_share %in% c(0.40, 0.60), , drop = FALSE]
  if (nrow(band) == 2L && all(is.finite(band$reference_radius))) {
    review_lower <- min(band$reference_radius)
    review_upper <- max(band$reference_radius)
  } else {
    review_lower <- NA_real_
    review_upper <- NA_real_
  }
  nearest_idx <- if (is.finite(primary$reference_radius[[1L]])) {
    which.min(abs(log(x$radius) - log(primary$reference_radius[[1L]])))
  } else {
    NA_integer_
  }
  nearest <- if (is.finite(nearest_idx)) x[nearest_idx, , drop = FALSE] else x[0, , drop = FALSE]
  reference <- data.frame(
    method = "majority_cover_transition",
    reference_radius = primary$reference_radius,
    radius_per_sqrt_dimension = primary$radius_per_sqrt_dimension,
    review_band_lower = review_lower,
    review_band_upper = review_upper,
    nearest_grid_radius = if (nrow(nearest)) nearest$radius[[1L]] else NA_real_,
    nearest_median_n_balls = if (nrow(nearest)) nearest$median_n_balls[[1L]] else NA_real_,
    nearest_median_effective_n_balls = if (nrow(nearest)) nearest$median_effective_n_balls[[1L]] else NA_real_,
    nearest_median_max_ball_share = if (nrow(nearest)) nearest$median_max_ball_share_observations[[1L]] else NA_real_,
    nearest_cover_stability = if (nrow(nearest)) nearest$mean_pairwise_cover_jaccard[[1L]] else NA_real_,
    selection_status = "NON_BINDING_RESOLUTION_REFERENCE",
    automatic_approval = FALSE,
    optimality_claim = "NONE",
    rationale = "Interpolated radius at which the median largest ball first covers 50% of observations; connectivity is descriptive only.",
    stringsAsFactors = FALSE
  )

  list(summary = x, transitions = transitions, dominance_crossings = crossings, reference = reference)
}

tdabm_rd_resolution_review_rows <- function(summary, reference_radius, points_each_side = 2L) {
  if (!is.data.frame(summary) || !nrow(summary)) stop("summary contains no rows.", call. = FALSE)
  reference_radius <- tdabm_rd_assert_scalar_numeric(reference_radius, "reference_radius", minimum = .Machine$double.eps)
  points_each_side <- tdabm_rd_assert_scalar_integer(points_each_side, "points_each_side", minimum = 1L)
  x <- summary[order(summary$radius), , drop = FALSE]
  closest <- which.min(abs(log(x$radius) - log(reference_radius)))
  idx <- seq.int(max(1L, closest - points_each_side), min(nrow(x), closest + points_each_side))
  out <- x[idx, , drop = FALSE]
  out$review_role <- ifelse(out$radius < reference_radius, "below_reference", ifelse(out$radius > reference_radius, "above_reference", "nearest_reference_grid_point"))
  out$approval_status <- "PENDING_USER_DECISION"
  out
}

tdabm_rd_user_radius_review <- function(summary, proposed_radius, resolution_reference, n_dimensions) {
  if (!is.data.frame(summary) || !nrow(summary)) stop("summary contains no rows.", call. = FALSE)
  proposed_radius <- tdabm_rd_assert_scalar_numeric(proposed_radius, "proposed_radius", minimum = .Machine$double.eps)
  n_dimensions <- tdabm_rd_assert_scalar_integer(n_dimensions, "n_dimensions", minimum = 1L)
  if (!is.data.frame(resolution_reference) || nrow(resolution_reference) != 1L) stop("resolution_reference must contain exactly one row.", call. = FALSE)
  ref <- as.numeric(resolution_reference$reference_radius[[1L]])
  lower <- as.numeric(resolution_reference$review_band_lower[[1L]])
  upper <- as.numeric(resolution_reference$review_band_upper[[1L]])
  data.frame(
    proposed_radius = proposed_radius,
    radius_per_sqrt_dimension = proposed_radius / sqrt(n_dimensions),
    pipeline_resolution_reference = ref,
    relative_difference_from_reference = if (is.finite(ref)) abs(proposed_radius - ref) / ref else NA_real_,
    review_band_lower = lower,
    review_band_upper = upper,
    inside_resolution_review_band = is.finite(lower) && is.finite(upper) && proposed_radius >= lower && proposed_radius <= upper,
    automatic_approval = FALSE,
    approval_status = "AWAITING_EXACT_RADIUS_EVIDENCE_AND_EXPLICIT_USER_DECISION",
    optimality_claim = "NONE",
    stringsAsFactors = FALSE
  )
}

tdabm_rd_technical_user_radius_review <- function(exact_summary_row, user_radius_review, severe_dominance_threshold = 0.75, radius_tolerance = 1e-8) {
  if (!is.data.frame(exact_summary_row) || nrow(exact_summary_row) != 1L) stop("exact_summary_row must contain exactly one radius summary row.", call. = FALSE)
  if (!is.data.frame(user_radius_review) || nrow(user_radius_review) != 1L) stop("user_radius_review must contain exactly one row.", call. = FALSE)
  severe_dominance_threshold <- tdabm_rd_assert_scalar_numeric(severe_dominance_threshold, "severe_dominance_threshold", minimum = 0.50, maximum = 1)
  radius_tolerance <- tdabm_rd_assert_scalar_numeric(radius_tolerance, "radius_tolerance", minimum = 0)
  required_summary <- c("radius","one_ball_rate","median_n_balls","median_effective_n_balls","median_max_ball_share_observations","mean_pairwise_cover_jaccard")
  missing_summary <- setdiff(required_summary,names(exact_summary_row))
  if (length(missing_summary)) stop("exact_summary_row is missing required technical fields: ",paste(missing_summary,collapse=";"),call.=FALSE)
  required_review <- c("proposed_radius","review_band_lower","review_band_upper","inside_resolution_review_band","automatic_approval","optimality_claim")
  missing_review <- setdiff(required_review,names(user_radius_review))
  if (length(missing_review)) stop("user_radius_review is missing required fields: ",paste(missing_review,collapse=";"),call.=FALSE)
  exact_radius<-suppressWarnings(as.numeric(exact_summary_row$radius[[1L]]))
  proposed_radius<-suppressWarnings(as.numeric(user_radius_review$proposed_radius[[1L]]))
  one_ball_rate<-suppressWarnings(as.numeric(exact_summary_row$one_ball_rate[[1L]]))
  median_n_balls<-suppressWarnings(as.numeric(exact_summary_row$median_n_balls[[1L]]))
  median_effective_n_balls<-suppressWarnings(as.numeric(exact_summary_row$median_effective_n_balls[[1L]]))
  dominance<-suppressWarnings(as.numeric(exact_summary_row$median_max_ball_share_observations[[1L]]))
  cover_stability<-suppressWarnings(as.numeric(exact_summary_row$mean_pairwise_cover_jaccard[[1L]]))
  lower<-suppressWarnings(as.numeric(user_radius_review$review_band_lower[[1L]]))
  upper<-suppressWarnings(as.numeric(user_radius_review$review_band_upper[[1L]]))
  integrity_checks <- data.frame(
    check=c("exact_radius_evaluated","exact_radius_positive_finite","one_ball_rate_finite_bounded","median_n_balls_finite_representable","median_effective_n_balls_finite_representable","dominance_share_finite_bounded","cover_stability_finite_bounded","review_band_recorded_valid","automatic_approval_false","optimality_claim_none"),
    pass=c(
      is.finite(exact_radius)&&is.finite(proposed_radius)&&abs(exact_radius-proposed_radius)<=radius_tolerance,
      is.finite(exact_radius)&&exact_radius>0,
      is.finite(one_ball_rate)&&one_ball_rate>=0&&one_ball_rate<=1,
      is.finite(median_n_balls)&&median_n_balls>=1,
      is.finite(median_effective_n_balls)&&median_effective_n_balls>=1,
      is.finite(dominance)&&dominance>=0&&dominance<=1,
      is.finite(cover_stability)&&cover_stability>=0&&cover_stability<=1,
      is.finite(lower)&&is.finite(upper)&&lower>0&&upper>lower,
      identical(user_radius_review$automatic_approval[[1L]],FALSE),
      identical(toupper(as.character(user_radius_review$optimality_claim[[1L]])),"NONE")
    ),
    detail=c(
      paste0("evaluated=",exact_radius,"; proposed=",proposed_radius,"; tolerance=",radius_tolerance),
      as.character(exact_radius),as.character(one_ball_rate),as.character(median_n_balls),
      as.character(median_effective_n_balls),as.character(dominance),as.character(cover_stability),
      paste0(lower," to ",upper),as.character(user_radius_review$automatic_approval[[1L]]),
      as.character(user_radius_review$optimality_claim[[1L]])
    ),
    blocking=TRUE,
    stringsAsFactors=FALSE
  )
  connected_rate<-if("connected_rate"%in%names(exact_summary_row)) suppressWarnings(as.numeric(exact_summary_row$connected_rate[[1L]])) else NA_real_
  no_isolate_rate<-if("no_isolate_rate"%in%names(exact_summary_row)) suppressWarnings(as.numeric(exact_summary_row$no_isolate_rate[[1L]])) else NA_real_
  diagnostic_flags <- data.frame(
    diagnostic=c("outside_resolution_review_band","one_ball_collapse_observed","median_ball_count_at_or_below_one","median_effective_ball_count_at_or_below_one","severe_dominance_marker_crossed","connectedness_state","isolate_state"),
    triggered=c(
      !isTRUE(user_radius_review$inside_resolution_review_band[[1L]]),
      is.finite(one_ball_rate)&&one_ball_rate>0,
      is.finite(median_n_balls)&&median_n_balls<=1,
      is.finite(median_effective_n_balls)&&median_effective_n_balls<=1,
      is.finite(dominance)&&dominance>=severe_dominance_threshold,
      FALSE,FALSE
    ),
    state=c(
      if(isTRUE(user_radius_review$inside_resolution_review_band[[1L]]))"INSIDE_REVIEW_BAND"else"OUTSIDE_REVIEW_BAND",
      if(is.finite(one_ball_rate)&&one_ball_rate>0)"OBSERVED_IN_AT_LEAST_ONE_REPEAT"else"NOT_OBSERVED",
      if(is.finite(median_n_balls)&&median_n_balls<=1)"LOW_RESOLUTION"else"MORE_THAN_ONE_MEDIAN_BALL",
      if(is.finite(median_effective_n_balls)&&median_effective_n_balls<=1)"LOW_EFFECTIVE_RESOLUTION"else"MORE_THAN_ONE_EFFECTIVE_BALL",
      if(is.finite(dominance)&&dominance>=severe_dominance_threshold)"SEVERE_DOMINANCE_MARKER_CROSSED"else"BELOW_SEVERE_DOMINANCE_MARKER",
      if(is.finite(connected_rate))paste0("CONNECTED_RATE=",connected_rate)else"NOT_REPORTED",
      if(is.finite(no_isolate_rate))paste0("NO_ISOLATE_RATE=",no_isolate_rate)else"NOT_REPORTED"
    ),
    detail=c(
      paste0(lower," to ",upper," (non-binding review region)"),
      as.character(one_ball_rate),as.character(median_n_balls),as.character(median_effective_n_balls),
      paste0(dominance," versus descriptive marker ",severe_dominance_threshold),
      as.character(connected_rate),as.character(no_isolate_rate)
    ),
    blocking=FALSE,
    stringsAsFactors=FALSE
  )
  complete <- all(integrity_checks$pass %in% TRUE)
  list(
    technical_evidence_complete=complete,
    status=if(complete)"TECHNICAL_EVIDENCE_COMPLETE_USER_DECISION_EXTERNAL"else"TECHNICAL_EVIDENCE_INVALID",
    integrity_checks=integrity_checks,
    diagnostic_flags=diagnostic_flags,
    user_approval_status="EXPLICIT_USER_DECISION_REQUIRED",
    automatic_approval=FALSE,
    optimality_claim="NONE"
  )
}

# Deprecated P1.4-B v1.0 helpers are retained only to fail loudly if old code
# attempts to recreate the connectedness-based candidate window.
tdabm_rd_transition_report <- function(...) {
  stop("P1.4-B v1.0 connectedness-based diagnostic windows are deprecated. Use tdabm_rd_resolution_transition_report().", call. = FALSE)
}

tdabm_rd_candidate_shortlist <- function(...) {
  stop("P1.4-B v1.0 connectedness-based candidate shortlists are deprecated. Use the non-binding resolution reference and review rows.", call. = FALSE)
}

tdabm_rd_compare_runs <- function(a, b, tolerance = 0) {
  if (!is.data.frame(a) || !is.data.frame(b)) stop("Both worker-invariance inputs must be data frames.", call. = FALSE)
  tolerance <- tdabm_rd_assert_scalar_numeric(tolerance, "tolerance", minimum = 0)
  same_names <- identical(names(a), names(b))
  same_rows <- nrow(a) == nrow(b)
  if (!same_names || !same_rows) {
    return(data.frame(check = c("column_identity", "row_count_identity"), pass = c(same_names, same_rows), detail = c(paste(names(a), collapse = ";"), paste(nrow(a), nrow(b), sep = " vs ")), stringsAsFactors = FALSE))
  }
  rows <- lapply(names(a), function(nm) {
    x <- a[[nm]]
    y <- b[[nm]]
    if (is.numeric(x) && is.numeric(y)) {
      ok <- length(x) == length(y) && all((is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & abs(x - y) <= tolerance))
      detail <- if (ok) "identical within tolerance" else paste0("max_abs_diff=", suppressWarnings(max(abs(x - y), na.rm = TRUE)))
    } else {
      ok <- identical(as.character(x), as.character(y))
      detail <- if (ok) "identical" else "different"
    }
    data.frame(check = paste0("column_", nm), pass = ok, detail = detail, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

tdabm_rd_write_diagnostic_plots <- function(summary, output_dir) {
  if (!is.data.frame(summary) || !nrow(summary)) stop("summary contains no rows for plotting.", call. = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Could not create diagnostic plot directory.", call. = FALSE)
  files <- c(
    balls = file.path(output_dir, "radius_vs_ball_count.png"),
    edges = file.path(output_dir, "radius_vs_edge_count.png"),
    connectivity = file.path(output_dir, "radius_vs_connectivity.png"),
    stability = file.path(output_dir, "radius_vs_cover_stability.png"),
    resolution = file.path(output_dir, "radius_vs_resolution_and_dominance.png")
  )

  grDevices::png(files[["balls"]], width = 1200, height = 800, res = 120)
  graphics::plot(summary$radius, summary$median_n_balls, type = "l", log = "x", xlab = "Radius", ylab = "Median number of balls")
  graphics::lines(summary$radius, summary$q10_n_balls, lty = 2)
  graphics::lines(summary$radius, summary$q90_n_balls, lty = 2)
  grDevices::dev.off()

  grDevices::png(files[["edges"]], width = 1200, height = 800, res = 120)
  graphics::plot(summary$radius, summary$median_n_edges, type = "l", log = "x", xlab = "Radius", ylab = "Median number of overlap edges")
  graphics::lines(summary$radius, summary$q10_n_edges, lty = 2)
  graphics::lines(summary$radius, summary$q90_n_edges, lty = 2)
  grDevices::dev.off()

  grDevices::png(files[["connectivity"]], width = 1200, height = 800, res = 120)
  graphics::plot(summary$radius, summary$connected_rate, type = "l", log = "x", ylim = c(0, 1), xlab = "Radius", ylab = "Fraction of repeated orders")
  graphics::lines(summary$radius, summary$no_isolate_rate, lty = 2)
  graphics::lines(summary$radius, summary$one_ball_rate, lty = 3)
  graphics::legend("bottomright", legend = c("connected", "no isolated balls", "one ball"), lty = c(1, 2, 3), bty = "n")
  grDevices::dev.off()

  grDevices::png(files[["stability"]], width = 1200, height = 800, res = 120)
  graphics::plot(summary$radius, summary$mean_pairwise_cover_jaccard, type = "l", log = "x", ylim = c(0, 1), xlab = "Radius", ylab = "Mean pairwise Jaccard stability")
  graphics::lines(summary$radius, summary$mean_pairwise_landmark_jaccard, lty = 2)
  graphics::legend("bottomright", legend = c("cover relation", "landmark set"), lty = c(1, 2), bty = "n")
  grDevices::dev.off()

  grDevices::png(files[["resolution"]], width = 1200, height = 800, res = 120)
  graphics::plot(summary$radius, summary$median_n_balls, type = "l", log = "x", xlab = "Radius", ylab = "Median balls / effective balls")
  graphics::lines(summary$radius, summary$median_effective_n_balls, lty = 2)
  graphics::par(new = TRUE)
  graphics::plot(summary$radius, summary$median_max_ball_share_observations, type = "l", log = "x", axes = FALSE, xlab = "", ylab = "", ylim = c(0, 1), lty = 3)
  graphics::axis(4)
  graphics::mtext("Median largest-ball share", side = 4, line = 3)
  graphics::abline(h = 0.5, lty = 4)
  graphics::legend("topright", legend = c("balls", "effective balls", "largest-ball share"), lty = c(1, 2, 3), bty = "n")
  grDevices::dev.off()

  sizes <- file.info(files)$size
  if (any(is.na(sizes) | sizes <= 100L)) stop("A radius-diagnostic plot was not written successfully.", call. = FALSE)
  data.frame(plot_id = names(files), file = unname(files), byte_size = as.numeric(sizes), stringsAsFactors = FALSE)
}
