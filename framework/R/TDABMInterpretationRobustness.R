# ==============================================================================
# TDABMInterpretationRobustness.R
# ==============================================================================
# Application-neutral post-approval robustness engine.
#
# IMPORTANT BOUNDARY:
#   * radius approval must already exist before this module is called;
#   * colour/context variables NEVER select or approve a radius;
#   * alternative Ball Mapper objects are transient robustness objects only;
#   * no canonical/frozen topology is written by this module.
# ==============================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

tdabm_ir_assert_radii <- function(radii, approved_radius) {
  radii <- suppressWarnings(as.numeric(radii))
  approved_radius <- suppressWarnings(as.numeric(approved_radius))
  if (!length(radii) || any(!is.finite(radii)) || any(radii <= 0)) stop("Robustness radii must be positive finite values.", call. = FALSE)
  if (length(approved_radius) != 1L || !is.finite(approved_radius) || approved_radius <= 0) stop("approved_radius must be one positive finite value.", call. = FALSE)
  radii <- sort(unique(radii))
  if (!any(abs(radii - approved_radius) <= 1e-10)) stop("Robustness grid must include the approved radius.", call. = FALSE)
  radii
}

tdabm_ir_review_band_grid <- function(approved_radius, review_band_lower, review_band_upper) {
  a <- as.numeric(approved_radius)
  lo <- as.numeric(review_band_lower)
  hi <- as.numeric(review_band_upper)
  if (any(!is.finite(c(a, lo, hi))) || lo <= 0 || hi <= lo || a < lo || a > hi) stop("Invalid review-band inputs.", call. = FALSE)
  sort(unique(c(lo, (lo + a) / 2, a, (a + hi) / 2, hi)))
}

tdabm_ir_compile_cpp <- function(cpp_path) {
  if (!requireNamespace("Rcpp", quietly = TRUE)) stop("Package 'Rcpp' is required.", call. = FALSE)
  cpp_path <- normalizePath(cpp_path, winslash = "/", mustWork = TRUE)
  Rcpp::sourceCpp(cpp_path, rebuild = FALSE, showOutput = FALSE, verbose = FALSE)
  if (!exists("SimplifiedBallMapperCppInterface", mode = "function", inherits = TRUE)) stop("Ball Mapper C++ interface did not load.", call. = FALSE)
  invisible(TRUE)
}

tdabm_ir_cpp_landmarks_one_based <- function(raw_landmarks, n_points) {
  raw <- suppressWarnings(as.integer(raw_landmarks))
  if (!length(raw) || anyNA(raw)) stop("Raw landmark positions are invalid.", call. = FALSE)
  if (raw[[1L]] != 0L || any(raw < 0L | raw >= n_points)) stop("Unexpected C++ landmark-index convention.", call. = FALSE)
  raw + 1L
}

tdabm_ir_graph_metrics <- function(cover, edges, n_points) {
  n_balls <- length(cover)
  e <- as.matrix(edges)
  if (length(e) == 0L) e <- matrix(integer(), ncol = 2L)
  adjacency <- vector("list", n_balls)
  for (i in seq_len(n_balls)) adjacency[[i]] <- integer()
  if (nrow(e)) {
    for (i in seq_len(nrow(e))) {
      a <- as.integer(e[i, 1L]); b <- as.integer(e[i, 2L])
      adjacency[[a]] <- unique(c(adjacency[[a]], b))
      adjacency[[b]] <- unique(c(adjacency[[b]], a))
    }
  }
  degree <- vapply(adjacency, length, integer(1))
  comp <- integer(n_balls); cid <- 0L; comp_sizes <- integer()
  for (v in seq_len(n_balls)) {
    if (comp[[v]] != 0L) next
    cid <- cid + 1L; q <- v; comp[[v]] <- cid; size <- 0L
    while (length(q)) {
      u <- q[[1L]]
      if (length(q) == 1L) q <- integer() else q <- q[-1L]
      size <- size + 1L
      for (w in adjacency[[u]]) if (comp[[w]] == 0L) { comp[[w]] <- cid; q <- c(q, w) }
    }
    comp_sizes <- c(comp_sizes, size)
  }
  sizes <- vapply(cover, length, integer(1))
  weights <- sizes / sum(sizes)
  effective <- exp(-sum(weights * log(weights)))
  membership_count <- integer(n_points)
  for (idx in cover) membership_count[unique(as.integer(idx))] <- membership_count[unique(as.integer(idx))] + 1L
  list(
    n_balls = n_balls,
    n_edges = nrow(e),
    n_components = length(comp_sizes),
    n_isolated_balls = sum(degree == 0L),
    effective_n_balls = effective,
    max_ball_share_observations = max(sizes) / n_points,
    mean_memberships_per_observation = mean(membership_count),
    fraction_multicovered = mean(membership_count > 1L)
  )
}

tdabm_ir_local_colour_from_cover <- function(cover, values, n_points, aggregation = "equal_ball_mean") {
  if (!identical(aggregation, "equal_ball_mean")) stop("Unsupported local-colour aggregation.", call. = FALSE)
  values <- suppressWarnings(as.numeric(values))
  if (length(values) != n_points || any(!is.finite(values))) stop("Colour values must be finite and match point count.", call. = FALSE)
  ball_means <- vapply(cover, function(idx) mean(values[as.integer(idx)]), numeric(1))
  memberships <- vector("list", n_points)
  for (ball_id in seq_along(cover)) {
    idx <- unique(as.integer(cover[[ball_id]]))
    for (i in idx) memberships[[i]] <- c(memberships[[i]], ball_id)
  }
  local <- numeric(n_points)
  count <- integer(n_points)
  for (i in seq_len(n_points)) {
    b <- unique(as.integer(memberships[[i]]))
    if (!length(b)) stop("Robustness topology left an observation uncovered.", call. = FALSE)
    local[[i]] <- mean(ball_means[b])
    count[[i]] <- length(b)
  }
  list(local = local, membership_count = count, ball_means = ball_means)
}

tdabm_ir_metric_row <- function(local, canonical_local) {
  local <- as.numeric(local); canonical_local <- as.numeric(canonical_local)
  if (length(local) != length(canonical_local) || any(!is.finite(local)) || any(!is.finite(canonical_local))) stop("Local-colour comparison vectors are invalid.", call. = FALSE)
  data.frame(
    pearson_to_canonical = if (stats::sd(local) > 0 && stats::sd(canonical_local) > 0) stats::cor(local, canonical_local, method = "pearson") else NA_real_,
    spearman_to_canonical = if (stats::sd(local) > 0 && stats::sd(canonical_local) > 0) stats::cor(local, canonical_local, method = "spearman") else NA_real_,
    rmse_to_canonical = sqrt(mean((local - canonical_local)^2)),
    mae_to_canonical = mean(abs(local - canonical_local)),
    stringsAsFactors = FALSE
  )
}

tdabm_ir_run_one_order <- function(
  axes, unit_ids, colour_frame, canonical_local_by_colour,
  radii, approved_radius, replicate_index, base_seed,
  aggregation = "equal_ball_mean"
) {
  n <- nrow(axes)
  seed <- as.integer(base_seed + replicate_index * 1009L)
  set.seed(seed)
  permutation <- sample.int(n, n, replace = FALSE)
  shuffled <- axes[permutation, , drop = FALSE]
  constant_values <- data.frame(interface_value = rep(1, n), stringsAsFactors = FALSE)

  topology_rows <- list()
  comparison_rows <- list()
  point_rows <- list()
  kt <- 0L; kc <- 0L; kp <- 0L

  for (radius in radii) {
    bm <- SimplifiedBallMapperCppInterface(shuffled, constant_values, radius)
    cover_shuffled <- bm$points_covered_by_landmarks
    if (!is.list(cover_shuffled) || !length(cover_shuffled)) stop("Transient robustness topology returned no cover.", call. = FALSE)
    cover <- lapply(cover_shuffled, function(idx) permutation[as.integer(idx)])
    raw_lm_one <- tdabm_ir_cpp_landmarks_one_based(bm$landmarks, n)
    landmarks <- permutation[raw_lm_one]
    if (any(landmarks < 1L | landmarks > n)) stop("Mapped transient landmarks are invalid.", call. = FALSE)

    gm <- tdabm_ir_graph_metrics(cover, bm$edges, n)
    kt <- kt + 1L
    topology_rows[[kt]] <- data.frame(
      replicate = replicate_index,
      replicate_seed = seed,
      radius = radius,
      approved_radius = approved_radius,
      is_approved_radius = abs(radius - approved_radius) <= 1e-10,
      n_balls = gm$n_balls,
      n_edges = gm$n_edges,
      n_components = gm$n_components,
      n_isolated_balls = gm$n_isolated_balls,
      effective_n_balls = gm$effective_n_balls,
      max_ball_share_observations = gm$max_ball_share_observations,
      mean_memberships_per_observation = gm$mean_memberships_per_observation,
      fraction_multicovered = gm$fraction_multicovered,
      stringsAsFactors = FALSE
    )

    for (v in names(colour_frame)) {
      lc <- tdabm_ir_local_colour_from_cover(cover, colour_frame[[v]], n, aggregation)
      canonical <- canonical_local_by_colour[[v]]
      metrics <- tdabm_ir_metric_row(lc$local, canonical)
      kc <- kc + 1L
      comparison_rows[[kc]] <- cbind(
        data.frame(
          replicate = replicate_index,
          replicate_seed = seed,
          radius = radius,
          approved_radius = approved_radius,
          is_approved_radius = abs(radius - approved_radius) <= 1e-10,
          colour_variable = v,
          stringsAsFactors = FALSE
        ),
        metrics
      )
      kp <- kp + 1L
      point_rows[[kp]] <- data.frame(
        replicate = replicate_index,
        replicate_seed = seed,
        radius = radius,
        approved_radius = approved_radius,
        is_approved_radius = abs(radius - approved_radius) <= 1e-10,
        colour_variable = v,
        point_index = seq_len(n),
        unit_id = unit_ids,
        local_colour = lc$local,
        membership_count = lc$membership_count,
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    topology = do.call(rbind, topology_rows),
    comparison = do.call(rbind, comparison_rows),
    points = do.call(rbind, point_rows)
  )
}

tdabm_ir_worker_initialize <- function(module_path, cpp_path) {
  source(module_path, local = .GlobalEnv)
  tdabm_ir_compile_cpp(cpp_path)
  TRUE
}

tdabm_ir_run <- function(
  axes, unit_ids, colour_frame, canonical_local_by_colour,
  radii, approved_radius, repetitions = 100L, base_seed = 20260807L,
  ncores = 1L, aggregation = "equal_ball_mean",
  module_path = NULL, cpp_path
) {
  axes <- as.data.frame(axes, stringsAsFactors = FALSE, check.names = FALSE)
  unit_ids <- enc2utf8(as.character(unit_ids))
  colour_frame <- as.data.frame(colour_frame, stringsAsFactors = FALSE, check.names = FALSE)
  radii <- tdabm_ir_assert_radii(radii, approved_radius)
  repetitions <- as.integer(repetitions); ncores <- as.integer(ncores); base_seed <- as.integer(base_seed)
  if (repetitions < 2L || ncores < 1L || base_seed < 1L) stop("Invalid robustness execution controls.", call. = FALSE)
  if (length(unit_ids) != nrow(axes) || nrow(colour_frame) != nrow(axes)) stop("Robustness inputs have inconsistent row counts.", call. = FALSE)
  if (any(!is.finite(as.matrix(axes))) || any(!is.finite(as.matrix(colour_frame)))) stop("Robustness inputs must be finite numeric values.", call. = FALSE)
  if (!identical(sort(names(canonical_local_by_colour)), sort(names(colour_frame)))) stop("Canonical local-colour inputs do not match colour variables.", call. = FALSE)
  if (any(vapply(canonical_local_by_colour, length, integer(1)) != nrow(axes))) stop("Canonical local-colour vectors have invalid length.", call. = FALSE)

  cpp_path <- normalizePath(cpp_path, winslash = "/", mustWork = TRUE)
  reps <- seq_len(repetitions)
  ncores <- min(ncores, repetitions)

  if (ncores == 1L) {
    tdabm_ir_compile_cpp(cpp_path)
    out <- lapply(reps, function(r) tdabm_ir_run_one_order(
      axes, unit_ids, colour_frame, canonical_local_by_colour,
      radii, approved_radius, r, base_seed, aggregation
    ))
  } else {
    module_path <- normalizePath(module_path %||% stop("module_path is required for parallel robustness.", call. = FALSE), winslash = "/", mustWork = TRUE)
    cl <- parallel::makePSOCKcluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    ok <- parallel::clusterCall(cl, tdabm_ir_worker_initialize, module_path = module_path, cpp_path = cpp_path)
    if (!all(vapply(ok, isTRUE, logical(1)))) stop("A robustness worker failed to initialize.", call. = FALSE)
    out <- parallel::parLapply(
      cl, reps,
      function(r, axes, unit_ids, colour_frame, canonical_local_by_colour, radii,
               approved_radius, base_seed, aggregation) {
        tdabm_ir_run_one_order(
          axes, unit_ids, colour_frame, canonical_local_by_colour,
          radii, approved_radius, r, base_seed, aggregation
        )
      },
      axes = axes,
      unit_ids = unit_ids,
      colour_frame = colour_frame,
      canonical_local_by_colour = canonical_local_by_colour,
      radii = radii,
      approved_radius = approved_radius,
      base_seed = base_seed,
      aggregation = aggregation
    )
  }

  topology <- do.call(rbind, lapply(out, `[[`, "topology"))
  comparison <- do.call(rbind, lapply(out, `[[`, "comparison"))
  points <- do.call(rbind, lapply(out, `[[`, "points"))
  topology <- topology[order(topology$radius, topology$replicate), , drop = FALSE]
  comparison <- comparison[order(comparison$colour_variable, comparison$radius, comparison$replicate), , drop = FALSE]
  points <- points[order(points$colour_variable, points$radius, points$replicate, points$point_index), , drop = FALSE]
  rownames(topology) <- rownames(comparison) <- rownames(points) <- NULL
  list(topology = topology, comparison = comparison, points = points)
}

tdabm_ir_q <- function(x, p) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, p, names = FALSE, type = 7, na.rm = FALSE))
}

tdabm_ir_safe_cor <- function(x, y, method) {
  x <- suppressWarnings(as.numeric(x)); y <- suppressWarnings(as.numeric(y))
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 2L || stats::sd(x) <= 0 || stats::sd(y) <= 0) return(NA_real_)
  as.numeric(stats::cor(x, y, method = method))
}

tdabm_ir_summarise_radius <- function(run) {
  topology <- run$topology
  comparison <- run$comparison
  keys <- unique(comparison[, c("colour_variable", "radius", "approved_radius", "is_approved_radius"), drop = FALSE])
  keys <- keys[order(keys$colour_variable, keys$radius), , drop = FALSE]
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    k <- keys[i, , drop = FALSE]
    csub <- comparison[comparison$colour_variable == k$colour_variable & abs(comparison$radius - k$radius) <= 1e-12, , drop = FALSE]
    tsub <- topology[abs(topology$radius - k$radius) <= 1e-12, , drop = FALSE]
    data.frame(
      colour_variable = k$colour_variable,
      radius = k$radius,
      approved_radius = k$approved_radius,
      is_approved_radius = k$is_approved_radius,
      repetitions = nrow(csub),
      median_n_balls = stats::median(tsub$n_balls),
      q10_n_balls = tdabm_ir_q(tsub$n_balls, 0.10),
      q90_n_balls = tdabm_ir_q(tsub$n_balls, 0.90),
      median_effective_n_balls = stats::median(tsub$effective_n_balls),
      median_max_ball_share_observations = stats::median(tsub$max_ball_share_observations),
      median_n_components = stats::median(tsub$n_components),
      median_n_isolated_balls = stats::median(tsub$n_isolated_balls),
      median_spearman_to_canonical = stats::median(csub$spearman_to_canonical, na.rm = TRUE),
      q10_spearman_to_canonical = tdabm_ir_q(csub$spearman_to_canonical, 0.10),
      median_pearson_to_canonical = stats::median(csub$pearson_to_canonical, na.rm = TRUE),
      median_rmse_to_canonical = stats::median(csub$rmse_to_canonical, na.rm = TRUE),
      q90_rmse_to_canonical = tdabm_ir_q(csub$rmse_to_canonical, 0.90),
      median_mae_to_canonical = stats::median(csub$mae_to_canonical, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

tdabm_ir_radius_point_mean_summary <- function(run) {
  pts <- run$points
  keys <- unique(pts[, c("colour_variable", "radius", "approved_radius", "is_approved_radius", "point_index", "unit_id"), drop = FALSE])
  keys <- keys[order(keys$colour_variable, keys$radius, keys$point_index), , drop = FALSE]
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    k <- keys[i, , drop = FALSE]
    x <- pts$local_colour[
      pts$colour_variable == k$colour_variable &
      abs(pts$radius - k$radius) <= 1e-12 &
      pts$point_index == k$point_index
    ]
    data.frame(
      colour_variable = k$colour_variable,
      radius = k$radius,
      approved_radius = k$approved_radius,
      is_approved_radius = k$is_approved_radius,
      point_index = k$point_index,
      unit_id = k$unit_id,
      repeated_order_mean_local_colour = mean(x),
      repeated_order_sd_local_colour = if (length(x) > 1L) stats::sd(x) else 0,
      repeated_order_q10_local_colour = tdabm_ir_q(x, 0.10),
      repeated_order_q90_local_colour = tdabm_ir_q(x, 0.90),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

tdabm_ir_radius_mean_pattern_summary <- function(point_mean_summary, approved_radius) {
  rows <- list(); k <- 0L
  for (v in sort(unique(point_mean_summary$colour_variable))) {
    base <- point_mean_summary[
      point_mean_summary$colour_variable == v & abs(point_mean_summary$radius - approved_radius) <= 1e-12,
      c("point_index", "repeated_order_mean_local_colour"), drop = FALSE
    ]
    names(base)[[2L]] <- "approved_repeated_mean"
    if (!nrow(base)) stop("Approved-radius repeated-order point means are missing.", call. = FALSE)
    for (radius in sort(unique(point_mean_summary$radius[point_mean_summary$colour_variable == v]))) {
      x <- point_mean_summary[
        point_mean_summary$colour_variable == v & abs(point_mean_summary$radius - radius) <= 1e-12,
        c("point_index", "repeated_order_mean_local_colour"), drop = FALSE
      ]
      names(x)[[2L]] <- "radius_repeated_mean"
      m <- merge(base, x, by = "point_index", sort = TRUE)
      if (nrow(m) != nrow(base)) stop("Radius point-mean comparison is incomplete.", call. = FALSE)
      k <- k + 1L
      rows[[k]] <- data.frame(
        colour_variable = v,
        radius = radius,
        approved_radius = approved_radius,
        is_approved_radius = abs(radius - approved_radius) <= 1e-12,
        pearson_to_approved_repeated_mean = tdabm_ir_safe_cor(m$radius_repeated_mean, m$approved_repeated_mean, "pearson"),
        spearman_to_approved_repeated_mean = tdabm_ir_safe_cor(m$radius_repeated_mean, m$approved_repeated_mean, "spearman"),
        rmse_to_approved_repeated_mean = sqrt(mean((m$radius_repeated_mean - m$approved_repeated_mean)^2)),
        mae_to_approved_repeated_mean = mean(abs(m$radius_repeated_mean - m$approved_repeated_mean)),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

tdabm_ir_landmark_order_observation_summary <- function(run, canonical_local_table, approved_radius) {
  pts <- run$points[abs(run$points$radius - approved_radius) <= 1e-12, , drop = FALSE]
  rows <- list()
  k <- 0L
  for (v in sort(unique(pts$colour_variable))) {
    ctab <- canonical_local_table[canonical_local_table$colour_variable == v, , drop = FALSE]
    for (i in sort(unique(pts$point_index))) {
      x <- pts$local_colour[pts$colour_variable == v & pts$point_index == i]
      canon <- ctab$canonical_local_colour[ctab$point_index == i]
      observed <- ctab$observed_value[ctab$point_index == i]
      if (length(canon) != 1L || length(observed) != 1L) stop("Canonical local-colour table is malformed.", call. = FALSE)
      q10 <- tdabm_ir_q(x, 0.10); q90 <- tdabm_ir_q(x, 0.90)
      k <- k + 1L
      rows[[k]] <- data.frame(
        colour_variable = v,
        point_index = i,
        unit_id = ctab$unit_id[ctab$point_index == i],
        unit_label = ctab$unit_label[ctab$point_index == i],
        observed_value = observed,
        canonical_local_colour = canon,
        repeated_order_mean = mean(x),
        repeated_order_sd = if (length(x) > 1L) stats::sd(x) else 0,
        repeated_order_q10 = q10,
        repeated_order_q90 = q90,
        canonical_minus_repeated_mean = canon - mean(x),
        absolute_canonical_minus_repeated_mean = abs(canon - mean(x)),
        canonical_inside_q10_q90 = canon >= q10 && canon <= q90,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

tdabm_ir_landmark_order_summary <- function(observation_summary) {
  rows <- lapply(sort(unique(observation_summary$colour_variable)), function(v) {
    x <- observation_summary[observation_summary$colour_variable == v, , drop = FALSE]
    data.frame(
      colour_variable = v,
      n_observations = nrow(x),
      canonical_vs_repeated_mean_pearson = tdabm_ir_safe_cor(x$canonical_local_colour, x$repeated_order_mean, "pearson"),
      canonical_vs_repeated_mean_spearman = tdabm_ir_safe_cor(x$canonical_local_colour, x$repeated_order_mean, "spearman"),
      rmse_canonical_vs_repeated_mean = sqrt(mean((x$canonical_local_colour - x$repeated_order_mean)^2)),
      median_absolute_difference = stats::median(x$absolute_canonical_minus_repeated_mean),
      q90_absolute_difference = tdabm_ir_q(x$absolute_canonical_minus_repeated_mean, 0.90),
      median_repeated_order_sd = stats::median(x$repeated_order_sd),
      q90_repeated_order_sd = tdabm_ir_q(x$repeated_order_sd, 0.90),
      share_canonical_inside_q10_q90 = mean(x$canonical_inside_q10_q90),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

tdabm_ir_compare_runs_exact <- function(a, b) {
  same_topology <- identical(a$topology, b$topology)
  same_comparison <- identical(a$comparison, b$comparison)
  same_points <- identical(a$points, b$points)
  data.frame(
    check = c("topology_exact", "comparison_exact", "point_local_exact"),
    pass = c(same_topology, same_comparison, same_points),
    stringsAsFactors = FALSE
  )
}
