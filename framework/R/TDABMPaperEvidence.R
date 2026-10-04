# ==============================================================================
# TDABMPaperEvidence.R
# ==============================================================================
# Application-neutral, read-only paper-evidence utilities for an already frozen
# TDABM topology. This module MUST NOT construct or modify topology.
# ==============================================================================

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

tdabm_pe_assert_scalar_character <- function(x, name) {
  if (length(x) != 1L || is.na(x) || !nzchar(as.character(x))) {
    stop(name, " must be one nonblank character value.", call. = FALSE)
  }
  enc2utf8(as.character(x))
}

tdabm_pe_assert_bm <- function(bm, n_points) {
  if (!is.list(bm)) stop("bm must be a frozen Ball Mapper object.", call. = FALSE)
  cover <- bm$points_covered_by_landmarks %||% NULL
  if (!is.list(cover) || !length(cover)) stop("Frozen topology has no cover.", call. = FALSE)
  idx <- suppressWarnings(as.integer(unlist(cover, use.names = FALSE)))
  if (!length(idx) || anyNA(idx) || any(idx < 1L | idx > n_points)) {
    stop("Frozen topology contains invalid membership indices.", call. = FALSE)
  }
  if (!identical(sort(unique(idx)), seq_len(as.integer(n_points)))) {
    stop("Frozen topology does not cover every observation.", call. = FALSE)
  }
  invisible(TRUE)
}

tdabm_pe_edge_table <- function(bm) {
  e <- as.matrix(bm$edges)
  if (length(e) == 0L) {
    return(data.frame(from = integer(), to = integer(), strength = integer(), stringsAsFactors = FALSE))
  }
  if (ncol(e) != 2L) stop("Frozen topology edge matrix must have two columns.", call. = FALSE)
  s <- suppressWarnings(as.integer(bm$strength_of_edges))
  if (length(s) != nrow(e)) stop("Frozen topology edge strengths are inconsistent.", call. = FALSE)
  data.frame(from = as.integer(e[, 1L]), to = as.integer(e[, 2L]), strength = s, stringsAsFactors = FALSE)
}

tdabm_pe_graph_structure <- function(bm) {
  n_balls <- length(bm$points_covered_by_landmarks)
  edges <- tdabm_pe_edge_table(bm)
  adjacency <- vector("list", n_balls)
  for (i in seq_len(n_balls)) adjacency[[i]] <- integer()
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      a <- edges$from[[i]]
      b <- edges$to[[i]]
      if (a < 1L || a > n_balls || b < 1L || b > n_balls) stop("Edge references an invalid ball.", call. = FALSE)
      adjacency[[a]] <- unique(c(adjacency[[a]], b))
      adjacency[[b]] <- unique(c(adjacency[[b]], a))
    }
  }
  degree <- vapply(adjacency, length, integer(1))
  component <- integer(n_balls)
  component_sizes <- integer()
  cid <- 0L
  for (v in seq_len(n_balls)) {
    if (component[[v]] != 0L) next
    cid <- cid + 1L
    queue <- v
    component[[v]] <- cid
    size <- 0L
    while (length(queue)) {
      current <- queue[[1L]]
      if (length(queue) == 1L) queue <- integer() else queue <- queue[-1L]
      size <- size + 1L
      for (w in adjacency[[current]]) {
        if (component[[w]] == 0L) {
          component[[w]] <- cid
          queue <- c(queue, w)
        }
      }
    }
    component_sizes <- c(component_sizes, size)
  }
  list(
    edges = edges,
    adjacency = adjacency,
    degree = degree,
    component = component,
    component_sizes = component_sizes
  )
}

tdabm_pe_membership_table <- function(bm, unit_ids, unit_labels = NULL) {
  unit_ids <- enc2utf8(as.character(unit_ids))
  if (is.null(unit_labels)) unit_labels <- unit_ids
  unit_labels <- enc2utf8(as.character(unit_labels))
  if (length(unit_ids) != length(unit_labels)) stop("unit_ids and unit_labels differ in length.", call. = FALSE)
  tdabm_pe_assert_bm(bm, length(unit_ids))
  rows <- lapply(seq_along(bm$points_covered_by_landmarks), function(ball_id) {
    idx <- sort(unique(as.integer(bm$points_covered_by_landmarks[[ball_id]])))
    data.frame(
      ball_id = as.integer(ball_id),
      point_index = idx,
      unit_id = unit_ids[idx],
      unit_label = unit_labels[idx],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

tdabm_pe_landmark_table <- function(bm, unit_ids, unit_labels = NULL) {
  unit_ids <- enc2utf8(as.character(unit_ids))
  if (is.null(unit_labels)) unit_labels <- unit_ids
  unit_labels <- enc2utf8(as.character(unit_labels))
  idx <- suppressWarnings(as.integer(bm$landmarks))
  if (length(idx) != length(bm$points_covered_by_landmarks) || anyNA(idx) ||
      any(idx < 1L | idx > length(unit_ids))) {
    stop("Frozen topology landmark indices are invalid.", call. = FALSE)
  }
  data.frame(
    ball_id = seq_along(idx),
    point_index = idx,
    unit_id = unit_ids[idx],
    unit_label = unit_labels[idx],
    stringsAsFactors = FALSE
  )
}

tdabm_pe_topology_summary <- function(bm, n_points, topology_fingerprint = NA_character_) {
  tdabm_pe_assert_bm(bm, n_points)
  graph <- tdabm_pe_graph_structure(bm)
  sizes <- vapply(bm$points_covered_by_landmarks, length, integer(1))
  membership_counts <- tabulate(
    suppressWarnings(as.integer(unlist(bm$points_covered_by_landmarks, use.names = FALSE))),
    nbins = n_points
  )
  weights <- sizes / sum(sizes)
  entropy <- -sum(weights * log(weights))
  effective <- exp(entropy)
  data.frame(
    n_observations = as.integer(n_points),
    radius = as.numeric(bm$epsilon),
    n_balls = length(sizes),
    n_edges = nrow(graph$edges),
    n_components = length(graph$component_sizes),
    n_isolated_balls = sum(graph$degree == 0L),
    largest_component_share_balls = max(graph$component_sizes) / length(sizes),
    min_ball_size = min(sizes),
    median_ball_size = stats::median(sizes),
    mean_ball_size = mean(sizes),
    max_ball_size = max(sizes),
    effective_n_balls = effective,
    max_ball_share_observations = max(sizes) / n_points,
    mean_memberships_per_observation = mean(membership_counts),
    fraction_multicovered = mean(membership_counts > 1L),
    topology_fingerprint_sha256 = as.character(topology_fingerprint),
    stringsAsFactors = FALSE
  )
}

tdabm_pe_overlap_tables <- function(bm, unit_ids, unit_labels = NULL) {
  membership <- tdabm_pe_membership_table(bm, unit_ids, unit_labels)
  split_balls <- split(membership$ball_id, membership$point_index)
  n <- length(unit_ids)
  counts <- integer(n)
  ids <- character(n)
  for (i in seq_len(n)) {
    b <- sort(unique(as.integer(split_balls[[as.character(i)]] %||% integer())))
    counts[[i]] <- length(b)
    ids[[i]] <- paste(b, collapse = ";")
  }
  observation <- data.frame(
    point_index = seq_len(n),
    unit_id = enc2utf8(as.character(unit_ids)),
    unit_label = enc2utf8(as.character(unit_labels %||% unit_ids)),
    membership_count = counts,
    ball_ids = ids,
    multicovered = counts > 1L,
    stringsAsFactors = FALSE
  )
  freq <- as.data.frame(table(counts), stringsAsFactors = FALSE)
  names(freq) <- c("membership_count", "n_observations")
  freq$membership_count <- as.integer(as.character(freq$membership_count))
  freq$share_observations <- freq$n_observations / n
  summary <- data.frame(
    n_observations = n,
    total_membership_records = nrow(membership),
    mean_memberships = mean(counts),
    median_memberships = stats::median(counts),
    max_memberships = max(counts),
    n_multicovered = sum(counts > 1L),
    share_multicovered = mean(counts > 1L),
    stringsAsFactors = FALSE
  )
  list(observation = observation, frequency = freq, summary = summary)
}

tdabm_pe_component_tables <- function(bm, unit_ids) {
  graph <- tdabm_pe_graph_structure(bm)
  membership <- tdabm_pe_membership_table(bm, unit_ids, unit_ids)
  ball_table <- data.frame(
    ball_id = seq_along(graph$component),
    component_id = graph$component,
    degree = graph$degree,
    isolated_ball = graph$degree == 0L,
    stringsAsFactors = FALSE
  )
  rows <- lapply(sort(unique(graph$component)), function(cid) {
    balls <- ball_table$ball_id[ball_table$component_id == cid]
    units <- sort(unique(membership$unit_id[membership$ball_id %in% balls]), method = "radix")
    data.frame(
      component_id = cid,
      n_balls = length(balls),
      n_unique_observations = length(units),
      isolated_component = length(balls) == 1L && ball_table$isolated_ball[ball_table$ball_id == balls[[1L]]],
      ball_ids = paste(balls, collapse = ";"),
      stringsAsFactors = FALSE
    )
  })
  list(ball = ball_table, component = do.call(rbind, rows))
}

tdabm_pe_profile_long <- function(bm, data, variables, variable_role, unit_ids) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  variables <- as.character(variables)
  if (!all(variables %in% names(data))) stop("Profile variable is missing from data.", call. = FALSE)
  rows <- list()
  k <- 0L
  for (v in variables) {
    values <- suppressWarnings(as.numeric(data[[v]]))
    if (length(values) != nrow(data) || any(!is.finite(values))) stop("Profile variables must be finite numeric values.", call. = FALSE)
    sample_mean <- mean(values)
    sample_sd <- stats::sd(values)
    for (ball_id in seq_along(bm$points_covered_by_landmarks)) {
      idx <- unique(as.integer(bm$points_covered_by_landmarks[[ball_id]]))
      x <- values[idx]
      k <- k + 1L
      rows[[k]] <- data.frame(
        ball_id = as.integer(ball_id),
        variable = v,
        variable_role = variable_role,
        n_members = length(idx),
        ball_mean = mean(x),
        ball_sd = if (length(x) > 1L) stats::sd(x) else 0,
        ball_min = min(x),
        ball_max = max(x),
        sample_mean = sample_mean,
        sample_sd = sample_sd,
        standardized_difference = if (is.finite(sample_sd) && sample_sd > 0) (mean(x) - sample_mean) / sample_sd else 0,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

tdabm_pe_ball_profile_wide <- function(
  bm, data, unit_ids, unit_labels, topology_variables, colour_variables = character()
) {
  membership <- tdabm_pe_membership_table(bm, unit_ids, unit_labels)
  landmarks <- tdabm_pe_landmark_table(bm, unit_ids, unit_labels)
  graph <- tdabm_pe_graph_structure(bm)
  out <- data.frame(
    ball_id = seq_along(bm$points_covered_by_landmarks),
    landmark_unit_id = landmarks$unit_id,
    landmark_unit_label = landmarks$unit_label,
    n_members = vapply(bm$points_covered_by_landmarks, length, integer(1)),
    component_id = graph$component,
    degree = graph$degree,
    isolated_ball = graph$degree == 0L,
    stringsAsFactors = FALSE
  )
  for (v in topology_variables) {
    values <- suppressWarnings(as.numeric(data[[v]]))
    means <- vapply(bm$points_covered_by_landmarks, function(idx) mean(values[as.integer(idx)]), numeric(1))
    s <- stats::sd(values)
    z <- if (is.finite(s) && s > 0) (means - mean(values)) / s else rep(0, length(means))
    out[[paste0("axis_mean__", v)]] <- means
    out[[paste0("axis_z__", v)]] <- z
  }
  for (v in colour_variables) {
    values <- suppressWarnings(as.numeric(data[[v]]))
    means <- vapply(bm$points_covered_by_landmarks, function(idx) mean(values[as.integer(idx)]), numeric(1))
    out[[paste0("colour_mean__", v)]] <- means
  }
  out
}

tdabm_pe_local_colour <- function(bm, values, unit_ids, unit_labels = NULL, aggregation = "equal_ball_mean") {
  if (!identical(aggregation, "equal_ball_mean")) stop("Unsupported local-colour aggregation.", call. = FALSE)
  values <- suppressWarnings(as.numeric(values))
  n <- length(unit_ids)
  if (length(values) != n || any(!is.finite(values))) stop("Local-colour values must be finite and match point count.", call. = FALSE)
  if (is.null(unit_labels)) unit_labels <- unit_ids
  ball_means <- vapply(
    bm$points_covered_by_landmarks,
    function(idx) mean(values[as.integer(idx)]),
    numeric(1)
  )
  memberships <- vector("list", n)
  for (ball_id in seq_along(bm$points_covered_by_landmarks)) {
    idx <- unique(as.integer(bm$points_covered_by_landmarks[[ball_id]]))
    for (i in idx) memberships[[i]] <- c(memberships[[i]], ball_id)
  }
  local <- numeric(n)
  n_memberships <- integer(n)
  ball_ids <- character(n)
  for (i in seq_len(n)) {
    b <- sort(unique(as.integer(memberships[[i]])))
    if (!length(b)) stop("An observation has no ball membership.", call. = FALSE)
    local[[i]] <- mean(ball_means[b])
    n_memberships[[i]] <- length(b)
    ball_ids[[i]] <- paste(b, collapse = ";")
  }
  data.frame(
    point_index = seq_len(n),
    unit_id = enc2utf8(as.character(unit_ids)),
    unit_label = enc2utf8(as.character(unit_labels)),
    observed_value = values,
    local_colour = local,
    membership_count = n_memberships,
    ball_ids = ball_ids,
    aggregation = aggregation,
    stringsAsFactors = FALSE
  )
}

tdabm_pe_fixed_layout <- function(bm, seed = 20260807L) {
  if (!requireNamespace("igraph", quietly = TRUE)) stop("Package 'igraph' is required for paper-map layout.", call. = FALSE)
  edges <- tdabm_pe_edge_table(bm)
  n <- length(bm$points_covered_by_landmarks)
  edge_df <- if (nrow(edges)) {
    data.frame(from = as.character(edges$from), to = as.character(edges$to), stringsAsFactors = FALSE)
  } else {
    data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  }
  vertices <- data.frame(name = as.character(seq_len(n)), stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(edge_df, directed = FALSE, vertices = vertices)
  set.seed(as.integer(seed))
  layout <- igraph::layout_with_fr(g)
  data.frame(ball_id = seq_len(n), x = layout[, 1L], y = layout[, 2L], stringsAsFactors = FALSE)
}

tdabm_pe_device_safe_text <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("\u2013|\u2014", "-", x)
  y <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "?")
  if (length(y) != length(x) || anyNA(y)) stop("Could not create device-safe plot text.", call. = FALSE)
  y
}

tdabm_pe_plot_fixed_map <- function(
  bm, layout_table, values, title, legend_title, png_file, pdf_file
) {
  if (!requireNamespace("igraph", quietly = TRUE)) stop("Package 'igraph' is required for paper maps.", call. = FALSE)
  title <- tdabm_pe_device_safe_text(title)
  legend_title <- tdabm_pe_device_safe_text(legend_title)
  values <- suppressWarnings(as.numeric(values))
  n <- length(bm$points_covered_by_landmarks)
  if (length(values) != n || any(!is.finite(values))) stop("Map values must be finite and match ball count.", call. = FALSE)
  if (!is.data.frame(layout_table) || !all(c("ball_id", "x", "y") %in% names(layout_table))) stop("Malformed fixed layout.", call. = FALSE)
  layout_table <- layout_table[match(seq_len(n), layout_table$ball_id), , drop = FALSE]
  if (anyNA(layout_table$ball_id)) stop("Fixed layout does not cover all balls.", call. = FALSE)

  edges <- tdabm_pe_edge_table(bm)
  edge_df <- if (nrow(edges)) {
    data.frame(from = as.character(edges$from), to = as.character(edges$to), stringsAsFactors = FALSE)
  } else {
    data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  }
  vertices <- data.frame(name = as.character(seq_len(n)), stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(edge_df, directed = FALSE, vertices = vertices)
  sizes <- vapply(bm$points_covered_by_landmarks, length, integer(1))
  rng <- range(values)
  scaled <- if (diff(rng) == 0) rep(0.5, n) else (values - rng[[1L]]) / diff(rng)
  palette <- grDevices::hcl.colors(101, "YlOrRd", rev = FALSE)
  cols <- palette[pmax(1L, pmin(101L, 1L + as.integer(round(scaled * 100))))]
  layout <- as.matrix(layout_table[, c("x", "y"), drop = FALSE])

  draw <- function(path, kind) {
    if (kind == "png") {
      grDevices::png(path, width = 2000, height = 1400, res = 200)
    } else {
      grDevices::pdf(path, width = 10, height = 7, useDingbats = FALSE)
    }
    on.exit(grDevices::dev.off(), add = TRUE)
    graphics::layout(matrix(c(1, 2), nrow = 1L), widths = c(8.7, 1.3))
    graphics::par(mar = c(1, 1, 4, 1))
    plot(
      g,
      layout = layout,
      vertex.color = cols,
      vertex.size = 10 + 3 * sqrt(sizes),
      vertex.label = seq_len(n),
      vertex.label.cex = 0.85,
      edge.width = 1,
      main = title
    )
    graphics::par(mar = c(4, 1, 4, 4))
    z <- matrix(seq(rng[[1L]], rng[[2L]], length.out = 101), ncol = 1L)
    graphics::image(
      x = 1, y = seq(rng[[1L]], rng[[2L]], length.out = 101),
      z = t(z), col = palette, axes = FALSE, xlab = "", ylab = ""
    )
    graphics::axis(4)
    graphics::mtext(legend_title, side = 4, line = 2.2)
    graphics::box()
  }

  dir.create(dirname(png_file), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(pdf_file), recursive = TRUE, showWarnings = FALSE)
  draw(png_file, "png")
  draw(pdf_file, "pdf")
  invisible(TRUE)
}
