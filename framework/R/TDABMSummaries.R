# ==============================================================================
# TDABMSummaries.R
# ==============================================================================
# Evidence-pack and summary-table layer for reusable TDABM projects.
#
# Stage 7 purpose:
#   This module reads completed framework production outputs and creates compact,
#   paper-ready summary files. It does not run BMStats(), BMStatsMembership(),
#   robustness analyses, or any expensive computation.
#
# Main public function:
#   BuildTDABMEvidencePack(project)
#
# Source order:
#   source("R/TDABMFramework.R")
#   source("R/TDABMUtilities.R")
#   source("R/TDABMProject.R")
#   source("R/TDABMValidation.R")
#   source("R/TDABMExecution.R")
#   source("R/TDABMMembership.R")
#   source("R/TDABMRobustness.R")
#   source("R/TDABMOrchestrator.R")
#   source("R/TDABMSummaries.R")
# ==============================================================================

tdabm_summary_require_packages <- function(packages = c("data.table")) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) > 0L) {
    stop(
      "Install the missing packages before building TDABM summaries:\n  install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "),
      "))",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

tdabm_safe_fread <- function(file) {
  if (!file.exists(file)) {
    stop("File not found: ", file, call. = FALSE)
  }
  data.table::fread(file)
}

tdabm_rbind_fill <- function(x) {
  x <- Filter(Negate(is.null), x)
  if (length(x) == 0L) {
    return(data.table::data.table())
  }
  data.table::rbindlist(
    lapply(x, data.table::as.data.table),
    use.names = TRUE,
    fill = TRUE
  )
}

tdabm_robustness_id_from_path <- function(file) {
  parts <- strsplit(
    normalizePath(file, winslash = "/", mustWork = FALSE),
    "/",
    fixed = TRUE
  )[[1L]]

  idx <- which(parts == "robustness")

  if (length(idx) == 0L || idx[[1L]] >= length(parts)) {
    return(NA_character_)
  }

  parts[[idx[[1L]] + 1L]]
}

tdabm_run_id_from_radius_summary_file <- function(file) {
  sub("_radius_summary\\.csv$", "", basename(file))
}

tdabm_eps_column <- function(x) {
  candidates <- c("eps", "epsilon", "radius", "r")
  hit <- intersect(candidates, names(x))
  if (length(hit) == 0L) NA_character_ else hit[[1L]]
}

tdabm_write_table <- function(x, file) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fwrite(data.table::as.data.table(x), file)
  } else {
    utils::write.csv(as.data.frame(x), file, row.names = FALSE)
  }
  invisible(file)
}

tdabm_collect_radius_summary_files <- function(output_root) {
  primary_dir <- file.path(output_root, "verification")
  robustness_dir <- file.path(output_root, "robustness")

  primary_files <- if (dir.exists(primary_dir)) {
    list.files(
      primary_dir,
      pattern = "_radius_summary\\.csv$",
      full.names = TRUE,
      recursive = FALSE
    )
  } else {
    character(0)
  }

  robustness_files <- if (dir.exists(robustness_dir)) {
    list.files(
      robustness_dir,
      pattern = "_radius_summary\\.csv$",
      full.names = TRUE,
      recursive = TRUE
    )
  } else {
    character(0)
  }

  data.table::data.table(
    file = c(primary_files, robustness_files),
    source_scope = c(
      rep("primary", length(primary_files)),
      rep("robustness", length(robustness_files))
    ),
    robustness_id = c(
      rep(NA_character_, length(primary_files)),
      vapply(robustness_files, tdabm_robustness_id_from_path, character(1))
    ),
    run_id = c(
      vapply(primary_files, tdabm_run_id_from_radius_summary_file, character(1)),
      vapply(robustness_files, tdabm_run_id_from_radius_summary_file, character(1))
    )
  )
}

tdabm_read_radius_summaries <- function(output_root) {
  files <- tdabm_collect_radius_summary_files(output_root)

  if (nrow(files) == 0L) {
    return(data.table::data.table())
  }

  rows <- lapply(seq_len(nrow(files)), function(i) {
    meta <- files[i]
    x <- tdabm_safe_fread(meta$file)

    eps_col <- tdabm_eps_column(x)

    if (is.na(eps_col)) {
      warning("Skipping radius-summary file without an epsilon/radius column: ", meta$file)
      return(NULL)
    }

    x <- data.table::as.data.table(x)

    if (!identical(eps_col, "eps")) {
      data.table::setnames(x, eps_col, "eps")
    }

    x[, source_scope := meta$source_scope]
    x[, robustness_id := meta$robustness_id]
    x[, run_id := meta$run_id]
    x[, summary_file := meta$file]

    front <- intersect(
      c("source_scope", "robustness_id", "run_id", "eps", "summary_file"),
      names(x)
    )

    x[, c(front, setdiff(names(x), front)), with = FALSE]
  })

  tdabm_rbind_fill(rows)
}

tdabm_enrich_with_project_specs <- function(x, project = NULL) {
  x <- data.table::as.data.table(x)

  if (is.null(project) || nrow(x) == 0L) {
    return(x)
  }

  if (!is.null(project$colourings) && is.data.frame(project$colourings) && "run_id" %in% names(x)) {
    colour_cols <- intersect(
      c("run_id", "variable", "label", "short_label", "family", "colour_type", "priority"),
      names(project$colourings)
    )

    colour_specs <- data.table::as.data.table(project$colourings[, colour_cols, drop = FALSE])

    drop_cols <- setdiff(intersect(names(colour_specs), names(x)), "run_id")
    if (length(drop_cols) > 0L) {
      x[, (drop_cols) := NULL]
    }

    x <- merge(x, colour_specs, by = "run_id", all.x = TRUE, sort = FALSE)
  }

  if (
    !is.null(project$robustness) &&
      is.data.frame(project$robustness) &&
      "robustness_id" %in% names(x)
  ) {
    robust_cols <- intersect(
      c("robustness_id", "type", "variable", "exclude", "method", "enabled"),
      names(project$robustness)
    )

    robust_specs <- data.table::as.data.table(project$robustness[, robust_cols, drop = FALSE])
    other_cols <- setdiff(names(robust_specs), "robustness_id")

    if (length(other_cols) > 0L) {
      data.table::setnames(
        robust_specs,
        old = other_cols,
        new = paste0("robustness_", other_cols)
      )
    }

    drop_cols <- setdiff(intersect(names(robust_specs), names(x)), "robustness_id")
    if (length(drop_cols) > 0L) {
      x[, (drop_cols) := NULL]
    }

    x <- merge(x, robust_specs, by = "robustness_id", all.x = TRUE, sort = FALSE)
  }

  x
}

tdabm_metric_columns <- function(x) {
  numeric_cols <- names(x)[vapply(x, is.numeric, logical(1))]

  setdiff(
    numeric_cols,
    c(
      "eps",
      "radius_target",
      "radius_distance",
      "priority",
      "robustness_enabled"
    )
  )
}

tdabm_nearest_radius_rows <- function(
  x,
  group_cols,
  radius_value = 1.00
) {
  x <- data.table::copy(data.table::as.data.table(x))

  if (nrow(x) == 0L) {
    return(x)
  }

  if (!"eps" %in% names(x)) {
    stop("Radius-summary table must contain an 'eps' column.", call. = FALSE)
  }

  group_cols <- intersect(group_cols, names(x))

  if (length(group_cols) == 0L) {
    x[, group_all__ := "all"]
    group_cols <- "group_all__"
  }

  x[, radius_target := radius_value]
  x[, radius_distance := abs(eps - radius_value)]

  data.table::setorderv(x, c(group_cols, "radius_distance", "eps"))

  out <- x[, .SD[1L], by = group_cols]

  if ("group_all__" %in% names(out)) {
    out[, group_all__ := NULL]
  }

  out
}

tdabm_radius_window_metric_summary <- function(
  x,
  group_cols,
  radius_value = 1.00,
  window = c(0.83, 1.06)
) {
  x <- data.table::copy(data.table::as.data.table(x))

  if (nrow(x) == 0L) {
    return(data.table::data.table())
  }

  if (!"eps" %in% names(x)) {
    stop("Radius-summary table must contain an 'eps' column.", call. = FALSE)
  }

  group_cols <- intersect(group_cols, names(x))

  if (length(group_cols) == 0L) {
    stop("No grouping columns are available in the radius-summary table.", call. = FALSE)
  }

  metric_cols <- tdabm_metric_columns(x)

  if (length(metric_cols) == 0L) {
    return(data.table::data.table())
  }

  window_x <- x[eps >= window[[1L]] & eps <= window[[2L]]]

  if (nrow(window_x) == 0L) {
    return(data.table::data.table())
  }

  window_x[, (metric_cols) := lapply(.SD, as.numeric), .SDcols = metric_cols]

  long <- data.table::melt(
    window_x,
    id.vars = c(group_cols, "eps"),
    measure.vars = metric_cols,
    variable.name = "metric",
    value.name = "value"
  )

  summary <- long[
    ,
    list(
      window_n = .N,
      window_eps_min = min(eps, na.rm = TRUE),
      window_eps_max = max(eps, na.rm = TRUE),
      window_mean = mean(value, na.rm = TRUE),
      window_sd = stats::sd(value, na.rm = TRUE),
      window_min = min(value, na.rm = TRUE),
      window_max = max(value, na.rm = TRUE)
    ),
    by = c(group_cols, "metric")
  ]

  radius_rows <- tdabm_nearest_radius_rows(
    x,
    group_cols = group_cols,
    radius_value = radius_value
  )

  radius_rows[, (metric_cols) := lapply(.SD, as.numeric), .SDcols = metric_cols]

  radius_long <- data.table::melt(
    radius_rows,
    id.vars = c(group_cols, "eps", "radius_target", "radius_distance"),
    measure.vars = metric_cols,
    variable.name = "metric",
    value.name = "value_at_radius"
  )

  data.table::setnames(radius_long, "eps", "nearest_radius")

  out <- merge(
    summary,
    radius_long,
    by = c(group_cols, "metric"),
    all.x = TRUE,
    sort = FALSE
  )

  out[, radius_minus_window_mean := value_at_radius - window_mean]

  out
}

tdabm_robustness_delta_long <- function(
  primary_radius_rows,
  robustness_radius_rows,
  project = NULL
) {
  primary_radius_rows <- data.table::as.data.table(primary_radius_rows)
  robustness_radius_rows <- data.table::as.data.table(robustness_radius_rows)

  if (nrow(primary_radius_rows) == 0L || nrow(robustness_radius_rows) == 0L) {
    return(data.table::data.table())
  }

  primary_radius_rows <- tdabm_enrich_with_project_specs(primary_radius_rows, project)
  robustness_radius_rows <- tdabm_enrich_with_project_specs(robustness_radius_rows, project)

  metric_cols <- intersect(
    tdabm_metric_columns(primary_radius_rows),
    tdabm_metric_columns(robustness_radius_rows)
  )

  if (length(metric_cols) == 0L) {
    return(data.table::data.table())
  }

  p <- primary_radius_rows[, c("run_id", metric_cols), with = FALSE]

  robust_id_vars <- intersect(
    c(
      "robustness_id",
      "robustness_type",
      "robustness_variable",
      "robustness_exclude",
      "robustness_method",
      "source_scope",
      "run_id"
    ),
    names(robustness_radius_rows)
  )

  r <- robustness_radius_rows[, c(robust_id_vars, metric_cols), with = FALSE]

  p[, (metric_cols) := lapply(.SD, as.numeric), .SDcols = metric_cols]
  r[, (metric_cols) := lapply(.SD, as.numeric), .SDcols = metric_cols]

  p_long <- data.table::melt(
    p,
    id.vars = "run_id",
    measure.vars = metric_cols,
    variable.name = "metric",
    value.name = "primary_value"
  )

  r_long <- data.table::melt(
    r,
    id.vars = robust_id_vars,
    measure.vars = metric_cols,
    variable.name = "metric",
    value.name = "robustness_value"
  )

  out <- merge(
    r_long,
    p_long,
    by = c("run_id", "metric"),
    all.x = TRUE,
    sort = FALSE
  )

  out[, difference := robustness_value - primary_value]
  out[, absolute_difference := abs(difference)]
  out[, relative_difference := ifelse(
    !is.na(primary_value) & primary_value != 0,
    difference / primary_value,
    NA_real_
  )]

  out
}

tdabm_production_inventory <- function(output_root) {
  files <- list.files(output_root, recursive = TRUE, full.names = TRUE)

  if (length(files) == 0L) {
    return(data.table::data.table())
  }

  info <- file.info(files)

  output_root_norm <- normalizePath(output_root, winslash = "/", mustWork = FALSE)
  file_norm <- normalizePath(files, winslash = "/", mustWork = FALSE)

  inventory <- data.table::data.table(
    file = file_norm,
    relative_file = sub(
      paste0("^", gsub("([\\W])", "\\\\\\1", output_root_norm), "/?"),
      "",
      file_norm
    ),
    size_bytes = as.numeric(info$size),
    modified = as.character(info$mtime),
    extension = tools::file_ext(files)
  )

  inventory[, output_section := "other"]
  inventory[grepl("/verification/", file), output_section := "verification"]
  inventory[grepl("/memberships/", file), output_section := "memberships"]
  inventory[grepl("/robustness/", file), output_section := "robustness"]
  inventory[grepl("/summary_plots/", file), output_section := "summary_plots"]
  inventory[grepl("/logs/", file), output_section := "logs"]
  inventory[grepl("/checkpoints/", file), output_section := "checkpoints"]
  inventory[grepl("/evidence_pack/", file), output_section := "evidence_pack"]

  inventory[, file_role := "other"]
  inventory[grepl("_result\\.rds$", file), file_role := "result_rds"]
  inventory[grepl("_radius_summary\\.csv$", file), file_role := "radius_summary"]
  inventory[grepl("_repetitions\\.csv$", file), file_role := "repetitions"]
  inventory[grepl("radius_1|radius1", file), file_role := "radius_1"]
  inventory[grepl("audit", file), file_role := "audit"]
  inventory[grepl("validation", file), file_role := "validation"]
  inventory[grepl("\\.(png|pdf|jpg|jpeg)$", file), file_role := "figure"]

  inventory
}

tdabm_read_focal_membership_tables <- function(output_root) {
  membership_dir <- file.path(output_root, "memberships")

  if (!dir.exists(membership_dir)) {
    return(list(
      focal_authority = data.table::data.table(),
      focal_isolation = data.table::data.table()
    ))
  }

  authority_files <- list.files(
    membership_dir,
    pattern = "focal_authority_summary\\.csv$",
    full.names = TRUE
  )

  isolation_files <- list.files(
    membership_dir,
    pattern = "focal_isolation_summary\\.csv$",
    full.names = TRUE
  )

  read_many <- function(files) {
    if (length(files) == 0L) {
      return(data.table::data.table())
    }

    tdabm_rbind_fill(lapply(files, function(f) {
      x <- tdabm_safe_fread(f)
      x[, source_file := f]
      x
    }))
  }

  list(
    focal_authority = read_many(authority_files),
    focal_isolation = read_many(isolation_files)
  )
}

tdabm_write_markdown_evidence_report <- function(
  evidence_dir,
  output_root,
  files,
  primary_dashboard,
  robustness_dashboard,
  robustness_delta,
  focal_tables,
  radius_value,
  window
) {
  report_file <- file.path(evidence_dir, "tdabm_evidence_pack_report.md")

  lines <- c(
    "# TDABM Evidence Pack",
    "",
    paste0("Generated: ", as.character(Sys.time())),
    "",
    paste0("Output root: `", output_root, "`"),
    "",
    "## Radius settings used in this evidence pack",
    "",
    paste0("- Focal radius: `", radius_value, "`"),
    paste0("- Window: `", window[[1L]], "` to `", window[[2L]], "`"),
    "",
    "## Core tables",
    "",
    paste0("- Primary dashboard rows: `", nrow(primary_dashboard), "`"),
    paste0("- Robustness dashboard rows: `", nrow(robustness_dashboard), "`"),
    paste0("- Robustness delta rows: `", nrow(robustness_delta), "`"),
    paste0("- Focal authority rows: `", nrow(focal_tables$focal_authority), "`"),
    paste0("- Focal isolation rows: `", nrow(focal_tables$focal_isolation), "`"),
    "",
    "## Files generated",
    ""
  )

  for (nm in names(files)) {
    lines <- c(lines, paste0("- `", nm, "`: `", files[[nm]], "`"))
  }

  lines <- c(
    lines,
    "",
    "## Suggested reading order",
    "",
    "1. `primary_stability_dashboard.csv`",
    "2. `primary_radius_window_metrics.csv`",
    "3. `focal_isolation_summary.csv`",
    "4. `robustness_radius1_dashboard.csv`",
    "5. `robustness_delta_vs_primary_radius1.csv`",
    "",
    "## Notes",
    "",
    "This evidence pack is generated from completed framework outputs only. It does not rerun Ball Mapper.",
    "The robustness delta table compares each robustness run with the matching primary run at the focal radius."
  )

  writeLines(lines, report_file)
  report_file
}

BuildTDABMEvidencePack <- function(
  project = NULL,
  output_root = NULL,
  evidence_dir = NULL,
  radius_value = 1.00,
  window = c(0.83, 1.06),
  overwrite = TRUE,
  verbose = TRUE
) {
  tdabm_summary_require_packages()

  if (!is.null(project)) {
    if (!is.TDABMProject(project)) {
      stop("project must be a TDABMProject object.", call. = FALSE)
    }
    output_root <- output_root %||% project$outputs$paths$root
  }

  if (is.null(output_root) || length(output_root) != 1L || is.na(output_root) || !nzchar(output_root)) {
    stop("Provide output_root or a TDABMProject object.", call. = FALSE)
  }

  output_root <- normalizePath(output_root, winslash = "/", mustWork = FALSE)

  if (!dir.exists(output_root)) {
    stop("Output root does not exist: ", output_root, call. = FALSE)
  }

  evidence_dir <- evidence_dir %||% file.path(output_root, "evidence_pack")

  if (dir.exists(evidence_dir) && !isTRUE(overwrite)) {
    stop("Evidence directory already exists and overwrite=FALSE: ", evidence_dir, call. = FALSE)
  }

  tdabm_create_dir(evidence_dir)

  if (isTRUE(verbose)) {
    message("Building TDABM evidence pack from: ", output_root)
    message("Evidence directory: ", evidence_dir)
  }

  inventory <- tdabm_production_inventory(output_root)
  all_summaries <- tdabm_read_radius_summaries(output_root)
  all_summaries <- tdabm_enrich_with_project_specs(all_summaries, project)

  if (nrow(all_summaries) == 0L) {
    stop("No radius summary files were found under: ", output_root, call. = FALSE)
  }

  primary_summaries <- all_summaries[source_scope == "primary"]
  robustness_summaries <- all_summaries[source_scope == "robustness"]

  primary_radius1 <- tdabm_nearest_radius_rows(
    primary_summaries,
    group_cols = "run_id",
    radius_value = radius_value
  )
  primary_radius1 <- tdabm_enrich_with_project_specs(primary_radius1, project)

  primary_window_metrics <- tdabm_radius_window_metric_summary(
    primary_summaries,
    group_cols = "run_id",
    radius_value = radius_value,
    window = window
  )
  primary_window_metrics <- tdabm_enrich_with_project_specs(primary_window_metrics, project)

  robustness_radius1 <- tdabm_nearest_radius_rows(
    robustness_summaries,
    group_cols = c("robustness_id", "run_id"),
    radius_value = radius_value
  )
  robustness_radius1 <- tdabm_enrich_with_project_specs(robustness_radius1, project)

  all_window_metrics <- tdabm_radius_window_metric_summary(
    all_summaries,
    group_cols = c("source_scope", "robustness_id", "run_id"),
    radius_value = radius_value,
    window = window
  )
  all_window_metrics <- tdabm_enrich_with_project_specs(all_window_metrics, project)

  robustness_delta <- tdabm_robustness_delta_long(
    primary_radius_rows = primary_radius1,
    robustness_radius_rows = robustness_radius1,
    project = project
  )

  focal_tables <- tdabm_read_focal_membership_tables(output_root)

  files <- list(
    production_inventory = file.path(evidence_dir, "production_inventory.csv"),
    all_radius_summaries = file.path(evidence_dir, "all_radius_summaries.csv"),
    primary_stability_dashboard = file.path(evidence_dir, "primary_stability_dashboard.csv"),
    primary_radius_window_metrics = file.path(evidence_dir, "primary_radius_window_metrics.csv"),
    robustness_radius1_dashboard = file.path(evidence_dir, "robustness_radius1_dashboard.csv"),
    robustness_delta_vs_primary_radius1 = file.path(evidence_dir, "robustness_delta_vs_primary_radius1.csv"),
    all_radius_window_metrics = file.path(evidence_dir, "all_radius_window_metrics.csv"),
    focal_authority_summary = file.path(evidence_dir, "focal_authority_summary.csv"),
    focal_isolation_summary = file.path(evidence_dir, "focal_isolation_summary.csv"),
    evidence_pack_file_index = file.path(evidence_dir, "evidence_pack_file_index.csv")
  )

  tdabm_write_table(inventory, files$production_inventory)
  tdabm_write_table(all_summaries, files$all_radius_summaries)
  tdabm_write_table(primary_radius1, files$primary_stability_dashboard)
  tdabm_write_table(primary_window_metrics, files$primary_radius_window_metrics)
  tdabm_write_table(robustness_radius1, files$robustness_radius1_dashboard)
  tdabm_write_table(robustness_delta, files$robustness_delta_vs_primary_radius1)
  tdabm_write_table(all_window_metrics, files$all_radius_window_metrics)
  tdabm_write_table(focal_tables$focal_authority, files$focal_authority_summary)
  tdabm_write_table(focal_tables$focal_isolation, files$focal_isolation_summary)

  report_file <- tdabm_write_markdown_evidence_report(
    evidence_dir = evidence_dir,
    output_root = output_root,
    files = files,
    primary_dashboard = primary_radius1,
    robustness_dashboard = robustness_radius1,
    robustness_delta = robustness_delta,
    focal_tables = focal_tables,
    radius_value = radius_value,
    window = window
  )

  files$markdown_report <- report_file

  file_index <- data.table::data.table(
    name = names(files),
    file = unlist(files, use.names = FALSE),
    exists = file.exists(unlist(files, use.names = FALSE)),
    size_bytes = as.numeric(file.info(unlist(files, use.names = FALSE))$size)
  )

  tdabm_write_table(file_index, files$evidence_pack_file_index)

  result <- list(
    output_root = output_root,
    evidence_dir = evidence_dir,
    radius_value = radius_value,
    window = window,
    files = files,
    file_index = file_index,
    inventory = inventory,
    all_summaries = all_summaries,
    primary_dashboard = primary_radius1,
    primary_window_metrics = primary_window_metrics,
    robustness_dashboard = robustness_radius1,
    robustness_delta = robustness_delta,
    all_window_metrics = all_window_metrics,
    focal_tables = focal_tables
  )

  saveRDS(result, file.path(evidence_dir, "tdabm_evidence_pack.rds"))

  if (isTRUE(verbose)) {
    message("\nTDABM evidence pack complete.")
    message("Evidence directory: ", evidence_dir)
    message("Report: ", report_file)
  }

  invisible(result)
}
