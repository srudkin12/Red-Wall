required <- c("BallMapper", "Rcpp", "data.table", "digest", "doSNOW", "dplyr", "fields", "foreach", "ggplot2", "igraph", "png")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) == 0L) {
  message("All declared TDABM dependencies are installed.")
} else {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = getOption("repos"))
}
