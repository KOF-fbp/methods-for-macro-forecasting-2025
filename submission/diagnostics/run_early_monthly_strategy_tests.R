#!/usr/bin/env Rscript

if (!interactive()) {
  args_all <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_all, value = TRUE)
  if (length(file_arg)) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)
    project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
    setwd(project_root)
  }
}

strategies <- c("fill", "omit")
diag_dir <- file.path("output", "diagnostics", "early_monthly_strategy")
if (!dir.exists(diag_dir)) dir.create(diag_dir, recursive = TRUE)

run_strategy <- function(strategy) {
  cat(sprintf("\n=== Running benchmarks with early-monthly strategy: %s ===\n", strategy))
  args <- c("main.R", "benchmarks", "--fast", "--no-cv", paste0("--early-monthly=", strategy))
  status <- system2("Rscript", args)
  if (!identical(status, 0L)) {
    stop(sprintf("Benchmark run failed for strategy '%s' (exit code %s)", strategy, status))
  }

  suffix <- paste0("_strategy_", strategy)
  copy_map <- list(
    list(
      src = file.path("output", "benchmarks", "csv", "model_benchmark_metrics.csv"),
      dest = file.path(diag_dir, paste0("model_benchmark_metrics", suffix, ".csv"))
    ),
    list(
      src = file.path("output", "benchmarks", "csv", "model_benchmark_holdout_detailed.csv"),
      dest = file.path(diag_dir, paste0("model_benchmark_holdout_detailed", suffix, ".csv"))
    ),
    list(
      src = file.path("output", "benchmarks", "model_benchmark_summary.md"),
      dest = file.path(diag_dir, paste0("model_benchmark_summary", suffix, ".md"))
    )
  )

  for (entry in copy_map) {
    src <- entry$src
    dest <- entry$dest
    if (!file.exists(src)) {
      warning(sprintf("Expected output '%s' not found after strategy '%s' run.", src, strategy))
      next
    }
    ok <- file.copy(src, dest, overwrite = TRUE)
    if (!ok) {
      warning(sprintf("Failed to copy '%s' to '%s'", src, dest))
    }
  }
}

for (strategy in strategies) {
  run_strategy(strategy)
}

cat("\nDiagnostics complete. Strategy-specific outputs stored in:", diag_dir, "\n")
