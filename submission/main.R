#!/usr/bin/env Rscript

# Main entry point for Swiss Macro Forecasting Pipeline
# ========================================================
# 
# This script provides a unified interface to run different workflows:
#   - MF-VAR estimation and forecasting (default)
#   - Benchmark model comparison
#   - Environment verification (Docker/CI mode)
#
# Usage:
#   Rscript main.R [workflow]
#
# Workflows:
#   mfvar      - Run MF-VAR pipeline (default)
#   benchmark  - Run benchmark model comparison
#   verify     - Environment verification only (Docker/CI)
#   help       - Show this help message

args <- commandArgs(trailingOnly = TRUE)
workflow <- if (length(args) >= 1) args[1] else "mfvar"

show_help <- function() {
  cat("
Swiss Macro Forecasting Pipeline
=================================

Usage: Rscript main.R [workflow]

Available workflows:
  mfvar      - Run Mixed-Frequency VAR estimation and forecasting (default)
               Produces forecasts, evaluation tables, and plots.
               Output: output/mfvar_forecasts_*.csv, mfvar_summary.txt, plots
               
  midas      - Run MIDAS regression estimation and forecasting
               Produces forecasts using mixed-data sampling approach.
               Output: output/midas_forecasts_*.csv, midas_summary.txt, plots
               
  benchmarks - Run benchmark model comparison (MF-VAR vs MIDAS vs AR(2) vs RW-trend)
               Includes holdout evaluation and cross-validation.
               Output: output/model_benchmark_*.csv, comparison plots
               
  verify     - Environment verification (for Docker/CI)
               Quick smoke test to validate package installation and data access.
               Output: Basic diagnostic messages only
               
  help       - Show this help message

Examples:
  Rscript main.R              # Run MF-VAR pipeline (default)
  Rscript main.R mfvar        # Explicitly run MF-VAR
  Rscript main.R midas        # Run MIDAS
  Rscript main.R benchmarks   # Run benchmark comparison
  Rscript main.R verify       # Verify environment setup

For more details, see README.md
")
  quit(save = "no", status = 0)
}

verify_environment <- function() {
  cat("\n=== Environment Verification ===\n\n")
  
  # Check R version
  cat("R version:", R.version.string, "\n")
  
  # Check renv
  if (file.exists("renv/activate.R")) {
    cat("✓ renv infrastructure present\n")
    tryCatch({
      renv::status()
      cat("✓ renv status check passed\n")
    }, error = function(e) {
      cat("✗ renv status check failed:", conditionMessage(e), "\n")
    })
  } else {
    cat("✗ renv not found\n")
  }
  
  # Check key packages
  required_pkgs <- c("mfbvar", "kofdata", "dplyr", "ggplot2", "readr")
  cat("\nChecking key packages:\n")
  for (pkg in required_pkgs) {
    has_pkg <- requireNamespace(pkg, quietly = TRUE)
    status <- if (has_pkg) "✓" else "✗"
    cat(sprintf("  %s %s\n", status, pkg))
  }
  
  # Check data directory
  cat("\nChecking data files:\n")
  data_file <- file.path("data", "processed", "data_quarterly.csv")
  if (file.exists(data_file)) {
    cat("✓ Quarterly data found:", data_file, "\n")
    df <- tryCatch(
      utils::read.csv(data_file, nrows = 3),
      error = function(e) NULL
    )
    if (!is.null(df)) {
      cat("  - Columns:", paste(names(df), collapse = ", "), "\n")
      cat("  - Rows:", nrow(df), "(sample)\n")
    }
  } else {
    cat("✗ Quarterly data not found at:", data_file, "\n")
  }
  
  # Check output directory
  if (dir.exists("output")) {
    cat("✓ Output directory exists\n")
  } else {
    cat("⚠ Output directory not found, will be created on first run\n")
  }
  
  cat("\n=== Verification Complete ===\n")
  cat("Ready to run workflows.\n")
  cat("Try: Rscript main.R mfvar\n\n")
}

run_mfvar <- function() {
  cat("\n=== Running MF-VAR Pipeline (Package Implementation) ===\n\n")
  cat("Starting Mixed-Frequency VAR estimation and forecasting...\n")
  cat("This may take several minutes.\n\n")
  
  tryCatch({
    source("run_mfvar_package.R", local = new.env())
    cat("\n✓ MF-VAR pipeline completed successfully!\n")
    cat("Check output/ directory for results.\n\n")
  }, error = function(e) {
    cat("\n✗ MF-VAR pipeline failed:\n")
    cat(conditionMessage(e), "\n")
    quit(save = "no", status = 1)
  })
}

run_midas <- function() {
  cat("\n=== Running MIDAS Pipeline ===\n\n")
  cat("Starting MIDAS regression estimation and forecasting...\n")
  cat("This may take several minutes.\n\n")
  
  tryCatch({
    source("run_midas.R", local = new.env())
    cat("\n✓ MIDAS pipeline completed successfully!\n")
    cat("Check output/ directory for results.\n\n")
  }, error = function(e) {
    cat("\n✗ MIDAS pipeline failed:\n")
    cat(conditionMessage(e), "\n")
    quit(save = "no", status = 1)
  })
}

run_benchmark <- function() {
  cat("\n=== Running Benchmark Model Comparison ===\n\n")
  cat("Comparing MF-VAR against MIDAS, AR(2), and RW-trend models...\n")
  cat("This includes holdout evaluation and cross-validation.\n")
  cat("This may take several minutes.\n\n")
  
  tryCatch({
    source("run_benchmarks.R", local = new.env())
    cat("\n✓ Benchmark comparison completed successfully!\n")
    cat("Check output/ directory for results.\n\n")
  }, error = function(e) {
    cat("\n✗ Benchmark comparison failed:\n")
    cat(conditionMessage(e), "\n")
    quit(save = "no", status = 1)
  })
}

# Route to appropriate workflow
switch(tolower(workflow),
  "mfvar" = run_mfvar(),
  "midas" = run_midas(),
  "benchmarks" = run_benchmark(),
  "benchmark" = run_benchmark(),  # alias for backwards compatibility
  "verify" = verify_environment(),
  "help" = show_help(),
  "-h" = show_help(),
  "--help" = show_help(),
  {
    cat(sprintf("Unknown workflow: '%s'\n", workflow))
    cat("Run 'Rscript main.R help' for usage information.\n")
    quit(save = "no", status = 1)
  }
)
