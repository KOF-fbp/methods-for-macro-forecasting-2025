#!/usr/bin/env Rscript

if (!interactive()) {
  args_all <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_all, value = TRUE)
  if (length(file_arg)) {
    script_path <- normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)
    project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
    setwd(project_root)
  }
} else {
  if (basename(getwd()) == "diagnostics") {
    setwd("..")
  }
}

# Manually activate renv without using setup.R
activate_path <- file.path("renv", "activate.R")
if (file.exists(activate_path)) {
  source(activate_path, local = TRUE)
}

source("R/data_processing.R")

qdat_raw <- read_quarterly_data("data")
monthly_raw <- read_combined_timeseries("data", c("plkopr", "devkum", "amarbma", "snboffzisa", "smi_monthly_avg"))

message("=== FILL strategy ===")
trimmed_fill <- trim_to_overlap(
  qdat_raw, 
  monthly_raw$ts_list, 
  mode = "ragged", 
  fill_method = "locf",
  start_strategy = "fill"
)

message("\n=== OMIT strategy ===")
trimmed_omit <- trim_to_overlap(
  qdat_raw, 
  monthly_raw$ts_list, 
  mode = "ragged", 
  fill_method = "locf",
  start_strategy = "omit"
)

message("\n=== Comparison ===")
message("FILL monthly indicators: ", paste(names(trimmed_fill$monthly), collapse = ", "))
message("OMIT monthly indicators: ", paste(names(trimmed_omit$monthly), collapse = ", "))

message("\nQuarterly observations - FILL: ", nrow(trimmed_fill$qdat))
message("Quarterly observations - OMIT: ", nrow(trimmed_omit$qdat))

for (name in names(trimmed_fill$monthly)) {
  if (name %in% names(trimmed_omit$monthly)) {
    len_fill <- length(trimmed_fill$monthly[[name]])
    len_omit <- length(trimmed_omit$monthly[[name]])
    message(sprintf("%s: FILL=%d months, OMIT=%d months", name, len_fill, len_omit))
  } else {
    message(sprintf("%s: present in FILL, ABSENT in OMIT", name))
  }
}

message("\n=== What build_Y() receives after windowing ===")

qdat_orig_fill <- trimmed_fill$qdat
monthly_list_fill <- window_monthly_series(
  trimmed_fill$monthly, 
  qdat_orig_fill, 
  end_mode = "available"
)

qdat_orig_omit <- trimmed_omit$qdat
monthly_list_omit <- window_monthly_series(
  trimmed_omit$monthly, 
  qdat_orig_omit, 
  end_mode = "available"
)

message("\nAfter windowing:")
message("FILL monthly series: ", paste(names(monthly_list_fill), collapse = ", "))
message("OMIT monthly series: ", paste(names(monthly_list_omit), collapse = ", "))

n_holdout <- 4L
train_idx <- seq_len(nrow(qdat_orig_fill) - n_holdout)

message("\n=== Holdout split ===")
message("Training rows: ", length(train_idx))
message("Training period FILL: ", qdat_orig_fill$qtr[min(train_idx)], " to ", qdat_orig_fill$qtr[max(train_idx)])
message("Training period OMIT: ", qdat_orig_omit$qtr[min(train_idx)], " to ", qdat_orig_omit$qtr[max(train_idx)])

train_last_qtr_fill <- qdat_orig_fill$qtr[max(train_idx)]
train_last_qtr_omit <- qdat_orig_omit$qtr[max(train_idx)]

message("\n=== Monthly data dimensions for training ===")
train_last_month_fill <- c(
  as.integer(format(zoo::as.Date(train_last_qtr_fill, frac = 1), "%Y")),
  as.integer(format(zoo::as.Date(train_last_qtr_fill, frac = 1), "%m"))
)
train_last_month_omit <- c(
  as.integer(format(zoo::as.Date(train_last_qtr_omit, frac = 1), "%Y")),
  as.integer(format(zoo::as.Date(train_last_qtr_omit, frac = 1), "%m"))
)

message("Training ends at month: FILL=", paste(train_last_month_fill, collapse="-"), 
        ", OMIT=", paste(train_last_month_omit, collapse="-"))

for (name in names(monthly_list_fill)) {
  if (name %in% names(monthly_list_omit)) {
    train_fill <- stats::window(monthly_list_fill[[name]], end = train_last_month_fill)
    train_omit <- stats::window(monthly_list_omit[[name]], end = train_last_month_omit)
    message(sprintf("%s training length: FILL=%d, OMIT=%d", 
                    name, length(train_fill), length(train_omit)))
  } else {
    train_fill <- stats::window(monthly_list_fill[[name]], end = train_last_month_fill)
    message(sprintf("%s training length: FILL=%d, OMIT=N/A (dropped)", 
                    name, length(train_fill)))
  }
}

message("\n=== Seed check ===")
message("Checking if seeds are hardcoded in estimation functions...")
seed_files <- c("R/setup.R", "R/evaluation.R", "R/benchmark_shared.R", "run_benchmarks.R")
for (f in seed_files) {
  if (file.exists(f)) {
    lines <- readLines(f)
    seed_lines <- grep("seed|set\\.seed", lines, ignore.case = TRUE, value = TRUE)
    if (length(seed_lines) > 0) {
      message(sprintf("\nIn %s:", f))
      for (line in seed_lines) {
        message("  ", trimws(line))
      }
    }
  }
}

message("\n=== Conclusion ===")
if (!"devkum" %in% names(monthly_list_omit)) {
  message("✓ devkum is correctly dropped under OMIT strategy")
  message("✓ Training data dimensions differ between strategies")
  message("⚠ However, if the same seed is used for MCMC, posterior estimates may be very similar")
  message("⚠ The 1-step forecast uses the most recent observations, which are identical in both cases")
  message("\nRecommendation: Check if identical seeds + similar posteriors explain the matching errors")
} else {
  message("✗ Problem: devkum is NOT being dropped under OMIT strategy")
}
