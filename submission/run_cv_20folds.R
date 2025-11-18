# Run benchmarks with 20 CV folds
options(mfvar.cv_max_folds = 20)

# Read and modify run_benchmarks.R to set skip_cv_temp = FALSE
benchmark_code <- readLines("run_benchmarks.R")
skip_line <- grep("skip_cv_temp <- TRUE", benchmark_code, fixed = TRUE)
if (length(skip_line)) {
  benchmark_code[skip_line] <- "skip_cv_temp <- FALSE"
}

# Execute modified code
eval(parse(text = benchmark_code))
