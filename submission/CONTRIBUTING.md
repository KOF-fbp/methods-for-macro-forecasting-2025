# Contributing Guide

## Project Organization Principles

### Directory Structure
- **R/**: Reusable helper functions only. No executable scripts.
- **data/raw_data/**: Source data files. Never modify these directly.
- **data/processed/**: Cleaned and prepared data. Generated from raw_data.
- **output/**: All generated results. Gitignored except `.gitkeep`.
- **docs/**: Documentation, presentations, notebooks.
- **Root**: Main workflow scripts (`Draft_MFVAR.r`, `run_benchmark_models.R`, `main.R`)

### Code Guidelines

#### Adding New Variables
1. Update `target_variables` in `R/setup.R`
2. Ensure column exists in `data/processed/data_quarterly.csv`
3. Add transformation logic to `stationarise_quarterly()` if needed
4. Update evaluation and plotting functions

#### Adding New Dependencies
```r
# Install and add to project
renv::install("package_name")

# Update lockfile
renv::snapshot()
```

#### Modifying Data Processing
- Keep changes in `R/data_processing.R`
- Maintain backward compatibility with existing CSV format
- Document any new column requirements

#### Adding Evaluation Metrics
- Extend `R/evaluation.R` with new functions
- Follow naming convention: `compute_*()`, `evaluate_*()`, `run_*_evaluation()`
- Return structured tibbles for consistency

### Workflow Scripts

#### Draft_MFVAR.r
- Main MF-VAR estimation pipeline
- Sources all R/ helpers
- Writes to `output/mfvar_*` files
- Keep linear flow: data → evaluation → estimation → forecasting → output

#### run_benchmark_models.R
- Comparative analysis script
- Writes to `output/model_benchmark_*` files
- Independent of Draft_MFVAR.r (can run standalone)

#### main.R
- Entry point wrapper only
- No heavy computation here
- Routes to workflow scripts

### Output Conventions

#### File Naming
- **Forecasts**: `mfvar_forecasts_*.csv` or `model_benchmark_*.csv`
- **Plots**: `forecast_*.png` or `model_benchmark_plot_*.png`
- **Summaries**: `*_summary.txt` or `*_summary.md`
- **Models**: `*_model_*.rds`

#### CSV Structure
- Include column headers
- Use ISO 8601 dates where possible
- Keep tidy format (long over wide when ambiguous)

### Testing Locally

```bash
# Verify environment
Rscript main.R verify

# Quick test (small n_reps)
# Edit Draft_MFVAR.r: n_reps = 100, n_burnin = 50
Rscript main.R mfvar

# Full run
Rscript main.R mfvar    # Takes 5-10 minutes
```

### Git Workflow

```bash
# Check what's changed
git status

# Stage changes (avoid output/ files)
git add R/
git add Draft_MFVAR.r
git add README.md

# Commit with clear message
git commit -m "Add new evaluation metric for exchange rate"

# Push
git push origin your-branch-name
```

### Common Mistakes to Avoid

❌ **Don't**:
- Commit files in `output/` (except `.gitkeep`)
- Hard-code absolute paths (use `file.path()`)
- Modify data files in place without documenting
- Add print/cat debugging in production code
- Install packages during script execution

✅ **Do**:
- Use relative paths from project root
- Document data transformations
- Use message() for user-facing output
- Use renv for dependency management
- Test changes with main.R verify before committing

### Documentation Standards

#### R Functions
```r
#' Brief one-line description
#'
#' Longer explanation if needed with usage context.
#'
#' @param param_name Description of parameter
#' @return Description of return value
#' @examples
#' result <- my_function(data, option = TRUE)
my_function <- function(param_name) {
  # Implementation
}
```

#### Script Headers
```r
# Script Name
# ---------------------------------------------------------------
# Brief description of what this script does.
# 
# Inputs: List key inputs
# Outputs: List key outputs
# Dependencies: Special requirements if any
# ---------------------------------------------------------------
```

### Performance Considerations

- **MCMC**: Default `n_reps = 4000` is suitable for production. Use 100-500 for testing.
- **Cross-validation**: Can take 10+ minutes. Adjust `max_folds` to limit runtime.
- **Parallel**: CV uses multiple cores by default. Set `options(mfvar.cv_workers = 1)` to disable.

### Debugging Tips

```r
# Check data loading
qdat <- read_quarterly_data("data")
str(qdat)

# Verify transformations
stationary <- stationarise_quarterly(qdat)
summary(stationary$data)

# Inspect model object
mod <- readRDS("output/mfvar_model_ss.rds")
print(mod)
summary(mod)

# Reload functions without rerunning full script
source("R/evaluation.R")
```

### Getting Help

1. Check `README.md` for usage
2. Review `REORGANIZATION.md` for structure
3. Look at existing functions in `R/` for patterns
4. Check `output/mfvar_summary.txt` for diagnostics
5. Open a GitHub issue if stuck

### Before Submitting PR

- [ ] Code follows existing style and conventions
- [ ] New functions documented with roxygen-style comments
- [ ] Tested with `Rscript main.R verify`
- [ ] No output files added to git
- [ ] `renv::status()` shows no unexpected changes
- [ ] README.md updated if workflow changed
- [ ] Commit messages are clear and descriptive

---

**Questions?** See `README.md` or open an issue.
