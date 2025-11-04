# Copilot Instructions

## Repository Layout
- Focus work in `submission/`; everything else is teaching material or references.
- `Draft_MFVAR.r` orchestrates the mixed-frequency VAR pipeline by sourcing helpers in `submission/R/`.
- `main.R` is a lightweight smoke-test used by Docker/CI; do not add business logic there.
- Outputs are written to `submission/output/` and the completion `message()` in `Draft_MFVAR.r` is used as an automation signal.

## Data Contracts
- Quarterly inputs live in `submission/data/data_quarterly.csv` and must keep the columns `date`, `rvgdp`, `cpi`, `wkfreuro` with `%Y-%m` timestamps.
- `read_quarterly_data()` converts those columns into `gdp_growth`, `inflation`, and logged `exch_rate`; downstream code assumes these exact names and annualised units.
- Update `submission/data/metadata_quarterly*.csv` whenever the quarterly dataset schema changes to keep provenance consistent.
- `fetch_kof_barometer()` pulls the monthly KOF barometer (`kofbarometer` fallback `ch.kof.barometer`); provide a cached `ts` when working offline instead of rewriting callers.

## Pipeline Architecture
- `activate_project()` (in `R/setup.R`) sets the working directory and activates `renv`; scripts depend on it before touching the filesystem.
- `stationarise_quarterly()` detrends and deseasonalises series while storing parameters in `transforms`; `restore_series_values()` depends on these for back-transforming forecasts.
- `build_Y()` packages the aligned quarterly and monthly series exactly as `mfbvar::set_prior()` expects; keep the structure when extending variables.
- `estimate_mfvar_model()` fixes Minnesota/Inverse-Wishart priors with options `mfvar.n_*`; adjust lag length, draws, and forecast horizon together to avoid mismatched burn-in settings.
- `predict_ar2()` and `predict_rw_trend()` implement benchmark models used in both holdout and CV evaluation; retain their multi-method fallbacks to keep evaluations robust when data are short.

## Evaluation & Outputs
- Holdout evaluations reserve up to four quarters (`run_holdout_evaluation()`); changing `n_lags` shrinks this window, so review the resulting `eval_horizon` before trusting comparisons.
- Rolling cross-validation walks the ragged edge (`run_cross_validation()`); cap folds via `options(mfvar.cv_max_folds = <n>)` instead of editing the loop.
- Forecast tables are saved as `mfvar_forecasts_full.csv` and `mfvar_forecasts_targets.csv`; the latter must include both step 1 and step 4 horizons for every target variable or a warning is raised.
- Plots in `R/plotting.R` expect tidy inputs with `lower/median/upper` columns and use logged exchange-rate histories; only convert to levels for presentation inside `Draft_MFVAR.r`.

## Development Workflow
- Run `renv::restore()` from `submission/` (or rely on `.Rprofile`) before executing scripts; add packages with `renv::install()` so `renv.lock` stays authoritative.
- Execute the main pipeline with `Rscript Draft_MFVAR.r` from `submission/`; the script resets `setwd()` so relative paths remain valid.
- Docker users can reproduce the run with `docker compose run --rm momf`, which mounts `submission/data/` and captures outputs in the named volume.
- When introducing new forecast targets, update `target_variables` in `R/setup.R`, ensure the quarterly CSV exposes the same columns, and confirm evaluation/plotting helpers handle them without manual branching.
- Keep error handling in place: the pipeline stops early if data or barometer pulls fail, which is preferable to propagating NAs into the Bayesian model.
