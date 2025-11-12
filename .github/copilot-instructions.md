# Copilot Instructions

## Overview
- Active work sits in `submission/`; teaching material lives elsewhere.
- `Draft_MFVAR.r` is the MF-VAR orchestrator (ingest → evaluation → estimation → outputs) sourcing helpers under `submission/R/`.
- `run_benchmark_models.R` extends the workflow with MIDAS and multi-coverage comparisons, emitting `model_benchmark_*` artefacts.
- Core outputs land in `submission/output/` and the closing `message()` in `Draft_MFVAR.r` signals automation that the run finished.

## Data Contracts
- Quarterly data (`submission/data/data_quarterly.csv`) must keep `date`, `rvgdp`, `cpi`, `wkfreuro` with `%Y-%m`; ingestion halts if any column is missing.
- `read_quarterly_data()` builds `gdp_growth`, `inflation`, `exch_rate` (annualised log differences except the logged FX level); downstream code assumes these exact names.
- `fetch_kof_barometer()` tries `kofbarometer` then `ch.kof.barometer`; during outages, hand a cached `ts` to the same function signature rather than editing callers.
- `trim_to_overlap()` and `window_baro()` clip quarters to available barometer data and extend two months pre-sample; changing them alters every forecast so double-check before modifying.
- Keep `submission/data/metadata_quarterly*.csv` aligned with any column or units change in the quarterly dataset.

## Architecture & Helpers
- `activate_project()` (in `R/setup.R`) resets `setwd()` from `commandArgs()` and sources `renv/activate.R`; call it before touching the filesystem or packages.
- `load_required_packages()` fails fast when dependencies are missing—run `renv::restore()` or `renv::install(<pkg>)` instead of installing on the fly.
- `stationarise_quarterly()` stores trend/seasonal adjustments in `transforms`; always back-transform predictions with `restore_series_values()` (e.g., before writing targets or plotting).
- `build_Y()` produces the `list(kofbarometer = ts, quarterly = ts matrix)` that `mfbvar::set_prior()` expects; preserve column order when adding targets.
- `estimate_mfvar_model()` locks in Minnesota/IW priors and reads tuning from `options(mfvar.n_reps|mfvar.n_burnin|mfvar.n_thin)`; adjust them in tandem with `n_lags` or forecast horizon to keep burn-in consistent.
- Benchmark helpers `predict_ar2()` (YW → OLS → ARIMA fallback) and `predict_rw_trend()` provide comparisons across holdout/CV—retain their multi-method safeguards for short samples.

## Pipeline & Evaluation
- Run `Rscript Draft_MFVAR.r` from within `submission/`; the script enforces relative paths and writes forecasts, evaluations, plots, and `mfvar_model_ss.rds`.
- Holdout horizon is `min(4, nrow(qdat) - (n_lags + 1))`; increasing `n_lags` shrinks evaluation data, so review `holdout_results$horizon` before trusting metrics.
- Rolling CV (`run_cross_validation()`) walks the ragged edge; limit folds via `options(mfvar.cv_max_folds = <n>)` rather than editing the loop.
- `mfvar_forecasts_targets.csv` must contain both step 1 and step 4 horizons for every `target_variables` entry; the script warns if any combination is missing.
- `output/mfvar_summary.txt` captures `summary(mod_ss)` plus evaluation tables—check it first when diagnosing runs.

## Benchmark Workflow
- `run_benchmark_models.R` adds MIDAS regressions (`midasr`, `forecast`, `purrr`) and stage logging via `stage_status()`; invoke with `Rscript run_benchmark_models.R` after restoring packages.
- It computes monthly barometer differences (`baro_diff_series`) and simulates partial-month availability via `extra_months` (0–3); reproducing results requires consistent monthly coverage settings.
- `run_benchmark_cross_validation()` starts folds at `2015 Q4` by default and writes per-fold predictions to `model_benchmark_cv_predictions.csv`; adjust `initial_train_quarter` instead of rewriting loops when changing windows.
- Benchmark summaries land in `model_benchmark_summary.md` and plots `model_benchmark_plot_<var>.png`; they rely on `convert_for_plot()` to re-level exchange-rate predictions.
- Update `target_variables` in `R/setup.R`, ensure quarterly data exposes matching columns, and extend `y_ts_list`/plot labelling when introducing new targets.

## Development Notes
- `main.R` is a Docker/CI smoke test—avoid business logic there; keep primary workflows in `Draft_MFVAR.r` and `run_benchmark_models.R`.
- Scripts follow tidyverse pipelines; suppress NSE notes with `utils::globalVariables()` rather than silencing warnings elsewhere.
- Preserve the final `message()` structure in `Draft_MFVAR.r`; downstream automation watches it to detect success and enumerate artefacts.
- Default to ASCII when editing data/scripts and prefer descriptive comments only for non-obvious logic.
