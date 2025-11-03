# Copilot Instructions

## Overview
- Mixed-frequency Bayesian VAR pipeline for Swiss macro indicators driven by `Draft_MFVAR.r`; helper modules live under `R/`.
- Core outputs land in `output/`: forecasts (`mfvar_forecasts_full.csv`, `mfvar_forecasts_targets.csv`), evaluation tables, `mfvar_summary.txt`, optional GDP plots, and `mfvar_model_ss.rds`.
- `main.R` is a Docker/CI smoke test only; concentrate workflow changes in `Draft_MFVAR.r` and `R/` helpers.

## Data & Inputs
- Quarterly history comes from `data/data_quarterly.csv` and must retain columns `date`, `rvgdp`, `cpi`, `wkfreuro` (`%Y-%m` strings); ingestion stops if any are missing.
- `R/data_processing.R` transforms the CSV into quarterly growth/log series (`gdp_growth`, `inflation`, `exch_rate`) and drops the leading NA; keep column names stable for downstream joins.
- Monthly KOF Barometer is fetched via `kofdata::get_time_series()` (tries `kofbarometer`, then `ch.kof.barometer`); provide a cached `ts` in `fetch_kof_barometer()` when offline.
- `trim_to_overlap()` and `window_baro()` align frequencies by truncating quarters beyond available barometer data and backfilling two months before the sample start; validate these before altering priors.
- Update `data/metadata_quarterly*.csv` whenever quarterly fields change to keep provenance records accurate.

## Code Structure
- `R/setup.R` enforces `renv` activation (`renv/activate.R`) and loads required packages (`mfbvar`, `kofdata`, tidyverse stack); run `renv::restore()` before executing scripts or adding dependencies.
- `Draft_MFVAR.r` follows a strict sequence: ingest -> align -> evaluations -> model estimation -> persistence. Preserve object names (`qdat`, `baro_ts`, `Y`) because later stages reuse them without recomputation.
- `build_Y()` returns the structure expected by `mfbvar::set_prior`: list with `kofbarometer` (`ts`) and `quarterly` (`ts` matrix). Keep quarterly column names when extending targets.
- `estimate_mfvar_model()` fixes Minnesota/IW prior hyperparameters (`n_reps = 4000`, `n_burnin = 2000`, `n_thin = 4`, `aggregation = "average"`); adjust these jointly if you change lag length or forecast horizon.
- Evaluation helpers (`run_holdout_evaluation()`, `run_cross_validation()`) benchmark MF-VAR against an AR(2) built via `predict_ar2()` which cycles through Yule-Walker, OLS, and ARIMA fits; retain fallbacks to keep robustness.
- Plotting utilities expect tidy columns `lower/median/upper` and actual GDP history filtered to 2023+; reuse those conventions for new visuals.

## Running & Debugging
- Execute `Rscript Draft_MFVAR.r` from the project root; the script resets `setwd()` based on `commandArgs()` so relative paths remain valid.
- Inspect `output/mfvar_summary.txt` for convergence diagnostics (`summary(mod_ss)` output) plus evaluation tables. Forecast CSVs contain both raw monthly aggregation and tagged target horizons.
- Holdout horizon is `min(4, nrow(qdat) - (n_lags + 1))`; increasing `n_lags` shortens evaluation samples, so coordinate adjustments with stakeholders.
- Cross-validation iterates over the last `cv_horizon` quarters (`min(8, nrow(qdat) - (n_lags + 2))`); failures emit warnings but should not abort the main run.
- Network outages surface early in `fetch_kof_barometer()`; fail fast and consider stubbing with `ts()` if needed for offline testing.

## Contribution Patterns
- Work within tidyverse pipelines; shared global variables are declared via `utils::globalVariables()` to appease R CMD check.
- Add new forecast targets by editing `target_variables` in `R/setup.R`, ensuring the quarterly dataset exposes the same columns and evaluations/plots reference them.
- Keep helper logic centralized in existing modules; automation relies on `Draft_MFVAR.r` emitting the final `message()` footer to detect completion.
- When introducing packages, prefer `renv::install()` so `renv.lock` stays authoritative; avoid runtime installs inside scripts.
