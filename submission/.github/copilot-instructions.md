# Copilot Instructions

## Project scope
- Mixed-frequency Bayesian VAR for Swiss macro forecasting; heavy lifting in `Draft_MFVAR.r`.
- Combines quarterly KOF forecast CSV (`data/data_quarterly.csv`) with live monthly KOF Barometer via `kofdata`.

## Key scripts
- `Draft_MFVAR.r`: end-to-end pipeline from data ingest to forecasts; keep the sequence of transformations (quarterly derive -> align monthly) intact because later steps assume those column names.
- `main.R`: minimal smoke test used by Docker/CI to confirm renv activation and plotting; do not rely on it for production outputs.

## Data expectations
- Quarterly CSV must expose columns `date`, `rvgdp`, `cpi`, `wkfreuro` (ISO month string, real GDP, CPI, EUR/CHF); script stops otherwise.
- Metadata files (`data/metadata_quarterly*.csv`) describe provenance; update alongside new or renamed columns.
- Forecast horizon alignment uses `zoo::as.yearqtr`; keep dates in `%Y-%m` format to avoid parsing failures.

## External dependencies
- `renv` is mandatory (`.Rprofile` auto-activates); run `renv::restore()` before executing scripts or installing packages.
- The script installs missing CRAN deps at runtime; prefer adding packages to `renv.lock` via `renv::install()` to keep builds deterministic.
- `kofdata::get_time_series()` hits the KOF API; ensure network access or provide a cached `ts` stub inside `Draft_MFVAR.r` when offline.

## Workflow
- Primary entrypoint: `Rscript Draft_MFVAR.r`; outputs land in `output/` (`mfvar_summary.txt`, forecast CSVs, optional GDP plot, serialized `mfvar_model_ss.rds`).
- Model prior uses `set_prior(..., n_reps = 4000, n_burnin = 2000, n_thin = 4)`; adjust together to keep effective samples stable.
- Mixed-frequency aggregation matrix comes from `mfbvar:::build_Lambda`; update `aggregation_vec` and `n_lags` in tandem.

## Validation and debugging
- Inspect generated `output/mfvar_summary.txt` for convergence diagnostics; `summary(mod_ss)` is the single authoritative check.
- When forecasts look off, confirm barometer alignment (`m_start_back2`, `baro_ts` window) before tweaking priors.
- Enable verbose logging by temporarily removing `suppressPackageStartupMessages()` during debugging.

## Contribution tips
- Keep new code vectorized and rely on `dplyr` verbs; the pipeline expects tidy data frames.
- Prefer writing helper functions inside the script rather than new files unless logic is shared elsewhere, to avoid fragmenting the single-script workflow.
- Mirror existing messaging style (`message()` footer) so downstream automation can continue to detect completion.
