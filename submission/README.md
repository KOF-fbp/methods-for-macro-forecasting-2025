# Swiss Macroeconomic Forecasting: MF-VAR vs MIDAS

**Course Project**: Methods for Macroeconomic Forecasting  
**Institution**: ETH Zurich 
**Authors**: See `AUTHORS.yml`    
**Date**: November 2025

## Overview

This project implements and compares two approaches for forecasting Swiss macroeconomic indicators using mixed-frequency data:

1. **Mixed-Frequency Bayesian VAR (MF-VAR)** - Incorporates monthly KOF Barometer data with quarterly GDP, inflation, and exchange rate series
2. **MIDAS Regression** - Uses polynomial distributed lag structures to mix data frequencies

The analysis includes rigorous evaluation through holdout testing and expanding window cross-validation, with comprehensive benchmark comparisons against AR(2) and random walk models.

## Authors

See [`AUTHORS.yml`](AUTHORS.yml) for contributor information.

## Project Structure

```
.
├── R/                          # Helper modules
│   ├── setup.R                 # Environment setup and package management
│   ├── data_processing.R       # Data ingestion and transformation
│   ├── evaluation.R            # Model evaluation and benchmarking
│   └── plotting.R              # Visualization functions
│
├── data/                       # Data files
│   ├── data_quarterly.csv      # Main quarterly dataset (GDP, CPI, exchange rate)
│   ├── metadata_quarterly*.csv # Data provenance and documentation
│   └── *.csv                   # Raw source data from SNB
│
├── output/                     # Generated outputs (gitignored except results)
│   ├── forecasts/             # Current forecasts from latest data
│   │   ├── mfvar/             # MF-VAR results (forecasts, latent states, plots)
│   │   ├── midas/             # MIDAS results (forecasts, plots)
│   │   ├── combined/          # Multi-model comparison plots
│   │   ├── *.png              # Forecast visualizations
│   │   └── *.rds              # Saved model objects
│   └── benchmarks/            # Model comparison & evaluation
│       ├── model_benchmark_*.csv   # Comparison metrics
│       ├── forecast_evaluation.csv # Holdout evaluation
│       └── *.png              # Comparison plots
│
├── docs/                       # Documentation and analysis
│   ├── mfvar_walkthrough.*     # Tutorial notebook
│   ├── presentation*.qmd       # Presentation slides
│   └── midas/                  # MIDAS model exploration
│
├── renv/                       # R package management
│
├── main.R                     # Entry point - unified workflow interface
├── run_mfvar_package.R        # MF-VAR workflow script
├── run_midas.R                # MIDAS workflow script
├── run_combined_plots.R       # Multi-model comparison visualization
├── run_benchmarks.R           # Benchmark comparison across all models
└── README.md                  # This file
```

## Quick Start

### Prerequisites

- R >= 4.5.0
- RStudio (recommended)
- Required packages managed via `renv`

### Installation

```r
# Restore package environment
renv::restore()
```

### Running the Analysis

All workflows can be run through the unified `main.R` interface:

#### Option 1: MF-VAR Pipeline (Default)

```bash
Rscript main.R mfvar
# or simply: Rscript main.R
```

This runs the complete MF-VAR workflow:
1. Loads quarterly data and KOF Barometer
2. Performs holdout evaluation
3. Runs cross-validation
4. Estimates full-sample model
5. Generates forecasts and plots

**Outputs:**
- `output/forecasts/mfvar/csv/mfvar_forecasts_full.csv` - All forecast horizons
- `output/forecasts/mfvar/csv/mfvar_forecasts_targets.csv` - 1-step and 1-year ahead only
- `output/forecasts/mfvar/csv/mfvar_latent_states.csv` - Extracted latent states (extension)
- `output/forecasts/mfvar/mfvar_summary.txt` - Model diagnostics and evaluation
- `output/forecasts/mfvar/plots/forecast_*.png` - Forecast visualizations
- `output/forecasts/mfvar/plots/mfvar_latent_states_*.png` - Latent state visualizations

#### Option 2: MIDAS Pipeline

```bash
Rscript main.R midas
```

Runs MIDAS regression models with the KOF Barometer:
1. Estimates MIDAS with and without trend
2. Generates forecasts for target variables
3. Computes evaluation metrics

**Outputs:**
- `output/forecasts/midas/csv/midas_forecasts_full.csv` - All forecast horizons
- `output/forecasts/midas/csv/midas_forecasts_targets.csv` - 1-step and 1-year ahead only
- `output/forecasts/midas/midas_summary.txt` - Model diagnostics
- `output/forecasts/midas/plots/midas_forecast_*.png` - Forecast visualizations

#### Option 3: Benchmark Comparison

```bash
Rscript main.R benchmarks
```

Compares all models (MF-VAR, MIDAS-KOF, MIDAS-Latent, AR(2), RW-trend) with:
- Holdout evaluation (last 4 quarters)
- Expanding window cross-validation (up to 20 folds, configurable via `--max-folds`)
- CV coverage: 1993 Q4 to 2025 Q2 (training starts from 6 quarters in 1992 Q2-1993 Q3)
- Per-fold timing breakdowns for performance profiling

**Outputs:**
- `output/benchmarks/csv/model_benchmark_*.csv` - Detailed metrics
- `output/benchmarks/model_benchmark_summary.md` - Summary tables
- `output/benchmarks/plots/model_benchmark_plot_*.png` - Comparison plots

#### Option 4: Combined Forecast Plots

```bash
Rscript run_combined_plots.R
```

Generates multi-model comparison visualizations:
- Overlays all 6 forecast series (MF-VAR, MIDAS-KOF trend/simple, MIDAS-Latent trend/simple, AR(2))
- Shows 1-year (4 quarters) of historical context
- Requires prior runs of `run_mfvar_package.R` and `run_midas.R`

**Outputs:**
- `output/forecasts/combined/plots/combined_forecast_*.png` - Multi-model comparison plots

## Target Variables

- **gdp_growth**: Real GDP annualized quarterly growth rate
- **inflation**: CPI annualized quarterly inflation rate
- **exch_rate**: CHF/EUR exchange rate (log-transformed)

## Forecast Horizons

- **1-step ahead**: Next quarter (nowcast/1-quarter ahead)
- **1-year ahead**: 4 quarters ahead (annual forecast)

## Model Specifications

### MF-VAR
- **Lags**: 5 quarters
- **Prior**: Minnesota with inverse-Wishart covariance
- **MCMC**: 10000 draws, 5000 burn-in, no thinning (package defaults)
- **Monthly indicators**: CPI, FX turnover, unemployment, SNB rate, SMI returns
- **Aggregation**: Average (for monthly-to-quarterly mapping)

### Benchmarks
- **AR(2)**: Autoregressive model (Yule-Walker/OLS/ARIMA fallbacks)
- **MIDAS**: Mixed-data sampling with exponential Almon lag polynomials
- **RW-trend**: Random walk with linear trend

## Data Sources

Quarterly data sourced from Swiss National Bank (SNB):
- Real GDP growth
- Consumer Price Index (CPI)
- Exchange rates (CHF/EUR)

Monthly indicators:
- KOF Economic Barometer (via `kofdata` package) for MIDAS models
- SNB series: CPI, FX turnover, unemployment, policy rate
- Swiss Market Index (SMI) monthly returns for MF-VAR models

See `data/data_sources.md` for detailed provenance.

## Key Configuration

Edit `R/setup.R` to modify:
- Target variables
- Required packages
- Global settings

Edit main scripts to adjust:
- Number of lags (`n_lags`)
- Forecast horizon (`n_fcst`)
- Prior hyperparameters
- Cross-validation settings

## Helper Modules

### `R/setup.R`
- Activates `renv` environment
- Loads required packages
- Defines target variables

### `R/data_processing.R`
- `read_quarterly_data()` - Load and validate quarterly CSV
- `fetch_kof_barometer()` - Retrieve KOF Barometer series
- `trim_to_overlap()` - Align quarterly and monthly frequencies
- `stationarise_quarterly()` - Apply transformations (growth rates, log)
- `build_Y()` - Construct input for `mfbvar` package

### `R/evaluation.R`
- `run_holdout_evaluation()` - Holdout forecast accuracy
- `run_cross_validation()` - Expanding window one-step-ahead CV
- `predict_ar2()` - AR(2) benchmark with fallbacks
- `compute_time_index()` - Map forecast steps to time indices
- `restore_series_values()` - Reverse transformations

### `R/plotting.R`
- `plot_gdp_forecasts()` - GDP forecast visualization
- `plot_inflation_forecasts()` - Inflation forecast visualization
- `plot_exch_rate_forecasts()` - Exchange rate visualization
- `*_with_history()` variants - Include historical context

## Workflow Details

### Data Preparation
1. Load `data_quarterly.csv` (requires `date`, `rvgdp`, `cpi`, `wkfreuro` columns)
2. Fetch KOF Barometer via API
3. Trim both series to overlapping time range
4. Transform quarterly variables to stationarity
5. Window barometer data (includes 2-month lookback)

### Model Estimation
1. Set up Minnesota prior with IW covariance
2. Run MCMC sampler (4000 reps, 2000 burn-in)
3. Generate 12-quarter-ahead forecasts
4. Aggregate monthly predictions to quarterly frequency
5. Restore original scale (reverse transformations)

### Evaluation
- **Holdout**: Reserve last 4 quarters, compare forecasts to actuals
- **Cross-validation**: Expanding window 1-step ahead starting from 2015 Q4
- **Metrics**: RMSE, MAE by horizon and overall

### Output
- Forecast tables (CSV)
- Model summary and diagnostics (TXT)
- Comparison plots (PNG)
- Serialized model object (RDS)

## Dependencies

Core packages:
- `mfbvar` - Mixed-frequency Bayesian VAR ([Ankargren et al.](https://github.com/ankargren/mfbvar))
- `kofdata` - KOF data API client
- `tidyverse` - Data manipulation and visualization
- `zoo` - Time series utilities
- `midasr` - MIDAS regression
- `forecast` - Time series forecasting

See `renv.lock` for complete dependency list with versions.

## Reproducibility

This project uses `renv` for package management to ensure reproducibility across different systems. All dependencies are locked to specific versions.

## Results

Pre-computed results are included in `output/forecasts/` and `output/benchmarks/` directories, allowing inspection without re-running the models (which can take 20-30 minutes per workflow with package default hyperparameters).

## Troubleshooting

### KOF Barometer fetch fails
- Check internet connection
- The `kofdata` package requires API access to KOF Swiss Economic Institute

### Insufficient data for evaluation
- Reduce `n_lags` in workflow scripts
- Shorten evaluation horizon
- Extend historical data sample

### MCMC convergence issues
- Increase `n_reps` and `n_burnin`
- Check for data quality issues
- Review prior hyperparameters

### Package installation problems
```r
renv::restore()        # Restore from lock file
renv::rebuild()        # Rebuild if needed
renv::status()         # Check consistency
```

## References

### Methodology
- Schorfheide, F., & Song, D. (2015). "Real-time forecasting with a mixed-frequency VAR." *Journal of Business & Economic Statistics*, 33(3), 403-418.
- Ghysels, E., Sinko, A., & Valkanov, R. (2007). "MIDAS regressions: Further results and new directions." *Econometric Reviews*, 26(1), 53-90.

### Data Sources
- KOF Swiss Economic Institute: https://kof.ethz.ch/
- Swiss National Bank (SNB) economic data

### Software
- `mfbvar` package: https://github.com/ankargren/mfbvar
- `midasr` package: https://CRAN.R-project.org/package=midasr

## License

This project is submitted as coursework for educational purposes. See `AUTHORS.yml` for contributor information.
