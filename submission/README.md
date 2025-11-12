# Swiss Macro Forecasting with Mixed-Frequency VAR

A Bayesian mixed-frequency VAR (MF-VAR) pipeline for forecasting Swiss macroeconomic indicators using quarterly data augmented with the monthly KOF Economic Barometer.

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
├── output/                     # Generated outputs (gitignored)
│   ├── mfvar_forecasts_*.csv   # Model forecasts
│   ├── mfvar_summary.txt       # Model diagnostics
│   ├── mfvar_model_ss.rds      # Saved model object
│   ├── forecast_*.csv          # Evaluation results
│   ├── model_benchmark_*.csv   # Benchmark comparison metrics
│   └── *.png                   # Forecast plots
│
├── docs/                       # Documentation and analysis
│   ├── mfvar_walkthrough.*     # Tutorial notebook
│   ├── presentation*.qmd       # Presentation slides
│   └── midas/                  # MIDAS model exploration
│
├── renv/                       # R package management
│
├── Draft_MFVAR.r              # Main MF-VAR workflow script
├── run_benchmark_models.R     # Benchmark models comparison
├── main.R                     # Entry point (Docker/CI smoke test)
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

#### Option 1: MF-VAR Pipeline (Default)

```bash
Rscript Draft_MFVAR.r
```

This runs the complete MF-VAR workflow:
1. Loads quarterly data and KOF Barometer
2. Performs holdout evaluation
3. Runs cross-validation
4. Estimates full-sample model
5. Generates forecasts and plots

**Outputs:**
- `output/mfvar_forecasts_full.csv` - All forecast horizons
- `output/mfvar_forecasts_targets.csv` - 1-step and 1-year ahead only
- `output/mfvar_summary.txt` - Model diagnostics and evaluation
- `output/forecast_*.png` - Forecast visualizations

#### Option 2: Benchmark Comparison

```bash
Rscript run_benchmark_models.R
```

Compares MF-VAR against MIDAS, AR(2), and RW-trend models with:
- Holdout evaluation
- Rolling cross-validation (28 folds)
- Multiple monthly data coverage scenarios

**Outputs:**
- `output/model_benchmark_*.csv` - Detailed metrics
- `output/model_benchmark_summary.md` - Summary tables
- `output/model_benchmark_plot_*.png` - Comparison plots

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
- **MCMC**: 4000 draws, 2000 burn-in, thinning=4
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

Monthly KOF Economic Barometer fetched via `kofdata` package.

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
- `run_cross_validation()` - Rolling one-step-ahead CV
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
- **Cross-validation**: Rolling 1-step ahead starting from 2015 Q4
- **Metrics**: RMSE, MAE by horizon and overall

### Output
- Forecast tables (CSV)
- Model summary and diagnostics (TXT)
- Comparison plots (PNG)
- Serialized model object (RDS)

## Dependencies

Core packages:
- `mfbvar` - Mixed-frequency Bayesian VAR
- `kofdata` - KOF data API client
- `tidyverse` - Data manipulation and visualization
- `zoo` - Time series utilities

Benchmark models:
- `midasr` - MIDAS regression
- `forecast` - Time series forecasting

See `renv.lock` for complete dependency list with versions.

## Docker Support

The repository includes Docker configuration for reproducible execution:

```bash
docker build -t mfvar-pipeline .
docker run -v $(pwd)/output:/app/output mfvar-pipeline
```

## CI/CD

GitHub Actions workflow validates the environment via `main.R` smoke test.

## Troubleshooting

### KOF Barometer fetch fails
- Check internet connection
- Try alternative series name in `fetch_kof_barometer()`
- Provide cached `ts` object for offline work

### Insufficient data for evaluation
- Reduce `n_lags` in main scripts
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

- Schorfheide, F., & Song, D. (2015). "Real-time forecasting with a mixed-frequency VAR." *Journal of Business & Economic Statistics*
- KOF Swiss Economic Institute: https://kof.ethz.ch/
- `mfbvar` package: https://github.com/ankargren/mfbvar

## License

See repository root for license information.

## Authors

See `AUTHORS.yml` for contributor information.

## Contact

For questions or issues, please open a GitHub issue or contact the maintainers.
