# mfvarR: Mixed Frequency VAR with Bayesian Estimation

## Overview

`mfvarR` implements a Mixed Frequency Vector Autoregression (MF-VAR) following the approach of Schorfheide and Song. The package supports:

- **Mixed frequency data**: Monthly and quarterly variables in a unified framework
- **Quarterly aggregation**: Quarterly observations as averages of latent monthly log levels
- **Ragged edge forecasting**: Real-time nowcasting and forecasting with release calendars
- **Bayesian estimation**: Minnesota-style prior via dummy observations
- **Gibbs sampling**: Alternates between parameters and latent states
- **Prior selection**: Automatic hyperparameter tuning via marginal data density
- **Predictive distributions**: Point forecasts and credible intervals
- **Benchmark comparison**: Built-in AR(2) benchmark with RMSE/MAE evaluation

## Installation

```r
# Install from source
install.packages("devtools")
devtools::install_local("path/to/mfvarR")

# Or install dependencies manually
install.packages(c("data.table", "zoo", "Matrix", "mvtnorm", "KFAS", 
                   "yaml", "ggplot2", "cli", "testthat"))
```

## Quick Start

### Run the Demo with Real Swiss Economic Data

```r
# From the mfvarR directory:
Rscript run_real_data_demo.R
```

This comprehensive demo:
- Loads real SNB data (2000-2023): CPI (monthly), EUR/CHF (monthly), GDP (quarterly)
- Estimates a mixed-frequency Bayesian VAR with quarterly aggregation
- Generates 24-month forecasts with uncertainty quantification
- Creates publication-quality graphs showing historical data, forecasts, and credible intervals
- Visualizes latent monthly GDP states vs quarterly observations
- Saves all results (RDS, CSV, PNG) to `../output/mfvar_real_data/`

### Use the Package Programmatically

```r
# Load from source
devtools::load_all(".")

# 1. Load data
df <- load_data("data/quarterly_data.csv")

# 2. Specify metadata
meta <- infer_meta(
  df,
  quarterly_vars = "GDP",
  monthly_vars = c("CPI", "ExRate"),
  gdp_var = "GDP",
  price_vars = "CPI",
  exrate_vars = "ExRate"
)

# 3. Prepare monthly grid
prepared <- prepare_monthly_grid(df, meta, log_vars = NULL)
calendar <- build_release_calendar(prepared$data, meta)
y_obs <- as.matrix(prepared$data[, meta$vars, with = FALSE])

# 4. Select hyperparameters
grid <- data.frame(lambda = c(0.1, 0.2, 0.5))
hyper_sel <- select_hyperparameters(y_obs, calendar, meta, grid, p = 2)

# 5. Run Gibbs sampler
posterior <- gibbs_mfvar(
  y_obs, calendar, meta, p = 2,
  hyper = hyper_sel$best_hyper,
  n_draws = 2000, burnin = 1000
)

# 6. Generate forecasts
pred_obj <- predict_mfvar(posterior, horizons_months = 12)

targets <- targets_from_latent(
  pred_obj, meta,
  horizons = c(1, 12)
)

# 7. Evaluate vs AR(2) benchmark
# (implement your own AR(2) forecasts)

# 8. Build report
build_report(posterior, pred_obj, targets, eval_table, "output/")
```

## Command-Line Interface

Run the full pipeline from a YAML config file:

```r
mfvar_cli("config.yaml")
```

Example `config.yaml`:

```yaml
data_dir: "data/"
data_files: 
  - "data_quarterly.csv"
output_dir: "output/"
quarterly_vars: ["GDP"]
monthly_vars: ["CPI", "ExRate"]
gdp_var: "GDP"
price_vars: ["CPI"]
exrate_vars: ["ExRate"]
var_lag: 2
n_draws: 2000
burnin: 1000
lambda_grid: [0.1, 0.2, 0.5]
```

## Key Features

### 1. Mixed Frequency Data Handling

The package handles monthly and quarterly variables seamlessly:
- Monthly variables: observed each month
- Quarterly variables: observed only at quarter ends (Mar, Jun, Sep, Dec) as averages of three monthly latent values

### 2. Release Calendar

Implements realistic data availability:
- Monthly data for month *t* available at origin *t+1*
- Quarterly data for quarter *Q* available one month after quarter end
- Custom calendars supported via CSV

### 3. State Space Representation

Uses companion form for VAR(*p*):
- State: α\_t = (y\_t, y\_{t-1}, ..., y\_{t-p+1})'
- Observation equation handles quarterly averaging
- KFAS package for Kalman filtering and simulation smoothing

### 4. Minnesota Prior

Implements dummy observation approach with hyperparameters:
- `lambda`: Overall tightness
- `lag_decay`: Decay for higher lags
- `cross_eq`: Cross-equation shrinkage
- `intercept_weight`: Prior weight on intercept

### 5. Gibbs Sampler

Alternates between:
1. Drawing VAR parameters (A, Σ) given latent states
2. Drawing latent monthly states given parameters

### 6. Target Transformations

Produces forecasts for:
- **GDP growth**: QoQ annualized or YoY from quarterly averages
- **Inflation**: YoY or monthly annualized from CPI
- **Exchange rate**: Monthly changes or 12-month cumulative

### 7. AR(2) Benchmark

Built-in AR(2) benchmark for comparison:
- Fits AR(2) on each target series
- Computes RMSE and MAE at horizons 1 and 12 months

## Functions

### Data I/O
- `load_data()`: Load CSV data
- `infer_meta()`: Infer or specify metadata

### Preprocessing
- `prepare_monthly_grid()`: Create complete monthly grid
- `build_release_calendar()`: Build data availability calendar

### State Space
- `companion_blocks()`: VAR companion form
- `build_Zt()`: Time-varying observation matrix
- `kf_build_model()`: Build KFAS model
- `sim_smoother_states()`: Simulation smoother

### Priors and Posteriors
- `build_dummies()`: Minnesota prior dummies
- `sample_A()`: Draw VAR coefficients
- `sample_Sigma()`: Draw covariance matrix
- `draw_states()`: Draw latent states

### Estimation
- `gibbs_mfvar()`: Main Gibbs sampler
- `select_hyperparameters()`: Grid search for prior tightness

### Forecasting
- `predict_mfvar()`: Generate predictive distributions
- `targets_from_latent()`: Extract target forecasts

### Evaluation
- `fit_ar2()`: Fit AR(2) benchmark
- `forecast_ar2()`: AR(2) forecasts
- `evaluate_forecasts()`: Compute RMSE and MAE

### Reporting
- `build_report()`: Generate tables and plots
- `mfvar_cli()`: Command-line interface

## Testing

Run tests with:

```r
devtools::test()
```

Test coverage includes:
- Preprocessing and aggregation
- State space construction
- Gibbs sampling
- Forecasting
- AR(2) benchmarks

## Demo Scripts

The main demonstration uses real Swiss National Bank economic data (2000-2023):

**`run_real_data_demo.R`**: Complete mixed-frequency VAR pipeline
- Real SNB data: Monthly CPI & EUR/CHF exchange rate (288 obs each)
- Quarterly Real GDP (48 observations, quarterly aggregation)
- Bayesian estimation with Minnesota prior
- 24-month ahead forecasts with uncertainty quantification
- Publication-quality visualizations (4 PNG graphs)
- Outputs: Posterior estimates, forecasts, latent states

Run from the package directory:
```r
Rscript run_real_data_demo.R
```

Output files saved to `../output/mfvar_real_data/`:
- Forecast plots with 90% credible intervals
- Latent monthly GDP states visualization
- Posterior parameter estimates (RDS and CSV)
- Forecast summaries

## Requirements

The package requires:
- R ≥ 4.0
- KFAS for state space models
- mvtnorm for multivariate normal sampling
- data.table for efficient data manipulation
- zoo for date handling

## Methodology

The MF-VAR follows Schorfheide and Song:

1. **Latent monthly VAR**: y\_t ~ VAR(p) at monthly frequency
2. **Quarterly measurement**: Observed quarterly value = (y\_t + y\_{t-1} + y\_{t-2}) / 3
3. **Bayesian estimation**: Minnesota prior + Gibbs sampling
4. **Prior selection**: Maximize marginal data density
5. **Forecasting**: Predictive simulation from posterior

## Citation

If you use this package, please cite:

Schorfheide, F., & Song, D. (2015). Real-time forecasting with a mixed-frequency VAR. *Journal of Business & Economic Statistics*, 33(3), 366-380.

## License

MIT License

## Author

Sam Bartlett (2025)

## Support

For issues and questions, please open an issue on the repository.
