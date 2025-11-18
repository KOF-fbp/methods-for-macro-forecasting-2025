# Mixed Frequency VAR - Quick Start

## Overview

This submission contains a complete implementation of a **Mixed Frequency Vector Autoregression (MF-VAR)** model following Schorfheide & Song's approach, with Bayesian estimation using Minnesota priors.

## Running the Analysis

From the `submission` directory:

```bash
cd mfvar
./run_pipeline.sh
```

The pipeline will:
1. Parse raw SNB data files
2. Merge monthly (CPI, forex, employment) and quarterly (GDP) data  
3. Estimate a Bayesian VAR(2) model via Gibbs sampling
4. Generate 24-month forecasts
5. Save results to `../output/mfvar_full_pipeline/`

## What You Get

**Data Coverage:**
- 285 monthly observations (2002-2025)
- 5 variables: CPI, EUR/CHF, USD/CHF, Employment, GDP
- 95 GDP quarters

**Model:**
- Specification: VAR(2) with mixed frequencies
- Prior: Minnesota (λ=0.5, ν=1.0, θ=0.5)
- Estimation: Gibbs sampling (500 iterations, 250 burnin)

**Outputs** (in `output/mfvar_full_pipeline/`):
- `forecast_log_*.png` - 5 forecast visualizations
- `forecast_summary.csv` - Point forecasts
- `posterior.rds` - MCMC posterior draws
- `forecasts.rds` - Full forecast distributions

## Structure

```
submission/
├── mfvar/                   # MF-VAR R package
│   ├── run_full_pipeline.R # Main script
│   ├── R/                  # Core functions
│   └── tests/              # Unit tests
├── data/                    # Raw SNB/KOF data
│   ├── CPI-*.csv
│   ├── Forex-*.csv
│   ├── Labour_market-*.csv
│   └── data_quarterly.csv
└── output/                  # Generated results
    └── mfvar_full_pipeline/
```

## Requirements

The following R packages are required (specified in `mfvar/DESCRIPTION`):
- `data.table`, `zoo`, `lubridate` - Data manipulation
- `KFAS` - State space models (Kalman filter)
- `mvtnorm` - Multivariate normal distributions
- `ggplot2` - Visualizations
- `Matrix`, `yaml`, `cli` - Utilities

## Technical Details

For detailed documentation:
- **Data Processing**: See `mfvar/RAW_DATA_PIPELINE.md`
- **Package Overview**: See `mfvar/README.md`
- **Data Sources**: See `data/data_sources.md`

## Key Features

✅ **Raw Data Parsing**: Directly reads SNB CSV files (no preprocessing)
✅ **Official Dimension Codes**: Uses SNB data cube labeling
✅ **Mixed Frequency**: Handles monthly and quarterly data simultaneously
✅ **Bayesian Estimation**: Minnesota prior via dummy observations
✅ **Gibbs Sampling**: MCMC for posterior inference
✅ **State Space**: KFAS framework for Kalman filtering
✅ **Forecasting**: 24-month ahead with uncertainty quantification

## Example Output

After running, check `output/mfvar_full_pipeline/forecast_summary.csv`:

```
horizon,date,log_cpi,log_eur,log_usd,log_unemp,log_gdp
1,2025-10-30,4.678,-0.054,-0.216,1.553,12.251
2,2025-11-30,4.679,-0.039,-0.205,1.553,12.269
3,2025-12-30,4.680,-0.025,-0.189,1.555,12.287
...
```

The forecasts show coherent trajectories:
- CPI: Stable around 4.68 (log level)
- EUR/CHF: Gradual appreciation (-0.054 → +0.025)
- GDP: Moderate growth (12.25 → 12.40)

---

**Authors**: See `AUTHORS.yml`
**License**: MIT (see `mfvar/LICENSE`)
