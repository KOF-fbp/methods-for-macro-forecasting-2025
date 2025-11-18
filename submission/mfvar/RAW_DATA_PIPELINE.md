# Raw SNB Data Pipeline

## Overview

The `run_full_pipeline.R` script has been updated to parse **raw SNB CSV files directly** instead of using pre-parsed data. This ensures full transparency and reproducibility of the data processing workflow.

## Data Sources

All data is parsed directly from the raw CSV files using official SNB dimension codes:

### Monthly Data (SNB)

1. **Consumer Price Index (CPI)**
   - File: `CPI-snb-data-plkopr-en-all-20251021_0900.csv`
   - Source: https://data.snb.ch/en/topics/uvo/cube/plkopr
   - Dimension: `LD2010100` (CPI index, base 2010=100)
   
2. **Foreign Exchange Rates**
   - File: `Forex_snb-data-devkum-en-all-20251001-1430.csv`
   - Source: https://data.snb.ch/en/topics/ziredev/cube/devkum
   - Dimensions: 
     - `M0;EUR1` (EUR/CHF monthly average)
     - `M0;USD1` (USD/CHF monthly average)

3. **Labour Market**
   - File: `Labour_market_snb-data-amarbma-en-all-20251021-1035.csv`
   - Source: https://data.snb.ch/en/topics/uvo/cube/amarbma
   - Dimension: `E` (Total employment in persons, scaled to millions)

### Quarterly Data (KOF)

4. **Real GDP**
   - File: `data_quarterly.csv`
   - KOF quarterly economic forecasts
   - Variable: `gdp` (Real GDP)

## Data Processing Steps

The pipeline executes the following steps:

1. **Parse Raw SNB CSVs**
   - Reads semicolon-separated files
   - Skips 3 header rows
   - Extracts specific dimension codes
   - Converts dates to end-of-month format

2. **Merge All Sources**
   - Merges monthly CPI, forex, and labour data
   - Left-joins quarterly GDP data
   - Finds maximum common period with all monthly variables

3. **Data Coverage**
   - **Period**: 2002-01-31 to 2025-09-30
   - **Monthly observations**: 285
   - **GDP quarters**: 95
   - **Rationale**: Restricted to 2002+ for numerical stability (previous attempts with 1999+ data encountered convergence issues)

4. **Transformations**
   - Log transformation: `log(cpi_index)`, `log(eur_chf)`, `log(usd_chf)`, `log(unemployment_total/1000000)`, `log(gdp)`
   - Standardization: All variables scaled to mean=0, sd=1

5. **Model Estimation**
   - **Specification**: VAR(2) with 5 variables (4 monthly + 1 quarterly)
   - **Prior**: Minnesota prior (λ=0.5, ν=1.0, θ=0.5)
   - **Method**: Gibbs sampling (500 iterations, 250 burnin)

6. **Forecasting**
   - **Horizon**: 24 months ahead
   - **Uncertainty**: 90% credible intervals
   - **Method**: Posterior predictive distribution

## Key Improvements Over Previous Versions

### No Pre-Parsed Data Dependency
- ❌ **OLD**: Required `submission/data/parsed/monthly_data.csv`
- ✅ **NEW**: Parses raw SNB CSVs directly

### Proper SNB Dimension Labeling
- Uses official dimension codes from SNB data cubes
- Transparent mapping documented in code comments
- Easy to verify against SNB website

### Numerical Stability
- Employment data scaled from millions to reasonable units
- Date range restricted to avoid convergence issues
- Standardization applied for KFAS compatibility

## File Structure

```
submission/
├── mfvar/                        # MF-VAR R package
│   ├── run_full_pipeline.R      # Main pipeline script
│   ├── run_pipeline.sh          # Shell wrapper for execution
│   ├── DESCRIPTION              # Package metadata
│   ├── NAMESPACE                # Exported functions
│   └── R/                       # Package source files
│       ├── gibbs.R
│       ├── forecast.R
│       ├── state_space.R
│       └── ...
├── data/                         # Raw data files
│   ├── CPI-snb-data-plkopr-en-all-20251021_0900.csv
│   ├── Forex_snb-data-devkum-en-all-20251001-1430.csv
│   ├── Labour_market_snb-data-amarbma-en-all-20251021-1035.csv
│   └── data_quarterly.csv
└── output/                       # Generated outputs
    └── mfvar_full_pipeline/
```

## Running the Pipeline

### Option 1: Shell Script
```bash
cd submission/mfvar
./run_pipeline.sh
```

### Option 2: Direct R Execution
```bash
cd submission/mfvar
Rscript run_full_pipeline.R
```

## Expected Output

The pipeline generates files in `../output/mfvar_full_pipeline/`:

### Data Files
- `forecasts_mean.csv` - Point forecasts for all variables
- `forecasts_intervals.csv` - 90% credible intervals
- `historical_data.csv` - Historical time series
- `posterior_draws.rds` - MCMC posterior samples

### Visualizations
- `forecast_cpi.png` - CPI forecast
- `forecast_eur.png` - EUR/CHF forecast
- `forecast_usd.png` - USD/CHF forecast
- `forecast_unemployment.png` - Employment forecast
- `forecast_gdp.png` - GDP forecast
- `forecast_all_variables.png` - Combined plot

## Troubleshooting

### Numerical Issues in Gibbs Sampler
If you encounter "Numerical issues in Sigma" warnings:
- Ensure data is standardized (mean=0, sd=1)
- Check for extreme outliers in raw data
- Consider tightening the Minnesota prior (increase λ)
- Verify date range alignment

### Missing Raw Data Files
If SNB files are missing:
- Download from SNB website URLs listed above
- Ensure filenames match exactly
- Place in `submission/data/` directory

## Data Quality Notes

### Employment vs. Unemployment
The SNB labour market data (dimension `E`) represents total **employment** (number of employed persons), not unemployment. Values are in millions of persons and range from ~2.2M (1948) to ~4.7M (2025).

### Scaling Rationale
Employment data is divided by 1,000,000 to convert to millions before log transformation. This prevents numerical overflow when combined with price indices and exchange rates in the VAR.

### Date Coverage Trade-off
While the raw data extends back to 1914 (forex) and 1921 (CPI), the pipeline uses 2002+ to ensure:
1. All monthly variables have observations
2. GDP quarterly data is available
3. Numerical stability in Gibbs sampling

## Verification

To verify the pipeline uses raw data:

1. Check Step 1 output: Should show "PARSING RAW SNB CSV FILES"
2. Inspect data counts: CPI (1257 obs), Forex (1341 obs), Labour (933 obs)
3. Confirm no references to `parsed/` directory
4. Review final dataset: 285 observations from 2002-01-31 to 2025-09-30

##Changes Made

1. Added `parse_snb_file()` function for semicolon-separated SNB format
2. Created specific parsers: `parse_monthly_cpi()`, `parse_monthly_forex()`, `parse_monthly_labour()`, `parse_quarterly_data()`
3. Integrated parsing into Step 1 of pipeline
4. Removed dependency on pre-parsed files
5. Added scaling for employment data (÷ 1,000,000)
6. Documented all SNB dimension codes with source URLs

---

**Status**: Pipeline updated and tested. All raw data parsing functional. Forecasts coherent and numerically stable.
