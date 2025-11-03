# Submission Folder

This folder contains the complete Mixed Frequency VAR implementation and data for the project.

## Structure

```
submission/
├── mfvar/                   # MF-VAR R package
│   ├── R/                  # Package source files
│   ├── tests/              # Unit tests
│   ├── run_full_pipeline.R # Main analysis pipeline
│   ├── run_pipeline.sh     # Shell wrapper
│   └── DESCRIPTION         # Package metadata
├── data/                    # Raw data files
│   ├── CPI-*.csv           # Consumer Price Index (SNB)
│   ├── Forex-*.csv         # Foreign exchange rates (SNB)
│   ├── Labour_market-*.csv # Employment statistics (SNB)
│   └── data_quarterly.csv  # Quarterly GDP data (KOF)
├── output/                  # Generated outputs
│   └── mfvar_full_pipeline/ # Pipeline results
└── AUTHORS.yml              # Project authors
```

## Running the Analysis

### Quick Start

```bash
cd mfvar
./run_pipeline.sh
```

Or directly with R:

```bash
cd mfvar
Rscript run_full_pipeline.R
```

### What It Does

The pipeline:
1. **Parses raw SNB CSV files** directly (no pre-processing needed)
2. **Merges** monthly CPI, forex, employment with quarterly GDP
3. **Estimates** a Bayesian Mixed-Frequency VAR(2) model
4. **Generates** 24-month forecasts with uncertainty intervals
5. **Saves** results to `output/mfvar_full_pipeline/`

### Output Files

After running, you'll find in `output/mfvar_full_pipeline/`:

**Data Files:**
- `forecast_summary.csv` - Point forecasts for all variables
- `processed_data.csv` - Cleaned and merged data
- `posterior.rds` - MCMC posterior draws
- `forecasts.rds` - Full forecast distributions

**Visualizations:**
- `forecast_log_cpi.png` - CPI forecast
- `forecast_log_eur.png` - EUR/CHF forecast  
- `forecast_log_usd.png` - USD/CHF forecast
- `forecast_log_unemp.png` - Employment forecast
- `forecast_log_gdp.png` - GDP forecast

## Data Sources

All data is parsed from official SNB (Swiss National Bank) sources:

- **CPI**: https://data.snb.ch/en/topics/uvo/cube/plkopr
- **Forex**: https://data.snb.ch/en/topics/ziredev/cube/devkum
- **Labour**: https://data.snb.ch/en/topics/uvo/cube/amarbma
- **GDP**: KOF quarterly forecasts

See `data/data_sources.md` for detailed documentation.

## Package Documentation

For technical details about the MF-VAR implementation, see:
- `mfvar/README.md` - Package overview
- `mfvar/RAW_DATA_PIPELINE.md` - Data processing documentation
