# Latent States Visualization Enhancement

## Overview

Enhanced latent states visualization comparing MF-VAR posterior mean latent states with actual quarterly observations. This provides visual validation of how well the state-space model tracks observed data.

## New Function

Added `plot_latent_states_with_actuals()` to `R/latent_states.R`:

```r
plot_latent_states_with_actuals(
  states_df,           # Latent states dataframe from extract_latent_states()
  qdat_orig,           # Original quarterly data (before stationarization)
  out_dir,             # Output directory for plot
  target_variables,    # Vector of variable names to plot
  filename = NULL      # Optional custom filename
)
```

## Features

1. **Side-by-side comparison**: Overlays latent states (dashed line) with actual quarterly values (solid line)
2. **Faceted by variable**: Separate panels for GDP growth, inflation, and exchange rate
3. **Free y-axis scales**: Each variable uses appropriate scale for readability
4. **Color coding**: 
   - Orange solid line = Actual quarterly observations
   - Green dashed line = MF-VAR latent state (posterior mean)

## Output Location

- **File**: `output/forecasts/mfvar/plots/mfvar_latent_vs_actual.png`
- **Dimensions**: 10" × 8" at 150 DPI
- **Format**: PNG

## Integration

Automatically generated when running:
```bash
Rscript main.R mfvar
# or
Rscript run_mfvar_package.R
```

The plot is created alongside existing latent states visualizations:
- `mfvar_latent_states_timeseries.png` (faceted time series)
- `mfvar_latent_states_heatmap.png` (standardized heatmap)
- `mfvar_latent_vs_actual.png` (NEW: comparison with actuals)

## Use Cases

1. **Model validation**: Verify latent states align with observed quarterly data
2. **Presentation slides**: Show how MF-VAR captures quarterly dynamics through monthly interpolation
3. **Extension documentation**: Demonstrates the quality of latent states used in MIDAS-Latent approach

## Technical Details

- Converts quarterly dates to Date format for alignment
- Handles missing variables gracefully with informative error messages
- Uses `zoo::as.yearqtr()` for proper quarter-to-date conversion
- Long-format data structure for efficient ggplot2 visualization
- Minimal theme with clear legend positioning

## Example Interpretation

When latent states closely track actual values:
- ✓ Model successfully captures quarterly dynamics from monthly data
- ✓ Kalman filter/smoother appropriately interpolates missing observations
- ✓ Latent states are reliable for use as MIDAS regressors (MIDAS-Latent extension)

Divergences may indicate:
- Structural breaks in the data
- Monthly indicator misalignment with quarterly targets
- Prior specification issues (Minnesota shrinkage too strong/weak)

## Date Created

2025-11-18
