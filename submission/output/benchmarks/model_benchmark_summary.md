# Benchmark Error Summary

## Holdout RMSE and MAE by Horizon (aggregated across variables)
| Model | Horizon | Observations | RMSE | MAE |
| --- | --- | --- | --- | --- |
| AR(2) | 1-step ahead | 3 | 0.2168 | 0.1709 |
| AR(2) | 1-year ahead | 3 | 1.6470 | 1.0700 |
| MF-VAR | 1-step ahead | 3 | 0.3775 | 0.2526 |
| MF-VAR | 1-year ahead | 3 | 1.7000 | 1.1287 |
| MIDAS | 1-step ahead | 0 | NA | NA |
| MIDAS | 1-year ahead | 0 | NA | NA |
| MIDAS (trend) | 1-step ahead | 0 | NA | NA |
| MIDAS (trend) | 1-year ahead | 0 | NA | NA |


## Holdout Overall Average Errors
| Model | Observations | RMSE | MAE |
| --- | --- | --- | --- |
| AR(2) | 12 | 0.9712 | 0.5480 |
| MF-VAR | 12 | 1.0264 | 0.5654 |
| MIDAS | 0 | NA | NA |
| MIDAS (trend) | 0 | NA | NA |


## Rolling Cross-Validation RMSE and MAE by Monthly Coverage
Cross-validation skipped (--fast/--no-cv).
