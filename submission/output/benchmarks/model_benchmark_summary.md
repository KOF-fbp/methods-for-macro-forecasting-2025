# Benchmark Error Summary

## Holdout RMSE and MAE by Horizon (aggregated across variables)
| Model | Horizon | Observations | RMSE | MAE |
| --- | --- | --- | --- | --- |
| AR(2) | 1-step ahead | 3 | 0.2168 | 0.1709 |
| AR(2) | 1-year ahead | 3 | 1.6470 | 1.0700 |
| MF-VAR | 1-step ahead | 3 | 0.3775 | 0.2526 |
| MF-VAR | 1-year ahead | 3 | 1.3051 | 0.9921 |
| MIDAS | 1-step ahead | 3 | 1.1472 | 0.8953 |
| MIDAS | 1-year ahead | 3 | 2.9014 | 1.7715 |
| MIDAS (trend) | 1-step ahead | 3 | 1.3077 | 0.9566 |
| MIDAS (trend) | 1-year ahead | 3 | 2.6849 | 1.5651 |
| MIDAS-Latent | 1-step ahead | 3 | 0.8129 | 0.6741 |
| MIDAS-Latent | 1-year ahead | 3 | 0.3265 | 0.2607 |
| MIDAS-Latent (trend) | 1-step ahead | 3 | 0.7912 | 0.6255 |
| MIDAS-Latent (trend) | 1-year ahead | 3 | 0.1583 | 0.1025 |


## Holdout Overall Average Errors
| Model | Observations | RMSE | MAE |
| --- | --- | --- | --- |
| AR(2) | 12 | 0.9712 | 0.5480 |
| MF-VAR | 12 | 0.9355 | 0.6284 |
| MIDAS | 12 | 1.7169 | 0.9829 |
| MIDAS (trend) | 12 | 1.6147 | 0.9110 |
| MIDAS-Latent | 12 | 0.7984 | 0.5603 |
| MIDAS-Latent (trend) | 12 | 0.7743 | 0.4569 |


## Expanding Window Cross-Validation RMSE and MAE by Monthly Coverage
Cross-validation skipped (--fast/--no-cv).
