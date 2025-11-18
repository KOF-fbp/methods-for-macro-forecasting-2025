# Benchmark Error Summary

## Holdout RMSE and MAE by Horizon (aggregated across variables)
| Model | Horizon | Observations | RMSE | MAE |
| --- | --- | --- | --- | --- |
| AR(2) | 1-step ahead | 3 | 0.2168 | 0.1709 |
| AR(2) | 1-year ahead | 3 | 1.6470 | 1.0700 |
| MF-VAR | 1-step ahead | 3 | 0.3775 | 0.2526 |
| MF-VAR | 1-year ahead | 3 | 0.9283 | 0.7577 |
| MIDAS | 1-step ahead | 3 | 1.1472 | 0.8953 |
| MIDAS | 1-year ahead | 3 | 2.9014 | 1.7715 |
| MIDAS (trend) | 1-step ahead | 3 | 1.3077 | 0.9566 |
| MIDAS (trend) | 1-year ahead | 3 | 2.6849 | 1.5651 |
| MIDAS-Latent | 1-step ahead | 3 | 1.6031 | 1.2035 |
| MIDAS-Latent | 1-year ahead | 3 | 0.7538 | 0.5252 |
| MIDAS-Latent (trend) | 1-step ahead | 3 | 1.6360 | 1.1479 |
| MIDAS-Latent (trend) | 1-year ahead | 3 | 0.8366 | 0.4928 |


## Holdout Overall Average Errors
| Model | Observations | RMSE | MAE |
| --- | --- | --- | --- |
| AR(2) | 12 | 0.9712 | 0.5480 |
| MF-VAR | 12 | 1.2983 | 0.8036 |
| MIDAS | 12 | 1.7169 | 0.9829 |
| MIDAS (trend) | 12 | 1.6147 | 0.9110 |
| MIDAS-Latent | 12 | 1.4621 | 0.9944 |
| MIDAS-Latent (trend) | 12 | 1.5073 | 0.9506 |


## Expanding Window Cross-Validation RMSE and MAE by Monthly Coverage
Cross-validation skipped (--fast/--no-cv).
