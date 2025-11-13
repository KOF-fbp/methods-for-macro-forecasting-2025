# Benchmark Error Summary

## Holdout RMSE and MAE by Horizon (aggregated across variables)
| Model | Horizon | Observations | RMSE | MAE |
| --- | --- | --- | --- | --- |
| AR(2) | 1-step ahead | 3 | 2.2741 | 1.6645 |
| AR(2) | 1-year ahead | 3 | 1.6076 | 1.0885 |
| MF-VAR | 1-step ahead | 3 | 1.5433 | 1.0849 |
| MF-VAR | 1-year ahead | 3 | 3.0170 | 2.2032 |
| MIDAS | 1-step ahead | 3 | 0.9803 | 0.6799 |
| MIDAS | 1-year ahead | 3 | 2.5684 | 1.5926 |
| MIDAS (trend) | 1-step ahead | 3 | 0.6828 | 0.4365 |
| MIDAS (trend) | 1-year ahead | 3 | 2.8991 | 1.8987 |
| MIDAS-Latent | 1-step ahead | 3 | 2.1473 | 1.7542 |
| MIDAS-Latent | 1-year ahead | 3 | 2.2686 | 1.8031 |
| MIDAS-Latent (trend) | 1-step ahead | 3 | 1.9975 | 1.6297 |
| MIDAS-Latent (trend) | 1-year ahead | 3 | 2.1612 | 1.6591 |


## Holdout Overall Average Errors
| Model | Observations | RMSE | MAE |
| --- | --- | --- | --- |
| AR(2) | 12 | 1.5964 | 1.0080 |
| MF-VAR | 12 | 1.7975 | 1.1411 |
| MIDAS | 12 | 1.4029 | 0.6837 |
| MIDAS (trend) | 12 | 1.5530 | 0.7946 |
| MIDAS-Latent | 12 | 1.7302 | 1.2405 |
| MIDAS-Latent (trend) | 12 | 1.5754 | 1.0984 |


## Expanding Window Cross-Validation RMSE and MAE by Monthly Coverage
Cross-validation skipped (--fast/--no-cv).
