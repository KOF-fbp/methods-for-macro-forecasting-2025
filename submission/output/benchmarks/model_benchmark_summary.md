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
| Monthly data | Model | Horizon | Observations | RMSE | MAE |
| --- | --- | --- | --- | --- | --- |
| Cutoff only | AR(2) | 1-step ahead | 3 | 2.2741 | 1.6645 |
| Cutoff only | AR(2) | 1-year ahead | 3 | 1.6076 | 1.0885 |
| Cutoff only | MF-VAR | 1-step ahead | 3 | 1.5433 | 1.0849 |
| Cutoff only | MF-VAR | 1-year ahead | 3 | 3.0609 | 2.2073 |
| Cutoff only | MIDAS | 1-step ahead | 3 | 3.3453 | 2.0643 |
| Cutoff only | MIDAS | 1-year ahead | 3 | 0.1428 | 0.1365 |
| Cutoff only | MIDAS (trend) | 1-step ahead | 3 | 3.0759 | 1.8265 |
| Cutoff only | MIDAS (trend) | 1-year ahead | 3 | 0.4945 | 0.4256 |
| Cutoff only | MIDAS-Latent | 1-step ahead | 3 | 2.1839 | 1.7842 |
| Cutoff only | MIDAS-Latent | 1-year ahead | 3 | 2.2880 | 1.8264 |
| Cutoff only | MIDAS-Latent (trend) | 1-step ahead | 3 | 2.0267 | 1.6555 |
| Cutoff only | MIDAS-Latent (trend) | 1-year ahead | 3 | 2.1834 | 1.6873 |
| Cutoff +1m | AR(2) | 1-step ahead | 3 | 2.2741 | 1.6645 |
| Cutoff +1m | AR(2) | 1-year ahead | 3 | 1.6076 | 1.0885 |
| Cutoff +1m | MF-VAR | 1-step ahead | 3 | 1.5433 | 1.0849 |
| Cutoff +1m | MF-VAR | 1-year ahead | 3 | 2.8771 | 2.1173 |
| Cutoff +1m | MIDAS | 1-step ahead | 3 | 0.1245 | 0.0949 |
| Cutoff +1m | MIDAS | 1-year ahead | 3 | 0.1428 | 0.1365 |
| Cutoff +1m | MIDAS (trend) | 1-step ahead | 3 | 0.2082 | 0.1663 |
| Cutoff +1m | MIDAS (trend) | 1-year ahead | 3 | 0.4945 | 0.4256 |
| Cutoff +1m | MIDAS-Latent | 1-step ahead | 3 | 2.1143 | 1.7278 |
| Cutoff +1m | MIDAS-Latent | 1-year ahead | 3 | 2.2756 | 1.8008 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-step ahead | 3 | 1.9639 | 1.6022 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-year ahead | 3 | 2.1808 | 1.6664 |

