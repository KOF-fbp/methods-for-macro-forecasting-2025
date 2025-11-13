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
| Cutoff only | AR(2) | 1-step ahead | 6 | 2.2460 | 1.7200 |
| Cutoff only | AR(2) | 1-year ahead | 6 | 1.2496 | 0.8385 |
| Cutoff only | MF-VAR | 1-step ahead | 6 | 1.3954 | 1.0401 |
| Cutoff only | MF-VAR | 1-year ahead | 6 | 2.3856 | 1.5902 |
| Cutoff only | MIDAS | 1-step ahead | 6 | 2.8080 | 1.8466 |
| Cutoff only | MIDAS | 1-year ahead | 6 | 0.5634 | 0.3825 |
| Cutoff only | MIDAS (trend) | 1-step ahead | 6 | 2.6106 | 1.7266 |
| Cutoff only | MIDAS (trend) | 1-year ahead | 6 | 0.6577 | 0.5302 |
| Cutoff only | MIDAS-Latent | 1-step ahead | 6 | 1.8573 | 1.4506 |
| Cutoff only | MIDAS-Latent | 1-year ahead | 6 | 1.6365 | 1.0186 |
| Cutoff only | MIDAS-Latent (trend) | 1-step ahead | 6 | 1.8371 | 1.4810 |
| Cutoff only | MIDAS-Latent (trend) | 1-year ahead | 6 | 1.5449 | 0.8731 |
| Cutoff +1m | AR(2) | 1-step ahead | 6 | 2.2460 | 1.7200 |
| Cutoff +1m | AR(2) | 1-year ahead | 6 | 1.2496 | 0.8385 |
| Cutoff +1m | MF-VAR | 1-step ahead | 6 | 1.3954 | 1.0401 |
| Cutoff +1m | MF-VAR | 1-year ahead | 6 | 2.2594 | 1.5895 |
| Cutoff +1m | MIDAS | 1-step ahead | 6 | 2.2839 | 1.1802 |
| Cutoff +1m | MIDAS | 1-year ahead | 6 | 0.5634 | 0.3825 |
| Cutoff +1m | MIDAS (trend) | 1-step ahead | 6 | 2.1855 | 1.2169 |
| Cutoff +1m | MIDAS (trend) | 1-year ahead | 6 | 0.6577 | 0.5302 |
| Cutoff +1m | MIDAS-Latent | 1-step ahead | 6 | 1.8314 | 1.4595 |
| Cutoff +1m | MIDAS-Latent | 1-year ahead | 6 | 1.6141 | 0.9632 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-step ahead | 6 | 1.8304 | 1.4889 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-year ahead | 6 | 1.5432 | 0.8634 |
| Cutoff +2m | AR(2) | 1-step ahead | 6 | 2.2460 | 1.7200 |
| Cutoff +2m | AR(2) | 1-year ahead | 6 | 1.2496 | 0.8385 |
| Cutoff +2m | MF-VAR | 1-step ahead | 6 | 1.3954 | 1.0401 |
| Cutoff +2m | MF-VAR | 1-year ahead | 6 | 2.1813 | 1.4785 |
| Cutoff +2m | MIDAS | 1-step ahead | 6 | 2.2393 | 1.1889 |
| Cutoff +2m | MIDAS | 1-year ahead | 6 | 0.5634 | 0.3825 |
| Cutoff +2m | MIDAS (trend) | 1-step ahead | 6 | 2.1589 | 1.2524 |
| Cutoff +2m | MIDAS (trend) | 1-year ahead | 6 | 0.6577 | 0.5302 |
| Cutoff +2m | MIDAS-Latent | 1-step ahead | 6 | 1.7964 | 1.4357 |
| Cutoff +2m | MIDAS-Latent | 1-year ahead | 6 | 1.6163 | 0.9625 |
| Cutoff +2m | MIDAS-Latent (trend) | 1-step ahead | 6 | 1.8046 | 1.4692 |
| Cutoff +2m | MIDAS-Latent (trend) | 1-year ahead | 6 | 1.5408 | 0.8780 |

