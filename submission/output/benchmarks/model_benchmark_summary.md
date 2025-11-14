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
| Cutoff only | AR(2) | 1-step ahead | 84 | 5.2372 | 1.9049 |
| Cutoff only | AR(2) | 1-year ahead | 84 | 4.7482 | 1.8450 |
| Cutoff only | MF-VAR | 1-step ahead | 81 | 7.0883 | 2.2485 |
| Cutoff only | MF-VAR | 1-year ahead | 81 | 4.8999 | 2.0034 |
| Cutoff only | MIDAS | 1-step ahead | 84 | 4.0665 | 1.8070 |
| Cutoff only | MIDAS | 1-year ahead | 84 | 4.9274 | 1.8849 |
| Cutoff only | MIDAS (trend) | 1-step ahead | 84 | 4.1587 | 1.8601 |
| Cutoff only | MIDAS (trend) | 1-year ahead | 84 | 4.9795 | 2.0038 |
| Cutoff only | MIDAS-Latent | 1-step ahead | 81 | 7.3996 | 2.1620 |
| Cutoff only | MIDAS-Latent | 1-year ahead | 81 | 7.0620 | 2.6733 |
| Cutoff only | MIDAS-Latent (trend) | 1-step ahead | 81 | 7.4410 | 2.1540 |
| Cutoff only | MIDAS-Latent (trend) | 1-year ahead | 81 | 7.0922 | 2.6949 |
| Cutoff +1m | AR(2) | 1-step ahead | 84 | 5.2372 | 1.9049 |
| Cutoff +1m | AR(2) | 1-year ahead | 84 | 4.7482 | 1.8450 |
| Cutoff +1m | MF-VAR | 1-step ahead | 81 | 7.0883 | 2.2485 |
| Cutoff +1m | MF-VAR | 1-year ahead | 81 | 4.9514 | 2.0340 |
| Cutoff +1m | MIDAS | 1-step ahead | 84 | 4.1519 | 1.5105 |
| Cutoff +1m | MIDAS | 1-year ahead | 84 | 4.9274 | 1.8849 |
| Cutoff +1m | MIDAS (trend) | 1-step ahead | 84 | 4.2527 | 1.6141 |
| Cutoff +1m | MIDAS (trend) | 1-year ahead | 84 | 4.9795 | 2.0038 |
| Cutoff +1m | MIDAS-Latent | 1-step ahead | 81 | 7.2272 | 2.1533 |
| Cutoff +1m | MIDAS-Latent | 1-year ahead | 81 | 6.9343 | 2.6507 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-step ahead | 81 | 7.2659 | 2.1248 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-year ahead | 81 | 6.9596 | 2.6721 |
| Cutoff +2m | AR(2) | 1-step ahead | 84 | 5.2372 | 1.9049 |
| Cutoff +2m | AR(2) | 1-year ahead | 84 | 4.7482 | 1.8450 |
| Cutoff +2m | MF-VAR | 1-step ahead | 81 | 7.0883 | 2.2485 |
| Cutoff +2m | MF-VAR | 1-year ahead | 81 | 4.9109 | 1.9776 |
| Cutoff +2m | MIDAS | 1-step ahead | 84 | 4.7811 | 1.6263 |
| Cutoff +2m | MIDAS | 1-year ahead | 84 | 4.9274 | 1.8849 |
| Cutoff +2m | MIDAS (trend) | 1-step ahead | 84 | 4.8181 | 1.7079 |
| Cutoff +2m | MIDAS (trend) | 1-year ahead | 84 | 4.9795 | 2.0038 |
| Cutoff +2m | MIDAS-Latent | 1-step ahead | 81 | 7.2954 | 2.1698 |
| Cutoff +2m | MIDAS-Latent | 1-year ahead | 81 | 6.9830 | 2.6442 |
| Cutoff +2m | MIDAS-Latent (trend) | 1-step ahead | 81 | 7.3331 | 2.1362 |
| Cutoff +2m | MIDAS-Latent (trend) | 1-year ahead | 81 | 7.0085 | 2.6638 |

