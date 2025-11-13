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
| Cutoff only | AR(2) | 1-step ahead | 9 | 1.9337 | 1.4090 |
| Cutoff only | AR(2) | 1-year ahead | 9 | 1.3256 | 0.9071 |
| Cutoff only | MF-VAR | 1-step ahead | 9 | 1.5801 | 1.2045 |
| Cutoff only | MF-VAR | 1-year ahead | 9 | 2.0729 | 1.3039 |
| Cutoff only | MIDAS | 1-step ahead | 9 | 2.4340 | 1.5829 |
| Cutoff only | MIDAS | 1-year ahead | 9 | 0.7982 | 0.4858 |
| Cutoff only | MIDAS (trend) | 1-step ahead | 9 | 2.2386 | 1.4219 |
| Cutoff only | MIDAS (trend) | 1-year ahead | 9 | 0.7884 | 0.6006 |
| Cutoff only | MIDAS-Latent | 1-step ahead | 9 | 1.7502 | 1.3299 |
| Cutoff only | MIDAS-Latent | 1-year ahead | 9 | 1.6121 | 1.0145 |
| Cutoff only | MIDAS-Latent (trend) | 1-step ahead | 9 | 1.6530 | 1.2880 |
| Cutoff only | MIDAS-Latent (trend) | 1-year ahead | 9 | 1.4560 | 0.8661 |
| Cutoff +1m | AR(2) | 1-step ahead | 9 | 1.9337 | 1.4090 |
| Cutoff +1m | AR(2) | 1-year ahead | 9 | 1.3256 | 0.9071 |
| Cutoff +1m | MF-VAR | 1-step ahead | 9 | 1.5801 | 1.2045 |
| Cutoff +1m | MF-VAR | 1-year ahead | 9 | 1.9738 | 1.3184 |
| Cutoff +1m | MIDAS | 1-step ahead | 9 | 2.1300 | 1.2155 |
| Cutoff +1m | MIDAS | 1-year ahead | 9 | 0.7982 | 0.4858 |
| Cutoff +1m | MIDAS (trend) | 1-step ahead | 9 | 1.9997 | 1.1592 |
| Cutoff +1m | MIDAS (trend) | 1-year ahead | 9 | 0.7884 | 0.6006 |
| Cutoff +1m | MIDAS-Latent | 1-step ahead | 9 | 1.7535 | 1.3550 |
| Cutoff +1m | MIDAS-Latent | 1-year ahead | 9 | 1.6201 | 0.9953 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-step ahead | 9 | 1.6671 | 1.3119 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-year ahead | 9 | 1.4752 | 0.8752 |
| Cutoff +2m | AR(2) | 1-step ahead | 9 | 1.9337 | 1.4090 |
| Cutoff +2m | AR(2) | 1-year ahead | 9 | 1.3256 | 0.9071 |
| Cutoff +2m | MF-VAR | 1-step ahead | 9 | 1.5801 | 1.2045 |
| Cutoff +2m | MF-VAR | 1-year ahead | 9 | 1.9346 | 1.2528 |
| Cutoff +2m | MIDAS | 1-step ahead | 9 | 2.1180 | 1.2340 |
| Cutoff +2m | MIDAS | 1-year ahead | 9 | 0.7982 | 0.4858 |
| Cutoff +2m | MIDAS (trend) | 1-step ahead | 9 | 1.9979 | 1.1932 |
| Cutoff +2m | MIDAS (trend) | 1-year ahead | 9 | 0.7884 | 0.6006 |
| Cutoff +2m | MIDAS-Latent | 1-step ahead | 9 | 1.7165 | 1.3166 |
| Cutoff +2m | MIDAS-Latent | 1-year ahead | 9 | 1.6109 | 0.9715 |
| Cutoff +2m | MIDAS-Latent (trend) | 1-step ahead | 9 | 1.6331 | 1.2742 |
| Cutoff +2m | MIDAS-Latent (trend) | 1-year ahead | 9 | 1.4596 | 0.8597 |

