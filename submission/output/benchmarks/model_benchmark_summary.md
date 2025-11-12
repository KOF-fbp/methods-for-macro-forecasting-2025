# Benchmark Error Summary

## Holdout RMSE and MAE by Horizon (aggregated across variables)
| Model | Horizon | Observations | RMSE | MAE |
|---|---|---|---|---|
| AR(2) | 1-step ahead | 3 | 0.2168 | 0.1709 |
| AR(2) | 1-year ahead | 3 | 1.6470 | 1.0700 |
| MF-VAR | 1-step ahead | 3 | 0.3775 | 0.2526 |
| MF-VAR | 1-year ahead | 3 | 1.7000 | 1.1287 |
| MIDAS | 1-step ahead | 3 | 1.0114 | 0.5926 |
| MIDAS | 1-year ahead | 3 | 2.8735 | 1.7123 |
| MIDAS (trend) | 1-step ahead | 3 | 1.2146 | 0.7355 |
| MIDAS (trend) | 1-year ahead | 3 | 2.6937 | 1.6040 |
| RW-trend | 1-step ahead | 3 | 0.2523 | 0.1623 |
| RW-trend | 1-year ahead | 3 | 1.3066 | 0.9492 |

## Holdout Overall Average Errors
| Model | Observations | RMSE | MAE |
|---|---|---|---|
| AR(2) | 12 | 0.9712 | 0.5480 |
| MF-VAR | 12 | 1.0264 | 0.5654 |
| MIDAS | 12 | 1.6789 | 0.8619 |
| MIDAS (trend) | 12 | 1.6022 | 0.8409 |
| RW-trend | 12 | 0.8732 | 0.4889 |

## Rolling Cross-Validation RMSE and MAE by Monthly Coverage
| Monthly data | Model | Horizon | Observations | RMSE | MAE |
|---|---|---|---|---|---|
| Cutoff only | AR(2) | 1-step ahead | 84 | 5.2488 | 1.8924 |
| Cutoff only | AR(2) | 1-year ahead | 84 | 4.7140 | 1.7552 |
| Cutoff only | MF-VAR | 1-step ahead | 84 | 7.3391 | 2.4448 |
| Cutoff only | MF-VAR | 1-year ahead | 84 | 4.6438 | 1.7089 |
| Cutoff only | MIDAS | 1-step ahead | 0 | NA | NA |
| Cutoff only | MIDAS | 1-year ahead | 0 | NA | NA |
| Cutoff only | MIDAS (trend) | 1-step ahead | 0 | NA | NA |
| Cutoff only | MIDAS (trend) | 1-year ahead | 0 | NA | NA |
| Cutoff only | RW-trend | 1-step ahead | 84 | 7.4792 | 2.3900 |
| Cutoff only | RW-trend | 1-year ahead | 84 | 7.1152 | 2.9191 |
| Cutoff +1m | AR(2) | 1-step ahead | 84 | 5.2488 | 1.8924 |
| Cutoff +1m | AR(2) | 1-year ahead | 84 | 4.7140 | 1.7552 |
| Cutoff +1m | MF-VAR | 1-step ahead | 84 | 7.3391 | 2.4448 |
| Cutoff +1m | MF-VAR | 1-year ahead | 84 | 4.6700 | 1.7491 |
| Cutoff +1m | MIDAS | 1-step ahead | 0 | NA | NA |
| Cutoff +1m | MIDAS | 1-year ahead | 0 | NA | NA |
| Cutoff +1m | MIDAS (trend) | 1-step ahead | 0 | NA | NA |
| Cutoff +1m | MIDAS (trend) | 1-year ahead | 0 | NA | NA |
| Cutoff +1m | RW-trend | 1-step ahead | 84 | 7.4792 | 2.3900 |
| Cutoff +1m | RW-trend | 1-year ahead | 84 | 7.1152 | 2.9191 |
| Cutoff +2m | AR(2) | 1-step ahead | 84 | 5.2488 | 1.8924 |
| Cutoff +2m | AR(2) | 1-year ahead | 84 | 4.7140 | 1.7552 |
| Cutoff +2m | MF-VAR | 1-step ahead | 84 | 7.3391 | 2.4448 |
| Cutoff +2m | MF-VAR | 1-year ahead | 84 | 4.6212 | 1.7257 |
| Cutoff +2m | MIDAS | 1-step ahead | 0 | NA | NA |
| Cutoff +2m | MIDAS | 1-year ahead | 0 | NA | NA |
| Cutoff +2m | MIDAS (trend) | 1-step ahead | 0 | NA | NA |
| Cutoff +2m | MIDAS (trend) | 1-year ahead | 0 | NA | NA |
| Cutoff +2m | RW-trend | 1-step ahead | 84 | 7.4792 | 2.3900 |
| Cutoff +2m | RW-trend | 1-year ahead | 84 | 7.1152 | 2.9191 |
