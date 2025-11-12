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
| Cutoff only | AR(2) | 1-step ahead | 108 | 4.6488 | 1.6134 |
| Cutoff only | AR(2) | 1-year ahead | 108 | 4.2236 | 1.5877 |
| Cutoff only | MF-VAR | 1-step ahead | 108 | 6.4912 | 2.0550 |
| Cutoff only | MF-VAR | 1-year ahead | 108 | 4.1694 | 1.5534 |
| Cutoff only | MIDAS | 1-step ahead | 108 | 4.0691 | 1.5669 |
| Cutoff only | MIDAS | 1-year ahead | 108 | 4.5524 | 1.8364 |
| Cutoff only | MIDAS (trend) | 1-step ahead | 108 | 4.1331 | 1.5812 |
| Cutoff only | MIDAS (trend) | 1-year ahead | 108 | 4.5438 | 1.8303 |
| Cutoff only | RW-trend | 1-step ahead | 108 | 6.6229 | 2.0308 |
| Cutoff only | RW-trend | 1-year ahead | 108 | 6.3685 | 2.6089 |
| Cutoff +1m | AR(2) | 1-step ahead | 108 | 4.6488 | 1.6134 |
| Cutoff +1m | AR(2) | 1-year ahead | 108 | 4.2236 | 1.5877 |
| Cutoff +1m | MF-VAR | 1-step ahead | 108 | 6.4912 | 2.0550 |
| Cutoff +1m | MF-VAR | 1-year ahead | 108 | 4.1737 | 1.5705 |
| Cutoff +1m | MIDAS | 1-step ahead | 108 | 4.2832 | 1.3539 |
| Cutoff +1m | MIDAS | 1-year ahead | 108 | 4.5382 | 1.8167 |
| Cutoff +1m | MIDAS (trend) | 1-step ahead | 108 | 4.3500 | 1.4167 |
| Cutoff +1m | MIDAS (trend) | 1-year ahead | 108 | 4.5354 | 1.8146 |
| Cutoff +1m | RW-trend | 1-step ahead | 108 | 6.6229 | 2.0308 |
| Cutoff +1m | RW-trend | 1-year ahead | 108 | 6.3685 | 2.6089 |
| Cutoff +2m | AR(2) | 1-step ahead | 108 | 4.6488 | 1.6134 |
| Cutoff +2m | AR(2) | 1-year ahead | 108 | 4.2236 | 1.5877 |
| Cutoff +2m | MF-VAR | 1-step ahead | 108 | 6.4912 | 2.0550 |
| Cutoff +2m | MF-VAR | 1-year ahead | 108 | 4.1551 | 1.5537 |
| Cutoff +2m | MIDAS | 1-step ahead | 108 | 4.8402 | 1.4530 |
| Cutoff +2m | MIDAS | 1-year ahead | 108 | 4.5102 | 1.7960 |
| Cutoff +2m | MIDAS (trend) | 1-step ahead | 108 | 4.8648 | 1.5102 |
| Cutoff +2m | MIDAS (trend) | 1-year ahead | 108 | 4.5130 | 1.7994 |
| Cutoff +2m | RW-trend | 1-step ahead | 108 | 6.6229 | 2.0308 |
| Cutoff +2m | RW-trend | 1-year ahead | 108 | 6.3685 | 2.6089 |
| Cutoff +3m | AR(2) | 1-step ahead | 108 | 4.6488 | 1.6134 |
| Cutoff +3m | AR(2) | 1-year ahead | 108 | 4.2236 | 1.5877 |
| Cutoff +3m | MF-VAR | 1-step ahead | 108 | 3.6292 | 1.3333 |
| Cutoff +3m | MF-VAR | 1-year ahead | 108 | 4.1627 | 1.5556 |
| Cutoff +3m | MIDAS | 1-step ahead | 108 | 4.4780 | 1.4572 |
| Cutoff +3m | MIDAS | 1-year ahead | 108 | 4.5102 | 1.8050 |
| Cutoff +3m | MIDAS (trend) | 1-step ahead | 108 | 4.5118 | 1.5085 |
| Cutoff +3m | MIDAS (trend) | 1-year ahead | 108 | 4.5071 | 1.8069 |
| Cutoff +3m | RW-trend | 1-step ahead | 108 | 6.6229 | 2.0308 |
| Cutoff +3m | RW-trend | 1-year ahead | 108 | 6.3685 | 2.6089 |
