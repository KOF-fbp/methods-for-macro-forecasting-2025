# Benchmark Error Summary

## Holdout RMSE and MAE by Horizon (aggregated across variables)
| Model | Horizon | Observations | RMSE | MAE |
| --- | --- | --- | --- | --- |
| AR(2) | 1-step ahead | 3 | 0.2168 | 0.1709 |
| AR(2) | 1-year ahead | 3 | 1.6470 | 1.0700 |
| MF-VAR | 1-step ahead | 3 | 0.3775 | 0.2526 |
| MF-VAR | 1-year ahead | 3 | 1.7000 | 1.1287 |
| MIDAS | 1-step ahead | 3 | 1.0114 | 0.5926 |
| MIDAS | 1-year ahead | 3 | 2.8735 | 1.7123 |
| MIDAS (trend) | 1-step ahead | 3 | 1.2146 | 0.7355 |
| MIDAS (trend) | 1-year ahead | 3 | 2.6937 | 1.6040 |


## Holdout Overall Average Errors
| Model | Observations | RMSE | MAE |
| --- | --- | --- | --- |
| AR(2) | 12 | 0.9712 | 0.5480 |
| MF-VAR | 12 | 1.0264 | 0.5654 |
| MIDAS | 12 | 1.6789 | 0.8619 |
| MIDAS (trend) | 12 | 1.6022 | 0.8409 |


## Expanding Window Cross-Validation RMSE and MAE by Monthly Coverage
| Monthly data | Model | Horizon | Observations | RMSE | MAE |
| --- | --- | --- | --- | --- | --- |
| Cutoff only | AR(2) | 1-step ahead | 6 | 0.4464 | 0.3277 |
| Cutoff only | AR(2) | 1-year ahead | 6 | 1.3203 | 0.8536 |
| Cutoff only | MF-VAR | 1-step ahead | 6 | 0.8118 | 0.5710 |
| Cutoff only | MF-VAR | 1-year ahead | 6 | 1.3202 | 0.8396 |
| Cutoff only | MIDAS | 1-step ahead | 6 | 0.4702 | 0.3047 |
| Cutoff only | MIDAS | 1-year ahead | 6 | 1.1829 | 0.8119 |
| Cutoff only | MIDAS (trend) | 1-step ahead | 6 | 0.3947 | 0.2161 |
| Cutoff only | MIDAS (trend) | 1-year ahead | 6 | 0.9725 | 0.6954 |
| Cutoff +1m | AR(2) | 1-step ahead | 6 | 0.4464 | 0.3277 |
| Cutoff +1m | AR(2) | 1-year ahead | 6 | 1.3203 | 0.8536 |
| Cutoff +1m | MF-VAR | 1-step ahead | 6 | 0.8118 | 0.5710 |
| Cutoff +1m | MF-VAR | 1-year ahead | 6 | 1.3753 | 0.8912 |
| Cutoff +1m | MIDAS | 1-step ahead | 6 | 0.6220 | 0.4548 |
| Cutoff +1m | MIDAS | 1-year ahead | 6 | 1.1813 | 0.8132 |
| Cutoff +1m | MIDAS (trend) | 1-step ahead | 6 | 0.7771 | 0.5517 |
| Cutoff +1m | MIDAS (trend) | 1-year ahead | 6 | 0.9737 | 0.6966 |
| Cutoff +2m | AR(2) | 1-step ahead | 6 | 0.4464 | 0.3277 |
| Cutoff +2m | AR(2) | 1-year ahead | 6 | 1.3203 | 0.8536 |
| Cutoff +2m | MF-VAR | 1-step ahead | 6 | 0.8118 | 0.5710 |
| Cutoff +2m | MF-VAR | 1-year ahead | 6 | 1.3323 | 0.8961 |
| Cutoff +2m | MIDAS | 1-step ahead | 6 | 0.6050 | 0.4294 |
| Cutoff +2m | MIDAS | 1-year ahead | 6 | 1.1838 | 0.8196 |
| Cutoff +2m | MIDAS (trend) | 1-step ahead | 6 | 0.7418 | 0.5322 |
| Cutoff +2m | MIDAS (trend) | 1-year ahead | 6 | 0.9754 | 0.7021 |

