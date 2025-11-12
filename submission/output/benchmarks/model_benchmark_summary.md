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
| Cutoff only | AR(2) | 1-step ahead | 12 | 0.8686 | 0.5055 |
| Cutoff only | AR(2) | 1-year ahead | 12 | 0.9912 | 0.5947 |
| Cutoff only | MF-VAR | 1-step ahead | 12 | 1.2883 | 0.7922 |
| Cutoff only | MF-VAR | 1-year ahead | 12 | 1.0030 | 0.5943 |
| Cutoff only | MIDAS | 1-step ahead | 12 | 0.4757 | 0.3102 |
| Cutoff only | MIDAS | 1-year ahead | 12 | 1.0747 | 0.7444 |
| Cutoff only | MIDAS (trend) | 1-step ahead | 12 | 0.5386 | 0.3002 |
| Cutoff only | MIDAS (trend) | 1-year ahead | 12 | 0.8576 | 0.6258 |
| Cutoff +1m | AR(2) | 1-step ahead | 12 | 0.8686 | 0.5055 |
| Cutoff +1m | AR(2) | 1-year ahead | 12 | 0.9912 | 0.5947 |
| Cutoff +1m | MF-VAR | 1-step ahead | 12 | 1.2883 | 0.7922 |
| Cutoff +1m | MF-VAR | 1-year ahead | 12 | 1.0555 | 0.6458 |
| Cutoff +1m | MIDAS | 1-step ahead | 12 | 1.6518 | 0.8509 |
| Cutoff +1m | MIDAS | 1-year ahead | 12 | 1.0651 | 0.7380 |
| Cutoff +1m | MIDAS (trend) | 1-step ahead | 12 | 1.7801 | 0.9346 |
| Cutoff +1m | MIDAS (trend) | 1-year ahead | 12 | 0.8501 | 0.6198 |

