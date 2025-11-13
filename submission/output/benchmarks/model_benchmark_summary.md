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
| Cutoff only | AR(2) | 1-step ahead | 84 | 5.2488 | 1.8924 |
| Cutoff only | AR(2) | 1-year ahead | 84 | 4.7140 | 1.7552 |
| Cutoff only | MF-VAR | 1-step ahead | 84 | 7.3391 | 2.4448 |
| Cutoff only | MF-VAR | 1-year ahead | 84 | 4.6515 | 1.7154 |
| Cutoff only | MIDAS | 1-step ahead | 84 | 4.5601 | 1.7607 |
| Cutoff only | MIDAS | 1-year ahead | 84 | 5.0698 | 2.0541 |
| Cutoff only | MIDAS (trend) | 1-step ahead | 84 | 4.6326 | 1.7836 |
| Cutoff only | MIDAS (trend) | 1-year ahead | 84 | 5.0491 | 2.0061 |
| Cutoff +1m | AR(2) | 1-step ahead | 84 | 5.2488 | 1.8924 |
| Cutoff +1m | AR(2) | 1-year ahead | 84 | 4.7140 | 1.7552 |
| Cutoff +1m | MF-VAR | 1-step ahead | 84 | 7.3391 | 2.4448 |
| Cutoff +1m | MF-VAR | 1-year ahead | 84 | 4.6579 | 1.7424 |
| Cutoff +1m | MIDAS | 1-step ahead | 84 | 4.8178 | 1.5346 |
| Cutoff +1m | MIDAS | 1-year ahead | 84 | 5.0555 | 2.0333 |
| Cutoff +1m | MIDAS (trend) | 1-step ahead | 84 | 4.8928 | 1.6196 |
| Cutoff +1m | MIDAS (trend) | 1-year ahead | 84 | 5.0413 | 1.9885 |
| Cutoff +2m | AR(2) | 1-step ahead | 84 | 5.2488 | 1.8924 |
| Cutoff +2m | AR(2) | 1-year ahead | 84 | 4.7140 | 1.7552 |
| Cutoff +2m | MF-VAR | 1-step ahead | 84 | 7.3391 | 2.4448 |
| Cutoff +2m | MF-VAR | 1-year ahead | 84 | 4.6314 | 1.7166 |
| Cutoff +2m | MIDAS | 1-step ahead | 84 | 5.4554 | 1.6894 |
| Cutoff +2m | MIDAS | 1-year ahead | 84 | 5.0240 | 2.0083 |
| Cutoff +2m | MIDAS (trend) | 1-step ahead | 84 | 5.4817 | 1.7650 |
| Cutoff +2m | MIDAS (trend) | 1-year ahead | 84 | 5.0158 | 1.9696 |

