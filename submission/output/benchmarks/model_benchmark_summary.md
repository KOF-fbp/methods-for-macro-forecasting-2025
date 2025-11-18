# Benchmark Error Summary

## Holdout RMSE and MAE by Horizon (aggregated across variables)
| Model | Horizon | Observations | RMSE | MAE |
| --- | --- | --- | --- | --- |
| AR(2) | 1-step ahead | 3 | 0.2168 | 0.1709 |
| AR(2) | 1-year ahead | 3 | 1.6470 | 1.0700 |
| MF-VAR | 1-step ahead | 3 | 0.3775 | 0.2526 |
| MF-VAR | 1-year ahead | 3 | 1.2498 | 0.9912 |
| MIDAS | 1-step ahead | 3 | 1.1472 | 0.8953 |
| MIDAS | 1-year ahead | 3 | 2.9014 | 1.7715 |
| MIDAS (trend) | 1-step ahead | 3 | 1.3077 | 0.9566 |
| MIDAS (trend) | 1-year ahead | 3 | 2.6849 | 1.5651 |
| MIDAS-Latent | 1-step ahead | 3 | 0.7772 | 0.6441 |
| MIDAS-Latent | 1-year ahead | 3 | 0.4079 | 0.3207 |
| MIDAS-Latent (trend) | 1-step ahead | 3 | 0.7020 | 0.5707 |
| MIDAS-Latent (trend) | 1-year ahead | 3 | 0.2838 | 0.1692 |


## Holdout Overall Average Errors
| Model | Observations | RMSE | MAE |
| --- | --- | --- | --- |
| AR(2) | 12 | 0.9712 | 0.5480 |
| MF-VAR | 12 | 1.1522 | 0.7616 |
| MIDAS | 12 | 1.7169 | 0.9829 |
| MIDAS (trend) | 12 | 1.6147 | 0.9110 |
| MIDAS-Latent | 12 | 0.7836 | 0.5745 |
| MIDAS-Latent (trend) | 12 | 0.7221 | 0.4660 |


## Expanding Window Cross-Validation RMSE and MAE by Monthly Coverage
| Monthly data | Model | Horizon | Observations | RMSE | MAE |
| --- | --- | --- | --- | --- | --- |
| Cutoff only | AR(2) | 1-step ahead | 6 | 0.4464 | 0.3277 |
| Cutoff only | AR(2) | 1-year ahead | 6 | 1.3203 | 0.8536 |
| Cutoff only | MF-VAR | 1-step ahead | 6 | 0.8118 | 0.5710 |
| Cutoff only | MF-VAR | 1-year ahead | 6 | 1.0041 | 0.7400 |
| Cutoff only | MIDAS | 1-step ahead | 6 | 0.5833 | 0.4202 |
| Cutoff only | MIDAS | 1-year ahead | 6 | 1.1837 | 0.7976 |
| Cutoff only | MIDAS (trend) | 1-step ahead | 6 | 0.4582 | 0.3045 |
| Cutoff only | MIDAS (trend) | 1-year ahead | 6 | 0.9277 | 0.6484 |
| Cutoff only | MIDAS-Latent | 1-step ahead | 6 | 1.5693 | 1.1417 |
| Cutoff only | MIDAS-Latent | 1-year ahead | 6 | 2.1786 | 1.2398 |
| Cutoff only | MIDAS-Latent (trend) | 1-step ahead | 6 | 1.4646 | 1.0384 |
| Cutoff only | MIDAS-Latent (trend) | 1-year ahead | 6 | 2.0758 | 1.0805 |
| Cutoff +1m | AR(2) | 1-step ahead | 6 | 0.4464 | 0.3277 |
| Cutoff +1m | AR(2) | 1-year ahead | 6 | 1.3203 | 0.8536 |
| Cutoff +1m | MF-VAR | 1-step ahead | 6 | 1.0585 | 0.7053 |
| Cutoff +1m | MF-VAR | 1-year ahead | 6 | 0.8503 | 0.6179 |
| Cutoff +1m | MIDAS | 1-step ahead | 6 | 0.7159 | 0.5891 |
| Cutoff +1m | MIDAS | 1-year ahead | 6 | 1.1837 | 0.7976 |
| Cutoff +1m | MIDAS (trend) | 1-step ahead | 6 | 0.8288 | 0.6579 |
| Cutoff +1m | MIDAS (trend) | 1-year ahead | 6 | 0.9277 | 0.6484 |
| Cutoff +1m | MIDAS-Latent | 1-step ahead | 6 | 1.7957 | 1.2036 |
| Cutoff +1m | MIDAS-Latent | 1-year ahead | 6 | 2.5081 | 1.4736 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-step ahead | 6 | 1.6838 | 1.1013 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-year ahead | 6 | 2.3998 | 1.3155 |
| Cutoff +2m | AR(2) | 1-step ahead | 6 | 0.4464 | 0.3277 |
| Cutoff +2m | AR(2) | 1-year ahead | 6 | 1.3203 | 0.8536 |
| Cutoff +2m | MF-VAR | 1-step ahead | 6 | 1.4305 | 0.9448 |
| Cutoff +2m | MF-VAR | 1-year ahead | 6 | 0.9018 | 0.6486 |
| Cutoff +2m | MIDAS | 1-step ahead | 6 | 0.6690 | 0.5344 |
| Cutoff +2m | MIDAS | 1-year ahead | 6 | 1.1837 | 0.7976 |
| Cutoff +2m | MIDAS (trend) | 1-step ahead | 6 | 0.7674 | 0.6052 |
| Cutoff +2m | MIDAS (trend) | 1-year ahead | 6 | 0.9277 | 0.6484 |
| Cutoff +2m | MIDAS-Latent | 1-step ahead | 6 | 1.7686 | 1.1425 |
| Cutoff +2m | MIDAS-Latent | 1-year ahead | 6 | 2.5662 | 1.6208 |
| Cutoff +2m | MIDAS-Latent (trend) | 1-step ahead | 6 | 1.6497 | 0.9996 |
| Cutoff +2m | MIDAS-Latent (trend) | 1-year ahead | 6 | 2.4568 | 1.4639 |

