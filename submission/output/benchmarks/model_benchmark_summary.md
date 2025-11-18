# Benchmark Error Summary

## Holdout RMSE and MAE by Horizon (aggregated across variables)
| Model | Horizon | Observations | RMSE | MAE |
| --- | --- | --- | --- | --- |
| AR(2) | 1-step ahead | 3 | 0.2168 | 0.1709 |
| AR(2) | 1-year ahead | 3 | 1.6470 | 1.0700 |
| MF-VAR | 1-step ahead | 3 | 0.3775 | 0.2526 |
| MF-VAR | 1-year ahead | 3 | 0.9283 | 0.7577 |
| MIDAS | 1-step ahead | 3 | 1.1472 | 0.8953 |
| MIDAS | 1-year ahead | 3 | 2.9014 | 1.7715 |
| MIDAS (trend) | 1-step ahead | 3 | 1.3077 | 0.9566 |
| MIDAS (trend) | 1-year ahead | 3 | 2.6849 | 1.5651 |
| MIDAS-Latent | 1-step ahead | 3 | 1.6031 | 1.2035 |
| MIDAS-Latent | 1-year ahead | 3 | 0.7538 | 0.5252 |
| MIDAS-Latent (trend) | 1-step ahead | 3 | 1.6360 | 1.1479 |
| MIDAS-Latent (trend) | 1-year ahead | 3 | 0.8366 | 0.4928 |


## Holdout Overall Average Errors
| Model | Observations | RMSE | MAE |
| --- | --- | --- | --- |
| AR(2) | 12 | 0.9712 | 0.5480 |
| MF-VAR | 12 | 1.2983 | 0.8036 |
| MIDAS | 12 | 1.7169 | 0.9829 |
| MIDAS (trend) | 12 | 1.6147 | 0.9110 |
| MIDAS-Latent | 12 | 1.4621 | 0.9944 |
| MIDAS-Latent (trend) | 12 | 1.5073 | 0.9506 |


## Expanding Window Cross-Validation RMSE and MAE by Monthly Coverage
| Monthly data | Model | Horizon | Observations | RMSE | MAE |
| --- | --- | --- | --- | --- | --- |
| Cutoff only | AR(2) | 1-step ahead | 9 | 0.4716 | 0.3596 |
| Cutoff only | AR(2) | 1-year ahead | 9 | 1.1310 | 0.7212 |
| Cutoff only | MF-VAR | 1-step ahead | 9 | 0.9894 | 0.6488 |
| Cutoff only | MF-VAR | 1-year ahead | 9 | 1.3212 | 0.7822 |
| Cutoff only | MIDAS | 1-step ahead | 9 | 0.4932 | 0.3414 |
| Cutoff only | MIDAS | 1-year ahead | 9 | 1.0023 | 0.6272 |
| Cutoff only | MIDAS (trend) | 1-step ahead | 9 | 0.4159 | 0.2803 |
| Cutoff only | MIDAS (trend) | 1-year ahead | 9 | 0.7900 | 0.5382 |
| Cutoff only | MIDAS-Latent | 1-step ahead | 9 | 1.9783 | 1.3607 |
| Cutoff only | MIDAS-Latent | 1-year ahead | 9 | 2.3159 | 1.4919 |
| Cutoff only | MIDAS-Latent (trend) | 1-step ahead | 9 | 1.9014 | 1.3094 |
| Cutoff only | MIDAS-Latent (trend) | 1-year ahead | 9 | 2.2067 | 1.3750 |
| Cutoff +1m | AR(2) | 1-step ahead | 9 | 0.4716 | 0.3596 |
| Cutoff +1m | AR(2) | 1-year ahead | 9 | 1.1310 | 0.7212 |
| Cutoff +1m | MF-VAR | 1-step ahead | 9 | 0.9894 | 0.6488 |
| Cutoff +1m | MF-VAR | 1-year ahead | 9 | 1.4993 | 0.8580 |
| Cutoff +1m | MIDAS | 1-step ahead | 9 | 0.8951 | 0.6445 |
| Cutoff +1m | MIDAS | 1-year ahead | 9 | 1.0023 | 0.6272 |
| Cutoff +1m | MIDAS (trend) | 1-step ahead | 9 | 0.8909 | 0.6746 |
| Cutoff +1m | MIDAS (trend) | 1-year ahead | 9 | 0.7900 | 0.5382 |
| Cutoff +1m | MIDAS-Latent | 1-step ahead | 9 | 1.9612 | 1.3141 |
| Cutoff +1m | MIDAS-Latent | 1-year ahead | 9 | 2.4683 | 1.4029 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-step ahead | 9 | 1.8809 | 1.2580 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-year ahead | 9 | 2.3679 | 1.2710 |
| Cutoff +2m | AR(2) | 1-step ahead | 9 | 0.4716 | 0.3596 |
| Cutoff +2m | AR(2) | 1-year ahead | 9 | 1.1310 | 0.7212 |
| Cutoff +2m | MF-VAR | 1-step ahead | 9 | 0.9894 | 0.6488 |
| Cutoff +2m | MF-VAR | 1-year ahead | 9 | 1.0910 | 0.7013 |
| Cutoff +2m | MIDAS | 1-step ahead | 9 | 0.9295 | 0.6349 |
| Cutoff +2m | MIDAS | 1-year ahead | 9 | 1.0023 | 0.6272 |
| Cutoff +2m | MIDAS (trend) | 1-step ahead | 9 | 0.9020 | 0.6659 |
| Cutoff +2m | MIDAS (trend) | 1-year ahead | 9 | 0.7900 | 0.5382 |
| Cutoff +2m | MIDAS-Latent | 1-step ahead | 9 | 1.8432 | 1.1157 |
| Cutoff +2m | MIDAS-Latent | 1-year ahead | 9 | 2.4777 | 1.4359 |
| Cutoff +2m | MIDAS-Latent (trend) | 1-step ahead | 9 | 1.7574 | 1.0658 |
| Cutoff +2m | MIDAS-Latent (trend) | 1-year ahead | 9 | 2.3741 | 1.2677 |

