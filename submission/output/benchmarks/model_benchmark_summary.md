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
| Cutoff only | AR(2) | 1-step ahead | 84 | 5.2488 | 1.8924 |
| Cutoff only | AR(2) | 1-year ahead | 84 | 4.7140 | 1.7552 |
| Cutoff only | MF-VAR | 1-step ahead | 84 | 7.3391 | 2.4448 |
| Cutoff only | MF-VAR | 1-year ahead | 84 | 5.5777 | 2.1416 |
| Cutoff only | MIDAS | 1-step ahead | 84 | 4.0269 | 1.7306 |
| Cutoff only | MIDAS | 1-year ahead | 84 | 4.8827 | 1.8197 |
| Cutoff only | MIDAS (trend) | 1-step ahead | 84 | 4.1133 | 1.7620 |
| Cutoff only | MIDAS (trend) | 1-year ahead | 84 | 4.9172 | 1.8640 |
| Cutoff only | MIDAS-Latent | 1-step ahead | 84 | 7.2988 | 2.8535 |
| Cutoff only | MIDAS-Latent | 1-year ahead | 84 | 7.3177 | 3.1245 |
| Cutoff only | MIDAS-Latent (trend) | 1-step ahead | 84 | 7.3013 | 2.8483 |
| Cutoff only | MIDAS-Latent (trend) | 1-year ahead | 84 | 7.3250 | 3.0969 |
| Cutoff +1m | AR(2) | 1-step ahead | 84 | 5.2488 | 1.8924 |
| Cutoff +1m | AR(2) | 1-year ahead | 84 | 4.7140 | 1.7552 |
| Cutoff +1m | MF-VAR | 1-step ahead | 84 | 7.3391 | 2.4448 |
| Cutoff +1m | MF-VAR | 1-year ahead | 84 | 5.6504 | 2.2001 |
| Cutoff +1m | MIDAS | 1-step ahead | 84 | 4.1542 | 1.5061 |
| Cutoff +1m | MIDAS | 1-year ahead | 84 | 4.8827 | 1.8197 |
| Cutoff +1m | MIDAS (trend) | 1-step ahead | 84 | 4.2593 | 1.6076 |
| Cutoff +1m | MIDAS (trend) | 1-year ahead | 84 | 4.9172 | 1.8640 |
| Cutoff +1m | MIDAS-Latent | 1-step ahead | 84 | 7.3875 | 2.9828 |
| Cutoff +1m | MIDAS-Latent | 1-year ahead | 84 | 7.2528 | 3.1307 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-step ahead | 84 | 7.4055 | 2.9918 |
| Cutoff +1m | MIDAS-Latent (trend) | 1-year ahead | 84 | 7.2693 | 3.1291 |
| Cutoff +2m | AR(2) | 1-step ahead | 84 | 5.2488 | 1.8924 |
| Cutoff +2m | AR(2) | 1-year ahead | 84 | 4.7140 | 1.7552 |
| Cutoff +2m | MF-VAR | 1-step ahead | 84 | 7.3391 | 2.4448 |
| Cutoff +2m | MF-VAR | 1-year ahead | 84 | 5.5994 | 2.2445 |
| Cutoff +2m | MIDAS | 1-step ahead | 84 | 4.7847 | 1.6287 |
| Cutoff +2m | MIDAS | 1-year ahead | 84 | 4.8827 | 1.8197 |
| Cutoff +2m | MIDAS (trend) | 1-step ahead | 84 | 4.8236 | 1.7035 |
| Cutoff +2m | MIDAS (trend) | 1-year ahead | 84 | 4.9172 | 1.8640 |
| Cutoff +2m | MIDAS-Latent | 1-step ahead | 84 | 7.3570 | 3.0508 |
| Cutoff +2m | MIDAS-Latent | 1-year ahead | 84 | 7.2546 | 3.1965 |
| Cutoff +2m | MIDAS-Latent (trend) | 1-step ahead | 84 | 7.3648 | 3.0469 |
| Cutoff +2m | MIDAS-Latent (trend) | 1-year ahead | 84 | 7.2530 | 3.1804 |

