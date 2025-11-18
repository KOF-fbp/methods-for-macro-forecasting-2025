# Methods of Macroeconomic Forecasting - Autumn 2025
Course repository for the ETH autumn 2025 course on "Methods of Macroeconomic Forecasting"

## General Information

- Lectures: [Dr. Samad Sarferaz](https://sites.google.com/site/samadsarferaz/home)
- Labs: Merlin Scherer, Lena Will
- [Tech Setup Checklist](/resources/tech_setup_checklist.pdf)
- [Project Description](/resources/project_description.pdf)

## Time Schedule

| Date | Time | Schedule |
|------|------| ---|
|02 Oct 2025 |10am - 3pm |Methods Boot Camp|
|02 Oct 2025 |3pm - 4pm |Lab|
|03 Oct 2025 |10am - 4pm |Methods Boot Camp with integrated Lab|
|10 Oct 2025 | EOB | Hand in your project preferences |
|14 Oct 2025 | EOB | Topic allocation |
|19 Nov 2025 | 11pm | Hand in your presentation slides |
|20 Nov 2025 |10am - 2pm |Presentations|
|21 Nov 2025 |10am - 2pm |Presentations|
|12 Dec 2025 |10am - 12pm |Exam|

# Documentation

### Dynamic Factor Model (PCA + VAR) and FAVAR Forecasts for Swiss Macro Indicators

This repository implements a **dynamic factor model (DFM)** and a **factor-augmented VAR (FVAR)** to forecast key Swiss macro variables:

* Real GDP (growth),
* CPI (inflation), and
* CHF/EUR exchange rate (`wkfreuro`),

and compares them with KOF forecasts. It also constructs an **economic indicator** from a large panel of indicators and compares it to GDP growth and the KOF barometer.

The code is written in R and uses `renv` for reproducible package management.

---

### 0. Loading data and setting up paths

At the top of the main script we:

1. **Activate renv** (to get the right package versions):

```r
source("renv/activate.R")
```

2. **Load required packages**:

```r
library(ggplot2)
library(dplyr)
library(vars)
library(tidyr)
library(urca)
library(rlang)
library(KFAS)
library(lubridate)
library(readxl)
```

3. **Define local file paths** for the quarterly data, metadata, and KOF barometer.

Example:

```r
kfilepath_data     <- "C:/Users/kfree/OneDrive/Desktop/MASTER 3/MACRO FORECAST/DATASET/data_quarterly.csv"
kfilepath_metadata <- "C:/Users/kfree/OneDrive/Desktop/MASTER 3/MACRO FORECAST/DATASET/metadata_quarterly_en.csv"
nfilepath_data     <- "~/Documents/school/methods-for-macro-forecasting-2025/submission/data/data_quarterly.csv"
nfilepath_metadata <- "~/Documents/school/methods-for-macro-forecasting-2025/submission/data/metadata_quarterly_en.csv"

kkof_path <- "C:/Users/kfree/OneDrive/Desktop/MASTER 3/MACRO FORECAST/DATASET/KOF_BAROMETER.xlsx"
nkof_path <- "~/Documents/school/methods-for-macro-forecasting-2025/submission/data/KOF_BAROMETER.xlsx"
```

To make the script run, please update these paths according to your file setup. 


4. **Read the data**:

```r
df       <- utils::read.csv(filepath_data)
metadata <- utils::read.csv(filepath_metadata)
kof_raw  <- readxl::read_excel(kof_path, sheet = 1)
```

#### Expected data structure

* `data_quarterly.csv`:

  * One row per quarter.
  * A `date` column (e.g. `"2000-01"`, `"2000-04"`, etc.), interpreted in the script as the first day of the month:

    ```r
    mutate(date = as.Date(paste0(date, "-01")))
    ```
  * One column for each macro indicator. For any column `X`, there should be a corresponding entry in `metadata$ts_key == "X"` that describes its units and desired transformation.

* `metadata_quarterly_en.csv`:

  * At minimum:

    * `ts_key`: matches the column names in `df` for each variable,
    * `unit`: a text description (e.g. `"real in millions of francs"`, `"in percent"`, `"index"`).
  * The script lowercases the column names and units:

    ```r
    names(metadata) <- tolower(names(metadata))
    metadata$unit_low <- tolower(metadata$unit)
    ```

    and builds a `metadata$transform` vector (e.g. `"logdiff"`, `"diff"`, `"none"`) that drives the stationarity transformation.

* `KOF_BAROMETER.xlsx`:

  * Contains a monthly KOF barometer series.
  * The first two columns are renamed to:

    ```r
    names(kof_raw)[1:2] <- c("month_str", "kof_barometer")
    ```

    where `month_str` is in `"YYYY-MM"` format (e.g. `"2015-09"`).

---

### 1. Pre-processing and stationarity

**Goal:** Transform the panel of indicators to (approximately) stationary series and standardize them.

Steps:

1. **Train/OOS split by pre-COVID cutoff**:


2. **Map units to transformations**:

   * The script defines a named vector `unit_to_transform` that maps `metadata$unit_low` values to:

     * `"logdiff"` (e.g. real quantities, indexes),
     * `"diff"` (rates, percentages, levels in persons),
     * or leaves them as-is if no mapping is provided.

   * This is attached as `metadata$transform`.

3. **Apply transformations**:

   * `transform_variable(x, transform_type)` applies `logdiff` (×100) or `diff` to a numeric series, returning a transformed series with a leading `NA`.
   * A loop applies this to each series with a defined `metadata$ts_key` and `metadata$transform`.

4. **Clean panel**:

   * Drop the first row (lost due to differencing),
   * Drop columns that are all `NA` or have (near) zero variance (`sd < 1e-8`),
   * Standardize all variables (except `date`) using `scale()` and store the means and sds in `scaler_params` for later back-transformation.

5. **Stationarity check (ADF)**:

   * For each transformed series, the script runs `ur.df(..., type = "drift", selectlags = "AIC")` and collects approximate p-values, mainly as a diagnostic.

---

### 2. OOS design and predictor-selection

**Goal:** Define the out-of-sample forecasting scheme and shrink the predictor set.

1. **OOS split (80/20 with room for h = 4)**

   * Let `T_total` be the number of (post-NA) observations.
   * `t0_idx` is set to roughly 80% of the sample, ensuring enough room for 4-step-ahead forecasts (`h_long = 4`).
   * Origins run from `t0_idx` to `T_total - h_long`.

2. **Handle missing data in X and drop sparse variables**

   * Compute the fraction of missing values for each candidate predictor in the initial training window.
   * Drop series with more than 30% missing data.

3. **Collinearity pruning**

   * On the remaining X variables, compute pairwise correlations and build a hierarchical clustering on `1 - |corr|`.
   * Cut at height `h = 0.1` (i.e. group series with |ρ| > 0.9) and keep, from each cluster, the series with the fewest missing values.
   * The resulting set of columns is `vars_X`.

4. **Targets**

   * Targets are:

     ```r
     vars_Y <- c("rvgdp", "cpi", "wkfreuro")
     ```
   * `growth_vars <- c("rvgdp")` marks GDP as a growth series to be kept in Δlog×100 rather than back-transformed to levels.

---

### 3. Bai–Ng factor selection and PCA

**Goal:** Choose the number of factors `r` and inspect the variance explained.

1. **Bai–Ng ICp2 selection on the initial training sample**

   * Use `X0` = standardized regressors in the initial training window.
   * Perform PCA (via `svd()` on mean-centered X) and compute the Bai–Ng ICp2 criterion for a grid of possible `r`.
   * Select `r_fixed = argmin(ICp2)`.

2. **PCA and cumulative variance explained**

   * Run `prcomp(X0_imp, center = TRUE, scale. = TRUE)` to get principal components.
   * Compute the cumulative variance explained and plot it.
   * Add a vertical line at `r_selected` (Bai–Ng’s choice) to visualize how much variance the chosen `r` captures.

---

### 4. Dynamic Factor Model (PCA + VAR) – OOS forecasts

**Goal:** Use a standard two-step DFM:

1. Extract factors from X via PCA.
2. Model factor dynamics with a VAR.
3. Map factors back to Y via a regression.

For each OOS origin:

1. **Standardize X and Y in the window**:

   * Compute window-specific means and sds for X and Y,
   * Standardize,
   * Impute any remaining NAs in standardized X with 0 (the mean on the standardized scale).

2. **PCA on X_std and factor extraction**

   * Fit `prcomp(X_std, center = FALSE, scale. = FALSE)`,
   * Keep the first `r_fixed` factors: `F_train`.

3. **OLS mapping from factors to Y**

   * Build regressors `[F_t, F_{t-1}]` (with `L_ols = 1`) and run OLS for each Y:

     ```r
     Y_std ~ const + F_t + F_{t-1}
     ```

4. **VAR on factors**

   * Fit `VAR(F_train, p = p_fix, type = "const")` with `p_fix = 2`.
   * Use `predict()` to get factor forecasts at horizons `h = 1` and `h = 4`.

5. **Predict Y in standardized units and back to transformed units**

   * Plug factor forecasts into the OLS mapping to get predictions for Y in standardized units,
   * De-standardize using `Y_mean` and `Y_sd`,
   * Save predictions and realized values for both horizons.

6. **Collect results**

   * Matrices:

     * `pred_h1_real`, `pred_h4_real`: predictions in transformed units,
     * `real_h1_real`, `real_h4_real`: realized transformed values.
   * Additional matrices in standardized units for metrics.

---

### 5. OOS evaluation and back-transformation to levels

**Goal:** Evaluate forecast performance and produce readable plots.

1. **Metrics in transformed units**

   * RMSE and MAE at h = 1 and h = 4 for each Y:

     ```r
     metrics_transformed <- data.frame(
       variable = vars_Y,
       RMSE_h1  = ...,
       RMSE_h4  = ...,
       MAE_h1   = ...,
       MAE_h4   = ...
     )
     ```

2. **Back-transform to levels**

   * For each target variable and origin:

     * If `v %in% growth_vars`: keep the transformed values (Δlog×100).
     * If `transform == "logdiff"`: reconstruct levels via `L_{t+1} = L_t * exp(y/100)`.
     * If `transform == "diff"`: `L_{t+1} = L_t + y`.
   * Construct `pred_h1_level`, `pred_h4_level`, `real_h1_level`, `real_h4_level` and compute level-based RMSE/MAE (`metrics_levels`).

3. **OOS result table and plots**

   * Build an `oos_results` data frame that combines:

     * origin dates,
     * forecast dates,
     * predicted and realized values (both transformed and levels).
   * Plot OOS trajectories for:

     * Transformed scale (model units),
     * Back-transformed levels,
       using `ggplot2` with facets by variable and horizon.

---

### 6. NOW-casting with DFM (PCA + VAR) and comparison with KOF

**Goal:** Use all available data up to a chosen cutoff (e.g. 2025-07-01) to generate NOW forecasts and compare them with KOF predictions.

1. **Build NOW sample**

   * Reconstruct `df_train_clean` using all data up to a `now` date:

     ```r
     df_train_clean <- df %>%
       mutate(date = as.Date(paste0(date, "-01"))) %>%
       mutate(across(-date, as.numeric)) %>%
       filter(date <= as.Date("2025-07-01"))
     ```
   * Reapply the same transformation logic (`transform_variable`) and standardize.

2. **Reuse predictor set and factor dimension**

   * Reuse `vars_X` (same predictors as in the OOS exercise),
   * Reuse `r_fixed` for the number of factors.

3. **PCA and VAR on full history**

   * Run PCA on the full standardized X,
   * Run VAR on the full factor history,
   * Forecast factors h = 1..4,
   * Map to Y via OLS as before.

4. **Back-transform to transformed units and levels**

   * Obtain NOW forecasts for:

     * Transformed units: `now_table_transformed`,
     * Levels (or growth for GDP): `now_table_levels`.

5. **Compare with KOF**

   * Construct a historical series for the targets (levels),
   * Convert `rvgdp` to QoQ growth (Δlog×100),
   * Align KOF forecasts with the same target dates,
   * Plot recent history plus points for:

     * DFM NOW forecast,
     * KOF forecast.

---

### 7. FVAR: Factor-Augmented VAR (OOS and NOW)

**Goal:** Estimate a VAR on `[Y_t, F_t]` jointly and compare performance with the DFM.

1. **OOS FVAR forecasts**

   * For each OOS origin:

     * Standardize X and Y,
     * Extract factors from X via PCA,
     * Build `Z_t = [Y_std, F_t]`,
     * Fit `VAR(Z_t, p = p_fix, type = "const")`,
     * Forecast Y at h = 1 and h = 4 in standardized units,
     * De-standardize to transformed units.
   * Compute RMSE/MAE in transformed units (`metrics_transformed_fvar`).
   * Back-transform to levels using the same logic as DFM and compute metrics (`metrics_levels_fvar`).

2. **NOW FVAR**

   * Using the full NOW sample (same as NOW DFM), standardize X and Y, extract factors, and build `Z_t = [Y_std, F]`.
   * Fit a VAR on `[Y, F]`, forecast Y at h = 1..4, de-standardize, and back-transform to levels (or growth for GDP).
   * Store NOW FVAR forecasts in:

     * `now_fvar_transformed`,
     * `now_fvar_levels`.

3. **DFM vs FVAR vs KOF comparison**

   * Build a plot with recent historical levels and points for:

     * DFM NOW forecasts,
     * FVAR NOW forecasts,
     * KOF forecasts,
       with separate colors/shapes.

---

### 8. Economic indicator construction and comparison

**Goal:** Build a single “economic indicator” from the panel of X variables and compare it to GDP growth and the KOF barometer.

1. **Economic indicator (factor extraction)**

   * Use the entire transformed and standardized X panel up to the NOW date.
   * Run PCA and extract the first factor `F1` (the “economic indicator”).

2. **Standardization and KOF-style scaling**

   * Construct:

     * `EI_z` as a z-score (mean 0, sd 1),
     * `EI_kof` as a KOF-style index: mean 100, sd 10.
   * Store in `economic_indicator`.

3. **Compare to GDP growth**

   * Compute real GDP QoQ growth (Δlog×100),
   * Standardize GDP growth to a z-score,
   * Plot `EI_z` and standardized GDP growth together.

4. **Merge with KOF barometer**

   * Aggregate the monthly KOF barometer to a quarterly series by taking the last month within each quarter.
   * Merge:

     * `EI_z`,
     * GDP QoQ growth,
     * KOF barometer (quarterly).
   * Re-scale all three series to mean 100, sd 100 (`EI_100`, `GDP_100`, `KOF_100`).
   * Plot the three standardized series to compare co-movement over time.

