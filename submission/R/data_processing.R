# Data ingestion and transformation utilities

utils::globalVariables(c(
  ".data", "qtr", "rvgdp", "cpi", "wkfreuro", "gdp_growth", "inflation",
  "exch_rate", "time_index"
))

qtr <- rvgdp <- cpi <- wkfreuro <- gdp_growth <- inflation <- exch_rate <- time_index <- NULL

read_quarterly_data <- function(data_dir) {
  q_path <- file.path(data_dir, "processed", "data_quarterly.csv")
  stopifnot(file.exists(q_path))
  qraw <- readr::read_csv(q_path, show_col_types = FALSE)

  vars_q <- c("rvgdp", "cpi", "wkfreuro")
  missing <- setdiff(vars_q, names(qraw))
  if (length(missing)) {
    stop("Missing expected columns in quarterly CSV: ", paste(missing, collapse = ", "))
  }

  # Build quarterly growth/log levels, drop the first NA induced by lagging, and
  # retain only the series needed by the MF-VAR.
  qraw |>
    dplyr::mutate(qtr = zoo::as.yearqtr(.data$date, format = "%Y-%m")) |>
    dplyr::arrange(.data$qtr) |>
      dplyr::mutate(
        gdp_growth = 400 * (log(.data$rvgdp) - dplyr::lag(log(.data$rvgdp))),
        inflation  = 400 * (log(.data$cpi) - dplyr::lag(log(.data$cpi))),
        exch_rate  = log(.data$wkfreuro)
      ) |>
      tidyr::drop_na() |>
      dplyr::select(dplyr::all_of(c("qtr", "gdp_growth", "inflation", "exch_rate")))
}

fetch_kof_barometer <- function() {
  baro_ts <- NULL
  for (key in c("kofbarometer", "ch.kof.barometer")) {
    baro_try <- try(kofdata::get_time_series(key), silent = TRUE)
    if (!inherits(baro_try, "try-error") && length(baro_try) > 0) {
      baro_ts <- baro_try[[1]]
      break
    }
  }
  if (is.null(baro_ts)) {
    stop("Could not download KOF Barometer via 'kofdata' (tried keys 'kofbarometer' and 'ch.kof.barometer').")
  }

  # Center the barometer and return it as a monthly ts object for aggregation.
  stats::ts(
    as.numeric(baro_ts) - mean(as.numeric(baro_ts), na.rm = TRUE),
    start = stats::start(baro_ts),
    frequency = 12
  )
}

trim_to_overlap <- function(qdat, baro_ts) {
  baro_end <- stats::end(baro_ts)
  last_q_num <- floor(baro_end[2] / 3)
  if (last_q_num == 0) {
    last_q_year <- baro_end[1] - 1
    last_q_num <- 4
  } else {
    last_q_year <- baro_end[1]
  }
  q_cutoff <- zoo::as.yearqtr(sprintf("%d Q%d", last_q_year, last_q_num))
  qdat <- qdat |>
    dplyr::filter(.data$qtr <= q_cutoff)
  if (!nrow(qdat)) {
    stop("No overlapping quarters between the quarterly dataset and the KOF Barometer.")
  }
  list(qdat = qdat, baro_ts = baro_ts)
}

quarter_to_month_end <- function(yq) {
  # Convert a year-quarter stamp to the corresponding month-end indices.
  end_month <- zoo::as.yearmon(yq) + (2 / 12)
  end_date <- as.Date(end_month)
  c(lubridate::year(end_date), lubridate::month(end_date))
}

build_q_ts <- function(q_subset) {
  q_z <- lapply(names(q_subset)[-1], function(v) zoo::zoo(q_subset[[v]], q_subset$qtr))
  names(q_z) <- names(q_subset)[-1]
  lapply(q_z, stats::as.ts)
}

build_Y <- function(q_subset, baro_subset) {
  # Arrange inputs into the mfbvar::set_prior structure.
  q_ts_local <- build_q_ts(q_subset)
  list(
    kofbarometer = baro_subset,
    quarterly = cbind(
      gdp_growth = q_ts_local[["gdp_growth"]],
      inflation  = q_ts_local[["inflation"]],
      exch_rate  = q_ts_local[["exch_rate"]]
    )
  )
}

window_baro <- function(baro_ts, qdat) {
  q_start <- qdat$qtr[1]
  q_start_date <- zoo::as.Date(q_start, frac = 1)
  q_start_year <- lubridate::year(q_start_date)
  q_start_q <- lubridate::quarter(q_start_date)
  m_start <- c(q_start_year, (q_start_q - 1) * 3 + 1)
  m_start_back2 <- c(
    m_start[1] - as.integer(m_start[2] <= 2),
    ((m_start[2] + 10) %% 12) + 1
  )
  q_end <- qdat$qtr[nrow(qdat)]
  q_end_date <- zoo::as.Date(q_end, frac = 1)
  q_end_year <- lubridate::year(q_end_date)
  q_end_q <- lubridate::quarter(q_end_date)
  m_end <- c(q_end_year, q_end_q * 3)
  # Extend two months before the sample start so the ragged-edge aggregation works.
  stats::window(baro_ts, start = m_start_back2, end = m_end)
}

stationarise_quarterly <- function(qdat, vars = c("gdp_growth", "inflation", "exch_rate"), frequency = 4L) {
  stopifnot(all(vars %in% names(qdat)))
  n_obs <- nrow(qdat)
  if (!n_obs) {
    return(list(data = qdat, transforms = vector("list", length = 0)))
  }

  indices <- seq_len(n_obs)
  qdat_adj <- qdat
  transforms <- vector("list", length(vars))
  names(transforms) <- vars

  for (var in vars) {
    series <- qdat[[var]]
    finite_mask <- is.finite(series)
    if (!any(finite_mask)) {
      warning(sprintf("Series '%s' contains no finite observations; skipping detrend/seasonal adjustment.", var))
      transforms[[var]] <- list(
        intercept = 0,
        slope = 0,
        frequency = frequency,
        seasonal = rep(0, frequency),
        has_seasonal = FALSE,
        last_index = n_obs
      )
      next
    }

    # Fit a deterministic linear trend. If the sample is too short for a slope,
    # fall back to mean-centering.
    if (sum(finite_mask) >= 2) {
      trend_fit <- stats::lm(series ~ indices)
      coef_fit <- stats::coef(trend_fit)
      intercept <- unname(coef_fit[[1]])
      slope <- if (length(coef_fit) > 1) unname(coef_fit[[2]]) else 0
    } else {
      intercept <- mean(series[finite_mask], na.rm = TRUE)
      slope <- 0
    }

    trend_component <- intercept + slope * indices
    detrended <- series - trend_component

    has_seasonal <- isTRUE(frequency > 1L) && (sum(finite_mask) >= frequency)
    if (has_seasonal) {
      season_ids <- ((indices - 1L) %% frequency) + 1L
      season_means <- tapply(detrended, season_ids, mean, na.rm = TRUE)
      seasonal_pattern <- rep(0, frequency)
      if (length(season_means)) {
        seasonal_pattern[as.integer(names(season_means))] <- season_means
      }
      seasonal_pattern <- seasonal_pattern - mean(seasonal_pattern, na.rm = TRUE)
      seasonal_component <- seasonal_pattern[season_ids]
      adjusted <- detrended - seasonal_component
    } else {
      seasonal_pattern <- rep(0, frequency)
      seasonal_component <- rep(0, n_obs)
      adjusted <- detrended
      has_seasonal <- FALSE
    }

    qdat_adj[[var]] <- adjusted

    transforms[[var]] <- list(
      intercept = intercept,
      slope = slope,
      frequency = frequency,
      seasonal = seasonal_pattern,
      has_seasonal = has_seasonal,
      last_index = n_obs
    )
  }

  list(data = qdat_adj, transforms = transforms)
}

restore_series_values <- function(values, variables, indices, transforms) {
  if (length(values) == 0) return(values)
  if (missing(variables) || missing(indices)) {
    stop("'variables' and 'indices' must be supplied when restoring series values.")
  }
  mapply(
    function(val, var, idx) {
      if (!is.finite(val) || is.na(idx) || !var %in% names(transforms)) {
        return(val)
      }
      transform <- transforms[[var]]
      trend <- transform$intercept + transform$slope * idx
      if (isTRUE(transform$has_seasonal)) {
        season_idx <- ((idx - 1L) %% transform$frequency) + 1L
        seasonal <- transform$seasonal[season_idx]
      } else {
        seasonal <- 0
      }
      val + trend + seasonal
    },
    values,
    variables,
    indices,
    SIMPLIFY = TRUE
  )
}

compute_time_index <- function(train_rows, steps) {
  stopifnot(length(train_rows) == 1)
  train_rows + steps
}
