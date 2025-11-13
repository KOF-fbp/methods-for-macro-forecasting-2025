# Data ingestion and transformation utilities

utils::globalVariables(c(
  ".data", "qtr", "rvgdp", "cpi", "wkfreuro", "gdp_growth", "inflation",
  "exch_rate", "time_index", "series", "value", "date"
))

qtr <- rvgdp <- cpi <- wkfreuro <- gdp_growth <- inflation <- exch_rate <- time_index <- NULL
series <- value <- date <- NULL

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

read_combined_timeseries <- function(data_dir, variables = NULL, deduplicate = c("first", "mean")) {
  combined_path <- file.path(data_dir, "combined_timeseries.csv")
  if (!file.exists(combined_path)) {
    stop("Combined monthly dataset not found at ", combined_path)
  }

  deduplicate <- match.arg(deduplicate)
  raw <- readr::read_csv(combined_path, show_col_types = FALSE, progress = interactive()) |>
    dplyr::mutate(date = as.Date(.data$date)) |>
    dplyr::select(-tidyselect::matches("^Unnamed"))

  available_vars <- setdiff(names(raw), "date")
  if (is.null(variables)) {
    variables <- available_vars
  }

  missing_vars <- setdiff(variables, available_vars)
  if (length(missing_vars)) {
    stop("Requested monthly columns missing in combined_timeseries.csv: ", paste(missing_vars, collapse = ", "))
  }

  tidy <- raw |>
    dplyr::select(date, dplyr::all_of(variables)) |>
    tidyr::pivot_longer(-date, names_to = "series", values_to = "value")

  deduped <- tidy |>
    dplyr::group_by(.data$series, .data$date) |>
    dplyr::summarise(
      value = {
        vals <- value[!is.na(value)]
        if (!length(vals)) {
          NA_real_
        } else if (identical(deduplicate, "mean")) {
          mean(vals)
        } else {
          vals[[1]]
        }
      },
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(names_from = "series", values_from = "value") |>
    dplyr::arrange(.data$date)

  complete_rows <- deduped |>
    dplyr::filter(dplyr::if_all(-date, ~ !is.na(.)))

  if (!nrow(complete_rows)) {
    stop("Combined monthly dataset does not contain any period with all requested variables.")
  }

  start_date <- dplyr::first(complete_rows$date)
  end_date <- dplyr::last(complete_rows$date)

  monthly_complete <- complete_rows |>
    dplyr::filter(.data$date >= start_date & .data$date <= end_date)

  start_year <- lubridate::year(start_date)
  start_month <- lubridate::month(start_date)

  ts_list <- lapply(setNames(variables, variables), function(var) {
    stats::ts(monthly_complete[[var]], start = c(start_year, start_month), frequency = 12)
  })

  list(
    data = monthly_complete,
    ts_list = ts_list,
    start_date = start_date,
    end_date = end_date
  )
}

trim_to_overlap <- function(qdat, monthly_input) {
  if (inherits(monthly_input, "ts")) {
    series_end <- stats::end(monthly_input)
    last_q_num <- floor(series_end[2] / 3)
    if (last_q_num == 0) {
      last_q_year <- series_end[1] - 1
      last_q_num <- 4
    } else {
      last_q_year <- series_end[1]
    }
    q_cutoff <- zoo::as.yearqtr(sprintf("%d Q%d", last_q_year, last_q_num))
    qdat <- qdat |>
      dplyr::filter(.data$qtr <= q_cutoff)
    if (!nrow(qdat)) {
      stop("No overlapping quarters between the quarterly dataset and the monthly indicator.")
    }
    return(list(qdat = qdat, baro_ts = stats::window(monthly_input, end = c(series_end[1], series_end[2]))))
  }

  if (!is.list(monthly_input) || !length(monthly_input)) {
    stop("Monthly input must be either a 'ts' object or a named list of 'ts' objects.")
  }

  ends_matrix <- vapply(monthly_input, stats::end, numeric(2))
  min_year <- min(ends_matrix[1, ])
  min_month <- min(ends_matrix[2, ])
  last_q_num <- floor(min_month / 3)
  if (last_q_num == 0) {
    last_q_year <- min_year - 1
    last_q_num <- 4
  } else {
    last_q_year <- min_year
  }
  q_cutoff <- zoo::as.yearqtr(sprintf("%d Q%d", last_q_year, last_q_num))
  qdat_trimmed <- qdat |>
    dplyr::filter(.data$qtr <= q_cutoff)
  if (!nrow(qdat_trimmed)) {
    stop("No overlapping quarters between the quarterly dataset and the monthly indicators.")
  }

  trimmed_monthly <- lapply(monthly_input, function(x) {
    series_end <- stats::end(x)
    end_year <- min(series_end[1], min_year)
    end_month <- if (series_end[1] == min_year) min(series_end[2], min_month) else min_month
    stats::window(x, end = c(end_year, end_month))
  })

  list(qdat = qdat_trimmed, monthly = trimmed_monthly)
}

quarter_to_month_end <- function(yq) {
  # Convert a year-quarter stamp to the corresponding month-end indices.
  end_month <- zoo::as.yearmon(yq) + (2 / 12)
  end_date <- zoo::as.Date(end_month)
  c(lubridate::year(end_date), lubridate::month(end_date))
}

build_q_ts <- function(q_subset) {
  q_z <- lapply(names(q_subset)[-1], function(v) zoo::zoo(q_subset[[v]], q_subset$qtr))
  names(q_z) <- names(q_subset)[-1]
  lapply(q_z, stats::as.ts)
}

build_Y <- function(q_subset, monthly_inputs) {
  # Arrange inputs into the mfbvar::set_prior structure. Monthly indicators can be
  # supplied either as a single ts object or a named list of ts objects.
  q_ts_local <- build_q_ts(q_subset)
  if (inherits(monthly_inputs, "ts")) {
    monthly_list <- list(kofbarometer = stats::as.ts(monthly_inputs))
  } else if (is.list(monthly_inputs)) {
    if (is.null(names(monthly_inputs)) || any(!nzchar(names(monthly_inputs)))) {
      names(monthly_inputs) <- paste0("indicator_", seq_along(monthly_inputs))
    }
    monthly_list <- lapply(monthly_inputs, stats::as.ts)
  } else {
    stop("monthly_inputs must be a 'ts' object or a list of 'ts' objects.")
  }

  c(
    monthly_list,
    list(
      quarterly = cbind(
        gdp_growth = q_ts_local[["gdp_growth"]],
        inflation  = q_ts_local[["inflation"]],
        exch_rate  = q_ts_local[["exch_rate"]]
      )
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

window_monthly_series <- function(monthly_list, qdat, back_months = 2L) {
  if (!length(monthly_list)) {
    stop("'monthly_list' must contain at least one series.")
  }
  q_start <- qdat$qtr[1]
  q_start_date <- zoo::as.Date(zoo::as.yearmon(q_start))
  back_period <- lubridate::period(back_months, units = "months")
  q_start_back <- suppressMessages(lubridate::`%m-%`(q_start_date, back_period))
  start_indices <- c(lubridate::year(q_start_back), lubridate::month(q_start_back))

  q_end <- qdat$qtr[nrow(qdat)]
  end_indices <- quarter_to_month_end(q_end)

  lapply(monthly_list, function(series) {
    series_start <- stats::start(series)
    adj_start <- start_indices
    if (series_start[1] > adj_start[1] || (series_start[1] == adj_start[1] && series_start[2] > adj_start[2])) {
      adj_start <- series_start
    }

    series_end <- stats::end(series)
    adj_end <- end_indices
    if (series_end[1] < adj_end[1] || (series_end[1] == adj_end[1] && series_end[2] < adj_end[2])) {
      adj_end <- series_end
    }
    stats::window(series, start = adj_start, end = adj_end)
  })
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
