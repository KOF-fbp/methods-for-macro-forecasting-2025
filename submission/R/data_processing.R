# Data ingestion and transformation utilities

qtr <- rvgdp <- cpi <- wkfreuro <- gdp_growth <- inflation <- exch_rate <- NULL

read_quarterly_data <- function(data_dir) {
  q_path <- file.path(data_dir, "data_quarterly.csv")
  stopifnot(file.exists(q_path))
  qraw <- readr::read_csv(q_path, show_col_types = FALSE)

  vars_q <- c("rvgdp", "cpi", "wkfreuro")
  missing <- setdiff(vars_q, names(qraw))
  if (length(missing)) {
    stop("Missing expected columns in quarterly CSV: ", paste(missing, collapse = ", "))
  }

  qraw |>
    dplyr::mutate(qtr = zoo::as.yearqtr(date, format = "%Y-%m")) |>
    dplyr::arrange(qtr) |>
      dplyr::mutate(
        gdp_growth = 400 * (log(rvgdp) - dplyr::lag(log(rvgdp))),
        inflation  = 400 * (log(cpi) - dplyr::lag(log(cpi))),
        exch_rate  = log(wkfreuro)
      ) |>
      tidyr::drop_na() |>
      dplyr::select(qtr, gdp_growth, inflation, exch_rate)
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
    dplyr::filter(qtr <= q_cutoff)
  if (!nrow(qdat)) {
    stop("No overlapping quarters between the quarterly dataset and the KOF Barometer.")
  }
  list(qdat = qdat, baro_ts = baro_ts)
}

quarter_to_month_end <- function(yq) {
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
  stats::window(baro_ts, start = m_start_back2, end = m_end)
}
