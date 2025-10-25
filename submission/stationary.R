# In this file we check for stationarity of the time series 
#and in case we apply a transformation to make them stationary

check_stationarity <- function(ts_data, min_length = 10) {
  # Robust ADF test: rimuove NA, controlla lunghezza minima e cattura errori
  vec <- na.omit(as.numeric(ts_data))
  if (length(vec) < min_length) {
    warning("Serie troppo corta per ADF test; ritorno FALSE.")
    return(FALSE)
  }
  adf_test <- tryCatch(
    tseries::adf.test(vec),
    error = function(e) {
      warning("ADF test fallito: ", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(adf_test)) return(FALSE)
  if (adf_test$p.value < 0.05) {
    cat("Time Series is stationary (p-value:", adf_test$p.value, ")\n")
  } else {
    cat("Time Series is non-stationary (p-value:", adf_test$p.value, ")\n")
  }
  return(adf_test$p.value < 0.05)
}

make_stationary <- function(ts_data, use_log = TRUE, align = TRUE, tiny = 1e-6) {
  # Trasforma la serie per renderla stazionaria.
  # Se align = TRUE restituisce un vettore della stessa lunghezza dell'input (prepende NA).
  orig_len <- length(ts_data)
  orig_names <- names(ts_data)

  if (check_stationarity(ts_data)) {
    cat("Series already stationary. No transformation applied.\n")
    return(ts_data)
  }

  vec <- as.numeric(ts_data)
  if (use_log) {
    # Gestione valori <= 0 aggiungendo un offset minimo
    if (any(vec <= 0, na.rm = TRUE)) {
      offset <- abs(min(vec, na.rm = TRUE)) + tiny
      warning("Valori <= 0 trovati: applico offset = ", round(offset, 6), " prima del log.")
      vec_adj <- vec + offset
    } else {
      vec_adj <- vec
    }
    ts_trans <- log(vec_adj)
    cat("Applied log to make series stationary.\n")
  } else {
    ts_trans <- diff(vec)
    cat("Applied simple differencing to make series stationary.\n")
  }

  if (align) {
    out <- rep(NA_real_, orig_len)
    if (orig_len > 1) out[-1] <- as.numeric(ts_trans)
    if (!is.null(orig_names)) names(out) <- orig_names
    return(out)
  } else {
    # Ritorna la serie differenziata (più corta) con nomi spostati
    if (!is.null(orig_names)) names(ts_trans) <- orig_names[-1]
    return(as.numeric(ts_trans))
  }
}