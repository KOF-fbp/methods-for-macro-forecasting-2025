#' Identify quarter-end months in a time index
#'
#' @param month_index Vector of yearmon dates
#' @return Logical vector, TRUE for quarter-end months (Mar, Jun, Sep, Dec)
#' @export
quarter_end_index <- function(month_index) {
  month_nums <- as.numeric(format(month_index, "%m"))
  return(month_nums %% 3 == 0)
}

#' Build triplets of month indices for quarterly averaging
#'
#' @param T_total Total number of time periods
#' @param quarter_end_locs Integer vector of quarter-end positions
#' @return List of integer triplets, each giving (t-2, t-1, t) for a quarter end at t
#' @export
quarter_average_rows <- function(T_total, quarter_end_locs) {
  triplets <- list()
  
  for (qe in quarter_end_locs) {
    if (qe >= 3) {
      triplets[[length(triplets) + 1]] <- c(qe - 2, qe - 1, qe)
    }
  }
  
  return(triplets)
}

#' Build observation matrix Z_t for a given origin date
#'
#' @param meta Metadata list
#' @param calendar Calendar object
#' @param origin_date yearmon forecast origin
#' @param n_state Dimension of state vector (n * p for companion form)
#' @param p VAR lag order
#' @param series_names Character vector of series names in order
#' @param month_index Vector of yearmon dates for the full sample
#' @return Matrix Z_t with one row per available observation
#' @export
build_Zt <- function(meta, calendar, origin_date, n_state, p, series_names, month_index) {
  
  n <- length(series_names)
  T_total <- length(month_index)
  
  # Get availability
  avail <- availability(calendar, origin_date)
  
  # Build Z row by row
  Z_rows <- list()
  row_count <- 0
  
  for (t in 1:T_total) {
    current_date <- month_index[t]
    
    for (i in 1:n) {
      v <- series_names[i]
      
      # Check if this observation is available
      if (length(avail[[v]]) >= t && avail[[v]][t]) {
        
        if (meta$freq[[v]] == "monthly") {
          # Monthly observation: select y_i,t from state
          # State is (y_t, y_{t-1}, ..., y_{t-p+1})
          # y_i,t is in position i of the first block
          z_row <- rep(0, n_state)
          z_row[i] <- 1
          
          Z_rows[[row_count + 1]] <- z_row
          row_count <- row_count + 1
          
        } else if (meta$freq[[v]] == "quarterly") {
          # Quarterly observation at quarter end: average of three months
          # Check if this is a quarter end
          if (quarter_end_index(current_date)) {
            # Average y_i,t + y_i,{t-1} + y_i,{t-2}
            z_row <- rep(0, n_state)
            
            # Position of y_i in each lag block
            # Block 1 (y_t): positions 1 to n
            # Block 2 (y_{t-1}): positions (n+1) to 2n
            # Block 3 (y_{t-2}): positions (2n+1) to 3n
            
            if (p >= 1) z_row[i] <- 1/3              # y_i,t
            if (p >= 2) z_row[n + i] <- 1/3          # y_i,{t-1}
            if (p >= 3) z_row[2*n + i] <- 1/3        # y_i,{t-2}
            
            # If p < 3, we only average available lags
            # For p=1, just y_i,t; for p=2, average y_i,t and y_i,{t-1}
            if (p == 1) {
              z_row[i] <- 1
            } else if (p == 2) {
              z_row[i] <- 0.5
              z_row[n + i] <- 0.5
            }
            
            Z_rows[[row_count + 1]] <- z_row
            row_count <- row_count + 1
          }
        }
      }
    }
  }
  
  if (row_count == 0) {
    # No observations available
    return(matrix(0, nrow = 0, ncol = n_state))
  }
  
  Z <- do.call(rbind, Z_rows)
  return(Z)
}

#' Build measurement error covariance matrix H_t
#'
#' @param series_names Character vector of series names
#' @param measurement_variances Named vector of measurement error variances (default: zero)
#' @return Diagonal matrix H
#' @export
build_Ht <- function(series_names, measurement_variances = NULL) {
  
  n <- length(series_names)
  
  if (is.null(measurement_variances)) {
    # Default: zero measurement error
    H <- diag(0, n)
  } else {
    # Use provided variances
    h_diag <- rep(0, n)
    for (i in 1:n) {
      v <- series_names[i]
      if (v %in% names(measurement_variances)) {
        h_diag[i] <- measurement_variances[v]
      }
    }
    H <- diag(h_diag, n)
  }
  
  rownames(H) <- colnames(H) <- series_names
  return(H)
}
