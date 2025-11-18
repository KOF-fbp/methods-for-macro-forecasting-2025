#' Prepare data on a complete monthly grid
#'
#' @param df data.table with date column and variables
#' @param meta Metadata list from infer_meta
#' @param log_vars Character vector of variables to log-transform (NULL = auto-detect positive vars)
#' @param standardize Logical, whether to standardize variables
#' @return List with data (monthly grid), transforms (means, sds, logs), freq
#' @export
prepare_monthly_grid <- function(df, meta, log_vars = NULL, standardize = FALSE) {
  
  # Parse dates to yearmon
  df <- data.table::copy(df)
  
  # Convert date column to yearmon
  if (is.character(df$date) || is.factor(df$date)) {
    # Try various date formats
    df$date <- tryCatch({
      as.Date(df$date)
    }, error = function(e) {
      # Try yearmon directly
      zoo::as.yearmon(df$date)
    })
  }
  
  if (!inherits(df$date, "yearmon")) {
    df$date <- zoo::as.yearmon(df$date)
  }
  
  # Create complete monthly grid
  date_range <- range(df$date, na.rm = TRUE)
  all_months <- seq(date_range[1], date_range[2], by = 1/12)
  
  # Initialize grid
  grid <- data.table::data.table(date = all_months)
  
  # Auto-detect log variables if not specified
  if (is.null(log_vars)) {
    log_vars <- character(0)
    for (v in meta$vars) {
      vals <- df[[v]][!is.na(df[[v]])]
      if (length(vals) > 0 && all(vals > 0)) {
        log_vars <- c(log_vars, v)
      }
    }
  }
  
  # Process each variable
  transforms <- list(means = list(), sds = list(), logs = log_vars)
  
  for (v in meta$vars) {
    # Merge variable onto grid
    temp <- data.table::data.table(date = df$date, value = df[[v]])
    temp <- temp[!is.na(temp$date), ]
    
    if (meta$freq[[v]] == "quarterly") {
      # For quarterly: observed value goes to quarter-end month
      # Keep NA for first two months of each quarter
      temp$value_q <- temp$value
      grid <- merge(grid, temp[, .(date, value_q)], by = "date", all.x = TRUE)
      
      # Identify quarter-end months (Mar, Jun, Sep, Dec = months 3, 6, 9, 12)
      grid[[v]] <- NA_real_
      qtr_end <- (as.numeric(format(grid$date, "%m")) %% 3 == 0)
      grid[[v]][qtr_end] <- grid$value_q[qtr_end]
      grid$value_q <- NULL
      
    } else {
      # Monthly: direct assignment
      grid <- merge(grid, temp[, .(date, value)], by = "date", all.x = TRUE)
      data.table::setnames(grid, "value", v)
    }
    
    # Apply log transform
    if (v %in% log_vars) {
      grid[[v]] <- log(grid[[v]])
    }
    
    # Standardize if requested
    if (standardize) {
      vals <- grid[[v]][!is.na(grid[[v]])]
      if (length(vals) > 0) {
        m <- mean(vals)
        s <- sd(vals)
        transforms$means[[v]] <- m
        transforms$sds[[v]] <- s
        grid[[v]] <- (grid[[v]] - m) / s
      }
    }
  }
  
  # Ensure grid is sorted by date
  data.table::setorder(grid, date)
  
  return(list(
    data = grid,
    transforms = transforms,
    freq = meta$freq
  ))
}
