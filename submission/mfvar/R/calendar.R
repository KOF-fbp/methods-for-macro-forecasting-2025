#' Build a release calendar for real-time data availability
#'
#' @param df data.table on monthly grid
#' @param meta Metadata list
#' @param user_calendar_csv Path to custom calendar CSV with columns: series, date, available
#' @return Calendar object with availability method
#' @export
build_release_calendar <- function(df, meta, user_calendar_csv = NULL) {
  
  # If user provides custom calendar, use it
  if (!is.null(user_calendar_csv) && file.exists(user_calendar_csv)) {
    cal_data <- data.table::fread(user_calendar_csv)
    
    calendar <- structure(
      list(
        custom = TRUE,
        data = cal_data,
        vars = meta$vars,
        freq = meta$freq
      ),
      class = "Calendar"
    )
    
    return(calendar)
  }
  
  # Default stylized calendar
  # Monthly series for month t available at t+1
  # Quarterly series for quarter Q available at first month after quarter end
  
  all_dates <- df$date
  n_dates <- length(all_dates)
  
  calendar <- structure(
    list(
      custom = FALSE,
      dates = all_dates,
      vars = meta$vars,
      freq = meta$freq,
      n_dates = n_dates
    ),
    class = "Calendar"
  )
  
  return(calendar)
}

#' Check data availability at a given origin date
#'
#' @param calendar Calendar object
#' @param origin_date yearmon, the forecast origin
#' @return Named logical vector indicating which observations are available
#' @export
availability <- function(calendar, origin_date) {
  UseMethod("availability")
}

#' @export
availability.Calendar <- function(calendar, origin_date) {
  
  if (!inherits(origin_date, "yearmon")) {
    origin_date <- zoo::as.yearmon(origin_date)
  }
  
  if (calendar$custom) {
    # Use custom calendar data
    cal_sub <- calendar$data[calendar$data$date <= origin_date, ]
    
    avail <- list()
    for (v in calendar$vars) {
      v_data <- cal_sub[cal_sub$series == v, ]
      if (nrow(v_data) > 0) {
        avail[[v]] <- v_data$available
      } else {
        avail[[v]] <- logical(0)
      }
    }
    
    return(avail)
  } else {
    # Stylized calendar
    avail <- list()
    
    for (v in calendar$vars) {
      if (calendar$freq[[v]] == "monthly") {
        # Monthly data for month t available at origin t+1
        # So at origin_date, we have data up to origin_date - 1 month
        last_available <- origin_date - 1/12
        avail[[v]] <- calendar$dates <= last_available
        
      } else if (calendar$freq[[v]] == "quarterly") {
        # Quarterly data for quarter Q available at first month after quarter end
        # Quarter ends: Mar (3), Jun (6), Sep (9), Dec (12)
        # Available one month later: Apr (4), Jul (7), Oct (10), Jan (1)
        
        # Find most recent quarter end before origin
        origin_month <- as.numeric(format(origin_date, "%m"))
        origin_year <- as.numeric(format(origin_date, "%Y"))
        
        # Most recent quarter end relative to origin
        if (origin_month >= 4) {
          last_qtr_end <- zoo::as.yearmon(paste0(origin_year, "-03"))
        } else {
          last_qtr_end <- zoo::as.yearmon(paste0(origin_year - 1, "-12"))
        }
        
        if (origin_month >= 7) {
          last_qtr_end <- zoo::as.yearmon(paste0(origin_year, "-06"))
        }
        if (origin_month >= 10) {
          last_qtr_end <- zoo::as.yearmon(paste0(origin_year, "-09"))
        }
        if (origin_month >= 1 && origin_month <= 3) {
          last_qtr_end <- zoo::as.yearmon(paste0(origin_year - 1, "-12"))
        }
        
        # Data available for quarter ends up to and including last_qtr_end
        is_qtr_end <- (as.numeric(format(calendar$dates, "%m")) %% 3 == 0)
        avail[[v]] <- is_qtr_end & (calendar$dates <= last_qtr_end)
        
      } else {
        # Unknown frequency, assume not available
        avail[[v]] <- rep(FALSE, calendar$n_dates)
      }
    }
    
    return(avail)
  }
}
