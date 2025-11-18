#' Load data from CSV file(s)
#'
#' @param paths_or_single_path Character vector of file paths or a single path
#' @param guess_wide Logical, whether to guess if data is in wide format
#' @return data.table with date column and variable columns
#' @export
load_data <- function(paths_or_single_path, guess_wide = TRUE) {
  if (length(paths_or_single_path) == 1) {
    df <- data.table::fread(paths_or_single_path)
    data.table::setDT(df)
    
    # Try to identify date column
    date_cols <- grep("date|time|period", names(df), ignore.case = TRUE, value = TRUE)
    if (length(date_cols) == 0) {
      stop("No date column found. Please ensure a column named 'date' exists.")
    }
    date_col <- date_cols[1]
    
    # Rename to standard 'date' if needed
    if (date_col != "date") {
      data.table::setnames(df, old = date_col, new = "date")
    }
    
    return(df)
  } else {
    # Multiple files - read and merge
    dfs <- lapply(paths_or_single_path, function(p) {
      dt <- data.table::fread(p)
      data.table::setDT(dt)
      
      # Identify date column
      date_cols <- grep("date|time|period", names(dt), ignore.case = TRUE, value = TRUE)
      if (length(date_cols) > 0) {
        date_col <- date_cols[1]
        if (date_col != "date") {
          data.table::setnames(dt, old = date_col, new = "date")
        }
      }
      dt
    })
    
    # Merge all on date
    result <- dfs[[1]]
    if (length(dfs) > 1) {
      for (i in 2:length(dfs)) {
        result <- merge(result, dfs[[i]], by = "date", all = TRUE)
      }
    }
    
    return(result)
  }
}

#' Infer metadata from data and user specifications
#'
#' @param df data.table with date and variable columns
#' @param quarterly_vars Character vector of quarterly variable names
#' @param monthly_vars Character vector of monthly variable names
#' @param price_vars Character vector of price index variable names
#' @param gdp_var Character, name of GDP variable
#' @param exrate_vars Character vector of exchange rate variable names
#' @param meta_yaml Path to YAML metadata file (optional)
#' @return List with metadata including frequency, roles, transformations
#' @export
infer_meta <- function(df, quarterly_vars = NULL, monthly_vars = NULL, 
                       price_vars = NULL, gdp_var = NULL, exrate_vars = NULL,
                       meta_yaml = NULL) {
  
  # If YAML provided, read it
  if (!is.null(meta_yaml) && file.exists(meta_yaml)) {
    meta_from_yaml <- yaml::read_yaml(meta_yaml)
    
    # Override with YAML values
    if (!is.null(meta_from_yaml$quarterly_vars)) quarterly_vars <- meta_from_yaml$quarterly_vars
    if (!is.null(meta_from_yaml$monthly_vars)) monthly_vars <- meta_from_yaml$monthly_vars
    if (!is.null(meta_from_yaml$price_vars)) price_vars <- meta_from_yaml$price_vars
    if (!is.null(meta_from_yaml$gdp_var)) gdp_var <- meta_from_yaml$gdp_var
    if (!is.null(meta_from_yaml$exrate_vars)) exrate_vars <- meta_from_yaml$exrate_vars
  }
  
  all_vars <- setdiff(names(df), "date")
  
  # Default: if not specified, guess based on data availability patterns
  if (is.null(quarterly_vars) && is.null(monthly_vars)) {
    # Simple heuristic: count non-NA values per variable
    # Quarterly should have ~1/3 the observations of monthly
    na_counts <- sapply(all_vars, function(v) sum(!is.na(df[[v]])))
    median_count <- median(na_counts)
    
    # Variables with significantly fewer observations might be quarterly
    quarterly_vars <- names(na_counts[na_counts < median_count * 0.5])
    monthly_vars <- setdiff(all_vars, quarterly_vars)
  }
  
  if (is.null(monthly_vars)) {
    monthly_vars <- setdiff(all_vars, quarterly_vars)
  }
  if (is.null(quarterly_vars)) {
    quarterly_vars <- setdiff(all_vars, monthly_vars)
  }
  
  # Build frequency map
  freq <- list()
  for (v in quarterly_vars) freq[[v]] <- "quarterly"
  for (v in monthly_vars) freq[[v]] <- "monthly"
  
  # Build metadata structure
  meta <- list(
    vars = all_vars,
    quarterly_vars = quarterly_vars,
    monthly_vars = monthly_vars,
    freq = freq,
    price_vars = price_vars,
    gdp_var = gdp_var,
    exrate_vars = exrate_vars
  )
  
  return(meta)
}
