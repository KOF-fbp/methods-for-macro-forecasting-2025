library(quantmod)
library(lubridate)

# Load SMI data from Yahoo Finance
getSymbols("^SSMI", src = "yahoo")

# get daily
smi_daily <- getSymbols("^SSMI", src = "yahoo", auto.assign = FALSE)
tail(smi_daily)

# convert to monthly OHLC (index at last business day of month)
smi_monthly_ohlc <- to.monthly(smi_daily, indexAt = "lastof", OHLC = TRUE)

# Extract the closing prices
SMI_monthly_close <- Cl(smi_monthly_ohlc)

# Print the first rows
# head(SMI_monthly_close)

# IMPORTANT: Since the data here is the closing price of the last business day of the month, 
# but the other ts are always 1992-01-01, i am choosing to set each smi closing date to be 2007-01-31 -> 2007-02-01.
# corresponds better with the actual values to certain times, imo.
# 1) Convert xts → data.frame
smi_df <- data.frame(
  date = index(SMI_monthly_close),
  smi = as.numeric(SMI_monthly_close),
  row.names = NULL
)

# 2) Shift date to first day of next month
smi_df$date <- ceiling_date(as.Date(smi_df$date), "month")

# inspect 
head(smi_df)

# use this for swissdata, then write final df to the data folder there