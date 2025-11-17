library(quantmod)
library(lubridate)
library(zoo)

# Load SMI data from Yahoo Finance
getSymbols("^SSMI", src = "yahoo")

# get daily
smi_daily <- getSymbols("^SSMI", src = "yahoo", auto.assign = FALSE)

# calculate the monthly average, instead of taking the closing price of the last day of the month
smi_monthly_avg <- period.apply(
  smi_daily$SSMI.Adjusted,
  INDEX = endpoints(smi_daily, "months"),
  FUN = function(x) {
    mean(as.numeric(x))
  }
)
sum(is.na(smi_monthly_avg)) 

colnames(smi_monthly_avg) <- "smi_monthly_avg"

View(smi_monthly_avg)


smi_df <- data.frame(
  date = floor_date(as.Date(index(smi_monthly_avg)), "month"),
  smi_monthly_avg = as.numeric(smi_monthly_avg),
  row.names = NULL
)

tail(smi_df)

#### ------------------ PREVIOUSLY -------
# used the closing price of the last day of the month, then converted that to the next month, i.e. 2007-01-31 becomes 2007-02-01

# convert to monthly OHLC (index at last business day of month)
# smi_monthly_ohlc <- to.monthly(smi_daily, indexAt = "lastof", OHLC = TRUE)

# Extract the closing prices
# SMI_monthly_close <- Cl(smi_monthly_ohlc)

# Print the first rows
# head(SMI_monthly_close)

# IMPORTANT: Since the data here is the closing price of the last business day of the month, 
# but the other ts are always 1992-01-01, i am choosing to set each smi closing date to be 2007-01-31 -> 2007-02-01.
# corresponds better with the actual values to certain times, imo.

# 1) Convert xts → data.frame
# smi_df <- data.frame(
#   date = index(SMI_monthly_close),
#   smi = as.numeric(SMI_monthly_close),
#   row.names = NULL
# )

# # 2) Shift date to first day of next month
# smi_df$date <- ceiling_date(as.Date(smi_df$date), "month")

# inspect 
head(smi_df)

# use this for swissdata, then write final df to the data folder there