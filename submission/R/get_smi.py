import yfinance as yf
import pandas as pd

# downloading smi data from yahoo finance
smi_daily = yf.download("^SSMI", start="1990-01-01", progress=False)

# Use Close price
smi_daily = smi_daily[['Close']].rename(columns={'Close': 'close'})

# get Monthly average
smi_monthly_avg = smi_daily['close'].resample('M').mean()

# If yfinance created a MultiIndex, drop ticker level
if isinstance(smi_monthly_avg.index, pd.MultiIndex):
    smi_monthly_avg = smi_monthly_avg.droplevel(0)

# rename from ticker
smi_monthly_avg = smi_monthly_avg.rename(columns={smi_monthly_avg.columns[0]: 'smi_monthly_avg'})

# remove ticker
smi_monthly_avg.columns.name = None

# Floor date to first of month
smi_monthly_avg['date'] = smi_monthly_avg.index.to_period('M').to_timestamp()

# get Monthly return
smi_monthly_avg['smi_monthly_return'] = smi_monthly_avg['smi_monthly_avg'].pct_change()

# Reset index
smi_df = smi_monthly_avg.reset_index(drop=True)

# print(smi_df.head())
# print(smi_df.tail())

# write to csv
output_path = "/Users/minna/Code/Macro_Forecasting/group_project/methods-for-macro-forecasting-2025/submission/data/raw_data/full_smi.csv"
smi_df.to_csv(output_path, index=False)

print("done!")
