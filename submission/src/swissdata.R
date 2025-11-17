rm(list = ls())
# library(devtools)
# install_github("mbannert/swissdata")
library(swissdata)
library(yaml)
library(tidyverse)
# set_update_all()


# get data from swissdata repo 
# cannot be run by others, sorry :(
# i have access to the pre-cleaned data
setwd("/Users/minna/swissdata")

# ------------------------------------------------------
#                       Import Data
# ------------------------------------------------------
print(getwd())
plkopr <- read.csv("ch.snb.plkopr/ch.snb.plkopr.csv")
devkum <- read.csv("ch.snb.devkum/ch.snb.devkum.csv")
# arbeitsmarkt
# for some reason in download?
amarbma <- read.csv("ch.snb.amarbma/ch.snb.amarbma.csv")
# official zinsrate
snboffzisa <- read.csv("ch.snb.snboffzisa/ch.snb.snboffzisa.csv")
# couldn't find conretail
# View(devkum)

# meta data
plkopr_meta <- read_yaml("ch.snb.plkopr/ch.snb.plkopr.yaml")
devkum_meta <- read_yaml("ch.snb.devkum/ch.snb.devkum.yaml")
amarbma_meta <- read_yaml("ch.snb.amarbma/ch.snb.amarbma.yaml")
# TODO: check publication date -> later than the others 
# how do we deal with this
snboffzisa_meta <- read_yaml("ch.snb.snboffzisa/ch.snb.snboffzisa.yaml")


# ------------------------------------------------------
#                       Clean Data
# ------------------------------------------------------

# --------------- swiss and european zins rate -> good predictor for swiss zinsraten
# snboffzisa_ug <- snboffzisa |> filter(overview == "UG") # swiss policy rate UG 


# Import SRF from the raw SNB CSV file in the data folder
# snboffzisa_raw_path <- "/Users/minna/Code/Macro_Forecasting/group_project/methods-for-macro-forecasting-2025/submission/data/Interest_rates_snb-data-snboffzisa-en-all-20251021-0900.csv"
# snboffzisa_raw <- read.csv(snboffzisa_raw_path, sep = ";", skip = 3, header = TRUE, stringsAsFactors = FALSE)




# ---------------- Filter for SRF (EU marginal lending facility) and clean empty values
snboffzisa_eu <- snboffzisa |> 
  filter(overview == "srf") |>
  mutate(date = as.Date(date)) |>
  select(date, snboffzisa_eu = value)


tail(snboffzisa_eu)


# ---------------- Devkum = DevisenKurse Monatlich 
# chf to eur, monthly avg (m0) 
devkum_eur <- devkum |>
    filter(mean_end == "m0") |>
    filter(currency == "eur1") |>
    mutate(date = as.Date(date)) |>
    select(date, devkum_eur = value)

tail(devkum_eur)
# ----------------- Amarbma = Arbeitsmarktzahlen (t0 = total)
# TODO: only use 1 value!!!
amarbma_t0 <- amarbma |>
  filter(overview == "t0") |>
  mutate(date = as.Date(date)) |>
  select(date, amarbma_t0 = value) 

tail(amarbma_t0)
# --------------- plkopr = inflationszahlen

plkopr_fin <- plkopr |>
  mutate(date = as.Date(date)) |>
  select(date, plkopr = value)

tail(plkopr_fin)
# the other 2 are already filtered


# Combine series with named columns
series_list <- list(plkopr_fin, devkum_eur, amarbma_t0, snboffzisa_eu)

combined_df <- reduce(series_list, full_join, by = "date") |>
  arrange(date)

tail(combined_df, n=10)


setwd("/Users/minna/Code/Macro_Forecasting/group_project/methods-for-macro-forecasting-2025/submission/data/")
write.csv(combined_df, "combined_timeseries.csv")

