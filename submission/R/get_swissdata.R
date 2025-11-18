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

# ---------------- Filter for SRF (snb policy rate -> after 2019, before not published here, before 2019 -> avg. between upper and lower bound policy rate
snboffzisa_eu <- snboffzisa |>
  mutate(date = as.Date(date)) |>
  select(overview, date, value) |>
  tidyr::pivot_wider(
    names_from = overview,
    values_from = value
  ) |>
  mutate(
    snboffzisa_rate = if_else(
      !is.na(lz),
      lz,                    # use lz when available
      (ug1 + og1) / 2        # otherwise use average of upper + lower
    )
  ) |>
  select(date, snboffzisa_rate) |>
  arrange(date)

tail(snboffzisa_eu)

# ---------------- Devkum = DevisenKurse Monatlich 
# chf to eur, monthly avg (m0) 
devkum_eur <- devkum |>
    filter(mean_end == "m0") |>
    filter(currency == "eur1") |>
    mutate(date = as.Date(date)) |>
    select(date, devkum_eur = value)

tail(devkum_eur)
# ----------------- Amarbma = Arbeitsmarktzahlen (t0 = total registered unemployment)
amarbma_t0 <- amarbma |>
  filter(overview == "t0") |>
  mutate(date = as.Date(date)) |>
  select(date, amarbma_t0 = value) 

tail(amarbma_t0)
# --------------- plkopr = inflationszahlen (in 2020 prices)
plkopr_fin <- plkopr |>
  mutate(date = as.Date(date)) |>
  filter(overview == "ld2010100") |>
  select(date, plkopr = value)

tail(plkopr_fin)
# the other 2 are already filtered


# source full_smi.py which fetches monthly smi data -> from 1990 till today. quantmod (used in smi.R) only has from 2007 until 2025.
setwd("/Users/minna/Code/Macro_Forecasting/group_project/methods-for-macro-forecasting-2025/submission/data/")
print(getwd())
# source("smi.R")
smi <- read.csv("raw_data/full_smi.csv") |>
   mutate(date = as.Date(date)) 
  
# str(smi)

# Combine series with named columns & smi
series_list <- list(plkopr_fin, devkum_eur, amarbma_t0, snboffzisa_eu, smi)

combined_df <- reduce(series_list, full_join, by = "date") |>
  arrange(date)

tail(combined_df, n=10)

View(combined_df)
write.csv(combined_df, "processed/combined_timeseries.csv")

