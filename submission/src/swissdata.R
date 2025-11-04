library(devtools)
install_github("mbannert/swissdata")
library(swissdata)
library(yaml)
library(tidyverse)
# set_update_all()


# get data from swissdata repo 
# cannot be run by others, sorry :(
# i have access to the pre-cleaned data
setwd("/Users/minna/KOF_Lab/swissdata")

# ------------------------------------------------------
#                       Import Data
# ------------------------------------------------------

plkopr <- read.csv("ch.snb.plkopr/ch.snb.plkopr.csv")
devkum <- read.csv("ch.snb.devkum/ch.snb.devkum.csv")
# arbeitsmarkt
amarbma <- read.csv("ch.snb.amarbma/ch.snb.amarbma.csv")
# official zinsrate
snboffzisa <- read.csv("ch.snb.snboffzisa/ch.snb.snboffzisa.csv")
# couldn't find conretail

# View(devkum)

# meta data
plkopr_meta <- read_yaml("ch.snb.plkopr/ch.snb.plkopr.yaml")
devkum_meta <- read_yaml("ch.snb.devkum/ch.snb.devkum.yaml")
amarbma_meta <- read_yaml("ch.snb.amarbma/ch.snb.amarbma.yaml")
snboffzisa_meta <- read_yaml("ch.snb.snboffzisa/ch.snb.snboffzisa.yaml")

# View(snboffzisa_eu)
# View(plkopr_meta) 
# View(devkum_meta) 
# View(amarbma_meta)
# View(snboffzisa_meta)
# View(concon_meta)

# ------------------------------------------------------
#                       Clean Data
# ------------------------------------------------------

# --------------- swiss and european zins rate -> good predictor for swiss zinsraten
snboffzisa_ug <- snboffzisa |> filter(overview == "UG") # swiss policy rate UG 


# Import SRF from the raw SNB CSV file in the data folder
snboffzisa_raw_path <- "/Users/minna/Code/Macro_Forecasting/group_project/methods-for-macro-forecasting-2025/submission/data/Interest_rates_snb-data-snboffzisa-en-all-20251021-0900.csv"
snboffzisa_raw <- read.csv(snboffzisa_raw_path, sep = ";", skip = 3, header = TRUE, stringsAsFactors = FALSE)


# ---------------- Filter for SRF (EU marginal lending facility) and clean empty values
snboffzisa_eu <- snboffzisa_raw |> 
  filter(D0 == "SRF") |>
  filter(Value != "" & !is.na(Value)) |>
  mutate(Value = as.numeric(Value),
         Date = as.Date(paste0(Date, "-01"))) |>
  select(Date, D0, Value) |>
  rename(overview = D0, value = Value)


# ---------------- Devkum = DevisenKurse Monatlich 
# chf to eur, monthly avg (m0) 
devkum_eur <- devkum |>
    filter(mean_end == "m0") |>
    filter(currency == "eur1")


# TODO: transform each to value & take data as data
# find best lag



# build matrix 

