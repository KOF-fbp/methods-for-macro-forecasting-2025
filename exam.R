############################################################
# Methods of Macroeconomic Forecasting - Final Exam HS 2025
# Part A: R-based applications
############################################################

# Name: FIRSTNAME LASTNAME
# Matriculation number: 12345678

# IMPORTANT:
# - Do NOT change the names of objects requested in the exam.
# - Do NOT remove any of the section headers.
# - Type your answers in the indicated places.
# - Objects gdp, cpi, fc_ar, fc_bvar, gdp_qoq_real are provided
#   in `exam_data.RData`.
#   Load it at the start of the exam using:
#       load("exam_data.RData")
#   Note: You may need to adjust the file path.

############################################################
# Part A.1: Transformations and visualization
############################################################

## A1(a) (2 pts)
## Compute:
##   - gdp_qoq  : annualised quarter-on-quarter log growth of GDP
##   - infl_qoq : annualised quarter-on-quarter log inflation
## Use the existing objects: gdp (ts), cpi (ts).

# TODO: your code for A1(a) here
# Example:
# gdp_qoq  <- ...
# infl_qoq <- ...


## A1(b) (2 pts)
## Construct a new ts/mts object data_ts that combines gdp_qoq and
## infl_qoq as two columns, aligned over the common sample.
## Name the object exactly: data_ts

# TODO: your code for A1(b) here
# Example:
# data_ts <- ...


############################################################
# Part A.2: Simple forecast evaluation in R
############################################################

## A2(a) (2 pts)
## You are given:
##   - fc_ar        : numeric vector of AR forecasts for gdp_qoq
##   - fc_bvar      : numeric vector of BVAR forecasts for gdp_qoq
##   - gdp_qoq_real : realised gdp_qoq
## Task:
##   - Create err_ar   = gdp_qoq_real - fc_ar
##   - Create err_bvar = gdp_qoq_real - fc_bvar
##   - Compute rmse_ar and rmse_bvar

# TODO: your code for A2(a) here
# Example:
# err_ar    <- ...
# err_bvar  <- ...
# rmse_ar   <- ...
# rmse_bvar <- ...


## A2(b) (1 pt)
## Based on rmse_ar and rmse_bvar, state which model performs better
## and by how much (absolute RMSE difference).
## --> This is a WRITTEN answer in the exam booklet, NOT in R.

## A2(c) (1 pt)
## Explain one limitation of using only RMSE to judge forecast quality.
## --> This is a WRITTEN answer in the exam booklet, NOT in R.


############################################################
# END OF R PART
############################################################
