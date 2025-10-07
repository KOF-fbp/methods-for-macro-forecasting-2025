
renv::install("tidyverse") # = pip 

library(tidyverse) # Install Y

myData <- read.csv("/Users/michal/Documents/MTEC/MMF/methods-for-macro-forecasting-2025/submission/data/data_quarterly.csv",sep=",") %>%
  select(c("date", "consg")) #numpy array

View(myData)
