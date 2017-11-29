
# Packages ----------------------------------------------------------------
packages <-
  c("tidyverse", "lubridate", "dplyr", "readr", "PerformanceAnalytics", "tbl2xts" )
load_pkg(packages)
# Data Prep ---------------------------------------------------------------

## Loading data
etfs <-
  readRDS("data/AllFunds.rds")

data_original <-
  readRDS("data/ReturnsData.rds")

## Calculating returns for ETFs
etf_returns <- 
  etfs %>%
  tbl_xts(.) %>% 
  Return.calculate(., method = c("compound", "simple")[1]) %>% 
  xts_tbl()
  


  



## merging data




# Stratification ----------------------------------------------------------
Df <-
  data.frame(
    date = dateconverter(ymd(20100101), ymd(20170901), Transform = "weekdayEOW"),
    Value = runif(length(dateconverter(ymd(20100101), ymd(20170901), Transform = "weekdayEOW")))
  )

StratValue1 <- 0.2 # Change threshold
StratValue2 <- 0.8 # Change threshold

df_Strat <- 
  Df %>% 
  mutate(Q1 = quantile(Value, StratValue1, na.rm = TRUE), Q2 = quantile(Value, StratValue2, na.rm = TRUE)) %>% 
  mutate(ID = ifelse(Value <= Q1, "Low", 
                     ifelse(Value > Q1 & Value <= Q2 , "Medium",
                            ifelse(Value > Q2 , "High", "NA")) )) %>% ungroup() 



HighDates <- df_Strat
LowDates <- df_Strat

# Now use these dates to truncate your sample in order to reflect your strata of choice.

# E.g., in your regression function:

# df %>% filter(date %in% HighDates) %>% ...  



