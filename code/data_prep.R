
# Packages ----------------------------------------------------------------
packages <-
  c("tidyverse", "lubridate", "dplyr", "readr", "PerformanceAnalytics", "tbl2xts", "broom" )
library("rmsfuns")
load_pkg(packages)

# Data Prep ---------------------------------------------------------------

## Loading data

etfs <-
  readRDS("data/AllFunds.rds") %>% tbl_df()

data_original <-
  readRDS("data/SA_Rand_Returns.rds")

## Calculating returns for ETFs

N_Capping <- 80 # Parameter that trims the universe set - won't be of much practical use if an obscure and small and thinly traded stock is a great hedge. Focus, e.g., on the top 80 stocks by Market Cap.

ETFReturns <-
  etfs %>% group_by(Ticker) %>% 
  rename("TRI" = TOT_RETURN_INDEX_NET_DVDS) %>% 
  mutate(Return = TRI / lag(TRI)-1) %>% ungroup()

SAData_Returns <-   
  data_original %>% 
  filter(Universe == "JALSHAll") %>% 
  mutate(Return = coalesce(Return, 0) ) %>%   # To make NA's zero - check whether this fits in to your study / makes sense --> motivate.
  ungroup() %>% select(date, Ticker, BICS_LEVEL_1_SECTOR_NAME, Market.Cap, Return) %>% 
  group_by(date) %>% 
  arrange(date, Market.Cap) %>% 
  top_n(N_Capping, "Market.Cap") %>% ungroup()



# Merging datasets:
mergeddataset <- 
  bind_rows(
    ETFReturns %>% select(date, Ticker, Return),
    SAData_Returns %>% select(date, Ticker, Return)
  )

## Add USDZAR

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



