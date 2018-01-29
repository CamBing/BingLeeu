
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

spots <-
  readRDS("data/Spots.rds")

spots <- 
  spots %>% 
  mutate(Days = format(date, "%A")) %>% filter(!Days %in% c("Saturday", "Sunday") ) %>% select(-Days)

## Calculating returns for ETFs

N_Capping <- 80 # Parameter that trims the universe set 

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


usdzar <- 
  spots %>% group_by(Spot) %>% 
  mutate(Return = Value/lag(Value)-1) %>%   
  filter(Spot == "ZAR_Spot") %>% 
  ungroup()
  
  
# Merging datasets:
mergeddataset <- 
  bind_rows(
    ETFReturns %>% select(date, Ticker, Return),
    SAData_Returns %>% select(date, Ticker, Return),
    usdzar %>% rename("Ticker" = Spot) %>% select(date, Ticker, Return)
  )


# Stratification ----------------------------------------------------------
Df <- 
  usdzar %>% select(date, Return) %>% filter(date > first(date))

StratValue1 <- 0.2 # Change threshold
StratValue2 <- 0.8 # Change threshold

df_Strat <- 
  Df %>% 
  mutate(Q1 = quantile(Return, StratValue1, na.rm = TRUE), Q2 = quantile(Return, StratValue2, na.rm = TRUE)) %>% 
  mutate(ID = ifelse(Return <= Q1, "Low", 
                     ifelse(Return > Q1 & Return <= Q2 , "Medium",
                            ifelse(Return > Q2 , "High", "NA")) )) %>% ungroup() 



HighDates <- df_Strat %>% filter(ID == "High") %>% pull(date)
LowDates <- df_Strat %>% filter(ID == "Low") %>% pull(date)


# And now you can do the following e.g.:
Regression_data <-   mergeddataset

Regression_data %>% filter(date %in% HighDates) # use this to isolate stratified sample in analysis


#========================#
# *** Nico Comments **** #
#========================#

# AGAIN: THIS IS ONLY an illustration. Use and tailor this to fit your own methodology - be creative in thinking how to use this. E.g., you might consider working with weekly returns after a high ZAR movement / weekly movement, or the day after a high return movement (the latter would require lagging your stock / ETF returns).

# I don't want to prescribe (this is ultimately your project) but think along the lines of:

# Returns day after high ZAR move - measures most immediately sensitive to ZAR movement.

# You could also use the return a week's return after a high currency movement (week movement).

# E.g. you could use the fornmat function above (used to trim out weekends) to calculate returns on a week to week basis (filter Days == "Wednesday" e.g.) and then look at ZAR movement's impact on same week stock movement. Here you won't lag.

# Please guys, let your mind go on this.


