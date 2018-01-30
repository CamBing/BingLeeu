# Libraries ---------------------------------------------------------------
library(tidyverse)
library(rmsfuns)
library(lubridate)
library(broom)
library(rugarch)
library(rmgarch)
library(tbl2xts)
library(MTS)
library(PerformanceAnalytics)

# Loading data ------------------------------------------------------------
etfs <-
  readRDS("data/AllFunds.rds") %>% tbl_df()

data_original <-
  readRDS("data/SA_Rand_Returns.rds")

spots <-
  readRDS("data/Spots.rds") %>% 
  mutate(Days = format(date, "%A")) %>% filter(!Days %in% c("Saturday", "Sunday") ) %>% select(-Days)

# Merging and Calculating returns -----------------------------------------------------


N_Capping <- 30 # Parameter that trims the universe set. Focus, e.g., on the top 80 stocks by Market Cap.

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

# Caluclating returns for USDZAR:

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


# Dlog Returns:
mergeddataset <- 
  mergeddataset %>% arrange(date) %>% group_by(Ticker) %>% mutate(Return = coalesce(Return, 0)) %>% 
  mutate(Index = cumprod(1+Return) ) %>% 
  mutate(DlogReturn =  log(Index) - log( lag(Index))) %>% ungroup() %>% 
  mutate(DlogReturn = coalesce(DlogReturn, 0)) %>% select(-Index)

# Now choose which you want to use.

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

Regression_data %>% filter(date %in% HighDates)


# AGAIN: THIS IS ONLY an illustration. Use and tailor this to fit your own methodology - be creative in thinking how to use this. E.g., you might consider working with weekly returns after a high ZAR movement / weekly movement, or the day after a high return movement (the latter would require lagging your stock / ETF returns).

# I don't want to prescribe (this is ultimately your project) but think along the lines of:

# Returns day after high ZAR move - measures most immediately sensitive to ZAR movement.

# You could also use the return a week's return after a high currency movement (week movement).

# E.g. you could use the fornmat function above (used to trim out weekends) to calculate returns on a week to week basis (filter Days == "Wednesday" e.g.) and then look at ZAR movement's impact on same week stock movement. Here you won't lag.

# Please guys, let your mind go on this.



# Regression approach --------------------------------------------------------------
Regression_data <-   mergeddataset

zar <- usdzar %>% select("date" , "Return") %>% rename("usdzar_spot" = Return) 

Regression_data <- 
  right_join(Regression_data, zar, by = "date") %>% 
  
  filter(Ticker != "ZAR_Spot") %>% 
  filter(!is.na(Return))
  
# REGRESSION ANALYSIS
# Running multiple regressions 

  #head(Regression_data)


Regressions <- 
  Regression_data %>%
  group_by(Ticker) %>% 
  do(reg = lm(usdzar_spot ~ (Return), data = .)) 

RegressionCoeffs <- 
  Regressions %>% tidy(reg)

head(RegressionCoeffs)

hedges <- RegressionCoeffs %>%  filter(., term == "Return") %>% select(., Ticker, estimate) %>% arrange(., desc(estimate))
#***** ^^ Try put into table ^^ ********


# Tidy output for the paper 

load_pkg("huxtable")

variable.names <- unique(Regression_data$Ticker, incomparables = FALSE) #**** WHAT SHOULD WE INCLUDE HERE? LEEU?  ****

Title <- "Regression Table"


ht <- 
  huxreg(Regressions %>% filter(Ticker %in% variable.names ) %>% 
           select(reg) %>% .[[1]], 
         statistics = c(N = "nobs", R2 = "r.squared"), 
         note = "%stars%." )

#*** This takes a while to run (1-2 mins)
for(i in 1:ncol(ht)) {
  ht[1,][[1+i]] <- variable.names[i]  
}

ht %>% 
  set_caption(Title)



# DCC (flexible specs) ----------------------------------------------------

# Univariate GARCH specifications

# Previously you were not using spread_by explicitly:
rtn <- 
  mergeddataset %>% # Orginally done by Nico with SAData_Returns
  select(date, Ticker, DlogReturn) %>% 
  filter(date >= "2002-01-01") %>% 
  tbl_xts(., spread_by = "Ticker")

# This is key. You cannot run the DCC with NAs...
rtn[is.na(rtn)] <- 0 



uspec <-
  ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
             mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
             distribution.model = "sstd")

multi_univ_garch_spec <- multispec(replicate(ncol(rtn), uspec))

spec.dcc = dccspec(multi_univ_garch_spec, 
                   dccOrder = c(1, 1), 
                   distribution = 'mvnorm',
                   lag.criterion = c("AIC", "HQ", "SC", "FPE")[1],
                   model = c("DCC", "aDCC")[1]) # Change to aDCC e.g.


cl = makePSOCKcluster(10)


# Building DCC model
# This takes a while to run (15 mins)
multf = multifit(multi_univ_garch_spec, rtn, cluster = cl)  

#== === === === === === === === ===  === === === === === === === === === ===
# Not working here:
#== === === === === === === === === === === === === === === === === === ===
# Now we can use multf to estimate the dcc model using our dcc.spec:
fit.dcc = dccfit(spec.dcc, 
                data = rtn, 
                solver = 'solnp', 
                cluster = cl, 
                fit.control = list(eval.se = FALSE), 
                fit = multf)
# Returns the following error:
# Error in -x@fit$log.likelihoods : invalid argument to unary operator
#== === === === === === === === === === === === === === === === === === ===



# We can now test the model's fit as follows:
#   Let's use the covariance matrices to test the adequacy of MV model in fitting mean residual processes:
RcovList <- rcov(fit.dcc) # This is now a list of the monthly covariances of our DCC model series.
covmat = matrix(RcovList,nrow(rtn),ncol(rtn)*ncol(rtn),byrow=TRUE)
mc1 = MCHdiag(rtn,covmat)


# DCC method 1 ------------------------------------------------------------
rtn <- rtn[-1,]
# Center the data:
rtn <- scale(rtn,center=T,scale=F) 

colnames(rtn) <- 
  colnames(rtn) %>% gsub("",".",.)

# And clean it using Boudt's technique:
# takes a while to run (=<5 mins)
rtn <- Return.clean(rtn, method = c("none", "boudt", "geltner")[2], alpha = 0.01) 

#MV Conditional Heteroskedasticity tests
MarchTest(rtn) 

#DCC

DCCPre <- dccPre(rtn/100, include.mean = T, p = 0)

  