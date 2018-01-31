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
library(ggplot2)
library(ggthemes)
# Loading data ------------------------------------------------------------
etfs <-
  readRDS("data/AllFunds.rds") %>% tbl_df()

data_original <-
  readRDS("data/SA_Rand_Returns.rds")

spots <-
  readRDS("data/Spots.rds") %>% 
  mutate(Days = format(date, "%A")) %>% filter(!Days %in% c("Saturday", "Sunday") ) %>% select(-Days)

# Merging and Calculating returns -----------------------------------------------------


N_Capping <- 80 # Parameter that trims the universe set. Focus, e.g., on the top 80 stocks by Market Cap.

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
Regression_data <-   mergeddataset %>% select("date", "Ticker", "DlogReturn")


#zar <- usdzar %>% select("date" , "Return") %>% rename("usdzar_spot" = Return) 

zar <- mergeddataset %>% 
  filter(Ticker == "ZAR_Spot")%>%
  select("date", "DlogReturn")  %>% 
  rename("usdzar_spot" = DlogReturn)

Regression_data <- 
  right_join(Regression_data, zar, by = "date") %>% 
  filter(Ticker != "ZAR_Spot") %>% 
  filter(!is.na(DlogReturn))

#== === === === === === === === === === === === === === ===
# TRIM THE REGRESSION DATA!!
# Do this now or after it has been run for the DCC
#== === === === === === === === === === === === === === ===
Pct_Valid <- 0.9  # This can change of course. 70% valid data over period at least
StartDate <- ymd(20050101)
EndDate <- ymd(20171031)

mergeddataset <- 
  mergeddataset %>% filter(date >= StartDate & date <= EndDate) #trimming dates covered

mergeddataset[is.na(mergeddataset)] <- 0 # (Added) replace NAs in mergeddata

NDates <- length(unique(mergeddataset %>% pull(date)) ) 

Tickers_Active_At_Last <- 
  mergeddataset %>% 
  select(date, Ticker, DlogReturn) %>% 
  filter(date >= ymd(20171001)) %>% # checking if valid at most recent date 
  group_by(Ticker) %>% 
  mutate(N_Valid = ifelse(DlogReturn == 0, 0, 1) ) %>% 
  summarise(S = sum(N_Valid)) %>% 
  filter(S >0) %>% pull(Ticker)

Tickers_To_Hold <- 
  mergeddataset %>% 
  select(date, Ticker, DlogReturn) %>% 
  filter(Ticker %in% Tickers_Active_At_Last) %>% 
  group_by(Ticker) %>% 
  mutate(N_Valid = ifelse(DlogReturn == 0, 0, 1) ) %>% summarise(N_Valid_Pct = sum(N_Valid)/NDates) %>% 
  filter(N_Valid_Pct >= Pct_Valid) %>% pull(Ticker) %>% unique()

rtn <- 
  mergeddataset %>% 
  select(date, Ticker, DlogReturn) %>% 
  filter(date >= StartDate) %>% 
  filter(Ticker %in% Tickers_To_Hold) %>% 
  tbl_xts(., spread_by = "Ticker")

regression.data.final <-  mergeddataset %>% 
  select(date, Ticker, DlogReturn) %>% 
  filter(date >= StartDate) %>% 
  filter(Ticker %in% Tickers_To_Hold) 

regression.data.final[is.na(regression.data.final)] <- 0 

regression.data.final <- 
  right_join(regression.data.final, zar, by = "date") %>% 
  filter(Ticker != "ZAR_Spot") %>% 
  filter(!is.na(DlogReturn))

#== === === === === === === === === ===
# REGRESSION ANALYSIS (NEW)
#== === === === === === === === === ===

Regressions <- 
  regression.data.final %>%
  group_by(Ticker) %>% 
  do(reg = lm(usdzar_spot ~ (DlogReturn), data = .)) 

RegressionCoeffs <- 
  Regressions %>% tidy(reg)

head(RegressionCoeffs)

hedges <- RegressionCoeffs %>%  
  filter(., term == "DlogReturn") %>% 
  select(., Ticker, estimate) %>% 
  arrange(., desc(estimate))
#***** ^^ Try put into table ^^ ********

top.10.reg <-
  (RegressionCoeffs %>%  
     filter(., term == "DlogReturn") %>% 
     select(., Ticker, estimate) %>% 
     arrange(., desc(estimate)))[1:10,]

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


# Original Regression analysis --------------------------------------------

#======= === === === === === === === ===
# ORIGINAL!!!
#== === === === === === === === === ===
# REGRESSION ANALYSIS
# Running multiple regressions 

  #head(Regression_data)


Regressions <- 
  Regression_data %>%
  group_by(Ticker) %>% 
  do(reg = lm(usdzar_spot ~ (DlogReturn), data = .)) 

RegressionCoeffs <- 
  Regressions %>% tidy(reg)

head(RegressionCoeffs)

hedges <- RegressionCoeffs %>%  
  filter(., term == "DlogReturn") %>% 
  select(., Ticker, estimate) %>% 
  arrange(., desc(estimate))
#***** ^^ Try put into table ^^ ********

top.10.reg <-
  (RegressionCoeffs %>%  
     filter(., term == "DlogReturn") %>% 
     select(., Ticker, estimate) %>% 
     arrange(., desc(estimate)))[1:10,]

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

# Only hold stocks with a minimum amount of valid data:

Pct_Valid <- 0.9  # This can change of course. 70% valid data over period at least #works with 0.95
StartDate <- ymd(20050101)
EndDate <- ymd(20171031)

mergeddataset <- 
  mergeddataset %>% filter(date >= StartDate & date <= EndDate)   #trimming dates covered
  
  
mergeddataset[is.na(mergeddataset)] <- 0 # (Added) replace NAs in mergeddata

NDates <- length(unique(mergeddataset %>% pull(date)) ) 

Tickers_Active_At_Last <- 
  mergeddataset %>% 
  select(date, Ticker, DlogReturn) %>% 
  filter(date >= ymd(20171001)) %>% # checking if valid at most recent date 
  group_by(Ticker) %>% 
  mutate(N_Valid = ifelse(DlogReturn == 0, 0, 1) ) %>% 
  summarise(S = sum(N_Valid)) %>% 
  filter(S >0) %>% pull(Ticker)

Tickers_To_Hold <- 
  mergeddataset %>% 
  select(date, Ticker, DlogReturn) %>% 
  filter(Ticker %in% Tickers_Active_At_Last) %>% 
  group_by(Ticker) %>% 
  mutate(N_Valid = ifelse(DlogReturn == 0, 0, 1) ) %>% summarise(N_Valid_Pct = sum(N_Valid)/NDates) %>% 
  filter(N_Valid_Pct >= Pct_Valid) %>% pull(Ticker) %>% unique()


rtn <- 
  mergeddataset %>% 
  select(date, Ticker, DlogReturn) %>% 
  filter(date >= StartDate) %>% 
  filter(Ticker %in% Tickers_To_Hold) %>% 
  tbl_xts(., spread_by = "Ticker")


# This is key. You cannot run the DCC with NAs...
rtn[is.na(rtn)] <- 0 

rtn <- rtn*100  #ADDED BY CAM

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
# This takes a while to run (5 mins)
multf = multifit(multi_univ_garch_spec, rtn, cluster = cl)  

# Now we can use multf to estimate the dcc model using our dcc.spec:
fit.dcc = dccfit(spec.dcc, 
                 data = rtn, 
                 solver = 'solnp', 
                 cluster = cl, 
                 fit.control = list(eval.se = FALSE), 
                 fit = multf,
                 tol = 3.02592e-21) # tol included to avoid df being considered computationally singular # 1e-10 , tol= 1.17639e-16


# We can now test the model's fit as follows:
#   Let's use the covariance matrices to test the adequacy of MV model in fitting mean residual processes:
RcovList <- rcov(fit.dcc) # This is now a list of the monthly covariances of our DCC model series.
covmat = matrix(RcovList,nrow(rtn),ncol(rtn)*ncol(rtn),byrow=TRUE)
mc1 = MCHdiag(rtn,covmat) # NEED TO INTERPRET RESULTS OF TEST

dcc.time.var.cor <- rcor(fit.dcc)
print(dcc.time.var.cor)

# Plotting
dcc.time.var.cor <- aperm(dcc.time.var.cor,c(3,2,1))
dim(dcc.time.var.cor) <- c(nrow(dcc.time.var.cor), ncol(dcc.time.var.cor)^2)

# Renaming function ==
renamingdcc <- function(ReturnSeries, DCC.TV.Cor) {
  
  ncolrtn <- ncol(ReturnSeries)
  namesrtn <- colnames(ReturnSeries)
  paste(namesrtn, collapse = "_")
  
  nam <- c()
  xx <- mapply(rep, times = ncolrtn:1, x = namesrtn)
    nam <- c()
  for (j in 1:(ncolrtn)) {
    for (i in 1:(ncolrtn)) {
      nam[(i + (j-1)*(ncolrtn))] <- paste(xx[[j]][1], xx[[i]][1], sep="_")
    }
  }
  
  colnames(DCC.TV.Cor) <- nam
  
  # So to plot all the time-varying correlations wrt SBK:
  # First append the date column that has (again) been removed...
  DCC.TV.Cor <- 
    data.frame( cbind( date = index(ReturnSeries), DCC.TV.Cor)) %>% # Add date column which dropped away...
    mutate(date = as.Date(date)) %>%  tbl_df() 
  
  DCC.TV.Cor <- DCC.TV.Cor %>% gather(Pairs, Rho, -date)
  
  DCC.TV.Cor
  
}

#end



dcc.time.var.cor <-
  renamingdcc(ReturnSeries = rtn, DCC.TV.Cor = dcc.time.var.cor)

all.correl <- 
  ggplot(dcc.time.var.cor %>% filter(grepl("ZAR_Spot_", Pairs ), !grepl("_ZAR_Spot", Pairs)) ) + 
  geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
  theme_hc() +
  ggtitle("Dynamic Conditional Correlations: ZAR")

print(all.correl)  # Prints all correlations in graph

# Print Top ten hedges

top.10.reg.list <- top.10.reg$Ticker %>% gsub(" ", ".", .) %>% paste0('ZAR_Spot_',.)


zarDCC <- dcc.time.var.cor %>% filter(grepl("ZAR_Spot_", Pairs ), !grepl("_ZAR_Spot", Pairs)) 
top.10.graph <- zarDCC %>% filter(Pairs %in% c(top.10.reg.list)) %>% 
  ggplot( ) + 
  geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
  theme_hc() +
  ggtitle("Top 10 Dynamic Conditional Correlations: ZAR")

print(top.10.graph)




# working for plotting ----------------------------------------------------

top.10.reg.list <- top.10.reg
variable.names(top.10.reg.list) <- variable.names(top.10.reg.list) %>% gsub(" ",".",.) %>% gsub("Equity","Equity_ZAR_Spot",.)

top.10.reg.list <- top.10.reg.list %>% gsub(" ",".",.) %>% gsub("Equity","Equity_ZAR_Spot",.)


top.10.reg.list <- top.10.reg %>% gsub(" ",".",.) %>% gsub("Equity","Equity_ZAR_Spot",.)

top.10 <- (dcc.time.var.cor %>% filter(grepl("ZAR_Spot_", Pairs ), !grepl("_ZAR_Spot", Pairs)) %>% filter(Pairs %in% top.10.reg.list))

top.10.reg.list <- top.10.reg$Ticker
top.10.reg.list %>% gsub(" ",".",.) %>% gsub("Equity","Equity_ZAR_Spot",.)


top.10.reg.list <- top.10.reg
test <- gsub(" ",".", top.10.reg.list$Ticker)

top.10 <- (dcc.time.var.cor %>% filter(grepl("ZAR_Spot_", Pairs ), !grepl("_ZAR_Spot", Pairs)) %>% filter(Pairs == top.10.reg.list))


top.10.reg.list <- c(top.10.reg$Ticker) 
top.10.reg.list <- gsub(" ", ".", top.10.reg.list)
top.10.reg.list <- variable.names(top.10.reg.list, prefix = "ZAR_Spot_")

