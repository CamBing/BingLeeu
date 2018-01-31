Pct_Valid <- 0.9  # This can change of course. 70% valid data over period at least #works with 0.95
StartDate <- ymd(20050101)
EndDate <- ymd(20171031)



# SAReturns ---------------------------------------------------------------

data_original <-
  readRDS("data/SA_Rand_Returns.rds")

SAData_Returns <-   
  data_original %>% 
  filter(Universe == "JALSHAll") %>% 
  ungroup() %>% select(date, Ticker, BICS_LEVEL_1_SECTOR_NAME, Market.Cap, Return) %>% 
  group_by(date) %>% 
  arrange(date, Market.Cap) %>% 
  top_n(N_Capping, "Market.Cap") %>% ungroup()


# ETFs --------------------------------------------------------------------

ETFReturns <-
  etfs %>% group_by(Ticker) %>% 
  rename("TRI" = TOT_RETURN_INDEX_NET_DVDS) %>% 
  mutate(Return = TRI / lag(TRI)-1) %>% ungroup()




# Spots -------------------------------------------------------------------

spots <-
  readRDS("data/Spots.rds") %>% 
  mutate(Days = format(date, "%A")) %>% 
  filter(!Days %in% c("Saturday", "Sunday") ) %>% 
  select(-Days)

usdzar <- 
  spots %>% group_by(Spot) %>% 
  mutate(Return = Value/lag(Value)-1) %>%  
  filter(Spot == "ZAR_Spot") %>% 
  ungroup()


# Merging -----------------------------------------------------------------

# Merging datasets:
mergeddataset <- 
  bind_rows(
    ETFReturns %>% select(date, Ticker, Return),
    SAData_Returns %>% select(date, Ticker, Return),
    usdzar %>% rename("Ticker" = Spot) %>% select(date, Ticker, Return)
  )


# Dlog Returns:
mergeddataset <- mergeddataset %>% arrange(date) %>% group_by(Ticker) %>% 
  mutate(Index = cumprod(1+Return) ) %>% 
  mutate(DlogReturn =  log(Index) - log( lag(Index))) %>% ungroup() %>% 
  select(-Index)

# working -----------------------------------------------------------------

test 


etfs <-
  readRDS("data/AllFunds.rds") %>% tbl_df()



spots <-
  readRDS("data/Spots.rds") %>% 
  mutate(Days = format(date, "%A")) %>% filter(!Days %in% c("Saturday", "Sunday") ) %>% select(-Days)


SAData_Returns <-   
  data_original %>% 
  filter(Universe == "JALSHAll") %>% 
  mutate(Return = coalesce(Return, 0) ) %>%   # To make NA's zero - check whether this fits in to your study / makes sense --> motivate.
  ungroup() %>% select(date, Ticker, BICS_LEVEL_1_SECTOR_NAME, Market.Cap, Return) %>% 
  group_by(date) %>% 
  arrange(date, Market.Cap) %>% 
  top_n(N_Capping, "Market.Cap") %>% ungroup()

# Cleaning data -----------------------------------------------------------



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



# Next thing --------------------------------------------------------------

rankifremoved <- sapply(1:ncol(rtn), function (x) qr(rtn[,-x])$rank)
which(rankifremoved == max(rankifremoved))

library("Smisc")
findDepMat(rtn, rows = FALSE, tol = 1e-10)


# And another -------------------------------------------------------------

# Merging datasets:
mergeddataset <- 
  bind_rows(
    SAData_Returns %>% select(date, Ticker, Return),
    usdzar %>% rename("Ticker" = Spot) %>% select(date, Ticker, Return)
  )


# Dlog Returns:
mergeddataset <- 
  mergeddataset %>% arrange(date) %>% group_by(Ticker) %>% 
  mutate(Return = coalesce(Return, 0)) %>% 
  mutate(Index = cumprod(1+Return) ) %>% 
  mutate(DlogReturn =  log(Index) - log( lag(Index))) %>% ungroup() %>% 
  mutate(DlogReturn = coalesce(DlogReturn, 0)) %>% select(-Index)


colnames(rtn) <- 
  colnames(rtn) %>% gsub(" ",".",.)
