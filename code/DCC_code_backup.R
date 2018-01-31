# Univariate GARCH specifications

# Only hold stocks with a minimum amount of valid data:

Pct_Valid <- 0.9 # This can change of course. 70% valid data over period at least
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
# This takes a while to run (5 mins)
multf = multifit(multi_univ_garch_spec, rtn, cluster = cl)  

# Now we can use multf to estimate the dcc model using our dcc.spec:
fit.dcc = dccfit(spec.dcc, 
                 data = rtn, 
                 solver = 'solnp', 
                 cluster = cl, 
                 fit.control = list(eval.se = FALSE), 
                 fit = multf,
                 tol=1e-10) # tol included to avoid df being considered computationally singular


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

# Renaming function ====
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

#end --- ---- --- --- --- --- --- ---

dcc.time.var.cor <-
  renamingdcc(ReturnSeries = rtn, DCC.TV.Cor = dcc.time.var.cor)

all.correl <- 
  ggplot(dcc.time.var.cor %>% filter(grepl("ZAR_Spot_", Pairs ), !grepl("_ZAR_Spot", Pairs)) ) + 
  geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
  theme_hc() +
  ggtitle("Dynamic Conditional Correlations: ZAR")

print(all.correl)  # Prints all correlations