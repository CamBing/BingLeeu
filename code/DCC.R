# Packages ----------------------------------------------------------------
library(rmsfuns)
load_pkg("MTS")
load_pkg(c("devtools", "rugarch", "forecast", "tidyr", 
           "tbl2xts", "lubridate", "readr", "PerformanceAnalytics", 
           "ggplot2", "dplyr", "ggthemes", "robustbase"))


# Data --------------------------------------------------------------------

rtn <- Regression_data

#drop the first observation and corresponding date:
rtn <- rtn[-1,]
# Center the data:
rtn <- scale(rtn,center=T,scale=F) 

colnames(rtn) <- 
  colnames(rtn) %>% gsub("JSE.","",.) %>% gsub(".Close","",.)

# And clean it using Boudt's technique:
rtn <- PerformanceAnalytics::Return.clean(rtn, method = c("none", "boudt", "geltner")[2], alpha = 0.01)

# MV Conditional Heteroskedasticity tests ---------------------------------




## Running the MV Conditional Heteroskedasticity test
MarchTest(rtn)

# *************************************************************************************************
# *************************************************************************************************
# ****************** WE GOT THIS FAR AND GOT STUCK. REST IS COPY & PASTE FROM TUT 7 ***************
# *************************************************************************************************
# *************************************************************************************************
# *************************************************************************************************




# Fitting DCC -------------------------------------------------------------

DCCPre <- dccPre(rtn/100, include.mean = T, p = 0)

# If you want to fit other univariate garch models for each series, use the fGarch package to do so.

names(DCCPre)

# We now have the estimates of volatility for each series. 
# Follow my lead below in changing the output to a usable Xts series for each column in rtn:
Vol <- DCCPre$marVol
colnames(Vol) <- colnames(rtn)
Vol <- 
  data.frame( cbind( date = index(rtn), Vol)) %>% # Add date column which dropped away...
  mutate(date = as.Date(date)) %>%  tbl_df()  # make date column a date column...
TidyVol <- Vol %>% gather(Stocks, Sigma, -date)
ggplot(TidyVol) + geom_line(aes(x = date, y = Sigma, colour = Stocks))


# ------- Back to DCC:

# After saving now the standardized residuals:
StdRes <- DCCPre$sresi
# We can now use these sresids to calculate the DCC model.

# BUT FIRST NOTE THIS:
# Now, here follows a CLASSIC example of bad names for package commands. 
# If you run the dccFit command, notice that it gives you an error for filter_ 

# Upon investigation (Cntrl + click on the word dccFit in Rstudio) - you will notice
# the dccFit function uses the command filter. The problem is, dplyr also uses filter.
# The MTS authors should have wrapped the command as stats::filter and not filter, as it 
# produces ambiguity between stats::filter and dplyr::filter...
# Try it yourself - Run the following code:
# DCC <- dccFit(StdRes, type="Engle")

# SO... to solve this petty issue, let's detach the tidyr and dplyr packages, 
# then run dccFit and then reload tidyr and dplyr...  
# (Note this takes a few minutes - go get coffee in the meantime):

detach("package:tidyr", unload=TRUE)
detach("package:tbl2xts", unload=TRUE)
detach("package:dplyr", unload=TRUE)
DCC <- dccFit(StdRes, type="Engle")

# Let’s plot all the bivariate time-varying correlations with RMH from our DCC model:
Rhot <- DCC$rho.t
# Right, so it gives us all the columns together in the form:
# X1,X1 ; X1,X2 ; X1,X3 ; ....

# So, let's be clever about defining more informative col names. 
# I will create a renaming function below:

renamingdcc <- function(ReturnSeries, DCC.TV.Cor) {
  
  ncolrtn <- ncol(ReturnSeries)
  namesrtn <- colnames(ReturnSeries)
  paste(namesrtn, collapse = "_")
  
  nam <- c()
  xx <- mapply(rep, times = ncolrtn:1, x = namesrtn)
  # Now let's be creative in designing a nested for loop to save the names corresponding to the columns of interest.. 
  
  # TIP: draw what you want to achieve on a paper first. Then apply code.
  
  # See if you can do this on your own first.. Then check vs my solution:
  
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

# Let's see if our function works! Excitement!
Rhot <- 
  renamingdcc(ReturnSeries = rtn, DCC.TV.Cor = Rhot)

head(Rhot)


# DCC: flexible Univariate specs ------------------------------------------

# Using the rugarch package, let's specify our own univariate functions to be used in the dcc process:

# Step 1: Give the specficiations to be used first:

# A) Univariate GARCH specifications:
uspec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)), 
                    mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), 
                    distribution.model = "sstd")
# B) Repeat uspec n times. This specification should be self-explanatory...
multi_univ_garch_spec <- multispec(replicate(ncol(rtn), uspec))

# Right, so now every series will have a GJR Garch univariate specification. (see ?ugarchspec for other options...)

# C) DCC Specs
spec.dcc = dccspec(multi_univ_garch_spec, 
                   dccOrder = c(1, 1), 
                   distribution = 'mvnorm',
                   lag.criterion = c("AIC", "HQ", "SC", "FPE")[1],
                   model = c("DCC", "aDCC")[1]) # Change to aDCC e.g.

# D) Enable clustering for speed:
cl = makePSOCKcluster(10)

# ------------------------
# Step 2:
# The specs are now saved. Let's now build our DCC models...
# ------------------------

# First, fit the univariate series for each column: 
multf = multifit(multi_univ_garch_spec, rtn, cluster = cl)

# Now we can use multf to estimate the dcc model using our dcc.spec:
fit.dcc = dccfit(spec.dcc, 
                 data = rtn, 
                 solver = 'solnp', 
                 cluster = cl, 
                 fit.control = list(eval.se = FALSE), 
                 fit = multf)

# And that is our DCC fitted model!

# We can now test the model's fit as follows:
#   Let's use the covariance matrices to test the adequacy of MV model in fitting mean residual processes:
RcovList <- rcov(fit.dcc) # This is now a list of the monthly covariances of our DCC model series.
covmat = matrix(RcovList,nrow(rtn),ncol(rtn)*ncol(rtn),byrow=TRUE)
mc1 = MCHdiag(rtn,covmat)

# ....Check Tsay (2014 on the interpretation of these Portmanteau tests)....

# Now to save the time-varying correlations as specified by the DCC model, 
# it again requires some gymnastics from our side.
# First consider what the list looks like:
dcc.time.var.cor <- rcor(fit.dcc)
print(dcc.time.var.cor[,,1:3])


# Okay, so this format presents a particular challenge in 
# getting our time-varying series into a plottable format...
# Every date is a list, and the list a matrix... How to get it in
# a tidy and plottable format?!!

# If you're up for the challenge - see if you can solve this 
# little conundrum of getting the lists as bivariate pair columns before using my solution.

# -------------SOLUTION:--------------------

# We will first use the base R function aperm - which transposes any array 
# into a list by changing the dimensions
dcc.time.var.cor <- aperm(dcc.time.var.cor,c(3,2,1))
dim(dcc.time.var.cor) <- c(nrow(dcc.time.var.cor), ncol(dcc.time.var.cor)^2)

# And now we can rename our columns the same way as before. 
# Luckily we wrote a function so we can use it again...

dcc.time.var.cor <-
  renamingdcc(ReturnSeries = rtn, DCC.TV.Cor = dcc.time.var.cor)

# Note that the figure is very similar to our earlier DCC TV correlations.
# So having a GJR univariate model didn't exactly change much...

g1 <- 
  ggplot(dcc.time.var.cor %>% filter(grepl("SBK_", Pairs ), !grepl("_SBK", Pairs)) ) + 
  geom_line(aes(x = date, y = Rho, colour = Pairs)) + 
  theme_hc() +
  ggtitle("Dynamic Conditional Correlations: SBK")

print(g1)


