
# Packages ----------------------------------------------------------------
library(rmsfuns)
packages_reg <- c("broom")
load_pkg(packages_reg)

# REGRESSION ANALYSIS
# Running multiple regressions --------------------------------------------

## From tut 4: application

colnames(Regression_data) <- gsub(" ", ".", colnames(Regression_data)) # regression code does not like spaces in colnames

Regressions <- 
  Regression_data %>%
  group_by(Spots) %>% 
  do(reg = lm(Returns ~ lag(Returns) + Dim.1 + Dim.2 + Dim.3 + Supp_VIX.Index, data = .)) 

# And that's it.
# Now all the regressions are in their own list according to each Spot
# And now we use broom to tidy up our results...

RegressionCoeffs <- 
  Regressions %>% tidy(reg)

head(RegressionCoeffs)



