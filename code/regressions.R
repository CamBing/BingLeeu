
# First off... -------------------------------------------------------------
#   Need to run 'code/data_prep.R' to obtain the mergeddataset

Regression_data <-   mergeddataset

# Packages ----------------------------------------------------------------
library(rmsfuns)
packages_reg <- c("broom")
load_pkg(packages_reg)


# Data prep ---------------------------------------------------------------

zar <- usdzar %>% select("date" , "Return") %>% rename("usdzar_spot" = Return) 

Regression_data <- 
  right_join(Regression_data, zar, by = "date")



# REGRESSION ANALYSIS
# Running multiple regressions --------------------------------------------

head(Regression_data)


Regressions <- 
  Regression_data %>%
  group_by(Ticker) %>% 
  do(reg = lm(usdzar_spot ~ (Return), data = .)) ##*THIS IS WHAT MY QUESTION REFERS TO*## 

RegressionCoeffs <- 
  Regressions %>% tidy(reg)

head(RegressionCoeffs)


# Tidy output for the paper -----------------------------------------------

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



