# This program does an ARDL analysis. This analysis is just to confirm that
# R and Stata produces the same output. These results are not used in the 
# thesis, it is just a verification that R giving the same results as Stata

rm(list = ls())  # Clear memory
cat("\f") # Clear console
options(scipen=1) 

library(ARDL)
library(sqldf)

setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory
df=read.csv("data/merged_complete_data_of_antibiotics_Ethacilin_shortNames.csv")

# keep if date > date("01jan2009", "DMY")
# df=sqldf("select * from df where date >= '2009-01-01' ")
df$Date = as.Date(df$Date)

names (df)
df$ln_pigs = log(df$pigs)
df$ln_pork_p = log(df$pork_p)

models <- auto_ardl(ln_Q ~ ln_p +  ln_pigs + ln_pork_p 
                    ,data = df,  max_order = 24, selection = "AIC")
models$top_orders[1, ] 
AIC(models$best_model)
BIC(models$best_model)
summary(models$best_model)

ecm_model <- uecm(models$best_model, case = 3)
summary(ecm_model)
ecm_coefs <- coef(summary(ecm_model))
speed_of_adjustment=ecm_coefs["L(ln_Q, 1)", "Estimate"]
if (speed_of_adjustment < (-2) | speed_of_adjustment > 0) {
  print(paste("WARNING: speed_of_adjustment is out of range! ", speed_of_adjustment))
} else {
  print(paste(" speed_of_adjustment is OK", speed_of_adjustment))
}

# Some verification of the speed of adjustment
recm_model <- recm(models$best_model, case = 3)
recm_model$coefficients[["ect"]]

# Compute SHORT RUN multipliers from the ECM model
multipliers(ecm_model, type = "sr") 
# Compute LONG RUN multipliers from the ECM model
multipliers(ecm_model, type = "lr") 

r_ecm_model <- recm(ecm_model, case=3)
summary(r_ecm_model) # This will show the ect on a row in the eoutput like this: 
#                   Estimate Std. Error t value Pr(>|t|) 
# ect               -0.05455    0.03188  -1.711 0.088501 .
# The value, but not the standard error, t-value and p-value, is the same as 
# in the output from summary(ecm_model) for the line L(ln_Q, 1) 
# L(ln_Q, 1)        -0.05455    0.04814  -1.133  0.25838 
#
# The L(ln_Q, 1) line in R is the same as the output from Stata: 
# ------------------------------------------------------------------------------
#       D.ln_q | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
# -------------+----------------------------------------------------------------
# ADJ          |
#         ln_q |
#          L1. |  -.0545463   .0481377    -1.13   0.258    -.1494139    .0403214
# -------------+----------------------------------------------------------------
# The value -0.05455 is the speed of adjustment. 


bounds_f_test(ecm_model, case = 3)
bounds_f_test(ecm_model, case = 3)$tab # A more clear view  of just the table
bounds_t_test(ecm_model, case = 3, alpha = 0.05)
bounds_t_test(ecm_model, case = 3, alpha = 0.05)$tab # A more clear view of just the table
# Neither the f nore the t bounds test can reject the null hypothesis of no cointegration
# since both the p-value of the f and t statistic is > 0.05. Thus, there is no
# long run relationship between quantity and price. 
# The F and t statistic is the same as in Stata. 

# Pesaran, Shin, and Smith (2001) bounds test
# 
# H0: no level relationship      F =     0.722
# Case 3                         t =    -1.133
# 

# The only difference is that the optimal lag selection is different in Stata as comapred
# to R. In the calculations above the optimal lags selected by R is used. 

# Check the stata Optimal lag order
stata_model <- ardl(ln_Q ~ ln_p + ln_pigs + ln_pork_p, data = df, order = c(20,2,23,8))
AIC(stata_model) # Outputs -216.6707
models$top_orders # Best model is -267.4369, second best is -267.2453. So it selects the
# model with the highest AIC. Stata AIC = 216.6707 is clearly worse. 
# Probably the AIC algorithm is different in Stata and R
