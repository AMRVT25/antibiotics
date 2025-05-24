# This program does ARDL analysis inclduing cross price elasiticty on
# Ethacilin of different pack size. 

rm(list = ls())  # Clear memory
cat("\f") # Clear console
options(scipen=1) 

library(ARDL)
library(sqldf)
library(tseries)
library(writexl)
library(beepr)

setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory
source("070_merge_sales_data_with_control_data_util.R")



# Create a dataframe to store the results from all the models. 
result_cols=c("ProductName","PackageSize","Term",
              "Estimate","Std. Error","Pr(>|t|)","Significance",
              "Observations","Adf_Q","Adf_P", "lagorder" ,
              "Bounds t-test p-value","Bounds f-test p-value",
              "LR Estimate","LR Std. Error","LR Pr(>|t|)","LR Significance")
df_results= data.frame(matrix(nrow = 0,  ncol = length(result_cols)))
colnames(df_results) = result_cols
row=0

getSalesAndControlMergedDataFrame = function(product, size) {
  print(paste0("Processing ", product, " ", size))
  df_antibiotics=read.csv("data/antibiotics_sales_data_cleaned.csv")
  df_sales = sqldf(paste0("select * from df_antibiotics where ProductName='",product,"' and PackageSize='",size,"'"))
  name = paste(product, size)
  name = gsub("/"," per ",name)
  name = gsub(" ","_",name)
  df = merge_sales_and_control_data(df_sales,name)
  #df = merge_sales_and_control_data_yearly(df_sales,name)
  print(paste("Length of time series: ", nrow(df)))
  df$Date = as.Date(df$Date)
  #df$Date = as.Date(paste0(df$Year, "-01-01"))
  return( df ) 
  
}

small = getSalesAndControlMergedDataFrame("Ethacilin vet., inj-vätska, suspension 300 mg/ml", "100 milliliter")
large = getSalesAndControlMergedDataFrame("Ethacilin vet., inj-vätska, suspension 300 mg/ml", "12 x 250 milliliter")

small$ln_crossprice = small$ln_AverageAntibioticsRealPricePerKg


df_cross = sqldf("select large.ln_TotalKgActiveSubstanceSold,
                 large.ln_AverageAntibioticsRealPricePerKg,
                 large.ln_headCount,
                 large.ln_RealAvrakningsprisKrPerKg,
                 small.ln_crossprice 
                 FROM large LEFT JOIN small
                 ON small.date = large.Date")
df_cross = na.omit(df_cross)

models <- auto_ardl(ln_TotalKgActiveSubstanceSold ~ ln_AverageAntibioticsRealPricePerKg + 
                      ln_headCount + ln_RealAvrakningsprisKrPerKg + ln_crossprice
                    ,data = df_cross,  max_order = 2, selection = "AIC")
lags=models$top_orders[1, -ncol(models$top_orders) ]
str_lags <- paste0("(", paste(lags, collapse = ", "), ")")
str_lags
ecm_model <- uecm(models$best_model, case = 3)
bounds_f_test(ecm_model, case=3)$tab[2]
bounds_t_test(ecm_model, case=3)$tab[2]
multipliers(ecm_model, type = "lr") 
multipliers(ecm_model, type = "sr") 

# Results are all insignificant. 

###########################################################################################
# Cross price effects between Ethacilin and Penovet 100 ml

df_cross=NULL
ethac = getSalesAndControlMergedDataFrame("Ethacilin vet., inj-vätska, suspension 300 mg/ml", "100 milliliter")
penov = getSalesAndControlMergedDataFrame("Penovet vet., inj-vätska, suspension 300 mg/ml", "100 milliliter")


ethac$ln_crossprice = ethac$ln_AverageAntibioticsRealPricePerKg


df_cross = sqldf("select 
                    penov.ln_TotalKgActiveSubstanceSold,
                    penov.ln_AverageAntibioticsRealPricePerKg,
                    penov.ln_headCount,
                    penov.ln_RealAvrakningsprisKrPerKg,
                    ethac.ln_crossprice 
                 FROM 
                    penov LEFT JOIN ethac
                 ON 
                    ethac.date = penov.Date")
df_cross = na.omit(df_cross)
nrow(df_cross)
models <- auto_ardl(ln_TotalKgActiveSubstanceSold ~ ln_AverageAntibioticsRealPricePerKg + 
                      ln_headCount + ln_RealAvrakningsprisKrPerKg + ln_crossprice
                    ,data = df_cross,  max_order = 2, selection = "AIC")
lags=models$top_orders[1, -ncol(models$top_orders) ]
str_lags <- paste0("(", paste(lags, collapse = ", "), ")")
str_lags
ecm_model <- uecm(models$best_model, case = 3)
bounds_f_test(ecm_model, case=3)$tab[2]
bounds_t_test(ecm_model, case=3)$tab[2]
multipliers(ecm_model, type = "lr") 
multipliers(ecm_model, type = "sr") 

# Here we find a long run relationship! 
# The price elasticity is very negativ and significant
#                               Term    Estimate Std. Error    t value     Pr(>|t|)
# ln_AverageAntibioticsRealPricePerKg  -6.5451239  1.8287172 -3.5790793 0.0004234032
# And the cross price elasiticty is positive and signifiant
#         Term    Estimate Std. Error    t value     Pr(>|t|)
# ln_crossprice   6.2073397  2.7603288  2.2487682 0.0255090317
# This indicate that a increase in the price of Ethacilin will increase the
# sales of Penovet. 


###########################################################################################
# Cross price effects between Ethacilin and Penovet 12x100 ml

df_cross=NULL
ethac = getSalesAndControlMergedDataFrame("Ethacilin vet., inj-vätska, suspension 300 mg/ml", "250 milliliter")
penov = getSalesAndControlMergedDataFrame("Penovet vet., inj-vätska, suspension 300 mg/ml", "250 milliliter")


ethac$ln_crossprice = ethac$ln_AverageAntibioticsRealPricePerKg


df_cross = sqldf("select 
                    penov.ln_TotalKgActiveSubstanceSold,
                    penov.ln_AverageAntibioticsRealPricePerKg,
                    penov.ln_headCount,
                    penov.ln_RealAvrakningsprisKrPerKg,
                    ethac.ln_crossprice 
                 FROM 
                    penov LEFT JOIN ethac
                 ON 
                    ethac.date = penov.Date")
df_cross = na.omit(df_cross)
nrow(df_cross)
models <- auto_ardl(ln_TotalKgActiveSubstanceSold ~ ln_AverageAntibioticsRealPricePerKg + 
                      ln_headCount + ln_RealAvrakningsprisKrPerKg + ln_crossprice
                    ,data = df_cross,  max_order = 2, selection = "AIC")
lags=models$top_orders[1, -ncol(models$top_orders) ]
str_lags <- paste0("(", paste(lags, collapse = ", "), ")")
str_lags
ecm_model <- uecm(models$best_model, case = 3)
bounds_f_test(ecm_model, case=3)$tab[2]
bounds_t_test(ecm_model, case=3)$tab[2]
multipliers(ecm_model, type = "lr") 
multipliers(ecm_model, type = "sr") 

# Here we find a long run relationship too! 
# But no sigificant cross price effects
# There is only 205 observations. So this dataset has many missing values. 

print("done")



