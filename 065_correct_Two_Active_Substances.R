# This is a utility program that takes care of the double rows for 
# products Streptocillin and Hippotrim that contain 2 active substances. For exampe: 
# mysql> select productname, price, substanse, ActiveSubstanceVolume, date from sales where productname ="Hippotrim vet., inj-vätska, lösning 200 mg/ml + 40 mg/ml" limit 2;
# +----------------------------------------------------------+---------+-------------+-----------------------+------------+
# | productname                                              | price   | substanse   | ActiveSubstanceVolume | date       |
# +----------------------------------------------------------+---------+-------------+-----------------------+------------+
# | Hippotrim vet., inj-vätska, lösning 200 mg/ml + 40 mg/ml | 12112.5 | sulfadiazin |                  1900 | 2005-01-01 |
# | Hippotrim vet., inj-vätska, lösning 200 mg/ml + 40 mg/ml | 12112.5 | trimetoprim |                   380 | 2005-01-01 |
# +----------------------------------------------------------+---------+-------------+-----------------------+------------+
# This program changes this so that it becomes just 1 row, in which substanse is 
# sulfadiazin, trimetoprim and ActiveSubstanceVolume is summed up to 2280. 
# Also KgActiveSubstance is summed up and RealPricePerKgActiveSubstance is recalculated


# Don't Clear memory in a util file
#rm(list = ls())  # Clear memory
#cat("\f") # Clear console
library(sqldf)
library(writexl)

setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory


correctTwoActiveSubstances=function(df) {
  df_original=df
  for (row in 1:nrow(df)-1) {
    # Merge Hippotrim
    if (df[row, "ProductName"] == "Hippotrim vet., inj-vätska, lösning 200 mg/ml + 40 mg/ml" &&
        df[row, "ProductName"] == df[row+1, "ProductName"] &&
        df[row, "Date"] == df[row+1, "Date"] &&
        df[row, "Price"] == df[row+1, "Price"] &&
        df[row, "PackType"] == df[row+1, "PackType"] && 
        df[row, "UnitsSold"] == df[row+1, "UnitsSold"] && 
        df[row, "Substanse"] == "sulfadiazin" && 
        df[row+1, "Substanse"] == "trimetoprim") {
      df[row, "ActiveSubstanceVolume"] = df[row, "ActiveSubstanceVolume"]+df[row+1, "ActiveSubstanceVolume"]
      df[row, "KgActiveSubstance"] = df[row, "KgActiveSubstance"]+df[row+1, "KgActiveSubstance"]
      df[row, "RealPricePerKgActiveSubstance"] = df[row, "RealPrice"] / df[row, "KgActiveSubstance"]
      df[row, "Substanse"] = "sulfadiazin, trimetoprim"
      df[row+1, ] <- NA
    }
    # Merge Streptocillin 
    if (df[row, "ProductName"] == "Streptocillin vet., inj-vätska, suspension 250 mg/ml + 200 mg/ml" &&
        df[row, "ProductName"] == df[row+1, "ProductName"] &&
        df[row, "Date"] == df[row+1, "Date"] &&
        df[row, "Price"] == df[row+1, "Price"] &&
        df[row, "PackType"] == df[row+1, "PackType"] && 
        df[row, "UnitsSold"] == df[row+1, "UnitsSold"] && 
        df[row, "Substanse"] == "bensylpenicillinprokainmonohydrat" && 
        df[row+1, "Substanse"] == "dihydrostreptomycinsulfat") {
      df[row, "ActiveSubstanceVolume"] = df[row, "ActiveSubstanceVolume"]+df[row+1, "ActiveSubstanceVolume"]
      df[row, "KgActiveSubstance"] = df[row, "KgActiveSubstance"]+df[row+1, "KgActiveSubstance"]
      df[row, "RealPricePerKgActiveSubstance"] = df[row, "RealPrice"] / df[row, "KgActiveSubstance"]
      df[row, "Substanse"] = "bensylpenicillinprokainmonohydrat, dihydrostreptomycinsulfat"
      df[row+1, ] <- NA # Remove the second line for this purchase. 
    }
  }
  df=  na.omit(df) # Clean out the second lines
  
  write.csv(df, "data/antibiotics_sales_data_cleaned_two_active_substances_fixed.csv", row.names = FALSE)
  write_xlsx(df, "data/antibiotics_sales_data_cleaned_two_active_substances_fixed.xlsx")
  
  # Veryfy Hippotrim
  c_original=sqldf("Select count(1) from df_original where ProductName='Hippotrim vet., inj-vätska, lösning 200 mg/ml + 40 mg/ml'")[[1]]
  c=sqldf("Select count(1) from df where ProductName='Hippotrim vet., inj-vätska, lösning 200 mg/ml + 40 mg/ml'")[[1]]
  c_original/c # SHoul show exactly 2 !!
  
  # Make sure that there is no single substanse Hippotrim left, 
  # Should say 0 !
  sqldf("select count(1) from df where ProductName='Hippotrim vet., inj-vätska, lösning 200 mg/ml + 40 mg/ml' and substanse != 'sulfadiazin, trimetoprim' ")[[1]]
  
  # Veryfy Streptocillin
  c_original=sqldf("Select count(1) from df_original where ProductName='Streptocillin vet., inj-vätska, suspension 250 mg/ml + 200 mg/ml'")[[1]]
  c=sqldf("Select count(1) from df where ProductName='Streptocillin vet., inj-vätska, suspension 250 mg/ml + 200 mg/ml'")[[1]]
  c_original/c # Should show exactly 2 !!
  
  # Make sure that there is no single substanse Streptocillin left, 
  # Should say 0 !
  sqldf("select count(1) from df where ProductName='Streptocillin vet., inj-vätska, suspension 250 mg/ml + 200 mg/ml' and substanse != 'bensylpenicillinprokainmonohydrat, dihydrostreptomycinsulfat' ")[[1]]
  return(df)
}

# For testing:
#df_original=read.csv("data/antibiotics_sales_data_cleaned.csv")
#df_corrected=correctTwoActiveSubstances(df_original)