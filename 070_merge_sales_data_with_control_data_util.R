# This is not a stand alone program. It is supposed to be sourced into 
# other programs that can call the two utility function speficied here. 

# This program contains the functionality to merge antibiotics sales data
# with other datasets that contains data that is later used as control variables
# in the ARDL-analysis. 

# Don't Clear memory in a util file 
#rm(list = ls())  # Clear memory
#cat("\f") # Clear console
#options(scipen=999) # Avoid scientific notation of numbers. 

setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory

library(sqldf)
library(writexl)
library(dplyr)



merge_sales_and_control_data = function(df_antibiotics, label) {
  
  df_avrakning=read.csv("data/avrakningspris_kr_per_100kg_swine.csv")
  df_total_monthly_pigs_count=read.csv("data/total_monthly_pigs_count.csv")
  df_total_monthly_company_count=read.csv("data/total_monthly_company_count.csv")
  months = unique (df_antibiotics$Date)
  length(months)
  # Calculate the Weighted average sales price for each month. 
  columns = c("Date", "TotalKgActiveSubstanceSold", "AverageAntibioticsRealPricePerKg")
  df_antibiotics_wasp = NULL
  df_antibiotics_wasp = data.frame(matrix(ncol = length(columns), nrow = length(months)))
  colnames(df_antibiotics_wasp) = columns
  for (row in 1 : length(months)) {
    # row=1
    #print(paste("row=",row))
    
    m=months[row]
    df_antibiotics_current_month=sqldf(paste0("select * from df_antibiotics where Date = '", m ,"'"))
    #write_xlsx(df, "data/df_antibiotics_current_month.xlsx")
    #write.csv(df,  "data/df_antibiotics_current_month.csv", row.names = FALSE)
    
    #print(paste0("m=",m, " rows in current month=",nrow(df_antibiotics_current_month)))
    df_antibiotics_wasp[row, "Date"]=m
    df_antibiotics_wasp[row,"TotalKgActiveSubstanceSold"]=sum(df_antibiotics_current_month$KgActiveSubstance)
    # Calculate the average price of Antibiotics sold. 
    df_antibiotics_wasp[row, "AverageAntibioticsRealPricePerKg"]=
      sum(df_antibiotics_current_month$RealPrice)/
      df_antibiotics_wasp[row,"TotalKgActiveSubstanceSold"]
    
  }
  range(df_antibiotics_wasp$AverageAntibioticsRealPricePerKg)
  df_antibiotics_wasp$Date = as.Date(df_antibiotics_wasp$Date)
  # Make a plot to check the results
  # Plot the time series
  # Plot without x-axis
  # imgWidth = 1600
  # imgHeight = 1000
  # png(paste0("plots/average_real_sales_price_",label,".png"), width = imgWidth, height = imgHeight, res = 150)
  # plot(df_antibiotics_wasp$Date, df_antibiotics_wasp$AverageAntibioticsRealPricePerKg, type = "l", col = "steelblue",
  #      main = paste0("Antibiotics (",label,") average real sales price per kg active substance, in year 2025 prices"), lwd=4,
  #      xlab = "Year", ylab = "Real Price / Kg Active Substance", xaxt = "n")  # Suppress default x-axis
  # 
  # # Generate yearly ticks
  # year_ticks <- seq(from = min(df_antibiotics_wasp$Date), 
  #                   to = max(df_antibiotics_wasp$Date), 
  #                   by = "year")
  # 
  # # Add custom x-axis with yearly labels
  # axis(1, at = year_ticks, labels = format(year_ticks, "%Y"), las = 2)  # Rotate labels
  # dev.off()
  
  # Save the average sales real prices to a csv file.
  write_xlsx(df_antibiotics_wasp, "data/antibiotics_wasp.xlsx")
  write.csv(df_antibiotics_wasp, "data/antibiotics_wasp.csv", row.names = FALSE)
  
  # Merge the data
  df = NULL
  df = merge(df_antibiotics_wasp,df_avrakning,
             by.x = "Date",by.y="YearMonth" ,all = TRUE)
  
  df = merge(df,df_total_monthly_pigs_count,
             by.x = "Date",by.y="Date" ,all = TRUE)
  
  df = merge(df,df_total_monthly_company_count,
             by.x = "Date",by.y="Date" ,all = TRUE)
  
  # Logarhitmic values are used later in the ARDL analysis
  df$ln_AverageAntibioticsRealPricePerKg=log(df$AverageAntibioticsRealPricePerKg)
  df$ln_TotalKgActiveSubstanceSold=log(df$TotalKgActiveSubstanceSold)
  df$ln_headCount = log(df$headCount)
  df$ln_RealAvrakningsprisKrPerKg = log(df$RealAvrakningsprisKrPerKg)
  df$ln_companyCount = log(df$companyCount)
  
  # Because there are more dates in company and pigs count, they start at
  # year 2000, but antibiotics in 2005, we must remove the null values
  # in the beginning of the time series. 
  df=  na.omit(df)
  
  return (df)
  
}

##############################################################################

# This function is the same as above, but instead it merges into yearly data points instead of monthly.
merge_sales_and_control_data_yearly = function(df_antibiotics, label) {
  
  #df_antibiotics=sqldf("select * from df_antibiotics where ProductName like 'Ethacilin%' ")
  df_avrakning_yearly=read.csv("data/avrakningspris_kr_per_100kg_swine_yearly_average.csv")
  df_total_yearly_pigs_count=read.csv("data/total_monthly_pigs_count_yearly_average.csv")
  df_total_yearly_company_count=read.csv("data/total_monthly_company_count_yearly_average.csv")
  years = unique (df_antibiotics$Year)
  length(years)
  # Calculate the Weighted average sales price for each year. 
  columns = c("Year", "TotalKgActiveSubstanceSold", "AverageAntibioticsRealPricePerKg")
  df_antibiotics_wasp = NULL
  df_antibiotics_wasp = data.frame(matrix(ncol = length(columns), nrow = length(years)))
  colnames(df_antibiotics_wasp) = columns
  for (row in 1 : length(years)) {
    
    #  print(paste("row=",row))
    
    #row=1
    y=years[row]
    df_antibiotics_current_year=sqldf(paste0("select * from df_antibiotics where Year = '", y ,"'"))
    df_antibiotics_wasp[row, "Year"]=y
    df_antibiotics_wasp[row,"TotalKgActiveSubstanceSold"]=sum(df_antibiotics_current_year$KgActiveSubstance)
    df_antibiotics_wasp[row, "AverageAntibioticsRealPricePerKg"]=
      sum(df_antibiotics_current_year$RealPrice)/
      df_antibiotics_wasp[row,"TotalKgActiveSubstanceSold"]
  }
  range(df_antibiotics_wasp$AverageAntibioticsRealPricePerKg)
  # Make a plot to check the results
  # Plot the time series
  # plot(df_antibiotics_wasp$Year, df_antibiotics_wasp$AverageAntibioticsRealPricePerKg, type = "l", col = "steelblue",
  #      main = paste0("Antibiotics (",label,") weighted average real sales price per kg active substance, in year 2025 prices"), lwd=4,
  #      xlab = "Year", ylab = "Real Price / Kg Active Substance")  # Suppress default x-axis
  
  
  # Save the weighted average sales real prices to a csv file.
  write_xlsx(df_antibiotics_wasp, "data/antibiotics_wasp_yearly.xlsx")
  write.csv(df_antibiotics_wasp, "data/antibiotics_wasp_yearly.csv", row.names = FALSE)
  
  # Merge the data
  df = NULL
  df = merge(df_antibiotics_wasp,df_avrakning_yearly,
             by.x = "Year",by.y="year" ,all = TRUE)
  
  df = merge(df,df_total_yearly_pigs_count,
             by.x = "Year",by.y="year" ,all = TRUE)
  
  df = merge(df,df_total_yearly_company_count,
             by.x = "Year",by.y="year" ,all = TRUE)
  names(df)
  # Logarhitmic values are used later in the ARDL analysis
  df$ln_AverageAntibioticsRealPricePerKg=log(df$AverageAntibioticsRealPricePerKg)
  df$ln_TotalKgActiveSubstanceSold=log(df$TotalKgActiveSubstanceSold)
  df$ln_headCount = log(df$AverageheadCount)
  df$ln_RealAvrakningsprisKrPerKg = log(df$AverageRealAvrakningsprisKrPerKg)
  df$ln_companyCount = log(df$AverageCompanyCount)
  # Because there is more dates in company and pigs count, they start at
  # year 2000, but antibiotics in 2005, we must remove the null values
  # in the beginning of the time series. 
  df=  na.omit(df)
  
  return (df)
  
}