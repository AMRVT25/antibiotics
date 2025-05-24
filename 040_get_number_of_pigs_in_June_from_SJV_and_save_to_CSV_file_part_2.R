# Det här programmet läser excelfilen från förra programmet
# och konverterar det till en csv fil. 

rm(list = ls())  # Clear memory
cat("\f") # Clear console
options(scipen=999) # Avoid scinetific notation of numbers. 
library(readxl)
library(writexl)
library(sqldf)
setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory


df <- read_excel("data/SJV_downloaded_animal_count.xlsx", sheet = 1 , col_names = FALSE)


# The code below extracts the relevant data given the 
# the structure of the Excel file. 
keys=c("Galtar",
       "Suggor, inklusive gyltor",
       "Slaktgrisar",
       "Smågrisar",
       "Summa grisar"
       )


# Create dataframes to store the number of animals
columns= c("animal","year", "headCount" ) 
df_animals = data.frame(matrix(nrow = 0,  ncol = length(columns)))
colnames(df_animals) = columns

columns= c("animal","year", "companyCount" ) 
df_companies = data.frame(matrix(nrow = 0,  ncol = length(columns)))
colnames(df_companies) = columns

c=0

for (key in keys) {
  for (row in 1 : nrow(df))  {
    if (!is.na(df[row, 1]) && df[row,1] ==  key) {
      cc=0
      for (i in 1 : 25) {
        c=c+1
        cc=cc+1
        df_animals[c,"animal"]=key
        df_animals[c,"year"]=df[row+cc-1,4]
        df_animals[c,"headCount"]=df[row+cc-1,5]
        df_companies[c,"animal"]=key
        df_companies[c,"year"]=df[row+cc+25-1,4]
        df_companies[c,"companyCount"]=df[row+cc+25-1,5]
      }
    }
    
  }
}

# There are missing values for the comapny count for the following years:
# 2000, 2001, 2002 ,2004,2006,2008,2009
# Extrapolate values into those years using OLS prediction. 
names(df_companies)
df_subset=sqldf("select year, companyCount from df_companies where animal='Summa grisar' and year<2011 and companyCount>0")
df_subset=as.data.frame(lapply(df_subset, as.numeric))
model=lm(companyCount ~ year, data=df_subset)
summary(model)
missing_data <- data.frame(year = c(2000, 2001, 2002 ,2004,2006,2008,2009))  # Create a new data frame with multiple years
predicted_companyCount <- predict(model, missing_data)  # Predict companyCount for these years
print(round(predicted_companyCount)) 
missing_data$predicted_companyCount=round(predicted_companyCount)
# Replace the missing values in the df_companies dataframe with the predicted values. 
for (row in 1:nrow(missing_data)) {
  y=missing_data[row,"year"]
  c=missing_data[row,"predicted_companyCount"]
  print(paste(y,c))
  df_companies[df_companies$year == y & df_companies$animal == "Summa grisar", "companyCount"] = c
}


write_xlsx(df_animals, "data/animal_count.xlsx")
write.csv(df_animals, "data/animal_count.csv", row.names = FALSE)
write_xlsx(df_companies, "data/company_count.xlsx")
write.csv(df_companies, "data/company_count.csv", row.names = FALSE)

# Used during debugging. Looks good now
df_simple_company_count=sqldf("select * from df_companies where animal='Summa grisar'")
plot(df_simple_company_count$year,df_simple_company_count$companyCount, type="l", main="Simple company count")

##############################################################################

# In later analysis, a monthly projection is also needed. 
# This data is yearly. So you OLS to predict the monthly values.
# Monthly values are needed for both company and animal count. 
# Start with animal count. 

df_total_pigs_count=sqldf("select year , headCount from df_animals where animal='Summa grisar'")
# The head count is done in June, according to SJV.
# So we need to fill in the gaps between June to June each pair of years
df_total_pigs_count$Date=as.Date(paste0(df_total_pigs_count$year,
                                        "06",
                                        "01"), format="%Y%m%d")
df_total_monthly_pigs_count <- data.frame(
  year = rep(df_total_pigs_count$year, each = 12),
  month = rep(1:12, times = nrow(df_total_pigs_count))
)
df_total_monthly_pigs_count$Date= 
  as.Date(paste0(df_total_monthly_pigs_count$year,
                 sprintf("%02d", df_total_monthly_pigs_count$month),
                 "01"), format="%Y%m%d")
df_total_monthly_pigs_count$year = NULL
df_total_monthly_pigs_count$month = NULL
df_total_monthly_pigs_count

# Merge the data from the yearly df into the monthly. 
df_total_monthly_pigs_count = merge(df_total_monthly_pigs_count,df_total_pigs_count,
            by.x = "Date",by.y="Date" ,all = TRUE)
df_total_monthly_pigs_count$year=NULL
df_total_monthly_pigs_count$strDate= as.character(df_total_monthly_pigs_count$Date)
for (i in seq(6, nrow(df_total_monthly_pigs_count), by = 12)) {
  print(df_total_monthly_pigs_count[i, "Date"])
  startDate=df_total_monthly_pigs_count[i, "Date"]
  endDate <- seq(startDate, by = "1 year", length.out = 2)[2]
  sql=paste0("select * from df_total_monthly_pigs_count where strDate='",startDate,"' or strDate='",endDate,"'")
  df_this_year=sqldf(sql)
  model=lm(headCount ~ Date , data=df_this_year)
  dates_in_between <- seq(as.Date(startDate), as.Date(endDate), by = "1 month")
  df_in_between <- data.frame(Date = dates_in_between)
  df_in_between$predicted_headCount <- predict(model, newdata = df_in_between)
  # Update the empty months with the predicted values
  df_total_monthly_pigs_count$headCount[is.na(df_total_monthly_pigs_count$headCount)] <- 
    df_in_between$predicted_headCount[match(df_total_monthly_pigs_count$Date[is.na(df_total_monthly_pigs_count$headCount)], 
                                           df_in_between$Date)]
  

  if (i==6) {
    #The first year, as the data starts in June, we also need to predict backwards to January 
    endDate=startDate
    startDate <- seq(endDate, by = "-5 months", length.out = 2)[2]
    dates_in_between <- seq(as.Date(startDate), as.Date(endDate), by = "1 month")
    df_in_between <- data.frame(Date = dates_in_between)
    df_in_between$predicted_headCount <- predict(model, newdata = df_in_between)
    df_total_monthly_pigs_count$headCount[is.na(df_total_monthly_pigs_count$headCount)] = 
      df_in_between$predicted_headCount[match(df_total_monthly_pigs_count$Date[is.na(df_total_monthly_pigs_count$headCount)], 
                                              df_in_between$Date)]
  }
  if (TRUE) {
    #break   # Used for debugging
  }
}
# Last iteration, predict the future, the rest of the months in 2024
# I keep the "in between" name for convenience, even though it is the future. 
startDate=endDate
endDate <- seq(startDate, by = "6 months", length.out = 2)[2]
dates_in_between <- seq(as.Date(startDate), as.Date(endDate), by = "1 month")
df_in_between <- data.frame(Date = dates_in_between)
df_in_between$predicted_headCount <- predict(model, newdata = df_in_between)

df_total_monthly_pigs_count$headCount[is.na(df_total_monthly_pigs_count$headCount)] <- 
  df_in_between$predicted_headCount[match(df_total_monthly_pigs_count$Date[is.na(df_total_monthly_pigs_count$headCount)], 
                                          df_in_between$Date)]

df_total_monthly_pigs_count$headCount=round(as.numeric(df_total_monthly_pigs_count$headCount))
df_total_monthly_pigs_count$strDate=NULL # No longer needed

# Plot without x-axis
plot(df_total_monthly_pigs_count$Date, df_total_monthly_pigs_count$headCount, type = "l", col = "steelblue",
     main = "Number of pigs over time", lwd=4,
    
     xlab = "Year", ylab = "Pig count", xaxt = "n")  # Suppress default x-axis

# Generate yearly ticks
year_ticks <- seq(from = min(df_total_monthly_pigs_count$Date), 
                  to = max(df_total_monthly_pigs_count$Date), 
                  by = "year")

# Add custom x-axis with yearly labels
axis(1, at = year_ticks, labels = format(year_ticks, "%Y"), las = 2)  # Rotate labels


write_xlsx(df_total_monthly_pigs_count, "data/total_monthly_pigs_count.xlsx")
write.csv(df_total_monthly_pigs_count, "data/total_monthly_pigs_count.csv", row.names = FALSE)

# Write out yearly average
df_total_monthly_pigs_count$year=substr(df_total_monthly_pigs_count$Date , 1, 4)
df_total_monthly_pigs_count_yearly=sqldf("select year, avg(headCount ) as AverageheadCount  from df_total_monthly_pigs_count group by year order by year")
write_xlsx(df_total_monthly_pigs_count_yearly, "data/total_monthly_pigs_count_yearly_average.xlsx")
write.csv(df_total_monthly_pigs_count_yearly, "data/total_monthly_pigs_count_yearly_average.csv", row.names = FALSE)


#################################################################################

# Now repeat the same code above, but now do it from companies (farms) instead. 

df_total_company_count=sqldf("select year , companyCount from df_companies where animal='Summa grisar'")
df_total_company_count$Date=as.Date(paste0(df_total_company_count$year,
                                           "06",
                                           "01"), format="%Y%m%d")
df_total_monthly_company_count <- data.frame(
  year = rep(df_total_company_count$year, each = 12),
  month = rep(1:12, times = nrow(df_total_company_count))
)
df_total_monthly_company_count$Date= 
  as.Date(paste0(df_total_monthly_company_count$year,
                 sprintf("%02d", df_total_monthly_company_count$month),
                 "01"), format="%Y%m%d")
df_total_monthly_company_count$year = NULL
df_total_monthly_company_count$month = NULL
df_total_monthly_company_count

# Merge the data from the yearly df into the monthly. 
df_total_monthly_company_count = merge(df_total_monthly_company_count,df_total_company_count,
                                       by.x = "Date",by.y="Date" ,all = TRUE)
df_total_monthly_company_count$year=NULL
df_total_monthly_company_count$strDate= as.character(df_total_monthly_company_count$Date)
for (i in seq(6, nrow(df_total_monthly_company_count), by = 12)) {
  print(df_total_monthly_company_count[i, "Date"])
  startDate=df_total_monthly_company_count[i, "Date"]
  endDate <- seq(startDate, by = "1 year", length.out = 2)[2]
  sql=paste0("select * from df_total_monthly_company_count where strDate='",startDate,"' or strDate='",endDate,"'")
  df_this_year=sqldf(sql)
  model=lm(companyCount ~ Date , data=df_this_year)
  dates_in_between <- seq(as.Date(startDate), as.Date(endDate), by = "1 month")
  df_in_between <- data.frame(Date = dates_in_between)
  df_in_between$predicted_companyCount <- predict(model, newdata = df_in_between)
  # Update the empty months with the predicted values
  df_total_monthly_company_count$companyCount[is.na(df_total_monthly_company_count$companyCount)] <- 
    df_in_between$predicted_companyCount[match(df_total_monthly_company_count$Date[is.na(df_total_monthly_company_count$companyCount)], 
                                               df_in_between$Date)]
  
  
  if (i==6) {
    #The first year, as the data starts in June, we also need to predict backwards to January 
    endDate=startDate
    startDate <- seq(endDate, by = "-5 months", length.out = 2)[2]
    dates_in_between <- seq(as.Date(startDate), as.Date(endDate), by = "1 month")
    df_in_between <- data.frame(Date = dates_in_between)
    df_in_between$predicted_companyCount <- predict(model, newdata = df_in_between)
    df_total_monthly_company_count$companyCount[is.na(df_total_monthly_company_count$companyCount)] = 
      df_in_between$predicted_companyCount[match(df_total_monthly_company_count$Date[is.na(df_total_monthly_company_count$companyCount)], 
                                                 df_in_between$Date)]
  }
  if (TRUE) {
    #break   # Used for debugging
  }
}
# Last iteration, predict the future, the rest of the months in 2024
# I keep the "in between" name for convenience, even though it is the future. 
startDate=endDate
endDate <- seq(startDate, by = "6 months", length.out = 2)[2]
dates_in_between <- seq(as.Date(startDate), as.Date(endDate), by = "1 month")
df_in_between <- data.frame(Date = dates_in_between)
df_in_between$predicted_companyCount <- predict(model, newdata = df_in_between)

df_total_monthly_company_count$companyCount[is.na(df_total_monthly_company_count$companyCount)] <- 
  df_in_between$predicted_companyCount[match(df_total_monthly_company_count$Date[is.na(df_total_monthly_company_count$companyCount)], 
                                             df_in_between$Date)]

df_total_monthly_company_count$companyCount=round(as.numeric(df_total_monthly_company_count$companyCount))
df_total_monthly_company_count$strDate=NULL # No longer needed

# Plot without x-axis
plot(df_total_monthly_company_count$Date, df_total_monthly_company_count$companyCount, type = "l", col = "steelblue",
     main = "Number of companies (farms) over time", lwd=4,
     
     xlab = "Year", ylab = "Company count", xaxt = "n")  # Suppress default x-axis

# Generate yearly ticks
year_ticks <- seq(from = min(df_total_monthly_company_count$Date), 
                  to = max(df_total_monthly_company_count$Date), 
                  by = "year")

# Add custom x-axis with yearly labels
axis(1, at = year_ticks, labels = format(year_ticks, "%Y"), las = 2)  # Rotate labels


write_xlsx(df_total_monthly_company_count, "data/total_monthly_company_count.xlsx")
write.csv(df_total_monthly_company_count, "data/total_monthly_company_count.csv", row.names = FALSE)

# Write out yearly average
df_total_monthly_company_count$year=substr(df_total_monthly_company_count$Date , 1, 4)
df_total_monthly_company_count_yearly=sqldf("select year, avg(companyCount ) as AverageCompanyCount  from df_total_monthly_company_count group by year order by year")
write_xlsx(df_total_monthly_company_count_yearly, "data/total_monthly_company_count_yearly_average.xlsx")
write.csv(df_total_monthly_company_count_yearly, "data/total_monthly_company_count_yearly_average.csv", row.names = FALSE)
