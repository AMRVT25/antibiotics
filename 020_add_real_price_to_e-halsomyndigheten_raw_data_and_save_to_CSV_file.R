# This program reads the KPI index from the csv file.
# Then it reads the antibiotics data from the csv file. 
# Then it adds the real price (price compensated for inflation) to 
# the antibiotics sales data and save it into a csv file. 

rm(list = ls())  # Clear memory
cat("\f") # Clear console

# Load necessary libraries
library(readxl) # Read Excel file. 
library(writexl) # Write Excel file. 

options(scipen=999) # Avoid scientific notation of numbers. 
setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory

df_kpi=read.csv("data/SCB_Konsumentprisindex.csv")


# Read the antibiotic sales data from e-Hälsomyndigheten 
# (e-Hälsomyndigheten = The Swedish eHealth Agency, which is a government 
# agency that works to digitalise and improve the sharing of information
# between patients, the healthcare system and pharmacies in Sweden
# They provided the data on sales of antibiotics to swine between 2005 and 2025
df <- read_excel("data/dist/Antibiotika_grisar_Dataleverans.xls", sheet = 1, col_names = TRUE)

# Rename columns
names(df) # Display original column names
colnames(df)[which(names(df) == "FaktureringsManadId")] <- "YearMonth"
colnames(df)[which(names(df) == "ATCKOD7")] <- "ATC"
colnames(df)[which(names(df) == "forpackningstyptext")] <- "PackType"
colnames(df)[which(names(df) == "SubstansText")] <- "Substanse"
colnames(df)[which(names(df) == "Summa_enheter")] <- "UnitsSold"
colnames(df)[which(names(df) == "DjurslagText")] <- "AnimalType"
colnames(df)[which(names(df) == "VaraNamn")] <- "ProductName"
colnames(df)[which(names(df) == "Forpackningsstorlektext")] <- "PackageSize"
colnames(df)[which(names(df) == "Summa_Utförsaljningsprisexmoms")] <- "Price"
colnames(df)[which(names(df) == "Summa aktiv substans, enhet")] <- "ActiveSubstanceVolume"
colnames(df)[which(names(df) == "Enhet")] <- "ActiveSubstanceUnit"
names(df) # Display renamed column names

# This function returns the KPI (Consumer Price Index) given a 
# specific year and month
getKPI = function(year, month) {
  map_month=c("Jan","Feb","Mar","Apr","Maj","Jun","Jul","Aug","Sep","Okt","Nov","Dec")
  return ( df_kpi[df_kpi$År == year,  map_month[month] ] )
}

# To calculate the real price we need the KPI (KPI=CPI) of the last month
# in the antibiotics sales data. This KIP is stored in variable KPI_base
# and then used later in the program. 
date_str=as.character(df[nrow(df), "YearMonth"])
year=as.numeric(substr(date_str, 1,4))
month=as.numeric(substr(date_str, 5,6))
KPI_base=getKPI(year, month)

# Other programs alse needs the base KPI, so save it into a file. 
df_KPI_base=as.data.frame(list(year=year, month=month, KPI_base=KPI_base))
write.csv(df_KPI_base, "data/KPI_base.csv", row.names = FALSE)

# Loop over each row in the dataframe of antibiotic sales
# and calculate the real price using the price index. 
# Also add Year and Month as separate columns to the dataframe.
for (row in 1:nrow(df)) {
  date_str=as.character(df[row, 1])
  year=as.numeric(substr(date_str, 1,4))
  month=as.numeric(substr(date_str, 5,6))
  KPI=getKPI(year, month)
  # Calculate real price using the algorithm from: https://www.scb.se/vara-tjanster/index/sa-har-raknar-du-med-index/
  # Slutligt pris inklusive indexpåslag = i2/ i1 * grundpris
  df[row, "RealPrice"] <- KPI_base / KPI * df[row, "Price"]
  df[row, "Year"] <- year
  df[row, "Month"] <- month
}


df$RealPricePerUnit=df$RealPrice/df$UnitsSold # Calcualte price per unit
# Add separate column for the date, adding 01 - that is the first day of the month
# so that the date takes the form yearm month day.
df$Date <- as.Date(paste0(df$YearMonth, "01"), format="%Y%m%d")

# Save the antibiotics sales data
write.csv( df , "data/antibiotics_sales_data.csv", row.names = FALSE) # Write out the data to a csv file. 

# Sometimes is is convenient to also see this data in Excel, so write also in Excel format
write_xlsx(df, "data/antibiotics_sales_data.xlsx")
