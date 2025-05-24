# See 030_get_number_of_pigs_in_June_from_SJV_and_save_to_CSV_file_part_1.R
# for instructions. This program does the same, but instead downloads
# avräkningspriser. Since the time series are split into 10 years, 
# multiple downloads is needed. 
rm(list = ls())  # Clear memory
cat("\f") # Clear console
options(scipen=999) # Avoid scientific notation of numbers. 
library(sqldf)
setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory

# Downloading from the following pages. 
# Base url: https://statistik.sjv.se/PXWeb/pxweb/sv/Jordbruksverkets%20statistikdatabas/
# Sub categories: 


# 1: / Jordbruksverkets statistikdatabas / 
#       Priser och prisindex / 
#       Priser /  Avräkningspriser, vikter enligt basår 2000=100 / 
#       Avräkningspriser, månad fr.o.m. januari 2000 (2000=100)
# Human readable: https://statistik.sjv.se/PXWeb/sq/b550c4c2-0f65-42af-a43d-43830d9d5ebf
# Excel:          https://statistik.sjv.se/PXWeb/sq/f22db512-e40f-4d8e-b65f-bd27556b95da         
#
# 2: / Jordbruksverkets statistikdatabas / 
#       Priser och prisindex / 
#       Priser / Avräkningspriser, vikter enligt basår 2010=100 / 
#       Avräkningspriser, månad fr.o.m. januari 2005
# Human readable: https://statistik.sjv.se/PXWeb/sq/a760b4c4-f3dd-48c5-940c-2b0eca3afde9
# Excel:          https://statistik.sjv.se/PXWeb/sq/52981281-2656-445f-ad6b-0ef533db6523

# 3: ... Avräkningspriser, vikter enligt basår 2015=100 / 
#        Avräkningspriser, månad fr.o.m. januari 2010
# Human readable: https://statistik.sjv.se/PXWeb/sq/f9003ff1-f430-46ec-bd5b-7ef39de48a05
# Excel:          https://statistik.sjv.se/PXWeb/sq/d49f8135-fa40-485e-8be0-8a85ff5787a6

# 4: ... Avräkningspriser, vikter enligt basår 2020=100 / 
#        Avräkningspriser, månad fr.o.m. januari 2015
# Human readable: https://statistik.sjv.se/PXWeb/sq/c799dfda-2acc-42c6-a9a4-fe16f46ca867
# Excel:          https://statistik.sjv.se/PXWeb/sq/63e2adb0-b062-4f87-a91f-cbf2369a40c1


url=c("https://statistik.sjv.se/PXWeb/sq/f22db512-e40f-4d8e-b65f-bd27556b95da",
      "https://statistik.sjv.se/PXWeb/sq/52981281-2656-445f-ad6b-0ef533db6523",
    "https://statistik.sjv.se/PXWeb/sq/d49f8135-fa40-485e-8be0-8a85ff5787a6",
    "https://statistik.sjv.se/PXWeb/sq/63e2adb0-b062-4f87-a91f-cbf2369a40c1")
destfile=c("data/avrakningspris_2000M01_2005M12_base_2000.xlsx",
           "data/avrakningspris_2005M01_2018M02_base_2010.xlsx",
           "data/avrakningspris_2010M01_2024M10_base_2015.xlsx",
           "data/avrakningspris_2015M01_2025M01_base_2020.xlsx")

for (i in 1:length(url)) {
  download.file(url[i], destfile[i], mode = "wb")  # "wb" ensures binary mode for Windows
  print(paste0("Downloaded file ", i , " of ", length(url) ))
}

print("Done downloading")


df_KPI_base=read.csv("data/KPI_base.csv")
KPI_base=df_KPI_base$KPI_base
df_kpi=read.csv("data/SCB_Konsumentprisindex.csv")
# This function returns the average KPI (Consumer Price Index) given a 
# specific year 
getKPIYearAverage = function(year) {
  return ( df_kpi[df_kpi$År == year,  "Årsmedel" ] )
}

# Calculate real price using the algorithm from: https://www.scb.se/vara-tjanster/index/sa-har-raknar-du-med-index/
# Slutligt pris inklusive indexpåslag = i2/ i1 * grundpris
# Add the real price, using base year=2025, month=February to the datasets
# in each of the Avräkningspris dataframes. Since each excel sheet has different
# length, it is needed to have a separate range for each. 

# Make a new dataframe for each file, named after the base year used in each file.
# Save only the plain Avräkningspris, not the classfication avräknignspris. 
df_avrakning_2000 <- read_excel(destfile[1], sheet = 1, range = "A3:B75", col_names = TRUE)
df_avrakning_2010 <- read_excel(destfile[2], sheet = 1, range = "A3:B161", col_names = TRUE)
df_avrakning_2015 <- read_excel(destfile[3], sheet = 1, range = "A3:B181", col_names = TRUE)
df_avrakning_2020 <- read_excel(destfile[4], sheet = 1, range = "A3:B124", col_names = TRUE)

# Set new column names
newColNames=c("YearMonth", "AvrakningsprisKrPerKg")
colnames(df_avrakning_2000) = newColNames
colnames(df_avrakning_2010) = newColNames
colnames(df_avrakning_2015) = newColNames
colnames(df_avrakning_2020) = newColNames
# Remove the M separator so that the yearmonth format is YYYYMM  
df_avrakning_2000$YearMonth=as.numeric(gsub("[^0-9]", "", df_avrakning_2000$YearMonth))
df_avrakning_2010$YearMonth=as.numeric(gsub("[^0-9]", "", df_avrakning_2010$YearMonth))
df_avrakning_2015$YearMonth=as.numeric(gsub("[^0-9]", "", df_avrakning_2015$YearMonth))
df_avrakning_2020$YearMonth=as.numeric(gsub("[^0-9]", "", df_avrakning_2020$YearMonth))

range(df_avrakning_2010$YearMonth)

# Get the average KIP for each year
KPI_2000=getKPIYearAverage(2000)
KPI_2010=getKPIYearAverage(2010)
KPI_2015=getKPIYearAverage(2015)
KPI_2020=getKPIYearAverage(2020)

# Calculate real price using the algorithm from: https://www.scb.se/vara-tjanster/index/sa-har-raknar-du-med-index/
# Slutligt pris inklusive indexpåslag = i2 / i1 * grundpris
df_avrakning_2000$RealAvrakningsprisKrPerKg =  KPI_base / KPI_2000 * df_avrakning_2000$AvrakningsprisKrPerKg
df_avrakning_2010$RealAvrakningsprisKrPerKg =  KPI_base / KPI_2010 * df_avrakning_2010$AvrakningsprisKrPerKg
df_avrakning_2015$RealAvrakningsprisKrPerKg =  KPI_base / KPI_2015 * df_avrakning_2015$AvrakningsprisKrPerKg
df_avrakning_2020$RealAvrakningsprisKrPerKg =  KPI_base / KPI_2020 * df_avrakning_2020$AvrakningsprisKrPerKg

# The different df_avrakning_YYYY overlaps. Now merge them into one single dataframe
# Use as much as possible from the newest. 

df_avrakning=df_avrakning_2000 # fill the merged dataframe with the first (oldest) dataframe
# Get the first data available in the next newer dataframe
first_time_stamp_of_next=as.numeric(df_avrakning_2010[1,"YearMonth"])
# Keep only data unique to the older dataframe
df_avrakning <- df_avrakning[df_avrakning$YearMonth < first_time_stamp_of_next, ]
# Add the newer dataframe to the end of the merged. 
df_avrakning=rbind(df_avrakning, df_avrakning_2010)
# Repeat for the next dataframes...

first_time_stamp_of_next=as.numeric(df_avrakning_2015[1,"YearMonth"])
df_avrakning <- df_avrakning[df_avrakning$YearMonth < first_time_stamp_of_next, ]
df_avrakning=rbind(df_avrakning, df_avrakning_2015)

first_time_stamp_of_next=as.numeric(df_avrakning_2020[1,"YearMonth"])
df_avrakning <- df_avrakning[df_avrakning$YearMonth < first_time_stamp_of_next, ]
df_avrakning=rbind(df_avrakning, df_avrakning_2020)

range(df_avrakning$YearMonth) # Check range
any(duplicated(df_avrakning$YearMonth)) # Check that there are no duplicates, Should show FALSE

# Make sure there is no missing months

full_seq <- seq(as.Date(paste0(min(df_avrakning$YearMonth), "01"), format="%Y%m%d"),
                as.Date(paste0(max(df_avrakning$YearMonth), "01"), format="%Y%m%d"),
                by = "month")
full_seq <- format(full_seq, "%Y%m")
missing_months <- setdiff(full_seq, as.character(df_avrakning$YearMonth))
print(missing_months) # Should say character(0) if we cot everything correct. 

# Make a plot to check the results
# Plot the time series
df_avrakning$YearMonth <- as.Date(paste0(df_avrakning$YearMonth, "01"), format="%Y%m%d")
# Plot without x-axis
plot(df_avrakning$YearMonth, df_avrakning$RealAvrakningsprisKrPerKg, type = "l", col = "steelblue",
     main = "Producer price per 100 kg for slaughtered swine, in year 2025 prices", lwd=4,
     xlab = "Year", ylab = "Real Price", xaxt = "n")  # Suppress default x-axis

# Generate yearly ticks
year_ticks <- seq(from = min(df_avrakning$YearMonth), 
                  to = max(df_avrakning$YearMonth), 
                  by = "year")

# Add custom x-axis with yearly labels
axis(1, at = year_ticks, labels = format(year_ticks, "%Y"), las = 2)  # Rotate labels

# Results look good and matches SJV corresponding graph
# even though time series is not as long and prices are in
# 2020 years prices. Shape of the curve is the same though. 

# Save the avräknignspriser to a csv file. 
write_xlsx(df_avrakning, "data/avrakningspris_kr_per_100kg_swine.xlsx")
write.csv(df_avrakning, "data/avrakningspris_kr_per_100kg_swine.csv", row.names = FALSE)

# Also I validated the real price conversion using
# https://www.scb.se/hitta-statistik/sverige-i-siffror/prisomraknaren/
# It looks correct. 

names(df_avrakning)
df_avrakning$year=substr(df_avrakning$YearMonth, 1, 4)
df_avrakning_yearly=sqldf("select year, avg(RealAvrakningsprisKrPerKg) as AverageRealAvrakningsprisKrPerKg from df_avrakning group by year order by year")
write_xlsx(df_avrakning_yearly, "data/avrakningspris_kr_per_100kg_swine_yearly_average.xlsx")
write.csv(df_avrakning_yearly, "data/avrakningspris_kr_per_100kg_swine_yearly_average.csv", row.names = FALSE)

