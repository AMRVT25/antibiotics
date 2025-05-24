# Det här programmet laddar hem statistik om antal djur per län över tid
# För att se manuellt vad för slags statistik det här kan man använda
# den här länken: 
# https://statistik.sjv.se/PXWeb/sq/ed092668-8f84-4aac-acfb-e4f00f405df5
# Eller långlänk: 
# https://statistik.sjv.se/PXWeb/pxweb/sv/Jordbruksverkets%20statistikdatabas/Jordbruksverkets%20statistikdatabas__Lantbrukets%20djur__Lantbruksdjur%20i%20juni/JO0103F01.px/table/tableViewLayout1/?loadedQueryId=ed092668-8f84-4aac-acfb-e4f00f405df5&timeType=item
# Men själva datat går att ladda hem direkt som en Excel-fil: 
# https://statistik.sjv.se/PXWeb/sq/fc820abe-57a5-4a72-bb57-e7c0cd7e6711

rm(list = ls())  # Clear memory
cat("\f") # Clear console
options(scipen=999) # Avoid scientific notation of numbers. 

setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory


url="https://statistik.sjv.se/PXWeb/sq/fc820abe-57a5-4a72-bb57-e7c0cd7e6711"

destfile <- "data/SJV_downloaded_animal_count.xlsx"  # The local file name to save as

download.file(url, destfile, mode = "wb")  # "wb" ensures binary mode for Windows



