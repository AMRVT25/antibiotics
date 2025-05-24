# This program reads all the data files produced 
# earlier and merge into a final complete dataset that can
# be used for data visualization and ARDL

rm(list = ls())  # Clear memory
cat("\f") # Clear console
options(scipen=999) # Avoid scientific notation of numbers. 

library(sqldf)
library(writexl)
setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory
source("065_correct_Two_Active_Substances.R")
source("070_merge_sales_data_with_control_data_util.R")


df_antibiotics=read.csv("data/antibiotics_sales_data.csv")

# As it turns out, there are some rows where price is 0. 
# This will later give problems, so remove them. 
sum(rowSums(is.na(df_antibiotics)) > 0) # Count rows that contin missing values, this shows 10. 
# If you want to see rows with missing values, store them in a dataframe for later inspection
df_tmp=df_antibiotics[!complete.cases(df_antibiotics), ] 
df_antibiotics=  na.omit(df_antibiotics) # Remove the rows with missing values
sum(rowSums(is.na(df_antibiotics)) > 0) # Verify there is no rows with missing values. This should show 0. 


# Count the number of observations that we want to filter out.
print(paste("Number of pet pigs", nrow(df_antibiotics[df_antibiotics$AnimalType == "Gris, sällskapsdjur - 50", ])))
print(paste("Number of rows with wrong unit", nrow(df_antibiotics[df_antibiotics$ActiveSubstanceUnit != "g", ])))
print(paste("Number of rows with wrong package type",nrow(df_antibiotics[df_antibiotics$PackType == "Påse", ])))
                        

# Filter out pigs that are kept as pets. 
df_antibiotics = df_antibiotics[df_antibiotics$AnimalType != "Gris, sällskapsdjur - 50", ]
# Filter so that only sales measured in g (gram) is kept
df_antibiotics = df_antibiotics[df_antibiotics$ActiveSubstanceUnit == "g", ]
# Filter out pack type = Påse. 
df_antibiotics = df_antibiotics[df_antibiotics$PackType != "Påse", ]

print(paste("Number of valid rows", nrow(df_antibiotics)))

# Convert to kg from g. 
df_antibiotics$KgActiveSubstance=df_antibiotics$ActiveSubstanceVolume/1000
# Calculate real price per kg active substrance. 
df_antibiotics$RealPricePerKgActiveSubstance=df_antibiotics$RealPrice/df_antibiotics$KgActiveSubstance

# In some calculations we need to take the log of the KgActiveSubstance, 
# meaning it can't contain 0. Clean that out (it is actually only one single observation that has 0).
df_antibiotics = df_antibiotics[df_antibiotics$KgActiveSubstance > 0, ]

# Some products have 2 active ingredients, and in the dataset that is 
# recored on 2 separate rows. This leads to that the price is wrongly counted twice. 
# This function takes care of that, including all details needed to be cared for: 
df_antibiotics=correctTwoActiveSubstances(df_antibiotics)

# Write the clean data to a file for later use
write.csv(df_antibiotics, "data/antibiotics_sales_data_cleaned.csv", row.names = FALSE)
write_xlsx(df_antibiotics, "data/antibiotics_sales_data_cleaned.xlsx")

# These are used to rename columns, and to generate Stata code. 
longNames=names(df)
shortNames = c("Date", "Q","p", "nom_pork_p", "pork_p", "pigs","farms", "ln_p","ln_Q","ln_pigs","ln_pork_p" )


# Now make different datasets for further analysis. 
makeDatasetAndSaveToFile = function(sql_statment, name) {
  df = merge_sales_and_control_data(sqldf(sql_statment),name)
  write_xlsx(df, paste0("data/merged_complete_data_of_antibiotics_",gsub(" ", "_", name),".xlsx"))
  csv_filename=paste0("data/merged_complete_data_of_antibiotics_",gsub(" ", "_", name),".csv")
  write.csv(df,csv_filename , row.names = FALSE)
  print(paste("Wrote csv file:", csv_filename))
  # Stata does not handle long varible names, so rename them here. 
  # It is easier than doing it in Stata. 
  df_shortnames=df
  colnames(df_shortnames)  = shortNames # Override the column names and save to file
  write_xlsx(df_shortnames, paste0("data/merged_complete_data_of_antibiotics_",gsub(" ", "_", name),"_shortNames.xlsx"))
  csv_filename=paste0("data/merged_complete_data_of_antibiotics_",gsub(" ", "_", name),"_shortNames.csv")
  write.csv(df_shortnames,  csv_filename, row.names = FALSE)
  return (df)
}

# This dataset df_Ethacilin_all was used to verify that R and Stata calcualtions 
# produce the same results. However, this analysis is never used in the thesis later
# as it does not distinguish between different package sizes, and thus are not valid. 
# But the conformation about the same output is still completely valid. This
# makes sure that R and Stata ARDl function is producing equal and correct results. 
df_Ethacilin_all = makeDatasetAndSaveToFile("select * from df_antibiotics where ProductName like 'Ethacilin%'","Ethacilin")

# Generate som Stata code for the labels of the short names
for (i in 1:length(shortNames)) {
  print( paste0("label variable ", shortNames[i], " '", longNames[i], "'" ) )
}
