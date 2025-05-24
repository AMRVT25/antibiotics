# This program reads all the data files produced 
# earlier and merge into a final complete dataset that can
# be used for data visualization and ARDL

rm(list = ls())  # Clear memory
cat("\f") # Clear console
options(scipen=999) # Avoid scientific notation of numbers. 

library(sqldf)
library(writexl)

setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory
source("070_merge_sales_data_with_control_data_util.R")

df_antibiotics=read.csv("data/antibiotics_sales_data_cleaned.csv")
names(df_antibiotics)
# Now make different datasets for further analysis. 
# Ethacilin only
df = merge_sales_and_control_data_yearly(sqldf("select * from df_antibiotics where ProductName like 'Ethacilin%' "),
                                  "Ethacilin (Yearly)")

write_xlsx(df, "data/merged_complete_data_of_antibiotics_Ethacilin_yearly.xlsx")
write.csv(df, "data/merged_complete_data_of_antibiotics_Ethacilin_yearly.csv", row.names = FALSE)

# Stata does not handle long varible names, so rename them here. 
# It is easier than doing it in Stata. 
longNames=names(df)
shortNames = c("Year", "Q","p", "pork_p", "pigs","farms", "ln_p","ln_Q" )
colnames(df)  = shortNames
names(df)
write_xlsx(df, "data/merged_complete_data_of_antibiotics_Ethacilin_yearly_shortNames.xlsx")
write.csv(df, "data/merged_complete_data_of_antibiotics_Ethacilin_yearly_shortNames.csv", row.names = FALSE)

# Generate some Stata code
for (i in 1:length(shortNames)) {
  print( paste0("label variable ", shortNames[i], " '", longNames[i], "'" ) )
}

