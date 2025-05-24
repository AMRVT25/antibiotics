# This program reads the KPI index from Sweden Statistics and convert it into 
# an R dataframe and saves it into a csv file. 

rm(list = ls())  # Clear memory
cat("\f") # Clear console

# Load necessary libraries
library(rvest) 
library(dplyr)
setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory



# URL of the webpage containing the table
url <- "https://www.scb.se/hitta-statistik/statistik-efter-amne/priser-och-ekonomiska-tendenser/priser/konsumentprisindex-kpi/pong/tabell-och-diagram/konsumentprisindex-kpi/kpi-faststallda-tal-1980100/"

# Read the HTML content from the URL
webpage <- read_html(url)

# Extract the table
table_node <- html_node(webpage, "table")

# Parse the table into a dataframe
df <- html_table(table_node, fill = TRUE)

# Change number format so that decimals are preceeded by . instead of ,
df <- df %>%
  mutate_all(~ gsub(",", ".", ., fixed = TRUE)) %>%
  mutate_all(~ as.numeric(.))

# Display the first few rows of the dataframe
head(df)

# Konsumentprisindex = Consumer price index
write.csv( df , "data/SCB_Konsumentprisindex.csv", row.names = FALSE) # Write out the data to a csv file. 

