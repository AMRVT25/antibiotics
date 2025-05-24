
rm(list = ls())  # Clear memory
cat("\f") # Clear console
options(scipen=999) # Avoid scientific notation of numbers. 

library(sqldf)
library(tidyr)
library(stringr)
library(ggplot2)
library(writexl)
library(RColorBrewer)


setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory

output_dir <- "plots/market_share/"
dir.create(output_dir, showWarnings = FALSE)  # Create directory if it doesn't exist

colours <- c(  "black",       "red",         "blue",        "green3",      "orange",    
  "purple",      "brown",       "cyan",        "magenta",     "gold",        "darkgreen", 
  "deeppink"   
)

df_antibiotics=read.csv("data/antibiotics_sales_data_cleaned.csv")
df_antibiotics$Date = as.Date(df_antibiotics$Date)


df_most_common_products = sqldf("SELECT 
ProductName, ROUND(SUM(KgActiveSubstance)) as totsales_kg
FROM 
df_antibiotics
WHERE 
ProductName LIKE '%inj-vätska%'
GROUP BY 
ProductName
HAVING 
totsales_kg > 1200
order by totsales_kg desc")



#for (product in c("Ethacilin vet., inj-vätska, suspension 300 mg/ml")) {
for (product in df_most_common_products$ProductName) {
  product_main_name=strsplit(product, " ")[[1]][1]
  print(product_main_name)
  cdf = sqldf(paste0("SELECT YearMonth, PackageSize, sum(UnitsSold) AS UnitsSold
    FROM 
    df_antibiotics where ProductName like '", product , "'
    GROUP BY 
    YearMonth, ProductName, PackageSize
    ORDER BY 
    YearMonth, ProductName, PackageSize")
  )
  #if (product_main_name == "Hippotrim") {
    #stop()  # Just checking the time gap. 
  #}
  packsizes = sqldf(paste0("select distinct PackageSize from df_antibiotics where productname ='",product,"'"))
  cpivoted_df <- cdf %>%
    pivot_wider(
      names_from = PackageSize,
      values_from = UnitsSold
      
    )
  cpivoted_df[is.na(cpivoted_df)] = 0
  row_sums <- rowSums(cpivoted_df[ , -1], na.rm = TRUE)
  cpivoted_df$RowTotal <- row_sums
  cpivoted_df_percent <- cpivoted_df
  cols_to_divide <- 2:(ncol(cpivoted_df) - 1)
  cpivoted_df_percent[, cols_to_divide] = 100 * cpivoted_df_percent[, cols_to_divide] / cpivoted_df_percent$RowTotal
  names(cpivoted_df_percent) = gsub(" ", "_", names(cpivoted_df_percent))
  names(cpivoted_df_percent)[-1] = paste0("size_", names(cpivoted_df_percent)[-1])
  cpivoted_df_percent$YearMonth = as.Date(paste0(cpivoted_df_percent$YearMonth, "01"), format = "%Y%m%d")
  cpivoted_df$YearMonth= as.Date(paste0(cpivoted_df$YearMonth, "01"), format = "%Y%m%d")
  

  imgWidth = 1600
  imgHeight = 1000
  
  
  filename=paste0("plots/market_share/", product_main_name , ".png")
  
  
  
  png(filename, width = imgWidth, height = imgHeight, res = 150)
  
  plot(cpivoted_df_percent$YearMonth, cpivoted_df_percent[[2]] , type = "l",
       ylim = c(0, 100),  
       lwd=2,
       xlab = "Time", ylab = "%",
       main = paste0(product_main_name, ", package size market share"),
       col = 1)
  for (c in 3: ncol(cpivoted_df_percent)-1) {
    lines(cpivoted_df_percent$YearMonth, cpivoted_df_percent[[c]]   , col=colours[c-1])
  }
  legend("topright", 
         legend = packsizes$PackageSize,  # Labels
         col = colours,  # Colors
         lty = 1,  # Solid lines
         cex = 0.8)
  dev.off()
  
  # Now sum each column and display. 
  selected_names = names(cpivoted_df)[2:(ncol(cpivoted_df) - 1)]
  sum_df = data.frame(matrix(ncol = length(selected_names), nrow = 1))
  names(sum_df) <- selected_names
  for (i in 2 : (length(names(cpivoted_df))-1)  ) {
    sum_df[1, i-1] = colSums (cpivoted_df[,i], na.rm = TRUE)
  }
  print (product)
  sum_df[is.na(sum_df)] = 0
  order_cols <- order(as.numeric(sum_df[1, ]), decreasing = TRUE)
  
  # Reorder df using that order
  sum_df = sum_df[, order_cols] 
  print(sum_df)  
  print("-----------------------------------------------------------------")  
}
print ("Check generated plots in plots/market_share ")



