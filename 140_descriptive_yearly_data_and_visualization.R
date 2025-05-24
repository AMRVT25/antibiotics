# This program reads the complete merged dataset of antibiotics
# and use it  for data visualization 

rm(list = ls())  # Clear memory
cat("\f") # Clear console
options(scipen=999) # Avoid scientific notation of numbers. 

library(urca)

setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory
df=read.csv("data/merged_complete_data_of_antibiotics_Ethacilin_yearly.csv")

names(df)

cols=c("TotalKgActiveSubstanceSold" , 
"AverageAntibioticsRealPricePerKg",
"AverageRealAvrakningsprisKrPerKg" ,
"AverageheadCount", 
"AverageCompanyCount")
mains=c("Total Sold Active Substance of Antibiotics",
        "Weighted Average Price of Antibiotics",
        "Real Settlement Price for Pork to the Producer",
        "Total Number of Living Pigs",
        "Total Number of Pig Producing Farms")
ylabs=c("kg", 
        "SEK/kg",
        "SEK/100 kg",
        "Count", "Count")

output_dir <- "plots/"
dir.create(output_dir, showWarnings = FALSE)  # Create directory if it doesn't exist

imgWidth = 1600
imgHeight = 1000

# Plot the time series 
for (i in 1:length(cols)) {

  file_name <- paste0(output_dir, "Yearly_", cols[i], ".png")
  if (file.exists(file_name)) {
    file.remove(file_name)  # Deletes the old file
  }
  png(file_name, width = imgWidth, height = imgHeight, res = 150)
  
  # Plot the time series of AverageAntibioticsRealPricePerKg
  plot(df$Year, df[[cols[i]]], type = "l", col = "steelblue", lwd = 2,
       main = mains[i], 
       xlab = "Year", ylab = ylabs[i])  # Suppress default x-axis
  dev.off()
}

#######################################################################

# Make plot of sales and price in a combined plot
file_name <- paste0(output_dir, "Yearly_sales_vs_price", ".png")
if (file.exists(file_name)) {
  file.remove(file_name)  # Deletes the old file
}
png(file_name, width = imgWidth, height = imgHeight, res = 150)


par(mar=c(5, 4, 4, 5) + 0.1, mfrow=c(1,1))  # (bottom, left, top, right)
plot(df$Year, 
     df$AverageAntibioticsRealPricePerKg, 
     main="Price and Quantity of Antibotics",
     xlab="Year",
     ylab="SEK/kg",
     type = "l", col = "steelblue", lwd = 2
)

# Second plot: units_sold (Secondary Y-Axis)
par(new=TRUE)
plot(df$Year, 
     df$TotalKgActiveSubstanceSold, 
     axes=FALSE,  # Suppress default axes
     xlab="", ylab="",  
     type = "l", lwd = 2, col="darkred")  

# Add right-side axis for units sold
axis(side=4)  # Side 4 = right y-axis
mtext("kg", side=4, line=3)  # Label for second axis

legend("top", legend = c("Left: Price", "Right: Sales"),
       col = c("steelblue", "darkred"), lty = 1, lwd = 2, bty = "n")

dev.off()

#sqldf("select * from df where TotalKgActiveSubstanceSold >400")

# names(df)


# Statistical tests of unit root
adf_test <- ur.df(df$TotalKgActiveSubstanceSold, type = "drift", selectlags = "AIC")  # "drift" allows an intercept
summary(adf_test)
# Given indatafile merged_complete_data_of_antibiotics_Ethacilin.csv then 
# Value of test-statistic is: -0.8008 0.8954
# Compare the first number -0.8008 to the tau2 numbers
#Critical values for test statistics: 
#  1pct  5pct 10pct
#tau2 -3.58 -2.93 -2.60
#phi1  7.06  4.86  3.94
# Since -0.8008 > -2.60 or any of the other tau2 numbers -3.58 and -2.93
# we do not reject the null hypothesis, and conclude that the 
# time series is non-stationary
print("####################################################################")
# Now check the differed values : 
adf_test <- ur.df(diff(df$TotalKgActiveSubstanceSold), type = "drift", selectlags = "AIC")  # "drift" allows an intercept
summary(adf_test)
# Now test-statistic is -20.4791, clearly below all tau2 critical values. 