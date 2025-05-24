# This program reads the complete merged dataset of antibiotics
# and use it  for data visualization 

rm(list = ls())  # Clear memory
cat("\f") # Clear console
options(scipen=999) # Avoid scientific notation of numbers. 

library(urca)
library(sqldf)

setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory

imgWidth = 1600
imgHeight = 1000

# Select which data you want to plot by changing drug_choice_index between 1 and 4
drug_choice_index=4

drug_choices=c("QJ01CE09","Ethacilin_Vet","Injektionsflaska","All")
drug=drug_choices[drug_choice_index]

suffix=paste0("_",drug)

csv_file=paste0("merged_complete_data_of_antibiotics",  suffix ,".csv")

df=read.csv(paste0("data/", csv_file))
df$Date = as.Date(df$Date)

names(df)

cols=c("TotalKgActiveSubstanceSold" , 
"AverageAntibioticsRealPricePerKg",
"RealAvrakningsprisKrPerKg" ,
"headCount", 
"companyCount")



mains=c( paste0("Total sold active substance of all antibiotics, by month") ,
         paste0("Average price of all antibiotics, by month"),
        "Real settlement price for pork to the producer, by month",
        "Total number of living pigs, by month",
        "Total number of pig producing farms, by month")
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
  file_name <- paste0(output_dir, cols[i], ".png")
  if (file.exists(file_name)) {
    file.remove(file_name)  # Deletes the old file
  }
  print(file_name)
  png(file_name, width = imgWidth, height = imgHeight, res = 150)
  
  # Plot the time series of AverageAntibioticsRealPricePerKg
  plot(df$Date, df[[cols[i]]], type = "l", col = "steelblue", lwd = 2,
       main = mains[i], 
       xlab = "Year", ylab = ylabs[i], xaxt = "n")  # Suppress default x-axis
  year_ticks <- seq(from = min(df$Date), 
                  to = max(df$Date), 
                  by = "year")
  axis(1, at = year_ticks, labels = format(year_ticks, "%Y"), las = 2)  # Rotate labels
  dev.off()
}




#######################################################################

# Make plot of sales and price in a combined plot
file_name <- paste0(output_dir, "sales_vs_price", ".png")
if (file.exists(file_name)) {
  file.remove(file_name)  # Deletes the old file
}
png(file_name, width = imgWidth, height = imgHeight, res = 150)


par(mar=c(5, 4, 4, 5) + 0.1, mfrow=c(1,1))  # (bottom, left, top, right)
plot(df$Date, 
     df$AverageAntibioticsRealPricePerKg, 
     main="Average price and total quantity of antibotics, by month",
     xlab="Year",
     ylab="SEK/kg",
     xaxt = "n",
     type = "l", col = "steelblue", lwd = 2
)
year_ticks <- seq(from = min(df$Date), 
                  to = max(df$Date), 
                  by = "year")
axis(1, at = year_ticks, labels = format(year_ticks, "%Y"), las = 2)  # Rotate labels

# Second plot: units_sold (Secondary Y-Axis)
par(new=TRUE)
plot(df$Date, 
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


################################################################################
# This does not fit here, but I did it to learn myself on the connection between 
# the shape of the curve and the ADF-test. This output is never used in the thesis. 
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
# Now check the differed values : 
adf_test <- ur.df(diff(df$TotalKgActiveSubstanceSold), type = "drift", selectlags = "AIC")  # "drift" allows an intercept
summary(adf_test)
# Now test-statistic is -20.4791, clearly below all tau2 critical values, thus stationary
################################################################################

# Antibiotics per pig #######################################################
png("plots/antibiotic_usage_per_pig.png", width = imgWidth, height = imgHeight, res = 150)
par(mar=c(5, 4, 4, 5) + 0.1, mfrow=c(1,1))  # (bottom, left, top, right)


df$antibiotics_per_pig=1000*1000*df$TotalKgActiveSubstanceSold / df$headCount
plot(df$Date, df$antibiotics_per_pig, type = "l", col = "darkred", lwd = 2,
     main = "Usage of antibiotics per pig, on average, by month", 
     xlab = "Year", ylab ="Milligram", xaxt = "n")  # Suppress default x-axis
year_ticks <- seq(from = min(df$Date), 
                  to = max(df$Date), 
                  by = "year")
axis(1, at = year_ticks, labels = format(year_ticks, "%Y"), las = 2)  # Rotate labels

# Second plot: average cost of antibotics per pig (Secondary Y-Axis)
df$antibiotics_cost_per_pig=df$TotalKgActiveSubstanceSold*df$AverageAntibioticsRealPricePerKg / df$headCount

par(new=TRUE)
plot(df$Date, 
     df$antibiotics_cost_per_pig, 
     axes=FALSE,  # Suppress default axes
     xlab="", ylab="",  
     type = "l", lwd = 2, col="steelblue") 

# Add right-side axis for units sold
axis(side=4)  # Side 4 = right y-axis
mtext("SEK", side=4, line=3)  # Label for second axis

legend("top", legend = c("Left: average usage of antibiotics per pig ", "Right: average cost of antibotics per pig"),
       col = c("darkred", "steelblue" ), lty = 1, lwd = 2, bty = "n")



dev.off()
# Tempting, but wrong, remember OLS assumptions don't hold
#model = lm(antibiotics_per_pig ~ Date, data=df)
#abline(model, col = "darkred", lwd = 2, lty = 2)

# pigs per farm
png("plots/pigs_per_pig.png", width = imgWidth, height = imgHeight, res = 150)

df$pigs_per_farm= df$headCount / df$companyCount
plot(df$Date, df$pigs_per_farm, type = "l", col = "steelblue", lwd = 2,
     main = "Pigs per farm", 
     xlab = "Year", ylab ="Number of pigs per farm", xaxt = "n")  # Suppress default x-axis
year_ticks <- seq(from = min(df$Date), 
                  to = max(df$Date), 
                  by = "year")
axis(1, at = year_ticks, labels = format(year_ticks, "%Y"), las = 2)  # Rotate labels
#model = lm(pigs_per_farm ~ Date, data=df)
#abline(model, col = "darkred", lwd = 2, lty = 2)


dev.off()

df=read.csv("data/antibiotics_sales_data_cleaned.CSV")


#########################################
## Plot the total quantity and sales volume
png(paste0("plots/total_sales_value_and_quanity_of_all_antibiotics.png"), width = imgWidth, height = imgHeight, res = 150)

df_tot = sqldf ("select  year,  round(sum(KgActiveSubstance)) as tot_kg, round(sum(RealPrice)/1000000) as salesvalue from df where year<2025 group by year order by year")

par(mar=c(5, 4, 4, 5) + 0.1, mfrow=c(1,1))  # (bottom, left, top, right)
plot(df_tot$Year, 
     df_tot$salesvalue, 
     main="Total sales value and quantity, by year, of all antibiotics",
     xlab="Year",
     ylab="Million SEK",
     #xaxt = "n",
     type = "l", col = "steelblue", lwd = 2
)

# Second plot: units_sold (Secondary Y-Axis)
par(new=TRUE)
plot(df_tot$Year, 
     df_tot$tot_kg, 
     axes=FALSE,  # Suppress default axes
     xlab="", ylab="",  
     type = "l", lwd = 2, col="darkred")  

# Add right-side axis for units sold
axis(side=4)  # Side 4 = right y-axis
mtext("kg", side=4, line=3)  # Label for second axis

legend("top", legend = c("Left: Sales value", "Right: Sales quanity"),
       col = c("steelblue", "darkred"), lty = 1, lwd = 2, bty = "n")

dev.off()