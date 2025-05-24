# This program reads the complete merged dataset of antibiotics
# and use it  for data visualization 

rm(list = ls())  # Clear memory
cat("\f") # Clear console
options(scipen=999) # Avoid scientific notation of numbers. 

library(sqldf)
library(writexl)

setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory



df_antibiotics=read.csv("data/antibiotics_sales_data_cleaned.csv")
df_antibiotics$Date = as.Date(df_antibiotics$Date)

names(df_antibiotics)

makeScatterPlotPriceVsKg = function(df, name) {
  package_sizes <- unique(df$PackageSize)
  #colors <- rainbow(length(package_sizes))
  colors <- c("red", "blue", "green3", "orange", "purple", "brown", "cyan", "magenta", "black", "grey")
  color_map <- setNames(colors, package_sizes)
  point_colors <- color_map[df$PackageSize]
  plot(df$KgActiveSubstance, df$RealPrice,
       xlab = "Kg Active Substance",
       ylab = "Real Price",
       main = name,
       pch = 19,       # solid circle
       col =point_colors)   # point color
  legend("topleft",
         legend = package_sizes,
         col = colors,
         pch = 19,
         title = "Package Size",
         cex = 0.8
  )  
}

makeScatterPlotPricePerKgVsTime = function(df, name) {
  package_sizes <- unique(df$PackageSize)
  #colors <- rainbow(length(package_sizes))
  colors <- c("red", "blue", "green3", "orange", "purple", "brown", "cyan", "magenta", "black", "grey")
  color_map <- setNames(colors, package_sizes)
  point_colors <- color_map[df$PackageSize]
  png(paste0(output_dir,name,".png"), width = imgWidth, height = imgHeight, res = imgResolution)
  #png(paste0(output_dir, name, ".png"), width = 900, height = 500, res = 300)
  
  plot(df$Date, df$RealPricePerKgActiveSubstance,
       xlab = "Year",
       ylab = "SEK/kg",
       #xlim=c(as.Date("2005-01-01"), as.Date("2025-01-01")),
       main = paste0(name,", real purchase price per kg of active substance, by package size"),
       pch = 19,       # solid circle
       col = point_colors)   # point color
  legend("topleft",
         legend = package_sizes,
         col = colors,
         pch = 19,
         title = "Package Size",
         bty = "n"
         )  
  dev.off()
}
output_dir <- "plots/packsizes/"
dir.create(output_dir, showWarnings = FALSE)  # Create directory if it doesn't exist
imgWidth = 1600
imgHeight = 1000
imgResolution = 150

df = sqldf("select * from df_antibiotics where ProductName like 'Penovet%'")
makeScatterPlotPriceVsKg(df, "Penovet")

df = sqldf("select * from df_antibiotics where ProductName like 'Ethacilin vet%'")
makeScatterPlotPriceVsKg(df,"Ethacilin vet.")

df = sqldf("select * from df_antibiotics where ProductName like 'Ultrapen%'")
makeScatterPlotPriceVsKg(df,"Ultrapen")


prod_and_size=c(
  "Ethacilin vet., inj-vätska, suspension 300 mg/ml;12 x 100 milliliter;100 milliliter;12 x 250 milliliter;250 milliliter;48 x 100 milliliter",
  "Penovet vet., inj-vätska, suspension 300 mg/ml;5 x 100 milliliter;100 milliliter;250 milliliter",
  "Ultrapen vet, inj-vätska, suspension 300 mg/ml;100 milliliter;12 x 100 milliliter",
  "Hippotrim vet., inj-vätska, lösning 200 mg/ml + 40 mg/ml;100 milliliter;12 x 100 milliliter",
  "Streptocillin vet., inj-vätska, suspension 250 mg/ml + 200 mg/ml;5 x 100 milliliter;100 milliliter",
  "Tylan vet., inj-vätska, lösning 200 mg/ml;100 milliliter",
  "Engemycin vet., inj-vätska, lösning 100 mg/ml;100 milliliter;10 x 100 milliliter"
)
for (i in 1 : length(prod_and_size)) {
  print(Sys.time())
  product=strsplit(prod_and_size[i], ";")[[1]][1]
  sizes=strsplit(prod_and_size[i], ";")[[1]][-1]
  sql = paste0("select * from df_antibiotics where ProductName like '", product, "' and (packagesize = '",sizes[1] ,"' ")
  
  for (j in 2:length(sizes)) {
    sql = paste0(sql, " or packagesize = '",sizes[j] ,"' ")
  }
  sql = paste0(sql, " ) ")
  df = sqldf(sql)
  print (sql)
  product_main_name=strsplit(product, " ")[[1]][1]
  makeScatterPlotPricePerKgVsTime(df, product_main_name)
}






  