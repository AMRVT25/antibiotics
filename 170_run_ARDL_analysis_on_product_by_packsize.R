# This program does ARDL analysis on MONTHLY on a couple of time series 
# selected by  ProductName and PackageSize. 
# It both obtains the data and then run the ARDL. 
# The product and sizes are the most common. 
# As calculated by 160_PackageSize_mix_graph.R
# The final output is stored in results/ARDL_models_result_style.xlsx

rm(list = ls())  # Clear memory
cat("\f") # Clear console
options(scipen=1) 

library(ARDL)
library(sqldf)
library(tseries)
library(writexl)
library(beepr)

setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics") # Change working directory
source("070_merge_sales_data_with_control_data_util.R")

output_dir = "plots/monthly_price_vs_quantity/"
dir.create(output_dir, showWarnings = FALSE)  # Create directory if it doesn't exist

output_dir = "plots/monthly_price_vs_quantity/not_significant"
dir.create(output_dir, showWarnings = FALSE)  # Create directory if it doesn't exist
output_dir = "plots/monthly_price_vs_quantity/significant"
dir.create(output_dir, showWarnings = FALSE)  # Create directory if it doesn't exist

makePlot = function(product, packsize, df, srGraphInfo, lrGraphInfo, isSignificant){
  if (isSignificant) {
    file_name=paste0("plots/monthly_price_vs_quantity/significant/",product,packsize,".png")
  }
  else {
    file_name=paste0("plots/monthly_price_vs_quantity/not_significant/",product,packsize,".png")
  }
  
  imgWidth = 1600
  imgHeight = 1000
  png(file_name, width = imgWidth, height = imgHeight, res = 150)
  par(mar=c(5, 4, 4, 5) + 0.1, mfrow=c(1,1))  # (bottom, left, top, right)
  plot(df$Date, 
       df$AverageAntibioticsRealPricePerKg, 
       main=paste0("Price and Quantity of ", product, ", ", packsize, "\n",
                   "SR:",srGraphInfo, ", LR:",lrGraphInfo) ,
       xlab="Year",
       ylab="SEK/kg",
       xaxt = "n",
       type = "l", col = "steelblue", lwd = 2
  )
  year_ticks <- seq(from = min(df$Date), 
                    to = max(df$Date), 
                    by = "year")
  axis(1, at = year_ticks, labels = format(year_ticks, "%Y"), las = 2) 


  par(new=TRUE)
  plot(df$Date, 
       df$TotalKgActiveSubstanceSold, 
       axes=FALSE, 
       xlab="", ylab="",  
       type = "l", lwd = 2, col="darkred")  
 
  axis(side=4)  
  mtext("kg", side=4, line=3)
  
  legend("top", legend = c("Left: Price", "Right: Sales"),
         col = c("steelblue", "darkred"), lty = 1, lwd = 2, bty = "n")
  
  dev.off()
  
}

getStars = function(mp, variableName) {
  stars=""
  if ( mp[mp$Term==variableName, "Pr(>|t|)" ] < 0.1) {
    stars="*"
  } 
  if ( mp[mp$Term==variableName, "Pr(>|t|)" ] < 0.05) {
    stars = "**"
  } 
  if ( mp[mp$Term==variableName, "Pr(>|t|)" ] < 0.01) {
    stars = "***"
  } 
  return (stars)
}

rd=function(n) {
  formatC(n, format = "f", digits = 4)
}

prod_and_size=c(
  "Ethacilin combined;5 most common",
  "Ethacilin vet., inj-vätska, suspension 300 mg/ml;12 x 100 milliliter;100 milliliter;12 x 250 milliliter;250 milliliter;48 x 100 milliliter",
  "Penovet vet., inj-vätska, suspension 300 mg/ml;5 x 100 milliliter;100 milliliter;250 milliliter",
  "Ultrapen vet, inj-vätska, suspension 300 mg/ml;100 milliliter;12 x 100 milliliter",
  "Hippotrim vet., inj-vätska, lösning 200 mg/ml + 40 mg/ml;100 milliliter;12 x 100 milliliter",
  "Streptocillin vet., inj-vätska, suspension 250 mg/ml + 200 mg/ml;5 x 100 milliliter;100 milliliter",
  "Tylan vet., inj-vätska, lösning 200 mg/ml;100 milliliter",
  "Engemycin vet., inj-vätska, lösning 100 mg/ml;100 milliliter;10 x 100 milliliter"
  )

# For testing just one of the products and package size. 
#prod_and_size=c("Ethacilin vet., inj-vätska, suspension 300 mg/ml;100 milliliter",
#                "Penovet vet., inj-vätska, suspension 300 mg/ml;250 milliliter")
# prod_and_size=c(  "Ethacilin vet., inj-vätska, suspension 300 mg/ml;12 x 100 milliliter")

# Create a dataframe to store the results from all the models. 
rownames=c("Product","Size","SR elasticity", "SR pigs", "SR pork price",
           "Time periods", "Lag order", 
           "Bounds f-test p-value",
           "Bounds t-test p-value",
           "LR elasticity", "LR pigs", "LR pork price",
           "Speed of adj",
           "Adjusted R2", "Significant results")
           
           
df_results= data.frame(matrix(nrow = length(rownames) ,  ncol = 0))
rownames(df_results) =rownames
col=0

for (i in 1 : length(prod_and_size)) {
#for (i in 1 : 1) {
  print(Sys.time())
  product=strsplit(prod_and_size[i], ";")[[1]][1]
  product_main_name=strsplit(product, " ")[[1]][1]
  sizes=strsplit(prod_and_size[i], ";")[[1]][-1]
  for (j in 1:length(sizes)) {
    col=col+1
    print(paste0("Processing ", product, " ", sizes[j]))
    df_antibiotics=read.csv("data/antibiotics_sales_data_cleaned.csv")
    if (product == "Ethacilin combined") {
      # Conbined
      #12 x 100 milliliter; 12 x 250 milliliter; 250 milliliter; 48 x 100 milliliter
      df_sales = sqldf(paste0("select * from df_antibiotics where ProductName='Ethacilin vet., inj-vätska, suspension 300 mg/ml' 
                        and (PackageSize='12 x 100 milliliter'  or 
                             PackageSize='12 x 250 milliliter' or 
                             PackageSize='250 milliliter' or 
                             PackageSize='48 x 100 milliliter' )  "))
    }
    else {
      sql=paste0("select * from df_antibiotics where ProductName='",product,"' and PackageSize='",sizes[j],"' ")
      # Need to cut of Hippotrim and Tylan who both have a 
      # huge gap in the time series towards the end
      if (product == "Hippotrim vet., inj-vätska, lösning 200 mg/ml + 40 mg/ml") {
        sql = paste(sql, " and YearMonth<'202007' ")
      }  
      if (product == "Tylan vet., inj-vätska, lösning 200 mg/ml") {
        sql = paste(sql, " and YearMonth<'201902' ")
      }  
      
      df_sales = sqldf(sql)
    }
    name = paste(product, sizes[j])
    name = gsub("/"," per ",name)
    name = gsub(" ","_",name)
    df = merge_sales_and_control_data(df_sales,name)
    print(paste("Length of time series: ", nrow(df)))
    df$Date = as.Date(df$Date)
    
    
    
    models <- auto_ardl(ln_TotalKgActiveSubstanceSold ~ ln_AverageAntibioticsRealPricePerKg + 
                          ln_headCount + ln_RealAvrakningsprisKrPerKg 
                        ,data = df,  max_order = 12, selection = "AIC")
    lags=models$top_orders[1, -ncol(models$top_orders) ]
    str_lags =  paste(lags, collapse = ", ")
    df_results["Lag order", col] = str_lags
    ecm_model <- uecm(models$best_model, case = 3)
    df_results["Adjusted R2", col] = rd(summary(ecm_model)[["adj.r.squared"]])
    print("---------------------------------------------")
    print(name)
    #print(summary(ecm_model))
    print(multipliers(ecm_model, type = "sr") )
    print("---------------------------------------------")
    mp = multipliers(ecm_model, type = "sr")
    # print(mp[mp$Term=="ln_AverageAntibioticsRealPricePerKg","Estimate"])
    

    df_results["Product", col] = product_main_name
    df_results["Size", col] = sizes[j]
    df_results["Time periods", col] = nrow(df)
    
    # These 2 variables are used for the graph
    srGraphInfo=""
    lrGraphInfo=""
    
    labels=c("SR elasticity", "SR pigs", "SR pork price")
    vars=c("ln_AverageAntibioticsRealPricePerKg", "ln_headCount", "ln_RealAvrakningsprisKrPerKg")
    for (L in 1:length(labels)) {
      df_results[labels[L], col] = paste0(
        rd(mp[mp$Term==vars[L], "Estimate" ]),
        getStars(mp, vars[L]),
        "\n(", 
        rd(mp[mp$Term==vars[L], "Std. Error" ]),
        ")"
      )
      if (labels[L]=="SR elasticity") {
        srGraphInfo=paste0(rd(mp[mp$Term==vars[L], "Estimate" ]), getStars(mp, vars[L]))
      }
    }
    
    if (adf.test(df$ln_TotalKgActiveSubstanceSold)[4]<0.05 || 
        adf.test(df$ln_AverageAntibioticsRealPricePerKg)[4]<0.05 ||
        adf.test(df$ln_headCount)[4]<0.05 ||
        adf.test(df$ln_RealAvrakningsprisKrPerKg)[4]<0.05 ) {
      print("**********************************************")
      print("Stationary time series found for: ")
      print(product)
      print(sizes[j])
      # Print p-values
      print(paste("ln_TotalKgActiveSubstanceSold",adf.test(df$ln_TotalKgActiveSubstanceSold)[4] ))
      print(paste("ln_AverageAntibioticsRealPricePerKg",adf.test(df$ln_AverageAntibioticsRealPricePerKg)[4] ))
      print(paste("ln_headCount",adf.test(df$ln_headCount)[4]))
      print(paste("ln_RealAvrakningsprisKrPerKg",adf.test(df$ln_RealAvrakningsprisKrPerKg)[4]))
      print("**********************************************")
      #df_results[row, "Adf_Q" ] = adf.test(df$ln_TotalKgActiveSubstanceSold)[4]
      #df_results[row, "Adf_P" ] = adf.test(df$ln_AverageAntibioticsRealPricePerKg)[4]
    }
    df_results["Bounds f-test p-value", col] = rd(bounds_f_test(ecm_model, case=3)$tab[[2]])
    df_results["Bounds t-test p-value", col] = rd(bounds_t_test(ecm_model, case=3)$tab[[2]])
    if(df_results["Bounds f-test p-value", col]<0.05 & df_results["Bounds t-test p-value", col]<0.05) {
      lrmp = multipliers(ecm_model, type = "lr")
      print("¤¤¤¤¤¤¤¤ long run multipliers ¤¤¤¤¤¤¤¤¤¤¤¤¤¤")
      print(lrmp)
      labels=c("LR elasticity", "LR pigs", "LR pork price")
      #vars=c("ln_AverageAntibioticsRealPricePerKg", "ln_headCount", "ln_RealAvrakningsprisKrPerKg")
      for (L in 1:length(labels)) {
        df_results[labels[L], col] = paste0(
          rd(lrmp[lrmp$Term==vars[L], "Estimate" ]),
          getStars(lrmp, vars[L]),
          "\n(", 
          rd(lrmp[lrmp$Term==vars[L], "Std. Error" ]),
          ")"
        )
        if (labels[L]=="LR elasticity") {
          lrGraphInfo=paste0(rd(lrmp[lrmp$Term==vars[L], "Estimate" ]), getStars(lrmp, vars[L]))
        }
      }
      speed_of_adjustment <- recm(models$best_model, case = 3)$coefficients[["ect"]]
      df_results["Speed of adj", col] = rd(speed_of_adjustment)
    }
    if (grepl("\\*\\*", df_results["SR elasticity", col]) | grepl("\\*\\*", df_results["LR elasticity", col]))   {
      df_results["Significant results", col]  = "YES"   
      makePlot(product_main_name, sizes[j], df, srGraphInfo, lrGraphInfo, TRUE)
    }
    else {
      makePlot(product_main_name, sizes[j], df, srGraphInfo, lrGraphInfo, FALSE)
    }
    
    
  }

}

df_results_with_rownames <- cbind(RowName = rownames(df_results), df_results)


write_xlsx(df_results, "results/ARDL_models_result.xlsx")
write.csv(df_results, "results/ARDL_models_result.csv", row.names = TRUE)

# Write to Excel and preserve newline
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Results")
writeData(wb, "Results", df_results_with_rownames, startCol = 1, startRow = 1)
wrap_style <- createStyle(wrapText = TRUE)
addStyle(
  wb, 
  sheet = "Results", 
  style = wrap_style, 
  rows = 1:(nrow(df_results_with_rownames) + 1),       # +1 for header row
  cols = 1:ncol(df_results_with_rownames), 
  gridExpand = TRUE
)

# Make one more sheet in the Excel file with only signficant results
df_sig=df_results_with_rownames
for (c in ncol(df_sig):1) {  
  if (is.na(df_sig["Significant results", c])) {
    df_sig <- df_sig[, -c]  # Drop the column
  }
}

addWorksheet(wb, "SignificantResults")
writeData(wb, "SignificantResults", df_sig, startCol = 1, startRow = 1)
wrap_style <- createStyle(wrapText = TRUE)
addStyle(
  wb, 
  sheet = "SignificantResults", 
  style = wrap_style, 
  rows = 1:(nrow(df_sig) + 1),       # +1 for header row
  cols = 1:ncol(df_sig), 
  gridExpand = TRUE
)


saveWorkbook(wb, "results/ARDL_models_result_style.xlsx", overwrite = TRUE)



print("done")
beep(2) 


