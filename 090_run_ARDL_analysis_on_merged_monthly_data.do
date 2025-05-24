clear all
cls
set more off
cd "C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics"

* Import data
import delimited "data/merged_complete_data_of_antibiotics_Ethacilin_shortNames.csv", clear

* Convert date from string to date-format. 
gen date_num = date(date, "YMD")
format date_num %td
replace date_num = date_num if date_num != .
drop date
rename date_num date
order date, first



* Label the variables
label variable date "Date"
label variable q "TotalKgActiveSubstanceSold"
label variable p "AverageAntibioticsRealPricePerKg"
label variable nom_pork_p "AvrakningsprisKrPerKg"
label variable pork_p "RealAvrakningsprisKrPerKg"
label variable pigs "headCount"
label variable farms "companyCount"
label variable ln_p "ln_AverageAntibioticsRealPricePerKg"
label variable ln_q "ln_TotalKgActiveSubstanceSold"
label variable ln_pigs "ln_headCount"
label variable ln_pork_p "ln_RealAvrakningsprisKrPerKg"

* No longer needed, these values are already in the dataset. 
* Generate logarithmic values of number of pigs and pork price
*gen ln_pigs = log(pigs)
*gen ln_pork_p = log(pork_p)

// Setting the time variable
gen year_month = mofd(date)   
format year_month %tm
tsset year_month  

*************************************************************
* make graphs
gen year = year(date)

local do_graphs = 0
if `do_graphs' {  
tsline pigs , ///
    title("Number of pigs") ///
    ytitle("Count") ///   
	xtitle("Time") ///
    lcolor(blue) lpattern(solid)  name(ts_pigs)
	
tsline pork_p , ///
    title("Real Pork Settlement Price") ///
    ytitle("SEK/100kg") ///   
	xtitle("Time") ///   
    lcolor(blue) lpattern(solid)  name(ts_pork_p)

graph combine ts_pigs ts_pork_p, ysize(2) xsize(5)
graph export "stata_plots/ts_pigs_ts_pork_p.png", as(png) replace width(1600)
	
	
	
twoway (line p year_month, yaxis(1) lcolor(blue)) ///
       (line q year_month, yaxis(2) lcolor(red)), ///
       xlabel(, angle(45)) ///
       ytitle("Price", axis(1) color(blue)) ///
       ytitle("Quantity", axis(2) color(red)) ///
       title("Price and Quantity Over Time") ///
       legend(order(1 "Price" 2 "Quantity"))	
graph export "stata_plots/Price_and_Quantity_Over_Time.png", as(png) replace width(1600)
	   
twoway (line ln_p year_month, yaxis(1) lcolor(blue)) ///
       (line ln_q year_month, yaxis(2) lcolor(red)), ///
       xlabel(, angle(45)) ///
       ytitle("ln(Price)", axis(1) color(blue)) ///
       ytitle("ln(Quantity)", axis(2) color(red)) ///
       title("Price and Quantity Over Time, in logarithmic form") ///
       legend(order(1 "Price" 2 "Quantity"))	
graph export "stata_plots/Price_and_Quantity_Over_Time_logs.png", as(png) replace width(1600)

	   
	   
twoway (scatter ln_q ln_p, mcolor(blue)) ///
       (lfit ln_q ln_p, lcolor(red)), ///
       xlabel(, angle(45)) ///
       ytitle("ln_q") xtitle("ln_p") ///
       title("Scatter Plot of ln_q vs ln_p with Regression Line, all years") ///
       legend(order(1 "Data Points" 2 "Regression Line"))
graph export "stata_plots/ln_q_vs_ln_p.png", as(png) replace width(1600)
	   

* Diff plot
*tsline d.ln_q d.ln_p if year > 2005 & year<2008, ///
tsline d.ln_q d.ln_p , ///
       title("Time Series of Δln_q and Δln_p") ///
       ytitle("Δln_q & Δln_p") ///
       xlabel(, angle(45)) ///
       legend(order(1 "Δln_q" 2 "Δln_p"))
graph export "stata_plots/diff_ln_q_vs_ln_p.png", as(png) replace width(1600)
	   
	   
twoway (scatter ln_q ln_p if year == 2024, mcolor(blue)) ///
       (lfit ln_q ln_p if year == 2024, lcolor(red)), ///
       xlabel(, angle(45)) ///
       ytitle("ln_q") xtitle("ln_p") ///
       title("Scatter Plot of ln_q vs ln_p, Year 2024") ///
       legend(order(1 "Data Points" 2 "Regression Line"))	   
graph export "stata_plots/ln_q_vs_ln_p_2024.png", as(png) replace width(1600)

twoway (scatter ln_q ln_p if year == 2010, mcolor(blue)) ///
       (lfit ln_q ln_p if year == 2010, lcolor(red)), ///
       xlabel(, angle(45)) ///
       ytitle("ln_q") xtitle("ln_p") ///
       title("Scatter Plot of ln_q vs ln_p, Year 2010") ///
       legend(order(1 "Data Points" 2 "Regression Line"))		   
graph export "stata_plots/ln_q_vs_ln_p_2010.png", as(png) replace width(1600)

gen qpp = q/pigs
label variable qpp "TotalKgActiveSubstanceSold per Pig"
tsline qpp , ///
    title("Kg Active Substance Per Pig") ///
    ytitle("kg/pig") ///   
	xtitle("Time") ///   
    lcolor(blue) lpattern(solid)  name(ts_pork_p)
graph export "stata_plots/qpp.png", as(png) replace width(1600)

}   /* end do graphs */

************************************************************
* Check if the time series variables are I(0) or I(1)
cls
*keep if date < date("01jan2024", "DMY")
/* 
Om MacKinnon approximate p-value for Z(t) < critical value (till exempel 0.05) så är variabeln stationary. 
Om p-value for Z(t) > critical value så är variabeln non-stationary. 
 */
dfuller p  
dfuller d.p
dfuller q
dfuller d.q


*************************************************************
* ARDL Models

/* START Replicating the results from R */
/* R code to replicate: 
models <- auto_ardl(ln_TotalKgActiveSubstanceSold ~ ln_AverageAntibioticsRealPricePerKg + 
                      ln_headCount + ln_RealAvrakningsprisKrPerKg 
                    ,data = df,  max_order = 24, selection = "AIC")
models$top_orders
summary(models$best_model)
ecm_model <- uecm(models$best_model, case = 3)
summary(ecm_model)
# Compute multipliers from the ECM model, lr ska ge long run elasticity
multipliers(ecm_model, type = "sr") 
multipliers(ecm_model, type = "lr") 

Using the indata from the file merged_complete_data_of_antibiotics_Ethacilin_shortNames.csv
and using the optimal lags as optimised by R */
cls
ardl ln_q ln_p ln_pigs ln_pork_p , lags(5 1 3 1) ec1 
/* This should shows:
ARDL(5,1,3,1) regression

Sample:  2005m6 thru 2024m12                            Number of obs =    235
                                                        R-squared     = 0.4715
                                                        Adj R-squared = 0.4405
Log likelihood = 148.7189                               Root MSE      = 0.1325

------------------------------------------------------------------------------
      D.ln_q | Coefficient  Std. err.      t    P>|t|     [95% conf. interval]
-------------+----------------------------------------------------------------
ADJ          |
        ln_q |
         L1. |  -.0545463   .0481377    -1.13   0.258     -.149414    .0403214
-------------+----------------------------------------------------------------
LR           |
        ln_p |
         L1. |  -2.838242   2.525012    -1.12   0.262    -7.814425     2.13794
             |
     ln_pigs |
         L1. |   1.292388   3.438946     0.38   0.707    -5.484937    8.069712
             |
   ln_pork_p |
         L1. |   2.062487   1.874224     1.10   0.272    -1.631151    5.756125
-------------+----------------------------------------------------------------
SR           |
        ln_q |
         LD. |  -.7679152   .0759141   -10.12   0.000    -.9175235   -.6183069
        L2D. |  -.5870341   .0899035    -6.53   0.000     -.764212   -.4098561
        L3D. |   -.279667   .0871754    -3.21   0.002    -.4514684   -.1078656
        L4D. |  -.2202047   .0667032    -3.30   0.001    -.3516605   -.0887489
             |
        ln_p |
         D1. |  -.5005453   .2360228    -2.12   0.035    -.9656887   -.0354019
             |
     ln_pigs |
         D1. |  -4.011074   6.588471    -0.61   0.543    -16.99535    8.973197
         LD. |   10.97223   8.804555     1.25   0.214    -6.379402    28.32386
        L2D. |  -10.07314   6.341967    -1.59   0.114    -22.57161    2.425332
             |
   ln_pork_p |
         D1. |  -.4315466   .3187133    -1.35   0.177    -1.059653    .1965596
             |
       _cons |  -.4221032    2.94706    -0.14   0.886    -6.230041    5.385835
------------------------------------------------------------------------------

The important part is
 ln_p |
  D1. |  -.5005453   .2360228    -2.12   0.035     -.9656887   -.0354019
where estimate -.5005453  and p-value 0.035 is the same as in the R output  
See file 100_run_ARDL_analysis_on_merged_monthly_data.R
 */

estat ectest
estat ic /* shows the AIC */

/* END Replicating the results from R */

/*************************************************************************/
/*************************************************************************/
/*************************************************************************/
/* Below are other experiments using Stata. They are NOT imporant */
/* Note that the optimal lag selection is different between R and Stata. This is due to 
   that Stata cuts the initial records in the time series in a different way than R. 
   AIC between different max lag runs are NOT comparable. 
*/

* I run the exit command here to stop the execution. 
* Feel free to remove if any of the methods below are of interest. 
exit

cls
* Remove the first part of the data, where the number of pigs changed a lot. 
*keep if date > date("01jan2009", "DMY")



* Case 3
ardl ln_q ln_p ln_pigs ln_pork_p , aic  maxcombs(2000000) maxlags(24) dots ec 
estat ectest
/*Raden nedan använder laggar som beräknats som optimala av R. 
  Resultatet blir inte det samma dock (på grund av ec. Det ska vara ec1 för att det ska bli samma
  ec1 gör att ECM-termen har värden laggade en tidsperiod. ). */
ardl ln_q ln_p ln_pigs ln_pork_p , lags(5 1 3 1)  ec
estat ectest

* Include a time trend (case 5)
* cls
ardl ln_q ln_p ln_pigs ln_pork_p , aic trendvar  maxcombs(2000000) maxlags(24) dots ec 
estat ectest

* Just checking, but ec1 does not change anything. 
cls
ardl ln_q ln_p ln_pigs ln_pork_p , aic  maxcombs(2000000) maxlags(24) dots ec1 
estat ectest

* Just run the pain ARDL, no ECM
cls
ardl ln_q ln_p ln_pigs ln_pork_p , aic maxcombs(2000000) maxlags(24) dots 

**************************************************************
** Experiment: 
* 1) Låsta laggar och bara p och q 
cls
di _N
ardl ln_q ln_p ln_pigs ln_pork_p , lags(5 1 3 1)  /* Samnma som R: ardl ln_q ln_p ln_pigs ln_pork_p , lags(5 1 3 1)  */ 


ardl ln_q ln_p ln_pigs ln_pork_p , lags(20 2 23 8) ec1 
estat ic /* shows the AIC */


cls
ardl ln_q ln_p ln_pigs ln_pork_p , aic  maxcombs(2000000) maxlags(6) dots ec1 
estat ectest
estat ic /* shows the AIC */
display e(aic)


cls
ardl ln_q ln_p ln_pigs ln_pork_p , aic matcrit() maxcombs(2000000) maxlags(6) dots ec1 
estat ectest
estat ic /* shows the AIC */
display e(aic)
display e(lagcombs)


