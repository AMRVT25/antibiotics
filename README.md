Here in the online appendix, all software is provided for the reader to reproduce the results. If the reader only wants to browse the code and view the results file, that is also fine. The results file referred to in the thesis resides in the results directory. The plots are in the plots directory. 

The following software was used to conduct the analysis:

Operating System: Windows 11  
R version: 4.4.2  
RStudio version: 2024.09.1  

The programs are written in R. They rely on the following libraries: ARDL, beepr, dplyr, ggplot2, openxlsx, RColorBrewer, readxl, rvest, sqldf, stringr, tidyr, tseries, urca, and writexl. If you do not have these packages installed, RStudio will notify you and guide you through the installation process. If you don’t have RStudio and R installed, follow the instructions here:  
https://rstudio-education.github.io/hopr/starting.html

All R programs begin with:  
setwd("C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics")  
To reproduce the results, first download all R files to a directory on your own computer. If you save them in the same path as above, you can run the code directly. If you save them elsewhere, you must update the setwd() path accordingly.

The R files are numbered in the order they are intended to be run. The file 070_merge_sales_data_with_control_data_util.R is a utility file and should not be run directly. Since many R files produce output required by the next file in the sequence, all R scripts should be run in order at least once to ensure that every program can access its required input.

Verification in Stata is done using a single DO file. In that file, change the line:
cd "C:/studier/master/x-job/anitbiotika/gitanalys/antibiotics"
if you saved the file in a different directory.

To run the Stata code, you first need to install the ardl function. Follow the installation instructions provided in Section "8 Programs and Supplemental Material" in:

Kripfganz, S., & Schneider, D. C. (2023). ardl: Estimating autoregressive distributed lag and equilibrium correction models. The Stata Journal, 23(4), 983–1019. https://doi.org/10.1177/1536867X231212434

Installation requires just three Stata commands:  
net sj 23-4  
net install st0734  
net get st0734

If you need any assistance, feel free to contact the author at: AMRVT25@gmail.com  
I will be happy to help.

