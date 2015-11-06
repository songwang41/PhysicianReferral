library(data.table)  # so fast!
# install.packages('igraph')
library(igraph)  # all the basic graph operations.


##################################READ ME######################
## This file reads in the 
## "physician-referrals-2015-days365.txt" in the Data folder
## "physician_compare/National_Downloadable_File.csv"

##############
setwd("~/Stat/Courses/Physisian_Referral_Network/RScripts/")
DataPath <- "../Data/"
ResultsPath <- "../Results/"
PlotsPath <- "../Plots/"


Et = fread(paste0(DataPath, 
        "physician-referrals-2015-days365/physician-referrals-2015-days365.txt"),
         sep = ",", 
         colClasses = c("character", "character","numeric", "numeric", "numeric"))
setkey(Et, V1)
head(Et)
b= c(rep("character", 6),rep("factor",4), "numeric", rep("factor",6), "character", 
     "character", "character", "numeric", rep("character",2), "factor", "character", 
     "factor", "character", rep("character", 10), rep("factor", 6))
DT = fread(paste0(DataPath,"physician_compare/National_Downloadable_File.csv"),colClasses = b)
setkey(DT, NPI)
rm(b)
Et = Et[,.(V1,V2,V3)] 


library("openxlsx") # mydf <- read.xlsx("BigExcelFile.xlsx", sheet = 1, startRow = 2, colNames = TRUE)

filename = paste0(DataPath,
   "Medicare_Provider_Util_Payment_PUF_a_CY2013/Medicare_Provider_Util_Payment_PUF_a_CY2013.xlsx")
Payment <- read.xlsx(filename, sheet = 1, startRow = 2, colNames = TRUE)


save(DT,Et, file = paste0(DataPath, "EtDT.RData"))

save(Payment,file = paste0(DataPath, "Payment.RData"))



