library(data.table)  # so fast!
library(igraph)  # all the basic graph operations.


##################################READ ME######################
## This file reads in the 
## "physician-referrals-2015-days365.txt" in the Data folder
## "physician_compare/National_Downloadable_File.csv"

##############
#"/Users/SongWang/Stat/Physician_Referral_Network/RScripts"
setwd("./RScripts/")
DataPath <- "../Data/"
ResultsPath <- "../Results/"
PlotsPath <- "../Plots/"

## 2012 data set  ## nrow = 73 M , ncol =5, 
## length(unique(Et$V1))  #1004294
## length(unique(Et$V2))  #1011848
Et = fread(paste0(DataPath, 
        "Physician-referrals/Physician-Referrals-2012-2013-DAYS30.txt"),
         colClasses = c("character", "character","numeric", "numeric", "numeric"))

setkey(Et, V1)
head(Et)


#           V1         V2   V3         V4          V5
# 1: 1000000004 1548295421 20         12          9
# 2: 1000000004 1528009412 136        32         36
# 3: 1000000004 1972546315 31         18         18
# 4: 1000000004 1376577247 31         20         21
# 5: 1000000004 1790775229 54         44         47
# 6: 1000000004 1558653212 20         11          9





### 2013 data set, the newest version is 2015 now.
#length(unique(DT$NPI))   #898620
# nrow = 2.1 M, ncol =43
b= c(rep("character", 6),rep("factor",4), "numeric", rep("factor",6), "character", 
     "character", "character", "numeric", rep("character",2), "factor", "character", 
     "factor", "character", rep("character", 10), rep("factor", 6))
DT = fread(paste0(DataPath,"physician_compare/National_Downloadable_File.csv"),colClasses = b)
setkey(DT, NPI)
rm(b)
Et = Et[,.(V1,V2,V3)] 







library("openxlsx") # mydf <- read.xlsx("BigExcelFile.xlsx", sheet = 1, startRow = 2, colNames = TRUE)


#subset with physician's last name starts with a
filename = paste0(DataPath,
   "Medicare_Provider_Util_Payment_PUF_a_CY2013/Medicare_Provider_Util_Payment_PUF_a_CY2013.xlsx")
Payment <- read.xlsx(filename, sheet = 1, startRow = 2, colNames = TRUE)
#nrow = 405 K, ncol = 28
# 37263 unique NPI


charges<- cbind(Payment$average_submitted_chrg_amt,
                Payment$average_Medicare_allowed_amt,Payment$average_Medicare_payment_amt)
names(charges) <- c("average_submitted_chrg_amt","average_Medicare_allowed_amt","average_Medicare_payment_amt")


save(DT,Et, file = paste0(DataPath, "EtDT2013.RData"))


save(Payment,file = paste0(DataPath, "Payment2013.RData"))



