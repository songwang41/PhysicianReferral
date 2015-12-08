
rm(list =ls())
library(data.table)  
library(igraph)  
#library(sna)
setwd("~/Stat/Physician_Referral_Network")
RSciptPath <- "./RScripts/"
DataPath <- "./Data/"
ResultsPath <- "./Results/"
PlotsPath <- "./Plots/"

####################################################


load("./Data/finalMerged.RData")
load("./Data/EdgeSet.RData")
zipHsaHrr <- read.csv("./Data/ZipHsaHrr12.csv")

load("./Data/Payment2012Cataract66984.RData")
freq <- table(Payment2012Cataract$PLACE_OF_SERVICE)
dat1 <- which(freq==2)
setkey(Payment2012Cataract,NPI)
merged$Allowed <- Payment2012Cataract[merged$NPI,.(AVERAGE_MEDICARE_ALLOWED_AMT)]
