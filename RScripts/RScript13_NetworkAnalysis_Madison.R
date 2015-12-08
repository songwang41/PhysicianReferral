rm(list =ls())
library(data.table)  
library(igraph)  
#library(sna)
setwd("~/Stat/Physician_Referral_Network")
RSciptPath <- "./RScripts/"
DataPath <- "./Data/"
ResultsPath <- "./Results/"
PlotsPath <- "./Plots/"

################################################# 
#
#calculate the ego-network contrained to some region
#
#################################################



zipHrr <- read.csv("./Data/ZipHsaHrr12.csv")
load("./Data/EdgeSet.RData")
load("./Data/finalMerged.RData")

names(zipHrr)
zipHrr[which(zipHrr$zipcode12 =="53706"),]
NPI_mad <- NpiHsaHrr$NPI[which(NpiHsaHrr$Hrrnum==449)]

Edge_mad <- 

FinalData1_mad<-FinalData1[NPI_mad]
plot(FinalData1_mad[,.(AVERAGE_SUBMITTED_CHRG_AMT,indegree,pgRank,Centrality)])

load("./Data/finalMerged.RData") 
merged <- data.table(merged)
setkey(merged,NPI)
merged_mad<-merged[NPI_mad]
plot(merged_mad[,.(AVERAGE_SUBMITTED_CHRG_AMT,indegree,pgRank,Centrality)])

