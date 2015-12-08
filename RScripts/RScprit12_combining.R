rm(list=ls())
library(data.table)  
library(igraph)  
#library(sna)
setwd("~/Stat/Physician_Referral_Network")
RSciptPath <- "./RScripts/"
DataPath <- "./Data/"
ResultsPath <- "./Results/"
PlotsPath <- "./Plots/"

#brief:   combining all the features from 4 sourses together.



load("./Data/merged.Rdata")  # merged
load("./Data/acsDataFinal.Rdata") # acsDataFinal

#acsDataFinal$NPPES_PROVIDER_ZIP <- as.numeric(acsDataFinal$NPPES_PROVIDER_ZIP)
acsDataFinal <- as.data.table(acsDataFinal) 
setkey(acsDataFinal, NPPES_PROVIDER_ZIP)
names(acsDataFinal) [5] <- "ge65Total"
names(acsDataFinal) [6] <- "ge65Poverty"
# [1] "NPPES_PROVIDER_ZIP" "medianHHincome"     "medianGrossRent"    "nonWhite"          
# [5] "65plusTotal"        "65plusPoverty"      "vetran65plus"  
merged1 <-merged 
merged$medianHHincome <- acsDataFinal[merged$NPPES_PROVIDER_ZIP,.(medianHHincome)]
merged$medianGrossRent <-  
  acsDataFinal[merged$NPPES_PROVIDER_ZIP,.(medianGrossRent)]
merged$nonWhite <- acsDataFinal[merged$NPPES_PROVIDER_ZIP,.(nonWhite)]
merged$ge65Total <- acsDataFinal[merged$NPPES_PROVIDER_ZIP,.(ge65Total)]
merged$ge65Poverty <- acsDataFinal[merged$NPPES_PROVIDER_ZIP,.(ge65Poverty)]
merged$vetran65plus <- acsDataFinal[merged$NPPES_PROVIDER_ZIP,.(vetran65plus)]


#egonetwork features.
load("./Data/EgoNetwork.RData")
EgoNetwork <- as.data.table(EgoNetwork)
setkey(EgoNetwork,"NPI")
merged$indegree <- EgoNetwork[merged$NPI,.(indegree)]
merged$Centrality <- EgoNetwork[merged$NPI,.(betweenCentrality)]
merged$pgRank <- EgoNetwork[merged$NPI,.(pgRank)]



# local network at Hospital referral, there are just 9000 cases, cannot fit 2000 HSAs
Net_Hrr_Features <- read.csv("./Data/Net_Hrr_Features.csv")
load("./Data/NpiHsaHrr.RData")  ## 
zipHrr <- read.csv("./Data/ZipHsaHrr12.csv")

rownames(Net_Hrr_Features) <- Net_Hrr_Features$HRR # change the rownames, easy to index
merged[["vcount_R"]] <- 
  Net_Hrr_Features[as.character(NpiHsaHrr[merged$NPI,3]),"vcount"]
merged[["degMean_R"]] <- 
  Net_Hrr_Features[as.character(NpiHsaHrr[merged$NPI,3]),"degMean"]
merged[["degSD_R"]] <- 
  Net_Hrr_Features[as.character(NpiHsaHrr[merged$NPI,3]),"degSD"]
merged[["edgeDensity_R"]] <- 
  Net_Hrr_Features[as.character(NpiHsaHrr[merged$NPI,3]),"edgeDensity"]
merged[["closeness_R"]] <- 
  Net_Hrr_Features[as.character(NpiHsaHrr[merged$NPI,3]),"closeness"]
merged[["transivity_R"]] <- 
  Net_Hrr_Features[as.character(NpiHsaHrr[merged$NPI,3]),"transivity"]





save(merged, file= "./Data/finalMerged.RData")
write.csv(merged, file="./Data/finalMerged.csv")
