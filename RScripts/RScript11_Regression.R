library(data.table)  
library(igraph)  
#library(sna)
setwd("~/Stat/Physician_Referral_Network")
RSciptPath <- "./RScripts/"
DataPath <- "./Data/"
ResultsPath <- "./Results/"
PlotsPath <- "./Plots/"

################################################# 

#calculate the ego-network contrained to some region


load("./Data/Payment2012Cataract66984.RData")
Payment2012Cataract <- as.data.table(Payment2012Cataract)
setkey(Payment2012Cataract,NPI)
table(Payment2012Cataract$PROVIDER_TYPE,Payment2012Cataract$NPPES_ENTITY_CODE)
# very concentrate


length(Payment2012Cataract$NPI)
length(unique(Payment2012Cataract$NPI))
npiCount <- table(Payment2012Cataract$NPI)
max(npiCount) ##=2

Payment2012Cataract["1003805995"]
table(Payment2012Cataract$PLACE_OF_SERVICE)
Pay_2place <- Payment2012Cataract[names(which(npiCount>1))]
table(Pay_2place$PLACE_OF_SERVICE)  ## 569 , 569



## random assign to O/F
FinalData <- Payment2012Cataract[names(which(npiCount==1))]
FinalData1 <- subset(Payment2012Cataract[names(which(npiCount>1))],PLACE_OF_SERVICE=="O")
FinalData <- rbind(FinalData,FinalData1)
PaymentCataract66984 <- FinalData
save(PaymentCataract66984, file="Payment_cleaned_Cataract66984.RData")
rm(PaymentCataract66984)
rm(FinalData1)

clusters <- list()
load("./Data/NpiHsaHrr.RData")
length(which(is.na(match(FinalData$NPI,NpiHsaHrr$NPI) )))  # 3892 missing
FinalData <- FinalData[which(!is.na(match(FinalData$NPI,NpiHsaHrr$NPI)))]
npis <- FinalData$NPI


load("./Data/EgoNetwork.RData")
FinalData1 <- FinalData
FinalData1[["indegree"]] <- EgoNetwork$indegree
FinalData1[["Centrality"]] <- EgoNetwork$betweenCentrality
FinalData1[["pgrank"]] <- EgoNetwork$pgRank


Net_Hrr_Features <- read.csv("./Data/Net_Hrr_Features.csv")
load("./Data/Network_Hrr.RData")

rownames(Net_Hrr_Features) <- Net_Hrr_Features$HRR
FinalData1[["vcount_R"]] <- 
  Net_Hrr_Features[as.character(NpiHsaHrr[FinalData1$NPI,3]),"vcount"]
FinalData1[["degMean_R"]] <- 
  Net_Hrr_Features[as.character(NpiHsaHrr[FinalData1$NPI,3]),"degMean"]
FinalData1[["degSD_R"]] <- 
  Net_Hrr_Features[as.character(NpiHsaHrr[FinalData1$NPI,3]),"degSD"]
FinalData1[["edgeDensity_R"]] <- 
  Net_Hrr_Features[as.character(NpiHsaHrr[FinalData1$NPI,3]),"edgeDensity"]
FinalData1[["closeness_R"]] <- 
  Net_Hrr_Features[as.character(NpiHsaHrr[FinalData1$NPI,3]),"closeness"]
FinalData1[["transivity_R"]] <- 
  Net_Hrr_Features[as.character(NpiHsaHrr[FinalData1$NPI,3]),"transivity"]

####



zipHrr <- read.csv("./Data/ZipHsaHrr12.csv")

names(zipHrr)
zipHrr[which(zipHrr$zipcode12 =="53706"),]
zipHrr[which(zipHrr$hsastate == "WI"),]
#         zipcode12 hsanum hsacity hsastate hrrnum hrrcity hrrstate
# 22256     53706  52048 Madison       WI    449 Madison       WI
NPI_mad <- NpiHsaHrr$NPI[which(NpiHsaHrr$Hrrnum==449)]
setkey(FinalData1,NPI)
FinalData1_mad<-FinalData1[NPI_mad]

plot(FinalData1_mad$AVERAGE_SUBMITTED_CHRG_AMT~FinalData1_mad$indegree)
plot(FinalData1_mad$AVERAGE_SUBMITTED_CHRG_AMT~FinalData1_mad$pgrank)
plot(FinalData1_mad$AVERAGE_SUBMITTED_CHRG_AMT~FinalData1_mad$Centrality)

plot(FinalData1_mad[,.(AVERAGE_SUBMITTED_CHRG_AMT,indegree,pgrank,Centrality)])





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
merged[["medianGrossRent"]] <-  
     acsDataFinal[merged$NPPES_PROVIDER_ZIP,.(medianGrossRent)]
merged[["nonWhite"]] <- acsDataFinal[merged$NPPES_PROVIDER_ZIP,.(nonWhite)]
merged[["65plusTotal"]] <- acsDataFinal[merged$NPPES_PROVIDER_ZIP,.(ge65total)]

merged[["65plusPoverty"]] <- acsDataFinal[merged$NPPES_PROVIDER_ZIP,.(ge65Poverty)]

merged[["vetran65plus"]] <- acsDataFinal[merged$NPPES_PROVIDER_ZIP,"vetran65plus"]



