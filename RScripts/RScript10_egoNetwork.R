
library(data.table)  
library(igraph)  
#library(sna)
setwd("~/Stat/Physician_Referral_Network")
RSciptPath <- "./RScripts/"
DataPath <- "./Data/"
ResultsPath <- "./Results/"
PlotsPath <- "./Plots/"

# calculate the ego-network contrained to some region


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


## to see whether there are differences in term of PLACE OF SERVICE
postscript(paste0(PlotsPath,"effects of PLACE_OF_SERVICE.pdf"))
layout(matrix(c(1,2),nrow=1))
boxplot(as.numeric(Pay_2place$AVERAGE_SUBMITTED_CHRG_AMT)~Pay_2place$PLACE_OF_SERVICE)
boxplot(as.numeric(Payment2012Cataract$AVERAGE_SUBMITTED_CHRG_AMT)~Payment2012Cataract$PLACE_OF_SERVICE)
dev.off()

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
# NpiHsaHrr <- NpiHsaHrr0
# save(NpiHsaHrr, file ="./Data/NpiHsaHrr.RData")
a=0
for(hrr in unique(NpiHsaHrr$Hrrnum)){
  cluster_hrr <- as.character(subset(NpiHsaHrr,Hrrnum==hrr)$NPI)
  #a <- a+length(cluster_hrr)
  match
  clusters[[as.character(hrr)]] <- npis[which(npis%in%cluster_hrr)]
  a <- a +length(clusters[[as.character(hrr)]])
}
a  # 12728
### Calculate the ego network characteristics




load("./Data/Network_Hrr.RData")

features <- c("NPI", "indegree", "betweenCentrality","pgRank")
EgoNetwork <- data.frame(matrix(0,nrow(FinalData),length(features)))
names(EgoNetwork) <- features
row.names(EgoNetwork) <- FinalData$NPI
EgoNetwork$NPI <- FinalData$NPI

for( hrr in names(clusters)){
   if (length(clusters[[hrr]])>0){
    egoGraph <- graph.data.frame(Networks_Hrr[[hrr]][,.(V1,V2,V3)])
    Degin <- degree(egoGraph, mode ="in")
    vcount <- vcount(egoGraph)
    between <- betweenness(egoGraph)/((vcount-1)*(vcount-2)/2)
    pgRank <- page.rank (egoGraph)$vector
    cluster_hrr <- clusters[[hrr]][!is.na(match(clusters[[hrr]],names(V(egoGraph)) ))]
    for(npi in cluster_hrr ){
      EgoNetwork[npi,2] <- Degin[npi]
      EgoNetwork[npi,3] <- between[npi]
      EgoNetwork[npi,4] <- pgRank[npi]
      cat(hrr,"npi =", npi, " " ,Degin[npi],"\n")
    }  
  }
}
save(EgoNetwork, file = paste0(DataPath,"EgoNetwork.RData"))
write.csv(EgoNetwork, file = paste0(DataPath,"EgoNetwork.csv"))
hist(log(indegree$X1+1), breaks =50)
  
