

# calculate the ego-network contrained to some region


load("./Data/Payment2012Cataract66984.RData")

setkey(Payment2012Cataract,NPI)
table(Payment2012Cataract$PROVIDER_TYPE,Payment2012Cataract$NPPES_ENTITY_CODE)
#                                       I     O
# Ambulatory Surgical Center            0  1902
# Clinical Laboratory                   0     2
# Emergency Medicine                    1     0
# Family Practice                       5     0
# General Surgery                       2     0
# Hospice and Palliative Care           1     0
# Internal Medicine                     5     0
# Neurology                             1     0
# Obstetrics/Gynecology                 1     0
# Ophthalmology                     10093     1
# Optometry                          5164     1
# Osteopathic Manipulative Medicine     2     0
# Otolaryngology                        7     0
# Pathology                             1     0

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
  clusters[[as.character(hrr)]] <- cluster_hrr[!is.na(match(cluster_hrr,npis))]
  a <- a +length(clusters[[as.character(hrr)]])
}

### Calculate the ego network characteristics

library(foreach)
library(doParallel)
library(igraph)

load("./Data/Network_Hrr.RData")


EgoNetwork <- list()
indegree <- data.frame(matrix(0,nrow(FinalData),3))
row.names(indegree) <- FinalData$NPI
for( hrr in names(clusters)){
  if (length(clusters[[hrr]])>0){
    egoGraph <- graph.data.frame(Networks_Hrr[[hrr]][,.(V1,V2)])
    Degin <- degree(egoGraph)
    vcount(egoGraph)
    for(npi in clusters[[hrr]] ){
      indegree[npi,2] <- as.numeric(hrr)
      indegree[npi,1] <- Degin[npi]
      cat(Degin[npi],"\n")
    }  
  }
}




load("./Data/Network_Hrr.RData")
a <-  FinalData$NPI[1]
hrrnum <- as.character(NpiHsaHrr[a,][3])
egoNet <- Networks_Hrr[[hrrnum]]
egoGraph <- graph.data.frame(egoNet[,1:2,with=F],directed = T)
V(egoGraph)
hist(degree(egoGraph))
  
