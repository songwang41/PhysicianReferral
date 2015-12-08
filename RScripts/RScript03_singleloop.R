
#since I parallel computeing need to copy the data multiple times. 
#I decided to copy the following codes manually about 40 times, the result will be obtained in
# half an hour.

#k= 0:39
library(data.table)  
#library(igraph)  
setwd("/afs/cs.wisc.edu/u/s/o/songwang/Stat/Physician_Referral_Network")
RSciptPath <- "./RScripts/"
DataPath <- "./Data/"
ResultsPath <- "./Results/"
PlotsPath <- "./Plots/"

load(paste0(DataPath,"DT1.RData")) ## loading referral and ACS data set\
freq_npi <- table(DT1$NPI)
npi_multi <- as.character(names(freq_npi[freq_npi>1]))
# 391135, have multiple occurrences
npis <- npi_multi[(k*10000+1):(min(length(npi_multi),(1+k)*10000))]

NpiHsaHrr<- list()

NpiHsaHrr$NPI <- npis 
NpiHsaHrr$Hsanum <- numeric(length(npis))
NpiHsaHrr$Hrrnum <- numeric(length(npis))
NpiHsaHrr <- data.frame(NpiHsaHrr)
row.names(NpiHsaHrr) <-as.character(npis)

for ( npi in npis){ 
  data <- DT1[npi]
  freq_hsa <- table(data$HSA)
  NpiHsaHrr[npi,'Hsanum'] <- as.numeric(names(which(freq_hsa==max(freq_hsa))[1]))
  freq_hrr <- table(data$HRR)
  NpiHsaHrr[npi,'Hrrnum'] <- as.numeric(names(which(freq_hrr==max(freq_hrr))[1]))
}

save(NpiHsaHrr, file = paste0(DataPath,"subNetwork/NpiHsaHrr_",k,".RData"))
