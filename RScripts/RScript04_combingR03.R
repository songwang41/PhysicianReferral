
library(data.table)  
  
setwd("~/Stat/Physician_Referral_Network")
RSciptPath <- "./RScripts/"
DataPath <- "./Data/"
ResultsPath <- "./Results/"
PlotsPath <- "./Plots/"

load(paste0(DataPath,"DT1.RData")) ## loading referral and ACS data set\
freq_npi <- table(DT1$NPI)

NpiHsaHrr0<- list()
NpiHsaHrr0$NPI <-  as.character( unique(DT1$NPI) )
NpiHsaHrr0$Hsanum <- numeric(length(unique(DT1$NPI)))
NpiHsaHrr0$Hrrnum <- numeric(length(unique(DT1$NPI)))
NpiHsaHrr0 <- data.frame(NpiHsaHrr0)

row.names(NpiHsaHrr0) <-as.character( unique(DT1$NPI) )
singlenpi <- names(which(freq_npi==1))
singlenpi[1:10]
setkey(DT1,NPI) 

NpiHsaHrr0[singlenpi,] <- DT1[singlenpi,.(NPI,HSA,HRR)]  # reduced half of the loops
npi_multi <- names(freq_npi[freq_npi>1])

for(k in 0:39){
  load(paste0(DataPath,"subNetwork/NpiHsaHrr_",k,".RData"))
  npi <- as.character(NpiHsaHrr$NPI)
  NpiHsaHrr0[npi,"Hsanum"] <- NpiHsaHrr[npi,"Hsanum"]
  NpiHsaHrr0[npi,"Hrrnum"] <- NpiHsaHrr[npi,"Hrrnum"]
}
save(NpiHsaHrr0, file = paste0(DataPath,"NpiHsaHrr.RData"))


rm(NpiHsaHrr)
NpiHsaHrr <- NpiHsaHrr0
rm(NpiHsaHrr0)
NpiHsaHrr <- data.table(NpiHsaHrr)
setkey(NpiHsaHrr,Hsanum)
# Networks_Hsa <- list()
# system.time( for( hsa in unique(NpiHsaHrr$Hsanum)[1:100] ){
#   npis <- as.character(NpiHsaHrr[NpiHsaHrr$Hsanum == hsa,]$NPI)
#   refer <- Et[V1 %in% npis,]
#   refer <- refer[(V2 %in% npis),]
#   Networks_Hsa[[as.character(hsa)]] <- refer
# })
# 
# # save(Et, file= paste0(DataPath,"EdgeSet.RData"))
# # ### 
#load (paste0(DataPath,"NpiHsaHrr.RData"))
#load(paste0(DataPath,"EdgeSet.RData"))
Networks_Hsa <- list()
system.time( for( hsa in unique(NpiHsaHrr$Hsanum) ){
  npis <- as.character(NpiHsaHrr[NpiHsaHrr$Hsanum==hsa,]$NPI)
  refer <- Et[V1 %in% npis ,]
  refer <- refer[V2 %in% npis ,]
  Networks_Hsa[[as.character(hsa)]] <- refer
})
save(Networks_Hsa, file = paste0(DataPath,"Network_Hsa.RData"))



Networks_Hrr <- list()
system.time(
  for( hrr in unique(NpiHsaHrr$Hrrnum)){
  npis <- as.character(NpiHsaHrr[NpiHsaHrr$Hrrnum==hrr,]$NPI)
  refer <- Et[V1 %in% npis ,]
  refer <- refer[V2 %in% npis,]
  Networks_Hrr[[as.character(hrr)]] <- refer
  }
)
save(Networks_Hrr, file = paste0(DataPath,"Network_Hrr.RData"))


