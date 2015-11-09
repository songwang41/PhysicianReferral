##################################READ ME######################
# data: 
# based on national referral network, 
# american community survey provides the billing Zipcode for physicians
# HSA 
# This is script will be used to generate a lot of small network at the HSA level
##############

library(data.table)  
#library(igraph)  
setwd("getwd()""/afs/cs.wisc.edu/u/s/o/songwang/Stat/Physician_Referral_Network")
RSciptPath <- "./RScripts/"
DataPath <- "./Data/"
ResultsPath <- "./Results/"
PlotsPath <- "./Plots/"


#why it is hard to read .xls
# ZipHsaHrr1 = fread(paste0(DataPath,"ZipHsaHrr12.xls"),
#            colClasses = c("numeric", "numeric","character", "character", 
#                           "numeric","character","character"))
ZipHsaHrr <- read.csv(paste0(DataPath,"ZipHsaHrr12.csv"))
row.names(ZipHsaHrr) <- ZipHsaHrr$zipcode12
ZipHsaHrr["60480",]
sort(unique(ZipHsaHrr$hrrstate)) # 50 states +DC
load(paste0(DataPath,"EtDT2013.RData")) ## loading referral and ACS data set


# add two extra columns in ACS corresponding to their zipcode
zipcode <- substr(DT$`Zip Code`,start=1, stop=5)
DT1 <- DT[which(zipcode %in% ZipHsaHrr$zipcode12),] # remove some zipcodes not belonging to any HSA
rm(DT)
zipcode <- substr(DT1$`Zip Code`, 1,5)
DT1$HSA <- ZipHsaHrr[zipcode,'hsanum']
DT1$HRR <- ZipHsaHrr[zipcode,'hrrnum']


# partition the graph based on HSA number or HRR number
length(DT1$NPI) #1915056
length(unique(DT1$NPI)) # 800923, on average, each physician has two billing address

freq_npi <- table(DT1$NPI)
names ( which(freq_npi == max(freq_npi))   )
data <- DT1[NPI == "1760561781",]  
data[,c(`City`,'State',`Zip Code`)]
sum(freq_npi>1)
## some physician has hundreds of zipcodes, we assign the hosptial region of this
# eg. 1760561781 559 lines
# HSA number tracking backing frequency 60 Huston, 52 St Antonio, 37 Dallas. He has services in three cities.

# for simplicity, assign the physician to HSA based plurality

NpiHsaHrr<- list()
NpiHsaHrr$NPI <- as.character( unique(DT1$NPI) )
NpiHsaHrr$Hsanum <- numeric(length(unique(DT1$NPI)))
NpiHsaHrr$Hrrnum <- numeric(length(unique(DT1$NPI)))
NpiHsaHrr <- data.frame(NpiHsaHrr)
row.names(NpiHsaHrr) <-as.character( unique(DT1$NPI) )
singlenpi <- names(which(freq_npi==1))
singlenpi[1:10]
setkey(DT1,NPI) 
NpiHsaHrr[singlenpi,] <- DT1[singlenpi,.(NPI,HSA,HRR)]  # reduced half of the loops
npi_multi <- names(freq_npi[freq_npi>1])
# to narrow down the range of NPI's. looked at payment 

library(foreach)
library(doParallel)
# cl <- makeCluster(10)
# registerDoParallel(cl)
# 
# 
# iters <-1e3
# seq (from = 1,to = 1999,by=2)
# strt <-Sys.time()
# ls <- foreach(i = seq (from = 1,to = 999,by=2)) %dopar%{
#   to.ls <- rnorm(1e4)
#   to.ls <- summary(to.ls)
#   to.ls
# }
# print(Sys.time() - strt)
# stopCluster(cl)
# ls
cl <- makeCluster(10)
registerDoParallel(cl)
strt <- Sys.time()
npihsahrr <- foreach(npi = npi_multi[1:100]) %dopar%{
  data <- DT1[npi]
  freq_hsa <- table(data$HSA)
  hsanum<- as.numeric(names(which(freq_hsa==max(freq_hsa))[1]))
  freq_hrr <- table(data$HRR)
  hrrnum <- as.numeric(names(which(freq_hrr==max(freq_hrr))[1]))
  a<- c(npi,hsanum, hrrnum)
  a
}
print(Sys.time() - strt)
stopCluster(cl)

for ( npi in npi_multi[1:1000]){ 
    data <- DT1[npi]
    freq_hsa <- table(data$HSA)
    NpiHsaHrr[npi,'Hsanum'] <- as.numeric(names(which(freq_hsa==max(freq_hsa))[1]))
    freq_hrr <- table(data$HRR)
    NpiHsaHrr[npi,'Hrrnum'] <- as.numeric(names(which(freq_hrr==max(freq_hrr))[1]))
}
Save(NpiHsaHrr, file = paste0(DataPath,"NpiHsaHrr.RData"))


Networks_Hsa <- list()
for( hsa in unique(NpiHsaHrr$Hsanum) ){
  npis <- as.character(NpiHsaHrr[NpiHsaHrr$Hsanum==hsa,]$NPI)
  refer <- Et[which (V1 %in% npis & V2 %in% npis),]
  Networks_Hsa[[as.character(hsa)]] <- refer
}

save(Network_Hsa, file = paste0(DataPath,"Network_Hsa.RData"))
Networks_Hrr <- list()
for( hrr in unique(NpiHsaHrr$Hrrnum) ){
  npis <- as.character(NpiHsaHrr[NpiHsaHrr$Hrrnum==hrr,]$NPI)
  refer <- Et[which (V1 %in% npis & V2 %in% npis),]
  Networks_Hrr[[as.character(hrr)]] <- refer
}
save(Network_Hrr, file = paste0(DataPath,"Network_Hrr.RData"))

