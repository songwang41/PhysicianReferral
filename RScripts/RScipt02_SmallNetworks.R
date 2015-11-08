##################################READ ME######################
# data: 
# based on national referral network, 
# american community survey provides the billing Zipcode for physicians
# HSA 
# This is script will be used to generate a lot of small network at the HSA level
##############
library(data.table)  
library(igraph)  

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
ZipHsaHrr$zipcode12
DT1 <- DT[which(zipcode %in% ZipHsaHrr$zipcode12),] # remove some zipcodes not belonging to any HSA
zipcode <- substr(DT1$`Zip Code`, 1,5)
DT1$HSA <- ZipHsaHrr[zipcode,'hsanum']
DT1$HRR <- ZipHsaHrr[zipcode,'hrrnum']


# partition the graph based on HSA number or HRR number
length(DT1$NPI) #1915056
length(unique(DT1$NPI)) # 800923, on average, each physician has two billing address

freq_npi <- table(DT1$NPI)
which(freq_npi == max(freq_npi))  
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
for ( npi in names(freq_npi[freq_npi>1])){ 
    data <- DT1[npi]
    freq_hsa <- table(data$HSA)
    NpiHsaHrr[npi,'Hsanum'] <- as.numeric(names(which(freq_hsa==max(freq_hsa))[1]))
    freq_hrr <- table(data$HRR)
    NpiHsaHrr[npi,'Hrrnum'] <- as.numeric(names(which(freq_hrr==max(freq_hrr))[1]))
}

Networks_Hsa <- list()
for( hsa in unique(NpiHsaHrr$Hsanum) ){
  npis <- as.character(NpiHsaHrr[NpiHsaHrr$Hsanum==hsa,]$NPI)
  refer <- Et[which (V1 %in% npis & V2 %in% npis),]
  Networks_Hsa[[as.character(hsa)]] <- refer
}

Networks_Hrr <- list()
for( hrr in unique(NpiHsaHrr$Hrrnum) ){
  npis <- as.character(NpiHsaHrr[NpiHsaHrr$Hrrnum==hrr,]$NPI)
  refer <- Et[which (V1 %in% npis & V2 %in% npis),]
  Networks_Hrr[[as.character(hrr)]] <- refer
}


