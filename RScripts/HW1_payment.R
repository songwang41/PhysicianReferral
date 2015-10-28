

rm(list=ls())
library(data.table)  # so fast!
# install.packages('igraph')
library(igraph)  # all the basic graph operations.

##############
setwd("~/Stat/Courses/Physisian_Referral_Network/RScripts/")
DataPath <- "../Data/"
ResultsPath <- "../Results/"
PlotsPath <- "../Plots/"

system.time(load(paste0(DataPath, "EtDT.RData")))
#system.time(load(paste0(DataPath, "Payment.RData")))
system.time(load(paste0(DataPath,"Payment_NPI_total_ca.RData")))


NPI_SF <- DT[City=="SAN FRANCISCO"&State=="CA"] 
Edge_SF <- Et[V1 %in% NPI_SF$NPI]
setkey(NPI_SF,NPI)
NPI_SF = NPI_SF[unique(NPI_SF$NPI), mult="first"]
Edge_SF <- Et[V1 %in% NPI_SF$NPI]

Payment <- Payment_NPI_total_ca[,.(NPI,totalPay,logPrice = log(totalPay+1))]
setkey(Payment,NPI)
Payment_SF <- Payment[NPI %in% NPI_SF$NPI,mult="first"]

high <- quantile(Payment_SF$totalPay,probs = 0.90) 
high_medium <- quantile(Payment_SF$totalPay,probs = 0.70) 
for()
boxplot(Payment$totalPay[Payment$totalPay<high])
## DT physician features for each NPI
## Et physician referral

head(Payment)
unique(Payment$nppes_credentials)
unique(Payment$nppes_entity_code) # I/O
unique(Payment$provider_type) # 90 levels
# gender, address1, address2, city, zip(9), state, country, type#
        