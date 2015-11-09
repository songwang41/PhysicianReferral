### 

#network characteristics:
install.packages("igraph")
library(data.table)  
library(igraph)  
setwd("~/Stat/Physician_Referral_Network")
RSciptPath <- "./RScripts/"
DataPath <- "./Data/"
ResultsPath <- "./Results/"
PlotsPath <- "./Plots/"



load(paste0(DataPath,"Network_Hsa0.RData"))

# mean degree 

# mean centrality ( pair-distance )

# mean clustering coefficients

# mean transitivity

# mean 