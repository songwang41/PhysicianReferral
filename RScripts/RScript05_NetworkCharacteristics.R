### 

#network characteristics:
library(data.table)  
library(igraph)  
setwd("~/Stat/Physician_Referral_Network")
RSciptPath <- "./RScripts/"
DataPath <- "./Data/"
ResultsPath <- "./Results/"
PlotsPath <- "./Plots/"



load(paste0(DataPath,"Network_Hsa.RData"))


Net_Hsa_features <- list()
Net_Hsa_features$HSA = names(Networks_Hsa)
# Net_Hsa_features$vcount <- numeric(length(Networks_Hsa)) 
# Net_Hsa_features$degMean <- numeric(length(Networks_Hsa)) 
# Net_Hsa_features$degSD <- numeric(length(Networks_Hsa)) 
# Net_Hsa_features$edgeDensity <- numeric(length(Networks_Hsa))
# Net_Hsa_features$transivity <- numeric(length(Networks_Hsa))
# Net_Hsa_features$sizeOfcomponent <- numeric(length(Networks_Hsa))
# Net_Hsa_features$clusteringCoef <- numeric(length(Networks_Hsa))
# Net_Hsa_features$nComp <- numeric(length(Networks_Hsa))
Net_Hsa_features <- data.frame(Net_Hsa_features)

row.names(Net_Hsa_features) <- names(Networks_Hsa)

for(hsa in names(Networks_Hsa) ){
  Edgelist <- Networks_Hsa[[hsa]]
  g <- Edgelist[,.(V1,V2,V3)]
  g <- graph.data.frame(g,directed = "FALSE")
  E(g)$weight <- log(as.numeric(Edgelist$V3))
  g <- simplify(g)
  vc  <- vcount(g)
  ec <- ecount(g)
  clust <-  clusters(g)
  ncomp <- clust$no
  idx_max_comp <- which(clust$csize == max(clust$csize))
  Net_Hsa_features[hsa,'sizeOfcomponent'] <- max(clust$csize[1])/vc
  ## this features are based on the biggest connected component
  
  g2 <- induced.subgraph(g, which(clust$membership==idx_max_comp))
  Net_Hsa_features[hsa,'edgeDensity'] <- 2* ec/vc/(vc-1)
  Net_Hsa_features[hsa,'degMean'] <- mean( degree(g) )
  Net_Hsa_features[hsa,'degSD'] <- sd( degree(g) )
  Net_Hsa_features[hsa,'vcount'] <- vc
  Net_Hsa_features[hsa,'ecount'] <- ec 
  Net_Hsa_features[hsa,'closeness'] <- mean(closeness(g))
  Net_Hsa_features[hsa,'edgeBetweenness'] <- mean(edge.betweenness(g))
  Net_Hsa_features[hsa,'transivity'] <- transitivity(g)
  Net_Hsa_features[hsa,'localClustCoef'] <- mean(transitivity(g,"local"),na.rm = T)
  
}

write.csv(Net_Hsa_features,file= paste0(DataPath,"Net_Hsa_Features.csv"), row.names = F)
clustCoef <- function(g){
  if (vcount(g)< 5000){
    
  }
}




 


