### 

#network characteristics:
library(data.table)  
library(igraph)  
setwd("~/Stat/Physician_Referral_Network")
RSciptPath <- "./RScripts/"
DataPath <- "./Data/"
ResultsPath <- "./Results/"
PlotsPath <- "./Plots/"



load(paste0(DataPath,"Network_Hrr.RData"))


Net_Hrr_features <- list()
Net_Hrr_features$HRR = names(Networks_Hrr)

Net_Hrr_features <- data.frame(Net_Hrr_features)

row.names(Net_Hrr_features) <- names(Networks_Hrr)

for(hrr in names(Networks_Hrr) ){
  Edgelist <- Networks_Hrr[[hrr]]
  g <- Edgelist[,.(V1,V2,V3)]
  g <- graph.data.frame(g,directed = "FALSE")
  E(g)$weight <- log(as.numeric(Edgelist$V3))
  g <- simplify(g)
  vc  <- vcount(g)
  ec <- ecount(g)
  clust <-  clusters(g)
  ncomp <- clust$no
  idx_max_comp <- which(clust$csize == max(clust$csize))
  Net_Hrr_features[hrr,'sizeOfcomponent'] <- max(clust$csize[1])/vc
  ## this features are based on the biggest connected component
  
  g2 <- induced.subgraph(g, which(clust$membership==idx_max_comp))
  Net_Hrr_features[hrr,'edgeDensity'] <- 2* ec/vc/(vc-1)
  Net_Hrr_features[hrr,'degMean'] <- mean( degree(g) )
  Net_Hrr_features[hrr,'degSD'] <- sd( degree(g) )
  Net_Hrr_features[hrr,'vcount'] <- vc
  Net_Hrr_features[hrr,'ecount'] <- ec 
  Net_Hrr_features[hrr,'closeness'] <- mean(closeness(g))
  Net_Hrr_features[hrr,'edgeBetweenness'] <- mean(edge.betweenness(g))
  Net_Hrr_features[hrr,'transivity'] <- transitivity(g)
  Net_Hrr_features[hrr,'localClustCoef'] <- mean(transitivity(g,"local"),na.rm = T)
  
}

write.csv(Net_Hrr_features,file= paste0(DataPath,"Net_Hrr_Features.csv"), row.names = F)







