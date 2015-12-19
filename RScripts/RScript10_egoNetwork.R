
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
Pay_2_place <- Payment2012Cataract[names(which(npiCount>1))]
table(Pay_2_place$PLACE_OF_SERVICE)  ## 569 , 569


## to see whether there are differences in term of PLACE OF SERVICE
postscript(paste0(PlotsPath,"effects of PLACE_OF_SERVICE.pdf"))
layout(matrix(c(1,2),nrow=1))
boxplot(as.numeric(Pay_2_place$AVERAGE_SUBMITTED_CHRG_AMT)~Pay_2_place$PLACE_OF_SERVICE)
title(main = "Summit Amount for physicians with F/O", cex.main=0.7, col.main = "blue")
boxplot(as.numeric(Payment2012Cataract$AVERAGE_SUBMITTED_CHRG_AMT)~Payment2012Cataract$PLACE_OF_SERVICE)
title(main = "Summit Amount for all physicians", cex.main=0.7, col.main = "blue")
dev.off()

## some doctors has two lcations
FinalData <- Payment2012Cataract[names(which(npiCount==1))]
FinalData1 <- subset(Payment2012Cataract[names(which(npiCount>1))],PLACE_OF_SERVICE=="O")
FinalData <- rbind(FinalData,FinalData1)
PaymentCataract66984 <- FinalData
#  save(PaymentCataract66984, file="Payment_cleaned_Cataract66984.RData")
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
  clusters[[as.character(hrr)]] <- npis[which(npis%in%cluster_hrr)]
  a <- a +length(clusters[[as.character(hrr)]])
}
a  # 12728






### Calculate the ego network characteristics
load("./Data/Networks_Hrr.RData")

EgoNetwork <- list()
EgoNetwork$NPI <- FinalData$NPI
nPhysicians <- nrow(FinalData)
EgoNetwork$Degin <- numeric(nPhysicians)
EgoNetwork$between.centrality <-numeric(nPhysicians)
EgoNetwork$close.centrality <- numeric(nPhysicians)
EgoNetwork$alpha.centrality <-numeric(nPhysicians)
EgoNetwork$eigen.centrality <-numeric(nPhysicians)
EgoNetwork$pgRank <-numeric(nPhysicians)
EgoNetwork <- as.data.frame(EgoNetwork)
rownames(EgoNetwork) <- EgoNetwork$NPI
r = 0 
for( hrr in names(clusters)){
   if (length(clusters[[hrr]])>0){
    
    Graph_hrr <- graph.data.frame(Networks_Hrr[[hrr]][,.(V1,V2,V3)])
    Degin_hrr <- degree(Graph_hrr, mode ="in")
    vcount <- vcount(Graph_hrr)
    between.centrality_hrr <- betweenness(Graph_hrr)
    close.centrality_hrr <- closeness(Graph_hrr)
    alpha.centrality_hrr <- alpha.centrality(Graph_hrr,alpha = 0.9)
    eigen.centrality_hrr <- evcent(Graph_hrr)$vector
    pgRank_hrr <- page.rank(Graph_hrr)$vector
    
    cluster_hrr <- clusters[[hrr]]
    cluster_hrr <- cluster_hrr[which(cluster_hrr%in%names(V(Graph_hrr)))]
    
    EgoNetwork[cluster_hrr,'Degin'] <- Degin_hrr[cluster_hrr]
    EgoNetwork[cluster_hrr,'between.centrality'] <- between.centrality_hrr[cluster_hrr]
    EgoNetwork[cluster_hrr,'close.centrality'] <- close.centrality_hrr[cluster_hrr]
    EgoNetwork[cluster_hrr,'alpha.centrality'] <- alpha.centrality_hrr[cluster_hrr] 
    EgoNetwork[cluster_hrr,'eigen.centrality'] <- eigen.centrality_hrr[cluster_hrr]
    EgoNetwork[cluster_hrr,'pgRank'] <- pgRank_hrr[cluster_hrr]
    r = r+1
    cat ("r = ", r, "  hrr = ", hrr, " "," \n")
  }
}

EgoNetwork <- as.data.frame(EgoNetwork)
if(!file.exists("./Data/EgoNetworkFeatures.csv")){
  write.csv(EgoNetwork, file = "./Data/EgoNetworkFeatures.csv", row.names = F)
  save(EgoNetwork, file = "./Data/EgoNetwork.RData")
}
# load("./Data/EgoNetwork.RData")
# data <- read.csv("./Data/EgoNetworkFeatures.csv")
# some analysis on those centrality measures







## Madison Graph visualization
zipHsaHrr <- read.csv("./Data/ZipHsaHrr12.csv")
hrr <- as.character( zipHsaHrr[which(zipHsaHrr$zipcode12 =="53706"),]$hrrnum ) 
#hrr = '449'
EdgeListMadison <- Networks_Hrr[[hrr]]
GraphMadison <- graph.edgelist(as.matrix(EdgeListMadison[,.(V1,V2)])) ## 2621 ,102573

vec.color <- rep("lightblue", vcount(GraphMadison))
vec.color[match(clusters[[hrr]], names(V(GraphMadison)) )] <- "red"


vec.size <- rep(1,vcount(GraphMadison))
eyeDoctorNPI <-  clusters[[hrr]] [ which (clusters[[hrr]]%in%names(V(GraphMadison))) ]
vec.size[eyeDoctorNPI] = 5

plot(GraphMadison,vertex.label = NA, vertex.color = vec.color, 
     vertex.size = vec.size, edge.arrow.size =0)

plot(GraphMadison,layout = layout.fruchterman.reingold, vertex.label = NA, 
     vertex.color = vec.color, vertex.size = vec.size, edge.arrow.size =0)

plot(GraphMadison,layout = layout.kamada.kawai, vertex.label = NA, vertex.color = vec.color, 
     vertex.size = vec.size, edge.arrow.size =0)

vec.size <- evcent(GraphMadison)$vector
plot(GraphMadison,layout = layout.kamada.kawai, vertex.label = NA, vertex.color = vec.color, 
     vertex.size = vec.size, edge.arrow.size =0)



plot(GraphMadison,layout = layout.fruchterman.reingold, vertex.label = NA, vertex.color = vec.color, 
     vertex.size = vec.size, edge.arrow.size =0 )
Deg.Madison <- degree(GraphMadison)
Vname.Madiaon <- names(V(GraphMadison))
Vname.Madiaon[898]
hist(Deg.Madison, breaks =50)
Deg.Madison[eyeDoctorNPI[1]]
Deg.Madison['1114084068']
#Ophthalmologists

eyeDoctorGraph <- induced.subgraph(GraphMadison,eyeDoctorNPI)
str(eyeDoctorGraph)
# igraph.options(vertex.label =NA, edge.arrow.size = 0.1, vertex.size = 5)
plot(eyeDoctorGraph )
hist(degree(eyeDoctorGraph),breaks=10) 


k.nbhds <- graph.neighborhood(GraphMadison, order = 1)
length(k.nbhds)
match.i <- match( eyeDoctorNPI, names(V(GraphMadison)))
# [1]  898  263  161 1714 1998  373  278  277 1461 1026 2281  912  948  700 1528  864 1289  965 1166
# [20] 1372   24  269 1298 2558 2426 1612  123 1925  592 1206 1138 1366 1546  473 1429 1135  261 1430
# [39] 1055 1063 1949 2556  938 2080 1148  351  961 1428  902 1927 1575  678  725   12

pos <- 1055
k.1 <- k.nbhds[[pos]] ## first eye doctor 
ver.color <-rep("lightblue", vcount(k.1))
names(ver.color) <- names(V(k.1))
ver.color[Vname.Madiaon[pos]] <- "red"
ver.size <- rep(5,vcount(k.1))
ver.size <- betweenness(k.1)/ sum(betweenness(k.1))* vcount(k.1) *2
ver.size <- 2* (degree(k.1)^(1/2))
plot(k.1, layout= layout.fruchterman.reingold, vertex.size = ver.size, vertex.color = ver.color)

plot(k.1, layout= layout.kamada.kawai, vertex.size = ver.size, 
     vertex.color = ver.color, edge.arrow.size =0.5)


k.size <- sapply(k.nbhds, vcount)
hist(k.size)
### visualize the whole graph
plot(k.size,Deg.Madison)
eyeDoctorNPI
NpiHsaHrr[eyeDoctorNPI,]
setkey(FinalData, NPI)
eyeDoctor.Madison <- FinalData[eyeDoctorNPI,]
substr(eyeDoctor.Madison$NPPES_PROVIDER_ZIP,start=1, stop=5)
library(acs)
library(zipcode)
data(zipcode)
str(zipcode)
rownames(zipcode) <- zipcode$zip
lat_long <- zipcode[substr(eyeDoctor.Madison$NPPES_PROVIDER_ZIP,start=1, stop=5),][,4:5]

# library(ggmap)
# map <- get_map(location = 'Madison, Wisconsin', zoom=10)
# ggmap(map)  +   geom_point(aes(x = lat_long[,2], y = lat_long[,1], alpha = .5))

library(maps); library(ggplot2)
map('state', region = c('wisconsin'))  # adds an outline
points(lat_long[,2], lat_long[,1])
#There are two wierd points
Deg.Madison["1790704120"]
plot(k.nbhds[[which(names(V(GraphMadison))=="1790704120")]])



##as a whole, visualize





  
