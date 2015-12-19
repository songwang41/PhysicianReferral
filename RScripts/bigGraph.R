\

# now we consider the problem totally from different angle; we don't use the domain knowledge,
# instead, we ask machine learning algorithm to tell us what kind of variables of predicting power

#feature preparation:


#Part 1: physician features: 
load("./Data/finalMerged.RData")
load("./Data/Payment2012Cataract66984.RData")
load("./Data/PhysicianInfo.RData")
load("./Data/refer.RData")
# 48 states and DC  
State.names <- names(table(PhysicianInfo$State))
table(PhysicianInfo$State) <10000
rm.names <- c('AS', 'GU', 'MP', 'PR', 'AK', 'HI', 'VI') 
lower.states <- State.names[which(!State.names%in%rm.names)]
length(lower.states)
Physician.lowerUS <- subset(PhysicianInfo, State%in%lower.states)
freq.npis <- table(Physician.lowerUS$NPI)
hist(freq.npis)

library(data.table)
Physician.lowerUS <- as.data.table(Physician.lowerUS)
setkey(Physician.lowerUS, NPI)
PhysicianNPIs <- unique(Physician.lowerUS$NPI)
refer1 <- refer[refer[,.(V1)]%in% PhysicianNPIs,]
refer1 <- refer1[refer[,.(V2)]%in% PhysicianNPIs,]

# guess the graph is too big, I cannot hand it as a whole, 
