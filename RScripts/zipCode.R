set.seed(1)
library(data.table)  # so fast!
library(igraph)  # all the basic graph operations.
library(zipcode)

source("http://pages.stat.wisc.edu/~karlrohe/netsci/code/loadData.R")


#sf = DT[City == "SAN FRANCISCO"]
wa =DT[State == "WA"]

zip = wa$"Zip Code"
zip = substr(zip, start = 1, stop = 5)

library(zipcode)
data(zipcode)  # this contains the locations of zip codes
zipcode = as.data.table(zipcode); setkey(zipcode, zip)  # thanks data.table for making things so fast!  
loc =  zipcode[zip, c("latitude", "longitude"), with = F]
loc = loc[complete.cases(loc)]
loc = as.matrix(loc)
#plot(loc)


library(maps); library(ggplot2)
plot(loc[,2], loc[,1])
map('state', region = c('washington'),fill = F, add = T)  #adds an outline


library(ggmap)
# map <- get_map(location = 'San Francisco, California', zoom=10)
# ggmap(map) +  geom_point(aes(x = loc[,2], y = loc[,1], alpha = .5))

sfMap = get_map(location = 'San Francisco', zoom = 10)
ggmap(sfMap) +  geom_point(aes(x = loc[,2], y = loc[,1], alpha = .5))
# map(database = 'county', region = c('san francisco'), add = T)  # adds an outline



library(geosphere)
# so ugly and so fast!

samp =   DT$NPI[sample(dim(DT)[1], 10000)]  # take a random sample of NPI's. 
DTsamp = DT[samp,mult ="first"]
dim(DTsamp)
DTsamp = DTsamp[complete.cases(DTsamp$"Zip Code")]
dim(DTsamp)
setkey(DTsamp, NPI)
tmp = Et[DTsamp$NPI]
Esamp = tmp[complete.cases(tmp)]  #lots of NA's.  Have not inspected why.
Esamp=as.matrix(Esamp)[,1:2] #igraph needs the edgelist to be in matrix format


ed = distGeo(
    zipcode[
        substr(
            DT[Esamp[,1], mult = "first"]$"Zip Code" ,start = 1, stop = 5
        )
        , c("longitude", "latitude"), with = F] 
    , zipcode[substr(DT[Esamp[,2], mult = "first"]$"Zip Code" ,start = 1, stop = 5), c("longitude", "latitude"), with = F] 
)/1000
mean(ed ==0, na.rm = T)


# How do the distribution of referral distances vary between providers?  
# Let's study it using some characteristics of the provider's zip code from the ACS.
library(acs)
source('~/dataFiles/physicianReferral/acskey.R')  # this loads my acs key



edgeZip = cbind(
    substr(DT[Esamp[,1], mult = "first"]$"Zip Code",1,5),
    substr(DT[Esamp[,2], mult = "first"]$"Zip Code",1,5)
)
names(which.max(table(Esamp[,1])))


us.zip=geo.make(zip.code = "*")

# B01002 is the median age
acsdat = acs.fetch(geography=us.zip, table.number="B01002", col.names="pretty")  
acsdat = estimate(acsdat)
# for fast call from data table, make zip code a column instead of a row name
acsDT = as.data.table(cbind(substr(rownames(acsdat),start=7, stop =11),acsdat))  
setnames(acsDT, "V1", "zip")
setkey(acsDT, zip)


x = acsDT[edgeZip[,1]]$"Median Age by Sex:  Median age -- Total: "
y = acsDT[edgeZip[,2]]$"Median Age by Sex:  Median age -- Total: "
plot(x,y, pch = '.')
abline(0,1)

library(MASS)
X = cbind(as.numeric(x), as.numeric(y))
X = X[complete.cases(X),]
X = X[X[,1]!=X[,2],]
contour(kde2d(X[,1],X[,2]))