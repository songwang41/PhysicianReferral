

load("./Data/Payment_a_2013.RData")

## we can focus on a certain type of service, which should only depe
PaymentRed <- Payment[,c("npi","nppes_provider_zip","average_submitted_chrg_amt",
                               "average_Medicare_allowed_amt","average_Medicare_payment_amt")]
system.time(sample(nrow(PaymentRed),1000))
plot(PaymentRed[sample(nrow(PaymentRed),1000),3:5])

## payment = 0.8 * allowed_amount

# # visualize on map 
load("./Data/referEdge.RData")
load("./Data/PhysicianInfo.RData")


library(zipcode)
library(data.table)
data(zipcode)   # this contains the locations of zip codes
PaymentRed <- as.data.table(PaymentRed)
setkey(PaymentRed,npi)

npi_samp <- sample(unique(PaymentRed$npi), 10000)
zip_samp = PaymentRed[npi_samp]$nppes_provider_zip
zip_samp = substr(zip_samp, start = 1, stop = 5)

zipcode = as.data.table(zipcode); setkey(zipcode, zip)  
loc =  zipcode[zip_samp, c("latitude", "longitude"), with = F]
loc = loc[complete.cases(loc)]
loc = data.frame(loc)

### show the geographic positions
library(maps); library(ggplot2)
library(ggmap)

data(zipcode)   # this contains the locations of zip codes
zipcode = as.data.table(zipcode); setkey(zipcode, zip)  
loc1 =  zipcode[zip, c("latitude", "longitude"), with = F]
loc1 = loc1[complete.cases(loc1)]
loc1 = data.frame(loc1)
plot(loc1[,2],loc1[,1], pch=".",col="red")

allowed <- PaymentRed[npi_samp]$average_submitted_chrg_amt
a <- quantile( allowed, probs = c(0.1,0.3,0.7,0.9) )
colors <- character(length(allowed))
colors[which(allowed<=a[1])] <- "black"
colors[which(allowed>a[1])] <- "blue"
colors[which(allowed>a[2])] <- "pink"
colors[which(allowed>a[3])] <- "green"
colors[which(allowed>a[4])] <- "red"

map('usa')
points(loc[,2],loc[,1],col=colors, pch=".")

#map(database = 'country', fill=F, add = T)
sfMap = get_map(location = 'San Francisco', zoom = 12)
ggmap(sfMap) +  geom_point(data=loc,aes(x = longitude, y = latitude, 
                                        position="jitter"),color="red", size=3 )



