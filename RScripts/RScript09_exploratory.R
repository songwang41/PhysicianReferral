



load("./Data/Payment2012Cataract66984.RData")

Payment2012_wi <- subset( Payment2012Cataract, NPPES_PROVIDER_STATE=="WI")
for(i in 20:28){
  Payment2012_wi[,i] <- as.numeric(Payment2012_wi[,i])
}
hist(Payment2012_wi$AVERAGE_SUBMITTED_CHRG_AMT)
hist(Payment2012_wi$AVERAGE_MEDICARE_ALLOWED_AMT,breaks =30)
all(Payment2012_wi$HCPCS_DRUG_INDICATOR=="N")


library(lattice)
#attach(Payment2012_wi)
dat <- Payment2012_wi[c("AVERAGE_MEDICARE_ALLOWED_AMT","AVERAGE_SUBMITTED_CHRG_AMT|NPPES_PROVIDER_CITY")]
a <- log(1+Payment2012_wi$AVERAGE_MEDICARE_ALLOWED_AMT)
xyplot(AVERAGE_MEDICARE_ALLOWED_AMT~AVERAGE_SUBMITTED_CHRG_AMT|NPPES_PROVIDER_CITY,data=Payment2012_wi,
       panel=function(x,y){
         panel.xyplot(x,y)
        # panel.loess(x,y,span=1)
        # panel.lmline(x,y,lty=2)
       }
)

xyplot(AVERAGE_SUBMITTED_CHRG_AMT~(1:length(AVERAGE_SUBMITTED_CHRG_AMT))|NPPES_PROVIDER_CITY,data=Payment2012_wi,
       panel=function(x,y){
         panel.xyplot(x,y)
         # panel.loess(x,y,span=1)
         # panel.lmline(x,y,lty=2)
       }
)

xyplot(AVERAGE_MEDICARE_ALLOWED_AMT~(1:length(AVERAGE_SUBMITTED_CHRG_AMT))|NPPES_PROVIDER_CITY,data=Payment2012_wi,
       panel=function(x,y){
         panel.xyplot(x,y)
         # panel.loess(x,y,span=1)
         # panel.lmline(x,y,lty=2)
       }
)



### we calculate the network in HRR,  calculate the degree for this node 




