load("./Data/modeldata.RData")
library(randomForest)
fit.rf <- randomForest(vdata1$AVERAGE_SUBMITTED_CHRG_AMT~ ., data= vdata1)






attach(vdata1)


model1 <- lm(log(AVERAGE_SUBMITTED_CHRG_AMT) ~ NPPES_PROVIDER_GENDER + log(LINE_SRVC_CNT)+YRS_EXP 
             +medianGrossRent + medianHHincome+nonWhite+ eligibility+pgRank)
summary(model1)


model2 <- lm(AVERAGE_SUBMITTED_CHRG_AMT ~ NPPES_PROVIDER_GENDER + log(LINE_SRVC_CNT)+YRS_EXP 
             +medianGrossRent + medianHHincome+nonWhite+ eligibility+pgRank)
summary(model2)

boxplot(AVERAGE_SUBMITTED_CHRG_AMT~NPPES_PROVIDER_GENDER)
t.test(AVERAGE_SUBMITTED_CHRG_AMT~NPPES_PROVIDER_GENDER)




plot(vdata1[,1:7], vdata1[1:1000,])



plot(medianHHincome,medianGrossRent)

qqnorm(AVERAGE_SUBMITTED_CHRG_AMT)
qqline(AVERAGE_SUBMITTED_CHRG_AMT)

hist(AVERAGE_SUBMITTED_CHRG_AMT,breaks=100)

x <- rnorm(100)
qqnorm(x)
qqline(x)



####################
library("QuantPsyc")
load("./Data/modeldata.RData")

model_full<-lm(scale(log(AVERAGE_SUBMITTED_CHRG_AMT))~NPPES_PROVIDER_GENDER+scale(LINE_SRVC_CNT)+scale(YRS_EXP)+scale(medianHHincome)+scale(medianGrossRent)+scale(nonWhite)+scale(eligibility)+scale(ge65Poverty)+scale(indegree)+scale(Centrality)+scale(pgRank)+scale(vcount_R)+scale(degMean_R)+scale(degSD_R)+scale(edgeDensity_R)+scale(closeness_R)+scale(transivity_R),data=vdata1)
model1<-lm(AVERAGE_SUBMITTED_CHRG_AMT~NPPES_PROVIDER_GENDER+scale(LINE_SRVC_CNT)+scale(YRS_EXP)
           +scale(eligibility)+scale(medianHHincome)+scale(medianGrossRent)+scale(nonWhite)
           +scale(vcount_R)+scale(degMean_R)+scale(edgeDensity_R)+scale(degSD_R),data=vdata1)
model2<-lm(AVERAGE_SUBMITTED_CHRG_AMT~NPPES_PROVIDER_GENDER+scale(LINE_SRVC_CNT)+scale(YRS_EXP)
           +scale(eligibility)+scale(medianHHincome)+scale(medianGrossRent)+scale(nonWhite)+scale(pgRank)
           +scale(vcount_R)+scale(degMean_R)+scale(edgeDensity_R)+scale(degSD_R),data=vdata1)
model3<-lm(AVERAGE_SUBMITTED_CHRG_AMT~NPPES_PROVIDER_GENDER+scale(LINE_SRVC_CNT)+scale(YRS_EXP)
           +scale(eligibility)+scale(medianHHincome)+scale(medianGrossRent)+scale(nonWhite)+scale(closeness_R)
           +scale(vcount_R)+scale(degMean_R)+scale(edgeDensity_R)+scale(degSD_R),data=vdata1)
model4<-lm(AVERAGE_SUBMITTED_CHRG_AMT~NPPES_PROVIDER_GENDER+scale(LINE_SRVC_CNT)+scale(YRS_EXP)
           +scale(eligibility)+scale(medianHHincome)+scale(medianGrossRent)+scale(nonWhite)
           +scale(Centrality)+scale(vcount_R)+scale(degMean_R)+scale(edgeDensity_R)+scale(degSD_R),data=vdata1)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
stargazer(model1,model2,model3,model4)

########
# analysis plots

listeria$growth[is.na(listeria$growth)]=0
attach(listeria)
library(lattice)
xyplot(LM~Time|percentMoisture,data=listeria,
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.loess(x,y,span=1)
         panel.lmline(x,y,lty=2)
       }
)


#######################
fin


