load("./Data/modeldata.RData")
names(vdata1)[1:2] <- c("gender","NPatients")
library(randomForest)

vdata1$gender<- as.factor(vdata1$gender)
fit.rf <- randomForest(AVERAGE_SUBMITTED_CHRG_AMT~ ., data= vdata1 ,importance = T)
submitted <- vdata1$AVERAGE_SUBMITTED_CHRG_AMT


varImpPlot(fit.rf)
importance(fit.rf)

plot(fit.rf$predicted,vdata1$AVERAGE_SUBMITTED_CHRG_AMT-fit.rf$predicted)
sqrt( mean ((fit.rf$predicted - vdata1$AVERAGE_SUBMITTED_CHRG_AMT)^2) )
R2.1 <- 1- sum(((fit.rf$predicted - vdata1$AVERAGE_SUBMITTED_CHRG_AMT)^2))/var(submitted)/(7702)


fit.lm <- lm(AVERAGE_SUBMITTED_CHRG_AMT~ ., data= vdata1)
plot(fit.lm$fitted.values, fit.lm$residuals)
sqrt( mean( (fit.lm$fitted.values)^2 ) )
R2.2 <- 1- sum((fit.lm$residuals)^2)/var(submitted)/(7702)




library(rpart)
fit.rpart <- rpart(AVERAGE_SUBMITTED_CHRG_AMT ~ .,
                   control = rpart.control(xval = 10, minbucket = 2, cp = 0), data=vdata1)
fit.rpart$cptable
pfit.rpart <- prune(fit.rpart, cp = 0.005)
plot(pfit.rpart)
text(pfit.rpart)
fitted <- predict(pfit.rpart,vdata1)

plot(fitted,vdata1$AVERAGE_SUBMITTED_CHRG_AMT-fitted)
sqrt( mean ((fitted - vdata1$AVERAGE_SUBMITTED_CHRG_AMT)^2) )
R2.3 <-  1- sum(((fitted - vdata1$AVERAGE_SUBMITTED_CHRG_AMT)^2))/var(submitted)/(7702)



data <- c(R2.2,R2.3 ,R2.1)
names(data) <- c("regresion","tree","randomForest")
library(xtable)
xtable(data)
######################################

vdata1$NPPES_PROVIDER_GENDER<- as.factor(vdata1$NPPES_PROVIDER_GENDER)
fit.rf <- randomForest(log(AVERAGE_SUBMITTED_CHRG_AMT)~ ., data= vdata1 ,importance = T)
submitted.log <- log(vdata1$AVERAGE_SUBMITTED_CHRG_AMT)


varImpPlot(fit.rf)
importance(fit.rf)

plot(fit.rf$predicted,submitted.log-fit.rf$predicted)
sqrt( mean ((fit.rf$predicted - submitted.log)^2) )
1- sum(((fit.rf$predicted - submitted.log)^2))/var(submitted.log)/(7702)


fit.lm <- lm(log(AVERAGE_SUBMITTED_CHRG_AMT)~ ., data= vdata1)
plot(fit.lm$fitted.values, fit.lm$residuals)
sqrt( mean( (fit.lm$fitted.values)^2 ) )
1- sum((fit.lm$residuals)^2)/var(submitted.log)/(7702)
