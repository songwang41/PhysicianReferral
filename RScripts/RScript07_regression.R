
#network characteristics:
library(data.table)  
library(igraph)  
setwd("~/Stat/Physician_Referral_Network")
RSciptPath <- "./RScripts/"
DataPath <- "./Data/"
ResultsPath <- "./Results/"
PlotsPath <- "./Plots/"



total<-fread("./Data/regression/totalAllowedPay.csv",header=T)
variable<-fread("./Data/regression/variables.csv",header=T)
variable <- variable[,c(-1,-3),with=F] # remove row.names, HSA names


names(variable)
# [1] "HSA Number"                                                                                      
# [2] "Medicare enrollees (2012)"                                                                       
# [3] "Total Mortality: ASR-adjusted % of deaths among Medicare enrollees (2012)"                       
# [4] "Non-HMO Mortality: ASR-adjusted % of deaths among Medicare enrollees without HMO coverage (2012)"
# [5] "Number_of_beneficiaries"                                                                         
# [6] "Average_annual_percent_visit"                                                                    
# [7] "Medicare_enrollees"                                                                              
# [8] "Total_Medicare_reimbursement_per_enrollee"                                                       
# [9] "Hospital_skill_nursing"                                                                          
# [10] "Pyhsician_reimbursements_perenrollee"                                                            
# [11] "Outpatient_facility_reimbursements_perenrollee"                                                  
# [12] "Durable_medical_equipment_reimbursements_perenrollee"                                            
# [13] "Resident_Population"                                                                             
# [14] "Acute_care_hospital_beds_per1000resident"                                                        
# [15] "Hospital-based_Registered_nerses_per1000residents"                                               
# [16] "FTE_Hospital_Employees_per1000residents"

names(variable) <- c("HSA","enrollees","Mortality","Mortality2","Nbeneficiaries","AvgAnnualVisit"
                      , "enrolle2","totalReimbursePerEnrollee","skill_nursing","ReimbursePerEnrollee1",
                      "ReimbursePerEnrollee2","ReimbursePerEnrollee3","Resident_Population",
                      "hospital_beds","Registered_nerses_ratio","Hospital_Employees_ratio")

network<-fread("./Data/regression/Net_Hsa_Features.csv",header=T)

pos<-match(total$Group.1,network$HSA)
which((is.na(pos)))  
#[1] 201 543 869   ## HSA in total, but not in network $HSA
name1 <- names(network)[-1]
for(i in 1:length(total$Group.1)){
  total[i,name1]=network[pos[i],2:11,with=F]
}
name2<- names(variable)[-1]
pos2<-match(total$Group.1,variable$HSA)
for(j in 1:length(total$Group.1)){
  total[j,name2]=variable[pos2[j],2:16,with=F]
}

t1<-total
factortonumeric <- function(x){
  if(!is.numeric(x) ){ x <- as.numeric(x)}
  return(x)
}
removecomma<- function(x){
  x<- as.character(x)
  x <- gsub(",", "",x )
  return(x)
}
missingIdx <- function(t1){
  if(!is.data.frame(t1)) {
    cat("not a data frame \n")
    return (NULL)
  }
  idx_na<-NULL
  for ( i in 1:nrow(t1)){
      if(any(is.na(t1[i,]))) {idx_na<-rbind(idx_na,i)}
  }
  return(idx_na)
}
t1<-data.frame(t1)
idx_na <- missingIdx(t1)
t1 <- t1[-idx_na,]
t2 <- apply(t1, MARGIN=2, FUN = removecomma)
t3 <- apply(t2, MARGIN=2, FUN = factortonumeric)
t3<-data.frame(t3)


row.names(t3)<-t3$Group.1
t3 <- t3[,-c(1,2)]
names(t3)[1] <- "TotalAllowedPayment"
t3


# log transformation on payment
lm1 <- lm(log(TotalAllowedPayment+1)~.,data=t3)
library(MASS)
step <- stepAIC(lm1, direction="both")
summary(step)

postscript(file=paste0(PlotsPath,"regression_diagonosis_plot.pdf"))
par(mfcol=c(2,2))
plot(step)
dev.off()



# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               5.220e+00  3.612e-01  14.452  < 2e-16 ***
#   edgeDensity              -1.988e+00  1.988e-01 -10.001  < 2e-16 ***
#   degMean                   2.712e-02  4.713e-03   5.754 9.92e-09 ***
#   degSD                     2.027e-02  4.712e-03   4.301 1.77e-05 ***
#   vcount                    1.736e-03  1.915e-04   9.065  < 2e-16 ***
#   ecount                   -9.058e-05  7.192e-06 -12.595  < 2e-16 ***
#   localClustCoef            7.540e-01  2.139e-01   3.525 0.000431 ***
#   enrollees                -8.892e-06  5.218e-06  -1.704 0.088476 .  
# Mortality                -5.992e-02  4.188e-02  -1.431 0.152637    
# Nbeneficiaries           -1.563e-04  6.216e-05  -2.515 0.011986 *  
#   AvgAnnualVisit            1.210e-02  3.017e-03   4.011 6.23e-05 ***
#   enrolle2                  1.852e-04  6.367e-05   2.909 0.003667 ** 
#   skill_nursing             1.402e-04  3.551e-05   3.948 8.13e-05 ***
#   ReimbursePerEnrollee1     4.319e-04  5.774e-05   7.479 1.07e-13 ***
#   ReimbursePerEnrollee2    -3.333e-04  7.664e-05  -4.348 1.43e-05 ***
#   Resident_Population       1.839e-06  5.165e-07   3.560 0.000379 ***
#   hospital_beds            -6.334e-02  4.037e-02  -1.569 0.116760    
# Registered_nerses_ratio   8.901e-02  3.044e-02   2.924 0.003489 ** 
#   Hospital_Employees_ratio -2.177e-02  7.500e-03  -2.902 0.003738 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.109 on 2238 degrees of freedom
# Multiple R-squared:  0.6797,  Adjusted R-squared:  0.6771 
# F-statistic: 263.8 on 18 and 2238 DF,  p-value: < 2.2e-16