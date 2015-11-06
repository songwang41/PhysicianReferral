
load("../Data/Payment_NPI_ca.RData")
data<- Payment_NPI_ca  # renaming
data [1:5,]
code <- table(data$HCPCS_CODE)
length(code)
top20 <- sort(code,decreasing = T)[1:20]
top20
# 99213 99214 99204 99203 99212 99232 99223 99215 99233 G0008 93000 99205 99222 99291 36415 96372 
# 33190 29059 13769 12809 11940 11438 11268 10543  9631  8885  7784  7242  6240  5449  5181  4956 
# 99284 93010 97110 71020 
# 4644  4634  4446  4336 
rownames(top20)

ord <- order(data$HCPCS_CODE)
data1 <- data[data$HCPCS_CODE %in% rownames(top20),]
unique(data1$HCPCS_CODE)

ord <- order(data1$HCPCS_CODE)
data1ordered <- data1[ord,]
idx <- cumsum(table(data1ordered$HCPCS_CODE))
# 36415  71020  93000  93010  96372  97110  99203  99204  99205  99212  99213  99214  99215  99222 
# 5181   9517  17301  21935  26891  31337  44146  57915  65157  77097 110287 139346 149889 156129 
# 99223  99232  99233  99284  99291  G0008 
# 167397 178835 188466 193110 198559 207444 
cbind(table(data1ordered$HCPCS_CODE), data1ordered[idx,c("HCPCS_DESCRIPTION")])
library(ggplot2)
hist(data1ordered$AVERAGE_MEDICARE_ALLOWED_AMT[5182:9517],breaks=100)
hist(data1ordered$AVERAGE_SUBMITTED_CHRG_AMT[5182:9517],breaks=1000)
hist(data1ordered$AVERAGE_MEDICARE_PAYMENT_AMT[5182:9517],breaks=100)

boxplot(data1ordered$AVERAGE_MEDICARE_PAYMENT_AMT~data1ordered$HCPCS_CODE, main="AVERAGE_MEDICARE_PAYMENT_AMT for top 20 frequent services")
boxplot(data1ordered$AVERAGE_MEDICARE_ALLOWED_AMT~data1ordered$HCPCS_CODE, main="AVERAGE_MEDICARE_ALLOWED_AMT for top 20 frequent services")



