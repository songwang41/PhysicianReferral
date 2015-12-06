library(data.table)  # so fast!
library(igraph)  # all the basic graph operations.


##################################READ ME######################
## This file reads in the 
## "physician-referrals-2015-days365.txt" in the Data folder
## "physician_compare/National_Downloadable_File.csv"

##############
#"/Users/SongWang/Stat/Physician_Referral_Network/RScripts"


DataPath <- "./Data/"
ResultsPath <- "./Results/"
PlotsPath <- "./Plots/"

## 2012 data set  ## nrow = 73 M , ncol =5, 
## length(unique(Et$V1))  #1004294
## length(unique(Et$V2))  #1011848

refer30_2012 = fread(paste0(DataPath, 
        "Physician-referrals/Physician-Referrals-2012-2013-DAYS30.txt"),
         colClasses = c("character", "character","numeric", "numeric", "numeric"))
setkey(refer30_2012, V1)
head(refer30_2012)
refer <- refer30_2012
rm(refer30_2012)
save(refer,file="./Data/refer.RData")



b= c(rep("character", 6),rep("factor",4), "numeric", rep("factor",6), "character", 
     "character", "character", "numeric", rep("character",2), "factor", "character", 
     "factor", "character", rep("character", 10), rep("factor", 6))
PhysData = fread(paste0(DataPath,"physician_compare/National_Downloadable_File.csv"), colClasses = b)
setkey(PhysData, NPI)
rm(b)
refer30_2012 = refer30_2012[,.(V1,V2,V3)] 
save(refer30_2012, file = paste0(DataPath, "refer2012.RData"))
## referral network in 2012 with period of 30-day


#           V1         V2   V3         V4          V5
# 1: 1000000004 1548295421 20         12          9
# 2: 1000000004 1528009412 136        32         36
# 3: 1000000004 1972546315 31         18         18
# 4: 1000000004 1376577247 31         20         21
# 5: 1000000004 1790775229 54         44         47
# 6: 1000000004 1558653212 20         11          9



#Medicare Physician and Other Supplier PUF, CY2012, Tab Delimited format
# NPI  NPPES_PROVIDER_LAST_ORG_NAME	NPPES_PROVIDER_FIRST_NAME	NPPES_PROVIDER_MI	NPPES_CREDENTIALS	NPPES_PROVIDER_GENDER	NPPES_ENTITY_CODE	NPPES_PROVIDER_STREET1	NPPES_PROVIDER_STREET2	NPPES_PROVIDER_CITY	NPPES_PROVIDER_ZIP	NPPES_PROVIDER_STATE	NPPES_PROVIDER_COUNTRY	PROVIDER_TYPE	MEDICARE_PARTICIPATION_INDICATOR	PLACE_OF_SERVICE	HCPCS_CODE	HCPCS_DESCRIPTION	HCPCS_DRUG_INDICATOR	LINE_SRVC_CNT	BENE_UNIQUE_CNT	BENE_DAY_SRVC_CNT	AVERAGE_MEDICARE_ALLOWED_AMT	STDEV_MEDICARE_ALLOWED_AMT	AVERAGE_SUBMITTED_CHRG_AMT	STDEV_SUBMITTED_CHRG_AMT	AVERAGE_MEDICARE_PAYMENT_AMT	STDEV_MEDICARE_PAYMENT_AMT
# 0000000001	CPT copyright 2011 American Medical Association.  All Rights Reserved.														
# 1003000126	ENKESHAFI	ARDALAN		M.D.	M	I	900 SETON DR		CUMBERLAND	215021854	MD	US	Internal Medicine	Y	F	99222	Initial hospital inpatient care, typically 50 minutes per day	N	115	112	115	135.25	0	199	0	108.11565217	0.9005883395
# 1003000126	ENKESHAFI	ARDALAN		M.D.	M	I	900 SETON DR		CUMBERLAND	215021854	MD	US	Internal Medicine	Y	F	99223	Initial hospital inpatient care, typically 70 minutes per day	N	93	88	93	198.59	0	291	9.5916630466	158.87	0

b= rep("character", 28)
Payment2012 <- fread(paste0(DataPath,
  "Medicare_Provider_Util_Payment_PUF_CY2012_update/Medicare_Provider_Util_Payment_PUF_CY2012.txt"),
  colClasses = b, header = T, sep='\t')
save(Payment2012, file=paste0(DataPath,"Payment2012.RData") )
#nrow = 405 K, ncol = 28
# 37263 unique NPI


charges<- cbind(Payment$average_submitted_chrg_amt,
                Payment$average_Medicare_allowed_amt,Payment$average_Medicare_payment_amt)
names(charges) <- c("average_submitted_chrg_amt","average_Medicare_allowed_amt",
                    "average_Medicare_payment_amt")



### >> ??????? what to model:

## focus on service with code = 66984

Payment2012Cataract <- subset(Payment2012,HCPCS_CODE=="66984")
save(Payment2012Cataract, file="./Data/Payment2012Cataract66984.RData")

## focus on a bunch of services related to eyes


desp <- unique(Payment2012$HCPCS_DESCRIPTION)
eyedisease <- desp[grep(pattern="eye",desp)]
# 213



