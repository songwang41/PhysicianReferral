library(dplyr)
library(tidyr)
library(data.table)
library(acs) 
library(choroplethr)


setwd("C:/Users/dgricci/Stat/Physician_Referral_Network/Data")
getwd()

load("payment2012.Rdata")

class(Payment2012)
dta <- as.data.frame(Payment2012)


dta <- dta[-1, ] # Removes row that contains copyright information


dta[ ,20] <- as.integer(dta[ ,20]) #line serv cnt
dta[ ,21] <- as.integer(dta[ ,21]) #bene uniq serv cnt
dta[ ,22] <- as.integer(dta[ ,22]) #bene day serv cnt 

dta[ ,23] <- as.numeric(dta[ ,23]) #avg maa
dta[ ,24] <- as.numeric(dta[ ,24]) #stdev maa
dta[ ,25] <- as.numeric(dta[ ,25]) #avg sub
dta[ ,26] <- as.numeric(dta[ ,26]) #stdev sub
dta[ ,27] <- as.numeric(dta[ ,27]) #avg pay
dta[ ,28] <- as.numeric(dta[ ,28]) #stdev pay
str(dta)


# Just some data exploration
table(dta$NPPES_PROVIDER_COUNTRY)
table(dta$NPPES_PROVIDER_STATE)
table(dta$MEDICARE_PARTICIPATION_INDICATOR)
length(unique(dta$HCPCS_CODE))
length(unique(dta$HCPCS_DESCRIPTION))
sum(is.na(dta$HCPCS_CODE))
sum(is.na(dta$HCPCS_DESCRIPTION))
length(unique(dta$NPI))


#######################################
### Organizational cataract surgery ###
#######################################

indx <- which(dta$NPPES_ENTITY_CODE == "O")
orgDta <- dta[indx, ]

indx <- which(orgDta$HCPCS_DESCRIPTION == "Removal of cataract with insertion of lens")
orgCataract <- orgDta[indx, ]
rm(orgDta)

tail(sort(orgCataract$AVERAGE_SUBMITTED_CHRG_AMT))
head(sort(orgCataract$AVERAGE_SUBMITTED_CHRG_AMT))
hist(orgCataract$AVERAGE_SUBMITTED_CHRG_AMT, breaks=100)

which.max(orgCataract$AVERAGE_SUBMITTED_CHRG_AMT)
View(orgCataract[1602, ])

table(orgCataract$HCPCS_CODE)

# rm(orgCataract)

###################################
### Individual cataract surgery ###
###################################

indx <- which(dta$NPPES_ENTITY_CODE == "I")
indDta <- dta[indx, ]

indx <- which(indDta$HCPCS_DESCRIPTION == "Removal of cataract with insertion of lens")
indCataract <- indDta[indx, ]
rm(indDta)

tail(sort(indCataract$AVERAGE_SUBMITTED_CHRG_AMT))
head(sort(indCataract$AVERAGE_SUBMITTED_CHRG_AMT))
hist(indCataract$AVERAGE_SUBMITTED_CHRG_AMT, breaks=100)

which.max(indCataract$AVERAGE_SUBMITTED_CHRG_AMT)
View(indCataract[7535, ])


sum(as.logical(which(indCataract$AVERAGE_SUBMITTED_CHRG_AMT <100)))
mean(indCataract$AVERAGE_SUBMITTED_CHRG_AMT <100)

sum(as.logical(which(indCataract$AVERAGE_SUBMITTED_CHRG_AMT <50)))
mean(indCataract$AVERAGE_SUBMITTED_CHRG_AMT <50)

sum(as.logical(which(indCataract$AVERAGE_SUBMITTED_CHRG_AMT <25)))
mean(indCataract$AVERAGE_SUBMITTED_CHRG_AMT <25)

sum(as.logical(which(indCataract$AVERAGE_SUBMITTED_CHRG_AMT <10)))
mean(indCataract$AVERAGE_SUBMITTED_CHRG_AMT <10)

sum(as.logical(which(indCataract$AVERAGE_SUBMITTED_CHRG_AMT <5)))
mean(indCataract$AVERAGE_SUBMITTED_CHRG_AMT <5)

sum(as.logical(which(indCataract$AVERAGE_SUBMITTED_CHRG_AMT <1)))
mean(indCataract$AVERAGE_SUBMITTED_CHRG_AMT <1)



table(indCataract$HCPCS_CODE)



####################################
### Differences between services ###
####################################

# There are three different codes that correspond to "Removal of cataract 
#      with insertion of lens"
# 66982 - EXTRACAPSULAR CATARACT REMOVAL WITH INSERTION OF INTRAOCULAR 
#      LENS PROSTHESIS (1 - STAGE PROCEDURE), MANUAL OR MECHANICAL TECHNIQUE 
#      (EG, IRRIGATION AND ASPIRATION OR PHACOEMULSIFICATION)
# 66983 - INTRACAPSULAR CATARACT EXTRACTION WITH INSERTION OF INTRAOCULAR 
#      LENS PROSTHESIS (1 STAGE PROCEDURE)
# 66984 - EXTRACAPSULAR CATARACT REMOVAL WITH INSERTION OF INTRAOCULAR LENS 
#      PROSTHESIS (1 STAGE PROCEDURE), MANUAL OR MECHANICAL TECHNIQUE 
#      (EG, IRRIGATION AND ASPIRATION OR PHACOEMULSIFICATION)

#############################################################
### Evidence of providers and organizations in same place ###
#############################################################

which(orgCataract$NPPES_PROVIDER_STREET1 == "1111 DELAFIELD ST")
View(orgCataract[which(orgCataract$NPPES_PROVIDER_STREET1 == "1111 DELAFIELD ST"), ])

which(indCataract$NPPES_PROVIDER_STREET1 == "1111 DELAFIELD ST")
View(indCataract[which(indCataract$NPPES_PROVIDER_STREET1 == "1111 DELAFIELD ST"), ])



##########
### WI ###
##########

indx <- which(indCataract$NPPES_PROVIDER_STATE == "WI")
View(indCataract[indx, ])

indx <- which(orgCataract$NPPES_PROVIDER_STATE == "WI")
View(orgCataract[indx, ])

########################
### WI service 66982 ###
########################

indx <- which(indCataract$NPPES_PROVIDER_STATE == "WI"& indCataract$HCPCS_CODE == "66982")
View(indCataract[indx, ])

indx <- which(orgCataract$NPPES_PROVIDER_STATE == "WI" & orgCataract$HCPCS_CODE == "66982")
View(orgCataract[indx, ])

########################
### WI service 66984 ###
########################

indx <- which(indCataract$NPPES_PROVIDER_STATE == "WI"& indCataract$HCPCS_CODE == "66984")
View(indCataract[indx, ])
dim(indCataract[indx, ])

indx <- which(orgCataract$NPPES_PROVIDER_STATE == "WI" & orgCataract$HCPCS_CODE == "66984")
View(orgCataract[indx, ])
dim(orgCataract[indx, ])


#####################################################################
### Loading physician characteristics and merging with above data ###
#####################################################################

b = c(rep("character", 6),
      rep("factor",4), 
      "numeric", 
      rep("factor",6), 
      "character", 
      "character", 
      "character", 
      "numeric", 
      rep("character",2), 
      "factor", 
      "character", 
      "factor", 
      "character", 
      rep("character", 10), 
      rep("factor", 6)
)

DT = fread("C:/Users/dgricci/Stat/Physician_Referral_Network/Data/Physician_compare/National_Downloadable_File.csv",
           colClasses = b
           )
setkey(DT, NPI)
glimpse(DT)
str(DT)
rm(b)

any(DT$`Graduation year` > 2012)
length(which(DT$`Graduation year` > 2012))
length(which(DT$`Graduation year` <= 2012))

indx <- which(DT$`Graduation year` > 2012)
DT <- DT[-indx, ]

any(DT$`Graduation year` > 2012)
max(DT$`Graduation year`)

sum(is.na(DT$`Graduation year`))
tail(sort(DT$`Graduation year`))

which(is.na(DT$`Graduation year`) == TRUE)
DT[which(is.na(DT$`Graduation year`) == TRUE), ] #Just an ER physician so he will be deleted

DT <- DT[-which(is.na(DT$`Graduation year`) == TRUE), ]

View(DT[1:100, ])
table(DT$`Primary specialty`)

class(DT)
names(DT)
physDta <- select(DT, 
                  NPI,
                  `Graduation year`,
                  `Organization legal name`,
                  `Organization DBA name`,
                  `Claims based hospital affiliation LBN 1`,
                  `Claims based hospital affiliation LBN 2`,
                  `Claims based hospital affiliation LBN 3`, 
                  `Claims based hospital affiliation LBN 4`,
                  `Claims based hospital affiliation LBN 5`
                  )
physDta <- as.data.frame(physDta)
# rm(DT)

glimpse(physDta)
glimpse(dta)


names(dta)

dta <- select(dta,
             NPI,
             NPPES_PROVIDER_LAST_ORG_NAME,
             NPPES_PROVIDER_FIRST_NAME,
             NPPES_PROVIDER_MI,
             NPPES_CREDENTIALS,
             NPPES_PROVIDER_GENDER,
             NPPES_ENTITY_CODE,
             NPPES_PROVIDER_STREET1,
             NPPES_PROVIDER_CITY,
             NPPES_PROVIDER_ZIP,
             NPPES_PROVIDER_STATE, 
             NPPES_PROVIDER_COUNTRY,
             PROVIDER_TYPE,
             PLACE_OF_SERVICE,
             HCPCS_CODE,
             HCPCS_DESCRIPTION,
             LINE_SRVC_CNT,
             BENE_UNIQUE_CNT,
             AVERAGE_SUBMITTED_CHRG_AMT
             )

dta <- as.data.table(dta)
setkey(dta, NPI)
physDta <- as.data.table(physDta)
setkey(physDta, NPI)


indx <- which(dta$HCPCS_CODE == 66984)
dta <- dta[indx, ]

merged <- left_join(dta, physDta, by = "NPI")


View(merged[1:200, ])

merged$NPPES_PROVIDER_ZIP <- substr(merged$NPPES_PROVIDER_ZIP, 1, 5)

indx <- which(merged$NPPES_ENTITY_CODE == "O")
merged <- merged[-indx, ]

indx <- which(merged$PROVIDER_TYPE == "Ophthalmology")
merged <- merged[indx, ]

merged <- merged[!duplicated(merged$NPI), ]


merged$YRS_EXP <- 2012 - merged$`Graduation year`

names(merged)

# save(merged, file = "merged.Rdata")
# load("merged.Rdata")

###############################
### exploratory regressions ###
###############################

reg1 <- lm(AVERAGE_SUBMITTED_CHRG_AMT ~ NPPES_PROVIDER_GENDER + YRS_EXP, data = merged)
summary(reg1)
reg2 <- lm(AVERAGE_SUBMITTED_CHRG_AMT ~ as.factor(NPPES_PROVIDER_GENDER) + YRS_EXP, data = merged)
summary(reg2)
reg3 <- lm(AVERAGE_SUBMITTED_CHRG_AMT ~ as.factor(NPPES_PROVIDER_GENDER) + YRS_EXP + as.factor(PLACE_OF_SERVICE), data = merged)
summary(reg3)



###############################
### Obtaining ACS varialbes ###
###############################

# for 2012 estimates ...see website below.
# http://www.socialexplorer.com/data/ACS2012/metadata/?ds=SE

acsKey <- "2f87a55e2a9f760214692ae5267b4c7aed96688e"
api.key.install(key =acsKey) 
us.county <- geo.make(zip.code="*")

########################
### median hh income ###
########################

us.medHHincome <- acs.fetch(geography=us.county, table.number="B19013", col.names="pretty")

medHHincome <- estimate(us.medHHincome)
medHHincome.data <- as.data.frame(medHHincome)
medHHincome.data <- cbind(ZIP.Code = rownames(medHHincome), medHHincome.data)

medHHincome.data$ZIP.Code <- substring(medHHincome.data$ZIP.Code, 7, 11)
typeof(medHHincome.data$ZIP.Code) # need change ZIP.Code to NPPES_PROVIDER_ZIP to match
row.names(medHHincome.data) <- NULL
colnames(medHHincome.data)[1] <- "NPPES_PROVIDER_ZIP"

medHHincome.data <- medHHincome.data[ ,c(1,2)]

#########################################################################
### gross median Rent ***includes estimated cost of utilities!!! woo! ###
#########################################################################

us.medGrossHHrent <- acs.fetch(geography=us.county, table.number="B25064", col.names="pretty")

medGrossHHrent <- estimate(us.medGrossHHrent)
medGrossHHrent.data <- as.data.frame(medGrossHHrent)
medGrossHHrent.data <- cbind(ZIP.Code = rownames(medGrossHHrent), medGrossHHrent.data)

medGrossHHrent.data$ZIP.Code <- substring(medGrossHHrent.data$ZIP.Code, 7, 11)
typeof(medGrossHHrent.data$ZIP.Code) # need change ZIP.Code to NPPES_PROVIDER_ZIP to match
row.names(medGrossHHrent.data) <- NULL
colnames(medGrossHHrent.data)[1] <- "NPPES_PROVIDER_ZIP"

medGrossHHrent.data <- medGrossHHrent.data[ ,c(1,2)]

###########################################################################
### Race  ***will divide by total pop to get percernt white or nonwhite ###
###########################################################################

us.race <- acs.fetch(geography=us.county, table.number="B02001", col.names="pretty")

race <- estimate(us.race)
race.data <- as.data.frame(race)
race.data <- cbind(ZIP.Code = rownames(race), race.data)

race.data$ZIP.Code <- substring(race.data$ZIP.Code, 7, 11)
typeof(race.data$ZIP.Code) # need change ZIP.Code to NPPES_PROVIDER_ZIP to match
row.names(race.data) <- NULL
colnames(race.data)[1] <- "NPPES_PROVIDER_ZIP"

# convert to percent white 

race.data[ ,12] <- race.data[ ,3]/race.data[ ,2]

race.data[ ,12] <- 1-race.data[ ,12]
colnames(race.data)[12] <- "nonWhite"

race.data <- race.data[ ,c(1,12)]

##############################################################
### Age 65+ poverty status ***must add up 65 to 74 and 75+ ###
##############################################################

us.poverty65plus <- acs.fetch(geography=us.county, table.number="B17001", col.names="pretty")

poverty65plus <- estimate(us.poverty65plus)
poverty65plus.data <- as.data.frame(poverty65plus)
poverty65plus.data <- cbind(ZIP.Code = rownames(poverty65plus), poverty65plus.data)

poverty65plus.data$ZIP.Code <- substring(poverty65plus.data$ZIP.Code, 7, 11)
typeof(poverty65plus.data$ZIP.Code) # need change ZIP.Code to NPPES_PROVIDER_ZIP to match
row.names(poverty65plus.data) <- NULL
colnames(poverty65plus.data)[1] <- "NPPES_PROVIDER_ZIP"

# combine 65-74 and 75 plus divide by total for both males and females and divide by total at or above.

poverty65plus.data[, 61] <- poverty65plus.data[ ,16] + poverty65plus.data[ ,17] 
colnames(poverty65plus.data)[61] <- "poorMale65plusTotal"

poverty65plus.data[, 62] <- poverty65plus.data[ ,30] + poverty65plus.data[ ,31]
colnames(poverty65plus.data)[62] <- "poorFemale65plusTotal"

###

poverty65plus.data[, 63] <- poverty65plus.data[ ,45] + poverty65plus.data[ ,46] + poverty65plus.data[ ,61]
colnames(poverty65plus.data)[63] <- "male65plusTotal"

poverty65plus.data[, 64] <- poverty65plus.data[ ,59] + poverty65plus.data[ ,60] + poverty65plus.data[ ,62]
colnames(poverty65plus.data)[64] <- "female65plusTotal"

###

poverty65plus.data[, 65] <- poverty65plus.data[ ,61] + poverty65plus.data[ ,62]
colnames(poverty65plus.data)[65] <- "poor65plusTotal"

poverty65plus.data[, 66] <- poverty65plus.data[ ,63] + poverty65plus.data[ ,64]
colnames(poverty65plus.data)[66] <- "65plusTotal"

###

poverty65plus.data[, 67] <- poverty65plus.data[ ,65] / poverty65plus.data[ ,66]
colnames(poverty65plus.data)[67] <- "65plusPoverty"

 
poverty65plus.data <- poverty65plus.data[ ,c(1,66,67)]

#####################
### Vetran status ###
#####################

us.vetran <- acs.fetch(geography=us.county, table.number="B21001", col.names="pretty")

vetran <- estimate(us.vetran)
vetran.data <- as.data.frame(vetran)
vetran.data <- cbind(ZIP.Code = rownames(vetran), vetran.data)

vetran.data$ZIP.Code <- substring(vetran.data$ZIP.Code, 7, 11)
typeof(vetran.data$ZIP.Code) # need change ZIP.Code to NPPES_PROVIDER_ZIP to match
row.names(vetran.data) <- NULL
colnames(vetran.data)[1] <- "NPPES_PROVIDER_ZIP"

#####################################################
### determine prop. of old folks that are vetrans ###
#####################################################

vetran.data[, 41] <- vetran.data[ ,18] + vetran.data[ ,21] 
colnames(vetran.data)[41] <- "vetranMale65plusTotal"

vetran.data[, 42] <- vetran.data[ ,36] + vetran.data[ ,39]
colnames(vetran.data)[42] <- "vetranFemale65plusTotal"

###

vetran.data[, 43] <- vetran.data[ ,41] + vetran.data[ ,42] 
colnames(vetran.data)[43] <- "vetran65plusTotal"

vetran.data[, 44] <- vetran.data[ ,43] / poverty65plus.data[ ,2]
colnames(vetran.data)[44] <- "vetran65plus"

vetran.data <- vetran.data[ ,c(1,44)]



###########################
### merge acs variables ###
###########################

acsData <- merge(medHHincome.data, medGrossHHrent.data, by = "NPPES_PROVIDER_ZIP")
acsData2 <- merge(acsData, race.data, by = "NPPES_PROVIDER_ZIP")
acsData3 <- merge(acsData2, poverty65plus.data, by = "NPPES_PROVIDER_ZIP")
acsData4 <- merge(acsData3, vetran.data, by = "NPPES_PROVIDER_ZIP")
acsDataFinal <- acsData4

colnames(acsDataFinal)[2] <- "medianHHincome"
colnames(acsDataFinal)[3] <- "medianGrossRent"


# save(acsDataFinal, file = "acsDataFinal.Rdata")
