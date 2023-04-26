# 2020 Race investigation
library(readxl)
library(dplyr)
library(stringr)
library(padr)

Data2020Alt <- read_excel("Race_2020_Data_Question.xlsx") #all 2020 data
Data2020Alt$Date <- paste0(Data2020Alt$DOD_YR, "-" ,Data2020Alt$DOD_MO, "-" ,Data2020Alt$DOD_DY)
Data2020Alt$Date <- as.Date(Data2020Alt$Date)
Data2020Alt_VAonly <- Data2020Alt[Data2020Alt$STATETEXT_R == "Virginia", ]

#Just VA Check

#Internal Daily totals

Daily_Int <- FullSM_2020_VAonly %>%
  group_by(Date) %>%
  summarize(count_by_day_internal =  n()) 

Daily_Alt <- Data2020Alt_VAonly %>%
  group_by(Date) %>%
  summarize(count_by_day_alternate =  n()) 

Checks <- left_join(Daily_Int, Daily_Alt, by = c("Date"="Date"))
Checks$Diff <- Checks$count_by_day_alternate - Checks$count_by_day_internal

BigDiff <- Checks[Checks$Diff >20, ]

plot(Checks$count_by_day_internal,type='l',col='grey', ylim = c(0,300))
lines(Checks$count_by_day_alternate,type='l',col='yellow')
lines(Checks$Diff, type='l',col='blue')

plot(Checks$Diff,type = "l")

par(mar = c(5, 4, 4, 4) + 0.3)              # Additional space for second y-axis
plot(Checks$count_by_day_internal, type='l', col = 2)              # Create first plot
lines(Checks$count_by_day_alternate)
par(new = TRUE)                             # Add new plot
plot(Checks$Diff, type='l', col = 3,              # Create second plot without axes
     axes = FALSE, xlab = "", ylab = "")
axis(side = 4, at = pretty(range(Checks$Diff)))      # Add second axis
mtext("Checks$Diff", side = 4, line = 3)             # Add second axis label

#Initial analysis done, now start to crunch

testV20 <- read.csv("testV20.csv")

testV20$X19 <- testV1$X18
testV20[testV20 == "BLANK"] <- 0
testV20[testV20 == 0] <- NA

#Ultimate Mort 2020
#Need to first make df that has dates (get original data to be same number
# of rows as the cleaned ICD to join the two. Check that they match)
# Also make sure to put column names proper and keep in mind how many have been filtered

#Just putting in the first three columns 

Data_work <- Data2020Alt_VAonly[!is.na(Data2020Alt_VAonly$Date),]
Data_work <- Data_work[1:77257, ]
WendyData <- read.csv("WendyData2020_Iteration6.csv")

Big3 <- WendyData[,17:19]
colnames(Big3) <- c("ICD10_2","ICD10_1","ICD10_0")
Big3$Date <- Data_work$Date
Big3$Race <- Data_work$NCHSBRIDGE
Big3$Age <- Data_work$AGE
Big3$Sex <- Data_work$SEX
Big3$Text_R_Upper <- Data_work$COUNTYTEXT_R
#write.csv(Big3, "BigThree2020.csv")

VA_Links_NoCities <- read_excel("VA_Links_NoCities.xlsx")
VA_Links_None <- VA_Links_NoCities

# Actually join the counties

CountyText_R <- read_excel("BigThreeCountyText_2020R.xlsx", 
                           col_types = c("text"))

Big3$Text_R <- CountyText_R$COUNTYTEXT_R
Big3$Text_R <- trimws(Big3$Text_R, which = c("both"))

Big3$Text_R <- tolower(Big3$Text_R)
VA_Links_None$County <- tolower(VA_Links_None$County)

Big3City <- left_join(Big3,VA_Links_None,by = c("Text_R" = "County"))

table(Big3City$ID)

NAS6 <- Big3City[is.na(Big3City$ID), ]
table(NAS6$Text_R)

FullID2020 <- Big3City[!is.na(Big3City$ID),]

FullID2020 <- Big3City[!is.na(Big3City$ID),]
sum(is.na(Big3City$ID))
sum(is.na(FullID2020$ID))


#Sex Check Procedure

FullID2020$ICD10_0 <-gsub("-$","",FullID2020$ICD10_0)
FullID2020$ICD10_0 <- trimws(FullID2020$ICD10_0, which = c("both"))
FullID2020$ACMESexCheck0 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",FullID2020$ICD10_0), 
                                     paste0(FullID2020$ICD10_0,"-",FullID2020$SEX),FullID2020$ICD10_0)) 
FullID2020$SexCheck1 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",FullID2020$ICD10_1), 
                                     paste0(FullID2020$ICD10_1,"-",FullID2020$SEX),FullID2020$ICD10_1)) 
FullID2020$SexCheck2 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",FullID2020$ICD10_2), 
                                     paste0(FullID2020$ICD10_2,"-",FullID2020$SEX),FullID2020$ICD10_2)) 

#Map ICD codes to MDC

ICD10_Mapping_v8 <- read_excel("~/Desktop/ICD10_Mapping_v8.xlsx", 
                               sheet = "MDC_Crosswalk")
ICD10_Mapping_v8$Mortality_ICD <-gsub("-$","",ICD10_Mapping_v8$Mortality_ICD)
key <- ICD10_Mapping_v8 %>%
  select(Mortality_ICD,MDC)

#2005-2014 matching each ICD code from ACME to the end

NewDFtest = left_join(FullID2020, key, by = c("ACMESexCheck0"="Mortality_ICD"))
NewDFtest1 = left_join(FullID2020,key, by = c("SexCheck1" = "Mortality_ICD"))
NewDFtest2 = left_join(FullID2020,key, by = c("SexCheck2" = "Mortality_ICD"))

FullID2020$MDC0ACME <- NewDFtest$MDC
FullID2020$MDC1 <- NewDFtest1$MDC
FullID2020$MDC2 <- NewDFtest2$MDC

#Correct Age

Age_Key <- read_excel("Age_Key.xlsx")

FullID2020$AgeUpdate <- ifelse(FullID2020$Age > 110, 1, FullID2020$Age)

FullID2020_AgeGroups <- left_join(FullID2020,Age_Key, by = c("AgeUpdate"="Age"))

table(FullID2020_AgeGroups$Group)

#Race Grouping

FullID2020_AgeGroups$RACE_Update <- str_replace_all(FullID2020_AgeGroups$Race, c("0"= "Other",
                                                               "1"="White",
                                                               "2"="Black",
                                                               "3"="AmericanIndian/AlaskanNative",
                                                               "4"="Chinese",
                                                               "5"="Japanese",
                                                               "6"="NativeHawaiian",
                                                               "7"="Filipino",
                                                               "8"="Unknown",
                                                               "9"="Unknown",
                                                               "10"="Unknown",
                                                               "11"="Unknown",
                                                               "12"="Unknown",
                                                               "13"="Unknown",
                                                               "14"="Unknown",
                                                               "15"="Unknown",
                                                               "21"="Unknown",
                                                               "22"="Unknown",
                                                               "23"="Unknown",
                                                               "99"="Unknown"))

table(FullID2020_AgeGroups$RACE_Update)
table(FullID2020_AgeGroups$Race)

FullID2020_AgeGroups$RACE_Update <- str_replace_all(FullID2020_AgeGroups$RACE_Update, c("BlackAmericanIndian/AlaskanNative"="Unknown",
                                                                                        "BlackBlack"="Black",
                                                                                        "BlackWhite"="Unknown",
                                                                                        "UnknownUnknown"="Unknown",
                                                                                        "WhiteAmericanIndian/AlaskanNative"="Unknown",
                                                                                        "WhiteBlack"="Unknown",
                                                                                        "WhiteChinese"="Unknown",
                                                                                        "WhiteJapanese"="Unknown",
                                                                                        "WhiteOther"="Unknown",
                                                                                        "WhiteWhite"="Unknown",
                                                                                        "Chinese"="AsianPacificIslander",
                                                                                        "Filipino"="AsianPacificIslander",
                                                                                        "Japanese"="AsianPacificIslander",
                                                                                        "NativeHawaiian"="AsianPacificIslander"))


table(FullID2020_AgeGroups$RACE_Update)
table(FullID2020_AgeGroups$Race)

write.csv(FullID2020_AgeGroups,"MDC2020_GoodToGo.csv")

#sample <- FullID2020_AgeGroups %>%
#  filter(Race > 9)
#write.csv(sample, "Race1099SampleforWendy.csv")
  
#Then proceed with original Ulimate Mort Process

#Gather Race Data

groupsEZF2020_Race <- FullID2020_AgeGroups %>%
  filter(ID == "EZF") %>%
  group_by(Date, RACE_Update) %>%
  summarize(count_by_siteyear =  n()) 

groupswideEZF2020_Race <- pivot_wider(groupsEZF2020_Race,id_cols = "Date",names_from = RACE_Update,values_from = count_by_siteyear)

EZFFullRacedf <- data.frame(groupswideEZF2020_Race)
EZFFullRacedf$Date <- as.Date(EZFFullRacedf$Date)

EZFFullRacepad <- pad(EZFFullRacedf, interval = "day", start_val = as.POSIXct('2020-01-01'), end_val = as.POSIXct('2020-12-31'))

EZFFullRacepad[is.na(EZFFullRacepad)] <- 0

#

#Gather Age Data

groupsEZF2020_Age <- FullID2020_AgeGroups %>%
  filter(ID == "EZF") %>%
  group_by(Date, Group) %>%
  summarize(count_by_siteyear =  n()) 

groupswideEZF2020_Age <- pivot_wider(groupsEZF2020_Age,id_cols = "Date",names_from = Group,values_from = count_by_siteyear)

EZFFullAgedf <- data.frame(groupswideEZF2020_Age)
EZFFullAgedf$Date <- as.Date(EZFFullAgedf$Date)

EZFFullAgepad <- pad(EZFFullAgedf, interval = "day", start_val = as.POSIXct('2020-01-01'), end_val = as.POSIXct('2020-12-31'))

EZFFullAgepad[is.na(EZFFullAgepad)] <- 0

# Combine Age and Race Data

EZFDemographics <- left_join(EZFFullAgepad, EZFFullRacepad, by = c("Date"="Date"))

OrderedEZFDemographics <- EZFDemographics %>% relocate(X0.11,X12.17,X18.29,X30.39,
                                                       X40.49,X50.64,X65.74,X75.,
                                                       Black,White,AsianPacificIslander,AmericanIndian.AlaskanNative,
                                                       Unknown,.after = Date)
#Gather MDC Data

groupsEZF2020 <- FullID2020_AgeGroups %>%
  filter(ID == "EZF") %>%
  group_by(Date,MDC0ACME) %>%
  summarize(count_by_siteyear =  n()) 

groupswideEZF2020 <- pivot_wider(groupsEZF2020,id_cols = "Date",names_from = MDC0ACME,values_from = count_by_siteyear)

OrderedEZF2020 <- groupswideEZF2020 %>% relocate(`MDC 02`, `MDC 03`,`MDC 04`,`MDC 05`,`MDC 06`,`MDC 07`,
                                                 `MDC 08`, `MDC 09`, `MDC 10`,`MDC 11`, `MDC 12`,
                                                 `MDC 13`, `MDC 14`, `MDC 15`, `MDC 16`, `MDC 17`,
                                                 `MDC 18`, `MDC 19`, `MDC 20`, #`MDC 21`,#`MDC 22`,
                                                 `MDC 23`, `MDC 25`,#`MDC 24`,
                                                 `Cancer NOS`,`Non-billable ICD`,.after = `MDC 01`) 

EZFFullMDC <- OrderedEZF2020

EZFFullMDC$`MDC 21` <- NA
EZFFullMDC$`MDC 22` <- NA
EZFFullMDC$`MDC 24` <- NA

EZFFullMDCdf <- data.frame(EZFFullMDC)
EZFFullMDCdf$Date <- as.Date(EZFFullMDCdf$Date)

EZFFullMDCpad <- pad(EZFFullMDCdf, interval = "day", start_val = as.POSIXct('2020-01-01'), end_val = as.POSIXct('2020-12-31'))

EZFFullMDCpad[is.na(EZFFullMDCpad)] <- 0

#write.csv(EZFFullMDCpad, file = "EZFFullMDC.csv")

#Gather SEX Data

groupsEZF2020_SEX <- FullID2020_AgeGroups %>%
  filter(ID == "EZF") %>%
  group_by(Date, Sex) %>%
  summarize(count_by_siteyear =  n()) 

groupswideEZF2020_Sex <- pivot_wider(groupsEZF2020_SEX,id_cols = "Date",names_from = Sex,values_from = count_by_siteyear)

EZFFullSEX <- groupswideEZF2020_Sex

EZFFullSEXdf <- data.frame(EZFFullSEX)
EZFFullSEXdf$Date <- as.Date(EZFFullSEXdf$Date)

EZFFullSEXpad <- pad(EZFFullSEXdf, interval = "day", start_val = as.POSIXct('2020-01-01'), end_val = as.POSIXct('2020-12-31'))

EZFFullSEXpad[is.na(EZFFullSEXpad)] <- 0

#write.csv(EZFFullSEXpad,"EZFMortUltimate_SEXOnly.csv")

#Ultimate Mortality

UltMort_EZF_almost2020 <- left_join(EZFFullMDCpad,OrderedEZFDemographics,by= c("Date"="Date"))
UltMort_EZF2020 <- left_join(UltMort_EZF_almost2020,EZFFullSEXpad,by= c("Date"="Date"))

write.csv(UltMort_EZF2020, file = "EZFMortUltimate2020.csv")


################ SCRATCH ########################

Data2020Alt <- read_excel("Race_2020_Data_Question.xlsx") #all 2020 data
Data2020Alt$Date <- paste0(Data2020Alt$DOD_YR, "-" ,Data2020Alt$DOD_MO, "-" ,Data2020Alt$DOD_DY)
Data2020Alt$Date <- as.Date(Data2020Alt$Date)
Data2020Alt_VAonly <- Data2020Alt[Data2020Alt$STATETEXT_R == "Virginia", ]

Data_work <- Data2020Alt_VAonly[!is.na(Data2020Alt_VAonly$ACME_UC), ]

Data_work$ICD18 <- word(Data_work$RAC,18)
Data_work$ICD17 <- word(Data_work$RAC,17)
Data_work$ICD16 <- word(Data_work$RAC,16)
Data_work$ICD15 <- word(Data_work$RAC,15)
Data_work$ICD14 <- word(Data_work$RAC,14)
Data_work$ICD13 <- word(Data_work$RAC,13)
Data_work$ICD12 <- word(Data_work$RAC,12)
Data_work$ICD11 <- word(Data_work$RAC,11)
Data_work$ICD10 <- word(Data_work$RAC,10)
Data_work$ICD9 <- word(Data_work$RAC,9)
Data_work$ICD8 <- word(Data_work$RAC,8)
Data_work$ICD7 <- word(Data_work$RAC,7)
Data_work$ICD6 <- word(Data_work$RAC,6)
Data_work$ICD5 <- word(Data_work$RAC,5)
Data_work$ICD4 <- word(Data_work$RAC,4)
Data_work$ICD3 <- word(Data_work$RAC,3)
Data_work$ICD2 <- word(Data_work$RAC,2)
Data_work$ICD1 <- word(Data_work$RAC,1)


Data_small <- Data_work[117:134]
Data_small[is.na(Data_small)] <- "0"

test <- data.frame(matrix(ncol = 18, nrow = length(Data_small$ICD18), NA))

for (i in (1:35000))  {
  for (j in (1:18)) {
    if (Data_small[i,j] == "")
    {
      test[i,j] <- Data_small[i,j-1]
    } else 
    {
      test[i,j] <- Data_small[i,j]
    }
  }
}

for (i in (35001:length(Data_small$ICD18)))  {
  for (j in (1:18)) {
    if (Data_small[i,j] == "")
    {
      test[i,j] <- Data_small[i,j-1]
    } else 
    {
      test[i,j] <- Data_small[i,j]
    }
  }
}

testV1 <- data.frame(matrix(ncol = 18, nrow = length(Data_small$ICD18), NA))
testblank <- test
testblank[testblank == "0"] <- "BLANK"

testV1$X1 <- ifelse(testblank$X1 == testblank$X2, "BLANK", testblank$X1)
testV1$X2 <- ifelse(testblank$X2 == testblank$X3, "BLANK", testblank$X2)
testV1$X3 <- ifelse(testblank$X3 == testblank$X4, "BLANK", testblank$X3)
testV1$X4 <- ifelse(testblank$X4 == testblank$X5, "BLANK", testblank$X4)
testV1$X5 <- ifelse(testblank$X5 == testblank$X6, "BLANK", testblank$X5)
testV1$X6 <- ifelse(testblank$X6 == testblank$X7, "BLANK", testblank$X6)
testV1$X7 <- ifelse(testblank$X7 == testblank$X8, "BLANK", testblank$X7)
testV1$X8 <- ifelse(testblank$X8 == testblank$X9, "BLANK", testblank$X8)
testV1$X9 <- ifelse(testblank$X9 == testblank$X10, "BLANK", testblank$X9)
testV1$X10 <- ifelse(testblank$X10 == testblank$X11, "BLANK", testblank$X10)
testV1$X11 <- ifelse(testblank$X11 == testblank$X12, "BLANK", testblank$X11)
testV1$X12 <- ifelse(testblank$X12 == testblank$X13, "BLANK", testblank$X12)
testV1$X13 <- ifelse(testblank$X13 == testblank$X14, "BLANK", testblank$X13)
testV1$X14 <- ifelse(testblank$X14 == testblank$X15, "BLANK", testblank$X14)
testV1$X15 <- ifelse(testblank$X15 == testblank$X16, "BLANK", testblank$X15)
testV1$X16 <- ifelse(testblank$X16 == testblank$X17, "BLANK", testblank$X16)
testV1$X17 <- ifelse(testblank$X17 == testblank$X18, "BLANK", testblank$X17)
testV1$X18 <- testblank$X18

#Iteration 1 is complete

test2 <- data.frame(matrix(ncol = 18, nrow = length(Data_small$ICD18), NA))

for (i in (1:35000))  {
  for (j in (2:18)) {
    if (testV1[i,j] == "BLANK")
    {
      test2[i,j] <- testV1[i,j-1]
    } else 
    {
      test2[i,j] <- testV1[i,j]
    }
  }
}

for (i in (35001:length(Data_small$ICD18)))  {
  for (j in (2:18)) {
    if (testV1[i,j] == "BLANK")
    {
      test2[i,j] <- testV1[i,j-1]
    } else 
    {
      test2[i,j] <- testV1[i,j]
    }
  }
}

testV2 <- data.frame(matrix(ncol = 18, nrow = length(Data_small$ICD18), NA))
testblank2 <- test2

testV2$X1 <- ifelse(testblank2$X1 == testblank2$X2, "BLANK", testblank2$X1)
testV2$X2 <- ifelse(testblank2$X2 == testblank2$X3, "BLANK", testblank2$X2)
testV2$X3 <- ifelse(testblank2$X3 == testblank2$X4, "BLANK", testblank2$X3)
testV2$X4 <- ifelse(testblank2$X4 == testblank2$X5, "BLANK", testblank2$X4)
testV2$X5 <- ifelse(testblank2$X5 == testblank2$X6, "BLANK", testblank2$X5)
testV2$X6 <- ifelse(testblank2$X6 == testblank2$X7, "BLANK", testblank2$X6)
testV2$X7 <- ifelse(testblank2$X7 == testblank2$X8, "BLANK", testblank2$X7)
testV2$X8 <- ifelse(testblank2$X8 == testblank2$X9, "BLANK", testblank2$X8)
testV2$X9 <- ifelse(testblank2$X9 == testblank2$X10, "BLANK", testblank2$X9)
testV2$X10 <- ifelse(testblank2$X10 == testblank2$X11, "BLANK", testblank2$X10)
testV2$X11 <- ifelse(testblank2$X11 == testblank2$X12, "BLANK", testblank2$X11)
testV2$X12 <- ifelse(testblank2$X12 == testblank2$X13, "BLANK", testblank2$X12)
testV2$X13 <- ifelse(testblank2$X13 == testblank2$X14, "BLANK", testblank2$X13)
testV2$X14 <- ifelse(testblank2$X14 == testblank2$X15, "BLANK", testblank2$X14)
testV2$X15 <- ifelse(testblank2$X15 == testblank2$X16, "BLANK", testblank2$X15)
testV2$X16 <- ifelse(testblank2$X16 == testblank2$X17, "BLANK", testblank2$X16)
testV2$X17 <- ifelse(testblank2$X17 == testblank2$X18, "BLANK", testblank2$X17)
testV2$X18 <- testblank2$X18

#Iteration 2 is complete

test3 <- data.frame(matrix(ncol = 18, nrow = length(Data_small$ICD18), NA))

for (i in (1:35000))  {
  for (j in (2:18)) {
    if (testV2[i,j] == "BLANK")
    {
      test3[i,j] <- testV2[i,j-1]
    } else 
    {
      test3[i,j] <- testV2[i,j]
    }
  }
}

for (i in (35001:length(Data_small$ICD18)))  {
  for (j in (2:18)) {
    if (testV2[i,j] == "BLANK")
    {
      test3[i,j] <- testV2[i,j-1]
    } else 
    {
      test3[i,j] <- testV2[i,j]
    }
  }
}

testV3 <- data.frame(matrix(ncol = 18, nrow = length(Data_small$ICD18), NA))
testblank3 <- test3

testV3$X1 <- ifelse(testblank3$X1 == testblank3$X2, "BLANK", testblank3$X1)
testV3$X2 <- ifelse(testblank3$X2 == testblank3$X3, "BLANK", testblank3$X2)
testV3$X3 <- ifelse(testblank3$X3 == testblank3$X4, "BLANK", testblank3$X3)
testV3$X4 <- ifelse(testblank3$X4 == testblank3$X5, "BLANK", testblank3$X4)
testV3$X5 <- ifelse(testblank3$X5 == testblank3$X6, "BLANK", testblank3$X5)
testV3$X6 <- ifelse(testblank3$X6 == testblank3$X7, "BLANK", testblank3$X6)
testV3$X7 <- ifelse(testblank3$X7 == testblank3$X8, "BLANK", testblank3$X7)
testV3$X8 <- ifelse(testblank3$X8 == testblank3$X9, "BLANK", testblank3$X8)
testV3$X9 <- ifelse(testblank3$X9 == testblank3$X10, "BLANK", testblank3$X9)
testV3$X10 <- ifelse(testblank3$X10 == testblank3$X11, "BLANK", testblank3$X10)
testV3$X11 <- ifelse(testblank3$X11 == testblank3$X12, "BLANK", testblank3$X11)
testV3$X12 <- ifelse(testblank3$X12 == testblank3$X13, "BLANK", testblank3$X12)
testV3$X13 <- ifelse(testblank3$X13 == testblank3$X14, "BLANK", testblank3$X13)
testV3$X14 <- ifelse(testblank3$X14 == testblank3$X15, "BLANK", testblank3$X14)
testV3$X15 <- ifelse(testblank3$X15 == testblank3$X16, "BLANK", testblank3$X15)
testV3$X16 <- ifelse(testblank3$X16 == testblank3$X17, "BLANK", testblank3$X16)
testV3$X17 <- ifelse(testblank3$X17 == testblank3$X18, "BLANK", testblank3$X17)
testV3$X18 <- testblank3$X18

#Iteration 3 is complete

test4 <- data.frame(matrix(ncol = 18, nrow = length(Data_small$ICD18), NA))

for (i in (1:35000))  {
  for (j in (3:18)) {
    if (testV3[i,j] == "BLANK")
    {
      test4[i,j] <- testV3[i,j-1]
    } else 
    {
      test4[i,j] <- testV3[i,j]
    }
  }
}

for (i in (35001:length(Data_small$ICD18)))  {
  for (j in (3:18)) {
    if (testV3[i,j] == "BLANK")
    {
      test4[i,j] <- testV3[i,j-1]
    } else 
    {
      test4[i,j] <- testV3[i,j]
    }
  }
}

testV4 <- data.frame(matrix(ncol = 18, nrow = length(Data_small$ICD18), NA))
testblank4 <- test4

testV4$X1 <- ifelse(testblank4$X1 == testblank4$X2, "BLANK", testblank4$X1)
testV4$X2 <- ifelse(testblank4$X2 == testblank4$X3, "BLANK", testblank4$X2)
testV4$X3 <- ifelse(testblank4$X3 == testblank4$X4, "BLANK", testblank4$X3)
testV4$X4 <- ifelse(testblank4$X4 == testblank4$X5, "BLANK", testblank4$X4)
testV4$X5 <- ifelse(testblank4$X5 == testblank4$X6, "BLANK", testblank4$X5)
testV4$X6 <- ifelse(testblank4$X6 == testblank4$X7, "BLANK", testblank4$X6)
testV4$X7 <- ifelse(testblank4$X7 == testblank4$X8, "BLANK", testblank4$X7)
testV4$X8 <- ifelse(testblank4$X8 == testblank4$X9, "BLANK", testblank4$X8)
testV4$X9 <- ifelse(testblank4$X9 == testblank4$X10, "BLANK", testblank4$X9)
testV4$X10 <- ifelse(testblank4$X10 == testblank4$X11, "BLANK", testblank4$X10)
testV4$X11 <- ifelse(testblank4$X11 == testblank4$X12, "BLANK", testblank4$X11)
testV4$X12 <- ifelse(testblank4$X12 == testblank4$X13, "BLANK", testblank4$X12)
testV4$X13 <- ifelse(testblank4$X13 == testblank4$X14, "BLANK", testblank4$X13)
testV4$X14 <- ifelse(testblank4$X14 == testblank4$X15, "BLANK", testblank4$X14)
testV4$X15 <- ifelse(testblank4$X15 == testblank4$X16, "BLANK", testblank4$X15)
testV4$X16 <- ifelse(testblank4$X16 == testblank4$X17, "BLANK", testblank4$X16)
testV4$X17 <- ifelse(testblank4$X17 == testblank4$X18, "BLANK", testblank4$X17)
testV4$X18 <- testblank4$X18

testV4$X1 <- testV1$X1
testV4$X2 <- testV2$X2
testV4$X3 <- testV3$X3

#Iteration 4 is complete

test5 <- data.frame(matrix(ncol = 18, nrow = length(Data_small$ICD18), NA))

for (i in (1:35000))  {
  for (j in (3:18)) {
    if (testV4[i,j] == "BLANK")
    {
      test5[i,j] <- testV4[i,j-1]
    } else 
    {
      test5[i,j] <- testV4[i,j]
    }
  }
}

for (i in (35001:length(Data_small$ICD18)))  {
  for (j in (3:18)) {
    if (testV4[i,j] == "BLANK")
    {
      test5[i,j] <- testV4[i,j-1]
    } else 
    {
      test5[i,j] <- testV4[i,j]
    }
  }
}

testV5 <- data.frame(matrix(ncol = 18, nrow = length(Data_small$ICD18), NA))
testblank5 <- test5

testV5$X1 <- ifelse(testblank5$X1 == testblank5$X2, "BLANK", testblank5$X1)
testV5$X2 <- ifelse(testblank5$X2 == testblank5$X3, "BLANK", testblank5$X2)
testV5$X3 <- ifelse(testblank5$X3 == testblank5$X4, "BLANK", testblank5$X3)
testV5$X4 <- ifelse(testblank5$X4 == testblank5$X5, "BLANK", testblank5$X4)
testV5$X5 <- ifelse(testblank5$X5 == testblank5$X6, "BLANK", testblank5$X5)
testV5$X6 <- ifelse(testblank5$X6 == testblank5$X7, "BLANK", testblank5$X6)
testV5$X7 <- ifelse(testblank5$X7 == testblank5$X8, "BLANK", testblank5$X7)
testV5$X8 <- ifelse(testblank5$X8 == testblank5$X9, "BLANK", testblank5$X8)
testV5$X9 <- ifelse(testblank5$X9 == testblank5$X10, "BLANK", testblank5$X9)
testV5$X10 <- ifelse(testblank5$X10 == testblank5$X11, "BLANK", testblank5$X10)
testV5$X11 <- ifelse(testblank5$X11 == testblank5$X12, "BLANK", testblank5$X11)
testV5$X12 <- ifelse(testblank5$X12 == testblank5$X13, "BLANK", testblank5$X12)
testV5$X13 <- ifelse(testblank5$X13 == testblank5$X14, "BLANK", testblank5$X13)
testV5$X14 <- ifelse(testblank5$X14 == testblank5$X15, "BLANK", testblank5$X14)
testV5$X15 <- ifelse(testblank5$X15 == testblank5$X16, "BLANK", testblank5$X15)
testV5$X16 <- ifelse(testblank5$X16 == testblank5$X17, "BLANK", testblank5$X16)
testV5$X17 <- ifelse(testblank5$X17 == testblank5$X18, "BLANK", testblank5$X17)
testV5$X18 <- testblank5$X18

testV5$X1 <- testV1$X1
testV5$X2 <- testV2$X2
testV5$X3 <- testV3$X3

#Iteration 5 is complete

test6 <- data.frame(matrix(ncol = 18, nrow = length(Data_small$ICD18), NA))

for (i in (1:35000))  {
  for (j in (3:18)) {
    if (testV5[i,j] == "BLANK")
    {
      test6[i,j] <- testV5[i,j-1]
    } else 
    {
      test6[i,j] <- testV5[i,j]
    }
  }
}

for (i in (35001:length(Data_small$ICD18)))  {
  for (j in (3:18)) {
    if (testV5[i,j] == "BLANK")
    {
      test6[i,j] <- testV5[i,j-1]
    } else 
    {
      test6[i,j] <- testV5[i,j]
    }
  }
}

testV6 <- data.frame(matrix(ncol = 18, nrow = length(Data_small$ICD18), NA))
testblank6 <- test6

testV6$X1 <- ifelse(testblank6$X1 == testblank6$X2, "BLANK", testblank6$X1)
testV6$X2 <- ifelse(testblank6$X2 == testblank6$X3, "BLANK", testblank6$X2)
testV6$X3 <- ifelse(testblank6$X3 == testblank6$X4, "BLANK", testblank6$X3)
testV6$X4 <- ifelse(testblank6$X4 == testblank6$X5, "BLANK", testblank6$X4)
testV6$X5 <- ifelse(testblank6$X5 == testblank6$X6, "BLANK", testblank6$X5)
testV6$X6 <- ifelse(testblank6$X6 == testblank6$X7, "BLANK", testblank6$X6)
testV6$X7 <- ifelse(testblank6$X7 == testblank6$X8, "BLANK", testblank6$X7)
testV6$X8 <- ifelse(testblank6$X8 == testblank6$X9, "BLANK", testblank6$X8)
testV6$X9 <- ifelse(testblank6$X9 == testblank6$X10, "BLANK", testblank6$X9)
testV6$X10 <- ifelse(testblank6$X10 == testblank6$X11, "BLANK", testblank6$X10)
testV6$X11 <- ifelse(testblank6$X11 == testblank6$X12, "BLANK", testblank6$X11)
testV6$X12 <- ifelse(testblank6$X12 == testblank6$X13, "BLANK", testblank6$X12)
testV6$X13 <- ifelse(testblank6$X13 == testblank6$X14, "BLANK", testblank6$X13)
testV6$X14 <- ifelse(testblank6$X14 == testblank6$X15, "BLANK", testblank6$X14)
testV6$X15 <- ifelse(testblank6$X15 == testblank6$X16, "BLANK", testblank6$X15)
testV6$X16 <- ifelse(testblank6$X16 == testblank6$X17, "BLANK", testblank6$X16)
testV6$X17 <- ifelse(testblank6$X17 == testblank6$X18, "BLANK", testblank6$X17)
testV6$X18 <- testblank6$X18

testV6$X1 <- testV1$X1
testV6$X2 <- testV2$X2
testV6$X3 <- testV3$X3

write.csv(testV6, "WendyData2020_Iteration6.csv")



################ MORE SCRATCH #####################
#I don't think this did anything but just in case

library(stringr)
library(readxl)
library(dplyr)

Data2020Alt <- read_excel("Race_2020_Data_Question.xlsx") #all 2020 data
Data2020Alt$Date <- paste0(Data2020Alt$DOD_YR, "-" ,Data2020Alt$DOD_MO, "-" ,Data2020Alt$DOD_DY)
Data2020Alt$Date <- as.Date(Data2020Alt$Date)
Data2020Alt_VAonly <- Data2020Alt[Data2020Alt$STATETEXT_R == "Virginia", ]


Data_work <- Data2020Alt_VAonly[!is.na(Data2020Alt_VAonly$ACME_UC), ]
Data_work$ICD1 <- word(Data_work$RAC,1)
Data_work$ICD2 <- word(Data_work$RAC,2)
Data_work$ICD3 <- word(Data_work$RAC,3)
Data_work$ICD4 <- word(Data_work$RAC,4)
Data_work$ICD5 <- word(Data_work$RAC,5)

Data_work$ICD6 <- word(Data_work$RAC,6)
Data_work$ICD7 <- word(Data_work$RAC,7)
Data_work$ICD8 <- word(Data_work$RAC,8)
Data_work$ICD9 <- word(Data_work$RAC,9)
Data_work$ICD10 <- word(Data_work$RAC,10)

Data_work$ICD11 <- word(Data_work$RAC,11)
Data_work$ICD12 <- word(Data_work$RAC,12)
Data_work$ICD13 <- word(Data_work$RAC,13)
Data_work$ICD14 <- word(Data_work$RAC,14)
Data_work$ICD15 <- word(Data_work$RAC,15)

Data_work$ICD16 <- word(Data_work$RAC,16)
Data_work$ICD17 <- word(Data_work$RAC,17)
Data_work$ICD18 <- word(Data_work$RAC,18)
#Data_work$ICD19 <- word(Data_work$RAC,19)
#Data_work$ICD20 <- word(Data_work$RAC,20)

Data_work$ICD18 <- word(Data_work$RAC,18)
Data_work$ICD17 <- word(Data_work$RAC,17)
Data_work$ICD16 <- word(Data_work$RAC,16)


Data_work$ICD18v1 <- NA
Data_work$ICD17v1 <- NA
Data_work$ICD16v1 <- NA

Data_small <- Data_work[117:127]
Data_small[is.na(Data_small)] <- "0"
Data_work[is.na(Data_work[117:127])] <- 0

test <- data.frame(matrix(ncol = 3, nrow = 100, NA))

for (i in (1:100))  {
  for (j in (7:11)) {
    if (Data_small[i,j] == "")
    {
      test[i,j-6] <- Data_small[i,j+1]
    } else 
    {
      test[i,j-6] <- Data_small[i,j]
    }
  }
}


for (i in (117:119)) {
  Data_work[,i+3] <- ifelse(Data_work[,i] == "", Data_work[,i] == Data_work[,i+1], Data_work[,i])
}

Data_work$ICD15v1 <- NA
Data_work$ICD14v1 <- NA
Data_work$ICD13v1 <- NA
Data_work$ICD12v1 <- NA

Data2020Alt_VAonly <- Data2020Alt[Data2020Alt$STATETEXT_R == "Virginia", ]
test <- na.omit(Data_work$ICD16)

for (j in (1:10))
  for (i in (117:119)) {
    if(Data_work[,i] == "") {
      Data_work[,i+3] == Data_work[,i+1]
    } else 
    {
      Data_work[,i]
    }
  }


for (i in (117:119)) {
  print(i)
  #print(Data_work[,i])
  #ifelse(Data_work[,i] == "", print("This is working"), print("Nope"))
  Data_work[,i] <- ifelse(Data_work[,i] == "", Data_work[,i] == Data_work[,i+1], Data_work[,i])
}

for (i in (117:119)) {
  for (j in (1:length(Data_work$CENSUS_TRACT))) {
    ifelse(Data_work[j,i] == "", Data_work[j,i] == 9999, Data_work[j,i])
  }
}


for (i in (133:117)) {
  print(i)
  ifelse(Data_work[,i] =="", Data_work[,i] == Data_work[,i+1], Data_work[,i])
}

Data_work$ICD1 <- ifelse(Data_work$ICD1 == "",Data_work$ICD2, Data_work$ICD1)
Data_work$ICD2 <- ifelse(Data_work$ICD2 == "",Data_work$ICD3, Data_work$ICD2)
Data_work$ICD3 <- ifelse(Data_work$ICD3 == Data_work$ICD2, "", Data_work$ICD3)

Data_work$ICD4 <- ifelse(Data_work$ICD4 == "", Data_work$ICD5, Data_work$ICD4)
Data_work$ICD3 <- ifelse(Data_work$ICD3 == "", Data_work$ICD4, Data_work$ICD3)
Data_work$ICD4 <- ifelse(Data_work$ICD4 == Data_work$ICD3, "", Data_work$ICD4)


Data_workNA <- Data_work %>%
  mutate_all(na_if,"")

for (i in (117:119)) {
  for (j in (1:77527)) {
    print(i)
    ifelse(Data_workNA[j,i] == "NA", Data_workNA[j,i] == Data_workNA[j,i+1], Data_workNA[j,i]) 
  }
}


ifelse(Data_workNA[,118] == NA, Data_workNA[,118] == Data_workNA[,118+1], Data_workNA[,118])





