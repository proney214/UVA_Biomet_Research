
library(dplyr)
library(stringr)
library(readxl)
library(zoo)
library(padr)
library(tidyr)

FullSM_0514 <- read.csv("MDC_Station_Matching_0514.csv")

#New Race Categorizatoins

FullSM_0514$RACE_Update <- str_replace_all(FullSM_0514$RACE, c("0"= "Other",
                                                   "1"="White",
                                                   "2"="Black",
                                                   "3"="AmericanIndian/AlaskanNative",
                                                   "4"="Chinese",
                                                   "5"="Japanese",
                                                   "6"="NativeHawaiian",
                                                   "7"="Filipino",
                                                   "8"="Unknown",
                                                   "9"="Unknown",
                                                   "A"="AsianIndian",
                                                   "B"="Korean",
                                                   "C"="Samoan",
                                                   "D"="Vietnamese",
                                                   "E"="GuamianOrChamorro",
                                                   "F"="OtherAsian"))

FullSM_0514$RACE_Update <- str_replace_all(FullSM_0514$RACE_Update, c("Koreanlack"="Black",
                                                                      "AsianIndianmericanIndian/AsianIndianlaskanNative"="AmericanIndian/AlaskanNative",
                                                                      "OtherAsianilipino"="Filipino",
                                                                      "Samoanhinese"="Chinese"))
FullSM_0514$RaceGroupings <- str_replace_all(FullSM_0514$RACE_Update, c("Chinese|Japanese|Filipino|AsianIndian|Korean|Vietnamese|Samoan|GuamianOrChamorro|NativeHawaiian|OtherAsian"="AsianPacificIslander",
                                                                        "Other"="Unknown"))

table(FullSM_0514$RaceGroupings)

#Race 2015

FullSM_1520 <- read.csv("MDC_Station_Matching_1520.csv")

FullSM_1520$NCHSBRIDGE <- sub("^$", "NAAA", FullSM_1520$NCHSBRIDGE)

FullSM_1520$RaceUpdate <- str_replace_all(FullSM_1520$NCHSBRIDGE, c("00"="Other",
                                                                    "01|10|11|12|13|14|15"="White",
                                                                    "02"="Black",
                                                                    "03"="AmericanIndian/AlaskanNative",
                                                                    "04"="Chinese",
                                                                    "05"="Japanese",
                                                                    "06"="NativeHawaiian",
                                                                    "07"="Filipino",
                                                                    "08"="Unknown",
                                                                    "09|99"="Unknown",
                                                                    "0A"="AsianIndian",
                                                                    "0B"="Korean",
                                                                    "0C"="Samoan",
                                                                    "0D"="Vietnamese",
                                                                    "0E"="GuamianOrChamorro",
                                                                    "0F"="OtherAsian",
                                                                    "0G"="Unknown",
                                                                    "NAAA"= "Unknown"))


table(FullSM_1520$RaceUpdate)
table(FullSM_1520$NCHSBRIDGE)

FullSM_1520$RaceGroupings <- str_replace_all(FullSM_1520$RaceUpdate, c("Chinese|Japanese|Filipino|AsianIndian|Korean|Vietnamese|Samoan|GuamianOrChamorro|NativeHawaiian|OtherAsian"="AsianPacificIslander",
                                                                        "Other"="Unknown"))
table(FullSM_1520$RaceGroupings)

# Fix Ages

Age_Key <- read_excel("Age_Key.xlsx")

#2005-2014 Data

FullSM_0514$AGE_OF_DECEASED_Update <- ifelse(FullSM_0514$AGE_OF_DECEASED > 110, 1, FullSM_0514$AGE_OF_DECEASED)

FullSM_0514_AgeGroups <- left_join(FullSM_0514,Age_Key, by = c("AGE_OF_DECEASED_Update"="Age"))

table(FullSM_0514_AgeGroups$Group)

#2015-2020 Data

FullSM_1520$AGE_OF_DECEASED_Update <- ifelse(FullSM_1520$AGE > 110, 1, FullSM_1520$AGE)

FullSM_1520_AgeGroups <- left_join(FullSM_1520,Age_Key, by = c("AGE_OF_DECEASED_Update"="Age"))

#Gather Race Data

groupsEZF0514_Race <- FullSM_0514_AgeGroups %>%
  filter(ID == "EZF") %>%
  group_by(Date, RaceGroupings) %>%
  summarize(count_by_siteyear =  n()) 

groupsEZF1520_Race <- FullSM_1520_AgeGroups %>%
  filter(ID == "EZF") %>%
  group_by(Date, RaceGroupings) %>%
  summarize(count_by_siteyear =  n()) 

groupswideEZF0514_Race <- pivot_wider(groupsEZF0514_Race,id_cols = "Date",names_from = RaceGroupings,values_from = count_by_siteyear)
groupswideEZF1520_Race <- pivot_wider(groupsEZF1520_Race,id_cols = "Date",names_from = RaceGroupings,values_from = count_by_siteyear)

EZFFullRace <- rbind(groupswideEZF0514_Race,groupswideEZF1520_Race)

EZFFullRacedf <- data.frame(EZFFullRace)
EZFFullRacedf$Date <- as.Date(EZFFullRacedf$Date)

EZFFullRacepad <- pad(EZFFullRacedf, interval = "day")#, start_val = as.POSIXct('2005-01-01'), end_val = as.POSIXct('2020-12-31'))

EZFFullRacepad[is.na(EZFFullRacepad)] <- 0

#

#Gather Age Data
groupsEZF0514_Age <- FullSM_0514_AgeGroups %>%
  filter(ID == "EZF") %>%
  group_by(Date, Group) %>%
  summarize(count_by_siteyear =  n()) 

groupsEZF1520_Age <- FullSM_1520_AgeGroups %>%
  filter(ID == "EZF") %>%
  group_by(Date, Group) %>%
  summarize(count_by_siteyear =  n()) 

groupswideEZF0514_Age <- pivot_wider(groupsEZF0514_Age,id_cols = "Date",names_from = Group,values_from = count_by_siteyear)
groupswideEZF1520_Age <- pivot_wider(groupsEZF1520_Age,id_cols = "Date",names_from = Group,values_from = count_by_siteyear)

EZFFullAge <- rbind(groupswideEZF0514_Age,groupswideEZF1520_Age)

EZFFullAgedf <- data.frame(EZFFullAge)
EZFFullAgedf$Date <- as.Date(EZFFullAgedf$Date)

EZFFullAgepad <- pad(EZFFullAgedf, interval = "day")#, start_val = as.POSIXct('2005-01-01'), end_val = as.POSIXct('2020-12-31'))

EZFFullAgepad[is.na(EZFFullAgepad)] <- 0

# Combine Age and Race Data

EZFDemographics <- left_join(EZFFullAgepad, EZFFullRacepad, by = c("Date"="Date"))

OrderedEZFDemographics <- EZFDemographics %>% relocate(X0.11,X12.17,X18.29,X30.39,
                                                       X40.49,X50.64,X65.74,X75.,
                                                       Black,White,AsianPacificIslander,AmericanIndian.AlaskanNative,
                                                       Unknown,.after = Date)
#Gather MDC Data

groupsEZF1520 <- FullSM_1520 %>%
  filter(ID == "EZF") %>%
  group_by(Date,MDCAcme) %>%
  summarize(count_by_siteyear =  n()) 

groupswideEZF1520 <- pivot_wider(groupsEZF1520,id_cols = "Date",names_from = MDCAcme,values_from = count_by_siteyear)

OrderedEZF1520 <- groupswideEZF1520 %>% relocate(`MDC 02`, `MDC 03`,`MDC 04`,`MDC 05`,`MDC 06`,`MDC 07`,
                                                 `MDC 08`, `MDC 09`, `MDC 10`,`MDC 11`, `MDC 12`,
                                                 `MDC 13`, `MDC 14`, `MDC 15`, `MDC 16`, `MDC 17`,
                                                 `MDC 18`, `MDC 19`, `MDC 20`, `MDC 21`,#`MDC 22`,
                                                 `MDC 23`, `MDC 25`,#`MDC 24`,
                                                 `Cancer NOS`,`Non-billable ICD`,.after = `MDC 01`) 

#2005-2014 Dataset

groupsEZF0514 <- FullSM_0514 %>%
  filter(ID == "EZF") %>%
  group_by(Date, MDCAcme) %>%
  summarize(count_by_siteyear =  n())

groupswideEZF0514 <- pivot_wider(groupsEZF0514,id_cols = "Date",names_from = MDCAcme,values_from = count_by_siteyear)

OrderedEZF0514 <- groupswideEZF0514 %>% relocate( `MDC 02`,`MDC 03`,`MDC 04`,`MDC 05`,`MDC 06`,`MDC 07`,
                                                  `MDC 08`, `MDC 09`, `MDC 10`,`MDC 11`, `MDC 12`,
                                                  `MDC 13`, `MDC 14`, `MDC 15`, `MDC 16`, `MDC 17`,
                                                  `MDC 18`, `MDC 19`, `MDC 20`, `MDC 21`,#`MDC 22`,
                                                  `MDC 23`, `MDC 25`,#`MDC 24`,
                                                  `Cancer NOS`,`Non-billable ICD`,.after = `MDC 01`)

EZFFullMDC <- rbind(OrderedEZF0514,OrderedEZF1520)

EZFFullMDC$`MDC 22` <- NA
EZFFullMDC$`MDC 24` <- NA

EZFFullMDCdf <- data.frame(EZFFullMDC)
EZFFullMDCdf$Date <- as.Date(EZFFullMDCdf$Date)

EZFFullMDCpad <- pad(EZFFullMDCdf, interval = "day")#, start_val = as.POSIXct('2005-01-01'), end_val = as.POSIXct('2020-12-31'))

EZFFullMDCpad[is.na(EZFFullMDCpad)] <- 0

#write.csv(EZFFullMDCpad, file = "EZFFullMDC.csv")

#Gather SEX Data

groupsEZF0514_SEX <- FullSM_0514_AgeGroups %>%
  filter(ID == "EZF") %>%
  group_by(Date, SEX) %>%
  summarize(count_by_siteyear =  n()) 

groupsEZF1520_SEX <- FullSM_1520_AgeGroups %>%
  filter(ID == "EZF") %>%
  group_by(Date, SEX) %>%
  summarize(count_by_siteyear =  n()) 

groupswideEZF0514_SEX <- pivot_wider(groupsEZF0514_SEX,id_cols = "Date",names_from = SEX,values_from = count_by_siteyear)
groupswideEZF1520_SEX <- pivot_wider(groupsEZF1520_SEX,id_cols = "Date",names_from = SEX,values_from = count_by_siteyear)

EZFFullSEX <- rbind(groupswideEZF0514_SEX,groupswideEZF1520_SEX)

EZFFullSEXdf <- data.frame(EZFFullSEX)
EZFFullSEXdf$Date <- as.Date(EZFFullSEXdf$Date)

EZFFullSEXpad <- pad(EZFFullSEXdf, interval = "day")#, start_val = as.POSIXct('2005-01-01'), end_val = as.POSIXct('2020-12-31'))

EZFFullSEXpad[is.na(EZFFullSEXpad)] <- 0

#write.csv(EZFFullSEXpad,"EZFMortUltimate_SEXOnly.csv")

#Ultimate Mortality

UltMort_EZF_almost <- left_join(EZFFullMDCpad,OrderedEZFDemographics,by= c("Date"="Date"))
UltMort_EZF <- left_join(UltMort_EZF_almost,EZFFullSEXpad,by= c("Date"="Date"))

#write.csv(UltMort_EZF, file = "EZFMortUltimate.csv")




