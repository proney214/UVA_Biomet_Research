#Complete MDC Work

# MDC Code Matching for each column

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(zoo)
library(padr)

#Load in Data, create date column and sex checks

Codes0514 <- read.csv("~/Desktop/ICD_Cleaned_Datasets/all_ICD_cs05_final.csv")
Codes0514$SEX <- str_replace_all(Codes0514$SEX, c("1" = "M", "2" = "F", "9" = "U"))
Codes0514$ACME <- trimws(Codes0514$ACME, which = c("both"))
Codes0514$SexCheck <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ACME), 
                                    paste0(Codes0514$ACME,"-",Codes0514$SEX),Codes0514$ACME))  
Codes0514$SexCheck0 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_0), 
                                     paste0(Codes0514$ICD10_0,"-",Codes0514$SEX),Codes0514$ICD10_0)) 
Codes0514$SexCheck1 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_1), 
                                     paste0(Codes0514$ICD10_1,"-",Codes0514$SEX),Codes0514$ICD10_1)) 
Codes0514$SexCheck2 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_2), 
                                     paste0(Codes0514$ICD10_2,"-",Codes0514$SEX),Codes0514$ICD10_2)) 
Codes0514$SexCheck3 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_3), 
                                     paste0(Codes0514$ICD10_3,"-",Codes0514$SEX),Codes0514$ICD10_3)) 

Codes0514$SexCheck4 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_4), 
                                     paste0(Codes0514$ICD10_4,"-",Codes0514$SEX),Codes0514$ICD10_4)) 
Codes0514$SexCheck5 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_5), 
                                     paste0(Codes0514$ICD10_5,"-",Codes0514$SEX),Codes0514$ICD10_5)) 
Codes0514$SexCheck6 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_6), 
                                     paste0(Codes0514$ICD10_6,"-",Codes0514$SEX),Codes0514$ICD10_6)) 
Codes0514$SexCheck7 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_7), 
                                     paste0(Codes0514$ICD10_7,"-",Codes0514$SEX),Codes0514$ICD10_7)) 

Codes0514$SexCheck8 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_8), 
                                     paste0(Codes0514$ICD10_8,"-",Codes0514$SEX),Codes0514$ICD10_8)) 
Codes0514$SexCheck9 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_9), 
                                     paste0(Codes0514$ICD10_9,"-",Codes0514$SEX),Codes0514$ICD10_9)) 
Codes0514$SexCheck10 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_10), 
                                      paste0(Codes0514$ICD10_10,"-",Codes0514$SEX),Codes0514$ICD10_10)) 
Codes0514$SexCheck11 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_11), 
                                      paste0(Codes0514$ICD10_11,"-",Codes0514$SEX),Codes0514$ICD10_11)) 


Codes0514$SexCheck12 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_12), 
                                      paste0(Codes0514$ICD10_12,"-",Codes0514$SEX),Codes0514$ICD10_12)) 
Codes0514$SexCheck13 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_13), 
                                      paste0(Codes0514$ICD10_13,"-",Codes0514$SEX),Codes0514$ICD10_13)) 
Codes0514$SexCheck14 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_14), 
                                      paste0(Codes0514$ICD10_14,"-",Codes0514$SEX),Codes0514$ICD10_14)) 
Codes0514$SexCheck15 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes0514$ICD10_15), 
                                      paste0(Codes0514$ICD10_15,"-",Codes0514$SEX),Codes0514$ICD10_15)) 

Codes0514$Date = paste(Codes0514$DATE_OF_DEATH_YEAR,Codes0514$DATE_OF_DEATH_MONTH,Codes0514$DATE_OF_DEATH_DAY,sep="-")
Codes0514$Date <- as.Date(Codes0514$Date)
Codes0514$Date <- sub('0','2',Codes0514$Date)
Codes0514$Date <- as.Date(Codes0514$Date)

Codes1520 <- read.csv("~/Desktop/ICD_Cleaned_Datasets/all_ICD_cs15_final.csv")
Codes1520$Date = as.Date(paste(Codes1520$DOD_YR,Codes1520$DOD_MO,Codes1520$DOD_DY,sep="-"))
colnames(Codes1520)[which(names(Codes1520) == "ACME_UC")] <- "ACME"
Codes1520$ACME <-gsub("-$","",Codes1520$ACME)
Codes1520$SexCheck <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ACME), 
                                    paste0(Codes1520$ACME,"-",Codes1520$SEX),Codes1520$ACME))  

Codes1520$ACME <- trimws(Codes1520$ACME, which = c("both"))
Codes1520$SexCheck <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ACME), 
                                    paste0(Codes1520$ACME,"-",Codes1520$SEX),Codes1520$ACME))  
Codes1520$SexCheck0 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_0), 
                                     paste0(Codes1520$ICD10_0,"-",Codes1520$SEX),Codes1520$ICD10_0)) 
Codes1520$SexCheck1 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_1), 
                                     paste0(Codes1520$ICD10_1,"-",Codes1520$SEX),Codes1520$ICD10_1)) 
Codes1520$SexCheck2 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_2), 
                                     paste0(Codes1520$ICD10_2,"-",Codes1520$SEX),Codes1520$ICD10_2)) 
Codes1520$SexCheck3 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_3), 
                                     paste0(Codes1520$ICD10_3,"-",Codes1520$SEX),Codes1520$ICD10_3)) 

Codes1520$SexCheck4 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_4), 
                                     paste0(Codes1520$ICD10_4,"-",Codes1520$SEX),Codes1520$ICD10_4)) 
Codes1520$SexCheck5 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_5), 
                                     paste0(Codes1520$ICD10_5,"-",Codes1520$SEX),Codes1520$ICD10_5)) 
Codes1520$SexCheck6 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_6), 
                                     paste0(Codes1520$ICD10_6,"-",Codes1520$SEX),Codes1520$ICD10_6)) 
Codes1520$SexCheck7 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_7), 
                                     paste0(Codes1520$ICD10_7,"-",Codes1520$SEX),Codes1520$ICD10_7)) 

Codes1520$SexCheck8 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_8), 
                                     paste0(Codes1520$ICD10_8,"-",Codes1520$SEX),Codes1520$ICD10_8)) 
Codes1520$SexCheck9 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_9), 
                                     paste0(Codes1520$ICD10_9,"-",Codes1520$SEX),Codes1520$ICD10_9)) 
Codes1520$SexCheck10 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_10), 
                                      paste0(Codes1520$ICD10_10,"-",Codes1520$SEX),Codes1520$ICD10_10)) 
Codes1520$SexCheck11 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_11), 
                                      paste0(Codes1520$ICD10_11,"-",Codes1520$SEX),Codes1520$ICD10_11)) 


Codes1520$SexCheck12 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_12), 
                                      paste0(Codes1520$ICD10_12,"-",Codes1520$SEX),Codes1520$ICD10_12)) 
Codes1520$SexCheck13 <- paste0(ifelse(grepl("A548|A64|C763|C764|C765|Q564|Q998",Codes1520$ICD10_13), 
                                      paste0(Codes1520$ICD10_13,"-",Codes1520$SEX),Codes1520$ICD10_13)) 

ICD10_Mapping_v8 <- read_excel("~/Desktop/ICD10_Mapping_v8.xlsx", 
                               sheet = "MDC_Crosswalk")
ICD10_Mapping_v8$Mortality_ICD <-gsub("-$","",ICD10_Mapping_v8$Mortality_ICD)
key <- ICD10_Mapping_v8 %>%
  select(Mortality_ICD,MDC)

#2005-2014 matching each ICD code from ACME to the end

NewDF0514 = left_join(Codes0514,key, by = c("SexCheck" = "Mortality_ICD"))
NewDF0514_0 = left_join(Codes0514,key, by = c("SexCheck0" = "Mortality_ICD"))
NewDF0514_1 = left_join(Codes0514,key, by = c("SexCheck1" = "Mortality_ICD"))
NewDF0514_2 = left_join(Codes0514,key, by = c("SexCheck2" = "Mortality_ICD"))
NewDF0514_3 = left_join(Codes0514,key, by = c("SexCheck3" = "Mortality_ICD"))
NewDF0514_4 = left_join(Codes0514,key, by = c("SexCheck4" = "Mortality_ICD"))
NewDF0514_5 = left_join(Codes0514,key, by = c("SexCheck5" = "Mortality_ICD"))
NewDF0514_6 = left_join(Codes0514,key, by = c("SexCheck6" = "Mortality_ICD"))
NewDF0514_7 = left_join(Codes0514,key, by = c("SexCheck7" = "Mortality_ICD"))
NewDF0514_8 = left_join(Codes0514,key, by = c("SexCheck8" = "Mortality_ICD"))
NewDF0514_9 = left_join(Codes0514,key, by = c("SexCheck9" = "Mortality_ICD"))
NewDF0514_10 = left_join(Codes0514,key, by = c("SexCheck10" = "Mortality_ICD"))
NewDF0514_11 = left_join(Codes0514,key, by = c("SexCheck11" = "Mortality_ICD"))
NewDF0514_12 = left_join(Codes0514,key, by = c("SexCheck12" = "Mortality_ICD"))
NewDF0514_13 = left_join(Codes0514,key, by = c("SexCheck13" = "Mortality_ICD"))
NewDF0514_14 = left_join(Codes0514,key, by = c("SexCheck14" = "Mortality_ICD"))
NewDF0514_15 = left_join(Codes0514,key, by = c("SexCheck15" = "Mortality_ICD"))


Final0514 <- Codes0514 
Final0514$MDCAcme <- NewDF0514$MDC
Final0514$MDC0 <- NewDF0514_0$MDC
Final0514$MDC1 <- NewDF0514_1$MDC
Final0514$MDC2 <- NewDF0514_2$MDC
Final0514$MDC3 <- NewDF0514_3$MDC
Final0514$MDC4 <- NewDF0514_4$MDC
Final0514$MDC5 <- NewDF0514_5$MDC
Final0514$MDC6 <- NewDF0514_6$MDC
Final0514$MDC7 <- NewDF0514_7$MDC
Final0514$MDC8 <- NewDF0514_8$MDC
Final0514$MDC9 <- NewDF0514_9$MDC
Final0514$MDC10 <- NewDF0514_10$MDC
Final0514$MDC11 <- NewDF0514_11$MDC
Final0514$MDC12 <- NewDF0514_12$MDC
Final0514$MDC13 <- NewDF0514_13$MDC
Final0514$MDC14 <- NewDF0514_14$MDC
Final0514$MDC15 <- NewDF0514_15$MDC

#Final0514 <- Final0514[order(Final0514$Date), ]

# write.csv(Final0514, file = "ICD_MDC_Matching_FULL0_15_2005_2014.csv")

#^writing new file

#2015-2020 matching each ICD code

NewDF1520 = left_join(Codes1520,key, by = c("SexCheck" = "Mortality_ICD"))
NewDF1520_0 = left_join(Codes1520,key, by = c("SexCheck0" = "Mortality_ICD"))
NewDF1520_1 = left_join(Codes1520,key, by = c("SexCheck1" = "Mortality_ICD"))
NewDF1520_2 = left_join(Codes1520,key, by = c("SexCheck2" = "Mortality_ICD"))
NewDF1520_3 = left_join(Codes1520,key, by = c("SexCheck3" = "Mortality_ICD"))
NewDF1520_4 = left_join(Codes1520,key, by = c("SexCheck4" = "Mortality_ICD"))
NewDF1520_5 = left_join(Codes1520,key, by = c("SexCheck5" = "Mortality_ICD"))
NewDF1520_6 = left_join(Codes1520,key, by = c("SexCheck6" = "Mortality_ICD"))
NewDF1520_7 = left_join(Codes1520,key, by = c("SexCheck7" = "Mortality_ICD"))
NewDF1520_8 = left_join(Codes1520,key, by = c("SexCheck8" = "Mortality_ICD"))
NewDF1520_9 = left_join(Codes1520,key, by = c("SexCheck9" = "Mortality_ICD"))
NewDF1520_10 = left_join(Codes1520,key, by = c("SexCheck10" = "Mortality_ICD"))
NewDF1520_11 = left_join(Codes1520,key, by = c("SexCheck11" = "Mortality_ICD"))
NewDF1520_12 = left_join(Codes1520,key, by = c("SexCheck12" = "Mortality_ICD"))
NewDF1520_13 = left_join(Codes1520,key, by = c("SexCheck13" = "Mortality_ICD"))

Final1520 <- Codes1520 
Final1520$MDCAcme <- NewDF1520$MDC
Final1520$MDC0 <- NewDF1520_0$MDC
Final1520$MDC1 <- NewDF1520_1$MDC
Final1520$MDC2 <- NewDF1520_2$MDC
Final1520$MDC3 <- NewDF1520_3$MDC
Final1520$MDC4 <- NewDF1520_4$MDC
Final1520$MDC5 <- NewDF1520_5$MDC
Final1520$MDC6 <- NewDF1520_6$MDC
Final1520$MDC7 <- NewDF1520_7$MDC
Final1520$MDC8 <- NewDF1520_8$MDC
Final1520$MDC9 <- NewDF1520_9$MDC
Final1520$MDC10 <- NewDF1520_10$MDC
Final1520$MDC11 <- NewDF1520_11$MDC
Final1520$MDC12 <- NewDF1520_12$MDC
Final1520$MDC13 <- NewDF1520_13$MDC

df = subset(Final1520, select = -c(ICD10_0.1,ICD10_1.1,ICD10_2.1,ICD10_3.1,ICD10_4.1,ICD10_5.1,
                                   ICD10_6.1,ICD10_7.1,ICD10_8.1,ICD10_9.1,ICD10_10.1,ICD10_11.1,
                                   ICD10_12.1,ICD10_13.1))

write.csv(df, file = "ICD_MDC_Matching_FULL0_13_2015_2020.csv")

#Now that MDC Mapping is complete, map each patient to a county and then Wx station

Codes1520 <- read.csv("ICD_MDC_Matching_FULL0_13_2015_2020.csv",na = "NA")
Codes520 <- Codes1520

#Codes1520 <- read_excel("~/Desktop/Codes1520.xlsx",na = "NA") Just to see what excel file it resides in

VA_Links_NoCities <- read_excel("VA_Links_NoCities.xlsx")
VA_Links_None <- VA_Links_NoCities

# Actually join the counties

CountyText_R <- read_excel("CountyText_R.xlsx", 
                           col_types = c("text"))

Codes520$Text_R <- CountyText_R$COUNTYTEXT_R
Codes520$Text_R <- trimws(Codes520$Text_R, which = c("both"))

Codes520$Text_R <- tolower(Codes520$Text_R)
VA_Links_None$County <- tolower(VA_Links_None$County)

test6 <- left_join(Codes520,VA_Links_None,by = c("Text_R" = "County"))

table(test6$ID)

NAS6 <- test6[is.na(test6$ID), ]
NAS6va <- NAS6[NAS6$STATETEXT_R == "Virginia", ]
table(NAS6va$Text_R)

FullID20152020 <- test6[!is.na(test6$ID),]

names(FullID20152020)[names(FullID20152020) == 'SexCheck'] <- 'ICD_SC'
names(FullID20152020)[names(FullID20152020) == 'SexCheck0'] <- 'ICD_SC0'
names(FullID20152020)[names(FullID20152020) == 'SexCheck1'] <- 'ICD_SC1'
names(FullID20152020)[names(FullID20152020) == 'SexCheck2'] <- 'ICD_SC2'
names(FullID20152020)[names(FullID20152020) == 'SexCheck3'] <- 'ICD_SC3'
names(FullID20152020)[names(FullID20152020) == 'SexCheck4'] <- 'ICD_SC4'
names(FullID20152020)[names(FullID20152020) == 'SexCheck5'] <- 'ICD_SC5'
names(FullID20152020)[names(FullID20152020) == 'SexCheck6'] <- 'ICD_SC6'
names(FullID20152020)[names(FullID20152020) == 'SexCheck7'] <- 'ICD_SC7'
names(FullID20152020)[names(FullID20152020) == 'SexCheck8'] <- 'ICD_SC8'
names(FullID20152020)[names(FullID20152020) == 'SexCheck9'] <- 'ICD_SC9'
names(FullID20152020)[names(FullID20152020) == 'SexCheck10'] <- 'ICD_SC10'
names(FullID20152020)[names(FullID20152020) == 'SexCheck11'] <- 'ICD_SC11'
names(FullID20152020)[names(FullID20152020) == 'SexCheck12'] <- 'ICD_SC12'
names(FullID20152020)[names(FullID20152020) == 'SexCheck13'] <- 'ICD_SC13'
names(FullID20152020)[names(FullID20152020) == 'SexCheck14'] <- 'ICD_SC14'
names(FullID20152020)[names(FullID20152020) == 'SexCheck15'] <- 'ICD_SC15'

Cul <- FullID20152020[FullID20152020$Text_R == "culpeper county", ]

#write.csv(FullID20152020, file = "MDC_Station_Matching_1520.csv")

# 2005-2014 County Join

Codes0514 <- read.csv("ICD_MDC_Matching_FULL0_15_2005_2014.csv")
#Codes0514 <- read_excel("~/Desktop/Codes0514_V2.xlsx")
Locality_Crosswalk <- read_excel("~/Desktop/Locality_Crosswalk.xlsx")
VA_City_Link_V2 <- read_excel("VA_City_Link_V2.xlsx")
VA_City_Link_V2$County <- trimws(VA_City_Link_V2$County, which = c("both"))

#Create Proper Matching Columm

Census_Tract <- read_excel("Census_Tract.xlsx", 
                           col_types = c("text"))

test2 <- data.frame(substring(Census_Tract$CENSUS_TRACT,1,5))

Codes0514$PLRact <- test2$substring.Census_Tract.CENSUS_TRACT..1..5.

test4 <- left_join(Codes0514,Locality_Crosswalk,by = c("PLRact" = "VR_CODE_TXT"))

test5 <- left_join(test4,VA_City_Link_V2, by = c("LOCALITYNAME" = "County"))

NAS5 <- test5[is.na(test5$ID), ]

FullID20052014 <- test5[!is.na(test5$ID),]

Cul0514 <- FullID20052014[FullID20052014$LOCALITYNAME == "Culpeper County", ]

#write.csv(FullID20052014, file = "MDC_Station_Matching_0514.csv")


#Create Excel Sheets for each MDC by station

FullSM_1520 <- read.csv("MDC_Station_Matching_1520.csv")

FullNAS <- FullSM_1520[is.na(FullSM_1520$MDCAcme), ]

groupsEZF1520 <- FullSM_1520 %>%
  filter(ID == "EZF") %>%
  group_by(MDCAcme) %>%
  summarize(count_by_siteyear =  n()) 

NAS <- groupsEZF1520[is.na(groupsEZF1520$MDCAcme), ]

groupswideEZF1520 <- pivot_wider(groupsEZF1520,id_cols = "Date",names_from = MDCAcme,values_from = count_by_siteyear)

OrderedEZF1520 <- groupswideEZF1520 %>% relocate(`MDC 02`, `MDC 03`,`MDC 04`,`MDC 05`,`MDC 06`,`MDC 07`,
                                                 `MDC 08`, `MDC 09`, `MDC 10`,`MDC 11`, `MDC 12`,
                                                 `MDC 13`, `MDC 14`, `MDC 15`, `MDC 16`, `MDC 17`,
                                                 `MDC 18`, `MDC 19`, `MDC 20`, `MDC 21`,#`MDC 22`,
                                                 `MDC 23`, `MDC 25`,#`MDC 24`,
                                                 `Cancer NOS`,`Non-billable ICD`,.after = `MDC 01`) 

#2005-2014 Dataset

FullSM_0514 <- read.csv("MDC_Station_Matching_0514.csv")

FullNAS0514 <- FullSM_0514[is.na(FullSM_0514$MDCAcme), ]

groupsEZF0514 <- FullSM_0514 %>%
  filter(ID == "EZF") %>%
  group_by(Date, MDCAcme) %>%
  summarize(count_by_siteyear =  n()) 

#NAS <- groupsEZF0514[is.na(groupsEZF0514$MDCAcme), ]

#NAS0514 <- FullSM_0514[is.na(FullSM_0514$MDCAcme), ]

#write.csv(NAS0514, file = "MDC_NAs_2005_2014.csv")

groupswideEZF0514 <- pivot_wider(groupsEZF0514,id_cols = "Date",names_from = MDCAcme,values_from = count_by_siteyear)

OrderedEZF0514 <- groupswideEZF0514 %>% relocate( `MDC 03`,`MDC 04`,`MDC 05`,`MDC 06`,`MDC 07`,
                                                  `MDC 08`, `MDC 09`, `MDC 10`,`MDC 11`, `MDC 12`,
                                                  `MDC 13`, `MDC 14`, `MDC 15`, `MDC 16`, `MDC 17`,
                                                  `MDC 18`, `MDC 19`, `MDC 20`, `MDC 21`,#`MDC 22`,
                                                  `MDC 23`, `MDC 25`,#`MDC 24`,
                                                  `Cancer NOS`,`Non-billable ICD`,.after = `MDC 01`)
OrderedEZF0514$`MDC 02` <- NA

EZFFullMDC <- rbind(OrderedEZF0514,OrderedEZF1520)

EZFFullMDC$`MDC 22` <- NA
EZFFullMDC$`MDC 24` <- NA

EZFFullMDCdf <- data.frame(EZFFullMDC)
EZFFullMDCdf$Date <- as.Date(EZFFullMDCdf$Date)

EZFFullMDCpad <- pad(EZFFullMDCdf, interval = "day")#, start_val = as.POSIXct('2005-01-01'), end_val = as.POSIXct('2020-12-31'))

write.csv(EZFFullMDCpad, file = "EZFFullMDC.csv")

