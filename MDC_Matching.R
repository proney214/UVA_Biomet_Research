# MDC Code Matching

library(readxl)
library(dplyr)
library(stringr)

#Load in Data

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

#2005-2014

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
Final0514$MDCAcme <- NewDf0514$MDC
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

Final0514 <- Final0514[order(Final0514$Date), ]

write.csv(Final0514, file = "ICD_MDC_Matching_FULL0_15_2005_2014.csv")

#2015-2020

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

write.csv(Final1520, file = "ICD_MDC_Matching_FULL0_13_2015_2020.csv")

NewDF1520 = left_join(Codes1520,key, by = c("SexCheck" = "Mortality_ICD"))
NewDF1520 <- NewDF1520[order(NewDF1520$Date), ]

#write.csv(NewDF1520, file = "ICD_MDC_Matching_2015_2020.csv")

