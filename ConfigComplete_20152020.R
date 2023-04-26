#2015-2020 Configuration complete

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

#Loac MDC Key
ICD10_Mapping_v8 <- read_excel("~/Desktop/ICD10_Mapping_v8.xlsx", 
                               sheet = "MDC_Crosswalk")
ICD10_Mapping_v8$Mortality_ICD <-gsub("-$","",ICD10_Mapping_v8$Mortality_ICD)
key <- ICD10_Mapping_v8 %>%
  select(Mortality_ICD,MDC)

#Join MDC Key
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

#Create Final MDC Join
Final1520 <- Codes1520 
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

#Final1520 <- Final1520[order(Final1520$Date), ]

#write.csv(Final1520, file = "ICD_MDC_Matching_FULL0_13_2015_2020.csv")

#Map Data to Stations
VA_Links_NoCities <- read_excel("~/Desktop/VA_Links_NoCities.xlsx")
VA_Links_None <- VA_Links_NoCities

Codes520 <- Final1520

#

CountyText_R <- read_excel("~/Desktop/CountyText_R.xlsx", 
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

#write.csv(FullID20152020, file = "MDC_Station_Matching_1520.csv")


