#2015-2020 County Join

library(dplyr)

Codes1520 <- read_excel("~/Desktop/Codes1520.xlsx", 
                        na = "NA")
VA_Links_NoCities <- read_excel("~/Desktop/VA_Links_NoCities.xlsx")
VA_Links_None <- VA_Links_NoCities

Codes520 <- Codes1520

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

write.csv(FullID20152020, file = "MDC_Station_Matching_1520.csv")

