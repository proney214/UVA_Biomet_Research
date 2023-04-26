#Mapping MDC to localities

library(readxl)
library(stringr)
library(dplyr)

#2005-2014 Place of Residence Mapping

Census_Tract <- read_excel("~/Desktop/Census_Tract.xlsx", 
                           col_types = c("text"))

test2 <- data.frame(substring(Census_Tract$CENSUS_TRACT,1,5))

Codes0514$PLRact <- test2$substring.Census_Tract.CENSUS_TRACT..1..5.

test4 <- left_join(Codes0514,Locality_Crosswalk,by = c("PLRact" = "VR_CODE_TXT"))

test5 <- left_join(test4,VA_City_Link_V2, by = c("LOCALITYNAME" = "County"))

NAS5 <- test5[is.na(test5$ID), ]

FullID20052014 <- test5[!is.na(test5$ID),]

Cul0514 <- FullID20052014[FullID20052014$LOCALITYNAME == "Culpeper County", ]

write.csv(FullID20052014, file = "MDC_Station_Matching_0514.csv")

