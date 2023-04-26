#More General Population Aggregation for 20#11-18

library(readxl)
library(dplyr)
library(stringr)
library(zoo)
library(padr)
library(ggplot2)
library(tidyr)

#Load in yearly population data
Census_2015_AgeSexEstimates_forVA <- read_excel("~/Downloads/Census_2015_AgeSexEstimates_forVA.xls", 
                                                skip = 4)
columns <- Census_2015_AgeSexEstimates_forVA

df <- Census_2015_AgeSexEstimates_forVA %>%
  mutate_if(is.character, as.numeric)

#Add together male and female values to create totals
for (i in (1:23)) {
  df[(48+i), ] <- df[(2+i), ] + df[(25+i), ]
}

dfa <- df[49:71, ]

#Transpose data (flip rows and columns)
dfb<-as.data.frame(t(dfa))
dfb <- dfb[-1,]

#Put stations in alphabetical order
dfb <- dfb[order(rownames(dfb)),]

#CorrectBedford for 20#11 and 20#12
#dfb[12, ] <- dfb[11, ] + dfb[12, ]
#dfb <- dfb[-11, ]

#Actually insert column names
names(dfb) <- columns$Locality[3:25]
colnames(dfb)[1] <- "Total"

#Get Station IDs
VA_county_wxstation_links_1_ <- read_excel("~/Downloads/VA.county.wxstation.links (1).xlsx")
stat <- VA_county_wxstation_links_1_$ID
stat <- str_replace_all(stat, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))

#Combine Station ID with df
dfb$StationID <- stat
dfinal <- dfb

#Get data per age group
total2015 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop2015=sum(Total))

totaleld2015 <- dfinal %>%
  group_by(StationID) %>%
  summarise(popeld_2015=sum(`75 to 79 years`,`80 to 84 years`,`85 years and over`))

total65_74_2015 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop65_74_2015=sum(`65 to 69 years`,`70 to 74 years`))

total50_64_2015 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop50_64_2015=sum(`50 to 54 years`,`55 to 59 years`,`60 to 64 years`))

total40_49_2015 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop40_49_2015=sum(`40 to 44 years`,`45 to 49 years`))

total30_39_2015 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop30_39_2015=sum(`30 to 34 years`,`35 to 39 years`))

total20_29_2015 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop20_29_2015=sum(`20 to 24 years`,`25 to 29 years`))

total10_19_2015 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop10_19_2015=sum(`10 to 14 years`,`15 to 19 years`))

total0_9_2015 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop0_9_2015=sum(`Under 5 years`,`5 to 9 years`))

dfpop2015 <- cbind(total2015,total0_9_2015,total10_19_2015,total20_29_2015,total30_39_2015,total40_49_2015,total50_64_2015,total65_74_2015,totaleld2015)
dfpop2015 <- dfpop2015[!duplicated(as.list(dfpop2015))]
