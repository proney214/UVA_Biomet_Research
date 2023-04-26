##Creating station population age groups and gender for long data (20#19+20#20)

library(readxl)
library(dplyr)
library(stringr)
library(zoo)
library(padr)
library(ggplot2)
library(tidyr)

VA_county_wxstation_links_1_ <- read_excel("~/Downloads/VA.county.wxstation.links (1).xlsx")
stat <- VA_county_wxstation_links_1_$ID

#Data for 20#20 (.xlsx) and data for 20#19 is (.xls)
Census_2020_AgeSexEstimates_forVA <- read_excel("~/Downloads/Census_2020_AgeSexEstimates_forVA.xlsx", 
                                                skip = 4)


#Make dataframes
df <- Census_2020_AgeSexEstimates_forVA

#Put rows in alphabelitcal order
df <- df[order(df$Locality),]

#Adding in and cleaning up station IDs
df$StationID <- stat
df$StationID <- str_replace_all(df$StationID, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))
dfinal <- select(df,-c(26:71))

VJI <- dfinal[dfinal$StationID == "VJI", ]

#Get data per age group
total2020 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop2020=sum(Total))

totaleld2020 <- dfinal %>%
  group_by(StationID) %>%
  summarise(popeld_2020=sum(`75 to 79 years...19`,`80 to 84 years...20`,`85 years and over...21`))

total65_74_2020 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop65_74_2020=sum(`65 to 69 years...17`,`70 to 74 years...18`))

total50_64_2020 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop50_64_2020=sum(`50 to 54 years...14`,`55 to 59 years...15`,`60 to 64 years...16`))

total40_49_2020 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop40_49_2020=sum(`40 to 44 years...12`,`45 to 49 years...13`))

total30_39_2020 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop30_39_2020=sum(`30 to 34 years...10`,`35 to 39 years...11`))

total20_29_2020 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop20_29_2020=sum(`20 to 24 years...8`,`25 to 29 years...9`))

total10_19_2020 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop10_19_2020=sum(`10 to 14 years...6`,`15 to 19 years...7`))

total0_9_2020 <- dfinal %>%
  group_by(StationID) %>%
  summarise(pop0_9_2020=sum(`Under 5 years...4`,`5 to 9 years...5`))

dfpop2020 <- cbind(total2020,total0_9_2020,total10_19_2020,total20_29_2020,total30_39_2020,total40_49_2020,total50_64_2020,total65_74_2020,totaleld2020)
dfpop2020 <- dfpop2020[!duplicated(as.list(dfpop2020))]
dfpop2020 <- dfpop2020[1:13, ]


#Standard population
V_A2020stdpopraw <- dfpop2020[12,]
V_A2020stdpopcalc <- (1000000 * V_A2020stdpopraw[,3:10])/V_A2020stdpopraw[,2]
#View(rowSums(V_A2020stdpopcalc))
V_A2020stdpoprev <- rev(V_A2020stdpopcalc)
V_A2020stdpopfinal <- data.frame(c(V_A2020stdpopraw [,1:2],V_A2020stdpoprev))



#Make Plots
library(tidyverse)

V %>%
  pivot_longer(cols = -meta1) %>%
  ggplot(aes(value)) +
  facet_wrap(~ meta1, scales = "free") +
  geom_histogram()


bartry <- t(V_A2020stdpopfinal[,3:10])
hist(bartry)
barplot(bartry)

bart <- t(bartry)
bart <- data.frame(rev(bart))
rownames(bart) <- colnames(V_A2020stdpoprev)

barplot(bart, xlab = "Population Group", ylab = "Count", ylim = c(0, 200000))

bargg <- data.frame(bart)

ggplot(bartry)+
  geom_bar()

