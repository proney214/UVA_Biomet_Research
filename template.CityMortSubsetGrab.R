##Plotting mortality for race

library(readxl)
library(dplyr)
library(stringr)
library(zoo)
library(padr)
library(ggplot2)
library(tidyr)

#Load in and prepare filter for dataset
BigMort_RACE_Full <- read_excel("~/Desktop/BigMort3.xlsx")
BigMortR <- BigMort_RACE_Full

BigMortR$Date = as.Date(paste(BigMortR$DOD_YR,BigMortR$DOD_MO,BigMortR$DOD_DY,sep="-"))
BigMortR$`Station ID` <- str_replace_all(BigMortR$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))
BigMortR$Count <- 1

#Single Station Filter command
RIC <- BigMortR %>%
  filter(BigMortR$`Station ID` == "RIC") 

RIC <- select(RIC,Count,Date,RACE,AGE_GROUPS)

RIC$RACE <- str_replace_all(RIC$RACE, c("Asian/PacificIslander"="Other",
                                        "AmerInd/AlaskNat"="Other",
                                        "Other"="Other"))

#Produce df for how many of each age group have died, look at station general mortalities
RICdfT <- RIC %>%
  group_by(Date)%>%
  summarise(Total=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

RICmort <- RICdfT$Total
plot(RICmort, type='l', main="RIC Total", col='blue')
lines(rollmean(rollmean(RICmort,40),40),col='yellow')
abline(v=seq(0,5844,by = 365))

##Filter by Race, Make each df the proper length

RICdf1 <- RIC %>%
  filter(RACE == 'White')%>%
  group_by(Date) %>%
  summarise(White=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))


RICdf2 <- RIC %>%
  filter(RACE == 'Black') %>%
  group_by(Date) %>%
  summarise(Black=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

RICdf3 <- RIC %>%
  filter(RACE == 'Other') %>%
  group_by(Date) %>%
  summarise(Other=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

RICdf4 <- RIC %>%
  filter(AGE_GROUPS == '75+') %>%
  group_by(Date) %>%
  summarise(RICmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

RICdfMerged <- do.call("cbind", list(RICdfT,RICdf1,RICdf2,RICdf3,RICdf4))
RICdfwide <- RICdfMerged[!duplicated(as.list(RICdfMerged))]
RICdfwide[is.na(RICdfwide)] <- 0

#Set your mort (Total is column 2, White in 3, Black in 4, Others in 5, Elderly in 6)
mort <- RICdfwide[,CHOOSEHERE]

