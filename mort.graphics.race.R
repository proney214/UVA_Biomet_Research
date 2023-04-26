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

RIC <- select(RIC,Count,Date,RACE)

RIC$RACE <- str_replace_all(RIC$RACE, c("AmerInd/AlaskNat"="Other",
                                          "Other"="Other"))

#Produce df for how many of each age group have died, look at station general mortalities
RICdf <- RIC %>%
  group_by(Date) %>%
  group_by(RACE) %>%
  summarise(total=sum(Count))

RICdfT <- RIC %>%
  group_by(Date)%>%
  summarise(Total=sum(Count))

RICmort <- RICdfT$totalRIC
plot(RICmort, type='l', main="RIC Total", col='blue')
lines(rollmean(rollmean(RICmort,40),40),col='yellow')
abline(v=seq(0,5844,by = 365))

##Filter by Race, Make each df the proper length

RICdf1 <- RIC %>%
  filter(RACE == 'White')%>%
  group_by(Date) %>%
  summarise(White=sum(Count))
#plot(RICdf1,type='l',main='75+')

RICdf1 <- pad(RICdf1,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


RICdf2 <- RIC %>%
  filter(RACE == 'Black') %>%
  group_by(Date) %>%
  summarise(Black=sum(Count))
#plot(RICdf2,type='l',main='65-74')

RICdf2 <- pad(RICdf2,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


RICdf3 <- RIC %>%
  filter(RACE == 'Other') %>%
  group_by(Date) %>%
  summarise(Other=sum(Count))
#plot(RICdf3,type='l',main='50-64')

RICdf3 <- pad(RICdf3,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


RICdf4 <- RIC %>%
  filter(RACE == 'Asian/PacificIslander') %>%
  group_by(Date) %>%
  summarise(Asian_PacificIslander=sum(Count))
#plot(RICdf5,type='l',main='30-39')

RICdf4 <- pad(RICdf4,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


RICdf6 <- RIC %>%
  filter(RACE == 'Unknown') %>%
  group_by(Date) %>%
  summarise(Unknown=sum(Count))
#plot(RICdf7,type='l',main='0-11')

RICdf6 <- pad(RICdf6,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)

### Compile data

RICdfMerged <- do.call("cbind", list(RICdfT,RICdf1,RICdf2,RICdf3,RICdf4,RICdf5,RICdf6))
                                     #,RICdf7,RICdf8,RICdf9,RICdf10,RICdf11,RICdf12,RICdf13,RICdf14,RICdf15
RICdfwide <- RICdfMerged[!duplicated(as.list(RICdfMerged))]
RICdfwide <- RICdfwide[,-3]

#Make new year column to then group by year rather than daily mortaility age buckets
Year <- format(as.Date(RICdfwide$Date, format="%Y-%m-%d"),"%Y")
RICdfwide$Year <- Year

#Put data into ggplot friendly form
dflong <- pivot_longer(data = RICdfwide,
                       cols = c(2:8),
                       names_to = "Race",
                       values_to = "Mort")

#Geom_smooth removes the noisiness of the data
ggplot(dflong, aes(Date, Mort, color=Race)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("RIC Mortality by Race")

#This produces a line plot
ggplot(dflong, aes(Date, Mort, color=Race)) +
  geom_line() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("RIC Mortality by Race")

