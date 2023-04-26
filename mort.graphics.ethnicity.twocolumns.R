##Plotting mortality for Ethnicity (Without Others and Unknown)

library(readxl)
library(dplyr)
library(stringr)
library(zoo)
library(padr)
library(ggplot2)
library(tidyr)

#Load in and prepare filter for dataset
#BigMort_RACE_Full <- read_excel("~/Desktop/BigMort_RACE.xlsx")
#BigMortR <- BigMort_RACE_Full

#BigMortR$Date = as.Date(paste(BigMortR$DOD_YR,BigMortR$DOD_MO,BigMortR$DOD_DY,sep="-"))
#BigMortR$`Station ID` <- str_replace_all(BigMortR$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))

#Single Station Filter command
VJI <- BigMortR %>%
  filter(BigMortR$`Station ID` == "VJI") 

VJI <- select(VJI,Count,Date,RACE)

VJI$RACE <- str_replace_all(VJI$RACE, c("Black"= "Non-White",
                                        "AmerInd/AlaskNat"="Non-White",
                                        "OtherAsian/PI"="Non-White",
                                        "AsianIndian"="Non-White",
                                        "Chinese"="Non-White",
                                        "Korean"="Non-White",
                                        "Vietnamese"="Non-White",
                                        "Japanese"="Non-White",
                                        "Filipino"="Non-White",
                                        "Hawaiian"="Non-White",
                                        "Samoan"="Non-White",
                                        "Guam/Cham"="Non-White"))

#Produce df for how many of each age group have died, look at station general mortalities
VJIdf <- VJI %>%
  group_by(Date) %>%
  group_by(RACE) %>%
  summarise(total=sum(Count))

VJIdfT <- VJI %>%
  group_by(Date)%>%
  summarise(Total=sum(Count))

VJIdfT <- pad(VJIdfT,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)

#VJImort <- VJIdfT$totalVJI
#plot(VJImort, type='l', main="VJI Total", col='blue')
#lines(rollmean(rollmean(VJImort,40),40),col='yellow')
#abline(v=seq(0,5844,by = 365))

##Filter by Race, Make each df the proper length

VJIdf1 <- VJI %>%
  filter(RACE == 'White')%>%
  group_by(Date) %>%
  summarise(White=sum(Count))
#plot(VJIdf1,type='l',main='75+')

VJIdf1 <- pad(VJIdf1,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


VJIdf2 <- VJI %>%
  filter(RACE == 'Non-White') %>%
  group_by(Date) %>%
  summarise(Non_White=sum(Count))
#plot(VJIdf2,type='l',main='65-74')

VJIdf2 <- pad(VJIdf2,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


VJIdf3 <- VJI %>%
  filter(RACE == 'Other') %>%
  group_by(Date) %>%
  summarise(Other=sum(Count))
#plot(VJIdf3,type='l',main='50-64')

VJIdf3 <- pad(VJIdf3,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


VJIdf4 <- VJI %>%
  filter(RACE == 'Unknown') %>%
  group_by(Date) %>%
  summarise(Unknown=sum(Count))
#plot(VJIdf4,type='l',main='40-49')

VJIdf4 <- pad(VJIdf4,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


### Compile data

VJIdfMerged <- do.call("cbind", list(VJIdfT,VJIdf1,VJIdf2))
VJIdfwide <- VJIdfMerged[!duplicated(as.list(VJIdfMerged))]
#VJIdfwide <- VJIdfwide[,-3]

#Make new year column to then group by year rather than daily mortaility age buckets
Year <- format(as.Date(VJIdfwide$Date, format="%Y-%m-%d"),"%Y")
VJIdfwide$Year <- Year

#Put data into ggplot friendly form
dflong <- pivot_longer(data = VJIdfwide,
                       cols = c(2:4),
                       names_to = "Ethnicity",
                       values_to = "Mort")

#Geom_smooth removes the noisiness of the data
ggplot(dflong, aes(Date, Mort, color=Ethnicity)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("VJI Mortality by Ethnicity")
