##Plotting mortality for Ethnicity

library(readxl)
library(dplyr)
library(stringr)
library(zoo)
library(padr)
library(ggplot2)
library(tidyr)

#Load in and prepare filter for dataset
BigMort_ETHNICITY_Full <- read_excel("~/Desktop/BigMort3.xlsx")
BigMortR <- BigMort_ETHNICITY_Full
BigMortR$Count <- 1

BigMortR$Date = as.Date(paste(BigMortR$DOD_YR,BigMortR$DOD_MO,BigMortR$DOD_DY,sep="-"))
BigMortR$`Station ID` <- str_replace_all(BigMortR$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))

#Single Station Filter command
RIC <- BigMortR %>%
  filter(BigMortR$`Station ID` == "RIC") 

RIC <- select(RIC,Count,Date,ETHNICITY)

#Produce df for how many of each age group have died, look at station general mortalities
RICdf <- RIC %>%
  group_by(Date) %>%
  group_by(ETHNICITY) %>%
  summarise(total=sum(Count))

RICdfT <- RIC %>%
  group_by(Date)%>%
  summarise(Total=sum(Count))

RICdfT <- pad(RICdfT,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)

#RICmort <- RICdfT$totalRIC
#plot(RICmort, type='l', main="RIC Total", col='blue')
#lines(rollmean(rollmean(RICmort,40),40),col='yellow')
#abline(v=seq(0,5844,by = 365))

##Filter by ETHNICITY, Make each df the proper length

RICdf1 <- RIC %>%
  filter(ETHNICITY == 'Hispanic')%>%
  group_by(Date) %>%
  summarise(Hispanic=sum(Count))
#plot(RICdf1,type='l',main='75+')

RICdf1 <- pad(RICdf1,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


RICdf2 <- RIC %>%
  filter(ETHNICITY == 'NonHispanic') %>%
  group_by(Date) %>%
  summarise(Non_Hispanic=sum(Count))
#plot(RICdf2,type='l',main='65-74')

RICdf2 <- pad(RICdf2,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


RICdf3 <- RIC %>%
  filter(ETHNICITY == 'Other/Unknown') %>%
  group_by(Date) %>%
  summarise(Other=sum(Count))
#plot(RICdf3,type='l',main='50-64')

RICdf3 <- pad(RICdf3,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


### Compile data

RICdfMerged <- do.call("cbind", list(RICdfT,RICdf1,RICdf2,RICdf3))
RICdfwide <- RICdfMerged[!duplicated(as.list(RICdfMerged))]
#RICdfwide <- RICdfwide[,-3]

#Make new year column to then group by year rather than daily mortaility age buckets
Year <- format(as.Date(RICdfwide$Date, format="%Y-%m-%d"),"%Y")
RICdfwide$Year <- Year

#Put data into ggplot friendly form
dflong <- pivot_longer(data = RICdfwide,
                       cols = c(2:5),
                       names_to = "Ethnicity",
                       values_to = "Mort")

#Geom_smooth removes the noisiness of the data
ggplot(dflong, aes(Date, Mort, color=Ethnicity)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("RIC Mortality by Ethnicity")
