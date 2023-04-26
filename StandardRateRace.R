#Calculating Standard Rate of Deaths and Estimated Mortality

library(readxl)
library(dplyr)
library(stringr)
library(zoo)
library(padr)
library(ggplot2)
library(tidyr)

#Load in and prepare filter for dataset
BigMort_Race <- read_excel("~/Desktop/BigMort3.xlsx")
BigMort <- BigMort_Race
BigMort$Date = as.Date(paste(BigMort$DOD_YR,BigMort$DOD_MO,BigMort$DOD_DY,sep="-"))
BigMort$`Station ID` <- str_replace_all(BigMort$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))
BigMort$Count <- 1

#Get your population subsets
TestVJI <- Test2 %>%
  filter(grepl('VJI',County))
dfVJIpopalmost <- pivot_wider(TestVJI, id_cols = Year, names_from = Race,values_from = x)
dfcpopfinalVJI <- dfVJIpopalmost[,c(3,5,2,4,1)]
#TestVJIWide2 <- pivot_wider(TestVJI, id_cols = Race, names_from = Year,values_from = x)


#Single Station Filter command
VJI <- BigMort %>%
  filter(BigMort$`Station ID` == "VJI")

VJI <- select(VJI,Count,Date,RACE)

#Produce df for how many of each age group have died, look at station general mortalities
VJIdf <- VJI %>%
  group_by(Date) %>%
  group_by(RACE) %>%
  summarise(total=sum(Count))

VJIdfT <- VJI %>%
  group_by(Date)%>%
  summarise(totalVJI=sum(Count)) %>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))


VJImort <- VJIdfT$totalVJI
plot(VJImort, type='l', main="VJI Total", col='blue')
lines(rollmean(rollmean(VJImort,40),40),col='yellow')
abline(v=seq(0,5844,by = 365))

##Filter by Age Group, Make each df the proper length

VJIdf1 <- VJI %>%
  filter(RACE == 'Black')%>%
  group_by(Date) %>%
  summarise(VJIBlack=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))


VJIdf2 <- VJI %>%
  filter(RACE == 'White') %>%
  group_by(Date) %>%
  summarise(VJIWhite=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))


VJIdf3 <- VJI %>%
  filter(RACE == 'Asian/PacificIslander') %>%
  group_by(Date) %>%
  summarise(VJIAsian_PI=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))


VJIdf4 <- VJI %>%
  filter(RACE == 'AmerInd/AlaskNat') %>%
  group_by(Date) %>%
  summarise(VJIIndigenous=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

### Compile data

VJIdfMerged <- do.call("cbind", list(VJIdfT, VJIdf1, VJIdf2, VJIdf3, VJIdf4))
VJIdfwide <- VJIdfMerged[!duplicated(as.list(VJIdfMerged))]
#VJIdfwide <- VJIdfwide[,-3]

#Make new year column to then group by year rather than daily mortality age buckets
Year <- format(as.Date(VJIdfwide$Date, format="%Y-%m-%d"),"%Y")
VJIdfwide$Year <- Year
VJIdfwide <- select(VJIdfwide,3:7)

dfmortfinalVJI <- VJIdfwide

#Annual Population Gathering

#Gathering process in another script 

#Calculate the Death Rate

deathrateVJI2005 <- data.frame(mapply(`/`,dfmortfinalVJI[1:365, 1:4],dfcpopfinalVJI[1,1:4]))
deathrateVJI2005$Date <- VJIdf1$Date[1:365]
deathrateVJI2005 <- deathrateVJI2005 %>%
  select(Date, everything())


deathrateVJI2006 <- data.frame(mapply(`/`,dfmortfinalVJI[366:730, 1:4],dfcpopfinalVJI [2,1:4]))
deathrateVJI2006$Date <- VJIdf1$Date[366:730]
deathrateVJI2006 <- deathrateVJI2006 %>%
  select(Date, everything())


deathrateVJI2007 <- data.frame(mapply(`/`,dfmortfinalVJI[731:1095, 1:4],dfcpopfinalVJI [3,1:4]))
deathrateVJI2007$Date <- VJIdf1$Date[731:1095]
deathrateVJI2007 <- deathrateVJI2007 %>%
  select(Date, everything())


deathrateVJI2008 <- data.frame(mapply(`/`,dfmortfinalVJI[1096:1461, 1:4],dfcpopfinalVJI [4,1:4]))
deathrateVJI2008$Date <- VJIdf1$Date[1096:1461]
deathrateVJI2008 <- deathrateVJI2008 %>%
  select(Date, everything())


deathrateVJI2009 <- data.frame(mapply(`/`,dfmortfinalVJI[1462:1826, 1:4],dfcpopfinalVJI [5,1:4]))
deathrateVJI2009$Date <- VJIdf1$Date[1462:1826]
deathrateVJI2009 <- deathrateVJI2009 %>%
  select(Date, everything())


deathrateVJI2010 <- data.frame(mapply(`/`,dfmortfinalVJI[1827:2191, 1:4],dfcpopfinalVJI [6,1:4]))
deathrateVJI2010$Date <- VJIdf1$Date[1827:2191]
deathrateVJI2010 <- deathrateVJI2010 %>%
  select(Date, everything())

deathrateVJI2011 <- data.frame(mapply(`/`,dfmortfinalVJI[2192:2556, 1:4],dfcpopfinalVJI [7,1:4]))
deathrateVJI2011$Date <- VJIdf1$Date[2192:2556]
deathrateVJI2011 <- deathrateVJI2011 %>%
  select(Date, everything())

deathrateVJI2012 <- data.frame(mapply(`/`,dfmortfinalVJI[2557:2922, 1:4],dfcpopfinalVJI [8,1:4]))
deathrateVJI2012$Date <- VJIdf1$Date[2557:2922]
deathrateVJI2012 <- deathrateVJI2012 %>%
  select(Date, everything())

deathrateVJI2013 <- data.frame(mapply(`/`,dfmortfinalVJI[2923:3287, 1:4],dfcpopfinalVJI [9,1:4]))
deathrateVJI2013$Date <- VJIdf1$Date[2923:3287]
deathrateVJI2013 <- deathrateVJI2013 %>%
  select(Date, everything())

deathrateVJI2014 <- data.frame(mapply(`/`,dfmortfinalVJI[3288:3652, 1:4],dfcpopfinalVJI [10,1:4]))
deathrateVJI2014$Date <- VJIdf1$Date[3288:3652]
deathrateVJI2014 <- deathrateVJI2014 %>%
  select(Date, everything())

deathrateVJI2015 <- data.frame(mapply(`/`,dfmortfinalVJI[3653:4017, 1:4],dfcpopfinalVJI [11,1:4]))
deathrateVJI2015$Date <- VJIdf1$Date[3653:4017]
deathrateVJI2015 <- deathrateVJI2015 %>%
  select(Date, everything())

deathrateVJI2016 <- data.frame(mapply(`/`,dfmortfinalVJI[4018:4383, 1:4],dfcpopfinalVJI [12,1:4]))
deathrateVJI2016$Date <- VJIdf1$Date[4018:4383]
deathrateVJI2016 <- deathrateVJI2016 %>%
  select(Date, everything())

deathrateVJI2017 <- data.frame(mapply(`/`,dfmortfinalVJI[4384:4748, 1:4],dfcpopfinalVJI [13,1:4]))
deathrateVJI2017$Date <- VJIdf1$Date[4384:4748]
deathrateVJI2017 <- deathrateVJI2017 %>%
  select(Date, everything())

deathrateVJI2018 <- data.frame(mapply(`/`,dfmortfinalVJI[4749:5113, 1:4],dfcpopfinalVJI [14,1:4]))
deathrateVJI2018$Date <- VJIdf1$Date[4749:5113]
deathrateVJI2018 <- deathrateVJI2018 %>%
  select(Date, everything())

deathrateVJI2019 <- data.frame(mapply(`/`,dfmortfinalVJI[5114:5478, 1:4],dfcpopfinalVJI [15,1:4]))
deathrateVJI2019$Date <- VJIdf1$Date[5114:5478]
deathrateVJI2019 <- deathrateVJI2019 %>%
  select(Date, everything())

deathrateVJI2020 <- data.frame(mapply(`/`,dfmortfinalVJI[5479:5844, 1:4],dfcpopfinalVJI [16,1:4]))
deathrateVJI2020$Date <- VJIdf1$Date[5479:5844]
deathrateVJI2020 <- deathrateVJI2020 %>%
  select(Date, everything())

#Calculate newmort dataframes
deathratecompleteVJI <- bind_rows(deathrateVJI2005,deathrateVJI2006,deathrateVJI2007,deathrateVJI2008,deathrateVJI2009,deathrateVJI2010,deathrateVJI2011,deathrateVJI2012,deathrateVJI2013,deathrateVJI2014,deathrateVJI2015,deathrateVJI2016,deathrateVJI2017,deathrateVJI2018,deathrateVJI2019,deathrateVJI2020)

#Get estimated mortality by multiplying Death Rate by VA 2020 pop.
#prepare dfpop2020VA data
V_A2020stdpopfinalrace <- (1000000 * dfstd[,2:5])/dfstd[,1]

#Calculate Estimated Mortality (Change columns for multiplication)
estmortVJIdf <- data.frame(mapply(`*`,deathratecompleteVJI[,2:5],V_A2020stdpopfinalrace[,1:4]))
estmortVJIdf$Year <- dfmortfinalVJI$Year
estmortVJIdf <- estmortVJIdf %>%
  select(Year, everything())

#Divide estimated mortaility by orginial mort to get new deaths



#Calculate newmort dataframes
newmortcompleteVJI <- bind_rows(estmortVJIdf)
newmortrowsumsVJI <- rowSums(newmortcompleteVJI[,2:5],na.rm = TRUE)
finalnewmortcompleteVJI <- cbind(newmortcompleteVJI,newmortrowsumsVJI)

Index <- 1:5844

plot(Index,newmortrowsumsVJI,type='l', main="VJI Est Mort Total", col='blue',ylim=c(0,60))
lines(rollmean(rollmean(newmortrowsumsVJI,40),40),col='yellow')
lines(rollmean(rollmean(VJImort,40),40),col='green')
abline(v=seq(0,5844,by = 365))

plot(VJImort, type='l', main="VJI Total", col='blue',ylim=c(0,60))
lines(rollmean(rollmean(newmortrowsumsVJI,40),40),col='yellow')
lines(rollmean(rollmean(VJImort,40),40),col='green')
abline(v=seq(0,5844,by = 365))

#Creating master file
finalnewmortcompletemaster <- do.call("cbind", list(finalnewmortcompleteCHO,finalnewmortcompleteEMV,finalnewmortcompleteEZF,finalnewmortcompleteIAD,finalnewmortcompleteLYH,finalnewmortcompleteOKV,finalnewmortcompleteORF,finalnewmortcompletePHF,finalnewmortcompleteEZF,finalnewmortcompleteROA,finalnewmortcompleteSHD,finalnewmortcompleteVJI))
finalnewmortcompletemaster <- finalnewmortcompletemaster[!duplicated(as.list(finalnewmortcompletemaster))]
finalnewmortcompletemaster$Year <- VJIdf4$Date

#Export to csv
write.csv(finalnewmortcompletemaster,"~/Desktop/NewRaceMortSumsMaster.csv",row.names=FALSE)
