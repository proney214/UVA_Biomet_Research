#Calculating Standard Rate of Deaths and Estimated Mortality

library(readxl)
library(dplyr)
library(stringr)
library(zoo)
library(padr)
library(ggplot2)
library(tidyr)

#Load in and prepare filter for dataset
BigMort_Race <- read_excel("~/Desktop/BigMort_Race.xlsx")
BigMort <- BigMort_Race
BigMort$Date = as.Date(paste(BigMort$DOD_YR,BigMort$DOD_MO,BigMort$DOD_DY,sep="-"))
BigMort$`Station ID` <- str_replace_all(BigMort$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))

#Single Station Filter command
CHO <- BigMort %>%
  filter(BigMort$`Station ID` == "CHO")

CHO <- select(CHO,Count,Date,AGE_GROUPS, SEX)

#Produce df for how many of each age group have died, look at station general mortalities
CHOdf <- CHO %>%
  group_by(Date) %>%
  group_by(AGE_GROUPS) %>%
  summarise(total=sum(Count))

CHOdfT <- CHO %>%
  group_by(Date)%>%
  summarise(totalCHO=sum(Count))

CHOdfT <- pad(CHOdfT,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)

CHOmort <- CHOdfT$totalCHO
plot(CHOmort, type='l', main="CHO Total", col='blue')
lines(rollmean(rollmean(CHOmort,40),40),col='yellow')
abline(v=seq(0,5844,by = 365))

##Filter by Age Group, Make each df the proper length

CHOdfA <- CHO %>%
  filter(AGE_GROUPS == '75+')%>%
  group_by(Date) %>%
  summarise(CHOtotal75=sum(Count))
#plot(CHOdfA,type='l',main='75+')

CHOdf1 <- pad(CHOdfA,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


CHOdfB <- CHO %>%
  filter(AGE_GROUPS == '65-74') %>%
  group_by(Date) %>%
  summarise(CHOtotal65_74=sum(Count))
#plot(CHOdfB,type='l',main='65-74')

CHOdf2 <- pad(CHOdfB,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


CHOdfc <- CHO %>%
  filter(AGE_GROUPS == '50-64') %>%
  group_by(Date) %>%
  summarise(CHOtotal50_64=sum(Count))
#plot(CHOdfc,type='l',main='50-64')

CHOdf3 <- pad(CHOdfc,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


CHOdfd <- CHO %>%
  filter(AGE_GROUPS == '40-49') %>%
  group_by(Date) %>%
  summarise(CHOtotal40_49=sum(Count))
#plot(CHOdfd,type='l',main='40-49')

CHOdf4 <- pad(CHOdfd,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


CHOdfe <- CHO %>%
  filter(AGE_GROUPS == '30-39') %>%
  group_by(Date) %>%
  summarise(CHOtotal30_39=sum(Count))
#plot(CHOdfe,type='l',main='30-39')

CHOdf5 <- pad(CHOdfe,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


CHOdff <- CHO %>%
  filter(AGE_GROUPS == '18-29') %>%
  group_by(Date) %>%
  summarise(CHOtotal18_29=sum(Count))
#plot(CHOdff,type='l',main='18-29')

CHOdf6 <- pad(CHOdff,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)


CHOdfg <- CHO %>%
  filter(AGE_GROUPS == '44912') %>%
  group_by(Date) %>%
  summarise(CHOtotal12_17=sum(Count))
#plot(CHOdfg,type='l',main='12-17')

CHOdf7 <- pad(CHOdfg,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)

CHOdfh <- CHO %>%
  filter(AGE_GROUPS == '0-11') %>%
  group_by(Date) %>%
  summarise(CHOtotal0_11=sum(Count))
#plot(CHOdfg,type='l',main='0-11')

CHOdf8 <- pad(CHOdfh,
              interval = "day",
              start_val = as.POSIXct('2005-01-01'),
              end_val = as.POSIXct('2020-12-31')
)

### Compile data

CHOdfMerged <- do.call("cbind", list(CHOdfT, CHOdf1, CHOdf2, CHOdf3, CHOdf4, CHOdf5, CHOdf6, CHOdf7,CHOdf8))
CHOdfwide <- CHOdfMerged[!duplicated(as.list(CHOdfMerged))]
#CHOdfwide <- CHOdfwide[,-3]

#Make new year column to then group by year rather than daily mortality age buckets
Year <- format(as.Date(CHOdfwide$Date, format="%Y-%m-%d"),"%Y")
CHOdfwide$Year <- Year
CHOdfwide <- select(CHOdfwide,3:11)

dfmortfinalCHO <- CHOdfwide

#CHOdfmortfinal <- CHOdfwide %>%
# group_by(Year) %>%
# summarise(across(everything(), .f = list(sum=sum), na.rm = TRUE))

#Annual Population Gathering

#Gathering process in another script 

#Calculate the Death Rate

deathrateCHO2005 <- data.frame(mapply(`/`,dfmortfinalCHO[1:365, 1:8],dfcpopfinalCHO[1,1:8]))
deathrateCHO2005$Date <- CHOdf1$Date[1:365]
deathrateCHO2005 <- deathrateCHO2005 %>%
  select(Date, everything())


deathrateCHO2006 <- data.frame(mapply(`/`,dfmortfinalCHO[366:730, 1:8],dfcpopfinalCHO [2,1:8]))
deathrateCHO2006$Date <- CHOdf1$Date[366:730]
deathrateCHO2006 <- deathrateCHO2006 %>%
  select(Date, everything())


deathrateCHO2007 <- data.frame(mapply(`/`,dfmortfinalCHO[731:1095, 1:8],dfcpopfinalCHO [3,1:8]))
deathrateCHO2007$Date <- CHOdf1$Date[731:1095]
deathrateCHO2007 <- deathrateCHO2007 %>%
  select(Date, everything())


deathrateCHO2008 <- data.frame(mapply(`/`,dfmortfinalCHO[1096:1461, 1:8],dfcpopfinalCHO [4,1:8]))
deathrateCHO2008$Date <- CHOdf1$Date[1096:1461]
deathrateCHO2008 <- deathrateCHO2008 %>%
  select(Date, everything())


deathrateCHO2009 <- data.frame(mapply(`/`,dfmortfinalCHO[1462:1826, 1:8],dfcpopfinalCHO [5,1:8]))
deathrateCHO2009$Date <- CHOdf1$Date[1462:1826]
deathrateCHO2009 <- deathrateCHO2009 %>%
  select(Date, everything())


deathrateCHO2010 <- data.frame(mapply(`/`,dfmortfinalCHO[1827:2191, 1:8],dfcpopfinalCHO [6,1:8]))
deathrateCHO2010$Date <- CHOdf1$Date[1827:2191]
deathrateCHO2010 <- deathrateCHO2010 %>%
  select(Date, everything())

deathrateCHO2011 <- data.frame(mapply(`/`,dfmortfinalCHO[2192:2556, 1:8],dfcpopfinalCHO [7,1:8]))
deathrateCHO2011$Date <- CHOdf1$Date[2192:2556]
deathrateCHO2011 <- deathrateCHO2011 %>%
  select(Date, everything())

deathrateCHO2012 <- data.frame(mapply(`/`,dfmortfinalCHO[2557:2922, 1:8],dfcpopfinalCHO [8,1:8]))
deathrateCHO2012$Date <- CHOdf1$Date[2557:2922]
deathrateCHO2012 <- deathrateCHO2012 %>%
  select(Date, everything())

deathrateCHO2013 <- data.frame(mapply(`/`,dfmortfinalCHO[2923:3287, 1:8],dfcpopfinalCHO [9,1:8]))
deathrateCHO2013$Date <- CHOdf1$Date[2923:3287]
deathrateCHO2013 <- deathrateCHO2013 %>%
  select(Date, everything())

deathrateCHO2014 <- data.frame(mapply(`/`,dfmortfinalCHO[3288:3652, 1:8],dfcpopfinalCHO [10,1:8]))
deathrateCHO2014$Date <- CHOdf1$Date[3288:3652]
deathrateCHO2014 <- deathrateCHO2014 %>%
  select(Date, everything())

deathrateCHO2015 <- data.frame(mapply(`/`,dfmortfinalCHO[3653:4017, 1:8],dfcpopfinalCHO [11,1:8]))
deathrateCHO2015$Date <- CHOdf1$Date[3653:4017]
deathrateCHO2015 <- deathrateCHO2015 %>%
  select(Date, everything())

deathrateCHO2016 <- data.frame(mapply(`/`,dfmortfinalCHO[4018:4383, 1:8],dfcpopfinalCHO [12,1:8]))
deathrateCHO2016$Date <- CHOdf1$Date[4018:4383]
deathrateCHO2016 <- deathrateCHO2016 %>%
  select(Date, everything())

deathrateCHO2017 <- data.frame(mapply(`/`,dfmortfinalCHO[4384:4748, 1:8],dfcpopfinalCHO [13,1:8]))
deathrateCHO2017$Date <- CHOdf1$Date[4384:4748]
deathrateCHO2017 <- deathrateCHO2017 %>%
  select(Date, everything())

deathrateCHO2018 <- data.frame(mapply(`/`,dfmortfinalCHO[4749:5113, 1:8],dfcpopfinalCHO [14,1:8]))
deathrateCHO2018$Date <- CHOdf1$Date[4749:5113]
deathrateCHO2018 <- deathrateCHO2018 %>%
  select(Date, everything())

deathrateCHO2019 <- data.frame(mapply(`/`,dfmortfinalCHO[5114:5478, 1:8],dfcpopfinalCHO [15,1:8]))
deathrateCHO2019$Date <- CHOdf1$Date[5114:5478]
deathrateCHO2019 <- deathrateCHO2019 %>%
  select(Date, everything())

deathrateCHO2020 <- data.frame(mapply(`/`,dfmortfinalCHO[5479:5844, 1:8],dfcpopfinalCHO [16,1:8]))
deathrateCHO2020$Date <- CHOdf1$Date[5479:5844]
deathrateCHO2020 <- deathrateCHO2020 %>%
  select(Date, everything())

#Calculate newmort dataframes
deathratecompleteCHO <- bind_rows(deathrateCHO2005,deathrateCHO2006,deathrateCHO2007,deathrateCHO2008,deathrateCHO2009,deathrateCHO2010,deathrateCHO2011,deathrateCHO2012,deathrateCHO2013,deathrateCHO2014,deathrateCHO2015,deathrateCHO2016,deathrateCHO2017,deathrateCHO2018,deathrateCHO2019,deathrateCHO2020)

#Get estimated mortality by multiplying Death Rate by VA 2020 pop.
#prepare dfpop2020VA data
V_A2020stdpopraw <- dfpop2020[12,]
V_A2020stdpopcalc <- (1000000 * V_A2020stdpopraw[,3:10])/V_A2020stdpopraw[,2]

#View(rowSums(V_A2020stdpopcalc))
V_A2020stdpoprev <- rev(V_A2020stdpopcalc)
V_A2020stdpopfinal <- data.frame(c(V_A2020stdpopraw [,1:2],V_A2020stdpoprev))

#Calculate Estimated Mortality (Change columns for multiplication)
estmortCHOdf <- data.frame(mapply(`*`,deathratecompleteCHO[,2:9],V_A2020stdpopfinal [, 3:10]))
estmortCHOdf$Year <- dfmortfinalCHO$Year
estmortCHOdf <- estmortCHOdf %>%
  select(Year, everything())

#Divide estimated mortaility by orginial mort to get new deaths
#prepare dfwideCHO for the calculations
#dfwideCHOnew <- CHOdfwide %>%
# select(Year, everything())


#Calculate newmort dataframes
newmortcompleteCHO <- bind_rows(estmortCHOdf)
newmortrowsumsCHO <- data.frame(rowSums(newmortcompleteCHO[,2:9],na.rm = TRUE))

#Creating master file
#newmortsumsmaster <- do.call("cbind", list(newmortrowsumsCHO,newmortrowsumsEMV,newmortrowsumsEZF,newmortrowsumsIAD,newmortrowsumsLYH,newmortrowsumsOKV,newmortrowsumsORF,newmortrowsumsPHF,newmortrowsumsRIC,newmortrowsumsROA,newmortrowsumsSHD,newmortrowsumsVJI))

#Export to csv
#write.csv(newmortsumsmaster,"~/Desktop/NotThis_Research/NewMortSumsMaster.csv",row.names=FALSE)