##Plotting mortality for each age group and gender

library(readxl)
library(dplyr)
library(stringr)
library(zoo)
library(padr)
library(ggplot2)
library(tidyr)

#Load in and prepare filter for dataset
BigMort_RACE_Full <- read_excel("~/Desktop/BigMort_RACE.xlsx")
BigMort <- BigMort_RACE_Full
#BigMort$Date = as.Date(paste(BigMortR$DOD_YR,BigMortR$DOD_MO,BigMortR$DOD_DY,sep="-"))
#BigMort$`Station ID` <- str_replace_all(BigMortR$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))

#Single Station Filter command
RIC <- BigMort %>%
  filter(BigMort$`Station ID` == "RIC") 

#Multi-Station Filter command
#RIC <- BigMort %>%
# filter(BigMort$`Station ID` == "RIC" | BigMort$`Station ID` == "MFV") 

RIC <- select(RIC,Count,Date,AGE_GROUPS, SEX)

#Produce df for how many of each age group have died, look at station general mortalities
df <- RIC %>%
  group_by(Date) %>%
  group_by(AGE_GROUPS) %>%
  summarise(total=sum(Count))

dfT <- RIC %>%
  group_by(Date)%>%
  summarise(totalRIC=sum(Count))

mort <- dfT$totalRIC
plot(mort, type='l', main="RIC Total", col='blue')
lines(rollmean(rollmean(mort,40),40),col='yellow')
abline(v=seq(0,5844,by = 365))

##Filter by Age Group, Make each df the proper length

df1 <- RIC %>%
  filter(AGE_GROUPS == '75+') %>%
  group_by(Date) %>%
  summarise(total75=sum(Count))
#plot(dfA,type='l',main='75+')

df1 <- pad(df1,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)


df2 <- RIC %>%
  filter(AGE_GROUPS == '65-74') %>%
  group_by(Date) %>%
  summarise(total65_74=sum(Count))
#plot(dfB,type='l',main='65-74')

df2 <- pad(df2,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)


df3 <- RIC %>%
  filter(AGE_GROUPS == '50-64')%>%
  group_by(Date) %>%
  summarise(total50_64=sum(Count))
#plot(dfC,type='l',main='50-64')

df3 <- pad(df3,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)


df4 <- RIC %>%
  filter(AGE_GROUPS == '40-49') %>%
  group_by(Date) %>%
  summarise(total40_49=sum(Count))
#plot(dfD,type='l',main='40-49')

df4 <- pad(df4,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)


df5 <- RIC %>%
  filter(AGE_GROUPS == '30-39') %>%
  group_by(Date) %>%
  summarise(total30_39=sum(Count))
#plot(dfE,type='l',main='30-39')

df5 <- pad(df5,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)


df6 <- RIC %>%
  filter(AGE_GROUPS == '18-29') %>%
  group_by(Date) %>%
  summarise(total18_29=sum(Count))
#plot(dfF,type='l',main='18-29')

df6 <- pad(df6,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)


df7 <- RIC %>%
  filter(AGE_GROUPS == '44912') %>%
  group_by(Date) %>%
  summarise(total12_17=sum(Count))
#plot(dfG,type='l',main='12-17')

df7 <- pad(df7,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)

df8 <- RIC %>%
  filter(AGE_GROUPS == '0-11') %>%
  group_by(Date) %>%
  summarise(total0_11=sum(Count))
#plot(dfG,type='l',main='0-11')

df8 <- pad(df8,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)

### Compile data

dfMerged <- do.call("cbind", list(df1, df2, df3, df4, df5, df6, df7,df8))
dfwide <- dfMerged[!duplicated(as.list(dfMerged))]

#Create variables for each age_group for gam models
#for(i in 1:ncol(dfwide)){
#  assign(names(dfwide)[i], dfwide[[i]])
#}

#Put data into ggplot friendly form
dflong <- pivot_longer(data = dfwide,
                       cols = c(2:9),
                       names_to = "Age_Group",
                       values_to = "Mort")

#This produces a line plot
ggplot(dflong, aes(Date, Mort, color=Age_Group)) +
  geom_line() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("RIC Mortality by Age Group")

#Geom_smooth removes the noisiness of the data
ggplot(dflong, aes(Date, Mort, color=Age_Group)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("RIC Mortality by Age Group")

#This produces a bar plot
ggplot(dflong, aes(Date, Mort, color=Age_Group)) +
  geom_bar(stat="identity") +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("RIC Mortality by Age Group")


### Analysis by Gender, repeats process for age group analysis

#Produce df for how many of each age group have died, look at station general mortalities
dfSex <- RIC %>%
  group_by(Date) %>%
  group_by(SEX) %>%
  summarise(total=sum(Count))

#Filter by Gender 
M <- RIC %>%
  filter(SEX == 'M')
dfM <- M %>%
  group_by(Date) %>%
  summarise(totalMale=sum(Count))

testM <- dfM$totalMale

plot(dfM,type='l',main='RIC Male Deaths')
plot(testM,type='l',col='blue',main='RIC Male Deaths')
lines(rollmean(rollmean(testM,40),40),col='red')
abline(v=seq(0,5844,by = 365))

df11 <- pad(dfM,
            interval = "day",
            start_val = as.POSIXct('2005-01-01'),
            end_val = as.POSIXct('2020-12-31')
)


Fe <- RIC %>%
  filter(SEX == 'F')
dfFe <- Fe %>%
  group_by(Date) %>%
  summarise(totalFemale=sum(Count))

testF <- dfFe$totalFemale

plot(dfFe,type='l',main='RIC Female Deaths')
plot(testF,type='l',col='blue',main='RIC Female Deaths')
lines(rollmean(rollmean(testF,40),40),col='red')
abline(v=seq(0,5844,by = 365))

df12 <- pad(dfFe,
            interval = "day",
            start_val = as.POSIXct('2005-01-01'),
            end_val = as.POSIXct('2020-12-31')
)


### Compile data

dfMergedG <- do.call("cbind", list(df11, df12))
dfwideG <- dfMergedG[!duplicated(as.list(dfMergedG))]

#for(i in 1:ncol(dfwideG)){
#  assign(names(dfwideG)[i], dfwideG[[i]])
#}

dflongG <- pivot_longer(data = dfwideG,
                        cols = c(2:3),
                        names_to = "Gender",
                        values_to = "Mort")

#This produces a line plot
ggplot(dflongG, aes(Date, Mort, color=Gender)) +
  geom_line() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("RIC Mortality by Gender")

ggplot(dflongG, aes(Date, Mort, color=Gender)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("RIC Mortality by Gender")

#This produces a bar plot
ggplot(dflongG, aes(Date, Mort, color=Gender)) +
  geom_bar(stat="identity") +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("RIC Mortality by Gender")


###Best Plots So Far
ggplot(dflongG, aes(Date, Mort, color=Gender)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("RIC Mortality by Gender")

ggplot(dflong, aes(Date, Mort, color=Age_Group)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("RIC Mortality by Age Group")

