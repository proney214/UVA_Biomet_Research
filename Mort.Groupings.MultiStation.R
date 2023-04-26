##Plotting mortality for each age group and gender

library(readxl)
library(dplyr)
library(stringr)
library(zoo)
library(padr)
library(ggplot2)
library(tidyr)

#Load in and prepare filter for dataset
BigMort <- read_excel("~/Desktop/BigMort.xlsx", 
                      sheet = "Sheet1")
BigMort$Date = as.Date(paste(BigMort$DOD_YR,BigMort$DOD_MO,BigMort$DOD_DY,sep="-"))

PHF <- BigMort %>%
  filter(BigMort$`Station ID` == "PHF" | BigMort$`Station ID` == "MFV") 

#All of the dates have the same character string, get around the issueby creating date above with paste
#PHF <- PHF %>%
# filter(str_detect(DateOfDeath, '^[0-9]{8}$',negate = TRUE))

PHF <- select(PHF,Count,Date,AGE_GROUPS, SEX)

#Produce df for how many of each age group have died, look at station general mortalities
df <- PHF %>%
  group_by(Date) %>%
  group_by(AGE_GROUPS) %>%
  summarise(total=sum(Count))

dfT <- PHF %>%
  group_by(Date)%>%
  summarise(totalPHF=sum(Count))

mort <- dfT$totalPHF
plot(mort, type='l', main="PHF Total", col='blue')
lines(rollmean(rollmean(mort,40),40),col='yellow')
abline(v=seq(0,5844,by = 365))

##Filter by Age Group, Make each df the proper length

A <- PHF %>%
  filter(AGE_GROUPS == '75+')
dfA <- A %>%
  group_by(Date) %>%
  summarise(total75=sum(Count))
plot(dfA,type='l',main='75+')

df1 <- pad(dfA,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)


B <- PHF %>%
  filter(AGE_GROUPS == '65-74')
dfB <- B %>%
  group_by(Date) %>%
  summarise(total65_74=sum(Count))
plot(dfB,type='l',main='65-74')

df2 <- pad(dfB,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)


C <- PHF %>%
  filter(AGE_GROUPS == '50-64')
dfC <- C %>%
  group_by(Date) %>%
  summarise(total50_64=sum(Count))
plot(dfC,type='l',main='50-64')

df3 <- pad(dfC,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)


D <- PHF %>%
  filter(AGE_GROUPS == '40-49')
dfD <- D %>%
  group_by(Date) %>%
  summarise(total40_49=sum(Count))
plot(dfD,type='l',main='40-49')

df4 <- pad(dfD,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)


E <- PHF %>%
  filter(AGE_GROUPS == '30-39')
dfE <- E %>%
  group_by(Date) %>%
  summarise(total30_39=sum(Count))
plot(dfE,type='l',main='30-39')

df5 <- pad(dfE,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)


F <- PHF %>%
  filter(AGE_GROUPS == '18-29')
dfF <- F %>%
  group_by(Date) %>%
  summarise(total18_29=sum(Count))
plot(dfF,type='l',main='18-29')

df6 <- pad(dfF,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)


G <- PHF %>%
  filter(AGE_GROUPS == '0-11')
dfG <- G %>%
  group_by(Date) %>%
  summarise(total0_11=sum(Count))
plot(dfG,type='l',main='0-11')

df7 <- pad(dfG,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)

### Compile data

dfMerged <- do.call("cbind", list(df1, df2, df3, df4, df5, df6, df7))
dfwide <- dfMerged[!duplicated(as.list(dfMerged))]

#Create variables for each age_group for gam models
for(i in 1:ncol(dfwide)){
 assign(names(dfwide)[i], dfwide[[i]])
}

#Put data into ggplot friendly form
dflong <- pivot_longer(data = dfwide,
                       cols = c(2:8),
                       names_to = "Age_Group",
                       values_to = "Mort")

#This produces a line plot
ggplot(dflong, aes(Date, Mort, color=Age_Group)) +
  geom_line() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("PHF Mortality by Age Group")

#Geom_smooth removes the noisiness of the data
ggplot(dflong, aes(Date, Mort, color=Age_Group)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("PHF Mortality by Age Group")

#This produces a bar plot
ggplot(dflong, aes(Date, Mort, color=Age_Group)) +
  geom_bar(stat="identity") +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("PHF Mortality by Age Group")


### Analysis by Gender, repeats process for age group analysis

#Produce df for how many of each age group have died, look at station general mortalities
dfSex <- PHF %>%
  group_by(Date) %>%
  group_by(SEX) %>%
  summarise(total=sum(Count))

#Filter by Gender 
M <- PHF %>%
  filter(SEX == 'M')
dfM <- M %>%
  group_by(Date) %>%
  summarise(totalMale=sum(Count))

testM <- dfM$totalMale

plot(dfM,type='l',main='PHF Male Deaths')
plot(testM,type='l',col='blue',main='PHF Male Deaths')
lines(rollmean(rollmean(testM,40),40),col='red')
abline(v=seq(0,5844,by = 365))

df11 <- pad(dfM,
            interval = "day",
            start_val = as.POSIXct('2005-01-01'),
            end_val = as.POSIXct('2020-12-31')
)


Fe <- PHF %>%
  filter(SEX == 'F')
dfFe <- Fe %>%
  group_by(Date) %>%
  summarise(totalFemale=sum(Count))

testF <- dfFe$totalFemale

plot(dfFe,type='l',main='PHF Female Deaths')
plot(testF,type='l',col='blue',main='PHF Female Deaths')
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

for(i in 1:ncol(dfwideG)){
 assign(names(dfwideG)[i], dfwideG[[i]])
}

dflongG <- pivot_longer(data = dfwideG,
                        cols = c(2:3),
                        names_to = "Gender",
                        values_to = "Mort")

#This produces a line plot
ggplot(dflongG, aes(Date, Mort, color=Gender)) +
  geom_line() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("PHF Mortality by Gender")

ggplot(dflongG, aes(Date, Mort, color=Gender)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("PHF Mortality by Gender")

#This produces a bar plot
ggplot(dflongG, aes(Date, Mort, color=Gender)) +
  geom_bar(stat="identity") +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("PHF Mortality by Gender")

