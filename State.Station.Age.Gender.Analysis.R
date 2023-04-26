##Plotting mortality for each age group and gender

library(readxl)
library(dplyr)
library(stringr)
library(zoo)
library(padr)
library(ggplot2)
library(tidyr)

#Load in and prepare filter for dataset
BigMortEx <- read_excel("~/Desktop/BigMort.xlsx", 
                      sheet = "Sheet1")

BigMort <- BigMortEx
BigMort$Date = as.Date(paste(BigMort$DOD_YR,BigMort$DOD_MO,BigMort$DOD_DY,sep="-"))

#Replace Station IDs with appropiriate station

BigMort$`Station ID` <- str_replace_all(BigMort$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))
  
#Make df focused
BigMort <- select(BigMort,Date,`Station ID`,SEX,AGE_GROUPS,Count)

BigMort$StationID <- BigMort$`Station ID`

#See total deaths for state in each age group
df <- BigMort %>%
  group_by(Date) %>%
  group_by(AGE_GROUPS) %>%
  summarise(total=sum(Count))

#Find sum of deaths for each station on each day by station
A <- BigMort %>%
  filter(AGE_GROUPS == '75+')%>%
  group_by(StationID, Date) %>%
  summarise(total75=sum(Count))

#65-74
B <- BigMort %>%
  filter(AGE_GROUPS == '65-74') %>%
  group_by(StationID, Date) %>%
  summarise(total65=sum(Count))

#50-64
C <- BigMort %>%
  filter(AGE_GROUPS == '50-64') %>%
  group_by(StationID, Date) %>%
  summarise(total50=sum(Count))

#40-49
D <- BigMort %>%
  filter(AGE_GROUPS == '40-49') %>%
  group_by(StationID, Date) %>%
  summarise(total40=sum(Count))

#30-39
E <- BigMort %>%
  filter(AGE_GROUPS == '30-39') %>%
  group_by(StationID, Date) %>%
  summarise(total30=sum(Count))

#18-29
F <- BigMort %>%
  filter(AGE_GROUPS == '18-29') %>%
  group_by(StationID, Date) %>%
  summarise(total18=sum(Count))

#12-17
G <- BigMort %>%
  filter(AGE_GROUPS == '44912') %>%
  group_by(StationID, Date) %>%
  summarise(total12=sum(Count))

#0-11
H <- BigMort %>%
  filter(AGE_GROUPS == '0-11') %>%
  group_by(StationID, Date) %>%
  summarise(total0=sum(Count))

#Plots!
ggplot(A, aes(Date, total75, color=StationID)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("75+ Mortality by Station")

ggplot(B, aes(Date, total65, color=StationID)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("65-74 Mortality by Station")

ggplot(C, aes(Date, total50, color=StationID)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("50-64 Mortality by Station")

ggplot(D, aes(Date, total40, color=StationID)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("40-49 Mortality by Station")

ggplot(E, aes(Date, total30, color=StationID)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("30-39 Mortality by Station")

ggplot(F, aes(Date, total18, color=StationID)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("18-29 Mortality by Station")

ggplot(G, aes(Date, total12, color=StationID)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("12-17 Mortality by Station")

ggplot(H, aes(Date, total0, color=StationID)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("0-11 Mortality by Station")


#Gender across the state

#Find sum of deaths for each station on each day by station
W <- BigMort %>%
  filter(SEX == 'F') %>%
  group_by(StationID, Date) %>%
  summarise(totalW=sum(Count))

M <- BigMort %>%
  filter(SEX == 'M') %>%
  group_by(StationID, Date) %>%
  summarise(totalM=sum(Count))

ggplot(W, aes(Date, totalW, color=StationID)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("Female Mortality by Station")

ggplot(M, aes(Date, totalM, color=StationID)) +
  geom_smooth() +
  xlab("Date") + ylab("Mortalities") +
  theme_gray()+
  ggtitle("Male Mortality by Station")

