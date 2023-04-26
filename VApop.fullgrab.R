library(readxl)
library(dplyr)
library(stringr)
library(zoo)
library(padr)
library(ggplot2)
library(tidyr)

Virginia_NCHS_Population_Locality_and_Age_Group_2005_2020 <- read_excel("~/Downloads/Virginia NCHS Population_Locality and Age Group_2005-2020 (1).xlsx", 
                                                                        skip = 1)

dfb <- Virginia_NCHS_Population_Locality_and_Age_Group_2005_2020

#Correct Bedford (Remove Bedford City)
dfb[181:200,4:19 ] <- dfb[181:200,4:19] + dfb[1921:1940,4:19 ]
dfb <- dfb[-(1921:1940),]

df <- dfb

#Replace Age Categories

df["Hs Age Group"][df["Hs Age Group"] == "0"] <- "0-9"
df[df == "1-4" | df == "5-9"] <- "0-9"
df[df == "10-14" | df == "15-17" | df == "18-19"] <- "10-19"
df[df == "20-24" | df == "25-29"] <- "20-29"
df[df == "30-34" | df == "35-39"] <- "30-39"
df[df == "40-44" | df == "45-49"] <- "40-49"
df[df == "50-54" | df == "55-59" | df == "60-64"] <- "50-64"
df[df == "65-69" | df == "70-74"] <- "65-74"
df[df == "75-79" | df == "80-84" | df == "85+"] <- "75+"

df$Localityname <- str_replace_all(df$Localityname, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))

df <- df[,-1]
df <- rename(df, AgeGroup = `Hs Age Group`)

dfLYH <- df[df$Localityname == "LYH", ]

#Use Paste Function to create sums for 2005-2020 quickly

dfLYH2005 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2005=sum(`2005`,na.rm=TRUE))

dfLYH2006 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2006=sum(`2006`,na.rm=TRUE))

dfLYH2007 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2007=sum(`2007`,na.rm=TRUE))

dfLYH2008 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2008=sum(`2008`,na.rm=TRUE))

dfLYH2009 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2009=sum(`2009`,na.rm=TRUE))

dfLYH2010 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2010=sum(`2010`,na.rm=TRUE))

dfLYH2011 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2011=sum(`2011`,na.rm=TRUE))

dfLYH2012 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2012=sum(`2012`,na.rm=TRUE))

dfLYH2013 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2013=sum(`2013`,na.rm=TRUE))

dfLYH2014 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2014=sum(`2014`,na.rm=TRUE))

dfLYH2015 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2015=sum(`2015`,na.rm=TRUE))

dfLYH2016 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2016=sum(`2016`,na.rm=TRUE))

dfLYH2017 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2017=sum(`2017`,na.rm=TRUE))

dfLYH2018 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2018=sum(`2018`,na.rm=TRUE))

dfLYH2019 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2019=sum(`2019`,na.rm=TRUE))

dfLYH2020 <- dfLYH %>%
  group_by(AgeGroup) %>%
  summarise(LYHsum2020=sum(`2020`,na.rm=TRUE))

myORdf <- data.frame(mget(paste0('dfLYH',2005:2020)))
toDelete <- seq(3, ncol(myORdf), 2)
LYHdffinal <- myORdf[,-toDelete]

LYHdf <- t(LYHdffinal)
colnames(LYHdf) <- LYHdf[1,]
LYHdf <- data.frame(LYHdf[-1,])
LYHdffinalSTD <- rev(LYHdf[,1:8])

dfcpopfinalLYH <- LYHdffinalSTD
dfcpopfinalLYH <- dfcpopfinalLYH %>% 
  mutate_if(is.character, as.numeric)
dfcpopfinalLYH <- data.frame(dfcpopfinalLYH)

dfmortfinalLYH <- data.frame(dfmortfinalLYH)

