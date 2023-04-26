#Race Population Grab

library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

Bridged_Race_Population_Estimates_1990_2020_2 <- read_excel("~/Downloads/Bridged-Race Population Estimates 1990-2020-2.xls",range = "A1:H9203")
dforg <- Bridged_Race_Population_Estimates_1990_2020_2

#Place Count/City in Alphbetical Order
df <- dforg[order(dforg$County),]

#Correct Bedford for each Race Category
#Indigenous
df[716:720,8] = df[716:720,8] + df[691:695,8]
df[732,8] = df[732,8] + df[696,8]

#Asian/PI
df[733:737,8] = df[733:737,8] + df[697:701,8]
df[749,8] = df[749,8] + df[702,8]

#Black
df[750:754,8] = df[750:754,8] + df[703:707,8]
df[766,8] = df[766,8] + df[708,8]

#White
df[767:771,8] = df[767:771,8] + df[709:713,8]
df[783,8] = df[783,8] + df[714,8]

#Total
df[784,8] = df[784,8] + df[715,8]

#Remove Bedford City
df <- df[-(691:715), ]
county <- data.frame(table(df$County))

#Replace County and City with WxID
VA_county_wxstation_links_1_ <- read_excel("~/Downloads/VA.county.wxstation.links (1).xlsx")
Key <- VA_county_wxstation_links_1_[VA_county_wxstation_links_1_$ID != "VA", ]

z<-rep(c(Key$ID),each=69)
z <- str_replace_all(z, c("BLF"= "VJI","MFV"="RIC","MTV"="EMV"))

#Replace county names with WxID by key
df$County <- z
df$Race <- str_replace_all(df$Race, c("American Indian or Alaska Native" = "Indigenous",
                                      "Asian or Pacific Islander" = "Asian/PI",
                                      "Black or African American" = "Black"))
df_nototal <- df[!grepl("Total", df$Notes),]
colnames(df_nototal)[6] <- "Year"

#Get Get Yearly Data for WxRace Categories

Test2 <- aggregate(df_nototal$Population, by=list(Race=df_nototal$Race, Year=df_nototal$Year, County=df_nototal$County), FUN=sum)

TestEZF <- Test2 %>%
  filter(grepl('EZF',County))
dfEZFpopalmost <- pivot_wider(TestEZF, id_cols = Year, names_from = Race,values_from = x)
dfcpopfinalEZF <- dfEZFpopalmost[,c(3,5,2,4,1)]
#TestEZFWide2 <- pivot_wider(TestEZF, id_cols = Race, names_from = Year,values_from = x)

#Standard Population

dfstdBlack <- df_nototal %>%
  filter(grepl('2020',Year))%>%
  filter(grepl('Black',Race))%>%
  summarise(VABlack2020=sum(Population))

dfstdWhite <- df_nototal %>%
  filter(grepl('2020',Year))%>%
  filter(grepl('White',Race))%>%
  summarise(VAWhite2020=sum(Population))

dfstdAsianPI <- df_nototal %>%
  filter(grepl('2020',Year))%>%
  filter(grepl('Asian/PI',Race))%>%
  summarise(VAAsianPI2020=sum(Population))

dfstdIndigenous <- df_nototal %>%
  filter(grepl('2020',Year))%>%
  filter(grepl('Indigenous',Race))%>%
  summarise(VAIndigenous2020=sum(Population))

dfstdtotal = dfstdBlack + dfstdWhite + dfstdAsianPI + dfstdIndigenous

dfstd <- data.frame((cbind(dfstdtotal,dfstdBlack,dfstdWhite,dfstdAsianPI,dfstdIndigenous)))
colnames(dfstd) <- c("Total","Black","White","AsianPI","Indigenous")  
                    