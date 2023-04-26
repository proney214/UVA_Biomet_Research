#casecross loop_v2 with no AQ values

library(season)
library(readxl)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(dplyr)

SHDF=read_excel("~/Desktop/SHD.Final.Weather.xlsx", sheet="Sheet1",na="NA")
SHD <- dplyr::select(SHDF, OzoneREVISED,PM2.5REVISED, Date, Mort,MDC.01,MDC.04,MDC.05,MDC.19,Non.billable.ICD,ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme,dow,holidays)
SHD$date=as.Date(SHD$Date)


SHDexc_Hot <- SHD %>%
  filter(month(Date) > 3 & month(Date) < 10)

SHD519exc_Hot <- SHD %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

SHD20exc_Hot <- SHD %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)


myORtable1_HeatWavesModerate__exc2 <- data.frame()
myORtable2_HeatWavesModerate__exc2 <- data.frame()

library(season)

for (i in (c(7,14,21))) {
  model1_HeatWavesModerate__exc2 = casecross(Mort ~ HeatWavesModerate, stratalength = i, exclusion = 2, data=SHD519exc_Hot)
  model2_HeatWavesModerate__exc2 = casecross(Mort ~ HeatWavesModerate, stratalength = i, exclusion = 2, data=SHD20exc_Hot)
  
  sum1_HeatWavesModerate__exc2 <- summary(model1_HeatWavesModerate__exc2)
  coef1_HeatWavesModerate__exc2 = sum1_HeatWavesModerate__exc2[1]
  se1_HeatWavesModerate__exc2 = sum1_HeatWavesModerate__exc2[3]
  
  sum2_HeatWavesModerate__exc2 <- summary(model2_HeatWavesModerate__exc2)
  coef2_HeatWavesModerate__exc2 = sum2_HeatWavesModerate__exc2[1]
  se2_HeatWavesModerate__exc2 = sum2_HeatWavesModerate__exc2[3]
  
  myORtable1_HeatWavesModerate__exc2[i,1] = i
  myORtable1_HeatWavesModerate__exc2[i,2] = sum1_HeatWavesModerate__exc2[1,2] #sum[3] = OR
  myORtable1_HeatWavesModerate__exc2[i,3] = exp(coef1_HeatWavesModerate__exc2+(1.96*se1_HeatWavesModerate__exc2))
  myORtable1_HeatWavesModerate__exc2[i,4] = exp(coef1_HeatWavesModerate__exc2-(1.96*se1_HeatWavesModerate__exc2))
  colnames(myORtable1_HeatWavesModerate__exc2) <- c("Model","OR","UpperCI","LowerCI")
  
  myORtable2_HeatWavesModerate__exc2[i,1] = i
  myORtable2_HeatWavesModerate__exc2[i,2] = sum2_HeatWavesModerate__exc2[1,2] #sum[3] = OR
  myORtable2_HeatWavesModerate__exc2[i,3] = exp(coef2_HeatWavesModerate__exc2+(1.96*se2_HeatWavesModerate__exc2))
  myORtable2_HeatWavesModerate__exc2[i,4] = exp(coef2_HeatWavesModerate__exc2-(1.96*se2_HeatWavesModerate__exc2))
  colnames(myORtable2_HeatWavesModerate__exc2) <- c("Model","OR","UpperCI","LowerCI")
}

#2005-2019 Data

myORtable1_HeatWavesModerate__exc2_complete <- myORtable1_HeatWavesModerate__exc2[complete.cases(myORtable1_HeatWavesModerate__exc2), ]
#myORtable1_HeatWavesModerate__exc2_complete[,1] <- seq(1:length(myORtable1_HeatWavesModerate__exc2_complete$Model))
colnames(myORtable1_HeatWavesModerate__exc2_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
#myORtable1_HeatWavesModerate__exc2_complete$StrataLength = myORtable1_HeatWavesModerate__exc2_complete$StrataLength + 6

ggplot(myORtable1_HeatWavesModerate__exc2_complete) +
  geom_line(aes(x=StrataLength, y=myORtable1_HeatWavesModerate__exc2_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable1_HeatWavesModerate__exc2_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable1_HeatWavesModerate__exc2_complete[,3], ymax=myORtable1_HeatWavesModerate__exc2_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="SHD 2005-2019 Mort HeatWavesModerate exc=2",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2020 Data

myORtable2_HeatWavesModerate__exc2_complete <- myORtable2_HeatWavesModerate__exc2[complete.cases(myORtable2_HeatWavesModerate__exc2), ]
#myORtable2_HeatWavesModerate__exc2_complete[,1] <- seq(1:length(myORtable2_HeatWavesModerate__exc2_complete$Model))
colnames(myORtable2_HeatWavesModerate__exc2_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
#myORtable2_HeatWavesModerate__exc2_complete$StrataLength = c(7,14,21)

ggplot(myORtable2_HeatWavesModerate__exc2_complete) +
  geom_line(aes(x=StrataLength, y=myORtable2_HeatWavesModerate__exc2_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable2_HeatWavesModerate__exc2_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable2_HeatWavesModerate__exc2_complete[,3], ymax=myORtable2_HeatWavesModerate__exc2_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="SHD 2020 Mort HeatWavesModerate exc=2",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

View(myORtable1_HeatWavesModerate__exc2_complete) #2005-2019
View(myORtable2_HeatWavesModerate__exc2_complete) #2020

casecross(Mort ~ HeatWavesModerate+OzoneREVISED, stratalength = 7, exclusion = 2, data=SHD20exc_Hot)

