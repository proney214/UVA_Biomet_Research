# Casecross v3

library(season)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)

#Load in Data
IADF=read_excel("~/Desktop/IAD.Final.Weather.xlsx", sheet="Sheet1",na="NA")
IAD <- dplyr::select(IADF, OzoneREVISED,PM2.5REVISED, Date, Mort,MDC.01,MDC.04,MDC.05,MDC.19,Non.billable.ICD,ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme,dow,holidays)
IAD$date=as.Date(IAD$Date)

#Run case crossover with only heatwave column

IADexc_Hot <- IAD %>%
  filter(month(Date) > 3 & month(Date) < 10)

IAD519exc_Hot <- IAD %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

IAD20exc_Hot <- IAD %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)



model0_HeatWavesModerate = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, data=IADexc_Hot)
model1_HeatWavesModerate = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, data=IAD519exc_Hot)
model2_HeatWavesModerate = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, data=IAD20exc_Hot)

sum0_HeatWavesModerate <- summary(model0_HeatWavesModerate)
coef0_HeatWavesModerate = sum0_HeatWavesModerate[1]
se0_HeatWavesModerate = sum0_HeatWavesModerate[7]

model0_HeatWavesModerate$c.model$coefficients
sum1_HeatWavesModerate <- summary(model1_HeatWavesModerate)
coef1_HeatWavesModerate = sum1_HeatWavesModerate[1]
se1_HeatWavesModerate = sum1_HeatWavesModerate[7]

sum2_HeatWavesModerate <- summary(model2_HeatWavesModerate)
coef2_HeatWavesModerate = sum2_HeatWavesModerate[1]
se2_HeatWavesModerate = sum2_HeatWavesModerate[7]

myORtable_HeatWavesModerate <- data.frame()
myORtable_HeatWavesModerate[1,1] = 1
myORtable_HeatWavesModerate[1,2] = sum1_HeatWavesModerate[1,2] #sum[3] = OR
myORtable_HeatWavesModerate[1,3] = exp(coef1_HeatWavesModerate+(1.96*se1_HeatWavesModerate))
myORtable_HeatWavesModerate[1,4] = exp(coef1_HeatWavesModerate-(1.96*se1_HeatWavesModerate))
myORtable_HeatWavesModerate[2,1] = 2
myORtable_HeatWavesModerate[2,2] = sum2_HeatWavesModerate[1,2] #sum[3] = OR
myORtable_HeatWavesModerate[2,3] = exp(coef2_HeatWavesModerate+(1.96*se2_HeatWavesModerate))
myORtable_HeatWavesModerate[2,4] = exp(coef2_HeatWavesModerate-(1.96*se2_HeatWavesModerate))
myORtable_HeatWavesModerate[3,1] = 0
myORtable_HeatWavesModerate[3,2] = sum0_HeatWavesModerate[1,2] #sum[3] = OR
myORtable_HeatWavesModerate[3,3] = exp(coef0_HeatWavesModerate+(1.96*se0_HeatWavesModerate))
myORtable_HeatWavesModerate[3,4] = exp(coef0_HeatWavesModerate-(1.96*se0_HeatWavesModerate))
colnames(myORtable_HeatWavesModerate) <- c("Model","OR","UpperCI","LowerCI")

IADHWM <- ggplot(myORtable_HeatWavesModerate) +
  geom_point(aes(x=Model, y=myORtable_HeatWavesModerate[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable_HeatWavesModerate[,3], ymax=myORtable_HeatWavesModerate[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD Mort HeatWavesModerate",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Total",color="black")+
  annotate(geom="text", x=1, y=1.08, label="2005-2019",color="black")+
  annotate(geom="text", x=2, y=1.08, label="2020",color="black")



model0_HeatWavesSevere = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, data=IADexc_Hot)
model1_HeatWavesSevere = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, data=IAD519exc_Hot)
model2_HeatWavesSevere = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, data=IAD20exc_Hot)

sum0_HeatWavesSevere <- summary(model0_HeatWavesSevere)
coef0_HeatWavesSevere = sum0_HeatWavesSevere[1]
se0_HeatWavesSevere = sum0_HeatWavesSevere[7]

model0_HeatWavesSevere$c.model$coefficients
sum1_HeatWavesSevere <- summary(model1_HeatWavesSevere)
coef1_HeatWavesSevere = sum1_HeatWavesSevere[1]
se1_HeatWavesSevere = sum1_HeatWavesSevere[7]

sum2_HeatWavesSevere <- summary(model2_HeatWavesSevere)
coef2_HeatWavesSevere = sum2_HeatWavesSevere[1]
se2_HeatWavesSevere = sum2_HeatWavesSevere[7]

myORtable_HeatWavesSevere <- data.frame()
myORtable_HeatWavesSevere[1,1] = 1
myORtable_HeatWavesSevere[1,2] = sum1_HeatWavesSevere[1,2] #sum[3] = OR
myORtable_HeatWavesSevere[1,3] = exp(coef1_HeatWavesSevere+(1.96*se1_HeatWavesSevere))
myORtable_HeatWavesSevere[1,4] = exp(coef1_HeatWavesSevere-(1.96*se1_HeatWavesSevere))
myORtable_HeatWavesSevere[2,1] = 2
myORtable_HeatWavesSevere[2,2] = sum2_HeatWavesSevere[1,2] #sum[3] = OR
myORtable_HeatWavesSevere[2,3] = exp(coef2_HeatWavesSevere+(1.96*se2_HeatWavesSevere))
myORtable_HeatWavesSevere[2,4] = exp(coef2_HeatWavesSevere-(1.96*se2_HeatWavesSevere))
myORtable_HeatWavesSevere[3,1] = 0
myORtable_HeatWavesSevere[3,2] = sum0_HeatWavesSevere[1,2] #sum[3] = OR
myORtable_HeatWavesSevere[3,3] = exp(coef0_HeatWavesSevere+(1.96*se0_HeatWavesSevere))
myORtable_HeatWavesSevere[3,4] = exp(coef0_HeatWavesSevere-(1.96*se0_HeatWavesSevere))
colnames(myORtable_HeatWavesSevere) <- c("Model","OR","UpperCI","LowerCI")

IADHWS <-ggplot(myORtable_HeatWavesSevere) +
  geom_point(aes(x=Model, y=myORtable_HeatWavesSevere[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable_HeatWavesSevere[,3], ymax=myORtable_HeatWavesSevere[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD Mort HeatWavesSevere",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Total",color="black")+
  annotate(geom="text", x=1, y=1.08, label="2005-2019",color="black")+
  annotate(geom="text", x=2, y=1.08, label="2020",color="black")



model0_HeatWavesExtreme = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, data=IADexc_Hot)
model1_HeatWavesExtreme = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, data=IAD519exc_Hot)
model2_HeatWavesExtreme = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, data=IAD20exc_Hot)

sum0_HeatWavesExtreme <- summary(model0_HeatWavesExtreme)
coef0_HeatWavesExtreme = sum0_HeatWavesExtreme[1]
se0_HeatWavesExtreme = sum0_HeatWavesExtreme[7]

model0_HeatWavesExtreme$c.model$coefficients
sum1_HeatWavesExtreme <- summary(model1_HeatWavesExtreme)
coef1_HeatWavesExtreme = sum1_HeatWavesExtreme[1]
se1_HeatWavesExtreme = sum1_HeatWavesExtreme[7]

sum2_HeatWavesExtreme <- summary(model2_HeatWavesExtreme)
coef2_HeatWavesExtreme = sum2_HeatWavesExtreme[1]
se2_HeatWavesExtreme = sum2_HeatWavesExtreme[7]

myORtable_HeatWavesExtreme <- data.frame()
myORtable_HeatWavesExtreme[1,1] = 1
myORtable_HeatWavesExtreme[1,2] = sum1_HeatWavesExtreme[1,2] #sum[3] = OR
myORtable_HeatWavesExtreme[1,3] = exp(coef1_HeatWavesExtreme+(1.96*se1_HeatWavesExtreme))
myORtable_HeatWavesExtreme[1,4] = exp(coef1_HeatWavesExtreme-(1.96*se1_HeatWavesExtreme))
myORtable_HeatWavesExtreme[2,1] = 2
myORtable_HeatWavesExtreme[2,2] = sum2_HeatWavesExtreme[1,2] #sum[3] = OR
myORtable_HeatWavesExtreme[2,3] = exp(coef2_HeatWavesExtreme+(1.96*se2_HeatWavesExtreme))
myORtable_HeatWavesExtreme[2,4] = exp(coef2_HeatWavesExtreme-(1.96*se2_HeatWavesExtreme))
myORtable_HeatWavesExtreme[3,1] = 0
myORtable_HeatWavesExtreme[3,2] = sum0_HeatWavesExtreme[1,2] #sum[3] = OR
myORtable_HeatWavesExtreme[3,3] = exp(coef0_HeatWavesExtreme+(1.96*se0_HeatWavesExtreme))
myORtable_HeatWavesExtreme[3,4] = exp(coef0_HeatWavesExtreme-(1.96*se0_HeatWavesExtreme))
colnames(myORtable_HeatWavesExtreme) <- c("Model","OR","UpperCI","LowerCI")

IADHWE <-ggplot(myORtable_HeatWavesExtreme) +
  geom_point(aes(x=Model, y=myORtable_HeatWavesExtreme[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable_HeatWavesExtreme[,3], ymax=myORtable_HeatWavesExtreme[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD Mort HeatWavesExtreme",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Total",color="black")+
  annotate(geom="text", x=1, y=1.08, label="2005-2019",color="black")+
  annotate(geom="text", x=2, y=1.08, label="2020",color="black")

#Days

model0_HotDaysModerate = casecross(Mort ~ HotDaysModerate+OzoneREVISED+PM2.5REVISED, data=IADexc_Hot)
model1_HotDaysModerate = casecross(Mort ~ HotDaysModerate+OzoneREVISED+PM2.5REVISED, data=IAD519exc_Hot)
model2_HotDaysModerate = casecross(Mort ~ HotDaysModerate+OzoneREVISED+PM2.5REVISED, data=IAD20exc_Hot)

sum0_HotDaysModerate <- summary(model0_HotDaysModerate)
coef0_HotDaysModerate = sum0_HotDaysModerate[1]
se0_HotDaysModerate = sum0_HotDaysModerate[7]

model0_HotDaysModerate$c.model$coefficients
sum1_HotDaysModerate <- summary(model1_HotDaysModerate)
coef1_HotDaysModerate = sum1_HotDaysModerate[1]
se1_HotDaysModerate = sum1_HotDaysModerate[7]

sum2_HotDaysModerate <- summary(model2_HotDaysModerate)
coef2_HotDaysModerate = sum2_HotDaysModerate[1]
se2_HotDaysModerate = sum2_HotDaysModerate[7]

myORtable_HotDaysModerate <- data.frame()
myORtable_HotDaysModerate[1,1] = 1
myORtable_HotDaysModerate[1,2] = sum1_HotDaysModerate[1,2] #sum[3] = OR
myORtable_HotDaysModerate[1,3] = exp(coef1_HotDaysModerate+(1.96*se1_HotDaysModerate))
myORtable_HotDaysModerate[1,4] = exp(coef1_HotDaysModerate-(1.96*se1_HotDaysModerate))
myORtable_HotDaysModerate[2,1] = 2
myORtable_HotDaysModerate[2,2] = sum2_HotDaysModerate[1,2] #sum[3] = OR
myORtable_HotDaysModerate[2,3] = exp(coef2_HotDaysModerate+(1.96*se2_HotDaysModerate))
myORtable_HotDaysModerate[2,4] = exp(coef2_HotDaysModerate-(1.96*se2_HotDaysModerate))
myORtable_HotDaysModerate[3,1] = 0
myORtable_HotDaysModerate[3,2] = sum0_HotDaysModerate[1,2] #sum[3] = OR
myORtable_HotDaysModerate[3,3] = exp(coef0_HotDaysModerate+(1.96*se0_HotDaysModerate))
myORtable_HotDaysModerate[3,4] = exp(coef0_HotDaysModerate-(1.96*se0_HotDaysModerate))
colnames(myORtable_HotDaysModerate) <- c("Model","OR","UpperCI","LowerCI")

IADHDM <-ggplot(myORtable_HotDaysModerate) +
  geom_point(aes(x=Model, y=myORtable_HotDaysModerate[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable_HotDaysModerate[,3], ymax=myORtable_HotDaysModerate[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD HotDaysModerate",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Total",color="black")+
  annotate(geom="text", x=1, y=1.08, label="2005-2019",color="black")+
  annotate(geom="text", x=2, y=1.08, label="2020",color="black")

model0_HotDaysSevere = casecross(Mort ~ HotDaysSevere+OzoneREVISED+PM2.5REVISED, data=IADexc_Hot)
model1_HotDaysSevere = casecross(Mort ~ HotDaysSevere+OzoneREVISED+PM2.5REVISED, data=IAD519exc_Hot)
model2_HotDaysSevere = casecross(Mort ~ HotDaysSevere+OzoneREVISED+PM2.5REVISED, data=IAD20exc_Hot)

sum0_HotDaysSevere <- summary(model0_HotDaysSevere)
coef0_HotDaysSevere = sum0_HotDaysSevere[1]
se0_HotDaysSevere = sum0_HotDaysSevere[7]

model0_HotDaysSevere$c.model$coefficients
sum1_HotDaysSevere <- summary(model1_HotDaysSevere)
coef1_HotDaysSevere = sum1_HotDaysSevere[1]
se1_HotDaysSevere = sum1_HotDaysSevere[7]

sum2_HotDaysSevere <- summary(model2_HotDaysSevere)
coef2_HotDaysSevere = sum2_HotDaysSevere[1]
se2_HotDaysSevere = sum2_HotDaysSevere[7]

myORtable_HotDaysSevere <- data.frame()
myORtable_HotDaysSevere[1,1] = 1
myORtable_HotDaysSevere[1,2] = sum1_HotDaysSevere[1,2] #sum[3] = OR
myORtable_HotDaysSevere[1,3] = exp(coef1_HotDaysSevere+(1.96*se1_HotDaysSevere))
myORtable_HotDaysSevere[1,4] = exp(coef1_HotDaysSevere-(1.96*se1_HotDaysSevere))
myORtable_HotDaysSevere[2,1] = 2
myORtable_HotDaysSevere[2,2] = sum2_HotDaysSevere[1,2] #sum[3] = OR
myORtable_HotDaysSevere[2,3] = exp(coef2_HotDaysSevere+(1.96*se2_HotDaysSevere))
myORtable_HotDaysSevere[2,4] = exp(coef2_HotDaysSevere-(1.96*se2_HotDaysSevere))
myORtable_HotDaysSevere[3,1] = 0
myORtable_HotDaysSevere[3,2] = sum0_HotDaysSevere[1,2] #sum[3] = OR
myORtable_HotDaysSevere[3,3] = exp(coef0_HotDaysSevere+(1.96*se0_HotDaysSevere))
myORtable_HotDaysSevere[3,4] = exp(coef0_HotDaysSevere-(1.96*se0_HotDaysSevere))
colnames(myORtable_HotDaysSevere) <- c("Model","OR","UpperCI","LowerCI")

IADHDS <-ggplot(myORtable_HotDaysSevere) +
  geom_point(aes(x=Model, y=myORtable_HotDaysSevere[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable_HotDaysSevere[,3], ymax=myORtable_HotDaysSevere[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD HotDaysSevere",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Total",color="black")+
  annotate(geom="text", x=1, y=1.08, label="2005-2019",color="black")+
  annotate(geom="text", x=2, y=1.08, label="2020",color="black")


model0_HotDaysExtreme = casecross(Mort ~ HotDaysExtreme+OzoneREVISED+PM2.5REVISED, data=IADexc_Hot)
model1_HotDaysExtreme = casecross(Mort ~ HotDaysExtreme+OzoneREVISED+PM2.5REVISED, data=IAD519exc_Hot)
model2_HotDaysExtreme = casecross(Mort ~ HotDaysExtreme+OzoneREVISED+PM2.5REVISED, data=IAD20exc_Hot)

sum0_HotDaysExtreme <- summary(model0_HotDaysExtreme)
coef0_HotDaysExtreme = sum0_HotDaysExtreme[1]
se0_HotDaysExtreme = sum0_HotDaysExtreme[7]

model0_HotDaysExtreme$c.model$coefficients
sum1_HotDaysExtreme <- summary(model1_HotDaysExtreme)
coef1_HotDaysExtreme = sum1_HotDaysExtreme[1]
se1_HotDaysExtreme = sum1_HotDaysExtreme[7]

sum2_HotDaysExtreme <- summary(model2_HotDaysExtreme)
coef2_HotDaysExtreme = sum2_HotDaysExtreme[1]
se2_HotDaysExtreme = sum2_HotDaysExtreme[7]

myORtable_HotDaysExtreme <- data.frame()
myORtable_HotDaysExtreme[1,1] = 1
myORtable_HotDaysExtreme[1,2] = sum1_HotDaysExtreme[1,2] #sum[3] = OR
myORtable_HotDaysExtreme[1,3] = exp(coef1_HotDaysExtreme+(1.96*se1_HotDaysExtreme))
myORtable_HotDaysExtreme[1,4] = exp(coef1_HotDaysExtreme-(1.96*se1_HotDaysExtreme))
myORtable_HotDaysExtreme[2,1] = 2
myORtable_HotDaysExtreme[2,2] = sum2_HotDaysExtreme[1,2] #sum[3] = OR
myORtable_HotDaysExtreme[2,3] = exp(coef2_HotDaysExtreme+(1.96*se2_HotDaysExtreme))
myORtable_HotDaysExtreme[2,4] = exp(coef2_HotDaysExtreme-(1.96*se2_HotDaysExtreme))
myORtable_HotDaysExtreme[3,1] = 0
myORtable_HotDaysExtreme[3,2] = sum0_HotDaysExtreme[1,2] #sum[3] = OR
myORtable_HotDaysExtreme[3,3] = exp(coef0_HotDaysExtreme+(1.96*se0_HotDaysExtreme))
myORtable_HotDaysExtreme[3,4] = exp(coef0_HotDaysExtreme-(1.96*se0_HotDaysExtreme))
colnames(myORtable_HotDaysExtreme) <- c("Model","OR","UpperCI","LowerCI")

IADHDE <-ggplot(myORtable_HotDaysExtreme) +
  geom_point(aes(x=Model, y=myORtable_HotDaysExtreme[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable_HotDaysExtreme[,3], ymax=myORtable_HotDaysExtreme[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD HotDaysExtreme",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Total",color="black")+
  annotate(geom="text", x=1, y=1.08, label="2005-2019",color="black")+
  annotate(geom="text", x=2, y=1.08, label="2020",color="black")

IADexc_Cold <- IAD %>%
  filter(month(Date) < 4 | month(Date) >= 10)

IAD519exc_Cold <- IAD %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) < 4 | month(Date) >= 10)

IAD20exc_Cold <- IAD %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) < 4 | month(Date) >= 10)

model0_ColdWavesModerate = casecross(Mort ~ ColdWavesModerate+OzoneREVISED+PM2.5REVISED, data=IADexc_Cold)
model1_ColdWavesModerate = casecross(Mort ~ ColdWavesModerate+OzoneREVISED+PM2.5REVISED, data=IAD519exc_Cold)
model2_ColdWavesModerate = casecross(Mort ~ ColdWavesModerate+OzoneREVISED+PM2.5REVISED, data=IAD20exc_Cold)

sum0_ColdWavesModerate <- summary(model0_ColdWavesModerate)
coef0_ColdWavesModerate = sum0_ColdWavesModerate[1]
se0_ColdWavesModerate = sum0_ColdWavesModerate[7]

model0_ColdWavesModerate$c.model$coefficients
sum1_ColdWavesModerate <- summary(model1_ColdWavesModerate)
coef1_ColdWavesModerate = sum1_ColdWavesModerate[1]
se1_ColdWavesModerate = sum1_ColdWavesModerate[7]

sum2_ColdWavesModerate <- summary(model2_ColdWavesModerate)
coef2_ColdWavesModerate = sum2_ColdWavesModerate[1]
se2_ColdWavesModerate = sum2_ColdWavesModerate[7]

myORtable_ColdWavesModerate <- data.frame()
myORtable_ColdWavesModerate[1,1] = 1
myORtable_ColdWavesModerate[1,2] = sum1_ColdWavesModerate[1,2] #sum[3] = OR
myORtable_ColdWavesModerate[1,3] = exp(coef1_ColdWavesModerate+(1.96*se1_ColdWavesModerate))
myORtable_ColdWavesModerate[1,4] = exp(coef1_ColdWavesModerate-(1.96*se1_ColdWavesModerate))
myORtable_ColdWavesModerate[2,1] = 2
myORtable_ColdWavesModerate[2,2] = sum2_ColdWavesModerate[1,2] #sum[3] = OR
myORtable_ColdWavesModerate[2,3] = exp(coef2_ColdWavesModerate+(1.96*se2_ColdWavesModerate))
myORtable_ColdWavesModerate[2,4] = exp(coef2_ColdWavesModerate-(1.96*se2_ColdWavesModerate))
myORtable_ColdWavesModerate[3,1] = 0
myORtable_ColdWavesModerate[3,2] = sum0_ColdWavesModerate[1,2] #sum[3] = OR
myORtable_ColdWavesModerate[3,3] = exp(coef0_ColdWavesModerate+(1.96*se0_ColdWavesModerate))
myORtable_ColdWavesModerate[3,4] = exp(coef0_ColdWavesModerate-(1.96*se0_ColdWavesModerate))
colnames(myORtable_ColdWavesModerate) <- c("Model","OR","UpperCI","LowerCI")

IADCWM <-ggplot(myORtable_ColdWavesModerate) +
  geom_point(aes(x=Model, y=myORtable_ColdWavesModerate[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable_ColdWavesModerate[,3], ymax=myORtable_ColdWavesModerate[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD Mort ColdWavesModerate",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Total",color="black")+
  annotate(geom="text", x=1, y=1.08, label="2005-2019",color="black")+
  annotate(geom="text", x=2, y=1.08, label="2020",color="black")



model0_ColdWavesSevere = casecross(Mort ~ ColdWavesSevere+OzoneREVISED+PM2.5REVISED, data=IADexc_Cold)
model1_ColdWavesSevere = casecross(Mort ~ ColdWavesSevere+OzoneREVISED+PM2.5REVISED, data=IAD519exc_Cold)
model2_ColdWavesSevere = casecross(Mort ~ ColdWavesSevere+OzoneREVISED+PM2.5REVISED, data=IAD20exc_Cold)

sum0_ColdWavesSevere <- summary(model0_ColdWavesSevere)
coef0_ColdWavesSevere = sum0_ColdWavesSevere[1]
se0_ColdWavesSevere = sum0_ColdWavesSevere[7]

model0_ColdWavesSevere$c.model$coefficients
sum1_ColdWavesSevere <- summary(model1_ColdWavesSevere)
coef1_ColdWavesSevere = sum1_ColdWavesSevere[1]
se1_ColdWavesSevere = sum1_ColdWavesSevere[7]

sum2_ColdWavesSevere <- summary(model2_ColdWavesSevere)
coef2_ColdWavesSevere = sum2_ColdWavesSevere[1]
se2_ColdWavesSevere = sum2_ColdWavesSevere[7]

myORtable_ColdWavesSevere <- data.frame()
myORtable_ColdWavesSevere[1,1] = 1
myORtable_ColdWavesSevere[1,2] = sum1_ColdWavesSevere[1,2] #sum[3] = OR
myORtable_ColdWavesSevere[1,3] = exp(coef1_ColdWavesSevere+(1.96*se1_ColdWavesSevere))
myORtable_ColdWavesSevere[1,4] = exp(coef1_ColdWavesSevere-(1.96*se1_ColdWavesSevere))
myORtable_ColdWavesSevere[2,1] = 2
myORtable_ColdWavesSevere[2,2] = sum2_ColdWavesSevere[1,2] #sum[3] = OR
myORtable_ColdWavesSevere[2,3] = exp(coef2_ColdWavesSevere+(1.96*se2_ColdWavesSevere))
myORtable_ColdWavesSevere[2,4] = exp(coef2_ColdWavesSevere-(1.96*se2_ColdWavesSevere))
myORtable_ColdWavesSevere[3,1] = 0
myORtable_ColdWavesSevere[3,2] = sum0_ColdWavesSevere[1,2] #sum[3] = OR
myORtable_ColdWavesSevere[3,3] = exp(coef0_ColdWavesSevere+(1.96*se0_ColdWavesSevere))
myORtable_ColdWavesSevere[3,4] = exp(coef0_ColdWavesSevere-(1.96*se0_ColdWavesSevere))
colnames(myORtable_ColdWavesSevere) <- c("Model","OR","UpperCI","LowerCI")

IADCWS <-ggplot(myORtable_ColdWavesSevere) +
  geom_point(aes(x=Model, y=myORtable_ColdWavesSevere[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable_ColdWavesSevere[,3], ymax=myORtable_ColdWavesSevere[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD Mort ColdWavesSevere",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Total",color="black")+
  annotate(geom="text", x=1, y=1.08, label="2005-2019",color="black")+
  annotate(geom="text", x=2, y=1.08, label="2020",color="black")



model0_ColdWavesExtreme = casecross(Mort ~ ColdWavesExtreme+OzoneREVISED+PM2.5REVISED, data=IADexc_Cold)
model1_ColdWavesExtreme = casecross(Mort ~ ColdWavesExtreme+OzoneREVISED+PM2.5REVISED, data=IAD519exc_Cold)
model2_ColdWavesExtreme = casecross(Mort ~ ColdWavesExtreme+OzoneREVISED+PM2.5REVISED, data=IAD20exc_Cold)

sum0_ColdWavesExtreme <- summary(model0_ColdWavesExtreme)
coef0_ColdWavesExtreme = sum0_ColdWavesExtreme[1]
se0_ColdWavesExtreme = sum0_ColdWavesExtreme[7]

model0_ColdWavesExtreme$c.model$coefficients
sum1_ColdWavesExtreme <- summary(model1_ColdWavesExtreme)
coef1_ColdWavesExtreme = sum1_ColdWavesExtreme[1]
se1_ColdWavesExtreme = sum1_ColdWavesExtreme[7]

sum2_ColdWavesExtreme <- summary(model2_ColdWavesExtreme)
coef2_ColdWavesExtreme = sum2_ColdWavesExtreme[1]
se2_ColdWavesExtreme = sum2_ColdWavesExtreme[7]

myORtable_ColdWavesExtreme <- data.frame()
myORtable_ColdWavesExtreme[1,1] = 1
myORtable_ColdWavesExtreme[1,2] = sum1_ColdWavesExtreme[1,2] #sum[3] = OR
myORtable_ColdWavesExtreme[1,3] = exp(coef1_ColdWavesExtreme+(1.96*se1_ColdWavesExtreme))
myORtable_ColdWavesExtreme[1,4] = exp(coef1_ColdWavesExtreme-(1.96*se1_ColdWavesExtreme))
myORtable_ColdWavesExtreme[2,1] = 2
myORtable_ColdWavesExtreme[2,2] = sum2_ColdWavesExtreme[1,2] #sum[3] = OR
myORtable_ColdWavesExtreme[2,3] = exp(coef2_ColdWavesExtreme+(1.96*se2_ColdWavesExtreme))
myORtable_ColdWavesExtreme[2,4] = exp(coef2_ColdWavesExtreme-(1.96*se2_ColdWavesExtreme))
myORtable_ColdWavesExtreme[3,1] = 0
myORtable_ColdWavesExtreme[3,2] = sum0_ColdWavesExtreme[1,2] #sum[3] = OR
myORtable_ColdWavesExtreme[3,3] = exp(coef0_ColdWavesExtreme+(1.96*se0_ColdWavesExtreme))
myORtable_ColdWavesExtreme[3,4] = exp(coef0_ColdWavesExtreme-(1.96*se0_ColdWavesExtreme))
colnames(myORtable_ColdWavesExtreme) <- c("Model","OR","UpperCI","LowerCI")

IADCWE <-ggplot(myORtable_ColdWavesExtreme) +
  geom_point(aes(x=Model, y=myORtable_ColdWavesExtreme[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable_ColdWavesExtreme[,3], ymax=myORtable_ColdWavesExtreme[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD Mort ColdWavesExtreme",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Total",color="black")+
  annotate(geom="text", x=1, y=1.08, label="2005-2019",color="black")+
  annotate(geom="text", x=2, y=1.08, label="2020",color="black")

#Days

model0_ColdDaysModerate = casecross(Mort ~ ColdDaysModerate+OzoneREVISED+PM2.5REVISED, data=IADexc_Cold)
model1_ColdDaysModerate = casecross(Mort ~ ColdDaysModerate+OzoneREVISED+PM2.5REVISED, data=IAD519exc_Cold)
model2_ColdDaysModerate = casecross(Mort ~ ColdDaysModerate+OzoneREVISED+PM2.5REVISED, data=IAD20exc_Cold)

sum0_ColdDaysModerate <- summary(model0_ColdDaysModerate)
coef0_ColdDaysModerate = sum0_ColdDaysModerate[1]
se0_ColdDaysModerate = sum0_ColdDaysModerate[7]

model0_ColdDaysModerate$c.model$coefficients
sum1_ColdDaysModerate <- summary(model1_ColdDaysModerate)
coef1_ColdDaysModerate = sum1_ColdDaysModerate[1]
se1_ColdDaysModerate = sum1_ColdDaysModerate[7]

sum2_ColdDaysModerate <- summary(model2_ColdDaysModerate)
coef2_ColdDaysModerate = sum2_ColdDaysModerate[1]
se2_ColdDaysModerate = sum2_ColdDaysModerate[7]

myORtable_ColdDaysModerate <- data.frame()
myORtable_ColdDaysModerate[1,1] = 1
myORtable_ColdDaysModerate[1,2] = sum1_ColdDaysModerate[1,2] #sum[3] = OR
myORtable_ColdDaysModerate[1,3] = exp(coef1_ColdDaysModerate+(1.96*se1_ColdDaysModerate))
myORtable_ColdDaysModerate[1,4] = exp(coef1_ColdDaysModerate-(1.96*se1_ColdDaysModerate))
myORtable_ColdDaysModerate[2,1] = 2
myORtable_ColdDaysModerate[2,2] = sum2_ColdDaysModerate[1,2] #sum[3] = OR
myORtable_ColdDaysModerate[2,3] = exp(coef2_ColdDaysModerate+(1.96*se2_ColdDaysModerate))
myORtable_ColdDaysModerate[2,4] = exp(coef2_ColdDaysModerate-(1.96*se2_ColdDaysModerate))
myORtable_ColdDaysModerate[3,1] = 0
myORtable_ColdDaysModerate[3,2] = sum0_ColdDaysModerate[1,2] #sum[3] = OR
myORtable_ColdDaysModerate[3,3] = exp(coef0_ColdDaysModerate+(1.96*se0_ColdDaysModerate))
myORtable_ColdDaysModerate[3,4] = exp(coef0_ColdDaysModerate-(1.96*se0_ColdDaysModerate))
colnames(myORtable_ColdDaysModerate) <- c("Model","OR","UpperCI","LowerCI")

IADCDM <-ggplot(myORtable_ColdDaysModerate) +
  geom_point(aes(x=Model, y=myORtable_ColdDaysModerate[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable_ColdDaysModerate[,3], ymax=myORtable_ColdDaysModerate[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD Mort ColdDaysModerate",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Total",color="black")+
  annotate(geom="text", x=1, y=1.08, label="2005-2019",color="black")+
  annotate(geom="text", x=2, y=1.08, label="2020",color="black")

model0_ColdDaysSevere = casecross(Mort ~ ColdDaysSevere+OzoneREVISED+PM2.5REVISED, data=IADexc_Cold)
model1_ColdDaysSevere = casecross(Mort ~ ColdDaysSevere+OzoneREVISED+PM2.5REVISED, data=IAD519exc_Cold)
model2_ColdDaysSevere = casecross(Mort ~ ColdDaysSevere+OzoneREVISED+PM2.5REVISED, data=IAD20exc_Cold)

sum0_ColdDaysSevere <- summary(model0_ColdDaysSevere)
coef0_ColdDaysSevere = sum0_ColdDaysSevere[1]
se0_ColdDaysSevere = sum0_ColdDaysSevere[7]

model0_ColdDaysSevere$c.model$coefficients
sum1_ColdDaysSevere <- summary(model1_ColdDaysSevere)
coef1_ColdDaysSevere = sum1_ColdDaysSevere[1]
se1_ColdDaysSevere = sum1_ColdDaysSevere[7]

sum2_ColdDaysSevere <- summary(model2_ColdDaysSevere)
coef2_ColdDaysSevere = sum2_ColdDaysSevere[1]
se2_ColdDaysSevere = sum2_ColdDaysSevere[7]

myORtable_ColdDaysSevere <- data.frame()
myORtable_ColdDaysSevere[1,1] = 1
myORtable_ColdDaysSevere[1,2] = sum1_ColdDaysSevere[1,2] #sum[3] = OR
myORtable_ColdDaysSevere[1,3] = exp(coef1_ColdDaysSevere+(1.96*se1_ColdDaysSevere))
myORtable_ColdDaysSevere[1,4] = exp(coef1_ColdDaysSevere-(1.96*se1_ColdDaysSevere))
myORtable_ColdDaysSevere[2,1] = 2
myORtable_ColdDaysSevere[2,2] = sum2_ColdDaysSevere[1,2] #sum[3] = OR
myORtable_ColdDaysSevere[2,3] = exp(coef2_ColdDaysSevere+(1.96*se2_ColdDaysSevere))
myORtable_ColdDaysSevere[2,4] = exp(coef2_ColdDaysSevere-(1.96*se2_ColdDaysSevere))
myORtable_ColdDaysSevere[3,1] = 0
myORtable_ColdDaysSevere[3,2] = sum0_ColdDaysSevere[1,2] #sum[3] = OR
myORtable_ColdDaysSevere[3,3] = exp(coef0_ColdDaysSevere+(1.96*se0_ColdDaysSevere))
myORtable_ColdDaysSevere[3,4] = exp(coef0_ColdDaysSevere-(1.96*se0_ColdDaysSevere))
colnames(myORtable_ColdDaysSevere) <- c("Model","OR","UpperCI","LowerCI")

IADCDS <-ggplot(myORtable_ColdDaysSevere) +
  geom_point(aes(x=Model, y=myORtable_ColdDaysSevere[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable_ColdDaysSevere[,3], ymax=myORtable_ColdDaysSevere[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD Mort ColdDaysSevere",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Total",color="black")+
  annotate(geom="text", x=1, y=1.08, label="2005-2019",color="black")+
  annotate(geom="text", x=2, y=1.08, label="2020",color="black")


model0_ColdDaysExtreme = casecross(Mort ~ ColdDaysExtreme+OzoneREVISED+PM2.5REVISED, data=IADexc_Cold)
model1_ColdDaysExtreme = casecross(Mort ~ ColdDaysExtreme+OzoneREVISED+PM2.5REVISED, data=IAD519exc_Cold)
model2_ColdDaysExtreme = casecross(Mort ~ ColdDaysExtreme+OzoneREVISED+PM2.5REVISED, data=IAD20exc_Cold)

sum0_ColdDaysExtreme <- summary(model0_ColdDaysExtreme)
coef0_ColdDaysExtreme = sum0_ColdDaysExtreme[1]
se0_ColdDaysExtreme = sum0_ColdDaysExtreme[7]

model0_ColdDaysExtreme$c.model$coefficients
sum1_ColdDaysExtreme <- summary(model1_ColdDaysExtreme)
coef1_ColdDaysExtreme = sum1_ColdDaysExtreme[1]
se1_ColdDaysExtreme = sum1_ColdDaysExtreme[7]

sum2_ColdDaysExtreme <- summary(model2_ColdDaysExtreme)
coef2_ColdDaysExtreme = sum2_ColdDaysExtreme[1]
se2_ColdDaysExtreme = sum2_ColdDaysExtreme[7]

myORtable_ColdDaysExtreme <- data.frame()
myORtable_ColdDaysExtreme[1,1] = 1
myORtable_ColdDaysExtreme[1,2] = sum1_ColdDaysExtreme[1,2] #sum[3] = OR
myORtable_ColdDaysExtreme[1,3] = exp(coef1_ColdDaysExtreme+(1.96*se1_ColdDaysExtreme))
myORtable_ColdDaysExtreme[1,4] = exp(coef1_ColdDaysExtreme-(1.96*se1_ColdDaysExtreme))
myORtable_ColdDaysExtreme[2,1] = 2
myORtable_ColdDaysExtreme[2,2] = sum2_ColdDaysExtreme[1,2] #sum[3] = OR
myORtable_ColdDaysExtreme[2,3] = exp(coef2_ColdDaysExtreme+(1.96*se2_ColdDaysExtreme))
myORtable_ColdDaysExtreme[2,4] = exp(coef2_ColdDaysExtreme-(1.96*se2_ColdDaysExtreme))
myORtable_ColdDaysExtreme[3,1] = 0
myORtable_ColdDaysExtreme[3,2] = sum0_ColdDaysExtreme[1,2] #sum[3] = OR
myORtable_ColdDaysExtreme[3,3] = exp(coef0_ColdDaysExtreme+(1.96*se0_ColdDaysExtreme))
myORtable_ColdDaysExtreme[3,4] = exp(coef0_ColdDaysExtreme-(1.96*se0_ColdDaysExtreme))
colnames(myORtable_ColdDaysExtreme) <- c("Model","OR","UpperCI","LowerCI")

IADCDE <-ggplot(myORtable_ColdDaysExtreme) +
  geom_point(aes(x=Model, y=myORtable_ColdDaysExtreme[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable_ColdDaysExtreme[,3], ymax=myORtable_ColdDaysExtreme[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD Mort ColdDaysExtreme",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Total",color="black")+
  annotate(geom="text", x=1, y=1.08, label="2005-2019",color="black")+
  annotate(geom="text", x=2, y=1.08, label="2020",color="black")


ggarrange(IADHWM, IADHWS,IADHWE, IADHDM,IADHDS,IADHDE, 
          #labels = c("A", "B", "C"),
          ncol = 3, nrow = 2)

ggarrange(IADCWM, IADCWS,IADCWE, IADCDM,IADCDS,IADCDE, 
          #labels = c("A", "B", "C"),
          ncol = 3, nrow = 2)
