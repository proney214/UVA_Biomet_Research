#case crossover for Heat Waves (Exclusion = 2 and StrataLength = 28) remove missing AQ

#case examples, with and without air quality entirely, impact of only one AQ variable, suppressing Severes

library(season)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

#Load in Data
RICF=read_excel("~/Desktop/RIC.working.weatherprep.xlsx", sheet="Sheet1",na="NA")
RIC <- dplyr::select(RICF, Ozone, PM2.5, Date, RH1pm, TdF7am, ATF1pm, TF7am, mort, AdjustedMort, ColdWavesModerate,ColdWavesSevere,ColdWavesSevere,ColdDaysModerate,ColdDaysSevere,ColdDaysSevere,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesSevere,HotDaysModerate,HotDaysSevere,HotDaysSevere, Lag1,Lag2,dow,holidays,PM2.5)
RIC$date=as.Date(RIC$Date)

#Run case crossover with only heatwave column

RICexc <- RIC %>%
  filter(month(Date) > 3 & month(Date) < 10)

RIC519exc <- RIC %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

RIC20exc <- RIC %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)

model0 = casecross(AdjustedMort ~ HeatWavesSevere+Ozone+PM2.5, data=RICexc)
model1 = casecross(AdjustedMort ~ HeatWavesSevere+Ozone+PM2.5, data=RIC519exc)
model2 = casecross(AdjustedMort ~ HeatWavesSevere+Ozone+PM2.5, data=RIC20exc)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[7]

sum2 <- summary(model2)
coef2 = sum2[1]
se2 = sum2[7]

myORtable <- data.frame()
myORtable[1,1] = 1
myORtable[1,2] = sum1[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef1+(1.96*se1))
myORtable[1,4] = exp(coef1-(1.96*se1))
myORtable[2,1] = 2
myORtable[2,2] = sum2[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef2+(1.96*se2))
myORtable[2,4] = exp(coef2-(1.96*se2))
myORtable[3,1] = 0
myORtable[3,2] = sum0[1,2] #sum[3] = OR
myORtable[3,3] = exp(coef0+(1.96*se0))
myORtable[3,4] = exp(coef0-(1.96*se0))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="RIC HeatWavesSevere",y="Odds Ratio")+
  ylim(0.8,1.4)+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Total",color="black")+
  annotate(geom="text", x=1, y=1.08, label="2005-2019",color="black")+
  annotate(geom="text", x=2, y=1.08, label="2020",color="black")
