#case crossover for Heat Waves (Exclusion = 2 and StrataLength = 28)
library(season)
library(readxl)
library(dplyr)
library(ggplot2)

RIC=read_excel("~/Desktop/RIC.working.weatherprep.xlsx", sheet="Sheet1",na="NA")
ORF <- dplyr::select(ORFF, Date, ATF1pm, TF7am, mort, AdjustedMort, ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme, Lag1,Lag2,dow,holidays,PM2.5,Ozone)
ORF$date=as.Date(ORF$Date)

ORF519 <- ORF %>%
  filter(Date < "2020-01-01")

ORF20 <- ORF %>%
  filter(Date >= "2020-01-01")

model01 = casecross(mort ~ HeatWavesModerate+TdF7am, data=ORF)
model11= casecross(mort ~ HeatWavesModerate+TdF7am, data=ORF519)
model21 = casecross(mort ~ HeatWavesModerate+Ozone+PM2.5, data=ORF20)

model0 = casecross(Ozone ~ HeatWavesModerate+PM2.5, data=ORF)
model1 = casecross(Ozone ~ HeatWavesModerate+PM2.5, data=ORF519)
model2 = casecross(Ozone ~ HeatWavesModerate+PM2.5, data=ORF20)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[3]

sum01 <- summary(model01)
coef01 = sum01[1]
se01 = sum01[3]


sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[3]

sum11 <- summary(model11)
coef11 = sum11[1]
se11 = sum11[3]

sum2 <- summary(model2)
coef2 = sum2[1]
se2 = sum2[3]

sum21 <- summary(model21)
coef21 = sum21[1]
se21 = sum21[3]

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
myORtable[4,1] = 0.5
myORtable[4,2] = sum01[1,2] #sum[3] = OR
myORtable[4,3] = exp(coef01+(1.96*se01))
myORtable[4,4] = exp(coef01-(1.96*se01))
myORtable[5,1] = 1.5
myORtable[5,2] = sum11[1,2] #sum[3] = OR
myORtable[5,3] = exp(coef11+(1.96*se11))
myORtable[5,4] = exp(coef11-(1.96*se11))
myORtable[6,1] = 2.5
myORtable[6,2] = sum21[1,2] #sum[3] = OR
myORtable[6,3] = exp(coef21+(1.96*se21))
myORtable[6,4] = exp(coef21-(1.96*se21))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="ORF HeatWavesModerate",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  geom_vline(xintercept=0.8, linetype="dashed", color = "green")+
  geom_vline(xintercept=1.8, linetype="dashed", color = "green")

ggsave(file.path("~/Desktop/HeatWaveAnalysis051920/ORF_HeatWavesModerate.png"))

#Adjusted is whole numbers, mort at adding a half



