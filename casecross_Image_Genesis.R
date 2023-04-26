#Casecross Image Genesis


library(readxl)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(season)
library(ggthemes)
library(ggtext)

#SCARTCH

CHOF=read_excel("~/Desktop/CHO.Final.Weather.xlsx", sheet="Sheet1",na="NA")
CHO <- dplyr::select(CHOF, OzoneREVISED,PM2.5REVISED, Date, Mort,MDC.01,MDC.04,MDC.05,MDC.19,Non.billable.ICD,ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme,dow,holidays)
CHO$date=as.Date(CHO$Date)

CHOexc_Hot <- CHO %>%
  filter(month(Date) > 3 & month(Date) < 10)

CHO519exc_Hot <- CHO %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

CHO20exc_Hot <- CHO %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)



model0_CHO = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED, stratalength = 7, exclusion = 2, data=CHO519exc_Hot)
model1_CHO = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED, stratalength = 7, exclusion = 2, data=CHO20exc_Hot)


sum0 <- summary(model0_CHO)
coef0 = sum0[1]
se0 = sum0[5]

sum1 <- summary(model1_CHO)
coef1 = sum1[1]
se1 = sum1[5]


myORtable_CHO <- data.frame()
myORtable_CHO[1,1] = "CHO20052019"
myORtable_CHO[1,2] = sum0[1,2] #sum[7] = OR
myORtable_CHO[1,3] = exp(coef0+(1.96*se0))
myORtable_CHO[1,4] = exp(coef0-(1.96*se0))
myORtable_CHO[2,1] = "CHO2020"
myORtable_CHO[2,2] = 0.944#sum1[1,2] #sum[7] = OR
myORtable_CHO[2,3] = 1.489#exp(coef1+(1.96*se1))
myORtable_CHO[2,4] = 0.599#exp(coef1-(1.96*se1))
colnames(myORtable_CHO) <- c("Model","OR","UpperCI","LowerCI")

#EMV

EMVF=read_excel("~/Desktop/EMV.Final.Weather.xlsx", sheet="Sheet1",na="NA")
EMV <- dplyr::select(EMVF, OzoneREVISED,PM2.5REVISED, Date, Mort,MDC.01,MDC.04,MDC.05,MDC.19,Non.billable.ICD,ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme,dow,holidays)
EMV$date=as.Date(EMV$Date)

EMVexc_Hot <- EMV %>%
  filter(month(Date) > 3 & month(Date) < 10)

EMV519exc_Hot <- EMV %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

EMV20exc_Hot <- EMV %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)



model0_EMV = casecross(Mort ~ HeatWavesModerate, stratalength = 7, exclusion = 2, data=EMV519exc_Hot)
model1_EMV = casecross(Mort ~ HeatWavesModerate, stratalength = 7, exclusion = 2, data=EMV20exc_Hot)


sum0 <- summary(model0_EMV)
coef0 = sum0[1]
se0 = sum0[3]

sum1 <- summary(model1_EMV)
coef1 = sum1[1]
se1 = sum1[3]


myORtable_EMV <- data.frame()
myORtable_EMV[1,1] = "EMV20052019"
myORtable_EMV[1,2] = sum0[1,2] #sum[3] = OR
myORtable_EMV[1,3] = exp(coef0+(1.96*se0))
myORtable_EMV[1,4] = exp(coef0-(1.96*se0))
myORtable_EMV[2,1] = "EMV2020"
myORtable_EMV[2,2] = sum1[1,2] #sum[3] = OR
myORtable_EMV[2,3] = exp(coef1+(1.96*se1))
myORtable_EMV[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable_EMV) <- c("Model","OR","UpperCI","LowerCI")

#SCARTCH

EZFF=read_excel("~/Desktop/EZF.Final.Weather.xlsx", sheet="Sheet1",na="NA")
EZF <- dplyr::select(EZFF, OzoneREVISED,PM2.5REVISED, Date, Mort,MDC.01,MDC.04,MDC.05,MDC.19,Non.billable.ICD,ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme,dow,holidays)
EZF$date=as.Date(EZF$Date)

EZFexc_Hot <- EZF %>%
  filter(month(Date) > 3 & month(Date) < 10)

EZF519exc_Hot <- EZF %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

EZF20exc_Hot <- EZF %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)



model0_EZF = casecross(Mort ~ HeatWavesModerate+OzoneREVISED, stratalength = 7, exclusion = 2, data=EZF519exc_Hot)
model1_EZF = casecross(Mort ~ HeatWavesModerate+OzoneREVISED, stratalength = 7, exclusion = 2, data=EZF20exc_Hot)


sum0 <- summary(model0_EZF)
coef0 = sum0[1]
se0 = sum0[5]

sum1 <- summary(model1_EZF)
coef1 = sum1[1]
se1 = sum1[5]


myORtable_EZF <- data.frame()
myORtable_EZF[1,1] = "EZF20052019"
myORtable_EZF[1,2] = sum0[1,2] #sum[5] = OR
myORtable_EZF[1,3] = exp(coef0+(1.96*se0))
myORtable_EZF[1,4] = exp(coef0-(1.96*se0))
myORtable_EZF[2,1] = "EZF2020"
myORtable_EZF[2,2] = sum1[1,2] #sum[5] = OR
myORtable_EZF[2,3] = exp(coef1+(1.96*se1))
myORtable_EZF[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable_EZF) <- c("Model","OR","UpperCI","LowerCI")

#IAD

#SCARTCH

IADF=read_excel("~/Desktop/IAD.Final.Weather.xlsx", sheet="Sheet1",na="NA")
IAD <- dplyr::select(IADF, OzoneREVISED,PM2.5REVISED, Date, Mort,MDC.01,MDC.04,MDC.05,MDC.19,Non.billable.ICD,ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme,dow,holidays)
IAD$date=as.Date(IAD$Date)

IADexc_Hot <- IAD %>%
  filter(month(Date) > 3 & month(Date) < 10)

IAD519exc_Hot <- IAD %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

IAD20exc_Hot <- IAD %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)



model0_IAD = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED+OzoneREVISED, stratalength = 7, exclusion = 2, data=IAD519exc_Hot)
model1_IAD = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED+OzoneREVISED, stratalength = 7, exclusion = 2, data=IAD20exc_Hot)


sum0 <- summary(model0_IAD)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1_IAD)
coef1 = sum1[1]
se1 = sum1[7]


myORtable_IAD <- data.frame()
myORtable_IAD[1,1] = "IAD20052019"
myORtable_IAD[1,2] = sum0[1,2] #sum[7] = OR
myORtable_IAD[1,3] = exp(coef0+(1.96*se0))
myORtable_IAD[1,4] = exp(coef0-(1.96*se0))
myORtable_IAD[2,1] = "IAD2020"
myORtable_IAD[2,2] = sum1[1,2] #sum[7] = OR
myORtable_IAD[2,3] = exp(coef1+(1.96*se1))
myORtable_IAD[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable_IAD) <- c("Model","OR","UpperCI","LowerCI")

#LYH

LYHF=read_excel("~/Desktop/LYH.Final.Weather.xlsx", sheet="Sheet1",na="NA")
LYH <- dplyr::select(LYHF, OzoneREVISED,PM2.5REVISED, Date, Mort,MDC.01,MDC.04,MDC.05,MDC.19,Non.billable.ICD,ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme,dow,holidays)
LYH$date=as.Date(LYH$Date)

LYHexc_Hot <- LYH %>%
  filter(month(Date) > 3 & month(Date) < 10)

LYH519exc_Hot <- LYH %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

LYH20exc_Hot <- LYH %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)



model0_LYH = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED, stratalength = 7, exclusion = 2, data=LYH519exc_Hot)
model1_LYH = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED, stratalength = 7, exclusion = 2, data=LYH20exc_Hot)


sum0 <- summary(model0_LYH)
coef0 = sum0[1]
se0 = sum0[5]

sum1 <- summary(model1_LYH)
coef1 = sum1[1]
se1 = sum1[5]


myORtable_LYH <- data.frame()
myORtable_LYH[1,1] = "LYH20052019"
myORtable_LYH[1,2] = sum0[1,2] #sum[5] = OR
myORtable_LYH[1,3] = exp(coef0+(1.96*se0))
myORtable_LYH[1,4] = exp(coef0-(1.96*se0))
myORtable_LYH[2,1] = "LYH2020"
myORtable_LYH[2,2] = sum1[1,2] #sum[5] = OR
myORtable_LYH[2,3] = exp(coef1+(1.96*se1))
myORtable_LYH[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable_LYH) <- c("Model","OR","UpperCI","LowerCI")

#OKV

OKVF=read_excel("~/Desktop/OKV.Final.Weather.xlsx", sheet="Sheet1",na="NA")
OKV <- dplyr::select(OKVF, OzoneREVISED,PM2.5REVISED, Date, Mort,MDC.01,MDC.04,MDC.05,MDC.19,Non.billable.ICD,ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme,dow,holidays)
OKV$date=as.Date(OKV$Date)

OKVexc_Hot <- OKV %>%
  filter(month(Date) > 3 & month(Date) < 10)

OKV519exc_Hot <- OKV %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

OKV20exc_Hot <- OKV %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)

model0_OKV = casecross(Mort ~ HeatWavesModerate, stratalength = 7, exclusion = 2, data=OKV519exc_Hot)
model1_OKV = casecross(Mort ~ HeatWavesModerate, stratalength = 7, exclusion = 2, data=OKV20exc_Hot)


sum0 <- summary(model0_OKV)
coef0 = sum0[1]
se0 = sum0[3]

sum1 <- summary(model1_OKV)
coef1 = sum1[1]
se1 = sum1[3]


myORtable_OKV <- data.frame()
myORtable_OKV[1,1] = "OKV20052019"
myORtable_OKV[1,2] = sum0[1,2] #sum[3] = OR
myORtable_OKV[1,3] = exp(coef0+(1.96*se0))
myORtable_OKV[1,4] = exp(coef0-(1.96*se0))
myORtable_OKV[2,1] = "OKV2020"
myORtable_OKV[2,2] = sum1[1,2] #sum[3] = OR
myORtable_OKV[2,3] = exp(coef1+(1.96*se1))
myORtable_OKV[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable_OKV) <- c("Model","OR","UpperCI","LowerCI")


ORFF=read_excel("~/Desktop/ORF.Final.Weather.xlsx", sheet="Sheet1",na="NA")
ORF <- dplyr::select(ORFF, OzoneREVISED,PM2.5REVISED, Date, Mort,MDC.01,MDC.04,MDC.05,MDC.19,Non.billable.ICD,ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme,dow,holidays)
ORF$date=as.Date(ORF$Date)

ORFexc_Hot <- ORF %>%
  filter(month(Date) > 3 & month(Date) < 10)

ORF519exc_Hot <- ORF %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

ORF20exc_Hot <- ORF %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)



model0_ORF = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED+OzoneREVISED, stratalength = 7, exclusion = 2, data=ORF519exc_Hot)
model1_ORF = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED+OzoneREVISED, stratalength = 7, exclusion = 2, data=ORF20exc_Hot)


sum0 <- summary(model0_ORF)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1_ORF)
coef1 = sum1[1]
se1 = sum1[7]


myORtable_ORF <- data.frame()
myORtable_ORF[1,1] = "ORF20052019"
myORtable_ORF[1,2] = sum0[1,2] #sum[7] = OR
myORtable_ORF[1,3] = exp(coef0+(1.96*se0))
myORtable_ORF[1,4] = exp(coef0-(1.96*se0))
myORtable_ORF[2,1] = "ORF2020"
myORtable_ORF[2,2] = sum1[1,2] #sum[7] = OR
myORtable_ORF[2,3] = exp(coef1+(1.96*se1))
myORtable_ORF[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable_ORF) <- c("Model","OR","UpperCI","LowerCI")

#SCARTCH

PHFF=read_excel("~/Desktop/PHF.Final.Weather.xlsx", sheet="Sheet1",na="NA")
PHF <- dplyr::select(PHFF, OzoneREVISED,PM2.5REVISED, Date, Mort,MDC.01,MDC.04,MDC.05,MDC.19,Non.billable.ICD,ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme,dow,holidays)
PHF$date=as.Date(PHF$Date)

PHFexc_Hot <- PHF %>%
  filter(month(Date) > 3 & month(Date) < 10)

PHF519exc_Hot <- PHF %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

PHF20exc_Hot <- PHF %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)



model0_PHF = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED+OzoneREVISED, stratalength = 7, exclusion = 2, data=PHF519exc_Hot)
model1_PHF = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED+OzoneREVISED, stratalength = 7, exclusion = 2, data=PHF20exc_Hot)


sum0 <- summary(model0_PHF)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1_PHF)
coef1 = sum1[1]
se1 = sum1[7]


myORtable_PHF <- data.frame()
myORtable_PHF[1,1] = "PHF20052019"
myORtable_PHF[1,2] = sum0[1,2] #sum[7] = OR
myORtable_PHF[1,3] = exp(coef0+(1.96*se0))
myORtable_PHF[1,4] = exp(coef0-(1.96*se0))
myORtable_PHF[2,1] = "PHF2020"
myORtable_PHF[2,2] = sum1[1,2] #sum[7] = OR
myORtable_PHF[2,3] = exp(coef1+(1.96*se1))
myORtable_PHF[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable_PHF) <- c("Model","OR","UpperCI","LowerCI")

RICF=read_excel("~/Desktop/RIC.Final.Weather.xlsx", sheet="Sheet1",na="NA")
RIC <- dplyr::select(RICF, OzoneREVISED,PM2.5REVISED, Date, Mort,MDC.01,MDC.04,MDC.05,MDC.19,Non.billable.ICD,ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme,dow,holidays)
RIC$date=as.Date(RIC$Date)

RICexc_Hot <- RIC %>%
  filter(month(Date) > 3 & month(Date) < 10)

RIC519exc_Hot <- RIC %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

RIC20exc_Hot <- RIC %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)



model0_RIC = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED+OzoneREVISED, stratalength = 7, exclusion = 2, data=RIC519exc_Hot)
model1_RIC = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED+OzoneREVISED, stratalength = 7, exclusion = 2, data=RIC20exc_Hot)


sum0 <- summary(model0_RIC)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1_RIC)
coef1 = sum1[1]
se1 = sum1[7]


myORtable_RIC <- data.frame()
myORtable_RIC[1,1] = "RIC20052019"
myORtable_RIC[1,2] = sum0[1,2] #sum[7] = OR
myORtable_RIC[1,3] = exp(coef0+(1.96*se0))
myORtable_RIC[1,4] = exp(coef0-(1.96*se0))
myORtable_RIC[2,1] = "RIC2020"
myORtable_RIC[2,2] = sum1[1,2] #sum[7] = OR
myORtable_RIC[2,3] = exp(coef1+(1.96*se1))
myORtable_RIC[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable_RIC) <- c("Model","OR","UpperCI","LowerCI")

#ROA

ROAF=read_excel("~/Desktop/ROA.Final.Weather.xlsx", sheet="Sheet1",na="NA")
ROA <- dplyr::select(ROAF, OzoneREVISED,PM2.5REVISED, Date, Mort,MDC.01,MDC.04,MDC.05,MDC.19,Non.billable.ICD,ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme,dow,holidays)
ROA$date=as.Date(ROA$Date)

ROAexc_Hot <- ROA %>%
  filter(month(Date) > 3 & month(Date) < 10)

ROA519exc_Hot <- ROA %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

ROA20exc_Hot <- ROA %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)



model0_ROA = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED+OzoneREVISED, stratalength = 7, exclusion = 2, data=ROA519exc_Hot)
model1_ROA = casecross(Mort ~ HeatWavesModerate+PM2.5REVISED+OzoneREVISED, stratalength = 7, exclusion = 2, data=ROA20exc_Hot)


sum0 <- summary(model0_ROA)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1_ROA)
coef1 = sum1[1]
se1 = sum1[7]


myORtable_ROA <- data.frame()
myORtable_ROA[1,1] = "ROA20052019"
myORtable_ROA[1,2] = sum0[1,2] #sum[7] = OR
myORtable_ROA[1,3] = exp(coef0+(1.96*se0))
myORtable_ROA[1,4] = exp(coef0-(1.96*se0))
myORtable_ROA[2,1] = "ROA2020"
myORtable_ROA[2,2] = sum1[1,2] #sum[7] = OR
myORtable_ROA[2,3] = exp(coef1+(1.96*se1))
myORtable_ROA[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable_ROA) <- c("Model","OR","UpperCI","LowerCI")

#SHD

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



model0_SHD = casecross(Mort ~ HeatWavesModerate, stratalength = 7, exclusion = 2, data=SHD519exc_Hot)
model1_SHD = casecross(Mort ~ HeatWavesModerate, stratalength = 7, exclusion = 2, data=SHD20exc_Hot)


sum0 <- summary(model0_SHD)
coef0 = sum0[1]
se0 = sum0[3]

sum1 <- summary(model1_SHD)
coef1 = sum1[1]
se1 = sum1[3]


myORtable_SHD <- data.frame()
myORtable_SHD[1,1] = "SHD20052019"
myORtable_SHD[1,2] = sum0[1,2] #sum[3] = OR
myORtable_SHD[1,3] = exp(coef0+(1.96*se0))
myORtable_SHD[1,4] = exp(coef0-(1.96*se0))
myORtable_SHD[2,1] = "SHD2020"
myORtable_SHD[2,2] = sum1[1,2] #sum[3] = OR
myORtable_SHD[2,3] = exp(coef1+(1.96*se1))
myORtable_SHD[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable_SHD) <- c("Model","OR","UpperCI","LowerCI")

#SCARTCH

VJIF=read_excel("~/Desktop/VJI.Final.Weather.xlsx", sheet="Sheet1",na="NA")
VJI <- dplyr::select(VJIF, OzoneREVISED,PM2.5REVISED, Date, Mort,MDC.01,MDC.04,MDC.05,MDC.19,Non.billable.ICD,ColdWavesModerate,ColdWavesSevere,ColdWavesExtreme,ColdDaysModerate,ColdDaysSevere,ColdDaysExtreme,
                     HeatWavesModerate,HeatWavesSevere,HeatWavesExtreme,HotDaysModerate,HotDaysSevere,HotDaysExtreme,dow,holidays)
VJI$date=as.Date(VJI$Date)

VJIexc_Hot <- VJI %>%
  filter(month(Date) > 3 & month(Date) < 10)

VJI519exc_Hot <- VJI %>%
  filter(Date <= "2019-12-31" )%>%
  filter(month(Date) > 3 & month(Date) < 10)

VJI20exc_Hot <- VJI %>%
  filter(Date >= "2020-01-01")%>%
  filter(month(Date) > 3 & month(Date) < 10)



model0_VJI = casecross(Mort ~ HeatWavesModerate, stratalength = 7, exclusion = 2, data=VJI519exc_Hot)
model1_VJI = casecross(Mort ~ HeatWavesModerate, stratalength = 7, exclusion = 2, data=VJI20exc_Hot)


sum0 <- summary(model0_VJI)
coef0 = sum0[1]
se0 = sum0[3]

sum1 <- summary(model1_VJI)
coef1 = sum1[1]
se1 = sum1[3]


myORtable_VJI <- data.frame()
myORtable_VJI[1,1] = "VJI20052019"
myORtable_VJI[1,2] = sum0[1,2] #sum[3] = OR
myORtable_VJI[1,3] = exp(coef0+(1.96*se0))
myORtable_VJI[1,4] = exp(coef0-(1.96*se0))
myORtable_VJI[2,1] = "VJI2020"
myORtable_VJI[2,2] = sum1[1,2] #sum[3] = OR
myORtable_VJI[2,3] = exp(coef1+(1.96*se1))
myORtable_VJI[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable_VJI) <- c("Model","OR","UpperCI","LowerCI")

myORtable_Master <- rbind(myORtable_CHO, myORtable_EMV, myORtable_EZF, myORtable_IAD, myORtable_LYH, myORtable_OKV, myORtable_ORF, myORtable_PHF, myORtable_RIC, myORtable_ROA, myORtable_SHD, myORtable_VJI)

myORtable_Master$index <- seq(c("NoCovid","Covid"))
errorColors <- ifelse(myORtable_Master$index==1,'blue',ifelse(myORtable_Master$index==2,'red',NA))

myORtable_Master$index <- c("NoCovidLite","CovidLite","NoCovidLite","CovidLite","NoCovid","Covid","NoCovid","Covid","NoCovid","Covid","NoCovid","Covid","NoCovid","Covid","NoCovid","Covid","NoCovid","Covid","NoCovidLite","CovidLite","NoCovid","Covid","NoCovid","Covid")
errorColors <- ifelse(myORtable_Master$index=="NoCovidLite",'lightskyblue4',ifelse(myORtable_Master$index=="CovidLite",'lightsalmon',ifelse(myORtable_Master$index=="NoCovid",'blue',ifelse(myORtable_Master$index=="Covid",'firebrick1',NA))))

myORtable_Master$anotherindex <- c("ColorLite","ColorLite","ColorLite","ColorLite","Color","Color","Color","Color","Color","Color","Color","Color","Color","Color","Color","Color","Color","Color","ColorLite","ColorLite","Color","Color","Color","Color")

ggplot(myORtable_Master) +
  geom_point(aes(x=Model, y=myORtable_Master[,2]), stat="identity", color=errorColors, alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable_Master[,3], ymax=myORtable_Master[,4]), width=0.4, color=errorColors, alpha=0.5, size=1.0)+
  labs(
    title = "<span style = 'color:blue;'>Pre <span style = 'color:black;'>/ <span style = 'color:red;'>Post <span style = 'color:black;'>Covid Heat Wave Mortality Odds Ratios",
    x = "Region",
    y = "Odds Ratio"
  ) +
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))+
  theme(plot.title = element_markdown(size = 22))+
  theme(axis.text.x=element_blank(),axis.ticks.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(panel.grid = element_blank(),
        plot.title = element_markdown())+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.4,2.25)+
  geom_vline(xintercept=c(2.5,4.5,6.5,8.5,10.5,12.5,14.5,16.5,18.5,20.5,22.5),linetype=2) +
  scale_color_manual(breaks=c(1,2),values='blue','red')+
  annotate("text", x=1.5, y=2.2, label= "CHO", size = 5, alpha=0.5)+
  annotate("text", x=3.5, y=2.2, label= "EMV", size = 5, alpha=0.5)+
  annotate("text", x=5.5, y=2.2, label= "EZF", size = 5,fontface="bold")+  
  annotate("text", x=7.5, y=2.2, label= "IAD", size = 5,fontface="bold")+
  annotate("text", x=9.5, y=2.2, label= "LYH", size = 5,fontface="bold")+
  annotate("text", x=11.5, y=2.2, label= "OKV", size = 5,fontface="bold")+
  annotate("text", x=13.5, y=2.2, label= "ORF", size = 5,fontface="bold")+
  annotate("text", x=15.5, y=2.2, label= "PHF", size = 5,fontface="bold")+
  annotate("text", x=17.5, y=2.2, label= "RIC", size = 5,fontface="bold")+  
  annotate("text", x=19.5, y=2.2, label= "ROA", size = 5, alpha=0.5)+
  annotate("text", x=21.5, y=2.2, label= "SHD", size = 5,fontface="bold")+
  annotate("text", x=23.5, y=2.2, label= "VJI", size = 5,fontface="bold")
