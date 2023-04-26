#casecross loop_v1

library(season)
library(readxl)
library(lubridate)
library(ggplot2)
library(ggpubr)

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

myORtable0_HeatWavesModerate <- data.frame()
myORtable1_HeatWavesModerate <- data.frame()
myORtable2_HeatWavesModerate <- data.frame()

for (i in (c(7,14,21,28))) {
  for (j in 0:2) {
    model0_HeatWavesModerate = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = j, data=IADexc_Hot)
    model1_HeatWavesModerate = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = j, data=IAD519exc_Hot)
    model2_HeatWavesModerate = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = j, data=IAD20exc_Hot)
    
    sum0_HeatWavesModerate <- summary(model0_HeatWavesModerate)
    coef0_HeatWavesModerate = sum0_HeatWavesModerate[1]
    se0_HeatWavesModerate = sum0_HeatWavesModerate[7]
    
    sum1_HeatWavesModerate <- summary(model1_HeatWavesModerate)
    coef1_HeatWavesModerate = sum1_HeatWavesModerate[1]
    se1_HeatWavesModerate = sum1_HeatWavesModerate[7]
    
    sum2_HeatWavesModerate <- summary(model2_HeatWavesModerate)
    coef2_HeatWavesModerate = sum2_HeatWavesModerate[1]
    se2_HeatWavesModerate = sum2_HeatWavesModerate[7]

    myORtable0_HeatWavesModerate[i+j,1] = 0
    myORtable0_HeatWavesModerate[i+j,2] = sum0_HeatWavesModerate[1,2] #sum[3] = OR
    myORtable0_HeatWavesModerate[i+j,3] = exp(coef0_HeatWavesModerate+(1.96*se0_HeatWavesModerate))
    myORtable0_HeatWavesModerate[i+j,4] = exp(coef0_HeatWavesModerate-(1.96*se0_HeatWavesModerate))
    colnames(myORtable0_HeatWavesModerate) <- c("Model","OR","UpperCI","LowerCI")    
    
    myORtable1_HeatWavesModerate[i+j,1] = 1
    myORtable1_HeatWavesModerate[i+j,2] = sum1_HeatWavesModerate[1,2] #sum[3] = OR
    myORtable1_HeatWavesModerate[i+j,3] = exp(coef1_HeatWavesModerate+(1.96*se1_HeatWavesModerate))
    myORtable1_HeatWavesModerate[i+j,4] = exp(coef1_HeatWavesModerate-(1.96*se1_HeatWavesModerate))
    colnames(myORtable1_HeatWavesModerate) <- c("Model","OR","UpperCI","LowerCI")
    
    myORtable2_HeatWavesModerate[i+j,1] = 2
    myORtable2_HeatWavesModerate[i+j,2] = sum2_HeatWavesModerate[1,2] #sum[3] = OR
    myORtable2_HeatWavesModerate[i+j,3] = exp(coef2_HeatWavesModerate+(1.96*se2_HeatWavesModerate))
    myORtable2_HeatWavesModerate[i+j,4] = exp(coef2_HeatWavesModerate-(1.96*se2_HeatWavesModerate))
    colnames(myORtable2_HeatWavesModerate) <- c("Model","OR","UpperCI","LowerCI")
  }
}

myORtable0_HeatWavesModerate_complete <- myORtable0_HeatWavesModerate[complete.cases(myORtable0_HeatWavesModerate), ]
myORtable0_HeatWavesModerate_complete[,1] <- seq(1:length(myORtable0_HeatWavesModerate_complete$Model))
colnames(myORtable0_HeatWavesModerate_complete) <- c("Model","OR","UpperCI","LowerCI")
rownames(myORtable0_HeatWavesModerate_complete) <- c("strata=7,exc=0","strata=7,exc=1","strata=7,exc=2","strata=14,exc=0","strata=14,exc=1","strata=14,exc=2","strata=21,exc=0","strata=21,exc=1","strata=21,exc=2","strata=28,exc=0","strata=28,exc=1","strata=28,exc=2")

ggplot(myORtable0_HeatWavesModerate_complete) +
  geom_line(aes(x=Model, y=myORtable0_HeatWavesModerate_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=Model, y=myORtable0_HeatWavesModerate_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable0_HeatWavesModerate_complete[,3], ymax=myORtable0_HeatWavesModerate_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD Mort HeatWavesModerate",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red") +
  geom_vline(xintercept=3.5, linetype="dashed", color = "black") +
  geom_vline(xintercept=6.5, linetype="dashed", color = "black") +
  geom_vline(xintercept=9.5, linetype="dashed", color = "black") +
  annotate(geom="text", x=2, y=1.2, label="strata=7",color="black") +
  annotate(geom="text", x=5, y=1.2, label="strata=14",color="black") +
  annotate(geom="text", x=8, y=1.2, label="strata=21",color="black") +
  annotate(geom="text", x=11, y=1.2, label="strata=28",color="black")

myORtable0_HeatWavesSevere <- data.frame()
myORtable1_HeatWavesSevere <- data.frame()
myORtable2_HeatWavesSevere <- data.frame()

for (i in (c(7,14,21,28))) {
  for (j in 0:2) {
    model0_HeatWavesSevere = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = j, data=IADexc_Hot)
    model1_HeatWavesSevere = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = j, data=IAD519exc_Hot)
    model2_HeatWavesSevere = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = j, data=IAD20exc_Hot)
    
    sum0_HeatWavesSevere <- summary(model0_HeatWavesSevere)
    coef0_HeatWavesSevere = sum0_HeatWavesSevere[1]
    se0_HeatWavesSevere = sum0_HeatWavesSevere[7]
    
    sum1_HeatWavesSevere <- summary(model1_HeatWavesSevere)
    coef1_HeatWavesSevere = sum1_HeatWavesSevere[1]
    se1_HeatWavesSevere = sum1_HeatWavesSevere[7]
    
    sum2_HeatWavesSevere <- summary(model2_HeatWavesSevere)
    coef2_HeatWavesSevere = sum2_HeatWavesSevere[1]
    se2_HeatWavesSevere = sum2_HeatWavesSevere[7]
    
    myORtable0_HeatWavesSevere[i+j,1] = 0
    myORtable0_HeatWavesSevere[i+j,2] = sum0_HeatWavesSevere[1,2] #sum[3] = OR
    myORtable0_HeatWavesSevere[i+j,3] = exp(coef0_HeatWavesSevere+(1.96*se0_HeatWavesSevere))
    myORtable0_HeatWavesSevere[i+j,4] = exp(coef0_HeatWavesSevere-(1.96*se0_HeatWavesSevere))
    colnames(myORtable0_HeatWavesSevere) <- c("Model","OR","UpperCI","LowerCI")    
    
    myORtable1_HeatWavesSevere[i+j,1] = 1
    myORtable1_HeatWavesSevere[i+j,2] = sum1_HeatWavesSevere[1,2] #sum[3] = OR
    myORtable1_HeatWavesSevere[i+j,3] = exp(coef1_HeatWavesSevere+(1.96*se1_HeatWavesSevere))
    myORtable1_HeatWavesSevere[i+j,4] = exp(coef1_HeatWavesSevere-(1.96*se1_HeatWavesSevere))
    colnames(myORtable1_HeatWavesSevere) <- c("Model","OR","UpperCI","LowerCI")
    
    myORtable2_HeatWavesSevere[i+j,1] = 2
    myORtable2_HeatWavesSevere[i+j,2] = sum2_HeatWavesSevere[1,2] #sum[3] = OR
    myORtable2_HeatWavesSevere[i+j,3] = exp(coef2_HeatWavesSevere+(1.96*se2_HeatWavesSevere))
    myORtable2_HeatWavesSevere[i+j,4] = exp(coef2_HeatWavesSevere-(1.96*se2_HeatWavesSevere))
    colnames(myORtable2_HeatWavesSevere) <- c("Model","OR","UpperCI","LowerCI")
  }
}

myORtable0_HeatWavesSevere_complete <- myORtable0_HeatWavesSevere[complete.cases(myORtable0_HeatWavesSevere), ]
myORtable0_HeatWavesSevere_complete[,1] <- seq(1:length(myORtable0_HeatWavesSevere_complete$Model))
colnames(myORtable0_HeatWavesSevere_complete) <- c("Model","OR","UpperCI","LowerCI")
rownames(myORtable0_HeatWavesSevere_complete) <- c("strata=7,exc=0","strata=7,exc=1","strata=7,exc=2","strata=14,exc=0","strata=14,exc=1","strata=14,exc=2","strata=21,exc=0","strata=21,exc=1","strata=21,exc=2","strata=28,exc=0","strata=28,exc=1","strata=28,exc=2")

ggplot(myORtable0_HeatWavesSevere_complete) +
  geom_point(aes(x=Model, y=myORtable0_HeatWavesSevere_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable0_HeatWavesSevere_complete[,3], ymax=myORtable0_HeatWavesSevere_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD Mort HeatWavesSevere",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red") +
  geom_vline(xintercept=3.5, linetype="dashed", color = "black") +
  geom_vline(xintercept=6.5, linetype="dashed", color = "black") +
  geom_vline(xintercept=9.5, linetype="dashed", color = "black") +
  annotate(geom="text", x=2, y=1.2, label="strata=7",color="black") +
  annotate(geom="text", x=5, y=1.2, label="strata=14",color="black") +
  annotate(geom="text", x=8, y=1.2, label="strata=21",color="black") +
  annotate(geom="text", x=11, y=1.2, label="strata=28",color="black")


myORtable0_HeatWavesExtreme <- data.frame()
myORtable1_HeatWavesExtreme <- data.frame()
myORtable2_HeatWavesExtreme <- data.frame()

for (i in (c(7,14,21,28))) {
  for (j in 0:2) {
    model0_HeatWavesExtreme = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = j, data=IADexc_Hot)
    model1_HeatWavesExtreme = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = j, data=IAD519exc_Hot)
    model2_HeatWavesExtreme = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = j, data=IAD20exc_Hot)
    
    sum0_HeatWavesExtreme <- summary(model0_HeatWavesExtreme)
    coef0_HeatWavesExtreme = sum0_HeatWavesExtreme[1]
    se0_HeatWavesExtreme = sum0_HeatWavesExtreme[7]
    
    sum1_HeatWavesExtreme <- summary(model1_HeatWavesExtreme)
    coef1_HeatWavesExtreme = sum1_HeatWavesExtreme[1]
    se1_HeatWavesExtreme = sum1_HeatWavesExtreme[7]
    
    sum2_HeatWavesExtreme <- summary(model2_HeatWavesExtreme)
    coef2_HeatWavesExtreme = sum2_HeatWavesExtreme[1]
    se2_HeatWavesExtreme = sum2_HeatWavesExtreme[7]
    
    myORtable0_HeatWavesExtreme[i+j,1] = 0
    myORtable0_HeatWavesExtreme[i+j,2] = sum0_HeatWavesExtreme[1,2] #sum[3] = OR
    myORtable0_HeatWavesExtreme[i+j,3] = exp(coef0_HeatWavesExtreme+(1.96*se0_HeatWavesExtreme))
    myORtable0_HeatWavesExtreme[i+j,4] = exp(coef0_HeatWavesExtreme-(1.96*se0_HeatWavesExtreme))
    colnames(myORtable0_HeatWavesExtreme) <- c("Model","OR","UpperCI","LowerCI")    
    
    myORtable1_HeatWavesExtreme[i+j,1] = 1
    myORtable1_HeatWavesExtreme[i+j,2] = sum1_HeatWavesExtreme[1,2] #sum[3] = OR
    myORtable1_HeatWavesExtreme[i+j,3] = exp(coef1_HeatWavesExtreme+(1.96*se1_HeatWavesExtreme))
    myORtable1_HeatWavesExtreme[i+j,4] = exp(coef1_HeatWavesExtreme-(1.96*se1_HeatWavesExtreme))
    colnames(myORtable1_HeatWavesExtreme) <- c("Model","OR","UpperCI","LowerCI")
    
    myORtable2_HeatWavesExtreme[i+j,1] = 2
    myORtable2_HeatWavesExtreme[i+j,2] = sum2_HeatWavesExtreme[1,2] #sum[3] = OR
    myORtable2_HeatWavesExtreme[i+j,3] = exp(coef2_HeatWavesExtreme+(1.96*se2_HeatWavesExtreme))
    myORtable2_HeatWavesExtreme[i+j,4] = exp(coef2_HeatWavesExtreme-(1.96*se2_HeatWavesExtreme))
    colnames(myORtable2_HeatWavesExtreme) <- c("Model","OR","UpperCI","LowerCI")
  }
}

myORtable0_HeatWavesExtreme_complete <- myORtable0_HeatWavesExtreme[complete.cases(myORtable0_HeatWavesExtreme), ]
myORtable0_HeatWavesExtreme_complete[,1] <- seq(1:length(myORtable0_HeatWavesExtreme_complete$Model))
colnames(myORtable0_HeatWavesExtreme_complete) <- c("Model","OR","UpperCI","LowerCI")
rownames(myORtable0_HeatWavesExtreme_complete) <- c("strata=7,exc=0","strata=7,exc=1","strata=7,exc=2","strata=14,exc=0","strata=14,exc=1","strata=14,exc=2","strata=21,exc=0","strata=21,exc=1","strata=21,exc=2","strata=28,exc=0","strata=28,exc=1","strata=28,exc=2")

ggplot(myORtable0_HeatWavesExtreme_complete) +
  geom_point(aes(x=Model, y=myORtable0_HeatWavesExtreme_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable0_HeatWavesExtreme_complete[,3], ymax=myORtable0_HeatWavesExtreme_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="IAD Mort HeatWavesExtreme",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red") +
  geom_vline(xintercept=3.5, linetype="dashed", color = "black") +
  geom_vline(xintercept=6.5, linetype="dashed", color = "black") +
  geom_vline(xintercept=9.5, linetype="dashed", color = "black") +
  annotate(geom="text", x=2, y=1.2, label="strata=7",color="black") +
  annotate(geom="text", x=5, y=1.2, label="strata=14",color="black") +
  annotate(geom="text", x=8, y=1.2, label="strata=21",color="black") +
  annotate(geom="text", x=11, y=1.2, label="strata=28",color="black")



