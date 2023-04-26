#casecross loop_v2 with both AQ values

library(season)
library(readxl)
library(lubridate)
library(ggplot2)
library(ggpubr)

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

myORtable0_HeatWavesModerate <- data.frame()
myORtable1_HeatWavesModerate <- data.frame()
myORtable2_HeatWavesModerate <- data.frame()

myORtable0_HeatWavesModerate_exc0 <- data.frame()
myORtable1_HeatWavesModerate_exc0 <- data.frame()
myORtable2_HeatWavesModerate_exc0 <- data.frame()

for (i in (c(7:28))) {
    model0_HeatWavesModerate_exc0 = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 0, data=PHFexc_Hot)
    model1_HeatWavesModerate_exc0 = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 0, data=PHF519exc_Hot)
    model2_HeatWavesModerate_exc0 = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 0, data=PHF20exc_Hot)
    
    sum0_HeatWavesModerate_exc0 <- summary(model0_HeatWavesModerate_exc0)
    coef0_HeatWavesModerate_exc0 = sum0_HeatWavesModerate_exc0[1]
    se0_HeatWavesModerate_exc0 = sum0_HeatWavesModerate_exc0[7]
    
    sum1_HeatWavesModerate_exc0 <- summary(model1_HeatWavesModerate_exc0)
    coef1_HeatWavesModerate_exc0 = sum1_HeatWavesModerate_exc0[1]
    se1_HeatWavesModerate_exc0 = sum1_HeatWavesModerate_exc0[7]
    
    sum2_HeatWavesModerate_exc0 <- summary(model2_HeatWavesModerate_exc0)
    coef2_HeatWavesModerate_exc0 = sum2_HeatWavesModerate_exc0[1]
    se2_HeatWavesModerate_exc0 = sum2_HeatWavesModerate_exc0[7]
    
    myORtable0_HeatWavesModerate_exc0[i,1] = 0
    myORtable0_HeatWavesModerate_exc0[i,2] = sum0_HeatWavesModerate_exc0[1,2] #sum[3] = OR
    myORtable0_HeatWavesModerate_exc0[i,3] = exp(coef0_HeatWavesModerate_exc0+(1.96*se0_HeatWavesModerate_exc0))
    myORtable0_HeatWavesModerate_exc0[i,4] = exp(coef0_HeatWavesModerate_exc0-(1.96*se0_HeatWavesModerate_exc0))
    colnames(myORtable0_HeatWavesModerate_exc0) <- c("Model","OR","UpperCI","LowerCI")    
    
    myORtable1_HeatWavesModerate_exc0[i,1] = 1
    myORtable1_HeatWavesModerate_exc0[i,2] = sum1_HeatWavesModerate_exc0[1,2] #sum[3] = OR
    myORtable1_HeatWavesModerate_exc0[i,3] = exp(coef1_HeatWavesModerate_exc0+(1.96*se1_HeatWavesModerate_exc0))
    myORtable1_HeatWavesModerate_exc0[i,4] = exp(coef1_HeatWavesModerate_exc0-(1.96*se1_HeatWavesModerate_exc0))
    colnames(myORtable1_HeatWavesModerate_exc0) <- c("Model","OR","UpperCI","LowerCI")
    
    myORtable2_HeatWavesModerate_exc0[i,1] = 2
    myORtable2_HeatWavesModerate_exc0[i,2] = sum2_HeatWavesModerate_exc0[1,2] #sum[3] = OR
    myORtable2_HeatWavesModerate_exc0[i,3] = exp(coef2_HeatWavesModerate_exc0+(1.96*se2_HeatWavesModerate_exc0))
    myORtable2_HeatWavesModerate_exc0[i,4] = exp(coef2_HeatWavesModerate_exc0-(1.96*se2_HeatWavesModerate_exc0))
    colnames(myORtable2_HeatWavesModerate_exc0) <- c("Model","OR","UpperCI","LowerCI")
}

#Full Mort

myORtable0_HeatWavesModerate_exc0_complete <- myORtable0_HeatWavesModerate_exc0[complete.cases(myORtable0_HeatWavesModerate_exc0), ]
myORtable0_HeatWavesModerate_exc0_complete[,1] <- seq(1:length(myORtable0_HeatWavesModerate_exc0_complete$Model))
colnames(myORtable0_HeatWavesModerate_exc0_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable0_HeatWavesModerate_exc0_complete$StrataLength = myORtable0_HeatWavesModerate_exc0_complete$StrataLength + 6

Full_exc0 <- ggplot(myORtable0_HeatWavesModerate_exc0_complete) +
  geom_line(aes(x=StrataLength, y=myORtable0_HeatWavesModerate_exc0_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable0_HeatWavesModerate_exc0_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable0_HeatWavesModerate_exc0_complete[,3], ymax=myORtable0_HeatWavesModerate_exc0_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF Full Mort HeatWavesModerate exc=0",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2005-2019 Data

myORtable1_HeatWavesModerate_exc0_complete <- myORtable1_HeatWavesModerate_exc0[complete.cases(myORtable1_HeatWavesModerate_exc0), ]
myORtable1_HeatWavesModerate_exc0_complete[,1] <- seq(1:length(myORtable1_HeatWavesModerate_exc0_complete$Model))
colnames(myORtable1_HeatWavesModerate_exc0_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable1_HeatWavesModerate_exc0_complete$StrataLength = myORtable1_HeatWavesModerate_exc0_complete$StrataLength + 6

Big_exc0 <- ggplot(myORtable1_HeatWavesModerate_exc0_complete) +
  geom_line(aes(x=StrataLength, y=myORtable1_HeatWavesModerate_exc0_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable1_HeatWavesModerate_exc0_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable1_HeatWavesModerate_exc0_complete[,3], ymax=myORtable1_HeatWavesModerate_exc0_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2005-2019 Mort HeatWavesModerate exc=0",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2020 Data

myORtable2_HeatWavesModerate_exc0_complete <- myORtable2_HeatWavesModerate_exc0[complete.cases(myORtable2_HeatWavesModerate_exc0), ]
myORtable2_HeatWavesModerate_exc0_complete[,1] <- seq(1:length(myORtable2_HeatWavesModerate_exc0_complete$Model))
colnames(myORtable2_HeatWavesModerate_exc0_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable2_HeatWavesModerate_exc0_complete$StrataLength = myORtable2_HeatWavesModerate_exc0_complete$StrataLength + 6

Little_exc0 <- ggplot(myORtable2_HeatWavesModerate_exc0_complete) +
  geom_line(aes(x=StrataLength, y=myORtable2_HeatWavesModerate_exc0_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable2_HeatWavesModerate_exc0_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable2_HeatWavesModerate_exc0_complete[,3], ymax=myORtable2_HeatWavesModerate_exc0_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2020 Mort HeatWavesModerate exc=0",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

myORtable0_HeatWavesModerate__exc1 <- data.frame()
myORtable1_HeatWavesModerate__exc1 <- data.frame()
myORtable2_HeatWavesModerate__exc1 <- data.frame()

for (i in (c(7:28))) {
  model0_HeatWavesModerate__exc1 = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 1, data=PHFexc_Hot)
  model1_HeatWavesModerate__exc1 = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 1, data=PHF519exc_Hot)
  model2_HeatWavesModerate__exc1 = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 1, data=PHF20exc_Hot)
  
  sum0_HeatWavesModerate__exc1 <- summary(model0_HeatWavesModerate__exc1)
  coef0_HeatWavesModerate__exc1 = sum0_HeatWavesModerate__exc1[1]
  se0_HeatWavesModerate__exc1 = sum0_HeatWavesModerate__exc1[7]
  
  sum1_HeatWavesModerate__exc1 <- summary(model1_HeatWavesModerate__exc1)
  coef1_HeatWavesModerate__exc1 = sum1_HeatWavesModerate__exc1[1]
  se1_HeatWavesModerate__exc1 = sum1_HeatWavesModerate__exc1[7]
  
  sum2_HeatWavesModerate__exc1 <- summary(model2_HeatWavesModerate__exc1)
  coef2_HeatWavesModerate__exc1 = sum2_HeatWavesModerate__exc1[1]
  se2_HeatWavesModerate__exc1 = sum2_HeatWavesModerate__exc1[7]
  
  myORtable0_HeatWavesModerate__exc1[i,1] = 0
  myORtable0_HeatWavesModerate__exc1[i,2] = sum0_HeatWavesModerate__exc1[1,2] #sum[3] = OR
  myORtable0_HeatWavesModerate__exc1[i,3] = exp(coef0_HeatWavesModerate__exc1+(1.96*se0_HeatWavesModerate__exc1))
  myORtable0_HeatWavesModerate__exc1[i,4] = exp(coef0_HeatWavesModerate__exc1-(1.96*se0_HeatWavesModerate__exc1))
  colnames(myORtable0_HeatWavesModerate__exc1) <- c("Model","OR","UpperCI","LowerCI")    
  
  myORtable1_HeatWavesModerate__exc1[i,1] = 1
  myORtable1_HeatWavesModerate__exc1[i,2] = sum1_HeatWavesModerate__exc1[1,2] #sum[3] = OR
  myORtable1_HeatWavesModerate__exc1[i,3] = exp(coef1_HeatWavesModerate__exc1+(1.96*se1_HeatWavesModerate__exc1))
  myORtable1_HeatWavesModerate__exc1[i,4] = exp(coef1_HeatWavesModerate__exc1-(1.96*se1_HeatWavesModerate__exc1))
  colnames(myORtable1_HeatWavesModerate__exc1) <- c("Model","OR","UpperCI","LowerCI")
  
  myORtable2_HeatWavesModerate__exc1[i,1] = 2
  myORtable2_HeatWavesModerate__exc1[i,2] = sum2_HeatWavesModerate__exc1[1,2] #sum[3] = OR
  myORtable2_HeatWavesModerate__exc1[i,3] = exp(coef2_HeatWavesModerate__exc1+(1.96*se2_HeatWavesModerate__exc1))
  myORtable2_HeatWavesModerate__exc1[i,4] = exp(coef2_HeatWavesModerate__exc1-(1.96*se2_HeatWavesModerate__exc1))
  colnames(myORtable2_HeatWavesModerate__exc1) <- c("Model","OR","UpperCI","LowerCI")
}

#Full Mort

myORtable0_HeatWavesModerate__exc1_complete <- myORtable0_HeatWavesModerate__exc1[complete.cases(myORtable0_HeatWavesModerate__exc1), ]
myORtable0_HeatWavesModerate__exc1_complete[,1] <- seq(1:length(myORtable0_HeatWavesModerate__exc1_complete$Model))
colnames(myORtable0_HeatWavesModerate__exc1_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable0_HeatWavesModerate__exc1_complete$StrataLength = myORtable0_HeatWavesModerate__exc1_complete$StrataLength + 6

Full_exc1 <- ggplot(myORtable0_HeatWavesModerate__exc1_complete) +
  geom_line(aes(x=StrataLength, y=myORtable0_HeatWavesModerate__exc1_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable0_HeatWavesModerate__exc1_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable0_HeatWavesModerate__exc1_complete[,3], ymax=myORtable0_HeatWavesModerate__exc1_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF Full Mort HeatWavesModerate exc=1",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2005-2019 Data

myORtable1_HeatWavesModerate__exc1_complete <- myORtable1_HeatWavesModerate__exc1[complete.cases(myORtable1_HeatWavesModerate__exc1), ]
myORtable1_HeatWavesModerate__exc1_complete[,1] <- seq(1:length(myORtable1_HeatWavesModerate__exc1_complete$Model))
colnames(myORtable1_HeatWavesModerate__exc1_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable1_HeatWavesModerate__exc1_complete$StrataLength = myORtable1_HeatWavesModerate__exc1_complete$StrataLength + 6

Big_exc1 <- ggplot(myORtable1_HeatWavesModerate__exc1_complete) +
  geom_line(aes(x=StrataLength, y=myORtable1_HeatWavesModerate__exc1_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable1_HeatWavesModerate__exc1_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable1_HeatWavesModerate__exc1_complete[,3], ymax=myORtable1_HeatWavesModerate__exc1_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2005-2019 Mort HeatWavesModerate exc=1",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2020 Data

myORtable2_HeatWavesModerate__exc1_complete <- myORtable2_HeatWavesModerate__exc1[complete.cases(myORtable2_HeatWavesModerate__exc1), ]
myORtable2_HeatWavesModerate__exc1_complete[,1] <- seq(1:length(myORtable2_HeatWavesModerate__exc1_complete$Model))
colnames(myORtable2_HeatWavesModerate__exc1_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable2_HeatWavesModerate__exc1_complete$StrataLength = myORtable2_HeatWavesModerate__exc1_complete$StrataLength + 6

Little_exc1 <- ggplot(myORtable2_HeatWavesModerate__exc1_complete) +
  geom_line(aes(x=StrataLength, y=myORtable2_HeatWavesModerate__exc1_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable2_HeatWavesModerate__exc1_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable2_HeatWavesModerate__exc1_complete[,3], ymax=myORtable2_HeatWavesModerate__exc1_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2020 Mort HeatWavesModerate exc=1",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)


myORtable0_HeatWavesModerate__exc2 <- data.frame()
myORtable1_HeatWavesModerate__exc2 <- data.frame()
myORtable2_HeatWavesModerate__exc2 <- data.frame()

for (i in (c(7:28))) {
  model0_HeatWavesModerate__exc2 = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 2, data=PHFexc_Hot)
  model1_HeatWavesModerate__exc2 = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 2, data=PHF519exc_Hot)
  model2_HeatWavesModerate__exc2 = casecross(Mort ~ HeatWavesModerate+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 2, data=PHF20exc_Hot)
  
  sum0_HeatWavesModerate__exc2 <- summary(model0_HeatWavesModerate__exc2)
  coef0_HeatWavesModerate__exc2 = sum0_HeatWavesModerate__exc2[1]
  se0_HeatWavesModerate__exc2 = sum0_HeatWavesModerate__exc2[7]
  
  sum1_HeatWavesModerate__exc2 <- summary(model1_HeatWavesModerate__exc2)
  coef1_HeatWavesModerate__exc2 = sum1_HeatWavesModerate__exc2[1]
  se1_HeatWavesModerate__exc2 = sum1_HeatWavesModerate__exc2[7]
  
  sum2_HeatWavesModerate__exc2 <- summary(model2_HeatWavesModerate__exc2)
  coef2_HeatWavesModerate__exc2 = sum2_HeatWavesModerate__exc2[1]
  se2_HeatWavesModerate__exc2 = sum2_HeatWavesModerate__exc2[7]
  
  myORtable0_HeatWavesModerate__exc2[i,1] = 0
  myORtable0_HeatWavesModerate__exc2[i,2] = sum0_HeatWavesModerate__exc2[1,2] #sum[3] = OR
  myORtable0_HeatWavesModerate__exc2[i,3] = exp(coef0_HeatWavesModerate__exc2+(1.96*se0_HeatWavesModerate__exc2))
  myORtable0_HeatWavesModerate__exc2[i,4] = exp(coef0_HeatWavesModerate__exc2-(1.96*se0_HeatWavesModerate__exc2))
  colnames(myORtable0_HeatWavesModerate__exc2) <- c("Model","OR","UpperCI","LowerCI")    
  
  myORtable1_HeatWavesModerate__exc2[i,1] = 1
  myORtable1_HeatWavesModerate__exc2[i,2] = sum1_HeatWavesModerate__exc2[1,2] #sum[3] = OR
  myORtable1_HeatWavesModerate__exc2[i,3] = exp(coef1_HeatWavesModerate__exc2+(1.96*se1_HeatWavesModerate__exc2))
  myORtable1_HeatWavesModerate__exc2[i,4] = exp(coef1_HeatWavesModerate__exc2-(1.96*se1_HeatWavesModerate__exc2))
  colnames(myORtable1_HeatWavesModerate__exc2) <- c("Model","OR","UpperCI","LowerCI")
  
  myORtable2_HeatWavesModerate__exc2[i,1] = 2
  myORtable2_HeatWavesModerate__exc2[i,2] = sum2_HeatWavesModerate__exc2[1,2] #sum[3] = OR
  myORtable2_HeatWavesModerate__exc2[i,3] = exp(coef2_HeatWavesModerate__exc2+(1.96*se2_HeatWavesModerate__exc2))
  myORtable2_HeatWavesModerate__exc2[i,4] = exp(coef2_HeatWavesModerate__exc2-(1.96*se2_HeatWavesModerate__exc2))
  colnames(myORtable2_HeatWavesModerate__exc2) <- c("Model","OR","UpperCI","LowerCI")
}

#Full Mort

myORtable0_HeatWavesModerate__exc2_complete <- myORtable0_HeatWavesModerate__exc2[complete.cases(myORtable0_HeatWavesModerate__exc2), ]
myORtable0_HeatWavesModerate__exc2_complete[,1] <- seq(1:length(myORtable0_HeatWavesModerate__exc2_complete$Model))
colnames(myORtable0_HeatWavesModerate__exc2_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable0_HeatWavesModerate__exc2_complete$StrataLength = myORtable0_HeatWavesModerate__exc2_complete$StrataLength + 6

Full_exc2 <- ggplot(myORtable0_HeatWavesModerate__exc2_complete) +
  geom_line(aes(x=StrataLength, y=myORtable0_HeatWavesModerate__exc2_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable0_HeatWavesModerate__exc2_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable0_HeatWavesModerate__exc2_complete[,3], ymax=myORtable0_HeatWavesModerate__exc2_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF Full Mort HeatWavesModerate exc=2",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2005-2019 Data

myORtable1_HeatWavesModerate__exc2_complete <- myORtable1_HeatWavesModerate__exc2[complete.cases(myORtable1_HeatWavesModerate__exc2), ]
myORtable1_HeatWavesModerate__exc2_complete[,1] <- seq(1:length(myORtable1_HeatWavesModerate__exc2_complete$Model))
colnames(myORtable1_HeatWavesModerate__exc2_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable1_HeatWavesModerate__exc2_complete$StrataLength = myORtable1_HeatWavesModerate__exc2_complete$StrataLength + 6

Big_exc2 <- ggplot(myORtable1_HeatWavesModerate__exc2_complete) +
  geom_line(aes(x=StrataLength, y=myORtable1_HeatWavesModerate__exc2_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable1_HeatWavesModerate__exc2_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable1_HeatWavesModerate__exc2_complete[,3], ymax=myORtable1_HeatWavesModerate__exc2_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2005-2019 Mort HeatWavesModerate exc=2",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2020 Data

myORtable2_HeatWavesModerate__exc2_complete <- myORtable2_HeatWavesModerate__exc2[complete.cases(myORtable2_HeatWavesModerate__exc2), ]
myORtable2_HeatWavesModerate__exc2_complete[,1] <- seq(1:length(myORtable2_HeatWavesModerate__exc2_complete$Model))
colnames(myORtable2_HeatWavesModerate__exc2_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable2_HeatWavesModerate__exc2_complete$StrataLength = myORtable2_HeatWavesModerate__exc2_complete$StrataLength + 6

Little_exc2 <- ggplot(myORtable2_HeatWavesModerate__exc2_complete) +
  geom_line(aes(x=StrataLength, y=myORtable2_HeatWavesModerate__exc2_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable2_HeatWavesModerate__exc2_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable2_HeatWavesModerate__exc2_complete[,3], ymax=myORtable2_HeatWavesModerate__exc2_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2020 Mort HeatWavesModerate exc=2",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

png("CaseCross_HeatWavesModerate_PHF.png",width = 1000, height = 800)
ggarrange(Full_exc0,Full_exc1,Full_exc2,Big_exc0,Big_exc1,Big_exc2,Little_exc0,Little_exc1,Little_exc2)
dev.off()

myORtable0_HeatWavesSevere <- data.frame()
myORtable1_HeatWavesSevere <- data.frame()
myORtable2_HeatWavesSevere <- data.frame()

myORtable0_HeatWavesSevere_exc0 <- data.frame()
myORtable1_HeatWavesSevere_exc0 <- data.frame()
myORtable2_HeatWavesSevere_exc0 <- data.frame()

for (i in (c(7:28))) {
  model0_HeatWavesSevere_exc0 = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 0, data=PHFexc_Hot)
  model1_HeatWavesSevere_exc0 = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 0, data=PHF519exc_Hot)
  model2_HeatWavesSevere_exc0 = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 0, data=PHF20exc_Hot)
  
  sum0_HeatWavesSevere_exc0 <- summary(model0_HeatWavesSevere_exc0)
  coef0_HeatWavesSevere_exc0 = sum0_HeatWavesSevere_exc0[1]
  se0_HeatWavesSevere_exc0 = sum0_HeatWavesSevere_exc0[7]
  
  sum1_HeatWavesSevere_exc0 <- summary(model1_HeatWavesSevere_exc0)
  coef1_HeatWavesSevere_exc0 = sum1_HeatWavesSevere_exc0[1]
  se1_HeatWavesSevere_exc0 = sum1_HeatWavesSevere_exc0[7]
  
  sum2_HeatWavesSevere_exc0 <- summary(model2_HeatWavesSevere_exc0)
  coef2_HeatWavesSevere_exc0 = sum2_HeatWavesSevere_exc0[1]
  se2_HeatWavesSevere_exc0 = sum2_HeatWavesSevere_exc0[7]
  
  myORtable0_HeatWavesSevere_exc0[i,1] = 0
  myORtable0_HeatWavesSevere_exc0[i,2] = sum0_HeatWavesSevere_exc0[1,2] #sum[3] = OR
  myORtable0_HeatWavesSevere_exc0[i,3] = exp(coef0_HeatWavesSevere_exc0+(1.96*se0_HeatWavesSevere_exc0))
  myORtable0_HeatWavesSevere_exc0[i,4] = exp(coef0_HeatWavesSevere_exc0-(1.96*se0_HeatWavesSevere_exc0))
  colnames(myORtable0_HeatWavesSevere_exc0) <- c("Model","OR","UpperCI","LowerCI")    
  
  myORtable1_HeatWavesSevere_exc0[i,1] = 1
  myORtable1_HeatWavesSevere_exc0[i,2] = sum1_HeatWavesSevere_exc0[1,2] #sum[3] = OR
  myORtable1_HeatWavesSevere_exc0[i,3] = exp(coef1_HeatWavesSevere_exc0+(1.96*se1_HeatWavesSevere_exc0))
  myORtable1_HeatWavesSevere_exc0[i,4] = exp(coef1_HeatWavesSevere_exc0-(1.96*se1_HeatWavesSevere_exc0))
  colnames(myORtable1_HeatWavesSevere_exc0) <- c("Model","OR","UpperCI","LowerCI")
  
  myORtable2_HeatWavesSevere_exc0[i,1] = 2
  myORtable2_HeatWavesSevere_exc0[i,2] = sum2_HeatWavesSevere_exc0[1,2] #sum[3] = OR
  myORtable2_HeatWavesSevere_exc0[i,3] = exp(coef2_HeatWavesSevere_exc0+(1.96*se2_HeatWavesSevere_exc0))
  myORtable2_HeatWavesSevere_exc0[i,4] = exp(coef2_HeatWavesSevere_exc0-(1.96*se2_HeatWavesSevere_exc0))
  colnames(myORtable2_HeatWavesSevere_exc0) <- c("Model","OR","UpperCI","LowerCI")
}

#Full Mort

myORtable0_HeatWavesSevere_exc0_complete <- myORtable0_HeatWavesSevere_exc0[complete.cases(myORtable0_HeatWavesSevere_exc0), ]
myORtable0_HeatWavesSevere_exc0_complete[,1] <- seq(1:length(myORtable0_HeatWavesSevere_exc0_complete$Model))
colnames(myORtable0_HeatWavesSevere_exc0_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable0_HeatWavesSevere_exc0_complete$StrataLength = myORtable0_HeatWavesSevere_exc0_complete$StrataLength + 6

Full_exc0 <- ggplot(myORtable0_HeatWavesSevere_exc0_complete) +
  geom_line(aes(x=StrataLength, y=myORtable0_HeatWavesSevere_exc0_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable0_HeatWavesSevere_exc0_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable0_HeatWavesSevere_exc0_complete[,3], ymax=myORtable0_HeatWavesSevere_exc0_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF Full Mort HeatWavesSevere exc=0",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2005-2019 Data

myORtable1_HeatWavesSevere_exc0_complete <- myORtable1_HeatWavesSevere_exc0[complete.cases(myORtable1_HeatWavesSevere_exc0), ]
myORtable1_HeatWavesSevere_exc0_complete[,1] <- seq(1:length(myORtable1_HeatWavesSevere_exc0_complete$Model))
colnames(myORtable1_HeatWavesSevere_exc0_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable1_HeatWavesSevere_exc0_complete$StrataLength = myORtable1_HeatWavesSevere_exc0_complete$StrataLength + 6

Big_exc0 <- ggplot(myORtable1_HeatWavesSevere_exc0_complete) +
  geom_line(aes(x=StrataLength, y=myORtable1_HeatWavesSevere_exc0_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable1_HeatWavesSevere_exc0_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable1_HeatWavesSevere_exc0_complete[,3], ymax=myORtable1_HeatWavesSevere_exc0_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2005-2019 Mort HeatWavesSevere exc=0",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2020 Data

myORtable2_HeatWavesSevere_exc0_complete <- myORtable2_HeatWavesSevere_exc0[complete.cases(myORtable2_HeatWavesSevere_exc0), ]
myORtable2_HeatWavesSevere_exc0_complete[,1] <- seq(1:length(myORtable2_HeatWavesSevere_exc0_complete$Model))
colnames(myORtable2_HeatWavesSevere_exc0_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable2_HeatWavesSevere_exc0_complete$StrataLength = myORtable2_HeatWavesSevere_exc0_complete$StrataLength + 6

Little_exc0 <- ggplot(myORtable2_HeatWavesSevere_exc0_complete) +
  geom_line(aes(x=StrataLength, y=myORtable2_HeatWavesSevere_exc0_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable2_HeatWavesSevere_exc0_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable2_HeatWavesSevere_exc0_complete[,3], ymax=myORtable2_HeatWavesSevere_exc0_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2020 Mort HeatWavesSevere exc=0",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

myORtable0_HeatWavesSevere__exc1 <- data.frame()
myORtable1_HeatWavesSevere__exc1 <- data.frame()
myORtable2_HeatWavesSevere__exc1 <- data.frame()

for (i in (c(7:28))) {
  model0_HeatWavesSevere__exc1 = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 1, data=PHFexc_Hot)
  model1_HeatWavesSevere__exc1 = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 1, data=PHF519exc_Hot)
  model2_HeatWavesSevere__exc1 = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 1, data=PHF20exc_Hot)
  
  sum0_HeatWavesSevere__exc1 <- summary(model0_HeatWavesSevere__exc1)
  coef0_HeatWavesSevere__exc1 = sum0_HeatWavesSevere__exc1[1]
  se0_HeatWavesSevere__exc1 = sum0_HeatWavesSevere__exc1[7]
  
  sum1_HeatWavesSevere__exc1 <- summary(model1_HeatWavesSevere__exc1)
  coef1_HeatWavesSevere__exc1 = sum1_HeatWavesSevere__exc1[1]
  se1_HeatWavesSevere__exc1 = sum1_HeatWavesSevere__exc1[7]
  
  sum2_HeatWavesSevere__exc1 <- summary(model2_HeatWavesSevere__exc1)
  coef2_HeatWavesSevere__exc1 = sum2_HeatWavesSevere__exc1[1]
  se2_HeatWavesSevere__exc1 = sum2_HeatWavesSevere__exc1[7]
  
  myORtable0_HeatWavesSevere__exc1[i,1] = 0
  myORtable0_HeatWavesSevere__exc1[i,2] = sum0_HeatWavesSevere__exc1[1,2] #sum[3] = OR
  myORtable0_HeatWavesSevere__exc1[i,3] = exp(coef0_HeatWavesSevere__exc1+(1.96*se0_HeatWavesSevere__exc1))
  myORtable0_HeatWavesSevere__exc1[i,4] = exp(coef0_HeatWavesSevere__exc1-(1.96*se0_HeatWavesSevere__exc1))
  colnames(myORtable0_HeatWavesSevere__exc1) <- c("Model","OR","UpperCI","LowerCI")    
  
  myORtable1_HeatWavesSevere__exc1[i,1] = 1
  myORtable1_HeatWavesSevere__exc1[i,2] = sum1_HeatWavesSevere__exc1[1,2] #sum[3] = OR
  myORtable1_HeatWavesSevere__exc1[i,3] = exp(coef1_HeatWavesSevere__exc1+(1.96*se1_HeatWavesSevere__exc1))
  myORtable1_HeatWavesSevere__exc1[i,4] = exp(coef1_HeatWavesSevere__exc1-(1.96*se1_HeatWavesSevere__exc1))
  colnames(myORtable1_HeatWavesSevere__exc1) <- c("Model","OR","UpperCI","LowerCI")
  
  myORtable2_HeatWavesSevere__exc1[i,1] = 2
  myORtable2_HeatWavesSevere__exc1[i,2] = sum2_HeatWavesSevere__exc1[1,2] #sum[3] = OR
  myORtable2_HeatWavesSevere__exc1[i,3] = exp(coef2_HeatWavesSevere__exc1+(1.96*se2_HeatWavesSevere__exc1))
  myORtable2_HeatWavesSevere__exc1[i,4] = exp(coef2_HeatWavesSevere__exc1-(1.96*se2_HeatWavesSevere__exc1))
  colnames(myORtable2_HeatWavesSevere__exc1) <- c("Model","OR","UpperCI","LowerCI")
}

#Full Mort

myORtable0_HeatWavesSevere__exc1_complete <- myORtable0_HeatWavesSevere__exc1[complete.cases(myORtable0_HeatWavesSevere__exc1), ]
myORtable0_HeatWavesSevere__exc1_complete[,1] <- seq(1:length(myORtable0_HeatWavesSevere__exc1_complete$Model))
colnames(myORtable0_HeatWavesSevere__exc1_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable0_HeatWavesSevere__exc1_complete$StrataLength = myORtable0_HeatWavesSevere__exc1_complete$StrataLength + 6

Full_exc1 <- ggplot(myORtable0_HeatWavesSevere__exc1_complete) +
  geom_line(aes(x=StrataLength, y=myORtable0_HeatWavesSevere__exc1_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable0_HeatWavesSevere__exc1_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable0_HeatWavesSevere__exc1_complete[,3], ymax=myORtable0_HeatWavesSevere__exc1_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF Full Mort HeatWavesSevere exc=1",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2005-2019 Data

myORtable1_HeatWavesSevere__exc1_complete <- myORtable1_HeatWavesSevere__exc1[complete.cases(myORtable1_HeatWavesSevere__exc1), ]
myORtable1_HeatWavesSevere__exc1_complete[,1] <- seq(1:length(myORtable1_HeatWavesSevere__exc1_complete$Model))
colnames(myORtable1_HeatWavesSevere__exc1_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable1_HeatWavesSevere__exc1_complete$StrataLength = myORtable1_HeatWavesSevere__exc1_complete$StrataLength + 6

Big_exc1 <- ggplot(myORtable1_HeatWavesSevere__exc1_complete) +
  geom_line(aes(x=StrataLength, y=myORtable1_HeatWavesSevere__exc1_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable1_HeatWavesSevere__exc1_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable1_HeatWavesSevere__exc1_complete[,3], ymax=myORtable1_HeatWavesSevere__exc1_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2005-2019 Mort HeatWavesSevere exc=1",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2020 Data

myORtable2_HeatWavesSevere__exc1_complete <- myORtable2_HeatWavesSevere__exc1[complete.cases(myORtable2_HeatWavesSevere__exc1), ]
myORtable2_HeatWavesSevere__exc1_complete[,1] <- seq(1:length(myORtable2_HeatWavesSevere__exc1_complete$Model))
colnames(myORtable2_HeatWavesSevere__exc1_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable2_HeatWavesSevere__exc1_complete$StrataLength = myORtable2_HeatWavesSevere__exc1_complete$StrataLength + 6

Little_exc1 <- ggplot(myORtable2_HeatWavesSevere__exc1_complete) +
  geom_line(aes(x=StrataLength, y=myORtable2_HeatWavesSevere__exc1_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable2_HeatWavesSevere__exc1_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable2_HeatWavesSevere__exc1_complete[,3], ymax=myORtable2_HeatWavesSevere__exc1_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2020 Mort HeatWavesSevere exc=1",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)


myORtable0_HeatWavesSevere__exc2 <- data.frame()
myORtable1_HeatWavesSevere__exc2 <- data.frame()
myORtable2_HeatWavesSevere__exc2 <- data.frame()

for (i in (c(7:28))) {
  model0_HeatWavesSevere__exc2 = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 2, data=PHFexc_Hot)
  model1_HeatWavesSevere__exc2 = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 2, data=PHF519exc_Hot)
  model2_HeatWavesSevere__exc2 = casecross(Mort ~ HeatWavesSevere+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 2, data=PHF20exc_Hot)
  
  sum0_HeatWavesSevere__exc2 <- summary(model0_HeatWavesSevere__exc2)
  coef0_HeatWavesSevere__exc2 = sum0_HeatWavesSevere__exc2[1]
  se0_HeatWavesSevere__exc2 = sum0_HeatWavesSevere__exc2[7]
  
  sum1_HeatWavesSevere__exc2 <- summary(model1_HeatWavesSevere__exc2)
  coef1_HeatWavesSevere__exc2 = sum1_HeatWavesSevere__exc2[1]
  se1_HeatWavesSevere__exc2 = sum1_HeatWavesSevere__exc2[7]
  
  sum2_HeatWavesSevere__exc2 <- summary(model2_HeatWavesSevere__exc2)
  coef2_HeatWavesSevere__exc2 = sum2_HeatWavesSevere__exc2[1]
  se2_HeatWavesSevere__exc2 = sum2_HeatWavesSevere__exc2[7]
  
  myORtable0_HeatWavesSevere__exc2[i,1] = 0
  myORtable0_HeatWavesSevere__exc2[i,2] = sum0_HeatWavesSevere__exc2[1,2] #sum[3] = OR
  myORtable0_HeatWavesSevere__exc2[i,3] = exp(coef0_HeatWavesSevere__exc2+(1.96*se0_HeatWavesSevere__exc2))
  myORtable0_HeatWavesSevere__exc2[i,4] = exp(coef0_HeatWavesSevere__exc2-(1.96*se0_HeatWavesSevere__exc2))
  colnames(myORtable0_HeatWavesSevere__exc2) <- c("Model","OR","UpperCI","LowerCI")    
  
  myORtable1_HeatWavesSevere__exc2[i,1] = 1
  myORtable1_HeatWavesSevere__exc2[i,2] = sum1_HeatWavesSevere__exc2[1,2] #sum[3] = OR
  myORtable1_HeatWavesSevere__exc2[i,3] = exp(coef1_HeatWavesSevere__exc2+(1.96*se1_HeatWavesSevere__exc2))
  myORtable1_HeatWavesSevere__exc2[i,4] = exp(coef1_HeatWavesSevere__exc2-(1.96*se1_HeatWavesSevere__exc2))
  colnames(myORtable1_HeatWavesSevere__exc2) <- c("Model","OR","UpperCI","LowerCI")
  
  myORtable2_HeatWavesSevere__exc2[i,1] = 2
  myORtable2_HeatWavesSevere__exc2[i,2] = sum2_HeatWavesSevere__exc2[1,2] #sum[3] = OR
  myORtable2_HeatWavesSevere__exc2[i,3] = exp(coef2_HeatWavesSevere__exc2+(1.96*se2_HeatWavesSevere__exc2))
  myORtable2_HeatWavesSevere__exc2[i,4] = exp(coef2_HeatWavesSevere__exc2-(1.96*se2_HeatWavesSevere__exc2))
  colnames(myORtable2_HeatWavesSevere__exc2) <- c("Model","OR","UpperCI","LowerCI")
}

#Full Mort

myORtable0_HeatWavesSevere__exc2_complete <- myORtable0_HeatWavesSevere__exc2[complete.cases(myORtable0_HeatWavesSevere__exc2), ]
myORtable0_HeatWavesSevere__exc2_complete[,1] <- seq(1:length(myORtable0_HeatWavesSevere__exc2_complete$Model))
colnames(myORtable0_HeatWavesSevere__exc2_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable0_HeatWavesSevere__exc2_complete$StrataLength = myORtable0_HeatWavesSevere__exc2_complete$StrataLength + 6

Full_exc2 <- ggplot(myORtable0_HeatWavesSevere__exc2_complete) +
  geom_line(aes(x=StrataLength, y=myORtable0_HeatWavesSevere__exc2_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable0_HeatWavesSevere__exc2_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable0_HeatWavesSevere__exc2_complete[,3], ymax=myORtable0_HeatWavesSevere__exc2_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF Full Mort HeatWavesSevere exc=2",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2005-2019 Data

myORtable1_HeatWavesSevere__exc2_complete <- myORtable1_HeatWavesSevere__exc2[complete.cases(myORtable1_HeatWavesSevere__exc2), ]
myORtable1_HeatWavesSevere__exc2_complete[,1] <- seq(1:length(myORtable1_HeatWavesSevere__exc2_complete$Model))
colnames(myORtable1_HeatWavesSevere__exc2_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable1_HeatWavesSevere__exc2_complete$StrataLength = myORtable1_HeatWavesSevere__exc2_complete$StrataLength + 6

Big_exc2 <- ggplot(myORtable1_HeatWavesSevere__exc2_complete) +
  geom_line(aes(x=StrataLength, y=myORtable1_HeatWavesSevere__exc2_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable1_HeatWavesSevere__exc2_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable1_HeatWavesSevere__exc2_complete[,3], ymax=myORtable1_HeatWavesSevere__exc2_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2005-2019 Mort HeatWavesSevere exc=2",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2020 Data

myORtable2_HeatWavesSevere__exc2_complete <- myORtable2_HeatWavesSevere__exc2[complete.cases(myORtable2_HeatWavesSevere__exc2), ]
myORtable2_HeatWavesSevere__exc2_complete[,1] <- seq(1:length(myORtable2_HeatWavesSevere__exc2_complete$Model))
colnames(myORtable2_HeatWavesSevere__exc2_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable2_HeatWavesSevere__exc2_complete$StrataLength = myORtable2_HeatWavesSevere__exc2_complete$StrataLength + 6

Little_exc2 <- ggplot(myORtable2_HeatWavesSevere__exc2_complete) +
  geom_line(aes(x=StrataLength, y=myORtable2_HeatWavesSevere__exc2_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable2_HeatWavesSevere__exc2_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable2_HeatWavesSevere__exc2_complete[,3], ymax=myORtable2_HeatWavesSevere__exc2_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2020 Mort HeatWavesSevere exc=2",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

png("CaseCross_HeatWavesSevere_PHF.png",width = 1000, height = 800)
ggarrange(Full_exc0,Full_exc1,Full_exc2,Big_exc0,Big_exc1,Big_exc2,Little_exc0,Little_exc1,Little_exc2)
dev.off()

myORtable0_HeatWavesExtreme <- data.frame()
myORtable1_HeatWavesExtreme <- data.frame()
myORtable2_HeatWavesExtreme <- data.frame()

myORtable0_HeatWavesExtreme_exc0 <- data.frame()
myORtable1_HeatWavesExtreme_exc0 <- data.frame()
myORtable2_HeatWavesExtreme_exc0 <- data.frame()

for (i in (c(7:28))) {
  model0_HeatWavesExtreme_exc0 = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 0, data=PHFexc_Hot)
  model1_HeatWavesExtreme_exc0 = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 0, data=PHF519exc_Hot)
  model2_HeatWavesExtreme_exc0 = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 0, data=PHF20exc_Hot)
  
  sum0_HeatWavesExtreme_exc0 <- summary(model0_HeatWavesExtreme_exc0)
  coef0_HeatWavesExtreme_exc0 = sum0_HeatWavesExtreme_exc0[1]
  se0_HeatWavesExtreme_exc0 = sum0_HeatWavesExtreme_exc0[7]
  
  sum1_HeatWavesExtreme_exc0 <- summary(model1_HeatWavesExtreme_exc0)
  coef1_HeatWavesExtreme_exc0 = sum1_HeatWavesExtreme_exc0[1]
  se1_HeatWavesExtreme_exc0 = sum1_HeatWavesExtreme_exc0[7]
  
  sum2_HeatWavesExtreme_exc0 <- summary(model2_HeatWavesExtreme_exc0)
  coef2_HeatWavesExtreme_exc0 = sum2_HeatWavesExtreme_exc0[1]
  se2_HeatWavesExtreme_exc0 = sum2_HeatWavesExtreme_exc0[7]
  
  myORtable0_HeatWavesExtreme_exc0[i,1] = 0
  myORtable0_HeatWavesExtreme_exc0[i,2] = sum0_HeatWavesExtreme_exc0[1,2] #sum[3] = OR
  myORtable0_HeatWavesExtreme_exc0[i,3] = exp(coef0_HeatWavesExtreme_exc0+(1.96*se0_HeatWavesExtreme_exc0))
  myORtable0_HeatWavesExtreme_exc0[i,4] = exp(coef0_HeatWavesExtreme_exc0-(1.96*se0_HeatWavesExtreme_exc0))
  colnames(myORtable0_HeatWavesExtreme_exc0) <- c("Model","OR","UpperCI","LowerCI")    
  
  myORtable1_HeatWavesExtreme_exc0[i,1] = 1
  myORtable1_HeatWavesExtreme_exc0[i,2] = sum1_HeatWavesExtreme_exc0[1,2] #sum[3] = OR
  myORtable1_HeatWavesExtreme_exc0[i,3] = exp(coef1_HeatWavesExtreme_exc0+(1.96*se1_HeatWavesExtreme_exc0))
  myORtable1_HeatWavesExtreme_exc0[i,4] = exp(coef1_HeatWavesExtreme_exc0-(1.96*se1_HeatWavesExtreme_exc0))
  colnames(myORtable1_HeatWavesExtreme_exc0) <- c("Model","OR","UpperCI","LowerCI")
  
  myORtable2_HeatWavesExtreme_exc0[i,1] = 2
  myORtable2_HeatWavesExtreme_exc0[i,2] = sum2_HeatWavesExtreme_exc0[1,2] #sum[3] = OR
  myORtable2_HeatWavesExtreme_exc0[i,3] = exp(coef2_HeatWavesExtreme_exc0+(1.96*se2_HeatWavesExtreme_exc0))
  myORtable2_HeatWavesExtreme_exc0[i,4] = exp(coef2_HeatWavesExtreme_exc0-(1.96*se2_HeatWavesExtreme_exc0))
  colnames(myORtable2_HeatWavesExtreme_exc0) <- c("Model","OR","UpperCI","LowerCI")
}

#Full Mort

myORtable0_HeatWavesExtreme_exc0_complete <- myORtable0_HeatWavesExtreme_exc0[complete.cases(myORtable0_HeatWavesExtreme_exc0), ]
myORtable0_HeatWavesExtreme_exc0_complete[,1] <- seq(1:length(myORtable0_HeatWavesExtreme_exc0_complete$Model))
colnames(myORtable0_HeatWavesExtreme_exc0_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable0_HeatWavesExtreme_exc0_complete$StrataLength = myORtable0_HeatWavesExtreme_exc0_complete$StrataLength + 6

Full_exc0 <- ggplot(myORtable0_HeatWavesExtreme_exc0_complete) +
  geom_line(aes(x=StrataLength, y=myORtable0_HeatWavesExtreme_exc0_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable0_HeatWavesExtreme_exc0_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable0_HeatWavesExtreme_exc0_complete[,3], ymax=myORtable0_HeatWavesExtreme_exc0_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF Full Mort HeatWavesExtreme exc=0",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2005-2019 Data

myORtable1_HeatWavesExtreme_exc0_complete <- myORtable1_HeatWavesExtreme_exc0[complete.cases(myORtable1_HeatWavesExtreme_exc0), ]
myORtable1_HeatWavesExtreme_exc0_complete[,1] <- seq(1:length(myORtable1_HeatWavesExtreme_exc0_complete$Model))
colnames(myORtable1_HeatWavesExtreme_exc0_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable1_HeatWavesExtreme_exc0_complete$StrataLength = myORtable1_HeatWavesExtreme_exc0_complete$StrataLength + 6

Big_exc0 <- ggplot(myORtable1_HeatWavesExtreme_exc0_complete) +
  geom_line(aes(x=StrataLength, y=myORtable1_HeatWavesExtreme_exc0_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable1_HeatWavesExtreme_exc0_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable1_HeatWavesExtreme_exc0_complete[,3], ymax=myORtable1_HeatWavesExtreme_exc0_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2005-2019 Mort HeatWavesExtreme exc=0",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2020 Data

myORtable2_HeatWavesExtreme_exc0_complete <- myORtable2_HeatWavesExtreme_exc0[complete.cases(myORtable2_HeatWavesExtreme_exc0), ]
myORtable2_HeatWavesExtreme_exc0_complete[,1] <- seq(1:length(myORtable2_HeatWavesExtreme_exc0_complete$Model))
colnames(myORtable2_HeatWavesExtreme_exc0_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable2_HeatWavesExtreme_exc0_complete$StrataLength = myORtable2_HeatWavesExtreme_exc0_complete$StrataLength + 6

Little_exc0 <- ggplot(myORtable2_HeatWavesExtreme_exc0_complete) +
  geom_line(aes(x=StrataLength, y=myORtable2_HeatWavesExtreme_exc0_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable2_HeatWavesExtreme_exc0_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable2_HeatWavesExtreme_exc0_complete[,3], ymax=myORtable2_HeatWavesExtreme_exc0_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2020 Mort HeatWavesExtreme exc=0",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

myORtable0_HeatWavesExtreme__exc1 <- data.frame()
myORtable1_HeatWavesExtreme__exc1 <- data.frame()
myORtable2_HeatWavesExtreme__exc1 <- data.frame()

for (i in (c(7:28))) {
  model0_HeatWavesExtreme__exc1 = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 1, data=PHFexc_Hot)
  model1_HeatWavesExtreme__exc1 = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 1, data=PHF519exc_Hot)
  model2_HeatWavesExtreme__exc1 = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 1, data=PHF20exc_Hot)
  
  sum0_HeatWavesExtreme__exc1 <- summary(model0_HeatWavesExtreme__exc1)
  coef0_HeatWavesExtreme__exc1 = sum0_HeatWavesExtreme__exc1[1]
  se0_HeatWavesExtreme__exc1 = sum0_HeatWavesExtreme__exc1[7]
  
  sum1_HeatWavesExtreme__exc1 <- summary(model1_HeatWavesExtreme__exc1)
  coef1_HeatWavesExtreme__exc1 = sum1_HeatWavesExtreme__exc1[1]
  se1_HeatWavesExtreme__exc1 = sum1_HeatWavesExtreme__exc1[7]
  
  sum2_HeatWavesExtreme__exc1 <- summary(model2_HeatWavesExtreme__exc1)
  coef2_HeatWavesExtreme__exc1 = sum2_HeatWavesExtreme__exc1[1]
  se2_HeatWavesExtreme__exc1 = sum2_HeatWavesExtreme__exc1[7]
  
  myORtable0_HeatWavesExtreme__exc1[i,1] = 0
  myORtable0_HeatWavesExtreme__exc1[i,2] = sum0_HeatWavesExtreme__exc1[1,2] #sum[3] = OR
  myORtable0_HeatWavesExtreme__exc1[i,3] = exp(coef0_HeatWavesExtreme__exc1+(1.96*se0_HeatWavesExtreme__exc1))
  myORtable0_HeatWavesExtreme__exc1[i,4] = exp(coef0_HeatWavesExtreme__exc1-(1.96*se0_HeatWavesExtreme__exc1))
  colnames(myORtable0_HeatWavesExtreme__exc1) <- c("Model","OR","UpperCI","LowerCI")    
  
  myORtable1_HeatWavesExtreme__exc1[i,1] = 1
  myORtable1_HeatWavesExtreme__exc1[i,2] = sum1_HeatWavesExtreme__exc1[1,2] #sum[3] = OR
  myORtable1_HeatWavesExtreme__exc1[i,3] = exp(coef1_HeatWavesExtreme__exc1+(1.96*se1_HeatWavesExtreme__exc1))
  myORtable1_HeatWavesExtreme__exc1[i,4] = exp(coef1_HeatWavesExtreme__exc1-(1.96*se1_HeatWavesExtreme__exc1))
  colnames(myORtable1_HeatWavesExtreme__exc1) <- c("Model","OR","UpperCI","LowerCI")
  
  myORtable2_HeatWavesExtreme__exc1[i,1] = 2
  myORtable2_HeatWavesExtreme__exc1[i,2] = sum2_HeatWavesExtreme__exc1[1,2] #sum[3] = OR
  myORtable2_HeatWavesExtreme__exc1[i,3] = exp(coef2_HeatWavesExtreme__exc1+(1.96*se2_HeatWavesExtreme__exc1))
  myORtable2_HeatWavesExtreme__exc1[i,4] = exp(coef2_HeatWavesExtreme__exc1-(1.96*se2_HeatWavesExtreme__exc1))
  colnames(myORtable2_HeatWavesExtreme__exc1) <- c("Model","OR","UpperCI","LowerCI")
}

#Full Mort

myORtable0_HeatWavesExtreme__exc1_complete <- myORtable0_HeatWavesExtreme__exc1[complete.cases(myORtable0_HeatWavesExtreme__exc1), ]
myORtable0_HeatWavesExtreme__exc1_complete[,1] <- seq(1:length(myORtable0_HeatWavesExtreme__exc1_complete$Model))
colnames(myORtable0_HeatWavesExtreme__exc1_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable0_HeatWavesExtreme__exc1_complete$StrataLength = myORtable0_HeatWavesExtreme__exc1_complete$StrataLength + 6

Full_exc1 <- ggplot(myORtable0_HeatWavesExtreme__exc1_complete) +
  geom_line(aes(x=StrataLength, y=myORtable0_HeatWavesExtreme__exc1_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable0_HeatWavesExtreme__exc1_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable0_HeatWavesExtreme__exc1_complete[,3], ymax=myORtable0_HeatWavesExtreme__exc1_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF Full Mort HeatWavesExtreme exc=1",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2005-2019 Data

myORtable1_HeatWavesExtreme__exc1_complete <- myORtable1_HeatWavesExtreme__exc1[complete.cases(myORtable1_HeatWavesExtreme__exc1), ]
myORtable1_HeatWavesExtreme__exc1_complete[,1] <- seq(1:length(myORtable1_HeatWavesExtreme__exc1_complete$Model))
colnames(myORtable1_HeatWavesExtreme__exc1_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable1_HeatWavesExtreme__exc1_complete$StrataLength = myORtable1_HeatWavesExtreme__exc1_complete$StrataLength + 6

Big_exc1 <- ggplot(myORtable1_HeatWavesExtreme__exc1_complete) +
  geom_line(aes(x=StrataLength, y=myORtable1_HeatWavesExtreme__exc1_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable1_HeatWavesExtreme__exc1_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable1_HeatWavesExtreme__exc1_complete[,3], ymax=myORtable1_HeatWavesExtreme__exc1_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2005-2019 Mort HeatWavesExtreme exc=1",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2020 Data

myORtable2_HeatWavesExtreme__exc1_complete <- myORtable2_HeatWavesExtreme__exc1[complete.cases(myORtable2_HeatWavesExtreme__exc1), ]
myORtable2_HeatWavesExtreme__exc1_complete[,1] <- seq(1:length(myORtable2_HeatWavesExtreme__exc1_complete$Model))
colnames(myORtable2_HeatWavesExtreme__exc1_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable2_HeatWavesExtreme__exc1_complete$StrataLength = myORtable2_HeatWavesExtreme__exc1_complete$StrataLength + 6

Little_exc1 <- ggplot(myORtable2_HeatWavesExtreme__exc1_complete) +
  geom_line(aes(x=StrataLength, y=myORtable2_HeatWavesExtreme__exc1_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable2_HeatWavesExtreme__exc1_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable2_HeatWavesExtreme__exc1_complete[,3], ymax=myORtable2_HeatWavesExtreme__exc1_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2020 Mort HeatWavesExtreme exc=1",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)


myORtable0_HeatWavesExtreme__exc2 <- data.frame()
myORtable1_HeatWavesExtreme__exc2 <- data.frame()
myORtable2_HeatWavesExtreme__exc2 <- data.frame()

for (i in (c(7:28))) {
  model0_HeatWavesExtreme__exc2 = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 2, data=PHFexc_Hot)
  model1_HeatWavesExtreme__exc2 = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 2, data=PHF519exc_Hot)
  model2_HeatWavesExtreme__exc2 = casecross(Mort ~ HeatWavesExtreme+OzoneREVISED+PM2.5REVISED, stratalength = i, exclusion = 2, data=PHF20exc_Hot)
  
  sum0_HeatWavesExtreme__exc2 <- summary(model0_HeatWavesExtreme__exc2)
  coef0_HeatWavesExtreme__exc2 = sum0_HeatWavesExtreme__exc2[1]
  se0_HeatWavesExtreme__exc2 = sum0_HeatWavesExtreme__exc2[7]
  
  sum1_HeatWavesExtreme__exc2 <- summary(model1_HeatWavesExtreme__exc2)
  coef1_HeatWavesExtreme__exc2 = sum1_HeatWavesExtreme__exc2[1]
  se1_HeatWavesExtreme__exc2 = sum1_HeatWavesExtreme__exc2[7]
  
  sum2_HeatWavesExtreme__exc2 <- summary(model2_HeatWavesExtreme__exc2)
  coef2_HeatWavesExtreme__exc2 = sum2_HeatWavesExtreme__exc2[1]
  se2_HeatWavesExtreme__exc2 = sum2_HeatWavesExtreme__exc2[7]
  
  myORtable0_HeatWavesExtreme__exc2[i,1] = 0
  myORtable0_HeatWavesExtreme__exc2[i,2] = sum0_HeatWavesExtreme__exc2[1,2] #sum[3] = OR
  myORtable0_HeatWavesExtreme__exc2[i,3] = exp(coef0_HeatWavesExtreme__exc2+(1.96*se0_HeatWavesExtreme__exc2))
  myORtable0_HeatWavesExtreme__exc2[i,4] = exp(coef0_HeatWavesExtreme__exc2-(1.96*se0_HeatWavesExtreme__exc2))
  colnames(myORtable0_HeatWavesExtreme__exc2) <- c("Model","OR","UpperCI","LowerCI")    
  
  myORtable1_HeatWavesExtreme__exc2[i,1] = 1
  myORtable1_HeatWavesExtreme__exc2[i,2] = sum1_HeatWavesExtreme__exc2[1,2] #sum[3] = OR
  myORtable1_HeatWavesExtreme__exc2[i,3] = exp(coef1_HeatWavesExtreme__exc2+(1.96*se1_HeatWavesExtreme__exc2))
  myORtable1_HeatWavesExtreme__exc2[i,4] = exp(coef1_HeatWavesExtreme__exc2-(1.96*se1_HeatWavesExtreme__exc2))
  colnames(myORtable1_HeatWavesExtreme__exc2) <- c("Model","OR","UpperCI","LowerCI")
  
  myORtable2_HeatWavesExtreme__exc2[i,1] = 2
  myORtable2_HeatWavesExtreme__exc2[i,2] = sum2_HeatWavesExtreme__exc2[1,2] #sum[3] = OR
  myORtable2_HeatWavesExtreme__exc2[i,3] = exp(coef2_HeatWavesExtreme__exc2+(1.96*se2_HeatWavesExtreme__exc2))
  myORtable2_HeatWavesExtreme__exc2[i,4] = exp(coef2_HeatWavesExtreme__exc2-(1.96*se2_HeatWavesExtreme__exc2))
  colnames(myORtable2_HeatWavesExtreme__exc2) <- c("Model","OR","UpperCI","LowerCI")
}

#Full Mort

myORtable0_HeatWavesExtreme__exc2_complete <- myORtable0_HeatWavesExtreme__exc2[complete.cases(myORtable0_HeatWavesExtreme__exc2), ]
myORtable0_HeatWavesExtreme__exc2_complete[,1] <- seq(1:length(myORtable0_HeatWavesExtreme__exc2_complete$Model))
colnames(myORtable0_HeatWavesExtreme__exc2_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable0_HeatWavesExtreme__exc2_complete$StrataLength = myORtable0_HeatWavesExtreme__exc2_complete$StrataLength + 6

Full_exc2 <- ggplot(myORtable0_HeatWavesExtreme__exc2_complete) +
  geom_line(aes(x=StrataLength, y=myORtable0_HeatWavesExtreme__exc2_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable0_HeatWavesExtreme__exc2_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable0_HeatWavesExtreme__exc2_complete[,3], ymax=myORtable0_HeatWavesExtreme__exc2_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF Full Mort HeatWavesExtreme exc=2",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2005-2019 Data

myORtable1_HeatWavesExtreme__exc2_complete <- myORtable1_HeatWavesExtreme__exc2[complete.cases(myORtable1_HeatWavesExtreme__exc2), ]
myORtable1_HeatWavesExtreme__exc2_complete[,1] <- seq(1:length(myORtable1_HeatWavesExtreme__exc2_complete$Model))
colnames(myORtable1_HeatWavesExtreme__exc2_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable1_HeatWavesExtreme__exc2_complete$StrataLength = myORtable1_HeatWavesExtreme__exc2_complete$StrataLength + 6

Big_exc2 <- ggplot(myORtable1_HeatWavesExtreme__exc2_complete) +
  geom_line(aes(x=StrataLength, y=myORtable1_HeatWavesExtreme__exc2_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable1_HeatWavesExtreme__exc2_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable1_HeatWavesExtreme__exc2_complete[,3], ymax=myORtable1_HeatWavesExtreme__exc2_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2005-2019 Mort HeatWavesExtreme exc=2",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

#2020 Data

myORtable2_HeatWavesExtreme__exc2_complete <- myORtable2_HeatWavesExtreme__exc2[complete.cases(myORtable2_HeatWavesExtreme__exc2), ]
myORtable2_HeatWavesExtreme__exc2_complete[,1] <- seq(1:length(myORtable2_HeatWavesExtreme__exc2_complete$Model))
colnames(myORtable2_HeatWavesExtreme__exc2_complete) <- c("StrataLength","OR","UpperCI","LowerCI")
myORtable2_HeatWavesExtreme__exc2_complete$StrataLength = myORtable2_HeatWavesExtreme__exc2_complete$StrataLength + 6

Little_exc2 <- ggplot(myORtable2_HeatWavesExtreme__exc2_complete) +
  geom_line(aes(x=StrataLength, y=myORtable2_HeatWavesExtreme__exc2_complete[,2]), stat="identity", alpha=1, size=1) +
  geom_point(aes(x=StrataLength, y=myORtable2_HeatWavesExtreme__exc2_complete[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable2_HeatWavesExtreme__exc2_complete[,3], ymax=myORtable2_HeatWavesExtreme__exc2_complete[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="PHF 2020 Mort HeatWavesExtreme exc=2",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  ylim(0.7,1.3)

png("CaseCross_HeatWavesExtreme_PHF.png",width = 1000, height = 800)
ggarrange(Full_exc0,Full_exc1,Full_exc2,Big_exc0,Big_exc1,Big_exc2,Little_exc0,Little_exc1,Little_exc2)
dev.off()


