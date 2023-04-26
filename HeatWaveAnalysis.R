#case crossover

#install.packages("season")
library(season)
library(readxl)
library(ggplot2)

RIC=read_excel("~/Desktop/RIC.working.apr-sept.xlsx", sheet="Sheet1",na="NA")
RIC$date=as.Date(RIC$date)

#**Moderate Heatwave Analysis**
myORtableModerateHW <- data.frame(matrix(data = NA, nrow = 24, ncol = 10))
colnames(myORtableModerateHW) <- c("StrataLength","ORex0","ORex1","ORex2","UpperCIex0","UpperCIex1","UpperCIex2","LowerCIex0","LowerCIex1","LowerCIex2")

for(i in 7:30) {
  for(j in 0:2) {
    output <- casecross(AdjustedMort ~ HeatWavesModerate, exclusion = j,stratalength = i, matchdow=FALSE, data=RIC)
    sum = summary(output)
    coef = sum[1]
    se = sum[3]
    myORtableModerateHW[i-6,1] = i
    myORtableModerateHW[i-6,j+2] = sum[2] #sum[3] = OR
    myORtableModerateHW[i-6,j+5] = exp(coef+(1.96*se))
    myORtableModerateHW[i-6,j+8] = exp(coef-(1.96*se))
  }
}

# Load ggplot2
ORtableRIC <- myORtableModerateHW
ORtableRIC <- data.frame(ORtableRIC)

# Most basic error bar
for(i in 0:2) {
print(ggplot(ORtableRIC) +
  geom_point( aes(x=StrataLength, y=ORtableRIC[,i+2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar( aes(x=StrataLength, ymin=ORtableRIC[,i+5], ymax=ORtableRIC[,i+8]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  ggtitle(paste0("RIC Heat Waves Moderate Lag0 (exclusion=",i,")")))
}


#Moderate Heat Analysis for Lag1
myORtableModerateHWlag1 <- data.frame(matrix(data = NA, nrow = 24, ncol = 10))
colnames(myORtableModerateHWlag1) <- c("StrataLength","ORex0lag1","ORex1lag1","ORex2lag1","UpperCIex0lag1","UpperCIex1lag1","UpperCIex2lag1","LowerCIex0lag1","LowerCIex1lag1","LowerCIex2lag1")

for(i in 7:30) {
  for(j in 0:2) {
    output <- casecross(AdjMortLag1 ~ HeatWavesModerate, exclusion = j,stratalength = i, matchdow=FALSE, data=RIC)
    sum = summary(output)
    coef = sum[1]
    se = sum[3]
    myORtableModerateHWlag1[i-6,1] = i
    myORtableModerateHWlag1[i-6,j+2] = sum[2] #sum[3] = OR
    myORtableModerateHWlag1[i-6,j+5] = exp(coef+(1.96*se))
    myORtableModerateHWlag1[i-6,j+8] = exp(coef-(1.96*se))
  }
}

# Plots
ORtablelag1RIC <- myORtableModerateHWlag1
ORtablelag1RIC <- data.frame(ORtablelag1RIC)

# Most basic error bar
ggplot(ORtablelag1RIC) +
  geom_point( aes(x=StrataLength, y=ORex1lag1), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar( aes(x=StrataLength, ymin=LowerCIex1lag1, ymax=UpperCIex1lag1), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  ggtitle("RIC Heat Waves Moderate Lag1 (exclusion=1)")

#Moderate Heat Analysis for Lag2
myORtableModerateHWlag2 <- data.frame(matrix(data = NA, nrow = 24, ncol = 10))
colnames(myORtableModerateHWlag2) <- c("StrataLength","ORex0lag2","ORex1lag2","ORex2lag2","UpperCIex0lag2","UpperCIex1lag2","UpperCIex2lag2","LowerCIex0lag2","LowerCIex1lag2","LowerCIex2lag2")

for(i in 7:30) {
  for(j in 0:2) {
    output <- casecross(AdjMortLag2 ~ HeatWavesModerate, exclusion = j,stratalength = i, matchdow=FALSE, data=RIC)
    sum = summary(output)
    coef = sum[1]
    se = sum[3]
    myORtableModerateHWlag2[i-6,1] = i
    myORtableModerateHWlag2[i-6,j+2] = sum[2] #sum[3] = OR
    myORtableModerateHWlag2[i-6,j+5] = exp(coef+(1.96*se))
    myORtableModerateHWlag2[i-6,j+8] = exp(coef-(1.96*se))
  }
}

# Plots
ORtablelag2RIC <- myORtableModerateHWlag2
ORtablelag2RIC <- data.frame(ORtablelag2RIC)

# Most basic error bar
ggplot(ORtablelag2RIC) +
  geom_point( aes(x=StrataLength, y=ORex1lag2), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar( aes(x=StrataLength, ymin=LowerCIex1lag2, ymax=UpperCIex1lag2), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  ggtitle("RIC Heat Waves Moderate Lag2 (exclusion=1)")


#*&*& Severe Heatwave analysis
myORtableSevereHW <- data.frame(matrix(data = NA, nrow = 24, ncol = 10))
colnames(myORtableSevereHW) <- c("StrataLength","ORex0","ORex1","ORex2","UpperCIex0","UpperCIex1","UpperCIex2","LowerCIex0","LowerCIex1","LowerCIex2")

for(i in 7:30) {
  for(j in 0:2) {
    output <- casecross(AdjustedMort ~ HeatWavesSevere, exclusion = j,stratalength = i, matchdow=FALSE, data=RIC)
    sum = summary(output)
    coef = sum[1]
    se = sum[3]
    myORtableSevereHW[i-6,1] = i
    myORtableSevereHW[i-6,j+2] = sum[2] #sum[3] = OR
    myORtableSevereHW[i-6,j+5] = exp(coef+(1.96*se))
    myORtableSevereHW[i-6,j+8] = exp(coef-(1.96*se))
  }
}

# Load ggplot2
ORtableRIC <- myORtableSevereHW
ORtableRIC <- data.frame(ORtableRIC)

# Most basic error bar
ggplot(ORtableRIC) +
  geom_point( aes(x=StrataLength, y=ORex0), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar( aes(x=StrataLength, ymin=LowerCIex0, ymax=UpperCIex0), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  ggtitle("RIC Heat Waves Severe Lag0 (exclusion=0)")

#Severe Heat Analysis for Lag1
myORtableSevereHWlag1 <- data.frame(matrix(data = NA, nrow = 24, ncol = 10))
colnames(myORtableSevereHWlag1) <- c("StrataLength","ORex0lag1","ORex1lag1","ORex2lag1","UpperCIex0lag1","UpperCIex1lag1","UpperCIex2lag1","LowerCIex0lag1","LowerCIex1lag1","LowerCIex2lag1")

for(i in 7:30) {
  for(j in 0:2) {
    output <- casecross(AdjMortLag1 ~ HeatWavesSevere, exclusion = j,stratalength = i, matchdow=FALSE, data=RIC)
    sum = summary(output)
    coef = sum[1]
    se = sum[3]
    myORtableSevereHWlag1[i-6,1] = i
    myORtableSevereHWlag1[i-6,j+2] = sum[2] #sum[3] = OR
    myORtableSevereHWlag1[i-6,j+5] = exp(coef+(1.96*se))
    myORtableSevereHWlag1[i-6,j+8] = exp(coef-(1.96*se))
  }
}

# Plots
ORtablelag1RIC <- myORtableSevereHWlag1
ORtablelag1RIC <- data.frame(ORtablelag1RIC)

# Most basic error bar
ggplot(ORtablelag1RIC) +
  geom_point( aes(x=StrataLength, y=ORex1lag1), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar( aes(x=StrataLength, ymin=LowerCIex1lag1, ymax=UpperCIex1lag1), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  ggtitle("RIC Heat Waves Severe Lag1 (exclusion=1)")

#Severe Heat Analysis for Lag2
myORtableSevereHWlag2 <- data.frame(matrix(data = NA, nrow = 24, ncol = 10))
colnames(myORtableSevereHWlag2) <- c("StrataLength","ORex0lag2","ORex1lag2","ORex2lag2","UpperCIex0lag2","UpperCIex1lag2","UpperCIex2lag2","LowerCIex0lag2","LowerCIex1lag2","LowerCIex2lag2")

for(i in 7:30) {
  for(j in 0:2) {
    output <- casecross(AdjMortLag2 ~ HeatWavesSevere, exclusion = j,stratalength = i, matchdow=FALSE, data=RIC)
    sum = summary(output)
    coef = sum[1]
    se = sum[3]
    myORtableSevereHWlag2[i-6,1] = i
    myORtableSevereHWlag2[i-6,j+2] = sum[2] #sum[3] = OR
    myORtableSevereHWlag2[i-6,j+5] = exp(coef+(1.96*se))
    myORtableSevereHWlag2[i-6,j+8] = exp(coef-(1.96*se))
  }
}

# Plots
ORtablelag2RIC <- myORtableSevereHWlag2
ORtablelag2RIC <- data.frame(ORtablelag2RIC)

# Most basic error bar
ggplot(ORtablelag2RIC) +
  geom_point( aes(x=StrataLength, y=ORex1lag2), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar( aes(x=StrataLength, ymin=LowerCIex1lag2, ymax=UpperCIex1lag2), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  ggtitle("RIC Heat Waves Severe Lag2 (exclusion=1)")


#**Extreme Heatwave Analysis**

myORtableExtremeHW <- data.frame(matrix(data = NA, nrow = 24, ncol = 10))
colnames(myORtableExtremeHW) <- c("StrataLength","ORex0","ORex1","ORex2","UpperCIex0","UpperCIex1","UpperCIex2","LowerCIex0","LowerCIex1","LowerCIex2")

for(i in 7:30) {
  for(j in 0:2) {
    output <- casecross(AdjustedMort ~ HeatWavesExtreme, exclusion = j,stratalength = i, matchdow=FALSE, data=RIC)
    sum = summary(output)
    coef = sum[1]
    se = sum[3]
    myORtableExtremeHW[i-6,1] = i
    myORtableExtremeHW[i-6,j+2] = sum[2] #sum[3] = OR
    myORtableExtremeHW[i-6,j+5] = exp(coef+(1.96*se))
    myORtableExtremeHW[i-6,j+8] = exp(coef-(1.96*se))
  }
}

# Load ggplot2

ORtableRIC <- myORtableExtremeHW
ORtableRIC <- data.frame(ORtableRIC)

# Most basic error bar
ggplot(ORtableRIC) +
  geom_point( aes(x=StrataLength, y=ORex0), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar( aes(x=StrataLength, ymin=LowerCIex0, ymax=UpperCIex0), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  ggtitle("RIC Heat Waves Extreme Lag0 (exclusion=0)")

#Extreme Heat Analysis for Lag1
myORtableExtremeHWlag1 <- data.frame(matrix(data = NA, nrow = 24, ncol = 10))
colnames(myORtableExtremeHWlag1) <- c("StrataLength","ORex0lag1","ORex1lag1","ORex2lag1","UpperCIex0lag1","UpperCIex1lag1","UpperCIex2lag1","LowerCIex0lag1","LowerCIex1lag1","LowerCIex2lag1")

for(i in 7:30) {
  for(j in 0:2) {
    output <- casecross(AdjMortLag1 ~ HeatWavesExtreme, exclusion = j,stratalength = i, matchdow=FALSE, data=RIC)
    sum = summary(output)
    coef = sum[1]
    se = sum[3]
    myORtableExtremeHWlag1[i-6,1] = i
    myORtableExtremeHWlag1[i-6,j+2] = sum[2] #sum[3] = OR
    myORtableExtremeHWlag1[i-6,j+5] = exp(coef+(1.96*se))
    myORtableExtremeHWlag1[i-6,j+8] = exp(coef-(1.96*se))
  }
}

# Plots
ORtablelag1RIC <- myORtableExtremeHWlag1
ORtablelag1RIC <- data.frame(ORtablelag1RIC)

# Most basic error bar
ggplot(ORtablelag1RIC) +
  geom_point( aes(x=StrataLength, y=ORex1lag1), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar( aes(x=StrataLength, ymin=LowerCIex1lag1, ymax=UpperCIex1lag1), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  ggtitle("RIC Heat Waves Extreme Lag1 (exclusion=1)")

#Extreme Heat Analysis for Lag2
myORtableExtremeHWlag2 <- data.frame(matrix(data = NA, nrow = 24, ncol = 10))
colnames(myORtableExtremeHWlag2) <- c("StrataLength","ORex0lag2","ORex1lag2","ORex2lag2","UpperCIex0lag2","UpperCIex1lag2","UpperCIex2lag2","LowerCIex0lag2","LowerCIex1lag2","LowerCIex2lag2")

for(i in 7:30) {
  for(j in 0:2) {
    output <- casecross(AdjMortLag2 ~ HeatWavesExtreme, exclusion = j,stratalength = i, matchdow=FALSE, data=RIC)
    sum = summary(output)
    coef = sum[1]
    se = sum[3]
    myORtableExtremeHWlag2[i-6,1] = i
    myORtableExtremeHWlag2[i-6,j+2] = sum[2] #sum[3] = OR
    myORtableExtremeHWlag2[i-6,j+5] = exp(coef+(1.96*se))
    myORtableExtremeHWlag2[i-6,j+8] = exp(coef-(1.96*se))
  }
}

# Plots
ORtablelag2RIC <- myORtableExtremeHWlag2
ORtablelag2RIC <- data.frame(ORtablelag2RIC)

# Most basic error bar
ggplot(ORtablelag2RIC) +
  geom_point( aes(x=StrataLength, y=ORex1lag2), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar( aes(x=StrataLength, ymin=LowerCIex1lag2, ymax=UpperCIex1lag2), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  ggtitle("RIC Heat Waves Extreme Lag2 (exclusion=1)")
