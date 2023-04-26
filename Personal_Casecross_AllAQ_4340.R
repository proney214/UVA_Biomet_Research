#Run casecross for different MDC categories ALL AQ

RIC_Final_Weather <- read_excel("~/Desktop/RIC.Final.Weather.xlsx")
RIC <- RIC_Final_Weather[1:5844,]

RIC %>% mutate_if(is.character, as.numeric)
RIC$HotDaysExtreme <- as.numeric(RIC$HotDaysExtreme)

RICProfileMort <- RIC[ ,107:149] 
RICProfileMDC <- RICProfileMort[ ,1:27]
RICProfileHotCold <- RIC[ , 92:103]
RIC$date <- as.Date(RIC$Date)
RICselect <- cbind(RICProfileHotCold,RICProfileMort)

model0 = casecross(RIC$MDC.04 ~ HotDaysModerate+OzoneREVISED+PM2.5REVISED, data=RIC)
model1 = casecross(RIC$MDC.04 ~ ColdDaysModerate+OzoneREVISED+PM2.5REVISED, data=RIC)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[7]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="RIC MDC.04 OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")+
  ylim(0.85,1.15)

#MDC05
model0 = casecross(RIC$MDC.05 ~ HotDaysModerate+OzoneREVISED+PM2.5REVISED, data=RIC)
model1 = casecross(RIC$MDC.05 ~ ColdDaysModerate+OzoneREVISED+PM2.5REVISED, data=RIC)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[7]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="RIC MDC.05 OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")+
  ylim(0.85,1.15)

#MDC01
model0 = casecross(RIC$MDC.01 ~ HotDaysModerate+OzoneREVISED+PM2.5REVISED, data=RIC)
model1 = casecross(RIC$MDC.01 ~ ColdDaysModerate+OzoneREVISED+PM2.5REVISED, data=RIC)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[7]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="RIC MDC.01 OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")+
  ylim(0.85,1.15)

#Mort
model0 = casecross(RIC$Mort ~ HotDaysModerate+OzoneREVISED+PM2.5REVISED, data=RIC)
model1 = casecross(RIC$Mort ~ ColdDaysModerate+OzoneREVISED+PM2.5REVISED, data=RIC)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[7]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="RIC Total Mortality OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")+
  ylim(0.85,1.15)


#Run casecross for different MDC categories NO AQ

SHD_Final_Weather <- read_excel("~/Desktop/SHD.Final.Weather.xlsx")
SHD <- SHD_Final_Weather[1:5844,]

SHD %>% mutate_if(is.character, as.numeric)
SHD$HotDaysExtreme <- as.numeric(SHD$HotDaysExtreme)

SHDProfileMort <- SHD[ ,107:149] 
SHDProfileMDC <- SHDProfileMort[ ,1:27]
SHDProfileHotCold <- SHD[ , 92:103]
SHD$date <- as.Date(SHD$Date)
SHDselect <- cbind(SHDProfileHotCold,SHDProfileMort)

model0 = casecross(SHD$MDC.04 ~ HotDaysModerate, data=SHD)
model1 = casecross(SHD$MDC.04 ~ ColdDaysModerate, data=SHD)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[3]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[3]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="SHD MDC.04 OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")+
  ylim(0.75,1.15)

#MDC05
model0 = casecross(SHD$MDC.05 ~ HotDaysModerate, data=SHD)
model1 = casecross(SHD$MDC.05 ~ ColdDaysModerate, data=SHD)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[3]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[3]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="SHD MDC.05 OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")+
  ylim(0.85,1.15)

#MDC01
model0 = casecross(SHD$MDC.01 ~ HotDaysModerate, data=SHD)
model1 = casecross(SHD$MDC.01 ~ ColdDaysModerate, data=SHD)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[3]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[3]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="SHD MDC.01 OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")+
  ylim(0.75,1.35)

#Mort
model0 = casecross(SHD$Mort ~ HotDaysModerate, data=SHD)
model1 = casecross(SHD$Mort ~ ColdDaysModerate, data=SHD)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[3]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[3]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="SHD Total Mortality OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")+
  ylim(0.85,1.15)

##############Run casecross for different MDC categories ONE AQ

model0 = casecross(EZF$MDC.04 ~ HotDaysModerate+OzoneREVISED, data=EZF)
model1 = casecross(EZF$MDC.04 ~ ColdDaysModerate+OzoneREVISED, data=EZF)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[5]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[5]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="EZF MDC.04 OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")+
  ylim(0.85,1.15)

#MDC05
model0 = casecross(EZF$MDC.05 ~ HotDaysModerate+OzoneREVISED, data=EZF)
model1 = casecross(EZF$MDC.05 ~ ColdDaysModerate+OzoneREVISED, data=EZF)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[5]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[5]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="EZF MDC.05 OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")+
  ylim(0.85,1.15)

#MDC01
model0 = casecross(EZF$MDC.01 ~ HotDaysModerate+OzoneREVISED, data=EZF)
model1 = casecross(EZF$MDC.01 ~ ColdDaysModerate+OzoneREVISED, data=EZF)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[5]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[5]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="EZF MDC.01 OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")+
  ylim(0.85,1.15)

#Mort
model0 = casecross(EZF$Mort ~ HotDaysModerate+OzoneREVISED, data=EZF)
model1 = casecross(EZF$Mort ~ ColdDaysModerate+OzoneREVISED, data=EZF)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[5]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[5]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="EZF Total Mortality OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")+
  ylim(0.85,1.15)




