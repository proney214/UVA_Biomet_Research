### RIC Modeling

#Load in Data
library(splines)
library(dlnm)
library(lubridate)
library(readxl)
library(mgcv)
library(ggplot2)
library(dplyr)

RICF <- read_excel("~/Desktop/RIC.working.weatherprep.xlsx",sheet = "Sheet1",na="NA")

#Compute variables quickly from after loading df, 
for(i in 1:ncol(RICF)){
  assign(names(RICF)[i], RICF[[i]])
}

#Remove Unnecessary for Variables
RICfactors <- select(RICF,ColdDaysExtreme,ColdDaysModerate,ColdDaysSevere,ColdWavesExtreme,ColdWavesModerate,ColdWavesSevere,HotDaysExtreme,HotDaysModerate,HotDaysSevere,HeatWavesExtreme,HeatWavesModerate,HeatWavesSevere,holidays,dow,WinterWeather)
RIC <- select(RICF,-Station,-Date,-Year,-Month,-Day,-`MaxT(C)`,-`MaxTDep(C)`,-`MinT(C)`,-`MinTDep(C)`,-`DTR(C)`,-`T(C)1am`,-`Td(C)1am`,-`Tw(C)1am`,-`AT(C)1am`,-`THI(C)1am`,-`Hx(C)1am`,-`WC(C)1am`,-`T(C)7am`,-`Td(C)7am`,-`Tw(C)7am`,-`AT(C)7am`,-`THI(C)7am`,-`Hx(C)7am`,-`WC(C)7am`,-`T(C)1pm`,-`Td(C)1pm`,-`Tw(C)1pm`,-`AT(C)1pm`,-`THI(C)1pm`,-`Hx(C)1pm`,-`WC(C)1pm`,-`T(C)7pm`,-`Td(C)7pm`,-`Tw(C)7pm`,-`AT(C)7pm`,-`THI(C)7pm`,-`Hx(C)7pm`,-`WC(C)7pm`,ColdDaysExtreme,ColdDaysModerate,ColdDaysSevere,ColdWavesExtreme,ColdWavesModerate,ColdWavesSevere,HotDaysExtreme,HotDaysModerate,HotDaysSevere,HeatWavesExtreme,HeatWavesModerate,HeatWavesSevere,holidays,dow,WinterWeather)
                  

#Convert Data to numeric
RIC %>% mutate_if(is.character, as.numeric)
head(RIC)

#Create Trend Term
Trend = seq(1,5845,1)

#Test number of splines for Trend
modeltrend1=gam(mort~s(Trend,k=16*2),family="quasipoisson")
summary(modeltrend1)
plot(modeltrend1)
plot.gam(modeltrend1)

modeltrend2=gam(mort~s(Trend,k=16*3),family="quasipoisson")
summary(modeltrend2)
gam.check(modeltrend2)
plot(modeltrend2)

modeltrend3=gam(mort~s(Trend,k=16*4),family="quasipoisson")
summary(modeltrend3)
gam.check(modeltrend3)
plot(modeltrend3)

modeltrend4=gam(mort~s(Trend,k=16*5),family="quasipoisson")
summary(modeltrend4)
gam.check(modeltrend4)
plot(modeltrend4)

modeltrend5=gam(mort~s(Trend,k=16*7),family="quasipoisson")
summary(modeltrend5)
gam.check(modeltrend5)
plot(modeltrend5)


#Run every column of dataset through the model
#Add k (3:7) as a loop, include air quality (replace missing with the average values)
f <- function(col,RIC) {
  form = as.formula(paste0("mort~s(Trend,k=16*3)+s(",col,",k=7)"))
  data.frame(col = col, gcv = gam(form, data = RIC, family="quasipoisson")$gcv)
}

#Compile outputs of model table into list of GCV values
t <- do.call(rbind, lapply(c("MaxTF","MinTF","MaxTDepF","MinTDepF","DTRF","TF1am","TwF1am","TdF1am","RH1am","Speedkts1am","SLPhPa1am","ehPa1am","esubshPa1am","ATF1am","THIF1am","HxF1am","WCF1am","TF7am","TwF7am","TdF7am","RH7am","Speedkts7am","SLPhPa7am","ehPa7am","esubshPa7am","ATF7am","THIF7am","HxF7am","WCF7am","TF1pm","TwF1pm","TdF1pm","RH1pm","Speedkts1pm","SLPhPa1pm","ehPa1pm","esubshPa1pm","ATF1pm","THIF1pm","HxF1pm","WCF1pm","TF7pm","TwF7pm","TdF7pm","RH7pm","Speedkts7pm","SLPhPa7pm","ehPa7pm","esubshPa7pm","ATF7pm","THIF7pm","HxF7pm","WCF7pm"), f, RIC=RIC))

#Find minimum GCV and variable associated with it
UseThisVariable <- t[which.min(t$gcv),]
View(UseThisVariable)
#Add in plot for UseThisVariable

**Using as.factors**
# Add more variables to base model to select "best" model for DLNM
#Adding in other as.factors to minimum variable found above

model0=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3),family="quasipoisson")
a=summary(model0)
#plot(model0)
  
model1=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(dow),family="quasipoisson")
A=summary(model1)
#plot(model1)

model2=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(holidays),family="quasipoisson")
B=summary(model2)
#plot(model2)

model3=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(dow)+as.factor(holidays),family="quasipoisson")
C=summary(model3)
#plot(model3)

model4=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(HeatWavesModerate),family="quasipoisson")
D=summary(model4)
#plot(model4)

model5=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(HeatWavesSevere),family="quasipoisson")
E=summary(model5)
#plot(model5)

model6=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(HeatWavesExtreme),family="quasipoisson")
F=summary(model6)
#plot(model6)

model7=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(ColdWavesModerate),family="quasipoisson")
G=summary(model7)
#plot(model7)

model8=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(ColdWavesSevere),family="quasipoisson")
H=summary(model8)
#plot(model8)

model9=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(ColdWavesExtreme),family="quasipoisson")
I=summary(model9)
#plot(model9)

model10=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(WinterWeather),family="quasipoisson")
J=summary(model10)
#plot(model10)

model11=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(HotDaysExtreme),family="quasipoisson")
K=summary(model11)
#plot(model11)

model12=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(dow)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")
L=summary(model12)
#plot(model12)

model13=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")
M=summary(model13)
#plot(model13)

model14=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
N=summary(model14)
#plot(model14)

model15=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(dow)+as.factor(HeatWavesModerate),family="quasipoisson")
O=summary(model15)
#plot(model15)

model16=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(dow)+as.factor(ColdWavesModerate),family="quasipoisson")
P=summary(model16)
#plot(model16)

model17=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(WinterWeather)+as.factor(ColdWavesModerate),family="quasipoisson")
Q=summary(model17)
#plot(model17)

model18=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+as.factor(holidays)+as.factor(ColdWavesModerate),family="quasipoisson")
R=summary(model18)
#plot(model18)

GAMbase <-matrix(c(a$sp.criterion,a$dev.expl,A$sp.criterion,A$dev.expl,B$sp.criterion,B$dev.expl,C$sp.criterion,C$dev.expl,D$sp.criterion,D$dev.expl,E$sp.criterion,E$dev.expl,F$sp.criterion,F$dev.expl,G$sp.criterion,G$dev.expl,H$sp.criterion,H$dev.expl,I$sp.criterion,I$dev.expl,J$sp.criterion,J$dev.expl,K$sp.criterion,K$dev.expl,L$sp.criterion,L$dev.expl,M$sp.criterion,M$dev.expl,N$sp.criterion,N$dev.expl,O$sp.criterion,O$dev.expl,P$sp.criterion,P$dev.expl,Q$sp.criterion,Q$dev.expl,R$sp.criterion,R$dev.expl),ncol=2,byrow=TRUE)
rownames(GAMbase)<-c('blank','dow','holidays','dow+holidays','HeatWavesModerate','HeatWavesSevere','HeatWavesExtreme','ColdWavesModerate','ColdWavesSevere','ColdWavesExtreme','WinterWeather','HotDaysExtreme','dow+holidays+WinterWeather','holidays+WinterWeather','dow+WinterWeather','dow+HeatWavesModerate','dow+ColdWavesModerate','WinterWeather+ColdWavesModerate','holidays+ColdWavesModerate')
colnames(GAMbase)<-c('GCV','Dev.Exp.')
GAMbase <- as.data.frame(GAMbase)
head(GAMbase)

UseThisFactor <- GAMbase[which.min(GAMbase$GCV),]
View(UseThisFactor)

### Test Best Model
model99=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+s(DTRF,k=3)+as.factor(holidays)+as.factor(ColdWavesModerate),family="quasipoisson")
S=summary(model99)
#plot(model99)

model98=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+s(MaxTF,k=3)+as.factor(holidays)+as.factor(ColdWavesModerate),family="quasipoisson")
T=summary(model98)
#plot(model99)

model97=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+s(Ozone,k=3)+as.factor(holidays)+as.factor(ColdWavesModerate),family="quasipoisson")
U=summary(model97)
plot(model97)

model96=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+s(PM2.5,k=3)+as.factor(holidays)+as.factor(ColdWavesModerate),family="quasipoisson")
V=summary(model96)
#plot(model99)

model95=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+s(PM2.5,k=3)+s(Ozone,k=3)+as.factor(holidays)+as.factor(ColdWavesModerate),family="quasipoisson")
W=summary(model95)
#plot(model99)


Finalbase <-matrix(c(S$sp.criterion,S$dev.expl,T$sp.criterion,T$dev.expl,U$sp.criterion,U$dev.expl,V$sp.criterion,V$dev.expl,W$sp.criterion,W$dev.expl),ncol=2,byrow=TRUE)
rownames(Finalbase)<-c('model99','model98','model97','model96','model95')
colnames(Finalbase)<-c('GCV','Dev.Exp.')
Finalbase <- as.data.frame(Finalbase)
head(Finalbase)

UseThisModel <- Finalbase[which.min(Finalbase$GCV),]
View(UseThisModel)
#Model97 creates best graph: model97=gam(mort~s(Trend,k=16*3)+s(RH7pm,k=3)+s(Ozone,k=3)+as.factor(holidays)+as.factor(ColdWavesModerate),family="quasipoisson")


