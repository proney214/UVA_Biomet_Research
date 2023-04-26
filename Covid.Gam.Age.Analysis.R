### PHF Modeling

#Load in Data
library(splines)
library(dlnm)
library(lubridate)
library(readxl)
library(mgcv)
library(ggplot2)
library(dplyr)
library(zoo)
library(dplyr)
library(stringr)
library(padr)
library(ggplot2)
library(tidyr)
library(padr)

#Prepare Age Data, Load in and prepare filter for dataset
BigMortEx <- read_excel("~/Desktop/BigMort.xlsx", 
                        sheet = "Sheet1")
BigMort <- BigMortEx
BigMort$Date = as.Date(paste(BigMort$DOD_YR,BigMort$DOD_MO,BigMort$DOD_DY,sep="-"))
BigMort$`Station ID` <- str_replace_all(BigMort$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))

#Single Station Filter command
PHFmort <- BigMort %>%
  filter(BigMort$`Station ID` == "PHF") 

PHFmort <- select(PHFmort,Count,Date,AGE_GROUPS, SEX)

#Age Analysis
PHFgroup <- PHFmort %>%
  filter(AGE_GROUPS == '75+') %>%
  group_by(Date) %>%
  summarise(total75=sum(Count))

#Begin typical analysis
PHFF <- read_excel("~/Desktop/PHF.working.weatherprep.xlsx",sheet = "Sheet1",na="NA")

Year <- PHFF$Year
Month <- PHFF$Month
Day <- PHFF$Month

#Remove Unnecessary for Variables
PHFfactors <- select(PHFF,ColdDaysExtreme,ColdDaysModerate,ColdDaysSevere,ColdWavesExtreme,ColdWavesModerate,ColdWavesSevere,HotDaysExtreme,HotDaysModerate,HotDaysSevere,HeatWavesExtreme,HeatWavesModerate,HeatWavesSevere,holidays,dow,WinterWeather)
PHF <- select(PHFF,-Station,-Date,-Year,-Month,-Day,-`MaxT(C)`,-`MaxTDep(C)`,-`MinT(C)`,-`MinTDep(C)`,-`DTR(C)`,-`T(C)1am`,-`Td(C)1am`,-`Tw(C)1am`,-`AT(C)1am`,-`THI(C)1am`,-`Hx(C)1am`,-`WC(C)1am`,-`T(C)7am`,-`Td(C)7am`,-`Tw(C)7am`,-`AT(C)7am`,-`THI(C)7am`,-`Hx(C)7am`,-`WC(C)7am`,-`T(C)1pm`,-`Td(C)1pm`,-`Tw(C)1pm`,-`AT(C)1pm`,-`THI(C)1pm`,-`Hx(C)1pm`,-`WC(C)1pm`,-`T(C)7pm`,-`Td(C)7pm`,-`Tw(C)7pm`,-`AT(C)7pm`,-`THI(C)7pm`,-`Hx(C)7pm`,-`WC(C)7pm`,ColdDaysExtreme,ColdDaysModerate,ColdDaysSevere,ColdWavesExtreme,ColdWavesModerate,ColdWavesSevere,HotDaysExtreme,HotDaysModerate,HotDaysSevere,HeatWavesExtreme,HeatWavesModerate,HeatWavesSevere,holidays,dow,WinterWeather)

#For age analysis, use this also to remove the total mort in original dataset
PHF <- select(PHF,-mort)

#Compute variables quickly from after loading df, 
for(i in 1:ncol(PHF)){
  assign(names(PHF)[i], PHF[[i]])
}

#Create Age Specific mort variable
df1 <- pad(PHFgroup,
           interval = "day",
           start_val = as.POSIXct('2005-01-01'),
           end_val = as.POSIXct('2020-12-31')
)

mort <- df1$total75

#Convert Data to numePHF
PHF %>% mutate_if(is.character, as.numeric)
dim(PHF)

#Create Trend Term
Trend = seq(1,5844,1)

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
test <- summary(modeltrend5)
gam.check(modeltrend5)
plot(modeltrend5)


#Run every column of dataset through the model
klist <- data.frame()
slist <- data.frame()
for (i in (3:7)) {
  print(i)
  f <- function(col,PHF) {
    form = as.formula(paste0("mort~s(Trend,k=16*3)+s(",col,",k=i)"))
    data.frame(col = col, gcv = gam(form, data = PHF, family="quasipoisson")$gcv,sig = summary(gam(form, data = PHF, family="quasipoisson"))$p.table[,4])
  }
  
  #Compile outputs of model table into list of GCV values
  t <- do.call(rbind, lapply(c("MaxTF","MinTF","MaxTDepF","MinTDepF","DTRF","TF1am","TwF1am","TdF1am","RH1am","Speedkts1am","SLPhPa1am","ehPa1am","esubshPa1am","ATF1am","THIF1am","HxF1am","WCF1am","TF7am","TwF7am","TdF7am","RH7am","Speedkts7am","SLPhPa7am","ehPa7am","esubshPa7am","ATF7am","THIF7am","HxF7am","WCF7am","TF1pm","TwF1pm","TdF1pm","RH1pm","Speedkts1pm","SLPhPa1pm","ehPa1pm","esubshPa1pm","ATF1pm","THIF1pm","HxF1pm","WCF1pm","TF7pm","TwF7pm","TdF7pm","RH7pm","Speedkts7pm","SLPhPa7pm","ehPa7pm","esubshPa7pm","ATF7pm","THIF7pm","HxF7pm","WCF7pm"), f, PHF=PHF))
  
  #Find minimum GCV and variable associated with it
  UseThisVariable <- t[which.min(t$gcv),]
  klist <- append(klist,t[which.min(t$gcv),]) #This is the variable created which lists index, gcv, UseThisVariable
  View(UseThisVariable)
  SigVar<- t[which.min(t$sig),]
  slist <- append(klist,t[which.min(t$sig),]) #This is the variable created which lists index, gcv, UseThisVariable
  View(SigVar)
  #Add in plot for UseThisVariable
  print(klist)
  print(slist)
}
print(klist)
list <- as.matrix(klist, ncol=2)
slist <- as.matrix(slist, ncol=2)
df <- as.table(list,ncol=1)


**Using as.factors**
  # Add more variables to base model to select "best" model for DLNM
  #Adding in other as.factors to minimum variable found above
  
model0=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3),family="quasipoisson")
a=summary(model0)
#plot(model0)

model1=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(dow),family="quasipoisson")
A=summary(model1)
#plot(model1)

model2=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(holidays),family="quasipoisson")
B=summary(model2)
#plot(model2)

model3=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(dow)+as.factor(holidays),family="quasipoisson")
C=summary(model3)
#plot(model3)

model4=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(HeatWavesModerate),family="quasipoisson")
D=summary(model4)
#plot(model4)

model5=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(HeatWavesSevere),family="quasipoisson")
E=summary(model5)
#plot(model5)

model6=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(HeatWavesExtreme),family="quasipoisson")
F=summary(model6)
#plot(model6)

model7=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(ColdWavesModerate),family="quasipoisson")
G=summary(model7)
#plot(model7)

model8=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(ColdWavesSevere),family="quasipoisson")
H=summary(model8)
#plot(model8)

model9=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(ColdWavesExtreme),family="quasipoisson")
I=summary(model9)
#plot(model9)

model10=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(WinterWeather),family="quasipoisson")
J=summary(model10)
#plot(model10)

model11=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(HotDaysExtreme),family="quasipoisson")
K=summary(model11)
#plot(model11)

model12=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(dow)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")
L=summary(model12)
#plot(model12)

model13=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")
M=summary(model13)
#plot(model13)

model14=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
N=summary(model14)
#plot(model14)

model15=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(dow)+as.factor(HeatWavesModerate),family="quasipoisson")
O=summary(model15)
#plot(model15)

model16=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(dow)+as.factor(ColdWavesModerate),family="quasipoisson")
P=summary(model16)
#plot(model16)

model17=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(WinterWeather)+as.factor(ColdWavesModerate),family="quasipoisson")
Q=summary(model17)
#plot(model17)

model18=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+as.factor(holidays)+as.factor(ColdWavesModerate),family="quasipoisson")
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
model99=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+s(DTRF,k=3)+as.factor(holidays)+as.factor(ColdWavesExtreme),family="quasipoisson")
S=summary(model99)
#plot(model99)

model98=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+s(MaxTF,k=3)+as.factor(holidays)+as.factor(ColdWavesExtreme),family="quasipoisson")
T=summary(model98)
#plot(model99)

model97=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+s(OzoneREVISED,k=3)+as.factor(holidays)+as.factor(ColdWavesExtreme),family="quasipoisson")
U=summary(model97)
#plot(model97)

model96=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+s(PM2.5REVISED,k=3)+as.factor(holidays)+as.factor(ColdWavesExtreme),family="quasipoisson")
V=summary(model96)
#plot(model99)

model95=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+s(PM2.5REVISED,k=3)+s(OzoneREVISED,k=3)+as.factor(holidays)+as.factor(ColdWavesExtreme),family="quasipoisson")
W=summary(model95)
#plot(model99)

model94=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+s(HxF1pm,k=3)+as.factor(ColdWavesSevere),family="quasipoisson")
X=summary(model94)
#plot(model98)

model93=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+s(SLPhPa7pm,k=3)+as.factor(ColdWavesSevere),family="quasipoisson")
Y=summary(model93)
#plot(model98)

Finalbase <-matrix(c(S$sp.criterion,S$dev.expl,T$sp.criterion,T$dev.expl,U$sp.criterion,U$dev.expl,V$sp.criterion,V$dev.expl,W$sp.criterion,W$dev.expl,X$sp.criterion,X$dev.expl,Y$sp.criterion,Y$dev.expl),ncol=2,byrow=TRUE)
rownames(Finalbase)<-c('model99','model98','model97','model96','model95','model94','model93')
colnames(Finalbase)<-c('GCV','Dev.Exp.')
Finalbase <- as.data.frame(Finalbase)
head(Finalbase)

UseThisModel <- Finalbase[which.min(Finalbase$GCV),]
View(UseThisModel)
#Model97 creates best graph for 75+: 
#model97=gam(mort~s(Trend,k=16*3)+s(SLPhPa7am,k=3)+s(OzoneREVISED,k=3)+as.factor(holidays)+as.factor(ColdWavesExtreme),family="quasipoisson")
plot(model97)
plot(mort,type = 'l',col='blue')
lines(rollmean(rollmean(mort,40),40),col='red')
abline(v=seq(0,5844,by = 365))

Ozone <- OzoneREVISED

# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(mort=mort, Trend=Trend, MinTF=MinTF,Ozone=Ozone,holidays=holidays, ColdWavesExtreme=ColdWavesExtreme, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numePHF(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MinTF=crossbasis(lagframe$MinTF,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)
basis.Ozone=crossbasis(lagframe$Ozone,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)
#basis.PM2.5=crossbasis(lagframe$PM2.5,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

# Define the position of knots for spline functions
varknotsMinTF=equalknots(lagframe$MinTF,fun="bs",df=4,degree=2)
varknotsMinTF
lagknotsMinTF=logknots(21,2)
lagknotsMinTF
varknotsOzone=equalknots(lagframe$Ozone,fun="bs",df=4,degree=2)
varknotsOzone
lagknotsOzone=logknots(21,2)
lagknotsOzone
#varknotsPM2.5=equalknots(lagframe$PM2.5,fun="bs",df=4,degree=2)
#varknotsPM2.5
#lagknotsPM2.5=logknots(21,2)
#lagknotsPM2.5

# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value
cb1.MinTF=crossbasis(lagframe$MinTF,lag=21,argvar=list(fun="bs",knots=varknotsMinTF),arglag=list(knots=lagknotsMinTF))
summary(cb1.MinTF)
cb1.Ozone=crossbasis(lagframe$Ozone,lag=21,argvar=list(fun="bs",knots=varknotsOzone),arglag=list(knots=lagknotsOzone))
summary(cb1.Ozone)
#cb1.PM2.5=crossbasis(lagframe$PM2.5,lag=21,argvar=list(fun="bs",knots=varknotsPM2.5),arglag=list(knots=lagknotsPM2.5))
#summary(cb1.PM2.5)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
modelA1=glm(mort~cb1.MinTF+cb1.Ozone+ns(Trend, 3*16)+as.factor(ColdWavesExtreme)+as.factor(holidays),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)
#Removed PM2.5

modelA2=glm(mort~cb1.MinTF+ns(Ozone, 3*16)+ns(Trend, 3*16)+as.factor(ColdWavesExtreme)+as.factor(holidays),family=quasipoisson(),lagframe)
summary(modelA2)

# Generate predictions from DLNM and plot results
pred1.MinTF=crosspred(cb1.MinTF,modelA1,by=1)
pred2.MinTF=crosspred(cb1.MinTF,modelA2,by=1)
#pred1.Ozone=crosspred(cb1.Ozone,modelA1,by=1)
#pred1.PM2.5=crosspred(cb1.PM2.5,modelA1,by=1)

# 3-D plot
plot(pred1.MinTF,xlab="MinTF",zlab="RR",theta=200,phi=40,lphi=30,main="PHF Mort")
plot(pred2.MinTF,xlab="MinTF",zlab="RR",theta=200,phi=40,lphi=30,main="PHF Mort Spline")
#plot(pred1.Ozone,xlab="Ozone",zlab="RR",theta=200,phi=40,lphi=30,main="PHF Mort")
#plot(pred1.PM2.5,xlab="PM2.5",zlab="RR",theta=200,phi=40,lphi=30,main="PHF Mort")

# Plot of lag effects at selected values ("var=??")
plot(pred1.MinTF,ptype = "slices",var=c(5),col='black',xlab="lag (days)",ylab="Relative Risk",main="Lag RR at 5 C")
plot(pred1.MinTF,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")
plot(pred1.MinTF,ptype="slices",var=c(20),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 20 HX")
plot(pred1.Ozone,ptype="slices",var=c(0.0),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 0")
plot(pred1.Ozone,ptype="slices",var=c(0.5),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 0.5")

# Plot of "basis" variables
cbind(pred1.MinTF$allRRfit,pred1.MinTF$allRRlow,pred1.MinTF$allRRhigh)
cbind(pred2.MinTF$allRRfit,pred2.MinTF$allRRlow,pred2.MinTF$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
plot(pred1.MinTF,"contour",xlab="MinTF",key.title=title("RR"),plot.title=title("mort Admissions",xlab="MinTF",ylab="Lag (days)"))
plot(pred2.MinTF,"contour",xlab="MinTF",key.title=title("RR"),plot.title=title("mort Admissions Spline",xlab="MinTF",ylab="Lag (days)"))


#plot(pred1.Ozone,"contour",xlab="Ozone",key.title=title("RR"),plot.title=title("mort Admissions",xlab="Ozone",ylab="Lag (days)"))
#plot(pred1.PM2.5,"contour",xlab="PM2.5",key.title=title("RR"),plot.title=title("mort Admissions",xlab="PM2.5",ylab="Lag (days)"))


#overall plot
plot(pred1.MinTF,"overall",xlab="MinT",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Overall cumulative association")
plot(pred2.MinTF,"overall",xlab="MinT",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Overall cumulative association Spline")

