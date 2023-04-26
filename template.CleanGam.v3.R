#Cleanest GAM model testing, allowing for mortality specifications (v.3)

#Load in Data
library(splines)
library(dlnm)
library(lubridate)
library(readxl)
library(mgcv)
library(ggplot2)
library(stringr)
library(dplyr)
library(padr)

RICF <- read_excel("~/Desktop/RIC.working.weatherprep.xlsx",sheet = "Sheet1",na="NA")

#Compute variables quickly from after loading df, 
for(i in 1:ncol(RICF)){
  assign(names(RICF)[i], RICF[[i]])
}

#Remove Unnecessary for Variables
RICfactors <- dplyr::select(RICF,ColdDaysExtreme,ColdDaysModerate,ColdDaysSevere,ColdWavesExtreme,ColdWavesModerate,ColdWavesSevere,HotDaysExtreme,HotDaysModerate,HotDaysSevere,HeatWavesExtreme,HeatWavesModerate,HeatWavesSevere,holidays,dow,WinterWeather)
RIC <- select(RICF,-Station,-Date,-Year,-Month,-Day,-`MaxT(C)`,-`MaxTDep(C)`,-`MinT(C)`,-`MinTDep(C)`,-`DTR(C)`,-`T(C)1am`,-`Td(C)1am`,-`Tw(C)1am`,-`AT(C)1am`,-`THI(C)1am`,-`Hx(C)1am`,-`WC(C)1am`,-`T(C)7am`,-`Td(C)7am`,-`Tw(C)7am`,-`AT(C)7am`,-`THI(C)7am`,-`Hx(C)7am`,-`WC(C)7am`,-`T(C)1pm`,-`Td(C)1pm`,-`Tw(C)1pm`,-`AT(C)1pm`,-`THI(C)1pm`,-`Hx(C)1pm`,-`WC(C)1pm`,-`T(C)7pm`,-`Td(C)7pm`,-`Tw(C)7pm`,-`AT(C)7pm`,-`THI(C)7pm`,-`Hx(C)7pm`,-`WC(C)7pm`,-ColdDaysExtreme,-ColdDaysModerate,-ColdDaysSevere,-ColdWavesExtreme,-ColdWavesModerate,-ColdWavesSevere,-HotDaysExtreme,-HotDaysModerate,-HotDaysSevere,-HeatWavesExtreme,-HeatWavesModerate,-HeatWavesSevere,-holidays,-dow,-WinterWeather,-Lag1,-Lag2,-Speedkts1am,-Speedkts1pm,-Speedkts7am,-Speedkts7pm,-WCF1am,-WCF1pm,-WCF7am,-WCF7pm,-ehPa1am,-ehPa1pm,-ehPa7am,-ehPa7pm,-esubshPa1am,-esubshPa1pm,-esubshPa7am,-esubshPa7pm,-SLPhPa1am,-SLPhPa1pm,-SLPhPa7am,-SLPhPa7pm) #,-AdjustedMort,-Ozone,-PM2.5)#,-OzoneREVISED,-PM2.5REVISED)
RICsplines <- dplyr::select(RICF,RH1am,RH7am,RH1pm,RH7pm,TdF1am,TdF7am,TdF1pm,TdF7pm,TwF1am,TwF7am,TwF1pm,TwF7pm,TF1am,TF7am,TF1pm,TF7pm,HxF1am,HxF7am,HxF1pm,HxF7pm,ATF1am,ATF7am,ATF1pm,ATF7pm,Speedkts1am,Speedkts1pm,Speedkts7am,Speedkts7pm,WCF1am,WCF1pm,WCF7am,WCF7pm,ehPa1am,ehPa1pm,ehPa7am,ehPa7pm,esubshPa1am,esubshPa1pm,esubshPa7am,esubshPa7pm)#,SLPhPa1am,SLPhPa1pm,SLPhPa7am,SLPhPa7pm,OzoneREVISED,PM2.5REVISED)

#Convert Data to numeric
RIC %>% mutate_if(is.character, as.numeric)
head(RIC)

#Customize your mortality

#Load in and prepare filter for dataset
BigMort_RACE_Full <- read_excel("~/Desktop/BigMort3.xlsx")
BigMortR <- BigMort_RACE_Full

BigMortR$Date = as.Date(paste(BigMortR$DOD_YR,BigMortR$DOD_MO,BigMortR$DOD_DY,sep="-"))
BigMortR$`Station ID` <- str_replace_all(BigMortR$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))
BigMortR$Count <- 1

#Single Station Filter command
RICmortdf <- BigMortR %>%
  filter(BigMortR$`Station ID` == "RIC") 

RICmortdf <- dplyr::select(RICmortdf,Count,Date,RACE,AGE_GROUPS)

RICmortdf$RACE <- str_replace_all(RICmortdf$RACE, c("Asian/PacificIslander"="Other",
                                                    "AmerInd/AlaskNat"="Other",
                                                    "Other"="Other"))

#Produce df for how many of each age group have died, look at station general mortalities
RICdfT <- RICmortdf %>%
  group_by(Date)%>%
  summarise(Total=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

RICmort <- RICdfT$Total
plot(RICmort, type='l', main="RIC Total", col='blue')
lines(rollmean(rollmean(RICmort,40),40),col='yellow')
abline(v=seq(0,5844,by = 365))

##Filter by Race, Make each df the proper length

RICdf1 <- RICmortdf %>%
  filter(RACE == 'White')%>%
  group_by(Date) %>%
  summarise(White=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))


RICdf2 <- RICmortdf %>%
  filter(RACE == 'Black') %>%
  group_by(Date) %>%
  summarise(Black=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

RICdf3 <- RICmortdf %>%
  filter(RACE == 'Other') %>%
  group_by(Date) %>%
  summarise(Other=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

RICdf4 <- RICmortdf %>%
  filter(AGE_GROUPS == '75+') %>%
  group_by(Date) %>%
  summarise(RICmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

RICdfMerged <- do.call("cbind", list(RICdfT,RICdf1,RICdf2,RICdf3,RICdf4))
RICdfwide <- RICdfMerged[!duplicated(as.list(RICdfMerged))]
RICdfwide[is.na(RICdfwide)] <- 0

#Set your mort (Total is column 2, White in 3, Black in 4, Others in 5, Elderly in 6)
mort <- RICdfwide[,4]

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
summary(modeltrend5)
gam.check(modeltrend5)
plot(modeltrend5)


#Run every column of dataset through the model
#Run every column of dataset through the model
klist <- data.frame()
for (i in (3:7)) {
  print(i)
  f <- function(col,RIC) {
    form = as.formula(paste0("mort~s(Trend,k=16*6)+s(",col,",k=i)"))
    data.frame(col = col, gcv = gam(form, data = RIC, family="quasipoisson")$gcv)
  }
  
  #Compile outputs of model table into list of GCV values
  t <- do.call(rbind, lapply(c("MaxTF","MinTF","MaxTDepF","MinTDepF","DTRF","TF1am","TwF1am","TdF1am","RH1am","ATF1am","THIF1am","HxF1am","TF7am","TwF7am","TdF7am","RH7am","ATF7am","THIF7am","HxF7am","TF1pm","TwF1pm","TdF1pm","RH1pm","ATF1pm","THIF1pm","HxF1pm","TF7pm","TwF7pm","TdF7pm","RH7pm","ATF7pm","THIF7pm","HxF7pm"), f, RIC=RIC))
  
  #Find minimum GCV and variable associated with it
  UseThisVariable <- t[which.min(t$gcv),]
  klist <- append(klist,t[which.min(t$gcv),]) #This is the variable created which lists index, gcv, UseThisVariable
  View(UseThisVariable)
  #Add in plot for UseThisVariable
  print(klist)
}
print(klist)
list <- as.matrix(klist, ncol=2)
df <- as.table(list,ncol=2)


WxParameter <- MinTF

**Using as.factors**
  # Add more variables to base model to select "best" model for DLNM
  #Adding in other as.factors to minimum variable found above
  
model0=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7),family="quasipoisson")
a=summary(model0)
#plot(model0)

model1=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(dow),family="quasipoisson")
A=summary(model1)
#plot(model1)

model2=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(holidays),family="quasipoisson")
B=summary(model2)
#plot(model2)

model3=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(dow)+as.factor(holidays),family="quasipoisson")
C=summary(model3)
#plot(model3)

model4=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(HeatWavesModerate),family="quasipoisson")
D=summary(model4)
#plot(model4)

model5=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(HeatWavesSevere),family="quasipoisson")
E=summary(model5)
#plot(model5)

model6=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(HeatWavesExtreme),family="quasipoisson")
F=summary(model6)
#plot(model6)

model7=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(ColdWavesModerate),family="quasipoisson")
G=summary(model7)
#plot(model7)

model8=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(ColdWavesSevere),family="quasipoisson")
H=summary(model8)
#plot(model8)

model9=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(ColdWavesExtreme),family="quasipoisson")
I=summary(model9)
#plot(model9)

model10=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(WinterWeather),family="quasipoisson")
J=summary(model10)
#plot(model10)

model11=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(HotDaysExtreme),family="quasipoisson")
K=summary(model11)
#plot(model11)

model12=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(dow)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")
L=summary(model12)
#plot(model12)

model13=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")
M=summary(model13)
#plot(model13)

model14=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
N=summary(model14)
#plot(model14)

model15=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(dow)+as.factor(HeatWavesModerate),family="quasipoisson")
O=summary(model15)
#plot(model15)

model16=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(dow)+as.factor(ColdWavesModerate),family="quasipoisson")
P=summary(model16)
#plot(model16)

model17=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(WinterWeather)+as.factor(ColdWavesModerate),family="quasipoisson")
Q=summary(model17)
#plot(model17)

model18=gam(mort~s(Trend,k=16*6)+s(WxParameter,k=7)+as.factor(holidays)+as.factor(ColdWavesModerate),family="quasipoisson")
R=summary(model18)
#plot(model18)

GAMbase <-matrix(c(a$sp.criterion,a$dev.expl,A$sp.criterion,A$dev.expl,B$sp.criterion,B$dev.expl,C$sp.criterion,C$dev.expl,D$sp.criterion,D$dev.expl,E$sp.criterion,E$dev.expl,F$sp.criterion,F$dev.expl,G$sp.criterion,G$dev.expl,H$sp.criterion,H$dev.expl,I$sp.criterion,I$dev.expl,J$sp.criterion,J$dev.expl,K$sp.criterion,K$dev.expl,L$sp.criterion,L$dev.expl,M$sp.criterion,M$dev.expl,N$sp.criterion,N$dev.expl,O$sp.criterion,O$dev.expl,P$sp.criterion,P$dev.expl,Q$sp.criterion,Q$dev.expl,R$sp.criterion,R$dev.expl),ncol=2,byrow=TRUE)
rownames(GAMbase)<-c('blank','dow','holidays','dow+holidays','HeatWavesModerate','HeatWavesSevere','HeatWavesExtreme','ColdWavesModerate','ColdWavesSevere','ColdWavesExtreme','WinterWeather','HotDaysExtreme','dow+holidays+WinterWeather','holidays+WinterWeather','dow+WinterWeather','dow+HeatWavesModerate','dow+ColdWavesModerate','WinterWeather+ColdWavesModerate','holidays+ColdWavesModerate')
colnames(GAMbase)<-c('GCV','Dev.Exp.')
GAMbase <- as.data.frame(GAMbase)

UseThisFactor <- rownames(GAMbase[which.min(GAMbase$GCV),])
View(UseThisFactor)

#Find your best pair for your WxParameter
RICsplinesRefined <- RICsplines
RICsplinesRefined <- RICsplines[,!grepl("TF|ATF", colnames(RICsplines))]
RICsplinesRefined <- RICsplines[,!grepl("RH|Td|Tw|Hx|ehPa|esub", colnames(RICsplines))]

Wxparameter <- MinTF

#Spline test
klist <- list()
f <- function(col,RICsplinesRefined) {
  form = as.formula(paste0("mort~s(Trend,k=16*6)+s(MinTF,k=7)+as.factor(HeatWavesSevere)+s(",col,",k=7)"))
  data.frame(col = col, gcv = gam(form, data = RIC, family="quasipoisson")$gcv)
}

t <- do.call(rbind, lapply(c(colnames(RICsplinesRefined)), f, RICsplinesRefined=RICsplinesRefined))

#Find minimum GCV and variable associated with it
UseThisSpline <- t[which.min(t$gcv),]
View(UseThisSpline)

klist <- append(klist,t[which.min(t$gcv),]) #This is the variable created which lists index, gcv, UseThisVariable
print(klist)

#Remember to upload your best GAM information to the google sheets :)
