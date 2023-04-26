### PHF Modeling for custom mortality subsets

#Load in Data
library(splines)
library(dlnm)
library(lubridate)
library(readxl)
library(mgcv)
library(ggplot2)
library(dplyr)

PHFF <- read_excel("~/Desktop/PHF.Working.Complete.xlsx",sheet = "Sheet1",na="NA")

#Compute variables quickly from after loading df, 
for(i in 1:ncol(PHFF)){
  assign(names(PHFF)[i], PHFF[[i]])
}

#Remove Unnecessary for Variables
PHFfactors <- dplyr::select(PHFF,ColdDaysExtreme,ColdDaysModerate,ColdDaysSevere,ColdWavesExtreme,ColdWavesModerate,ColdWavesSevere,HotDaysExtreme,HotDaysModerate,HotDaysSevere,HeatWavesExtreme,HeatWavesModerate,HeatWavesSevere,holidays,dow,WinterWeather)
PHF <- dplyr::select(PHFF,-Station,-Date,-Year,-Month,-Day,-`MaxT(C)`,-`MaxTDep(C)`,-`MinT(C)`,-`MinTDep(C)`,-`DTR(C)`,-`T(C)1am`,-`Td(C)1am`,-`Tw(C)1am`,-`AT(C)1am`,-`THI(C)1am`,-`Hx(C)1am`,-`WC(C)1am`,-`T(C)7am`,-`Td(C)7am`,-`Tw(C)7am`,-`AT(C)7am`,-`THI(C)7am`,-`Hx(C)7am`,-`WC(C)7am`,-`T(C)1pm`,-`Td(C)1pm`,-`Tw(C)1pm`,-`AT(C)1pm`,-`THI(C)1pm`,-`Hx(C)1pm`,-`WC(C)1pm`,-`T(C)7pm`,-`Td(C)7pm`,-`Tw(C)7pm`,-`AT(C)7pm`,-`THI(C)7pm`,-`Hx(C)7pm`,-`WC(C)7pm`,-ColdDaysExtreme,-ColdDaysModerate,-ColdDaysSevere,-ColdWavesExtreme,-ColdWavesModerate,-ColdWavesSevere,-HotDaysExtreme,-HotDaysModerate,-HotDaysSevere,-HeatWavesExtreme,-HeatWavesModerate,-HeatWavesSevere,-holidays,-dow,-WinterWeather,-AdjustedMort,-Lag1,-Lag2,-Speedkts1am,-Speedkts1pm,-Speedkts7am,-Speedkts7pm,-WCF1am,-WCF1pm,-WCF7am,-WCF7pm,-ehPa1am,-ehPa1pm,-ehPa7am,-ehPa7pm,-esubshPa1am,-esubshPa1pm,-esubshPa7am,-esubshPa7pm,-SLPhPa1am,-SLPhPa1pm,-SLPhPa7am,-SLPhPa7pm,-Ozone,-PM2.5,-OzoneREVISED,-PM2.5REVISED)
PHFsplines <- dplyr::select(PHFF,RH1am,RH7am,RH1pm,RH7pm,TdF1am,TdF7am,TdF1pm,TdF7pm,TwF1am,TwF7am,TwF1pm,TwF7pm,TF1am,TF7am,TF1pm,TF7pm,HxF1am,HxF7am,HxF1pm,HxF7pm,ATF1am,ATF7am,ATF1pm,ATF7pm,Speedkts1am,Speedkts1pm,Speedkts7am,Speedkts7pm,WCF1am,WCF1pm,WCF7am,WCF7pm,ehPa1am,ehPa1pm,ehPa7am,ehPa7pm,esubshPa1am,esubshPa1pm,esubshPa7am,esubshPa7pm,SLPhPa1am,SLPhPa1pm,SLPhPa7am,SLPhPa7pm,OzoneREVISED,PM2.5REVISED)

#Convert Data to numeric
PHF %>% mutate_if(is.character, as.numeric)
head(PHF)

#Customize your mortality

#Load in and prepare filter for dataset
BigMort_RACE_Full <- read_excel("~/Desktop/BigMort3.xlsx")
BigMortR <- BigMort_RACE_Full

BigMortR$Date = as.Date(paste(BigMortR$DOD_YR,BigMortR$DOD_MO,BigMortR$DOD_DY,sep="-"))
BigMortR$`Station ID` <- str_replace_all(BigMortR$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))
BigMortR$Count <- 1

#Single Station Filter command
PHFmortdf <- BigMortR %>%
  filter(BigMortR$`Station ID` == "PHF") 

PHFmortdf <- dplyr::select(PHFmortdf,Count,Date,RACE,AGE_GROUPS)

PHFmortdf$RACE <- str_replace_all(PHFmortdf$RACE, c("Asian/PacificIslander"="Other",
                                                    "AmerInd/AlaskNat"="Other",
                                                    "Other"="Other"))

#Produce df for how many of each age group have died, look at station general mortalities
PHFdfT <- PHFmortdf %>%
  group_by(Date)%>%
  summarise(Total=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

PHFmort <- PHFdfT$Total
plot(PHFmort, type='l', main="PHF Total", col='blue')
lines(rollmean(rollmean(PHFmort,40),40),col='yellow')
abline(v=seq(0,5844,by = 365))

##Filter by Race, Make each df the proper length

PHFdf1 <- PHFmortdf %>%
  filter(RACE == 'White')%>%
  group_by(Date) %>%
  summarise(White=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))


PHFdf2 <- PHFmortdf %>%
  filter(RACE == 'Black') %>%
  group_by(Date) %>%
  summarise(Black=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

PHFdf3 <- PHFmortdf %>%
  filter(RACE == 'Other') %>%
  group_by(Date) %>%
  summarise(Other=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

PHFdf4 <- PHFmortdf %>%
  filter(AGE_GROUPS == '75+') %>%
  group_by(Date) %>%
  summarise(PHFmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

PHFdfMerged <- do.call("cbind", list(PHFdfT,PHFdf1,PHFdf2,PHFdf3,PHFdf4))
PHFdfwide <- PHFdfMerged[!duplicated(as.list(PHFdfMerged))]
PHFdfwide[is.na(PHFdfwide)] <- 0

#Set your mort (Total is column 2, White in 3, Black in 4, Others in 5, Elderly in 6)
mort <- PHFdfwide[,4]

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
  f <- function(col,PHF) {
    form = as.formula(paste0("mort~s(Trend,k=16*4)+s(",col,",k=i)"))
    data.frame(col = col, gcv = gam(form, data = PHF, family="quasipoisson")$gcv)
  }
  
  #Compile outputs of model table into list of GCV values
  t <- do.call(rbind, lapply(c("MaxTF","MinTF","MaxTDepF","MinTDepF","DTRF","TF1am","TwF1am","TdF1am","RH1am","ATF1am","THIF1am","HxF1am","TF7am","TwF7am","TdF7am","RH7am","ATF7am","THIF7am","HxF7am","TF1pm","TwF1pm","TdF1pm","RH1pm","ATF1pm","THIF1pm","HxF1pm","TF7pm","TwF7pm","TdF7pm","RH7pm","ATF7pm","THIF7pm","HxF7pm"), f, PHF=PHF))
  
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

#Add in plot for UseThisVariable

**Using as.factors**
  # Add more variables to base model to select "best" model for DLNM
  #Adding in other as.factors to minimum variable found above
  
model0=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3),family="quasipoisson")
a=summary(model0)
#plot(model0)

model1=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(dow),family="quasipoisson")
A=summary(model1)
#plot(model1)

model2=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(holidays),family="quasipoisson")
B=summary(model2)
#plot(model2)

model3=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(dow)+as.factor(holidays),family="quasipoisson")
C=summary(model3)
#plot(model3)

model4=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(HeatWavesModerate),family="quasipoisson")
D=summary(model4)
#plot(model4)

model5=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(HeatWavesSevere),family="quasipoisson")
E=summary(model5)
#plot(model5)

model6=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(HeatWavesExtreme),family="quasipoisson")
F=summary(model6)
#plot(model6)

model7=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(ColdWavesModerate),family="quasipoisson")
G=summary(model7)
#plot(model7)

model8=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(ColdWavesSevere),family="quasipoisson")
H=summary(model8)
#plot(model8)

model9=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(ColdWavesExtreme),family="quasipoisson")
I=summary(model9)
#plot(model9)

model10=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(WinterWeather),family="quasipoisson")
J=summary(model10)
#plot(model10)

model11=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(HotDaysExtreme),family="quasipoisson")
K=summary(model11)
#plot(model11)

model12=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(dow)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")
L=summary(model12)
#plot(model12)

model13=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")
M=summary(model13)
#plot(model13)

model14=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
N=summary(model14)
#plot(model14)

model15=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(dow)+as.factor(HeatWavesModerate),family="quasipoisson")
O=summary(model15)
#plot(model15)

model16=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(dow)+as.factor(ColdWavesModerate),family="quasipoisson")
P=summary(model16)
#plot(model16)

model17=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(WinterWeather)+as.factor(ColdWavesModerate),family="quasipoisson")
Q=summary(model17)
#plot(model17)

model18=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+as.factor(holidays)+as.factor(ColdWavesModerate),family="quasipoisson")
R=summary(model18)
#plot(model18)

GAMbase <-matrix(c(a$sp.criterion,a$dev.expl,A$sp.criterion,A$dev.expl,B$sp.criterion,B$dev.expl,C$sp.criterion,C$dev.expl,D$sp.criterion,D$dev.expl,E$sp.criterion,E$dev.expl,F$sp.criterion,F$dev.expl,G$sp.criterion,G$dev.expl,H$sp.criterion,H$dev.expl,I$sp.criterion,I$dev.expl,J$sp.criterion,J$dev.expl,K$sp.criterion,K$dev.expl,L$sp.criterion,L$dev.expl,M$sp.criterion,M$dev.expl,N$sp.criterion,N$dev.expl,O$sp.criterion,O$dev.expl,P$sp.criterion,P$dev.expl,Q$sp.criterion,Q$dev.expl,R$sp.criterion,R$dev.expl),ncol=2,byrow=TRUE)
rownames(GAMbase)<-c('blank','dow','holidays','dow+holidays','HeatWavesModerate','HeatWavesSevere','HeatWavesExtreme','ColdWavesModerate','ColdWavesSevere','ColdWavesExtreme','WinterWeather','HotDaysExtreme','dow+holidays+WinterWeather','holidays+WinterWeather','dow+WinterWeather','dow+HeatWavesModerate','dow+ColdWavesModerate','WinterWeather+ColdWavesModerate','holidays+ColdWavesModerate')
colnames(GAMbase)<-c('GCV','Dev.Exp.')
GAMbase <- as.data.frame(GAMbase)

UseThisFactor <- GAMbase[which.min(GAMbase$GCV),]
View(UseThisFactor)

#Adding in other variables outside of the best minimum gcv found above (with as.factors IF they improved the model)



modelnewvar1=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(RH1am,k=4),family="quasipoisson")
S=summary(modelnewvar1)
#plot(modelnewvar1)

modelnewvar2=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(RH7am,k=4),family="quasipoisson")
T=summary(modelnewvar2)
#plot(modelnewvar2)

modelnewvar3=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(RH1pm,k=4),family="quasipoisson")
Uu=summary(modelnewvar3)
#plot(modelnewvar3)

modelnewvar4=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(RH7pm,k=4),family="quasipoisson")
U=summary(modelnewvar4)
#plot(modelnewvar4)

modelnewvar5=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(TdF1pm,k=4),family="quasipoisson")
V=summary(modelnewvar5)
#plot(modelnewvar5)

modelnewvar6=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(TdF1am,k=4),family="quasipoisson")
W=summary(modelnewvar6)
#plot(modelnewvar6)

modelnewvar7=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(TdF7am,k=4),family="quasipoisson")
X=summary(modelnewvar7)
#plot(modelnewvar7)

modelnewvar8=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(TdF1pm,k=4),family="quasipoisson")
Y=summary(modelnewvar8)
#plot(modelnewvar8)

modelnewvar9=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(TdF7pm,k=4),family="quasipoisson")
Z=summary(modelnewvar9)
#plot(modelnewvar9)

modelnewvar10=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(TwF1am,k=4),family="quasipoisson")
b=summary(modelnewvar10)
#plot(modelnewvar10)

modelnewvar11=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(TwF7am,k=4),family="quasipoisson")
c=summary(modelnewvar11)
#plot(modelnewvar11)

modelnewvar12=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(TwF1pm,k=4),family="quasipoisson")
d=summary(modelnewvar12)
#plot(modelnewvar12)

modelnewvar13=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(TwF7pm,k=4),family="quasipoisson")
E=summary(modelnewvar13)
#plot(modelnewvar13)

modelnewvar130=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(TF1am,k=4),family="quasipoisson")
e=summary(modelnewvar130)
#plot(modelnewvar130)

modelnewvar14=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(TF7am,k=4),family="quasipoisson")
f=summary(modelnewvar14)
#plot(modelnewvar14)

modelnewvar15=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(TwF7pm,k=3),family="quasipoisson")
g=summary(modelnewvar15)
#plot(modelnewvar15)

modelnewvar16=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(TF7pm,k=4),family="quasipoisson")
h=summary(modelnewvar16)
#plot(modelnewvar16)

modelnewvar17=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(ATF1am,k=4),family="quasipoisson")
i=summary(modelnewvar17)
#plot(modelnewvar17)

modelnewvar18=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(ATF7am,k=4),family="quasipoisson")
j=summary(modelnewvar18)
#plot(modelnewvar18)

modelnewvar19=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(HxF1am,k=4),family="quasipoisson")
k=summary(modelnewvar19)
#plot(modelnewvar19)

modelnewvar20=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(HxF7am,k=4),family="quasipoisson")
l=summary(modelnewvar20)
#plot(modelnewvar20)

modelnewvar21=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(HxF1pm,k=4),family="quasipoisson")
m=summary(modelnewvar21)
#plot(modelnewvar21)

modelnewvar22=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(HxF7pm,k=4),family="quasipoisson")
n=summary(modelnewvar22)
#plot(modelnewvar22)

modelnewvar23=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(ATwF7pm,k=3),family="quasipoisson")
o=summary(modelnewvar23)
#plot(modelnewvar23)

modelnewvar24=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(ATF7pm,k=4),family="quasipoisson")
p=summary(modelnewvar24)
#plot(modelnewvar24)

GAMbase <-matrix(c(H$sp.criterion,H$dev.expl,S$sp.criterion,S$dev.expl,T$sp.criterion,T$dev.expl,Uu$sp.criterion,Uu$dev.expl,U$sp.criterion,U$dev.expl,V$sp.criterion,V$dev.expl,W$sp.criterion,W$dev.expl,X$sp.criterion,X$dev.expl,Y$sp.criterion,Y$dev.expl,Z$sp.criterion,Z$dev.expl,b$sp.criterion,b$dev.expl,c$sp.criterion,c$dev.expl,d$sp.criterion,d$dev.expl,E$sp.criterion,E$dev.expl,e$sp.criterion,e$dev.expl,f$sp.criterion,f$dev.expl,g$sp.criterion,g$dev.expl,h$sp.criterion,h$dev.expl,k$sp.criterion,k$dev.expl,l$sp.criterion,l$dev.expl,m$sp.criterion,m$dev.expl,n$sp.criterion,n$dev.expl,i$sp.criterion,i$dev.expl,j$sp.criterion,j$dev.expl,o$sp.criterion,o$dev.expl,p$sp.criterion,p$dev.expl),ncol=2,byrow=TRUE)
rownames(GAMbase)<-c('blank','RH1am','RH7am','RH1pm','RH7pm','TdF1am','TdF7am','TdF1pm','TdF7pm','TwF1am','TwF7am','TwF1pm','TwF7pm','TF1am','TF7am','TF1pm','TF7pm','Hx1am','Hx7am','Hx1pm','Hx7pm','ATF1am','ATF7am','ATF1pm','ATF7pm')
colnames(GAMbase)<-c('GCV','Dev.Exp.')
GAMbase <- as.data.frame(GAMbase)
#head(GAMbase)

RH1am,RH7am,RH1pm,RH7pm,TdF1am,TdF7am,TdF1pm,TdF7pm,TwF1am,TwF7am,TwF1pm,TwF7pm,TF1am,TF7am,TF1pm,TF7pm,Hx1am,Hx7am,Hx1pm,Hx7pm,ATF1am,ATF7am,ATF1pm,ATF7pm


test <- GAMbase[!grepl("TF|ATF", rownames(GAMbase)), ]
test <- GAMbase[!grepl("RH|Td|Tw|Hx", rownames(GAMbase)), ]

UseThisModel <- GAMbase[which.min(GAMbase$GCV),]
View(UseThisModel)

#Model97 creates best graph for 75+: 
#model97=gam(mort~s(Trend,k=16*3)+s(TwF7pm,k=3)+s(OzoneREVISED,k=3)+as.factor(holidays)+as.factor(ColdWavesExtreme),family="quasipoisson")
plot(modelnewvar24)
plot(mort,type = 'l',col='blue')
lines(rollmean(rollmean(mort,40),40),col='red')
abline(v=seq(0,5844,by = 365))

# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(mort=mort, Trend=Trend, TF1pm=TF1pm,RH1am=RH1am, ColdWavesExtreme=ColdWavesExtreme, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.TF1pm=crossbasis(lagframe$TF1pm,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)
basis.RH1am=crossbasis(lagframe$RH1am,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)
#basis.PM2.5=crossbasis(lagframe$PM2.5,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

# Define the position of knots for spline functions
varknotsTF1pm=equalknots(lagframe$TF1pm,fun="bs",df=4,degree=2)
varknotsTF1pm
lagknotsTF1pm=logknots(21,2)
lagknotsTF1pm
varknotsRH1am=equalknots(lagframe$RH1am,fun="bs",df=4,degree=2)
varknotsRH1am
lagknotsRH1am=logknots(21,2)
lagknotsRH1am
#varknotsPM2.5=equalknots(lagframe$PM2.5,fun="bs",df=4,degree=2)
#varknotsPM2.5
#lagknotsPM2.5=logknots(21,2)
#lagknotsPM2.5

# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value
cb1.TF1pm=crossbasis(lagframe$TF1pm,lag=21,argvar=list(fun="bs",knots=varknotsTF1pm),arglag=list(knots=lagknotsTF1pm))
summary(cb1.TF1pm)
cb1.RH1am=crossbasis(lagframe$RH1am,lag=21,argvar=list(fun="bs",knots=varknotsRH1am),arglag=list(knots=lagknotsRH1am))
summary(cb1.RH1am)
#cb1.PM2.5=crossbasis(lagframe$PM2.5,lag=21,argvar=list(fun="bs",knots=varknotsPM2.5),arglag=list(knots=lagknotsPM2.5))
#summary(cb1.PM2.5)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
modelA1=glm(mort~cb1.TF1pm+cb1.RH1am+ns(Trend, 3*16)+as.factor(ColdWavesExtreme),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)
#Removed PM2.5

modelA2=glm(mort~cb1.TF1pm+ns(RH1am, 3*16)+ns(Trend, 3*16)+as.factor(ColdWavesExtreme),family=quasipoisson(),lagframe)
summary(modelA2)

# Generate predictions from DLNM and plot results
pred1.TF1pm=crosspred(cb1.TF1pm,modelA1,by=1)
pred2.TF1pm=crosspred(cb1.TF1pm,modelA2,by=1)
#pred1.RH1am=crosspred(cb1.RH1am,modelA1,by=1)
#pred1.PM2.5=crosspred(cb1.PM2.5,modelA1,by=1)

# 3-D plot
plot(pred1.TF1pm,xlab="TF1pm",zlab="RR",theta=200,phi=40,lphi=30,main="RIC Mort")
plot(pred2.TF1pm,xlab="TF1pm",zlab="RR",theta=200,phi=40,lphi=30,main="RIC Mort Spline")
#plot(pred1.RH1am,xlab="RH1am",zlab="RR",theta=200,phi=40,lphi=30,main="RIC Mort")
#plot(pred1.PM2.5,xlab="PM2.5",zlab="RR",theta=200,phi=40,lphi=30,main="RIC Mort")

# Plot of lag effects at selected values ("var=??")
plot(pred1.TF1pm,ptype = "slices",var=c(5),col='black',xlab="lag (days)",ylab="Relative Risk",main="Lag RR at 5 C")
plot(pred1.TF1pm,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")
plot(pred1.TF1pm,ptype="slices",var=c(20),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 20 C")
plot(pred1.RH1am,ptype="slices",var=c(0.0),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 0")
plot(pred1.RH1am,ptype="slices",var=c(0.5),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 0.5")

# Plot of "basis" variables
cbind(pred1.TF1pm$allRRfit,pred1.TF1pm$allRRlow,pred1.TF1pm$allRRhigh)
cbind(pred2.TF1pm$allRRfit,pred2.TF1pm$allRRlow,pred2.TF1pm$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
plot(pred1.TF1pm,"contour",xlab="TF1pm",key.title=title("RR"),plot.title=title("mort Admissions",xlab="TF1pm",ylab="Lag (days)"))
plot(pred2.TF1pm,"contour",xlab="TF1pm",key.title=title("RR"),plot.title=title("mort Admissions Spline",xlab="TF1pm",ylab="Lag (days)"))


#plot(pred1.RH1am,"contour",xlab="RH1am",key.title=title("RR"),plot.title=title("mort Admissions",xlab="RH1am",ylab="Lag (days)"))
#plot(pred1.PM2.5,"contour",xlab="PM2.5",key.title=title("RR"),plot.title=title("mort Admissions",xlab="PM2.5",ylab="Lag (days)"))


#overall plot
plot(pred1.TF1pm,"overall",xlab="TF1pm",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Overall cumulative association")
plot(pred2.TF1pm,"overall",xlab="TF1pm",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Overall cumulative association Spline")




