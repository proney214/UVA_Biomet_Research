

#Load in libraries

library(splines)
library(dlnm)
library(lubridate)
library(readxl)
library(mgcv)
library(ggplot2)
library(dplyr)
library(AER)
library(qcc)

#Load in Data

VJI <- read_excel("~/Desktop/VJI.Final.Weather.xlsx",na="NA")

#Convert Data to numeric

VJI %>% mutate_if(is.character, as.numeric)
#head(VJI)

VJI$MaxTC <- VJI$`MaxT(C)`
VJI$MinTC <- VJI$`MinT(C)`
VJI$MaxTDepC <- VJI$`MaxTDep(C)`
VJI$MinTDepC <- VJI$`MinTDep(C)`
VJI$ATC1am <- VJI$`AT(C)1am`
VJI$ATC7am <- VJI$`AT(C)7am`
VJI$ATC1pm <- VJI$`AT(C)1pm`
VJI$ATC7pm <- VJI$`AT(C)7pm`
VJI$DTRC <- VJI$`DTR(C)`
VJI$HxC1am <- VJI$`Hx(C)1am`
VJI$HxC7am <- VJI$`Hx(C)7am`
VJI$HxC1pm <- VJI$`Hx(C)1pm`
VJI$HxC7pm <- VJI$`Hx(C)7pm`
VJI$TC1am <- VJI$`T(C)1am`
VJI$TC7am <- VJI$`T(C)7am`
VJI$TC1pm <- VJI$`T(C)1pm`
VJI$TC7pm <- VJI$`T(C)7pm`
VJI$TdC1am <- VJI$`Td(C)1am`
VJI$TdC7am <- VJI$`Td(C)7am`
VJI$TdC1pm <- VJI$`Td(C)1pm`
VJI$TdC7pm <- VJI$`Td(C)7pm`
VJI$THIC1am <- VJI$`THI(C)1am`
VJI$THIC7am <- VJI$`THI(C)7am`
VJI$THIC1pm <- VJI$`THI(C)1pm`
VJI$THIC7pm <- VJI$`THI(C)7pm`
VJI$TwC1am <- VJI$`Tw(C)1am`
VJI$TwC7am <- VJI$`Tw(C)7am`
VJI$TwC1pm <- VJI$`Tw(C)1pm`
VJI$TwC7pm <- VJI$`Tw(C)7pm`
VJI$WCC1am <- VJI$`WC(C)1am`
VJI$WCC7am <- VJI$`WC(C)7am`
VJI$WCC1pm <- VJI$`WC(C)1pm`
VJI$WCC7pm <- VJI$`WC(C)7pm`

for(i in 1:ncol(VJI)){
  assign(names(VJI)[i], VJI[[i]])
}

plot(VJI$Black,type='l')
plot(VJI$White,type='l')

#Create Trend Term

Trend = seq(1,5844,1)
VJI$AccountChange2015 <- ifelse(VJI$Year >= 2015, 1, 0)

#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing VJI$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

#SPECIFY WHAT'S YOUR MORT

Mort <- VJI$Black

#Test number of splines for Trend

modeltrend1=gam(Mort~s(Trend,k=16*2),family="quasipoisson")
summary(modeltrend1)
# gam.check(modeltrend1)
plot(modeltrend1)
# plot.gam(modeltrend1)

modeltrend2=gam(Mort~s(Trend,k=16*3),family="quasipoisson")
summary(modeltrend2)
# gam.check(modeltrend2)
plot(modeltrend2)
 
 
modeltrend2_factor=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015),family="quasipoisson")
summary(modeltrend2_factor)
# gam.check(modeltrend2)
plot(modeltrend2_factor) 

modeltrend3=gam(Mort~s(Trend,k=16*4)+as.factor(VJI$AccountChange2015),family="quasipoisson")
summary(modeltrend3)
#gam.check(modeltrend3)
plot(modeltrend3)

modeltrend4=gam(Mort~s(Trend,k=16*5),family="quasipoisson")
summary(modeltrend4)
gam.check(modeltrend4)
plot(modeltrend4)

modeltrend5=gam(Mort~s(Trend,k=16*7),family="quasipoisson")
summary(modeltrend5)
gam.check(modeltrend5)
plot(modeltrend5)


#Adding in other as.factors to minimum variable found above

model0a=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3),family="quasipoisson")
summary(model0a)
#plot(model0a)
#MaxTC is significant
#GCV = 1.0674
#Dev. Expl = 9.37%

model0b=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MinTC,k=3),family="quasipoisson")
summary(model0b)
#plot(model0b)
#MinTC is significant (k=3)
#GCV = 1.0659
#Dev. Expl = 9.52%

model0c=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(TC7pm,k=3)+as.factor(ColdWavesModerate),family="quasipoisson")
summary(model0c)
#TF7pm is significant (k=3)
#GCV = 1.0666
#Dev. Expl = 9.36%

model0d=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(DTRC,k=3),family="quasipoisson")
summary(model0d)

model0e=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(ATC1pm,k=3),family="quasipoisson")
summary(model0e)

summary(model0a)
summary(model0b)
summary(model0c)
summary(model0d)
summary(model0e)


#Test Out Factors
model1=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(dow),family="quasipoisson")
summary(model1)
#plot(model1)
#dow is not significant

model2=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(holidays),family="quasipoisson")
summary(model2)
#plot(model2)
#Holidays are not significant

model3=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(dow)+as.factor(holidays),family="quasipoisson")
summary(model3)
#plot(model3)

model4=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(HeatWavesModerate),family="quasipoisson")
summary(model4)
#plot(model4)
#HeatWavesModerate are not significant

model5=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(HeatWavesSevere),family="quasipoisson")
summary(model5)
#plot(model5)
#HeatWavesSevere are not significant

model6=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(HeatWavesExtreme),family="quasipoisson")
summary(model6)
#plot(model6)
#HeatWavesExtreme is not significant

model7=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(ColdWavesModerate),family="quasipoisson")
summary(model7)
#plot(model7)
#ColdWavesModerate is significant

model8=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(ColdWavesSevere),family="quasipoisson")
summary(model8)
#plot(model8)
#ColdWavesSevere is not significant

model9=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(ColdWavesExtreme),family="quasipoisson")
summary(model9)
#plot(model9)
#ColdWavesExtreme is not significant

model10=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(WinterWeather),family="quasipoisson")
summary(model10)
#plot(model10)
#WinterWeather is not significant

model11=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(HotDaysExtreme),family="quasipoisson")
summary(model11)
#plot(model11)
#HotDaysExtreme is not significant

model12=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(dow)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")
summary(model12)
#plot(model12)

model13=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")
summary(model13)
#plot(model13)

model14=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
summary(model14)
#plot(model14)

model15=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(dow)+as.factor(HeatWavesModerate),family="quasipoisson")
summary(model15)
#plot(model15)

model16=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(dow)+as.factor(ColdWavesModerate),family="quasipoisson")
summary(model16)
#plot(model16)

model17=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(WinterWeather)+as.factor(ColdWavesModerate),family="quasipoisson")
summary(model17)
#plot(model17)

model18=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+as.factor(ColdWavesModerate)+as.factor(holidays)+as.factor(ColdWavesModerate),family="quasipoisson")
summary(model18)
#plot(model18)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
summary(model7)
summary(model8)
summary(model9)
summary(model10)
summary(model11)
summary(model12)
summary(model13)
summary(model14)
summary(model15)
summary(model16)
summary(model17)
summary(model18)

 #Add other Wx indicators to best model from previous step to get the overall best model for the dnlm stage

model1a=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3),family="quasipoisson")
summary(model1a)
#plot(model1a)
#GCV=1.0655
#dev.expl= 9.58%

model1b=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+s(ATC1pm,k=3),family="quasipoisson")
summary(model1b)
#plot(model1b)
#DTR is not significant

model1c=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+s(ATC1pm,k=7),family="quasipoisson")
summary(model1c)
#plot(model1c)
#Increasing dof for DTR doesn't make a difference

model1d=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+s(RH1pm,k=3),family="quasipoisson")
summary(model1d)
#plot(model1d)
#RH1pm is significant
#GCV=1.0647
#dev.expl= 9.62%

model1e=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+s(THIF7pm,k=3),family="quasipoisson")
summary(model1e)
#plot(model1e)
#THIF1pm is not significant

model1f=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+s(HxF1pm,k=3),family="quasipoisson")
summary(model1f)
#plot(model1f)
#HxF1pm is not significant

model1g=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+s(TdC1pm,k=3),family="quasipoisson")
summary(model1g)
#plot(model1g)
#TdC1pm is not significant (and all other times)

model1h=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+s(TC7pm,k=3),family="quasipoisson")
summary(model1h)
#plot(model1h)
#MaxTC is not significant (and all other times)

model1i=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3)+s(PM2.5REVISED,k=3),family="quasipoisson")
summary(model1i)
#AIC(model1i)
#plot(model1i)
#PM2.5 is not significant

summary(model1a)
summary(model1b)
summary(model1c)
summary(model1d)
summary(model1e)
summary(model1f)
summary(model1g)
summary(model1h)
summary(model1i)

