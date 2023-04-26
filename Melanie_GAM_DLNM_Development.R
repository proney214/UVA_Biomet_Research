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

VA <- read_excel("~/Desktop/VA.Final.Weather.xlsx",na="NA")
VA$MaxTC <- VA$`MaxT(C)`
VA$MinTC <- VA$`MinT(C)`
VA$MaxTDepC <- VA$`MaxTDep(C)`
VA$MinTDepC <- VA$`MinTDep(C)`
VA$ATC1am <- VA$`AT(C)1am`
VA$ATC7am <- VA$`AT(C)7am`
VA$ATC1pm <- VA$`AT(C)1pm`
VA$ATC7pm <- VA$`AT(C)7pm`
VA$DTRC <- VA$`DTR(C)`
VA$HxC1am <- VA$`Hx(C)1am`
VA$HxC7am <- VA$`Hx(C)7am`
VA$HxC1pm <- VA$`Hx(C)1pm`
VA$HxC7pm <- VA$`Hx(C)7pm`
VA$TC1am <- VA$`T(C)1am`
VA$TC7am <- VA$`T(C)7am`
VA$TC1pm <- VA$`T(C)1pm`
VA$TC7pm <- VA$`T(C)7pm`
VA$TdC1am <- VA$`Td(C)1am`
VA$TdC7am <- VA$`Td(C)7am`
VA$TdC1pm <- VA$`Td(C)1pm`
VA$TdC7pm <- VA$`Td(C)7pm`
VA$THIC1am <- VA$`THI(C)1am`
VA$THIC7am <- VA$`THI(C)7am`
VA$THIC1pm <- VA$`THI(C)1pm`
VA$THIC7pm <- VA$`THI(C)7pm`
VA$TwC1am <- VA$`Tw(C)1am`
VA$TwC7am <- VA$`Tw(C)7am`
VA$TwC1pm <- VA$`Tw(C)1pm`
VA$TwC7pm <- VA$`Tw(C)7pm`
VA$WCC1am <- VA$`WC(C)1am`
VA$WCC7am <- VA$`WC(C)7am`
VA$WCC1pm <- VA$`WC(C)1pm`
VA$WCC7pm <- VA$`WC(C)7pm`

#Convert Data to numeric

VA %>% mutate_if(is.character, as.numeric)
head(VA)

for(i in 1:ncol(VA)){
  
  assign(names(VA)[i], VA[[i]])
  
}


#Create Trend Term

Trend = seq(1,5844,1)

#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing VA$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

#SPECIFY WHAT'S YOUR MORT

Mort <- VA$White

#Test number of splines for Trend

modeltrend1=gam(Mort~s(Trend,k=16*2),family="quasipoisson")
summary(modeltrend1)
gam.check(modeltrend1)
plot(modeltrend1)
plot.gam(modeltrend1)

modeltrend2=gam(Mort~s(Trend,k=16*3),family="quasipoisson")
summary(modeltrend2)
gam.check(modeltrend2)
plot(modeltrend2)

modeltrend3=gam(Mort~s(Trend,k=16*4),family="quasipoisson")
summary(modeltrend3)
gam.check(modeltrend3)
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

model0a=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3),family="quasipoisson")
summary(model0a)
#plot(model0a)
#MaxTC is significant
#GCV = 1.0674
#Dev. Expl = 9.37%

model0b=gam(Mort~s(Trend,k=16*3)+s(MinTC,k=3),family="quasipoisson")
summary(model0b)
#plot(model0b)
#MinTC is significant (k=3)
#GCV = 1.0659
#Dev. Expl = 9.52%

model0c=gam(Mort~s(Trend,k=16*3)+s(TC7pm,k=3),family="quasipoisson")
summary(model0c)
#TF7pm is significant (k=3)
#GCV = 1.0666
#Dev. Expl = 9.36%

model0d=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3),family="quasipoisson")
summary(model0d)

model0e=gam(Mort~s(Trend,k=16*3)+s(ATC1pm,k=3),family="quasipoisson")
summary(model0e)



#Test Out Factors
model1=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(dow),family="quasipoisson")
summary(model1)
#plot(model1)
#dow is not significant

model2=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(holidays),family="quasipoisson")
summary(model2)
#plot(model2)
#Holidays are not significant

model3=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(dow)+as.factor(holidays),family="quasipoisson")
summary(model3)
#plot(model3)

model4=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(HeatWavesModerate),family="quasipoisson")
summary(model4)
#plot(model4)
#HeatWavesModerate are not significant

model5=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(HeatWavesSevere),family="quasipoisson")
summary(model5)
#plot(model5)
#HeatWavesSevere are not significant

model6=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(HeatWavesExtreme),family="quasipoisson")
summary(model6)
#plot(model6)
#HeatWavesExtreme is not significant

model7=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(ColdWavesModerate),family="quasipoisson")
summary(model7)
#plot(model7)
#ColdWavesModerate is significant

model8=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(ColdWavesSevere),family="quasipoisson")
summary(model8)
#plot(model8)
#ColdWavesSevere is not significant

model9=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(ColdWavesExtreme),family="quasipoisson")
summary(model9)
#plot(model9)
#ColdWavesExtreme is not significant

model10=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(WinterWeather),family="quasipoisson")
summary(model10)
#plot(model10)
#WinterWeather is not significant

model11=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(HotDaysExtreme),family="quasipoisson")
summary(model11)
#plot(model11)
#HotDaysExtreme is not significant

model12=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(dow)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")
summary(model12)
#plot(model12)

model13=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")
summary(model13)
#plot(model13)

model14=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
summary(model14)
#plot(model14)

model15=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(dow)+as.factor(HeatWavesModerate),family="quasipoisson")
summary(model15)
#plot(model15)

model16=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate),family="quasipoisson")
summary(model16)
#plot(model16)

model17=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(WinterWeather)+as.factor(ColdWavesModerate),family="quasipoisson")
summary(model17)
#plot(model17)

model18=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(holidays)+as.factor(ColdWavesModerate),family="quasipoisson")
summary(model18)
#plot(model18)


#Add other Wx indicators to best model from previous step to get the overall best model for the dnlm stage

model1a=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+as.factor(holidays)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
summary(model1a)
#plot(model1a)
#GCV=1.0655
#dev.expl= 9.58%

model1b=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+s(DTRC,k=3)+as.factor(holidays)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
summary(model1b)
plot(model1b)
#DTR is not significant

model1c=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+s(DTRC,k=7)+as.factor(holidays)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
summary(model1c)
#plot(model1c)
#Increasing dof for DTR doesn't make a difference

model1d=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+s(RH1pm,k=3)+as.factor(holidays)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
summary(model1d)
#plot(model1d)
#RH1pm is significant
#GCV=1.0647
#dev.expl= 9.62%

model1e=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+s(THIF7pm,k=3)+as.factor(holidays)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
summary(model1e)
#plot(model1e)
#THIF1pm is not significant

model1f=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+s(HxF1pm,k=3)+as.factor(holidays)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
summary(model1f)
#plot(model1f)
#HxF1pm is not significant

model1g=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+s(TdC1pm,k=3)+as.factor(holidays)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
summary(model1g)
#plot(model1g)
#TdC1pm is not significant (and all other times)

model1h=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+s(ATC1pm,k=3)+as.factor(holidays)+as.factor(dow)+as.factor(WinterWeather),family="quasipoisson")
summary(model1h)
#plot(model1h)
#ATC1pm is not significant (and all other times)

model1i=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(holidays)+as.factor(dow)+as.factor(WinterWeather)+s(PM2.5REVISED,k=3),family="quasipoisson")
summary(model1i)
plot(model1i)
#PM2.5 is not significant


model1f=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+s(HxF1pm,k=3)+as.factor(holidays)+as.factor(WinterWeather),family="poisson")
summary(model1f)
plot(model1f)
dispersiontest(model1f)
#BEST MODEL VA

model1b=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+s(DTRC,k=3)+as.factor(holidays)+as.factor(WinterWeather),family="poisson")
summary(model1b)
plot(model1b)
dispersiontest(model1b)

model1a=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(holidays)+as.factor(WinterWeather),family="poisson")
summary(model1a)
plot(model1a)
dispersiontest(model1a)

model1b=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+s(DTRC,k=3)+as.factor(holidays)+as.factor(dow)+as.factor(WinterWeather),family="poisson")
dispersiontest(model1b)

#Use quasipoisson


# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, DTRC=DTRC, holidays=holidays, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
plot(time)
basis.MaxTC=crossbasis(lagframe$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

# Define the position of knots for spline functions
varknotsMaxTC=equalknots(lagframe$MaxTC,fun="bs",df=4,degree=2)
varknotsMaxTC
lagknotsMaxTC=logknots(21,2)
lagknotsMaxTC

# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value
cb1.MaxTC=crossbasis(lagframe$MaxTC,lag=21,argvar=list(fun="bs",knots=varknotsMaxTC),arglag=list(knots=lagknotsMaxTC))
summary(cb1.MaxTC)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
modelA1=glm(Mort~cb1.MaxTC+ns(Trend, 16*3)+ns(DTRC, 3)+as.factor(holidays)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VA Mort")


# Plot of lag effects at selected values ("var=??")
plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
plot(modelA1)

# Plot of "heat map"
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Virginia (VA)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))

#overall plot
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Virginia (VA)")


#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Virginia (VA)"))

dev.off()



# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#

model1f=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+as.factor(holidays)+as.factor(dow),family="poisson")
summary(model1f)
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

#MaxTC <- VA$`AT(C)1pm`

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, holidays=holidays, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
plot(time)
basis.MaxTC=crossbasis(lagframe$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

# Define the position of knots for spline functions
varknotsMaxTC=equalknots(lagframe$MaxTC,fun="bs",df=4,degree=2)
varknotsMaxTC
lagknotsMaxTC=logknots(21,2)
lagknotsMaxTC

# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value
cb1.MaxTC=crossbasis(lagframe$MaxTC,lag=21,argvar=list(fun="bs",knots=varknotsMaxTC),arglag=list(knots=lagknotsMaxTC))
summary(cb1.MaxTC)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
modelA1=glm(Mort~cb1.MaxTC+ns(Trend, 16*3)+as.factor(holidays)+as.factor(dow),family=poisson(),lagframe)
summary(modelA1)
plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VA Mort")


# Plot of lag effects at selected values ("var=??")
plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
plot(modelA1)

# Plot of "heat map"
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Newport News (VA)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))

#overall plot
plot(pred1.MaxTC,"overall",xlab="Apparent Temperature 1PM (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Virginia (VA)")


#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Apparent Temperature 1PM (\u00B0C)" ,ylab="Lag (days)",main="Newport News (VA)"))


