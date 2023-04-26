# Use this for Race

library(readxl)
library(dplyr)

# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#Load in Data

VA <- read_excel("~/Desktop/VA.Final.Weather.xlsx",na="NA")
VA <- VA[1:5844, ]

#Convert Data to numeric

VA %>% mutate_if(is.character, as.numeric)
#head(VA)

VA$MaxTC <- VA$`MaxT(C)`
VA$MinTC <- VA$`MinT(C)`
VA$MaxTDepC <- VA$`MaxTDep(C)`
VA$MinTDepC <- VA$`MinTDep(C)`
VA$ATC1am <- VA$`AT(C)1am`
VA$ATC7am <- VA$`AT(C)7am`
VA$ATC1pm <- VA$`AT(C)1pm`
VA$ATC7pm <- VA$`AT(C)7pm`
VA$DTRC <- VA$`DTR(C)`
VA$TC1am <- VA$`T(C)1am`
VA$TC7am <- VA$`T(C)7am`
VA$TC1pm <- VA$`T(C)1pm`
VA$TC7pm <- VA$`T(C)7pm`

for(i in 1:ncol(VA)){
  assign(names(VA)[i], VA[[i]])
}


#Create Trend Term

Trend = seq(1,5844,1)
VA$AccountChange2015 <- ifelse(VA$Year >= 2015, 1, 0)
AccountChange2015 <- VA$AccountChange2015
#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing VA$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

#VA

ifelse((sum(is.na(VA$PM2.5REVISED)) > 1000), "STOP, ADJUST YOUR MODEL", "Keep on going!")
ifelse((sum(is.na(VA$SLPhPa7pm)) > 1000), "STOP, ADJUST YOUR MODEL", "Keep on going!")
ifelse((sum(is.na(VA$WinterWeather)) > 1000), "STOP, ADJUST YOUR MODEL", "Keep on going!")


#SPECIFY WHAT'S YOUR MORT

Mort <- MortMortVA
MainSpline <- MaxTC
#SecondSpline <- PM2.5REVISED
#ThirdSpline <- SLPhPa7pm
Factor1 <- dow
#Factor2 <- HeatWavesModerate
#Factor3 <- WinterWeather
#Factor4 <- ColdWavesModerate
#Factor5 <- holidays
#
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

#SecondSpline=SecondSpline,ThirdSpline=ThirdSpline,
#,Factor2=Factor2,Factor3=Factor3,Factor4=Factor4,Factor5=Factor5
# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(Mort=Mort, Trend=Trend, MainSpline=MainSpline,Factor1=Factor1,datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MainSpline=crossbasis(lagframe$MainSpline,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

# Define the position of knots for spline functions
varknotsMainSpline=equalknots(lagframe$MainSpline,fun="bs",df=4,degree=2)
varknotsMainSpline
lagknotsMainSpline=logknots(21,2)
lagknotsMainSpline

# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value
cb1.MainSpline=crossbasis(lagframe$MainSpline,lag=21,argvar=list(fun="bs",knots=varknotsMainSpline),arglag=list(knots=lagknotsMainSpline))
summary(cb1.MainSpline)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
#+ns(SecondSpline,3)+ns(ThirdSpline,3)
#+as.factor(Factor2)+as.factor(Factor3)+as.factor(Factor4)+as.factor(Factor5)
modelA1=glm(Mort~cb1.MainSpline+ns(Trend, 16*3)+as.factor(Factor1),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MainSpline=crosspred(cb1.MainSpline,modelA1,by=1)

# 3-D plot
plot(pred1.MainSpline,xlab="MainSpline",zlab="RR",theta=200,phi=40,lphi=30,main="VA Mort")


# Plot of lag effects at selected values ("var=??")
plot(pred1.MainSpline,ptype="slices",var=c(50),col='Total',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
plot(pred1.MainSpline,ptype="slices",var=c(25),col='Total',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MainSpline$allRRfit,pred1.MainSpline$allRRlow,pred1.MainSpline$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
plot(pred1.MainSpline,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Virginia Total Mortality Risk by Temperature and Lag",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))

#overall plot
plot(pred1.MainSpline,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Virginia Total 0-21 Day Mortality Risk vs Temperature")

test <- pred1.MainSpline$allRRfit
MortDLNM <- pred1.MainSpline

Consensus_Curve_VA_Total
Consensus_Heat_VA_Total

#Image dimensinos 602 x 482

#Heat map with more levels
noeff=1
levels=pretty(pred1.MainSpline$matRRfit,100)
col1 <- colorRampPalette(c("blue","Total"))
col2 <- colorRampPalette(c("Total","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MainSpline$predvar,y=seq(0,21,1),z=pred1.MainSpline$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Lynchburg (VA)"))
# 
# dev.off()

MortLag <- MortDLNM$matRRfit
MortLag <- MortDLNM$matRRfit


