# Consensus Models on each MDC Grouping

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

CHO <- read_excel("~/Desktop/CHO.Final.Weather.xlsx",na="NA")
CHO <- CHO[1:5844, ]

#Convert Data to numeric

CHO %>% mutate_if(is.character, as.numeric)
head(CHO)

for(i in 1:ncol(CHO)){
  
  assign(names(CHO)[i], CHO[[i]])
  
}


#Create Trend Term

Trend = seq(1,5844,1)

#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing CHO$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

Mort <- as.numeric(CHO$Mort)
MDC01 <- as.numeric(CHO$MDC.01)
MDC04 <- as.numeric(CHO$MDC.04)
MDC05 <- as.numeric(CHO$MDC.05)
MDC19 <- as.numeric(CHO$MDC.19)
NonBillICD <- as.numeric(CHO$Non.billable.ICD)
MaxTC <- as.numeric(CHO$`MaxT(C)`)
dow <- as.factor(CHO$dow)
ColdWavesModerate <- as.factor(CHO$ColdWavesModerate)
HeatWavesModerate <- as.factor(CHO$HeatWavesModerate)

# model1b=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate)+as.factor(HeatWavesModerate),family="poisson")
# summary(model1b)
# #plot(model1b)
# dispersiontest(model1b)

#Use quasipoisson


# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(Mort~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="CHO Mort")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_Mort_CHO_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Charlottesville Mort (CHO)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_Mort_CHO_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Charlottesville Mort (CHO)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_Mort_CHO_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Charlottesville Mort (CHO)"))
dev.off()

#total MDC01 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC01~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="CHO MDC01")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC01_CHO_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Charlottesville MDC01 (CHO)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC01_CHO_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Charlottesville MDC01 (CHO)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC01_CHO_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Charlottesville MDC01 (CHO)"))
dev.off()

#total MDC04 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC04~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="CHO MDC04")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC04_CHO_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Charlottesville MDC04 (CHO)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC04_CHO_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Charlottesville MDC04 (CHO)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC04_CHO_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Charlottesville MDC04 (CHO)"))
dev.off()

#total MDC05 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC05~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="CHO MDC05")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC05_CHO_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Charlottesville MDC05 (CHO)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC05_CHO_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Charlottesville MDC05 (CHO)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC05_CHO_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Charlottesville MDC05 (CHO)"))
dev.off()

#total MDC19 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC19~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="CHO MDC19")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC19_CHO_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Charlottesville MDC19 (CHO)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC19_CHO_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Charlottesville MDC19 (CHO)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC19_CHO_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Charlottesville MDC19 (CHO)"))
dev.off()

#total NonBillICD DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(NonBillICD~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="CHO NonBillICD")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_NonBillICD_CHO_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Charlottesville NonBillICD (CHO)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_NonBillICD_CHO_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Charlottesville NonBillICD (CHO)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_NonBillICD_CHO_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Charlottesville NonBillICD (CHO)"))
dev.off()

#Load in Data

EMV <- read_excel("~/Desktop/EMV.Final.Weather.xlsx",na="NA")
EMV <- EMV[1:5844, ]

#Convert Data to numeric

EMV %>% mutate_if(is.character, as.numeric)
head(EMV)

for(i in 1:ncol(EMV)){
  
  assign(names(EMV)[i], EMV[[i]])
  
}


#Create Trend Term

Trend = seq(1,5844,1)

#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing EMV$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

Mort <- as.numeric(EMV$Mort)
MDC01 <- as.numeric(EMV$MDC.01)
MDC04 <- as.numeric(EMV$MDC.04)
MDC05 <- as.numeric(EMV$MDC.05)
MDC19 <- as.numeric(EMV$MDC.19)
NonBillICD <- as.numeric(EMV$Non.billable.ICD)
MaxTC <- as.numeric(EMV$`MaxT(C)`)
dow <- as.factor(EMV$dow)
ColdWavesModerate <- as.factor(EMV$ColdWavesModerate)
HeatWavesModerate <- as.factor(EMV$HeatWavesModerate)

# model1b=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate)+as.factor(HeatWavesModerate),family="poisson")
# summary(model1b)
# #plot(model1b)
# dispersiontest(model1b)

#Use quasipoisson


# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(Mort~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EMV Mort")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_Mort_EMV_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Emporia Mort (EMV)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_Mort_EMV_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Emporia Mort (EMV)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_Mort_EMV_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Emporia Mort (EMV)"))
dev.off()

#total MDC01 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC01~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EMV MDC01")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC01_EMV_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Emporia MDC01 (EMV)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC01_EMV_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Emporia MDC01 (EMV)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC01_EMV_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Emporia MDC01 (EMV)"))
dev.off()

#total MDC04 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC04~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EMV MDC04")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC04_EMV_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Emporia MDC04 (EMV)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC04_EMV_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Emporia MDC04 (EMV)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC04_EMV_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Emporia MDC04 (EMV)"))
dev.off()

#total MDC05 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC05~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EMV MDC05")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC05_EMV_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Emporia MDC05 (EMV)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC05_EMV_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Emporia MDC05 (EMV)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC05_EMV_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Emporia MDC05 (EMV)"))
dev.off()

#total MDC19 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC19~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EMV MDC19")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC19_EMV_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Emporia MDC19 (EMV)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC19_EMV_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Emporia MDC19 (EMV)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC19_EMV_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Emporia MDC19 (EMV)"))
dev.off()

#total NonBillICD DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(NonBillICD~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EMV NonBillICD")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_NonBillICD_EMV_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Emporia NonBillICD (EMV)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_NonBillICD_EMV_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Emporia NonBillICD (EMV)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_NonBillICD_EMV_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Emporia NonBillICD (EMV)"))
dev.off()


#Load in Data

EZF <- read_excel("~/Desktop/EZF.Final.Weather.xlsx",na="NA")
EZF <- EZF[1:5844, ]

#Convert Data to numeric

EZF %>% mutate_if(is.character, as.numeric)
head(EZF)

for(i in 1:ncol(EZF)){
  
  assign(names(EZF)[i], EZF[[i]])
  
}


#Create Trend Term

Trend = seq(1,5844,1)

#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing EZF$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

Mort <- as.numeric(EZF$Mort)
MDC01 <- as.numeric(EZF$MDC.01)
MDC04 <- as.numeric(EZF$MDC.04)
MDC05 <- as.numeric(EZF$MDC.05)
MDC19 <- as.numeric(EZF$MDC.19)
NonBillICD <- as.numeric(EZF$Non.billable.ICD)
MaxTC <- as.numeric(EZF$`MaxT(C)`)
dow <- as.factor(EZF$dow)
ColdWavesModerate <- as.factor(EZF$ColdWavesModerate)
HeatWavesModerate <- as.factor(EZF$HeatWavesModerate)

# model1b=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate)+as.factor(HeatWavesModerate),family="poisson")
# summary(model1b)
# #plot(model1b)
# dispersiontest(model1b)

#Use quasipoisson


# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(Mort~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EZF Mort")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_Mort_EZF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Fredericksburg Mort (EZF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_Mort_EZF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Fredericksburg Mort (EZF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_Mort_EZF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Fredericksburg Mort (EZF)"))
dev.off()

#total MDC01 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC01~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EZF MDC01")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC01_EZF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Fredericksburg MDC01 (EZF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC01_EZF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Fredericksburg MDC01 (EZF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC01_EZF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Fredericksburg MDC01 (EZF)"))
dev.off()

#total MDC04 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC04~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EZF MDC04")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC04_EZF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Fredericksburg MDC04 (EZF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC04_EZF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Fredericksburg MDC04 (EZF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC04_EZF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Fredericksburg MDC04 (EZF)"))
dev.off()

#total MDC05 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC05~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EZF MDC05")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC05_EZF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Fredericksburg MDC05 (EZF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC05_EZF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Fredericksburg MDC05 (EZF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC05_EZF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Fredericksburg MDC05 (EZF)"))
dev.off()

#total MDC19 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC19~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EZF MDC19")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC19_EZF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Fredericksburg MDC19 (EZF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC19_EZF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Fredericksburg MDC19 (EZF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC19_EZF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Fredericksburg MDC19 (EZF)"))
dev.off()

#total NonBillICD DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(NonBillICD~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EZF NonBillICD")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_NonBillICD_EZF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Fredericksburg NonBillICD (EZF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_NonBillICD_EZF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Fredericksburg NonBillICD (EZF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_NonBillICD_EZF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Fredericksburg NonBillICD (EZF)"))
dev.off()


#Load in Data

IAD <- read_excel("~/Desktop/IAD.Final.Weather.xlsx",na="NA")
IAD <- IAD[1:5844, ]

#Convert Data to numeric

IAD %>% mutate_if(is.character, as.numeric)
head(IAD)

for(i in 1:ncol(IAD)){
  
  assign(names(IAD)[i], IAD[[i]])
  
}


#Create Trend Term

Trend = seq(1,5844,1)

#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing IAD$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

Mort <- as.numeric(IAD$Mort)
MDC01 <- as.numeric(IAD$MDC.01)
MDC04 <- as.numeric(IAD$MDC.04)
MDC05 <- as.numeric(IAD$MDC.05)
MDC19 <- as.numeric(IAD$MDC.19)
NonBillICD <- as.numeric(IAD$Non.billable.ICD)
MaxTC <- as.numeric(IAD$`MaxT(C)`)
dow <- as.factor(IAD$dow)
ColdWavesModerate <- as.factor(IAD$ColdWavesModerate)
HeatWavesModerate <- as.factor(IAD$HeatWavesModerate)

# model1b=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate)+as.factor(HeatWavesModerate),family="poisson")
# summary(model1b)
# #plot(model1b)
# dispersiontest(model1b)

#Use quasipoisson


# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(Mort~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="IAD Mort")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_Mort_IAD_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Dulles Mort (IAD)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_Mort_IAD_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Dulles Mort (IAD)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_Mort_IAD_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Dulles Mort (IAD)"))
dev.off()

#total MDC01 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC01~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="IAD MDC01")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC01_IAD_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Dulles MDC01 (IAD)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC01_IAD_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Dulles MDC01 (IAD)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC01_IAD_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Dulles MDC01 (IAD)"))
dev.off()

#total MDC04 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC04~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="IAD MDC04")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC04_IAD_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Dulles MDC04 (IAD)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC04_IAD_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Dulles MDC04 (IAD)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC04_IAD_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Dulles MDC04 (IAD)"))
dev.off()

#total MDC05 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC05~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="IAD MDC05")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC05_IAD_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Dulles MDC05 (IAD)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC05_IAD_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Dulles MDC05 (IAD)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC05_IAD_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Dulles MDC05 (IAD)"))
dev.off()

#total MDC19 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC19~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="IAD MDC19")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC19_IAD_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Dulles MDC19 (IAD)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC19_IAD_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Dulles MDC19 (IAD)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC19_IAD_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Dulles MDC19 (IAD)"))
dev.off()

#total NonBillICD DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(NonBillICD~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="IAD NonBillICD")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_NonBillICD_IAD_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Dulles NonBillICD (IAD)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_NonBillICD_IAD_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Dulles NonBillICD (IAD)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_NonBillICD_IAD_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Dulles NonBillICD (IAD)"))
dev.off()


#Load in Data

LYH <- read_excel("~/Desktop/LYH.Final.Weather.xlsx",na="NA")
LYH <- LYH[1:5844, ]

#Convert Data to numeric

LYH %>% mutate_if(is.character, as.numeric)
head(LYH)

for(i in 1:ncol(LYH)){
  
  assign(names(LYH)[i], LYH[[i]])
  
}


#Create Trend Term

Trend = seq(1,5844,1)

#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing LYH$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

Mort <- as.numeric(LYH$Mort)
MDC01 <- as.numeric(LYH$MDC.01)
MDC04 <- as.numeric(LYH$MDC.04)
MDC05 <- as.numeric(LYH$MDC.05)
MDC19 <- as.numeric(LYH$MDC.19)
NonBillICD <- as.numeric(LYH$Non.billable.ICD)
MaxTC <- as.numeric(LYH$`MaxT(C)`)
dow <- as.factor(LYH$dow)
ColdWavesModerate <- as.factor(LYH$ColdWavesModerate)
HeatWavesModerate <- as.factor(LYH$HeatWavesModerate)

# model1b=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate)+as.factor(HeatWavesModerate),family="poisson")
# summary(model1b)
# #plot(model1b)
# dispersiontest(model1b)

#Use quasipoisson


# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(Mort~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="LYH Mort")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_Mort_LYH_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Lynchburg Mort (LYH)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_Mort_LYH_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Lynchburg Mort (LYH)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_Mort_LYH_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Lynchburg Mort (LYH)"))
dev.off()

#total MDC01 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC01~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="LYH MDC01")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC01_LYH_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Lynchburg MDC01 (LYH)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC01_LYH_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Lynchburg MDC01 (LYH)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC01_LYH_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Lynchburg MDC01 (LYH)"))
dev.off()

#total MDC04 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC04~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="LYH MDC04")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC04_LYH_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Lynchburg MDC04 (LYH)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC04_LYH_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Lynchburg MDC04 (LYH)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC04_LYH_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Lynchburg MDC04 (LYH)"))
dev.off()

#total MDC05 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC05~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="LYH MDC05")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC05_LYH_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Lynchburg MDC05 (LYH)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC05_LYH_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Lynchburg MDC05 (LYH)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC05_LYH_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Lynchburg MDC05 (LYH)"))
dev.off()

#total MDC19 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC19~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="LYH MDC19")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC19_LYH_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Lynchburg MDC19 (LYH)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC19_LYH_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Lynchburg MDC19 (LYH)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC19_LYH_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Lynchburg MDC19 (LYH)"))
dev.off()

#total NonBillICD DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(NonBillICD~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="LYH NonBillICD")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_NonBillICD_LYH_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Lynchburg NonBillICD (LYH)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_NonBillICD_LYH_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Lynchburg NonBillICD (LYH)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_NonBillICD_LYH_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Lynchburg NonBillICD (LYH)"))
dev.off()

#Load in Data

OKV <- read_excel("~/Desktop/OKV.Final.Weather.xlsx",na="NA")
OKV <- OKV[1:5844, ]

#Convert Data to numeric

OKV %>% mutate_if(is.character, as.numeric)
head(OKV)

for(i in 1:ncol(OKV)){
  
  assign(names(OKV)[i], OKV[[i]])
  
}


#Create Trend Term

Trend = seq(1,5844,1)

#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing OKV$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

Mort <- as.numeric(OKV$Mort)
MDC01 <- as.numeric(OKV$MDC.01)
MDC04 <- as.numeric(OKV$MDC.04)
MDC05 <- as.numeric(OKV$MDC.05)
MDC19 <- as.numeric(OKV$MDC.19)
NonBillICD <- as.numeric(OKV$Non.billable.ICD)
MaxTC <- as.numeric(OKV$`MaxT(C)`)
dow <- as.factor(OKV$dow)
ColdWavesModerate <- as.factor(OKV$ColdWavesModerate)
HeatWavesModerate <- as.factor(OKV$HeatWavesModerate)

# model1b=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate)+as.factor(HeatWavesModerate),family="poisson")
# summary(model1b)
# #plot(model1b)
# dispersiontest(model1b)

#Use quasipoisson


# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(Mort~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="OKV Mort")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_Mort_OKV_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Winchester Mort (OKV)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_Mort_OKV_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Winchester Mort (OKV)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_Mort_OKV_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Winchester Mort (OKV)"))
dev.off()

#total MDC01 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC01~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="OKV MDC01")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC01_OKV_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Winchester MDC01 (OKV)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC01_OKV_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Winchester MDC01 (OKV)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC01_OKV_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Winchester MDC01 (OKV)"))
dev.off()

#total MDC04 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC04~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="OKV MDC04")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC04_OKV_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Winchester MDC04 (OKV)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC04_OKV_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Winchester MDC04 (OKV)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC04_OKV_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Winchester MDC04 (OKV)"))
dev.off()

#total MDC05 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC05~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="OKV MDC05")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC05_OKV_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Winchester MDC05 (OKV)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC05_OKV_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Winchester MDC05 (OKV)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC05_OKV_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Winchester MDC05 (OKV)"))
dev.off()

#total MDC19 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC19~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="OKV MDC19")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC19_OKV_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Winchester MDC19 (OKV)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC19_OKV_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Winchester MDC19 (OKV)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC19_OKV_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Winchester MDC19 (OKV)"))
dev.off()

#total NonBillICD DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(NonBillICD~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="OKV NonBillICD")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_NonBillICD_OKV_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Winchester NonBillICD (OKV)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_NonBillICD_OKV_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Winchester NonBillICD (OKV)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_NonBillICD_OKV_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Winchester NonBillICD (OKV)"))
dev.off()


#Load in Data

ORF <- read_excel("~/Desktop/ORF.Final.Weather.xlsx",na="NA")
ORF <- ORF[1:5844, ]

#Convert Data to numeric

ORF %>% mutate_if(is.character, as.numeric)
head(ORF)

for(i in 1:ncol(ORF)){
  
  assign(names(ORF)[i], ORF[[i]])
  
}


#Create Trend Term

Trend = seq(1,5844,1)

#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing ORF$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

Mort <- as.numeric(ORF$Mort)
MDC01 <- as.numeric(ORF$MDC.01)
MDC04 <- as.numeric(ORF$MDC.04)
MDC05 <- as.numeric(ORF$MDC.05)
MDC19 <- as.numeric(ORF$MDC.19)
NonBillICD <- as.numeric(ORF$Non.billable.ICD)
MaxTC <- as.numeric(ORF$`MaxT(C)`)
dow <- as.factor(ORF$dow)
ColdWavesModerate <- as.factor(ORF$ColdWavesModerate)
HeatWavesModerate <- as.factor(ORF$HeatWavesModerate)

# model1b=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate)+as.factor(HeatWavesModerate),family="poisson")
# summary(model1b)
# #plot(model1b)
# dispersiontest(model1b)

#Use quasipoisson


# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(Mort~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ORF Mort")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_Mort_ORF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Norfolk Mort (ORF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_Mort_ORF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Norfolk Mort (ORF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_Mort_ORF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Norfolk Mort (ORF)"))
dev.off()

#total MDC01 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC01~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ORF MDC01")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC01_ORF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Norfolk MDC01 (ORF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC01_ORF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Norfolk MDC01 (ORF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC01_ORF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Norfolk MDC01 (ORF)"))
dev.off()

#total MDC04 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC04~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ORF MDC04")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC04_ORF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Norfolk MDC04 (ORF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC04_ORF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Norfolk MDC04 (ORF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC04_ORF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Norfolk MDC04 (ORF)"))
dev.off()

#total MDC05 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC05~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ORF MDC05")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC05_ORF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Norfolk MDC05 (ORF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC05_ORF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Norfolk MDC05 (ORF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC05_ORF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Norfolk MDC05 (ORF)"))
dev.off()

#total MDC19 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC19~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ORF MDC19")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC19_ORF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Norfolk MDC19 (ORF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC19_ORF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Norfolk MDC19 (ORF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC19_ORF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Norfolk MDC19 (ORF)"))
dev.off()

#total NonBillICD DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(NonBillICD~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ORF NonBillICD")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_NonBillICD_ORF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Norfolk NonBillICD (ORF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_NonBillICD_ORF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Norfolk NonBillICD (ORF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_NonBillICD_ORF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Norfolk NonBillICD (ORF)"))
dev.off()





#Load in Data

PHF <- read_excel("~/Desktop/PHF.Final.Weather.xlsx",na="NA")
PHF <- PHF[1:5844, ]

#Convert Data to numeric

PHF %>% mutate_if(is.character, as.numeric)
head(PHF)

for(i in 1:ncol(PHF)){
  
  assign(names(PHF)[i], PHF[[i]])
  
}


#Create Trend Term

Trend = seq(1,5844,1)

#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing PHF$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

Mort <- as.numeric(PHF$Mort)
MDC01 <- as.numeric(PHF$MDC.01)
MDC04 <- as.numeric(PHF$MDC.04)
MDC05 <- as.numeric(PHF$MDC.05)
MDC19 <- as.numeric(PHF$MDC.19)
NonBillICD <- as.numeric(PHF$Non.billable.ICD)
MaxTC <- as.numeric(PHF$`MaxT(C)`)
dow <- as.factor(PHF$dow)
ColdWavesModerate <- as.factor(PHF$ColdWavesModerate)
HeatWavesModerate <- as.factor(PHF$HeatWavesModerate)

# model1b=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate)+as.factor(HeatWavesModerate),family="poisson")
# summary(model1b)
# #plot(model1b)
# dispersiontest(model1b)

#Use quasipoisson


# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(Mort~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="PHF Mort")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_Mort_PHF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Newport News Mort (PHF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_Mort_PHF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Newport News Mort (PHF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_Mort_PHF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Newport News Mort (PHF)"))
dev.off()

#total MDC01 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC01~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="PHF MDC01")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC01_PHF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Newport News MDC01 (PHF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC01_PHF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Newport News MDC01 (PHF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC01_PHF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Newport News MDC01 (PHF)"))
dev.off()

#total MDC04 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC04~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="PHF MDC04")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC04_PHF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Newport News MDC04 (PHF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC04_PHF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Newport News MDC04 (PHF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC04_PHF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Newport News MDC04 (PHF)"))
dev.off()

#total MDC05 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC05~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="PHF MDC05")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC05_PHF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Newport News MDC05 (PHF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC05_PHF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Newport News MDC05 (PHF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC05_PHF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Newport News MDC05 (PHF)"))
dev.off()

#total MDC19 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC19~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="PHF MDC19")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC19_PHF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Newport News MDC19 (PHF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC19_PHF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Newport News MDC19 (PHF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC19_PHF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Newport News MDC19 (PHF)"))
dev.off()

#total NonBillICD DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(NonBillICD~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="PHF NonBillICD")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_NonBillICD_PHF_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Newport News NonBillICD (PHF)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_NonBillICD_PHF_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Newport News NonBillICD (PHF)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_NonBillICD_PHF_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Newport News NonBillICD (PHF)"))
dev.off()





#Load in Data

RIC <- read_excel("~/Desktop/RIC.Final.Weather.xlsx",na="NA")
RIC <- RIC[1:5844, ]

#Convert Data to numeric

RIC %>% mutate_if(is.character, as.numeric)
head(RIC)

for(i in 1:ncol(RIC)){
  
  assign(names(RIC)[i], RIC[[i]])
  
}


#Create Trend Term

Trend = seq(1,5844,1)

#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing RIC$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

Mort <- as.numeric(RIC$Mort)
MDC01 <- as.numeric(RIC$MDC.01)
MDC04 <- as.numeric(RIC$MDC.04)
MDC05 <- as.numeric(RIC$MDC.05)
MDC19 <- as.numeric(RIC$MDC.19)
NonBillICD <- as.numeric(RIC$Non.billable.ICD)
MaxTC <- as.numeric(RIC$`MaxT(C)`)
dow <- as.factor(RIC$dow)
ColdWavesModerate <- as.factor(RIC$ColdWavesModerate)
HeatWavesModerate <- as.factor(RIC$HeatWavesModerate)

# model1b=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate)+as.factor(HeatWavesModerate),family="poisson")
# summary(model1b)
# #plot(model1b)
# dispersiontest(model1b)

#Use quasipoisson


# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(Mort~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="RIC Mort")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_Mort_RIC_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Richmond Mort (RIC)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_Mort_RIC_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Richmond Mort (RIC)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_Mort_RIC_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Richmond Mort (RIC)"))
dev.off()

#total MDC01 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC01~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="RIC MDC01")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC01_RIC_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Richmond MDC01 (RIC)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC01_RIC_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Richmond MDC01 (RIC)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC01_RIC_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Richmond MDC01 (RIC)"))
dev.off()

#total MDC04 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC04~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="RIC MDC04")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC04_RIC_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Richmond MDC04 (RIC)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC04_RIC_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Richmond MDC04 (RIC)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC04_RIC_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Richmond MDC04 (RIC)"))
dev.off()

#total MDC05 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC05~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="RIC MDC05")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC05_RIC_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Richmond MDC05 (RIC)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC05_RIC_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Richmond MDC05 (RIC)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC05_RIC_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Richmond MDC05 (RIC)"))
dev.off()

#total MDC19 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC19~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="RIC MDC19")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC19_RIC_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Richmond MDC19 (RIC)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC19_RIC_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Richmond MDC19 (RIC)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC19_RIC_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Richmond MDC19 (RIC)"))
dev.off()

#total NonBillICD DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(NonBillICD~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="RIC NonBillICD")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_NonBillICD_RIC_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Richmond NonBillICD (RIC)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_NonBillICD_RIC_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Richmond NonBillICD (RIC)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_NonBillICD_RIC_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Richmond NonBillICD (RIC)"))
dev.off()



#Load in Data

ROA <- read_excel("~/Desktop/ROA.Final.Weather.xlsx",na="NA")
ROA <- ROA[1:5844, ]

#Convert Data to numeric

ROA %>% mutate_if(is.character, as.numeric)
head(ROA)

for(i in 1:ncol(ROA)){
  
  assign(names(ROA)[i], ROA[[i]])
  
}


#Create Trend Term

Trend = seq(1,5844,1)

#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing ROA$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

Mort <- as.numeric(ROA$Mort)
MDC01 <- as.numeric(ROA$MDC.01)
MDC04 <- as.numeric(ROA$MDC.04)
MDC05 <- as.numeric(ROA$MDC.05)
MDC19 <- as.numeric(ROA$MDC.19)
NonBillICD <- as.numeric(ROA$Non.billable.ICD)
MaxTC <- as.numeric(ROA$`MaxT(C)`)
dow <- as.factor(ROA$dow)
ColdWavesModerate <- as.factor(ROA$ColdWavesModerate)
HeatWavesModerate <- as.factor(ROA$HeatWavesModerate)

# model1b=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate)+as.factor(HeatWavesModerate),family="poisson")
# summary(model1b)
# #plot(model1b)
# dispersiontest(model1b)

#Use quasipoisson


# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(Mort~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ROA Mort")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_Mort_ROA_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Roanoke Mort (ROA)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_Mort_ROA_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Roanoke Mort (ROA)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_Mort_ROA_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Roanoke Mort (ROA)"))
dev.off()

#total MDC01 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC01~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ROA MDC01")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC01_ROA_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Roanoke MDC01 (ROA)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC01_ROA_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Roanoke MDC01 (ROA)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC01_ROA_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Roanoke MDC01 (ROA)"))
dev.off()

#total MDC04 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC04~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ROA MDC04")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC04_ROA_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Roanoke MDC04 (ROA)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC04_ROA_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Roanoke MDC04 (ROA)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC04_ROA_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Roanoke MDC04 (ROA)"))
dev.off()

#total MDC05 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC05~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ROA MDC05")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC05_ROA_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Roanoke MDC05 (ROA)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC05_ROA_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Roanoke MDC05 (ROA)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC05_ROA_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Roanoke MDC05 (ROA)"))
dev.off()

#total MDC19 DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(MDC19~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ROA MDC19")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_MDC19_ROA_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Roanoke MDC19 (ROA)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_MDC19_ROA_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Roanoke MDC19 (ROA)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_MDC19_ROA_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Roanoke MDC19 (ROA)"))
dev.off()

#total NonBillICD DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
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
modelA1=glm(NonBillICD~cb1.MaxTC+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe)
summary(modelA1)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC=crosspred(cb1.MaxTC,modelA1,by=1)

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ROA NonBillICD")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
png("MDC_NonBillICD_ROA_Heat.png", width = 602, height = 482)
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Roanoke NonBillICD (ROA)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))
dev.off()

#overall plot
png("MDC_NonBillICD_ROA_Curve.png", width = 602, height = 482)
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Roanoke NonBillICD (ROA)")
dev.off()

#Heat map with more levels
noeff=1
levels=pretty(pred1.MaxTC$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))

png("MDC_NonBillICD_ROA_Heat_Extra.png", width = 602, height = 482)
filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Roanoke NonBillICD (ROA)"))
dev.off()























