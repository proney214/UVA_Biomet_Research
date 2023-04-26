###### Create DLNMs for the MDCs with Full AQ
library(splines)
library(dlnm)
library(lubridate)
library(readxl)
library(mgcv)
library(ggplot2)
library(dplyr)
library(AER)
library(qcc)

ROA_Final_Weather <- read_excel("~/Desktop/ROA.Final.Weather.xlsx")
ROA <- ROA_Final_Weather[1:5844,]

ROA %>% mutate_if(is.character, as.numeric)
ROA$HotDaysExtreme <- as.numeric(ROA$HotDaysExtreme)
ROA$date <- as.Date(ROA$Date)

ROA$Trend <- seq(1,5844,1)
for(i in 1:ncol(ROA)){
  assign(names(ROA)[i], ROA[[i]])
}

ATC1pm <- as.numeric(ROA$`AT(C)1pm`)
MaxTF <- as.numeric(ROA$MaxTF)
MaxTC <- as.numeric(ROA$`MaxT(C)`)
PM2.5REVISED <- as.numeric(ROA$PM2.5REVISED)
OzoneREVISED <- as.numeric(ROA$OzoneREVISED)
Mort <- as.numeric(ROA$Mort)

lagframe=data.frame(Trend,MaxTC,ATC1pm,TF7am,Mort,MDC.01,MDC.04,MDC.05,OzoneREVISED,PM2.5REVISED)

varknotsMaxTC=equalknots(MaxTC, nk=NULL, fun="bs",df=5,degree=3)
varknotsMaxTC
lagknotsMaxTC=logknots(21,3)
lagknotsMaxTC

# "Crossbasis" is the key command of the DLNM.  It describes the two-dimensional relationship between your basis variable and the lag.  The first item is the variable you are using as your basis (in this case, maximum temperature),
# "lag" is the maximum lag you will examine (21 days)
# argvar and arglag are passing data to a spline-fitting routine ("onebasis") that will generate the matrices for predictor and lags.

cb1.MaxTC=crossbasis(lagframe$MaxTC,lag=21,argvar=list(fun="bs",knots=varknotsMaxTC),arglag=list(fun="ns",knots=lagknotsMaxTC))
summary(cb1.MaxTC)

# Now run best GAM model as GLM (note the syntax between GAMs and GLMs is slightly different, but you have one (or more) main variable(s) (your crossbasis), splines (ns, this is like controlling for a variable in a GAM, where the degrees of freedom are specified for each), and various "factor" variables (categorical or binary).

modelMDC.04=glm(MDC.04~cb1.MaxTC+ns(PM2.5REVISED, 3)+ns(OzoneREVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.04)
#plot(modelMDC.04)
#dispersiontest(modelMDC.04)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.04=crosspred(cb1.MaxTC,modelMDC.04,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="ROA MDC.04 Mortality")

# Plot of "heat map"
noeff=1
levels=pretty(pred1.MaxTC.MDC.04$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC.MDC.04$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.04$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="ROA MDC.04: MaxTC"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("ROA MDC.04 Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

plot(pred1.MaxTC.MDC.04,ptype="slices",var=c(-5),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA MDC.04: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.1))
plot(pred1.MaxTC.MDC.04,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA MDC.04: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.1))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(0,10,25,40),lag=c(0,3,14,21),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.MaxTC.MDC.04,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="ROA MDC.04 Overall Cumulative Association")


###DLNM for MDC.05
modelMDC.05=glm(MDC.05~cb1.MaxTC+ns(PM2.5REVISED, 3)+ns(OzoneREVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.05)
#plot(modelMDC.05)
#dispersiontest(modelMDC.05)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.05=crosspred(cb1.MaxTC,modelMDC.05,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="ROA MDC.05 Mortality")

# Plot of "heat map"

noeff=1
levels=pretty(pred1.MaxTC.MDC.05$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC.MDC.05$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.05$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="ROA MDC.05: MaxTC"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("ROA MDC.05 Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.MaxTC,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA: RR vs. Lag (days) at 10°C (MaxTC)")
#plot(pred1.MaxTC,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA: RR vs. Lag (days) at 30°C (MaxTC)")

plot(pred1.MaxTC.MDC.05,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA MDC.05: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.1))
plot(pred1.MaxTC.MDC.05,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA MDC.05: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.1))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.MaxTC.MDC.05,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="ROA MDC.05 Overall Cumulative Association")

###DLNM for MDC.01
modelMDC.01=glm(MDC.01~cb1.MaxTC+ns(PM2.5REVISED, 3)+ns(OzoneREVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.01)
#plot(modelMDC.01)
#dispersiontest(modelMDC.01)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.01=crosspred(cb1.MaxTC,modelMDC.01,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="ROA MDC.01 Mortality")

# Plot of "heat map"

noeff=1
levels=pretty(pred1.MaxTC.MDC.01$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC.MDC.01$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.01$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="ROA MDC.01: MaxTC"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("ROA MDC.01 Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.MaxTC,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA: RR vs. Lag (days) at 10°C (MaxTC)")
#plot(pred1.MaxTC,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA: RR vs. Lag (days) at 30°C (MaxTC)")

plot(pred1.MaxTC.MDC.01,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA MDC.01: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.01,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA MDC.01: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.2))


# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.MaxTC.MDC.01,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="ROA MDC.01 Overall Cumulative Association")


###DLNM for Mort
modelMort=glm(Mort~cb1.MaxTC+ns(PM2.5REVISED, 3)+ns(OzoneREVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMort)
#plot(modelMort)
#dispersiontest(modelMort)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.Mort=crosspred(cb1.MaxTC,modelMort,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="ROA Mort Mortality")

# Plot of "heat map"

noeff=1
levels=pretty(pred1.MaxTC.MDC.Mort$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC.MDC.Mort$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.Mort$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="ROA Total Mortality"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("ROA Mort Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.MaxTC,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA: RR vs. Lag (days) at 10°C (MaxTC)")
#plot(pred1.MaxTC,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA: RR vs. Lag (days) at 30°C (MaxTC)")

plot(pred1.MaxTC.MDC.Mort,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA Mortality Total: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.Mort,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="ROA Mortality Total: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.2))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.MaxTC.MDC.Mort,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="ROA Total Mortality Overall Cumulative Association")


plot(pred1.MaxTC.MDC.05$matRRfit)

test01 <- cor(pred1.MaxTC.MDC.01$matRRfit, pred1.MaxTC.MDC.Mort$matRRfit)
MDC01corvalueswithTotal <- data.frame(diag(as.matrix(test01)))

test04 <- cor(pred1.MaxTC.MDC.04$matRRfit, pred1.MaxTC.MDC.Mort$matRRfit)
MDC04corvalueswithTotal <- data.frame(diag(as.matrix(test04)))

test05 <- cor(pred1.MaxTC.MDC.05$matRRfit, pred1.MaxTC.MDC.Mort$matRRfit)
MDC05corvalueswithTotal <- data.frame(diag(as.matrix(test05)))

testMort <- cor(pred1.MaxTC.MDC.Mort$matRRfit, pred1.MaxTC.MDC.Mort$matRRfit)
MDCMortcorvalueswithTotal <- data.frame(diag(as.matrix(testMort)))

corsum <- cbind(MDC01corvalueswithTotal, MDC04corvalueswithTotal,MDC05corvalueswithTotal)

# Plot :)
plot(corsum$diag.as.matrix.test04..,col='blue',type='l',main="ROA Cummulative Lag Correlations with Total",ylab="Correlation Value",xlab="Lag (Days)",ylim = c(-1,1))
lines(corsum$diag.as.matrix.test05..,col='red')
lines(corsum$diag.as.matrix.test01..,col='green')
abline(h=0)
abline(h=0.433,lty=2)
abline(h=(-0.433),lty=2)
legend(16, -0.5, legend=c("MDC05", "MDC04","MDC01"),
       col=c("red", "blue","green"), lty=1, cex=0.8)

# Correlation at 40

MDC01RR <- data.frame(pred1.MaxTC.MDC.01$matRRfit)
MDC04RR <- data.frame(pred1.MaxTC.MDC.04$matRRfit)
MDC05RR <- data.frame(pred1.MaxTC.MDC.05$matRRfit)
MDCMoRR <- data.frame(pred1.MaxTC.MDC.Mort$matRRfit)

MDC01RR40 <- t(MDC01RR['40', ])
MDC04RR40 <- t(MDC04RR['40', ])
MDC05RR40 <- t(MDC05RR['40', ])
MDCMoRR40 <- t(MDCMoRR['40', ])

test01_40 <- cor(MDC01RR40, MDCMoRR40)
MDC01corvalueswithTotal_40 <- data.frame(diag(as.matrix(test01_40)))

test04_40 <- cor(MDC04RR40, MDCMoRR40)
MDC04corvalueswithTotal_40 <- data.frame(diag(as.matrix(test04_40)))

test05_40 <- cor(MDC05RR40, MDCMoRR40)
MDC05corvalueswithTotal_40 <- data.frame(diag(as.matrix(test05_40)))

testMort_40 <- cor(MDCMoRR40, MDCMoRR40)
MDCMortcorvalueswithTotal_40 <- data.frame(diag(as.matrix(testMort)))

corsum_40 <- cbind(MDC01corvalueswithTotal_40, MDC04corvalueswithTotal_40,MDC05corvalueswithTotal_40)

plot(corsum_40$diag.as.matrix.test04_40..,col='blue',type='l',main="ROA Cummulative Lag Correlations with Total",ylab="Correlation Value",xlab="Lag (Days)")
lines(corsum_40$diag.as.matrix.test05_40..,col='red')
lines(corsum_40$diag.as.matrix.test01_40..,col='green')
abline(h=0.433)
legend(13, 0, legend=c("MDC05", "MDC04","MDC01"),
       col=c("red", "blue","green"), lty=1, cex=0.8)

# Correlations at 0 C

MDC01RR0 <- t(MDC01RR['0', ])
MDC04RR0 <- t(MDC04RR['0', ])
MDC05RR0 <- t(MDC05RR['0', ])
MDCMoRR0 <- t(MDCMoRR['0', ])

test01_0 <- cor(MDC01RR0, MDCMoRR0)
MDC01corvalueswithTotal_0 <- data.frame(diag(as.matrix(test01_0)))

test04_0 <- cor(MDC04RR0, MDCMoRR0)
MDC04corvalueswithTotal_0 <- data.frame(diag(as.matrix(test04_0)))

test05_0 <- cor(MDC05RR0, MDCMoRR0)
MDC05corvalueswithTotal_0 <- data.frame(diag(as.matrix(test05_0)))

testMort_0 <- cor(MDCMoRR0, MDCMoRR0)
MDCMortcorvalueswithTotal_0 <- data.frame(diag(as.matrix(testMort)))

corsum_0 <- cbind(MDC01corvalueswithTotal_0, MDC04corvalueswithTotal_0,MDC05corvalueswithTotal_0)


# Plot :)
plot(corsum$diag.as.matrix.test04..,col='blue',type='l',main="ROA Cummulative Lag Correlations with Total",ylab="Correlation Value",xlab="Lag (Days)",ylim = c(-1,1))
lines(corsum$diag.as.matrix.test05..,col='red')
lines(corsum$diag.as.matrix.test01..,col='green')
abline(h=0)
abline(h=0.433,lty=2)
abline(h=(-0.433),lty=2)
legend(3, -0.5, legend=c("MDC05", "MDC04","MDC01"),
       col=c("red", "blue","green"), lty=1, cex=0.8)

