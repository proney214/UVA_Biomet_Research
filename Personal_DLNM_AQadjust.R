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

EZF_Final_Weather <- read_excel("~/Desktop/EZF.Final.Weather.xlsx")
EZF <- EZF_Final_Weather[1:5844,]

EZF %>% mutate_if(is.character, as.numeric)
EZF$HotDaysExtreme <- as.numeric(EZF$HotDaysExtreme)
EZF$date <- as.Date(EZF$Date)

EZF$Trend <- seq(1,5844,1)
for(i in 1:ncol(EZF)){
  assign(names(EZF)[i], EZF[[i]])
}

ATC1pm <- as.numeric(EZF$`AT(C)1pm`)
MaxTF <- as.numeric(EZF$MaxTF)
MaxTC <- as.numeric(EZF$`MaxT(C)`)
PM2.5REVISED <- as.numeric(EZF$PM2.5REVISED)
OzoneREVISED <- as.numeric(EZF$OzoneREVISED)
Mort <- as.numeric(EZF$Mort)

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

modelMDC.04=glm(MDC.04~cb1.MaxTC+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.04)
#plot(modelMDC.04)
#dispersiontest(modelMDC.04)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.04=crosspred(cb1.MaxTC,modelMDC.04,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="EZF MDC.04 Mortality")

# Plot of "heat map"
noeff=1
levels=pretty(pred1.MaxTC.MDC.04$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC.MDC.04$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.04$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="EZF MDC.04: MaxTC"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("EZF MDC.04 Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

plot(pred1.MaxTC.MDC.04,ptype="slices",var=c(-5),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF MDC.04: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.1))
plot(pred1.MaxTC.MDC.04,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF MDC.04: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.1))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(0,10,25,40),lag=c(0,3,14,21),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.MaxTC.MDC.04,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="EZF MDC.04 Overall Cumulative Association")


###DLNM for MDC.05
modelMDC.05=glm(MDC.05~cb1.MaxTC+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.05)
#plot(modelMDC.05)
#dispersiontest(modelMDC.05)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.05=crosspred(cb1.MaxTC,modelMDC.05,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="EZF MDC.05 Mortality")

# Plot of "heat map"

noeff=1
levels=pretty(pred1.MaxTC.MDC.05$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC.MDC.05$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.05$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="EZF MDC.05: MaxTC"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("EZF MDC.05 Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.MaxTC,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF: RR vs. Lag (days) at 10°C (MaxTC)")
#plot(pred1.MaxTC,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF: RR vs. Lag (days) at 30°C (MaxTC)")

plot(pred1.MaxTC.MDC.05,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF MDC.05: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.1))
plot(pred1.MaxTC.MDC.05,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF MDC.05: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.1))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.MaxTC.MDC.05,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="EZF MDC.05 Overall Cumulative Association")

###DLNM for MDC.01
modelMDC.01=glm(MDC.01~cb1.MaxTC+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.01)
#plot(modelMDC.01)
#dispersiontest(modelMDC.01)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.01=crosspred(cb1.MaxTC,modelMDC.01,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="EZF MDC.01 Mortality")

# Plot of "heat map"

noeff=1
levels=pretty(pred1.MaxTC.MDC.01$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC.MDC.01$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.01$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="EZF MDC.01: MaxTC"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("EZF MDC.01 Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.MaxTC,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF: RR vs. Lag (days) at 10°C (MaxTC)")
#plot(pred1.MaxTC,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF: RR vs. Lag (days) at 30°C (MaxTC)")

plot(pred1.MaxTC.MDC.01,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF MDC.01: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.01,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF MDC.01: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.2))


# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.MaxTC.MDC.01,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="EZF MDC.01 Overall Cumulative Association")


###DLNM for Mort
modelMort=glm(Mort~cb1.MaxTC+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMort)
#plot(modelMort)
#dispersiontest(modelMort)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.Mort=crosspred(cb1.MaxTC,modelMort,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="EZF Mort Mortality")

# Plot of "heat map"

noeff=1
levels=pretty(pred1.MaxTC.MDC.Mort$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC.MDC.Mort$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.Mort$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="EZF Total Mortality"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("EZF Mort Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.MaxTC,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF: RR vs. Lag (days) at 10°C (MaxTC)")
#plot(pred1.MaxTC,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF: RR vs. Lag (days) at 30°C (MaxTC)")

plot(pred1.MaxTC.MDC.Mort,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF Mortality Total: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.Mort,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="EZF Mortality Total: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.2))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.MaxTC.MDC.Mort,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="EZF Total Mortality Overall Cumulative Association")


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
plot(corsum$diag.as.matrix.test04..,col='blue',type='l',main="EZF Cummulative Lag Correlations with Total",ylab="Correlation Value",xlab="Lag (Days)",ylim = c(-1,1))
lines(corsum$diag.as.matrix.test05..,col='red')
lines(corsum$diag.as.matrix.test01..,col='green')
abline(h=0)
abline(h=0.433,lty=2)
abline(h=(-0.433),lty=2)
legend(3, -0.5, legend=c("MDC05", "MDC04","MDC01"),
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

plot(corsum_40$diag.as.matrix.test04_40..,col='blue',type='l',main="EZF Cummulative Lag Correlations with Total",ylab="Correlation Value",xlab="Lag (Days)")
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
plot(corsum$diag.as.matrix.test04..,col='blue',type='l',main="EZF Cummulative Lag Correlations with Total",ylab="Correlation Value",xlab="Lag (Days)",ylim = c(-1,1))
lines(corsum$diag.as.matrix.test05..,col='red')
lines(corsum$diag.as.matrix.test01..,col='green')
abline(h=0)
abline(h=0.433,lty=2)
abline(h=(-0.433),lty=2)
legend(3, -0.5, legend=c("MDC05", "MDC04","MDC01"),
       col=c("red", "blue","green"), lty=1, cex=0.8)


###### Create DLNMs for the MDCs with Just PM2.5
library(splines)
library(dlnm)
library(lubridate)
library(readxl)
library(mgcv)
library(ggplot2)
library(dplyr)
library(AER)
library(qcc)

CHO_Final_Weather <- read_excel("~/Desktop/CHO.Final.Weather.xlsx")
CHO <- CHO_Final_Weather[1:5844,]

CHO %>% mutate_if(is.character, as.numeric)
CHO$HotDaysExtreme <- as.numeric(CHO$HotDaysExtreme)
CHO$date <- as.Date(CHO$Date)

CHO$Trend <- seq(1,5844,1)
for(i in 1:ncol(CHO)){
  assign(names(CHO)[i], CHO[[i]])
}

ATC1pm <- as.numeric(CHO$`AT(C)1pm`)
MaxTF <- as.numeric(CHO$MaxTF)
MaxTC <- as.numeric(CHO$`MaxT(C)`)
PM2.5REVISED <- as.numeric(CHO$PM2.5REVISED)
OzoneREVISED <- as.numeric(CHO$OzoneREVISED)
Mort <- as.numeric(CHO$Mort)

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

modelMDC.04=glm(MDC.04~cb1.MaxTC+ns(PM2.5REVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.04)
#plot(modelMDC.04)
#dispersiontest(modelMDC.04)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.04=crosspred(cb1.MaxTC,modelMDC.04,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="CHO MDC.04 Mortality")

# Plot of "heat map"
noeff=1
levels=pretty(pred1.MaxTC.MDC.04$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC.MDC.04$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.04$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="CHO MDC.04: MaxTC"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("CHO MDC.04 Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

plot(pred1.MaxTC.MDC.04,ptype="slices",var=c(-5),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO MDC.04: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.1))
plot(pred1.MaxTC.MDC.04,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO MDC.04: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.1))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(0,10,25,40),lag=c(0,3,14,21),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.MaxTC.MDC.04,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="CHO MDC.04 Overall Cumulative Association")


###DLNM for MDC.05
modelMDC.05=glm(MDC.05~cb1.MaxTC+ns(PM2.5REVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.05)
#plot(modelMDC.05)
#dispersiontest(modelMDC.05)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.05=crosspred(cb1.MaxTC,modelMDC.05,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="CHO MDC.05 Mortality")

# Plot of "heat map"

noeff=1
levels=pretty(pred1.MaxTC.MDC.05$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC.MDC.05$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.05$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="CHO MDC.05: MaxTC"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("CHO MDC.05 Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.MaxTC,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO: RR vs. Lag (days) at 10°C (MaxTC)")
#plot(pred1.MaxTC,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO: RR vs. Lag (days) at 30°C (MaxTC)")

plot(pred1.MaxTC.MDC.05,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO MDC.05: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.1))
plot(pred1.MaxTC.MDC.05,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO MDC.05: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.1))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.MaxTC.MDC.05,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="CHO MDC.05 Overall Cumulative Association")

###DLNM for MDC.01
modelMDC.01=glm(MDC.01~cb1.MaxTC+ns(PM2.5REVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.01)
#plot(modelMDC.01)
#dispersiontest(modelMDC.01)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.01=crosspred(cb1.MaxTC,modelMDC.01,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="CHO MDC.01 Mortality")

# Plot of "heat map"

noeff=1
levels=pretty(pred1.MaxTC.MDC.01$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC.MDC.01$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.01$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="CHO MDC.01: MaxTC"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("CHO MDC.01 Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.MaxTC,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO: RR vs. Lag (days) at 10°C (MaxTC)")
#plot(pred1.MaxTC,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO: RR vs. Lag (days) at 30°C (MaxTC)")

plot(pred1.MaxTC.MDC.01,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO MDC.01: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.01,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO MDC.01: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.2))


# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.MaxTC.MDC.01,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="CHO MDC.01 Overall Cumulative Association")


###DLNM for Mort
modelMort=glm(Mort~cb1.MaxTC+ns(PM2.5REVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMort)
#plot(modelMort)
#dispersiontest(modelMort)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.Mort=crosspred(cb1.MaxTC,modelMort,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="CHO Mort Mortality")

# Plot of "heat map"

noeff=1
levels=pretty(pred1.MaxTC.MDC.Mort$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.MaxTC.MDC.Mort$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.Mort$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="CHO Total Mortality"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("CHO Mort Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.MaxTC,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO: RR vs. Lag (days) at 10°C (MaxTC)")
#plot(pred1.MaxTC,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO: RR vs. Lag (days) at 30°C (MaxTC)")

plot(pred1.MaxTC.MDC.Mort,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO Mortality Total: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.Mort,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO Mortality Total: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.2))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.MaxTC.MDC.Mort,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="CHO Total Mortality Overall Cumulative Association")


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
plot(corsum$diag.as.matrix.test04..,col='blue',type='l',main="CHO Cummulative Lag Correlations with Total",ylab="Correlation Value",xlab="Lag (Days)",ylim = c(-1,1))
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

plot(corsum_40$diag.as.matrix.test04_40..,col='blue',type='l',main="CHO Cummulative Lag Correlations with Total",ylab="Correlation Value",xlab="Lag (Days)")
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

View(corsum_0)
View(corsum_35)

# Plot :)
plot(corsum$diag.as.matrix.test04..,col='blue',type='l',main="CHO Cummulative Lag Correlations with Total",ylab="Correlation Value",xlab="Lag (Days)",ylim = c(-1,1))
lines(corsum$diag.as.matrix.test05..,col='red')
lines(corsum$diag.as.matrix.test01..,col='green')
abline(h=0)
abline(h=0.433,lty=2)
abline(h=(-0.433),lty=2)
legend(16, -0.5, legend=c("MDC05", "MDC04","MDC01"),
       col=c("red", "blue","green"), lty=1, cex=0.8)

###### Create DLNMs for the MDCs with NO AQ ehhhhh
library(splines)
library(dlnm)
library(lubridate)
library(readxl)
library(mgcv)
library(ggplot2)
library(dplyr)
library(AER)
library(qcc)

RIC_Final_Weather <- read_excel("~/Desktop/RIC.Final.Weather.xlsx")
RIC <- RIC_Final_Weather[1:5844,]

RIC %>% mutate_if(is.character, as.numeric)
RIC$HotDaysExtreme <- as.numeric(RIC$HotDaysExtreme)
RIC$date <- as.Date(RIC$Date)

RIC$Trend <- seq(1,5844,1)
for(i in 1:ncol(RIC)){
  assign(names(RIC)[i], RIC[[i]])
}

ATC1pm <- as.numeric(RIC$`AT(C)1pm`)
MaxTF <- as.numeric(RIC$MaxTF)
MaxTC <- as.numeric(RIC$`MaxT(C)`)
PM2.5REVISED <- as.numeric(RIC$PM2.5REVISED)
OzoneREVISED <- as.numeric(RIC$OzoneREVISED)
Mort <- as.numeric(RIC$Mort)

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

modelMDC.04=glm(MDC.04~cb1.MaxTC+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.04)
#plot(modelMDC.04)
#dispersiontest(modelMDC.04)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.04=crosspred(cb1.MaxTC,modelMDC.04,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="RIC MDC.04 Mortality")

# Plot of "heat map"
# noeff=1
# levels=pretty(pred1.MaxTC.MDC.04$matRRfit,100)
# col1 <- colorRampPalette(c("blue","white"))
# col2 <- colorRampPalette(c("white","red"))
# col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
# filled.contour(x=pred1.MaxTC.MDC.04$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.04$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="RIC MDC.04: MaxTC"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("RIC MDC.04 Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.MaxTC.MDC.04,ptype="slices",var=c(-5),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.04: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.1))
#plot(pred1.MaxTC.MDC.04,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.04: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.1))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(0,10,25,40),lag=c(0,3,14,21),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

#plot(pred1.MaxTC.MDC.04,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="RIC MDC.04 Overall Cumulative Association")


###DLNM for MDC.05
modelMDC.05=glm(MDC.05~cb1.MaxTC+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.05)
#plot(modelMDC.05)
#dispersiontest(modelMDC.05)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.05=crosspred(cb1.MaxTC,modelMDC.05,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="RIC MDC.05 Mortality")

# Plot of "heat map"

# noeff=1
# levels=pretty(pred1.MaxTC.MDC.05$matRRfit,100)
# col1 <- colorRampPalette(c("blue","white"))
# col2 <- colorRampPalette(c("white","red"))
# col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
# filled.contour(x=pred1.MaxTC.MDC.05$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.05$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="RIC MDC.05: MaxTC"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("RIC MDC.05 Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.MaxTC,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC: RR vs. Lag (days) at 10°C (MaxTC)")
#plot(pred1.MaxTC,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC: RR vs. Lag (days) at 30°C (MaxTC)")

# plot(pred1.MaxTC.MDC.05,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.05: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.1))
# plot(pred1.MaxTC.MDC.05,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.05: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.1))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

#plot(pred1.MaxTC.MDC.05,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="RIC MDC.05 Overall Cumulative Association")

###DLNM for MDC.01
modelMDC.01=glm(MDC.01~cb1.MaxTC+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.01)
#plot(modelMDC.01)
#dispersiontest(modelMDC.01)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.01=crosspred(cb1.MaxTC,modelMDC.01,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="RIC MDC.01 Mortality")

# Plot of "heat map"

# noeff=1
# levels=pretty(pred1.MaxTC.MDC.01$matRRfit,100)
# col1 <- colorRampPalette(c("blue","white"))
# col2 <- colorRampPalette(c("white","red"))
# col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
# filled.contour(x=pred1.MaxTC.MDC.01$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.01$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="RIC MDC.01: MaxTC"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("RIC MDC.01 Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.MaxTC,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC: RR vs. Lag (days) at 10°C (MaxTC)")
#plot(pred1.MaxTC,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC: RR vs. Lag (days) at 30°C (MaxTC)")

# plot(pred1.MaxTC.MDC.01,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.01: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.2))
# plot(pred1.MaxTC.MDC.01,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.01: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.2))


# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

#plot(pred1.MaxTC.MDC.01,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="RIC MDC.01 Overall Cumulative Association")


###DLNM for Mort
modelMort=glm(Mort~cb1.MaxTC+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMort)
#plot(modelMort)
#dispersiontest(modelMort)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.Mort=crosspred(cb1.MaxTC,modelMort,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="RIC Mort Mortality")

# Plot of "heat map"

# noeff=1
# levels=pretty(pred1.MaxTC.MDC.Mort$matRRfit,100)
# col1 <- colorRampPalette(c("blue","white"))
# col2 <- colorRampPalette(c("white","red"))
# col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
# filled.contour(x=pred1.MaxTC.MDC.Mort$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.Mort$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="RIC Total Mortality"))

#plot(pred1.MaxTC,"contour",key.title=title("RR"),plot.title=title("RIC Mort Mortality",xlab="MaxTC",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.MaxTC,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC: RR vs. Lag (days) at 10°C (MaxTC)")
#plot(pred1.MaxTC,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC: RR vs. Lag (days) at 30°C (MaxTC)")

# plot(pred1.MaxTC.MDC.Mort,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC Mortality Total: RR vs. Lag (days) at 0°C (MaxTC)",ylim=c(.8,1.2))
# plot(pred1.MaxTC.MDC.Mort,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC Mortality Total: RR vs. Lag (days) at 40°C (MaxTC)",ylim=c(.8,1.2))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.MaxTC,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

# plot(pred1.MaxTC.MDC.Mort,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="RIC Total Mortality Overall Cumulative Association")
# plot(pred1.MaxTC.MDC.Mort,"overall")

# plot(pred1.MaxTC.MDC.05$matRRfit)

test01 <- cor(pred1.MaxTC.MDC.01$matRRfit, pred1.MaxTC.MDC.Mort$matRRfit)
MDC01corvalueswithTotal <- data.frame(diag(as.matrix(test01)))

test04 <- cor(pred1.MaxTC.MDC.04$matRRfit, pred1.MaxTC.MDC.Mort$matRRfit)
MDC04corvalueswithTotal <- data.frame(diag(as.matrix(test04)))

test05 <- cor(pred1.MaxTC.MDC.05$matRRfit, pred1.MaxTC.MDC.Mort$matRRfit)
test05 <- cor(pred1.MaxTC.MDC.05$matRRfit[], pred1.MaxTC.MDC.Mort$matRRfit)

test <- data.frame(pred1.MaxTC.MDC.05$matRRfit)
test2 <- data.frame(pred1.MaxTC.MDC.Mort$matRRfit)

testt <- t(test)
test2t <- t(test2)

testtcor <- cor(testt,test2t)
testtcordiag <- data.frame(diag(as.matrix(testtcor)))

plot(testtcordiag$diag.as.matrix.testtcor..,type='l')

MDC05corvalueswithTotal <- data.frame(diag(as.matrix(test05)))

testMort <- cor(pred1.MaxTC.MDC.Mort$matRRfit, pred1.MaxTC.MDC.Mort$matRRfit)
MDCMortcorvalueswithTotal <- data.frame(diag(as.matrix(testMort)))

corsum <- cbind(MDC01corvalueswithTotal, MDC04corvalueswithTotal,MDC05corvalueswithTotal)

plot(corsum$diag.as.matrix.test04..,col='blue',type='l',main="RIC Cummulative Lag Correlations with Total",ylab="Correlation Value",xlab="Lag (Days)",ylim = c(-1,1))
lines(corsum$diag.as.matrix.test05..,col='red')
lines(corsum$diag.as.matrix.test01..,col='green')
abline(h=0)
abline(h=0.433,lty=2)
abline(h=(-0.433),lty=2)
legend(3, -0.5, legend=c("MDC05", "MDC04","MDC01"),
       col=c("red", "blue","green"), lty=1, cex=0.8)

# Plots :)

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(pred1.MaxTC.MDC.01,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="RIC MDC.01")
abline(v=-5.5,col='blue',lty=2)
abline(v=35,col='red',lty=2)
plot(pred1.MaxTC.MDC.04,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="RIC MDC.04")
abline(v=-5.5,col='blue',lty=2)
abline(v=35,col='red',lty=2)
plot(pred1.MaxTC.MDC.05,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="RIC MDC.05")
abline(v=-5.5,col='blue',lty=2)
abline(v=35,col='red',lty=2)
plot(pred1.MaxTC.MDC.Mort,"overall",xlab="Maximum Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="RIC Total Mortality")
abline(v=-5.5,col='blue',lty=2)
abline(v=35,col='red',lty=2)

dev.off()


layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

plot(pred1.MaxTC.MDC.01,ptype="slices",var=c(33),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.01: Severe Hot Day",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.04,ptype="slices",var=c(33),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.04: Severe Hot Day",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.05,ptype="slices",var=c(33),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.05: Severe Hot Day",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.Mort,ptype="slices",var=c(33),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.Mort: Severe Hot Day",ylim=c(.8,1.2))

plot(pred1.MaxTC.MDC.01,ptype="slices",var=c(1),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.01: Severe Cold Day",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.04,ptype="slices",var=c(1),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.04: Severe Cold Day",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.05,ptype="slices",var=c(1),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.05: Severe Cold Day",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.Mort,ptype="slices",var=c(1),col='black',xlab='lag (days)',ylab="Relative Risk",main="RIC MDC.Mort: Severe Cold Day",ylim=c(.8,1.2))

# Correlation at Hot Days

MDC01RR <- data.frame(pred1.MaxTC.MDC.01$matRRfit)
MDC04RR <- data.frame(pred1.MaxTC.MDC.04$matRRfit)
MDC05RR <- data.frame(pred1.MaxTC.MDC.05$matRRfit)
MDCMoRR <- data.frame(pred1.MaxTC.MDC.Mort$matRRfit)

MDC01RR35 <- t(MDC01RR['33', ])
MDC04RR35 <- t(MDC04RR['33', ])
MDC05RR35 <- t(MDC05RR['33', ])
MDCMoRR35 <- t(MDCMoRR['33', ])

test01_35 <- cor(MDC01RR35, MDCMoRR35)
MDC01corvalueswithTotal_35 <- data.frame(diag(as.matrix(test01_35)))

test04_35 <- cor(MDC04RR35, MDCMoRR35)
MDC04corvalueswithTotal_35 <- data.frame(diag(as.matrix(test04_35)))

test05_35 <- cor(MDC05RR35, MDCMoRR35)
MDC05corvalueswithTotal_35 <- data.frame(diag(as.matrix(test05_35)))

testMort_35 <- cor(MDCMoRR35, MDCMoRR35)
MDCMortcorvalueswithTotal_35 <- data.frame(diag(as.matrix(testMort)))

corsum_35 <- cbind(MDC01corvalueswithTotal_35, MDC04corvalueswithTotal_35,MDC05corvalueswithTotal_35)

# Correlations at Cold Days

MDC01RR0 <- t(MDC01RR['1', ])
MDC04RR0 <- t(MDC04RR['1', ])
MDC05RR0 <- t(MDC05RR['1', ])
MDCMoRR0 <- t(MDCMoRR['1', ])

test01_0 <- cor(MDC01RR0, MDCMoRR0)
MDC01corvalueswithTotal_0 <- data.frame(diag(as.matrix(test01_0)))

test04_0 <- cor(MDC04RR0, MDCMoRR0)
MDC04corvalueswithTotal_0 <- data.frame(diag(as.matrix(test04_0)))

test05_0 <- cor(MDC05RR0, MDCMoRR0)
MDC05corvalueswithTotal_0 <- data.frame(diag(as.matrix(test05_0)))

testMort_0 <- cor(MDCMoRR0, MDCMoRR0)
MDCMortcorvalueswithTotal_0 <- data.frame(diag(as.matrix(testMort)))

corsum_0 <- cbind(MDC01corvalueswithTotal_0, MDC04corvalueswithTotal_0,MDC05corvalueswithTotal_0)

# #Heat Maps
# noeff=1
# col1 <- colorRampPalette(c("blue","white"))
# col2 <- colorRampPalette(c("white","red"))
# 
# levels=pretty(pred1.MaxTC.MDC.01$matRRfit,100)
# col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
# filled.contour(x=pred1.MaxTC.MDC.01$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.01$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="RIC MDC.01"))
# 
# levels=pretty(pred1.MaxTC.MDC.04$matRRfit,100)
# col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
# filled.contour(x=pred1.MaxTC.MDC.04$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.04$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="RIC MDC.04"))
# 
# levels=pretty(pred1.MaxTC.MDC.05$matRRfit,100)
# col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
# filled.contour(x=pred1.MaxTC.MDC.05$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.05$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="RIC MDC.05"))
# 
# levels=pretty(pred1.MaxTC.MDC.Mort$matRRfit,100)
# col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
# filled.contour(x=pred1.MaxTC.MDC.Mort$predvar,y=seq(0,21,1),z=pred1.MaxTC.MDC.Mort$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="MaxTC" ,ylab="Lag (Days)",main="RIC Total Mortality"))
# 

