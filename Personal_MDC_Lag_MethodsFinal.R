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

VJI_Final_Weather <- read_excel("~/Desktop/VJI.Final.Weather.xlsx")
VJI <- VJI_Final_Weather[1:5844,]

VJI %>% mutate_if(is.character, as.numeric)
VJI$HotDaysExtreme <- as.numeric(VJI$HotDaysExtreme)
VJI$date <- as.Date(VJI$Date)

VJI$Trend <- seq(1,5844,1)
for(i in 1:ncol(VJI)){
  assign(names(VJI)[i], VJI[[i]])
}

ATC1pm <- as.numeric(VJI$`AT(C)1pm`)
MaxTF <- as.numeric(VJI$MaxTF)
MaxTC <- as.numeric(VJI$`MaxT(C)`)
PM2.5REVISED <- as.numeric(VJI$PM2.5REVISED)
OzoneREVISED <- as.numeric(VJI$OzoneREVISED)
Mort <- as.numeric(VJI$Mort)

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

###DLNM for MDC.05
modelMDC.05=glm(MDC.05~cb1.MaxTC+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.05)
#plot(modelMDC.05)
#dispersiontest(modelMDC.05)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.05=crosspred(cb1.MaxTC,modelMDC.05,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

###DLNM for MDC.01
modelMDC.01=glm(MDC.01~cb1.MaxTC+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.01)
#plot(modelMDC.01)
#dispersiontest(modelMDC.01)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.01=crosspred(cb1.MaxTC,modelMDC.01,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

###DLNM for Mort
modelMort=glm(Mort~cb1.MaxTC+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMort)
#plot(modelMort)
#dispersiontest(modelMort)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.MaxTC.MDC.Mort=crosspred(cb1.MaxTC,modelMort,by=1,cen=(mean(MaxTC,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=30,lphi=30,main="VJI Mort Mortality")

# Correlation at Hot Days

MDC01RR <- data.frame(pred1.MaxTC.MDC.01$matRRfit)
MDC04RR <- data.frame(pred1.MaxTC.MDC.04$matRRfit)
MDC05RR <- data.frame(pred1.MaxTC.MDC.05$matRRfit)
MDCMoRR <- data.frame(pred1.MaxTC.MDC.Mort$matRRfit)

MDC01RR32 <- t(MDC01RR['32', ])
MDC04RR32 <- t(MDC04RR['32', ])
MDC05RR32 <- t(MDC05RR['32', ])
MDCMoRR32 <- t(MDCMoRR['32', ])

test01_32 <- cor(MDC01RR32, MDCMoRR32)
MDC01corvalueswithTotal_32 <- data.frame(diag(as.matrix(test01_32)))

test04_32 <- cor(MDC04RR32, MDCMoRR32)
MDC04corvalueswithTotal_32 <- data.frame(diag(as.matrix(test04_32)))

test05_32 <- cor(MDC05RR32, MDCMoRR32)
MDC05corvalueswithTotal_32 <- data.frame(diag(as.matrix(test05_32)))

# testMort_32 <- cor(MDCMoRR32, MDCMoRR32)
# MDCMortcorvalueswithTotal_32 <- data.frame(diag(as.matrix(testMort)))

corsum_32 <- cbind(MDC01corvalueswithTotal_32, MDC04corvalueswithTotal_32,MDC05corvalueswithTotal_32)

# Correlations at Cold Days

MDC01RR0 <- t(MDC01RR['-7', ])
MDC04RR0 <- t(MDC04RR['-7', ])
MDC05RR0 <- t(MDC05RR['-7', ])
MDCMoRR0 <- t(MDCMoRR['-7', ])

test01_6 <- cor(MDC01RR0, MDCMoRR0)
MDC01corvalueswithTotal_6 <- data.frame(diag(as.matrix(test01_6)))

test04_6 <- cor(MDC04RR0, MDCMoRR0)
MDC04corvalueswithTotal_6 <- data.frame(diag(as.matrix(test04_6)))

test05_6 <- cor(MDC05RR0, MDCMoRR0)
MDC05corvalueswithTotal_6 <- data.frame(diag(as.matrix(test05_6)))

# testMort_6 <- cor(MDCMoRR0, MDCMoRR0)
# MDCMortcorvalueswithTotal_6 <- data.frame(diag(as.matrix(testMort)))

corsum_6 <- cbind(MDC01corvalueswithTotal_6, MDC04corvalueswithTotal_6,MDC05corvalueswithTotal_6)


# Plots :)

#CHECK SLICES TOO

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

plot(pred1.MaxTC.MDC.01,ptype="slices",var=c(32),col='black',xlab='lag (days)',ylab="Relative Risk",main="VJI MDC.01: Severe Hot Day",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.04,ptype="slices",var=c(32),col='black',xlab='lag (days)',ylab="Relative Risk",main="VJI MDC.04: Severe Hot Day",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.05,ptype="slices",var=c(32),col='black',xlab='lag (days)',ylab="Relative Risk",main="VJI MDC.05: Severe Hot Day",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.Mort,ptype="slices",var=c(32),col='black',xlab='lag (days)',ylab="Relative Risk",main="VJI MDC.Mort: Severe Hot Day",ylim=c(.8,1.2))

plot(pred1.MaxTC.MDC.01,ptype="slices",var=c(-7),col='black',xlab='lag (days)',ylab="Relative Risk",main="VJI MDC.01: Severe Cold Day",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.04,ptype="slices",var=c(-7),col='black',xlab='lag (days)',ylab="Relative Risk",main="VJI MDC.04: Severe Cold Day",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.05,ptype="slices",var=c(-7),col='black',xlab='lag (days)',ylab="Relative Risk",main="VJI MDC.05: Severe Cold Day",ylim=c(.8,1.2))
plot(pred1.MaxTC.MDC.Mort,ptype="slices",var=c(-7),col='black',xlab='lag (days)',ylab="Relative Risk",main="VJI MDC.Mort: Severe Cold Day",ylim=c(.8,1.2))

