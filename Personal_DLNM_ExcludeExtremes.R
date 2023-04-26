###### Create DLNMs for the MDCs REMOVING EXTREME TEMPS
library(readxl)
library(dplyr)
library(zoo)
library(season)
library(splines)
library(dlnm)

PHF_Final_Weather <- read_excel("~/Desktop/PHF.Final.Weather.xlsx")
PHF <- PHF_Final_Weather

PHF %>% mutate_if(is.character, as.numeric)

PHF$Trend <- seq(1,5844,1)

PHF <- PHF %>%
  filter(PHF$`AT(C)1pm` > 0 & PHF$`AT(C)1pm` < 40)

PHF$ATC1pm <- as.numeric(PHF$`AT(C)1pm`)


for(i in 1:ncol(PHF)){
  assign(names(PHF)[i], PHF[[i]])
}

lagframe=data.frame(MaxTF,ATC1pm,TF7am,Mort,MDC.01,MDC.04,MDC.05,OzoneREVISED,PM2.5REVISED,Trend)

varknotsATC1pm=equalknots(ATC1pm, nk=NULL, fun="bs",df=5,degree=3)
varknotsATC1pm
lagknotsATC1pm=logknots(21,3)
lagknotsATC1pm

# "Crossbasis" is the key command of the DLNM.  It describes the two-dimensional relationship between your basis variable and the lag.  The first item is the variable you are using as your basis (in this case, maximum temperature),
# "lag" is the maximum lag you will examine (21 days)
# argvar and arglag are passing data to a spline-fitting routine ("onebasis") that will generate the matrices for predictor and lags.

cb1.ATC1pm=crossbasis(lagframe$ATC1pm,lag=21,argvar=list(fun="bs",knots=varknotsATC1pm),arglag=list(fun="ns",knots=lagknotsATC1pm))
summary(cb1.ATC1pm)

# Now run best GAM model as GLM (note the syntax between GAMs and GLMs is slightly different, but you have one (or more) main variable(s) (your crossbasis), splines (ns, this is like controlling for a variable in a GAM, where the degrees of freedom are specified for each), and various "factor" variables (categorical or binary).

modelMDC.04=glm(MDC.04~cb1.ATC1pm+ns(OzoneREVISED, 3)+ns(PM2.5REVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.04)
#plot(modelMDC.04)
#dispersiontest(modelMDC.04)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.ATC1pm=crosspred(cb1.ATC1pm,modelMDC.04,by=1,cen=(mean(ATC1pm,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

plot(pred1.ATC1pm,xlab="ATC1pm",zlab="RR",theta=200,phi=30,lphi=30,main="PHF MDC.04 Mortality")

# Plot of "heat map"

plot(pred1.ATC1pm,"contour",key.title=title("RR"),plot.title=title("PHF MDC.04 Mortality",xlab="ATC1pm",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

plot(pred1.ATC1pm,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="PHF: RR vs. Lag (days) at 10°C (ATC1pm)")
plot(pred1.ATC1pm,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="PHF: RR vs. Lag (days) at 30°C (ATC1pm)")

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
plot(pred1.ATC1pm,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.ATC1pm,"overall",xlab="Apparent Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="PHF MDC.04 Overall Cumulative Association")


###DLNM for MDC.05
modelMDC.05=glm(MDC.05~cb1.ATC1pm+ns(OzoneREVISED, 3)+ns(PM2.5REVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.05)
#plot(modelMDC.05)
#dispersiontest(modelMDC.05)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.ATC1pm=crosspred(cb1.ATC1pm,modelMDC.05,by=1,cen=(mean(ATC1pm,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

plot(pred1.ATC1pm,xlab="ATC1pm",zlab="RR",theta=200,phi=30,lphi=30,main="PHF MDC.05 Mortality")

# Plot of "heat map"

plot(pred1.ATC1pm,"contour",key.title=title("RR"),plot.title=title("PHF MDC.05 Mortality",xlab="ATC1pm",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

plot(pred1.ATC1pm,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="PHF: RR vs. Lag (days) at 10°C (ATC1pm)")
plot(pred1.ATC1pm,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="PHF: RR vs. Lag (days) at 30°C (ATC1pm)")

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
plot(pred1.ATC1pm,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.ATC1pm,"overall",xlab="Apparent Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="PHF MDC.05 Overall Cumulative Association")

###DLNM for MDC.01
modelMDC.01=glm(MDC.01~cb1.ATC1pm+ns(OzoneREVISED, 3)+ns(PM2.5REVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.01)
#plot(modelMDC.01)
#dispersiontest(modelMDC.01)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.ATC1pm=crosspred(cb1.ATC1pm,modelMDC.01,by=1,cen=(mean(ATC1pm,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

plot(pred1.ATC1pm,xlab="ATC1pm",zlab="RR",theta=200,phi=30,lphi=30,main="PHF MDC.01 Mortality")

# Plot of "heat map"

plot(pred1.ATC1pm,"contour",key.title=title("RR"),plot.title=title("PHF MDC.01 Mortality",xlab="ATC1pm",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

plot(pred1.ATC1pm,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="PHF: RR vs. Lag (days) at 10°C (ATC1pm)")
plot(pred1.ATC1pm,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="PHF: RR vs. Lag (days) at 30°C (ATC1pm)")

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
plot(pred1.ATC1pm,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.ATC1pm,"overall",xlab="Apparent Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="PHF MDC.01 Overall Cumulative Association")

#change gradation

lag <- data.frame(seq(1,21,1))
lag <- t(lag)
dim(lag)

pred <- data.frame(seq(1,39,1))
dim(pred)

dim(pred1.ATC1pm$matRRfit)

noeff=1
levels=pretty(pred1.ATC1pm$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.ATC1pm$predvar,y=seq(0,21,1),z=pred1.ATC1pm$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="ATC1pm" ,ylab="Lag (Days)",main="Respiratory: Minimum Temperature"))


plot(pred1.ATC1pm,"contour", key.title=title("RR"),plot.title=title("PHF MDC.01 Mortality",xlab="ATC1pm",ylab="Lag (days)"))


