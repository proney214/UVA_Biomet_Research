# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#
ATF1pm <- RIC$`AT(C)1pm`

# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(mort=mort, Trend=Trend, ATF1pm=ATF1pm,dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.ATF1pm=crossbasis(lagframe$ATF1pm,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)
basis.dow=crossbasis(lagframe$dow,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)
#basis.PM2.5=crossbasis(lagframe$PM2.5,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

# Define the position of knots for spline functions
varknotsATF1pm=equalknots(lagframe$ATF1pm,fun="bs",df=4,degree=2)
varknotsATF1pm
lagknotsATF1pm=logknots(21,2)
lagknotsATF1pm
varknotsdow=equalknots(lagframe$dow,fun="bs",df=4,degree=2)
varknotsdow
lagknotsdow=logknots(21,2)
lagknotsdow
#varknotsPM2.5=equalknots(lagframe$PM2.5,fun="bs",df=4,degree=2)
#varknotsPM2.5
#lagknotsPM2.5=logknots(21,2)
#lagknotsPM2.5

# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value
cb1.ATF1pm=crossbasis(lagframe$ATF1pm,lag=21,argvar=list(fun="bs",knots=varknotsATF1pm),arglag=list(knots=lagknotsATF1pm))
summary(cb1.ATF1pm)
cb1.dow=crossbasis(lagframe$dow,lag=21,argvar=list(fun="bs",knots=varknotsdow),arglag=list(knots=lagknotsdow))
summary(cb1.dow)
#cb1.PM2.5=crossbasis(lagframe$PM2.5,lag=21,argvar=list(fun="bs",knots=varknotsPM2.5),arglag=list(knots=lagknotsPM2.5))
#summary(cb1.PM2.5)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
modelA1=glm(mort~cb1.ATF1pm+cb1.dow+ns(Trend, 3*16),family=quasipoisson(),lagframe)
summary(modelA1)
plot(modelA1)
#Removed PM2.5

modelA2=glm(mort~cb1.ATF1pm+ns(dow, 3*16)+ns(Trend, 3*16),family=quasipoisson(),lagframe)
summary(modelA2)

# Generate predictions from DLNM and plot results
pred1.ATF1pm=crosspred(cb1.ATF1pm,modelA1,by=1)
#pred2.ATF1pm=crosspred(cb1.ATF1pm,modelA2,by=1)
#pred1.dow=crosspred(cb1.dow,modelA1,by=1)
#pred1.PM2.5=crosspred(cb1.PM2.5,modelA1,by=1)

# 3-D plot
plot(pred1.ATF1pm,xlab="ATF1pm",zlab="RR",theta=200,phi=40,lphi=30,main="RIC Mort")
plot(pred2.ATF1pm,xlab="ATF1pm",zlab="RR",theta=200,phi=40,lphi=30,main="RIC Mort Spline")
#plot(pred1.dow,xlab="dow",zlab="RR",theta=200,phi=40,lphi=30,main="RIC Mort")
#plot(pred1.PM2.5,xlab="PM2.5",zlab="RR",theta=200,phi=40,lphi=30,main="RIC Mort")

# Plot of lag effects at selected values ("var=??")
plot(pred1.ATF1pm,ptype = "slices",var=c(5),col='black',xlab="lag (days)",ylab="Relative Risk",main="Lag RR at 5 C")
plot(pred1.ATF1pm,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")
plot(pred1.ATF1pm,ptype="slices",var=c(20),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 20 HX")
plot(pred1.dow,ptype="slices",var=c(0.0),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 0")
plot(pred1.dow,ptype="slices",var=c(0.5),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 0.5")

# Plot of "basis" variables
cbind(pred1.ATF1pm$allRRfit,pred1.ATF1pm$allRRlow,pred1.ATF1pm$allRRhigh)
cbind(pred2.ATF1pm$allRRfit,pred2.ATF1pm$allRRlow,pred2.ATF1pm$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
plot(pred1.ATF1pm,"contour",xlab="ATF1pm",key.title=title("RR"),plot.title=title("RIC Total Mortalities",xlab="ATF1pm",ylab="Lag (days)"))
plot(pred2.ATF1pm,"contour",xlab="ATF1pm",key.title=title("RR"),plot.title=title("mort Admissions Spline",xlab="ATF1pm",ylab="Lag (days)"))


#plot(pred1.dow,"contour",xlab="dow",key.title=title("RR"),plot.title=title("mort Admissions",xlab="dow",ylab="Lag (days)"))
#plot(pred1.PM2.5,"contour",xlab="PM2.5",key.title=title("RR"),plot.title=title("mort Admissions",xlab="PM2.5",ylab="Lag (days)"))


#overall plot
plot(pred1.ATF1pm,"overall",xlab="ATF1pm",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Overall cumulative association")
plot(pred2.ATF1pm,"overall",xlab="MinT",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Overall cumulative association Spline")

#Save these plots
plot(pred1.ATF1pm,"contour",key.title=title("RR"),plot.title=title("RIC Total Mortalities",ylab="Lag (days)",xlab="ATF1pm (\u00B0C)"))
plot(pred1.ATF1pm,"overall",xlab="ATF1pm (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="RIC Total Mortalities")
