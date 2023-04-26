###BEGINNING MODEL OF DATA###
###PHF TEST###

library(readxl)
PHFmort=read_excel("PHF.weatherprep.test.xlsx",na="NA")
PHFmort=as.data.frame(PHFmort)
names(PHFmort)
head(PHFmort)
tail(PHFmort)
plot(PHFmort$Mort)

library(mgcv)
trend=seq(1,2192,1)
plot(PHFmort$'mortalityCount')
count=PHFmort$Mort
AT=PHFmort$`AT(C)-1am`
DOW=PHFmort$dow
Td=PHFmort$`Td(C)-1am`
T=PHFmort$`T(C)-1am`
model1=gam(count~s(trend,k=3*6))
head(T)
head(trend)
head(count)
names(model1)
plot(model1$'residuals')
summary(model1)
plot(model1,scale=0)

model1=gam(count~s(trend,k=4*6),family="quasipoisson")
summary(model1)
plot(model1)

p1=predict.gam(model1,type="response")
summary(p1)
plot(p1)

model1=gam(count~s(trend,k=5*6)+s(T,k=3)+as.factor(DOW)+as.factor(PHFmort$holidays),family="quasipoisson")
summary(model1)
plot(model1)

model1=gam(count~s(trend,k=7*5)+s(T,k=3)+s(Td,k=3)+as.factor(DOW),family="quasipoisson")
plot(model1)

newdf2=data.frame(trend=10,T=30,DOW=2,Td=15)
pf2=predict.gam(model1,newdf1,type="terms",se=TRUE)
newdf3=data.frame(trend=10,T=seq(0,35,1),DOW=2,Td=15)
pf3=predict.gam(model1,newdf3,type="terms",se=TRUE)
plot((pf3$fit)[,1])
pf3=predict.gam(model1,newdf3,type="response",se=TRUE)
plot(exp(pf3$fit)[,1])
#
newdf5=data.frame(trend=1000,T=seq(-10,39,1),DOW=3)
model5=gam(count~s(trend,k=7*5)+s(T,k=3)+as.factor(DOW),family="quasipoisson")
p5=predict.gam(model5,newdf5,type="response",se=TRUE)
plot(p5$fit)
plot(p5$se.fit)
plot(exp(p5$fit[4]))
highci=exp(p5$fit[3]+1.96*p5$se.fit[3])
lowci=exp(p5$fit[3]-1.96*p5$se.fit[3])
lines(highci)
lines(lowci)
head(p5$fit)
summary(p5$fit)
#
newdf5=data.frame(trend=1000,T=seq(-10,39,1),DOW=3)
model5=gam(count~s(trend,k=7*5)+s(T,k=3)+as.factor(DOW),family="quasipoisson")
p5=predict.gam(model5,newdf5,type="terms",se=TRUE)
plot(p5$fit)
plot(p5$se.fit)
plot(exp(p5$fit[,3]))
highci=exp(p5$fit[,3]+1.96*p5$se.fit[,3])
lowci=exp(p5$fit[,3]-1.96*p5$se.fit[,3])
lines(highci)
lines(lowci)
head(p5$fit)
summary(p5$fit)
#
library(dlnm)
library(splines)
library(lubridate)
datevar=ymd(PHFmort$DateTime)
lagframe=data.frame(count=count,trend=trend,T=T,DOW=DOW,datevar=datevar)
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=00)
plot(time)
basis.temp=crossbasis(lagframe$T,vartype="ns",vardf=3,cen=T,maxlag=10,lagtype="ns",lagdf=3,cenvalue=15)
plot(basis.temp)
varknots=equalknots(lagframe$T,fun="bs",df=4,degree=1)
varknots
lagknots=logknots(10,2)
lagknots
cb1.temp=crossbasis(lagframe$T,lag=10,argvar=list(fun="bs",knots=varknots,cen=20),arglag=list(knots=lagknots))
summary(cb1.temp)

#Convert GAM to GLM format
model5=glm(count~cb1.temp+ns(trend,7*5)+as.factor(DOW),family=quasipoisson(),lagframe)
summary(model5)
pred1.temp=crosspred(cb1.temp,model5,by=1)
plot(pred1.temp,"overall",xlab="Temp (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Overall cumulative association")
plot(pred1.temp,"overall",xlab="Temp (°C)",ylab="RR",zlab="RR",theta=200,phi=40,lphi=20,main="Exposure-lag response surface",cex.axis=0.5)
plot(pred1.temp,ptype="slices",var=c(30),col='black',xlab="lag (days)",ylab="Relative Risk",main="Lag response at 30°C")
plot(pred1.temp,ptype="slices",var=c(10),col='black',xlab="lag (days)",ylab="Relative Risk",main="Lag response at 10°C")
cbind(pred1.temp$allRRfit,pred1.temp$allRRlow,pred1.temp$allRRhigh)
plot(model5)
plot(pred1.temp,xlab="Temp (°C)",ylab="lag",lwd=3,main="Overall cumulative assocation")
plot(pred1.temp,ptype="contour",key.title=title("RR"),plot.title=title("Overall Effect",xlab="Temperature (°C)",ylab="Lag (days)"))
