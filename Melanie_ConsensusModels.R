# Consensus Models

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
MaxTC <- as.numeric(LYH$`MaxT(C)`)
dow <- as.factor(LYH$dow)
ColdWavesModerate <- as.factor(LYH$ColdWavesModerate)
HeatWavesModerate <- as.factor(LYH$HeatWavesModerate)

model1b=gam(Mort~s(Trend,k=16*3)+s(MaxTC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate)+as.factor(HeatWavesModerate),family="poisson")
summary(model1b)
#plot(model1b)
dispersiontest(model1b)

#Use quasipoisson


# DLNM component
library(splines)
library(dlnm)
library(lubridate)
#
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

Mort

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
plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="LYH Mort")


# Plot of lag effects at selected values ("var=??")
#plot(pred1.MaxTC,ptype="slices",var=c(50),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 50 C")
#plot(pred1.MaxTC,ptype="slices",var=c(25),col='black',xlab='lag (days)',ylab="Relative Risk",main="Lag RR at 25 C")

# Plot of "basis" variables
cbind(pred1.MaxTC$allRRfit,pred1.MaxTC$allRRlow,pred1.MaxTC$allRRhigh)
#plot(modelA1)

# Plot of "heat map"
plot(pred1.MaxTC,"contour",xlab="Temp",key.title=title("RR"),plot.title=title("Lynchburg (LYH)",xlab="Maximum Temperature (\u00B0C)",ylab="Lag (days)"))

#overall plot
plot(pred1.MaxTC,"overall",xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="Lynchburg (LYH)")


#Heat map with more levels
# noeff=1
# levels=pretty(pred1.MaxTC$matRRfit,100)
# col1 <- colorRampPalette(c("blue","white"))
# col2 <- colorRampPalette(c("white","red"))
# col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
# filled.contour(x=pred1.MaxTC$predvar,y=seq(0,21,1),z=pred1.MaxTC$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="Maximum Temperature (\u00B0C)" ,ylab="Lag (days)",main="Lynchburg (LYH)"))
# 
# dev.off()

