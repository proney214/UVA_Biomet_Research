#Load in libraries

library(splines)

library(dlnm)

library(lubridate)

library(readxl)

library(mgcv)

library(ggplot2)

library(dplyr)



#RIC

#Load in Data

RICF <- read_excel("~/Desktop/RIC.working.weatherprep.xlsx", sheet="Sheet1",na="NA")



for(i in 1:ncol(RICF)){
  
  assign(names(RICF)[i], RICF[[i]])
  
}



OzoneREVISED<-as.numeric(OzoneREVISED)

PM2.5REVISED<-as.numeric(PM2.5REVISED)

RICfactors <- select(RICF,ColdDaysExtreme,ColdDaysModerate,ColdDaysSevere,ColdWavesExtreme,ColdWavesModerate,ColdWavesSevere,HotDaysExtreme,HotDaysModerate,HotDaysSevere,HeatWavesExtreme,HeatWavesModerate,HeatWavesSevere,holidays,dow,WinterWeather)

RIC <- select(RICF,-Station,-Date,-Year,-Month,-Day,-`MaxT(C)`,-`MaxTDep(C)`,-`MinT(C)`,-`MinTDep(C)`,-`DTR(C)`,-`T(C)1am`,-`Td(C)1am`,-`Tw(C)1am`,-`AT(C)1am`,-`THI(C)1am`,-`Hx(C)1am`,-`WC(C)1am`,-`T(C)7am`,-`Td(C)7am`,-`Tw(C)7am`,-`AT(C)7am`,-`THI(C)7am`,-`Hx(C)7am`,-`WC(C)7am`,-`T(C)1pm`,-`Td(C)1pm`,-`Tw(C)1pm`,-`AT(C)1pm`,-`THI(C)1pm`,-`Hx(C)1pm`,-`WC(C)1pm`,-`T(C)7pm`,-`Td(C)7pm`,-`Tw(C)7pm`,-`AT(C)7pm`,-`THI(C)7pm`,-`Hx(C)7pm`,-`WC(C)7pm`,-`ColdDaysExtreme`,-`ColdDaysModerate`,-`ColdDaysSevere`,-`ColdWavesExtreme`,-`ColdWavesModerate`,-`ColdWavesSevere`,-`HotDaysExtreme`,-`HotDaysModerate`,-`HotDaysSevere`,-`HeatWavesExtreme`,-`HeatWavesModerate`,-`HeatWavesSevere`,-`holidays`,-`dow`,-`WinterWeather`)



#Convert Data to numeric

RIC %>% mutate_if(is.character, as.numeric)

head(RIC)



#Create Trend Term

Trend = seq(1,5844,1)



#DLNM component



#Create a date variable needed in model ("datevar" is a "lubridate" function)

datevar=make_date(Year,Month,Day)



#Create a data frame with lagged variables using all variables in "final" GAM

lagframe=data.frame(mort=mort, Trend=Trend, MaxTF=MaxTF, OzoneREVISED=OzoneREVISED, PM2.5REVISED=PM2.5REVISED, datevar=datevar)



#Generate basis matrix for predictors and lags

df.time=4

time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=00)

plot(time)

basis.MaxTF=crossbasis(lagframe$MaxTF,vartype="ns",vardf=3,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

basis.OzoneREVISED=crossbasis(lagframe$OzoneREVISED,vartype="ns",vardf=3,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

basis.PM2.5REVISED=crossbasis(lagframe$PM2.5REVISED,vartype="ns",vardf=3,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)



#Define the position of knots for spline functions

varknotsMaxTF=equalknots(lagframe$MaxTF,fun="bs",df=3,degree=2)

varknotsMaxTF

lagknotsMaxTF=logknots(21,2)

lagknotsMaxTF

varknotsOzoneREVISED=equalknots(lagframe$OzoneREVISED,fun="bs",df=3,degree=2)

varknotsOzoneREVISED

lagknotsOzoneREVISED=logknots(21,2)

lagknotsOzoneREVISED

varknotsPM2.5REVISED=equalknots(lagframe$PM2.5REVISED,fun="bs",df=3,degree=2)

varknotsPM2.5REVISED

lagknotsPM2.5REVISED=logknots(21,2)

lagknotsPM2.5REVISED



# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value

cb1.MaxTF=crossbasis(lagframe$MaxTF,lag=21,argvar=list(fun="bs",knots=varknotsMaxTF),arglag=list(knots=lagknotsMaxTF))

summary(cb1.MaxTF)



# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)

# This is the "best" model from GAMs

modelA1=glm(mort~cb1.MaxTF+ns(OzoneREVISED, 3)+ns(PM2.5REVISED, 3)+ns(Trend, 16*3)+as.factor(ColdWavesModerate)+as.factor(HeatWavesExtreme)+as.factor(holidays),family=quasipoisson(),lagframe)

summary(modelA1)



# Generate predictions from DLNM and plot results

pred1.MaxTF=crosspred(cb1.MaxTF,modelA1,by=1)



#OVERALL PLOT

plot(pred1.MaxTF,"overall",xlab="MaxTF",y="Relative Risk",lwd=3,ylim=c(0.7,1.3),xlim=c(0,150),main="RIC Black Comparison Model Overall Cumulative Association")

#Save file as RIC_Comparison_Plot



# Plot of "heat map"

plot(pred1.MaxTF,"contour",xlab="MaxTF",plot.title=title("RIC Black Comparison Model",xlab="MaxTF",ylab="Lag (days)")
     ,key.title=title("RR"))

plot(pred1.MaxTF,"contour",zlim = c(0.9,1.1))

#Save file as RIC_Comparison_Map

df <- data.frame(matrix(ncol = 3))
df[,1] <- MaxTF
df[,2]


library(plotly)
p <- plotly(data = pred1.MaxTF, x=~x,y=~y, z=~z, type = "contour", colorscale='Jet')

