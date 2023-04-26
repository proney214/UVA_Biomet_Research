library(readxl)
library(dplyr)
library(splines)
library(dlnm) # DLNM component
library(lubridate)

#State Special Data

#VA

VA <- read_excel("~/Desktop/VA.Final.Weather.xlsx",na="NA")
VA <- VA[1:5844, ]
#Convert Data to numeric
VA %>% mutate_if(is.character, as.numeric)
head(VA)
for(i in 1:ncol(VA)){
  assign(names(VA)[i], VA[[i]])
}
#Create Trend Term
Trend = seq(1,5844,1)
#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing VA$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

MortMortVA <- CHO_Final_Weather$Mort + EMV_Final_Weather$Mort + EZF_Final_Weather$Mort + IAD_Final_Weather$Mort + LYH_Final_Weather$Mort+OKV_Final_Weather$Mort+ORF_Final_Weather$Mort+PHF_Final_Weather$Mort+RIC_Final_Weather$Mort+ROA_Final_Weather$Mort+SHD_Final_Weather$Mort+VJI_Final_Weather$Mort
MortMDC01VA <- CHO_Final_Weather$MDC.01 + EMV_Final_Weather$MDC.01 + EZF_Final_Weather$MDC.01 + IAD_Final_Weather$MDC.01 + LYH_Final_Weather$MDC.01+OKV_Final_Weather$MDC.01+ORF_Final_Weather$MDC.01+PHF_Final_Weather$MDC.01+RIC_Final_Weather$MDC.01+ROA_Final_Weather$MDC.01+SHD_Final_Weather$MDC.01+VJI_Final_Weather$MDC.01
MortMDC04VA <- CHO_Final_Weather$MDC.04 + EMV_Final_Weather$MDC.04 + EZF_Final_Weather$MDC.04 + IAD_Final_Weather$MDC.04 + LYH_Final_Weather$MDC.04+OKV_Final_Weather$MDC.04+ORF_Final_Weather$MDC.04+PHF_Final_Weather$MDC.04+RIC_Final_Weather$MDC.04+ROA_Final_Weather$MDC.04+SHD_Final_Weather$MDC.04+VJI_Final_Weather$MDC.04
MortMDC05VA <- CHO_Final_Weather$MDC.05 + EMV_Final_Weather$MDC.05 + EZF_Final_Weather$MDC.05 + IAD_Final_Weather$MDC.05 + LYH_Final_Weather$MDC.05+OKV_Final_Weather$MDC.05+ORF_Final_Weather$MDC.05+PHF_Final_Weather$MDC.05+RIC_Final_Weather$MDC.05+ROA_Final_Weather$MDC.05+SHD_Final_Weather$MDC.05+VJI_Final_Weather$MDC.05
MortMDC19VA <- CHO_Final_Weather$MDC.19 + EMV_Final_Weather$MDC.19 + EZF_Final_Weather$MDC.19 + IAD_Final_Weather$MDC.19 + LYH_Final_Weather$MDC.19+OKV_Final_Weather$MDC.19+ORF_Final_Weather$MDC.19+PHF_Final_Weather$MDC.19+RIC_Final_Weather$MDC.19+ROA_Final_Weather$MDC.19+SHD_Final_Weather$MDC.19+VJI_Final_Weather$MDC.19
MortNonBillVA <- CHO_Final_Weather$Non.billable.ICD + EMV_Final_Weather$Non.billable.ICD + EZF_Final_Weather$Non.billable.ICD + IAD_Final_Weather$Non.billable.ICD + LYH_Final_Weather$Non.billable.ICD+OKV_Final_Weather$Non.billable.ICD+ORF_Final_Weather$Non.billable.ICD+PHF_Final_Weather$Non.billable.ICD+RIC_Final_Weather$Non.billable.ICD+ROA_Final_Weather$Non.billable.ICD+SHD_Final_Weather$Non.billable.ICD+VJI_Final_Weather$Non.billable.ICD

Mort <- MortMortVA
MDC01 <- MortMDC01VA
MDC04 <- MortMDC04VA
MDC05 <- MortMDC05VA
MDC19 <- MortMDC19VA
NonBillICD <- MortNonBillVA

VA$Mort <- MortMortVA
VA$MDC01 <- MortMDC01VA
VA$MDC04 <- MortMDC04VA
VA$MDC05 <- MortMDC05VA
VA$MDC19 <- MortMDC19VA
VA$NonBillICD <- MortNonBillVA

MaxTC <- as.numeric(VA$`MaxT(C)`)
dow <- as.factor(VA$dow)
ColdWavesModerate <- as.factor(VA$ColdWavesModerate)
HeatWavesModerate <- as.factor(VA$HeatWavesModerate)
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_Mort=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC,  dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_Mort$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_Mort=crossbasis(lagframe_Mort$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue = median(MaxTC,na.rm = T))

# Define the position of knots for spline functions
varknotsMaxTC_Mort=equalknots(lagframe_Mort$MaxTC,fun="bs",df=4,degree=2)
#varknotsMaxTC
lagknotsMaxTC_Mort=logknots(21,2)
#lagknotsMaxTC

# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value
cb1.MaxTC_Mort=crossbasis(lagframe_Mort$MaxTC,lag=21,argvar=list(fun="bs",knots=varknotsMaxTC_Mort),arglag=list(knots=lagknotsMaxTC_Mort))
#summary(cb1.MaxTC)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VA Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VA Mort")


# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC01=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC,  dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC01$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC01=crossbasis(lagframe_MDC01$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue = median(MaxTC,na.rm = T))

# Define the position of knots for spline functions
varknotsMaxTC_MDC01=equalknots(lagframe_MDC01$MaxTC,fun="bs",df=4,degree=2)
#varknotsMaxTC
lagknotsMaxTC_MDC01=logknots(21,2)
#lagknotsMaxTC

# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value
cb1.MaxTC_MDC01=crossbasis(lagframe_MDC01$MaxTC,lag=21,argvar=list(fun="bs",knots=varknotsMaxTC_MDC01),arglag=list(knots=lagknotsMaxTC_MDC01))
#summary(cb1.MaxTC)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
modelA1_MDC01=glm(MDC01~cb1.MaxTC_MDC01+ns(Trend, 16*3)+as.factor(dow),family=quasipoisson(),lagframe_MDC01)
summary(modelA1_MDC01)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC01=crosspred(cb1.MaxTC_MDC01,modelA1_MDC01,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VA MDC01")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC04=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC,  dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC04$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC04=crossbasis(lagframe_MDC04$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue = median(MaxTC,na.rm = T))

# Define the position of knots for spline functions
varknotsMaxTC_MDC04=equalknots(lagframe_MDC04$MaxTC,fun="bs",df=4,degree=2)
#varknotsMaxTC
lagknotsMaxTC_MDC04=logknots(21,2)
#lagknotsMaxTC

# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value
cb1.MaxTC_MDC04=crossbasis(lagframe_MDC04$MaxTC,lag=21,argvar=list(fun="bs",knots=varknotsMaxTC_MDC04),arglag=list(knots=lagknotsMaxTC_MDC04))
#summary(cb1.MaxTC)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
modelA1_MDC04=glm(MDC04~cb1.MaxTC_MDC04+ns(Trend, 16*3)+as.factor(dow),family=quasipoisson(),lagframe_MDC04)
summary(modelA1_MDC04)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC04=crosspred(cb1.MaxTC_MDC04,modelA1_MDC04,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VA MDC04")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC05=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC,  dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC05$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC05=crossbasis(lagframe_MDC05$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue = median(MaxTC,na.rm = T))

# Define the position of knots for spline functions
varknotsMaxTC_MDC05=equalknots(lagframe_MDC05$MaxTC,fun="bs",df=4,degree=2)
#varknotsMaxTC
lagknotsMaxTC_MDC05=logknots(21,2)
#lagknotsMaxTC

# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value
cb1.MaxTC_MDC05=crossbasis(lagframe_MDC05$MaxTC,lag=21,argvar=list(fun="bs",knots=varknotsMaxTC_MDC05),arglag=list(knots=lagknotsMaxTC_MDC05))
#summary(cb1.MaxTC)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
modelA1_MDC05=glm(MDC05~cb1.MaxTC_MDC05+ns(Trend, 16*3)+as.factor(dow),family=quasipoisson(),lagframe_MDC05)
summary(modelA1_MDC05)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC05=crosspred(cb1.MaxTC_MDC05,modelA1_MDC05,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VA MDC05")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC19=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC,  dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC19$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC19=crossbasis(lagframe_MDC19$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue = median(MaxTC,na.rm = T))

# Define the position of knots for spline functions
varknotsMaxTC_MDC19=equalknots(lagframe_MDC19$MaxTC,fun="bs",df=4,degree=2)
#varknotsMaxTC
lagknotsMaxTC_MDC19=logknots(21,2)
#lagknotsMaxTC

# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value
cb1.MaxTC_MDC19=crossbasis(lagframe_MDC19$MaxTC,lag=21,argvar=list(fun="bs",knots=varknotsMaxTC_MDC19),arglag=list(knots=lagknotsMaxTC_MDC19))
#summary(cb1.MaxTC)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
modelA1_MDC19=glm(MDC19~cb1.MaxTC_MDC19+ns(Trend, 16*3)+as.factor(dow),family=quasipoisson(),lagframe_MDC19)
summary(modelA1_MDC19)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC19=crosspred(cb1.MaxTC_MDC19,modelA1_MDC19,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VA MDC19")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_NonBillICD=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC,  dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_NonBillICD$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue = median(MaxTC,na.rm = T))

# Define the position of knots for spline functions
varknotsMaxTC_NonBillICD=equalknots(lagframe_NonBillICD$MaxTC,fun="bs",df=4,degree=2)
#varknotsMaxTC
lagknotsMaxTC_NonBillICD=logknots(21,2)
#lagknotsMaxTC

# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value
cb1.MaxTC_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,lag=21,argvar=list(fun="bs",knots=varknotsMaxTC_NonBillICD),arglag=list(knots=lagknotsMaxTC_NonBillICD))
#summary(cb1.MaxTC)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
modelA1_NonBillICD=glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)
summary(modelA1_NonBillICD)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_NonBillICD=crosspred(cb1.MaxTC_NonBillICD,modelA1_NonBillICD,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VA NonBillICD")

TempQuant_All <- quantile(MaxTC, probs = c(0.05,0.10,0.50,0.90,0.95),na.rm=T)

TempQuant_05 <- round(quantile(MaxTC, probs = c(0.05),na.rm=T),digits = 0)
TempQuant_10 <- round(quantile(MaxTC, probs = c(0.10),na.rm=T),digits = 0)
TempQuant_50 <- round(quantile(MaxTC, probs = c(0.50),na.rm=T),digits = 0)
TempQuant_90 <- round(quantile(MaxTC, probs = c(0.90),na.rm=T),digits = 0)
TempQuant_95 <- round(quantile(MaxTC, probs = c(0.95),na.rm=T),digits = 0)

TempQuant_Med <- round(median(MaxTC,na.rm=T),digits=0)

png("MDC_Stack_CurvesWx_Quant_VertLines_VA.png", width = 500, height = 1400)

par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VA Mort Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_05,lty = 2, col = 'blue')
abline(v = TempQuant_10,lty = 2, col = 'lightblue')
abline(v = TempQuant_50,lty = 2, col = 'grey')
abline(v = TempQuant_90,lty = 2, col = 'red1')
abline(v = TempQuant_95,lty = 2, col = 'red4')
#abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VA MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_05,lty = 2, col = 'blue')
abline(v = TempQuant_10,lty = 2, col = 'lightblue')
abline(v = TempQuant_50,lty = 2, col = 'grey')
abline(v = TempQuant_90,lty = 2, col = 'red1')
abline(v = TempQuant_95,lty = 2, col = 'red4')
#abline(v = TempQuant_All,lty = 2)


plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VA MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_05,lty = 2, col = 'blue')
abline(v = TempQuant_10,lty = 2, col = 'lightblue')
abline(v = TempQuant_50,lty = 2, col = 'grey')
abline(v = TempQuant_90,lty = 2, col = 'red1')
abline(v = TempQuant_95,lty = 2, col = 'red4')
#abline(v = TempQuant_All,lty = 2)


plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VA MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_05,lty = 2, col = 'blue')
abline(v = TempQuant_10,lty = 2, col = 'lightblue')
abline(v = TempQuant_50,lty = 2, col = 'grey')
abline(v = TempQuant_90,lty = 2, col = 'red1')
abline(v = TempQuant_95,lty = 2, col = 'red4')
#abline(v = TempQuant_All,lty = 2)


plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VA MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_05,lty = 2, col = 'blue')
abline(v = TempQuant_10,lty = 2, col = 'lightblue')
abline(v = TempQuant_50,lty = 2, col = 'grey')
abline(v = TempQuant_90,lty = 2, col = 'red1')
abline(v = TempQuant_95,lty = 2, col = 'red4')
#abline(v = TempQuant_All,lty = 2)


plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VA NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_05,lty = 2, col = 'blue')
abline(v = TempQuant_10,lty = 2, col = 'lightblue')
abline(v = TempQuant_50,lty = 2, col = 'grey')
abline(v = TempQuant_90,lty = 2, col = 'red1')
abline(v = TempQuant_95,lty = 2, col = 'red4')
#abline(v = TempQuant_All,lty = 2)


dev.off()

MortRRValues <- data.frame(pred1.MaxTC_Mort$allRRfit)
MortRRValues$rownames <- rownames(MortRRValues)

Mort_Diff_05 <- round(((MortRRValues[MortRRValues$rownames == TempQuant_05, 1]) - (MortRRValues[MortRRValues$rownames == TempQuant_Med, 1])),digits = 4)
Mort_Diff_10 <- round(((MortRRValues[MortRRValues$rownames == TempQuant_10, 1]) - (MortRRValues[MortRRValues$rownames == TempQuant_Med, 1])),digits = 4)
Mort_Diff_50 <- round(((MortRRValues[MortRRValues$rownames == TempQuant_50, 1]) - (MortRRValues[MortRRValues$rownames == TempQuant_Med, 1])),digits = 4)
Mort_Diff_90 <- round(((MortRRValues[MortRRValues$rownames == TempQuant_90, 1]) - (MortRRValues[MortRRValues$rownames == TempQuant_Med, 1])),digits = 4)
Mort_Diff_95 <- round(((MortRRValues[MortRRValues$rownames == TempQuant_95, 1]) - (MortRRValues[MortRRValues$rownames == TempQuant_Med, 1])),digits = 4)

MDC01RRValues <- data.frame(pred1.MaxTC_MDC01$allRRfit)
MDC01RRValues$rownames <- rownames(MDC01RRValues)

MDC01_Diff_05 <- round(((MDC01RRValues[MDC01RRValues$rownames == TempQuant_05, 1]) - (MDC01RRValues[MDC01RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC01_Diff_10 <- round(((MDC01RRValues[MDC01RRValues$rownames == TempQuant_10, 1]) - (MDC01RRValues[MDC01RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC01_Diff_50 <- round(((MDC01RRValues[MDC01RRValues$rownames == TempQuant_50, 1]) - (MDC01RRValues[MDC01RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC01_Diff_90 <- round(((MDC01RRValues[MDC01RRValues$rownames == TempQuant_90, 1]) - (MDC01RRValues[MDC01RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC01_Diff_95 <- round(((MDC01RRValues[MDC01RRValues$rownames == TempQuant_95, 1]) - (MDC01RRValues[MDC01RRValues$rownames == TempQuant_Med, 1])),digits = 4)

MDC04RRValues <- data.frame(pred1.MaxTC_MDC04$allRRfit)
MDC04RRValues$rownames <- rownames(MDC04RRValues)

MDC04_Diff_05 <- round(((MDC04RRValues[MDC04RRValues$rownames == TempQuant_05, 1]) - (MDC04RRValues[MDC04RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC04_Diff_10 <- round(((MDC04RRValues[MDC04RRValues$rownames == TempQuant_10, 1]) - (MDC04RRValues[MDC04RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC04_Diff_50 <- round(((MDC04RRValues[MDC04RRValues$rownames == TempQuant_50, 1]) - (MDC04RRValues[MDC04RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC04_Diff_90 <- round(((MDC04RRValues[MDC04RRValues$rownames == TempQuant_90, 1]) - (MDC04RRValues[MDC04RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC04_Diff_95 <- round(((MDC04RRValues[MDC04RRValues$rownames == TempQuant_95, 1]) - (MDC04RRValues[MDC04RRValues$rownames == TempQuant_Med, 1])),digits = 4)

MDC05RRValues <- data.frame(pred1.MaxTC_MDC05$allRRfit)
MDC05RRValues$rownames <- rownames(MDC05RRValues)

MDC05_Diff_05 <- round(((MDC05RRValues[MDC05RRValues$rownames == TempQuant_05, 1]) - (MDC05RRValues[MDC05RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC05_Diff_10 <- round(((MDC05RRValues[MDC05RRValues$rownames == TempQuant_10, 1]) - (MDC05RRValues[MDC05RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC05_Diff_50 <- round(((MDC05RRValues[MDC05RRValues$rownames == TempQuant_50, 1]) - (MDC05RRValues[MDC05RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC05_Diff_90 <- round(((MDC05RRValues[MDC05RRValues$rownames == TempQuant_90, 1]) - (MDC05RRValues[MDC05RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC05_Diff_95 <- round(((MDC05RRValues[MDC05RRValues$rownames == TempQuant_95, 1]) - (MDC05RRValues[MDC05RRValues$rownames == TempQuant_Med, 1])),digits = 4)

MDC19RRValues <- data.frame(pred1.MaxTC_MDC19$allRRfit)
MDC19RRValues$rownames <- rownames(MDC19RRValues)

MDC19_Diff_05 <- round(((MDC19RRValues[MDC19RRValues$rownames == TempQuant_05, 1]) - (MDC19RRValues[MDC19RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC19_Diff_10 <- round(((MDC19RRValues[MDC19RRValues$rownames == TempQuant_10, 1]) - (MDC19RRValues[MDC19RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC19_Diff_50 <- round(((MDC19RRValues[MDC19RRValues$rownames == TempQuant_50, 1]) - (MDC19RRValues[MDC19RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC19_Diff_90 <- round(((MDC19RRValues[MDC19RRValues$rownames == TempQuant_90, 1]) - (MDC19RRValues[MDC19RRValues$rownames == TempQuant_Med, 1])),digits = 4)
MDC19_Diff_95 <- round(((MDC19RRValues[MDC19RRValues$rownames == TempQuant_95, 1]) - (MDC19RRValues[MDC19RRValues$rownames == TempQuant_Med, 1])),digits = 4)


NonBillICDRRValues <- data.frame(pred1.MaxTC_NonBillICD$allRRfit)
NonBillICDRRValues$rownames <- rownames(NonBillICDRRValues)

NonBillICD_Diff_05 <- round(((NonBillICDRRValues[NonBillICDRRValues$rownames == TempQuant_05, 1]) - (NonBillICDRRValues[NonBillICDRRValues$rownames == TempQuant_Med, 1])),digits = 4)
NonBillICD_Diff_10 <- round(((NonBillICDRRValues[NonBillICDRRValues$rownames == TempQuant_10, 1]) - (NonBillICDRRValues[NonBillICDRRValues$rownames == TempQuant_Med, 1])),digits = 4)
NonBillICD_Diff_50 <- round(((NonBillICDRRValues[NonBillICDRRValues$rownames == TempQuant_50, 1]) - (NonBillICDRRValues[NonBillICDRRValues$rownames == TempQuant_Med, 1])),digits = 4)
NonBillICD_Diff_90 <- round(((NonBillICDRRValues[NonBillICDRRValues$rownames == TempQuant_90, 1]) - (NonBillICDRRValues[NonBillICDRRValues$rownames == TempQuant_Med, 1])),digits = 4)
NonBillICD_Diff_95 <- round(((NonBillICDRRValues[NonBillICDRRValues$rownames == TempQuant_95, 1]) - (NonBillICDRRValues[NonBillICDRRValues$rownames == TempQuant_Med, 1])),digits = 4)

DiffTable_VA <- data.frame(matrix(nrow = 5,ncol = 6))
rownames(DiffTable_VA) <- c("5th","10th","50th","90th","95th")
colnames(DiffTable_VA) <- c("Mort","MDC01","MDC04","MDC05","MDC19","NonBillable ICD")

DiffTable_VA[1,] <- c(Mort_Diff_05,MDC01_Diff_05,MDC04_Diff_05,MDC05_Diff_05,MDC19_Diff_05,NonBillICD_Diff_05)
DiffTable_VA[2,] <- c(Mort_Diff_10,MDC01_Diff_10,MDC04_Diff_10,MDC05_Diff_10,MDC19_Diff_10,NonBillICD_Diff_10)
DiffTable_VA[3,] <- c(Mort_Diff_50,MDC01_Diff_50,MDC04_Diff_50,MDC05_Diff_50,MDC19_Diff_50,NonBillICD_Diff_50)
DiffTable_VA[4,] <- c(Mort_Diff_90,MDC01_Diff_90,MDC04_Diff_90,MDC05_Diff_90,MDC19_Diff_90,NonBillICD_Diff_90)
DiffTable_VA[5,] <- c(Mort_Diff_95,MDC01_Diff_95,MDC04_Diff_95,MDC05_Diff_95,MDC19_Diff_95,NonBillICD_Diff_95)

#Attributable Fraction Attempt
Betas_Mort <- pred1.MaxTC_Mort$coefficients
totalBetas_Mort <- sum(Betas_Mort)
AF_Mort <- 1 - exp(-(totalBetas_Mort))

Betas_MDC04 <- pred1.MaxTC_MDC04$coefficients
totalBetas_MDC04 <- sum(Betas_MDC04)
AF_MDC04 <- 1 - exp(-(totalBetas_MDC04))

Betas_MDC05 <- pred1.MaxTC_MDC05$coefficients
totalBetas_MDC05 <- sum(Betas_MDC05)
AF_MDC05 <- 1 - exp(-(totalBetas_MDC05))

Betas_Mort <- pred1.MaxTC_Mort$coefficients
totalBetas_Mort <- sum(Betas_Mort)
AF_Mort <- 1 - exp(-(totalBetas_Mort))

Betas_NonBillICD <- pred1.MaxTC_NonBillICD$coefficients
totalBetas_NonBillICD <- sum(Betas_NonBillICD)
AF_NonBillICD <- 1-exp(-totalBetas_NonBillICD)


