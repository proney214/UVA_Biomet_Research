library(readxl)
library(dplyr)
# DLNM component
library(splines)
library(dlnm)
library(lubridate)

#Load in Data

#CHO

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
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_Mort=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_Mort$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_Mort=crossbasis(lagframe_Mort$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="CHO Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="CHO Mort")


# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC01=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC01$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC01=crossbasis(lagframe_MDC01$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC01=glm(MDC01~cb1.MaxTC_MDC01+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC01)
summary(modelA1_MDC01)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC01=crosspred(cb1.MaxTC_MDC01,modelA1_MDC01,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="CHO MDC01")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC04=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC04$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC04=crossbasis(lagframe_MDC04$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC04=glm(MDC04~cb1.MaxTC_MDC04+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC04)
summary(modelA1_MDC04)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC04=crosspred(cb1.MaxTC_MDC04,modelA1_MDC04,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="CHO MDC04")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC05=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC05$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC05=crossbasis(lagframe_MDC05$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC05=glm(MDC05~cb1.MaxTC_MDC05+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC05)
summary(modelA1_MDC05)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC05=crosspred(cb1.MaxTC_MDC05,modelA1_MDC05,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="CHO MDC05")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC19=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC19$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC19=crossbasis(lagframe_MDC19$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC19=glm(MDC19~cb1.MaxTC_MDC19+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC19)
summary(modelA1_MDC19)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC19=crosspred(cb1.MaxTC_MDC19,modelA1_MDC19,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="CHO MDC19")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_NonBillICD=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_NonBillICD$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_NonBillICD=glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)
summary(modelA1_NonBillICD)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_NonBillICD=crosspred(cb1.MaxTC_NonBillICD,modelA1_NonBillICD,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="CHO NonBillICD")

TempQuant_All <- quantile(MaxTC, probs = c(0.05,0.10,0.50,0.90,0.95),na.rm=T)

TempQuant_05 <- round(quantile(MaxTC, probs = c(0.05),na.rm=T),digits = 0)
TempQuant_10 <- round(quantile(MaxTC, probs = c(0.10),na.rm=T),digits = 0)
TempQuant_50 <- round(quantile(MaxTC, probs = c(0.50),na.rm=T),digits = 0)
TempQuant_90 <- round(quantile(MaxTC, probs = c(0.90),na.rm=T),digits = 0)
TempQuant_95 <- round(quantile(MaxTC, probs = c(0.95),na.rm=T),digits = 0)

TempQuant_Med <- round(median(MaxTC,na.rm=T),digits=0)

png("MDC_Stack_CurvesWx_Quant_VertLines_CHO.png", width = 500, height = 1400)

par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="CHO Mort Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="CHO MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="CHO MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="CHO MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="CHO MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="CHO NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

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

DiffTable_CHO <- data.frame(matrix(nrow = 5,ncol = 6))
rownames(DiffTable_CHO) <- c("5th","10th","50th","90th","95th")
colnames(DiffTable_CHO) <- c("Mort","MDC01","MDC04","MDC05","MDC19","NonBillable ICD")

DiffTable_CHO[1,] <- c(Mort_Diff_05,MDC01_Diff_05,MDC04_Diff_05,MDC05_Diff_05,MDC19_Diff_05,NonBillICD_Diff_05)
DiffTable_CHO[2,] <- c(Mort_Diff_10,MDC01_Diff_10,MDC04_Diff_10,MDC05_Diff_10,MDC19_Diff_10,NonBillICD_Diff_10)
DiffTable_CHO[3,] <- c(Mort_Diff_50,MDC01_Diff_50,MDC04_Diff_50,MDC05_Diff_50,MDC19_Diff_50,NonBillICD_Diff_50)
DiffTable_CHO[4,] <- c(Mort_Diff_90,MDC01_Diff_90,MDC04_Diff_90,MDC05_Diff_90,MDC19_Diff_90,NonBillICD_Diff_90)
DiffTable_CHO[5,] <- c(Mort_Diff_95,MDC01_Diff_95,MDC04_Diff_95,MDC05_Diff_95,MDC19_Diff_95,NonBillICD_Diff_95)

#Load in Data

#EMV

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
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_Mort=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_Mort$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_Mort=crossbasis(lagframe_Mort$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EMV Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EMV Mort")


# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC01=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC01$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC01=crossbasis(lagframe_MDC01$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC01=glm(MDC01~cb1.MaxTC_MDC01+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC01)
summary(modelA1_MDC01)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC01=crosspred(cb1.MaxTC_MDC01,modelA1_MDC01,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EMV MDC01")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC04=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC04$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC04=crossbasis(lagframe_MDC04$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC04=glm(MDC04~cb1.MaxTC_MDC04+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC04)
summary(modelA1_MDC04)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC04=crosspred(cb1.MaxTC_MDC04,modelA1_MDC04,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EMV MDC04")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC05=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC05$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC05=crossbasis(lagframe_MDC05$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC05=glm(MDC05~cb1.MaxTC_MDC05+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC05)
summary(modelA1_MDC05)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC05=crosspred(cb1.MaxTC_MDC05,modelA1_MDC05,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EMV MDC05")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC19=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC19$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC19=crossbasis(lagframe_MDC19$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC19=glm(MDC19~cb1.MaxTC_MDC19+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC19)
summary(modelA1_MDC19)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC19=crosspred(cb1.MaxTC_MDC19,modelA1_MDC19,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EMV MDC19")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_NonBillICD=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_NonBillICD$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_NonBillICD=glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)
summary(modelA1_NonBillICD)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_NonBillICD=crosspred(cb1.MaxTC_NonBillICD,modelA1_NonBillICD,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EMV NonBillICD")

TempQuant_All <- quantile(MaxTC, probs = c(0.05,0.10,0.50,0.90,0.95),na.rm=T)

TempQuant_05 <- round(quantile(MaxTC, probs = c(0.05),na.rm=T),digits = 0)
TempQuant_10 <- round(quantile(MaxTC, probs = c(0.10),na.rm=T),digits = 0)
TempQuant_50 <- round(quantile(MaxTC, probs = c(0.50),na.rm=T),digits = 0)
TempQuant_90 <- round(quantile(MaxTC, probs = c(0.90),na.rm=T),digits = 0)
TempQuant_95 <- round(quantile(MaxTC, probs = c(0.95),na.rm=T),digits = 0)

TempQuant_Med <- round(median(MaxTC,na.rm=T),digits=0)

png("MDC_Stack_CurvesWx_Quant_VertLines_EMV.png", width = 500, height = 1400)

par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EMV Mort Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EMV MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EMV MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EMV MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EMV MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EMV NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

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

DiffTable_EMV <- data.frame(matrix(nrow = 5,ncol = 6))
rownames(DiffTable_EMV) <- c("5th","10th","50th","90th","95th")
colnames(DiffTable_EMV) <- c("Mort","MDC01","MDC04","MDC05","MDC19","NonBillable ICD")

DiffTable_EMV[1,] <- c(Mort_Diff_05,MDC01_Diff_05,MDC04_Diff_05,MDC05_Diff_05,MDC19_Diff_05,NonBillICD_Diff_05)
DiffTable_EMV[2,] <- c(Mort_Diff_10,MDC01_Diff_10,MDC04_Diff_10,MDC05_Diff_10,MDC19_Diff_10,NonBillICD_Diff_10)
DiffTable_EMV[3,] <- c(Mort_Diff_50,MDC01_Diff_50,MDC04_Diff_50,MDC05_Diff_50,MDC19_Diff_50,NonBillICD_Diff_50)
DiffTable_EMV[4,] <- c(Mort_Diff_90,MDC01_Diff_90,MDC04_Diff_90,MDC05_Diff_90,MDC19_Diff_90,NonBillICD_Diff_90)
DiffTable_EMV[5,] <- c(Mort_Diff_95,MDC01_Diff_95,MDC04_Diff_95,MDC05_Diff_95,MDC19_Diff_95,NonBillICD_Diff_95)


#Load in Data

#EZF

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
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_Mort=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_Mort$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_Mort=crossbasis(lagframe_Mort$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EZF Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EZF Mort")


# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC01=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC01$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC01=crossbasis(lagframe_MDC01$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC01=glm(MDC01~cb1.MaxTC_MDC01+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC01)
summary(modelA1_MDC01)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC01=crosspred(cb1.MaxTC_MDC01,modelA1_MDC01,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EZF MDC01")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC04=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC04$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC04=crossbasis(lagframe_MDC04$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC04=glm(MDC04~cb1.MaxTC_MDC04+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC04)
summary(modelA1_MDC04)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC04=crosspred(cb1.MaxTC_MDC04,modelA1_MDC04,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EZF MDC04")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC05=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC05$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC05=crossbasis(lagframe_MDC05$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC05=glm(MDC05~cb1.MaxTC_MDC05+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC05)
summary(modelA1_MDC05)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC05=crosspred(cb1.MaxTC_MDC05,modelA1_MDC05,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EZF MDC05")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC19=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC19$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC19=crossbasis(lagframe_MDC19$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC19=glm(MDC19~cb1.MaxTC_MDC19+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC19)
summary(modelA1_MDC19)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC19=crosspred(cb1.MaxTC_MDC19,modelA1_MDC19,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EZF MDC19")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_NonBillICD=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_NonBillICD$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_NonBillICD=glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)
summary(modelA1_NonBillICD)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_NonBillICD=crosspred(cb1.MaxTC_NonBillICD,modelA1_NonBillICD,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="EZF NonBillICD")

TempQuant_All <- quantile(MaxTC, probs = c(0.05,0.10,0.50,0.90,0.95),na.rm=T)

TempQuant_05 <- round(quantile(MaxTC, probs = c(0.05),na.rm=T),digits = 0)
TempQuant_10 <- round(quantile(MaxTC, probs = c(0.10),na.rm=T),digits = 0)
TempQuant_50 <- round(quantile(MaxTC, probs = c(0.50),na.rm=T),digits = 0)
TempQuant_90 <- round(quantile(MaxTC, probs = c(0.90),na.rm=T),digits = 0)
TempQuant_95 <- round(quantile(MaxTC, probs = c(0.95),na.rm=T),digits = 0)

TempQuant_Med <- round(median(MaxTC,na.rm=T),digits=0)

png("MDC_Stack_CurvesWx_Quant_VertLines_EZF.png", width = 500, height = 1400)

par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EZF Mort Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EZF MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EZF MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EZF MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EZF MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="EZF NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

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

DiffTable_EZF <- data.frame(matrix(nrow = 5,ncol = 6))
rownames(DiffTable_EZF) <- c("5th","10th","50th","90th","95th")
colnames(DiffTable_EZF) <- c("Mort","MDC01","MDC04","MDC05","MDC19","NonBillable ICD")

DiffTable_EZF[1,] <- c(Mort_Diff_05,MDC01_Diff_05,MDC04_Diff_05,MDC05_Diff_05,MDC19_Diff_05,NonBillICD_Diff_05)
DiffTable_EZF[2,] <- c(Mort_Diff_10,MDC01_Diff_10,MDC04_Diff_10,MDC05_Diff_10,MDC19_Diff_10,NonBillICD_Diff_10)
DiffTable_EZF[3,] <- c(Mort_Diff_50,MDC01_Diff_50,MDC04_Diff_50,MDC05_Diff_50,MDC19_Diff_50,NonBillICD_Diff_50)
DiffTable_EZF[4,] <- c(Mort_Diff_90,MDC01_Diff_90,MDC04_Diff_90,MDC05_Diff_90,MDC19_Diff_90,NonBillICD_Diff_90)
DiffTable_EZF[5,] <- c(Mort_Diff_95,MDC01_Diff_95,MDC04_Diff_95,MDC05_Diff_95,MDC19_Diff_95,NonBillICD_Diff_95)


#Load in Data

#IAD

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
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_Mort=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_Mort$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_Mort=crossbasis(lagframe_Mort$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="IAD Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="IAD Mort")


# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC01=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC01$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC01=crossbasis(lagframe_MDC01$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC01=glm(MDC01~cb1.MaxTC_MDC01+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC01)
summary(modelA1_MDC01)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC01=crosspred(cb1.MaxTC_MDC01,modelA1_MDC01,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="IAD MDC01")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC04=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC04$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC04=crossbasis(lagframe_MDC04$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC04=glm(MDC04~cb1.MaxTC_MDC04+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC04)
summary(modelA1_MDC04)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC04=crosspred(cb1.MaxTC_MDC04,modelA1_MDC04,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="IAD MDC04")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC05=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC05$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC05=crossbasis(lagframe_MDC05$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC05=glm(MDC05~cb1.MaxTC_MDC05+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC05)
summary(modelA1_MDC05)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC05=crosspred(cb1.MaxTC_MDC05,modelA1_MDC05,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="IAD MDC05")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC19=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC19$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC19=crossbasis(lagframe_MDC19$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC19=glm(MDC19~cb1.MaxTC_MDC19+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC19)
summary(modelA1_MDC19)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC19=crosspred(cb1.MaxTC_MDC19,modelA1_MDC19,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="IAD MDC19")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_NonBillICD=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_NonBillICD$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_NonBillICD=glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)
summary(modelA1_NonBillICD)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_NonBillICD=crosspred(cb1.MaxTC_NonBillICD,modelA1_NonBillICD,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="IAD NonBillICD")

TempQuant_All <- quantile(MaxTC, probs = c(0.05,0.10,0.50,0.90,0.95),na.rm=T)

TempQuant_05 <- round(quantile(MaxTC, probs = c(0.05),na.rm=T),digits = 0)
TempQuant_10 <- round(quantile(MaxTC, probs = c(0.10),na.rm=T),digits = 0)
TempQuant_50 <- round(quantile(MaxTC, probs = c(0.50),na.rm=T),digits = 0)
TempQuant_90 <- round(quantile(MaxTC, probs = c(0.90),na.rm=T),digits = 0)
TempQuant_95 <- round(quantile(MaxTC, probs = c(0.95),na.rm=T),digits = 0)

TempQuant_Med <- round(median(MaxTC,na.rm=T),digits=0)

png("MDC_Stack_CurvesWx_Quant_VertLines_IAD.png", width = 500, height = 1400)

par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="IAD Mort Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="IAD MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="IAD MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="IAD MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="IAD MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="IAD NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

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

DiffTable_IAD <- data.frame(matrix(nrow = 5,ncol = 6))
rownames(DiffTable_IAD) <- c("5th","10th","50th","90th","95th")
colnames(DiffTable_IAD) <- c("Mort","MDC01","MDC04","MDC05","MDC19","NonBillable ICD")

DiffTable_IAD[1,] <- c(Mort_Diff_05,MDC01_Diff_05,MDC04_Diff_05,MDC05_Diff_05,MDC19_Diff_05,NonBillICD_Diff_05)
DiffTable_IAD[2,] <- c(Mort_Diff_10,MDC01_Diff_10,MDC04_Diff_10,MDC05_Diff_10,MDC19_Diff_10,NonBillICD_Diff_10)
DiffTable_IAD[3,] <- c(Mort_Diff_50,MDC01_Diff_50,MDC04_Diff_50,MDC05_Diff_50,MDC19_Diff_50,NonBillICD_Diff_50)
DiffTable_IAD[4,] <- c(Mort_Diff_90,MDC01_Diff_90,MDC04_Diff_90,MDC05_Diff_90,MDC19_Diff_90,NonBillICD_Diff_90)
DiffTable_IAD[5,] <- c(Mort_Diff_95,MDC01_Diff_95,MDC04_Diff_95,MDC05_Diff_95,MDC19_Diff_95,NonBillICD_Diff_95)


#Load in Data

#LYH

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
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_Mort=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_Mort$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_Mort=crossbasis(lagframe_Mort$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="LYH Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="LYH Mort")


# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC01=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC01$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC01=crossbasis(lagframe_MDC01$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC01=glm(MDC01~cb1.MaxTC_MDC01+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC01)
summary(modelA1_MDC01)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC01=crosspred(cb1.MaxTC_MDC01,modelA1_MDC01,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="LYH MDC01")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC04=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC04$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC04=crossbasis(lagframe_MDC04$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC04=glm(MDC04~cb1.MaxTC_MDC04+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC04)
summary(modelA1_MDC04)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC04=crosspred(cb1.MaxTC_MDC04,modelA1_MDC04,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="LYH MDC04")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC05=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC05$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC05=crossbasis(lagframe_MDC05$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC05=glm(MDC05~cb1.MaxTC_MDC05+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC05)
summary(modelA1_MDC05)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC05=crosspred(cb1.MaxTC_MDC05,modelA1_MDC05,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="LYH MDC05")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC19=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC19$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC19=crossbasis(lagframe_MDC19$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC19=glm(MDC19~cb1.MaxTC_MDC19+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC19)
summary(modelA1_MDC19)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC19=crosspred(cb1.MaxTC_MDC19,modelA1_MDC19,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="LYH MDC19")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_NonBillICD=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_NonBillICD$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_NonBillICD=glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)
summary(modelA1_NonBillICD)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_NonBillICD=crosspred(cb1.MaxTC_NonBillICD,modelA1_NonBillICD,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="LYH NonBillICD")

TempQuant_All <- quantile(MaxTC, probs = c(0.05,0.10,0.50,0.90,0.95),na.rm=T)

TempQuant_05 <- round(quantile(MaxTC, probs = c(0.05),na.rm=T),digits = 0)
TempQuant_10 <- round(quantile(MaxTC, probs = c(0.10),na.rm=T),digits = 0)
TempQuant_50 <- round(quantile(MaxTC, probs = c(0.50),na.rm=T),digits = 0)
TempQuant_90 <- round(quantile(MaxTC, probs = c(0.90),na.rm=T),digits = 0)
TempQuant_95 <- round(quantile(MaxTC, probs = c(0.95),na.rm=T),digits = 0)

TempQuant_Med <- round(median(MaxTC,na.rm=T),digits=0)

png("MDC_Stack_CurvesWx_Quant_VertLines_LYH.png", width = 500, height = 1400)

par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="LYH Mort Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="LYH MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="LYH MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="LYH MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="LYH MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="LYH NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

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

DiffTable_LYH <- data.frame(matrix(nrow = 5,ncol = 6))
rownames(DiffTable_LYH) <- c("5th","10th","50th","90th","95th")
colnames(DiffTable_LYH) <- c("Mort","MDC01","MDC04","MDC05","MDC19","NonBillable ICD")

DiffTable_LYH[1,] <- c(Mort_Diff_05,MDC01_Diff_05,MDC04_Diff_05,MDC05_Diff_05,MDC19_Diff_05,NonBillICD_Diff_05)
DiffTable_LYH[2,] <- c(Mort_Diff_10,MDC01_Diff_10,MDC04_Diff_10,MDC05_Diff_10,MDC19_Diff_10,NonBillICD_Diff_10)
DiffTable_LYH[3,] <- c(Mort_Diff_50,MDC01_Diff_50,MDC04_Diff_50,MDC05_Diff_50,MDC19_Diff_50,NonBillICD_Diff_50)
DiffTable_LYH[4,] <- c(Mort_Diff_90,MDC01_Diff_90,MDC04_Diff_90,MDC05_Diff_90,MDC19_Diff_90,NonBillICD_Diff_90)
DiffTable_LYH[5,] <- c(Mort_Diff_95,MDC01_Diff_95,MDC04_Diff_95,MDC05_Diff_95,MDC19_Diff_95,NonBillICD_Diff_95)


#Load in Data

#OKV

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
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_Mort=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_Mort$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_Mort=crossbasis(lagframe_Mort$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="OKV Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="OKV Mort")


# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC01=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC01$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC01=crossbasis(lagframe_MDC01$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC01=glm(MDC01~cb1.MaxTC_MDC01+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC01)
summary(modelA1_MDC01)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC01=crosspred(cb1.MaxTC_MDC01,modelA1_MDC01,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="OKV MDC01")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC04=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC04$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC04=crossbasis(lagframe_MDC04$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC04=glm(MDC04~cb1.MaxTC_MDC04+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC04)
summary(modelA1_MDC04)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC04=crosspred(cb1.MaxTC_MDC04,modelA1_MDC04,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="OKV MDC04")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC05=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC05$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC05=crossbasis(lagframe_MDC05$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC05=glm(MDC05~cb1.MaxTC_MDC05+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC05)
summary(modelA1_MDC05)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC05=crosspred(cb1.MaxTC_MDC05,modelA1_MDC05,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="OKV MDC05")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC19=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC19$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC19=crossbasis(lagframe_MDC19$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC19=glm(MDC19~cb1.MaxTC_MDC19+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC19)
summary(modelA1_MDC19)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC19=crosspred(cb1.MaxTC_MDC19,modelA1_MDC19,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="OKV MDC19")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_NonBillICD=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_NonBillICD$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_NonBillICD=glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)
summary(modelA1_NonBillICD)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_NonBillICD=crosspred(cb1.MaxTC_NonBillICD,modelA1_NonBillICD,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="OKV NonBillICD")

TempQuant_All <- quantile(MaxTC, probs = c(0.05,0.10,0.50,0.90,0.95),na.rm=T)

TempQuant_05 <- round(quantile(MaxTC, probs = c(0.05),na.rm=T),digits = 0)
TempQuant_10 <- round(quantile(MaxTC, probs = c(0.10),na.rm=T),digits = 0)
TempQuant_50 <- round(quantile(MaxTC, probs = c(0.50),na.rm=T),digits = 0)
TempQuant_90 <- round(quantile(MaxTC, probs = c(0.90),na.rm=T),digits = 0)
TempQuant_95 <- round(quantile(MaxTC, probs = c(0.95),na.rm=T),digits = 0)

TempQuant_Med <- round(median(MaxTC,na.rm=T),digits=0)

png("MDC_Stack_CurvesWx_Quant_VertLines_OKV.png", width = 500, height = 1400)

par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="OKV Mort Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="OKV MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="OKV MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="OKV MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="OKV MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="OKV NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

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

DiffTable_OKV <- data.frame(matrix(nrow = 5,ncol = 6))
rownames(DiffTable_OKV) <- c("5th","10th","50th","90th","95th")
colnames(DiffTable_OKV) <- c("Mort","MDC01","MDC04","MDC05","MDC19","NonBillable ICD")

DiffTable_OKV[1,] <- c(Mort_Diff_05,MDC01_Diff_05,MDC04_Diff_05,MDC05_Diff_05,MDC19_Diff_05,NonBillICD_Diff_05)
DiffTable_OKV[2,] <- c(Mort_Diff_10,MDC01_Diff_10,MDC04_Diff_10,MDC05_Diff_10,MDC19_Diff_10,NonBillICD_Diff_10)
DiffTable_OKV[3,] <- c(Mort_Diff_50,MDC01_Diff_50,MDC04_Diff_50,MDC05_Diff_50,MDC19_Diff_50,NonBillICD_Diff_50)
DiffTable_OKV[4,] <- c(Mort_Diff_90,MDC01_Diff_90,MDC04_Diff_90,MDC05_Diff_90,MDC19_Diff_90,NonBillICD_Diff_90)
DiffTable_OKV[5,] <- c(Mort_Diff_95,MDC01_Diff_95,MDC04_Diff_95,MDC05_Diff_95,MDC19_Diff_95,NonBillICD_Diff_95)


#Load in Data

#ORF

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
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_Mort=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_Mort$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_Mort=crossbasis(lagframe_Mort$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ORF Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ORF Mort")


# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC01=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC01$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC01=crossbasis(lagframe_MDC01$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC01=glm(MDC01~cb1.MaxTC_MDC01+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC01)
summary(modelA1_MDC01)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC01=crosspred(cb1.MaxTC_MDC01,modelA1_MDC01,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ORF MDC01")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC04=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC04$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC04=crossbasis(lagframe_MDC04$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC04=glm(MDC04~cb1.MaxTC_MDC04+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC04)
summary(modelA1_MDC04)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC04=crosspred(cb1.MaxTC_MDC04,modelA1_MDC04,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ORF MDC04")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC05=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC05$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC05=crossbasis(lagframe_MDC05$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC05=glm(MDC05~cb1.MaxTC_MDC05+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC05)
summary(modelA1_MDC05)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC05=crosspred(cb1.MaxTC_MDC05,modelA1_MDC05,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ORF MDC05")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC19=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC19$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC19=crossbasis(lagframe_MDC19$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC19=glm(MDC19~cb1.MaxTC_MDC19+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC19)
summary(modelA1_MDC19)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC19=crosspred(cb1.MaxTC_MDC19,modelA1_MDC19,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ORF MDC19")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_NonBillICD=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_NonBillICD$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_NonBillICD=glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)
summary(modelA1_NonBillICD)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_NonBillICD=crosspred(cb1.MaxTC_NonBillICD,modelA1_NonBillICD,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ORF NonBillICD")

TempQuant_All <- quantile(MaxTC, probs = c(0.05,0.10,0.50,0.90,0.95),na.rm=T)

TempQuant_05 <- round(quantile(MaxTC, probs = c(0.05),na.rm=T),digits = 0)
TempQuant_10 <- round(quantile(MaxTC, probs = c(0.10),na.rm=T),digits = 0)
TempQuant_50 <- round(quantile(MaxTC, probs = c(0.50),na.rm=T),digits = 0)
TempQuant_90 <- round(quantile(MaxTC, probs = c(0.90),na.rm=T),digits = 0)
TempQuant_95 <- round(quantile(MaxTC, probs = c(0.95),na.rm=T),digits = 0)

TempQuant_Med <- round(median(MaxTC,na.rm=T),digits=0)

png("MDC_Stack_CurvesWx_Quant_VertLines_ORF.png", width = 500, height = 1400)

par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ORF Mort Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ORF MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ORF MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ORF MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ORF MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ORF NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

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

DiffTable_ORF <- data.frame(matrix(nrow = 5,ncol = 6))
rownames(DiffTable_ORF) <- c("5th","10th","50th","90th","95th")
colnames(DiffTable_ORF) <- c("Mort","MDC01","MDC04","MDC05","MDC19","NonBillable ICD")

DiffTable_ORF[1,] <- c(Mort_Diff_05,MDC01_Diff_05,MDC04_Diff_05,MDC05_Diff_05,MDC19_Diff_05,NonBillICD_Diff_05)
DiffTable_ORF[2,] <- c(Mort_Diff_10,MDC01_Diff_10,MDC04_Diff_10,MDC05_Diff_10,MDC19_Diff_10,NonBillICD_Diff_10)
DiffTable_ORF[3,] <- c(Mort_Diff_50,MDC01_Diff_50,MDC04_Diff_50,MDC05_Diff_50,MDC19_Diff_50,NonBillICD_Diff_50)
DiffTable_ORF[4,] <- c(Mort_Diff_90,MDC01_Diff_90,MDC04_Diff_90,MDC05_Diff_90,MDC19_Diff_90,NonBillICD_Diff_90)
DiffTable_ORF[5,] <- c(Mort_Diff_95,MDC01_Diff_95,MDC04_Diff_95,MDC05_Diff_95,MDC19_Diff_95,NonBillICD_Diff_95)


#Load in Data

#PHF

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
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_Mort=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_Mort$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_Mort=crossbasis(lagframe_Mort$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="PHF Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="PHF Mort")


# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC01=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC01$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC01=crossbasis(lagframe_MDC01$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC01=glm(MDC01~cb1.MaxTC_MDC01+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC01)
summary(modelA1_MDC01)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC01=crosspred(cb1.MaxTC_MDC01,modelA1_MDC01,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="PHF MDC01")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC04=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC04$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC04=crossbasis(lagframe_MDC04$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC04=glm(MDC04~cb1.MaxTC_MDC04+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC04)
summary(modelA1_MDC04)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC04=crosspred(cb1.MaxTC_MDC04,modelA1_MDC04,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="PHF MDC04")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC05=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC05$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC05=crossbasis(lagframe_MDC05$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC05=glm(MDC05~cb1.MaxTC_MDC05+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC05)
summary(modelA1_MDC05)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC05=crosspred(cb1.MaxTC_MDC05,modelA1_MDC05,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="PHF MDC05")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC19=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC19$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC19=crossbasis(lagframe_MDC19$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC19=glm(MDC19~cb1.MaxTC_MDC19+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC19)
summary(modelA1_MDC19)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC19=crosspred(cb1.MaxTC_MDC19,modelA1_MDC19,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="PHF MDC19")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_NonBillICD=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_NonBillICD$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_NonBillICD=glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)
summary(modelA1_NonBillICD)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_NonBillICD=crosspred(cb1.MaxTC_NonBillICD,modelA1_NonBillICD,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="PHF NonBillICD")

TempQuant_All <- quantile(MaxTC, probs = c(0.05,0.10,0.50,0.90,0.95),na.rm=T)

TempQuant_05 <- round(quantile(MaxTC, probs = c(0.05),na.rm=T),digits = 0)
TempQuant_10 <- round(quantile(MaxTC, probs = c(0.10),na.rm=T),digits = 0)
TempQuant_50 <- round(quantile(MaxTC, probs = c(0.50),na.rm=T),digits = 0)
TempQuant_90 <- round(quantile(MaxTC, probs = c(0.90),na.rm=T),digits = 0)
TempQuant_95 <- round(quantile(MaxTC, probs = c(0.95),na.rm=T),digits = 0)

TempQuant_Med <- round(median(MaxTC,na.rm=T),digits=0)

png("MDC_Stack_CurvesWx_Quant_VertLines_PHF.png", width = 500, height = 1400)

par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="PHF Mort Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="PHF MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="PHF MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="PHF MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="PHF MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="PHF NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

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

DiffTable_PHF <- data.frame(matrix(nrow = 5,ncol = 6))
rownames(DiffTable_PHF) <- c("5th","10th","50th","90th","95th")
colnames(DiffTable_PHF) <- c("Mort","MDC01","MDC04","MDC05","MDC19","NonBillable ICD")

DiffTable_PHF[1,] <- c(Mort_Diff_05,MDC01_Diff_05,MDC04_Diff_05,MDC05_Diff_05,MDC19_Diff_05,NonBillICD_Diff_05)
DiffTable_PHF[2,] <- c(Mort_Diff_10,MDC01_Diff_10,MDC04_Diff_10,MDC05_Diff_10,MDC19_Diff_10,NonBillICD_Diff_10)
DiffTable_PHF[3,] <- c(Mort_Diff_50,MDC01_Diff_50,MDC04_Diff_50,MDC05_Diff_50,MDC19_Diff_50,NonBillICD_Diff_50)
DiffTable_PHF[4,] <- c(Mort_Diff_90,MDC01_Diff_90,MDC04_Diff_90,MDC05_Diff_90,MDC19_Diff_90,NonBillICD_Diff_90)
DiffTable_PHF[5,] <- c(Mort_Diff_95,MDC01_Diff_95,MDC04_Diff_95,MDC05_Diff_95,MDC19_Diff_95,NonBillICD_Diff_95)


#Load in Data

#RIC

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
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_Mort=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_Mort$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_Mort=crossbasis(lagframe_Mort$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="RIC Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="RIC Mort")


# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC01=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC01$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC01=crossbasis(lagframe_MDC01$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC01=glm(MDC01~cb1.MaxTC_MDC01+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC01)
summary(modelA1_MDC01)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC01=crosspred(cb1.MaxTC_MDC01,modelA1_MDC01,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="RIC MDC01")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC04=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC04$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC04=crossbasis(lagframe_MDC04$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC04=glm(MDC04~cb1.MaxTC_MDC04+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC04)
summary(modelA1_MDC04)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC04=crosspred(cb1.MaxTC_MDC04,modelA1_MDC04,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="RIC MDC04")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC05=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC05$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC05=crossbasis(lagframe_MDC05$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC05=glm(MDC05~cb1.MaxTC_MDC05+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC05)
summary(modelA1_MDC05)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC05=crosspred(cb1.MaxTC_MDC05,modelA1_MDC05,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="RIC MDC05")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC19=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC19$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC19=crossbasis(lagframe_MDC19$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC19=glm(MDC19~cb1.MaxTC_MDC19+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC19)
summary(modelA1_MDC19)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC19=crosspred(cb1.MaxTC_MDC19,modelA1_MDC19,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="RIC MDC19")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_NonBillICD=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_NonBillICD$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_NonBillICD=glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)
summary(modelA1_NonBillICD)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_NonBillICD=crosspred(cb1.MaxTC_NonBillICD,modelA1_NonBillICD,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="RIC NonBillICD")

TempQuant_All <- quantile(MaxTC, probs = c(0.05,0.10,0.50,0.90,0.95),na.rm=T)

TempQuant_05 <- round(quantile(MaxTC, probs = c(0.05),na.rm=T),digits = 0)
TempQuant_10 <- round(quantile(MaxTC, probs = c(0.10),na.rm=T),digits = 0)
TempQuant_50 <- round(quantile(MaxTC, probs = c(0.50),na.rm=T),digits = 0)
TempQuant_90 <- round(quantile(MaxTC, probs = c(0.90),na.rm=T),digits = 0)
TempQuant_95 <- round(quantile(MaxTC, probs = c(0.95),na.rm=T),digits = 0)

TempQuant_Med <- round(median(MaxTC,na.rm=T),digits=0)

png("MDC_Stack_CurvesWx_Quant_VertLines_RIC.png", width = 500, height = 1400)

par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="RIC Mort Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="RIC MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="RIC MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="RIC MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="RIC MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="RIC NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

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

DiffTable_RIC <- data.frame(matrix(nrow = 5,ncol = 6))
rownames(DiffTable_RIC) <- c("5th","10th","50th","90th","95th")
colnames(DiffTable_RIC) <- c("Mort","MDC01","MDC04","MDC05","MDC19","NonBillable ICD")

DiffTable_RIC[1,] <- c(Mort_Diff_05,MDC01_Diff_05,MDC04_Diff_05,MDC05_Diff_05,MDC19_Diff_05,NonBillICD_Diff_05)
DiffTable_RIC[2,] <- c(Mort_Diff_10,MDC01_Diff_10,MDC04_Diff_10,MDC05_Diff_10,MDC19_Diff_10,NonBillICD_Diff_10)
DiffTable_RIC[3,] <- c(Mort_Diff_50,MDC01_Diff_50,MDC04_Diff_50,MDC05_Diff_50,MDC19_Diff_50,NonBillICD_Diff_50)
DiffTable_RIC[4,] <- c(Mort_Diff_90,MDC01_Diff_90,MDC04_Diff_90,MDC05_Diff_90,MDC19_Diff_90,NonBillICD_Diff_90)
DiffTable_RIC[5,] <- c(Mort_Diff_95,MDC01_Diff_95,MDC04_Diff_95,MDC05_Diff_95,MDC19_Diff_95,NonBillICD_Diff_95)


#Load in Data

#ROA

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
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_Mort=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_Mort$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_Mort=crossbasis(lagframe_Mort$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ROA Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ROA Mort")


# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC01=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC01$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC01=crossbasis(lagframe_MDC01$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC01=glm(MDC01~cb1.MaxTC_MDC01+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC01)
summary(modelA1_MDC01)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC01=crosspred(cb1.MaxTC_MDC01,modelA1_MDC01,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ROA MDC01")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC04=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC04$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC04=crossbasis(lagframe_MDC04$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC04=glm(MDC04~cb1.MaxTC_MDC04+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC04)
summary(modelA1_MDC04)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC04=crosspred(cb1.MaxTC_MDC04,modelA1_MDC04,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ROA MDC04")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC05=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC05$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC05=crossbasis(lagframe_MDC05$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC05=glm(MDC05~cb1.MaxTC_MDC05+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC05)
summary(modelA1_MDC05)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC05=crosspred(cb1.MaxTC_MDC05,modelA1_MDC05,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ROA MDC05")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC19=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC19$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC19=crossbasis(lagframe_MDC19$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC19=glm(MDC19~cb1.MaxTC_MDC19+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC19)
summary(modelA1_MDC19)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC19=crosspred(cb1.MaxTC_MDC19,modelA1_MDC19,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ROA MDC19")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_NonBillICD=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_NonBillICD$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_NonBillICD=glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)
summary(modelA1_NonBillICD)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_NonBillICD=crosspred(cb1.MaxTC_NonBillICD,modelA1_NonBillICD,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="ROA NonBillICD")

TempQuant_All <- quantile(MaxTC, probs = c(0.05,0.10,0.50,0.90,0.95),na.rm=T)

TempQuant_05 <- round(quantile(MaxTC, probs = c(0.05),na.rm=T),digits = 0)
TempQuant_10 <- round(quantile(MaxTC, probs = c(0.10),na.rm=T),digits = 0)
TempQuant_50 <- round(quantile(MaxTC, probs = c(0.50),na.rm=T),digits = 0)
TempQuant_90 <- round(quantile(MaxTC, probs = c(0.90),na.rm=T),digits = 0)
TempQuant_95 <- round(quantile(MaxTC, probs = c(0.95),na.rm=T),digits = 0)

TempQuant_Med <- round(median(MaxTC,na.rm=T),digits=0)

png("MDC_Stack_CurvesWx_Quant_VertLines_ROA.png", width = 500, height = 1400)

par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ROA Mort Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ROA MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ROA MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ROA MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ROA MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="ROA NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

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

DiffTable_ROA <- data.frame(matrix(nrow = 5,ncol = 6))
rownames(DiffTable_ROA) <- c("5th","10th","50th","90th","95th")
colnames(DiffTable_ROA) <- c("Mort","MDC01","MDC04","MDC05","MDC19","NonBillable ICD")

DiffTable_ROA[1,] <- c(Mort_Diff_05,MDC01_Diff_05,MDC04_Diff_05,MDC05_Diff_05,MDC19_Diff_05,NonBillICD_Diff_05)
DiffTable_ROA[2,] <- c(Mort_Diff_10,MDC01_Diff_10,MDC04_Diff_10,MDC05_Diff_10,MDC19_Diff_10,NonBillICD_Diff_10)
DiffTable_ROA[3,] <- c(Mort_Diff_50,MDC01_Diff_50,MDC04_Diff_50,MDC05_Diff_50,MDC19_Diff_50,NonBillICD_Diff_50)
DiffTable_ROA[4,] <- c(Mort_Diff_90,MDC01_Diff_90,MDC04_Diff_90,MDC05_Diff_90,MDC19_Diff_90,NonBillICD_Diff_90)
DiffTable_ROA[5,] <- c(Mort_Diff_95,MDC01_Diff_95,MDC04_Diff_95,MDC05_Diff_95,MDC19_Diff_95,NonBillICD_Diff_95)


#Load in Data

#SHD

SHD <- read_excel("~/Desktop/SHD.Final.Weather.xlsx",na="NA")
SHD <- SHD[1:5844, ]
#Convert Data to numeric
SHD %>% mutate_if(is.character, as.numeric)
head(SHD)
for(i in 1:ncol(SHD)){
  assign(names(SHD)[i], SHD[[i]])
}
#Create Trend Term
Trend = seq(1,5844,1)
#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing SHD$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

Mort <- as.numeric(SHD$Mort)
MDC01 <- as.numeric(SHD$MDC.01)
MDC04 <- as.numeric(SHD$MDC.04)
MDC05 <- as.numeric(SHD$MDC.05)
MDC19 <- as.numeric(SHD$MDC.19)
NonBillICD <- as.numeric(SHD$Non.billable.ICD)
MaxTC <- as.numeric(SHD$`MaxT(C)`)
dow <- as.factor(SHD$dow)
ColdWavesModerate <- as.factor(SHD$ColdWavesModerate)
HeatWavesModerate <- as.factor(SHD$HeatWavesModerate)
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_Mort=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_Mort$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_Mort=crossbasis(lagframe_Mort$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="SHD Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="SHD Mort")


# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC01=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC01$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC01=crossbasis(lagframe_MDC01$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC01=glm(MDC01~cb1.MaxTC_MDC01+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC01)
summary(modelA1_MDC01)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC01=crosspred(cb1.MaxTC_MDC01,modelA1_MDC01,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="SHD MDC01")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC04=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC04$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC04=crossbasis(lagframe_MDC04$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC04=glm(MDC04~cb1.MaxTC_MDC04+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC04)
summary(modelA1_MDC04)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC04=crosspred(cb1.MaxTC_MDC04,modelA1_MDC04,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="SHD MDC04")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC05=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC05$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC05=crossbasis(lagframe_MDC05$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC05=glm(MDC05~cb1.MaxTC_MDC05+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC05)
summary(modelA1_MDC05)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC05=crosspred(cb1.MaxTC_MDC05,modelA1_MDC05,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="SHD MDC05")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC19=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC19$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC19=crossbasis(lagframe_MDC19$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC19=glm(MDC19~cb1.MaxTC_MDC19+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC19)
summary(modelA1_MDC19)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC19=crosspred(cb1.MaxTC_MDC19,modelA1_MDC19,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="SHD MDC19")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_NonBillICD=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_NonBillICD$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_NonBillICD=glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)
summary(modelA1_NonBillICD)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_NonBillICD=crosspred(cb1.MaxTC_NonBillICD,modelA1_NonBillICD,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="SHD NonBillICD")

TempQuant_All <- quantile(MaxTC, probs = c(0.05,0.10,0.50,0.90,0.95),na.rm=T)

TempQuant_05 <- round(quantile(MaxTC, probs = c(0.05),na.rm=T),digits = 0)
TempQuant_10 <- round(quantile(MaxTC, probs = c(0.10),na.rm=T),digits = 0)
TempQuant_50 <- round(quantile(MaxTC, probs = c(0.50),na.rm=T),digits = 0)
TempQuant_90 <- round(quantile(MaxTC, probs = c(0.90),na.rm=T),digits = 0)
TempQuant_95 <- round(quantile(MaxTC, probs = c(0.95),na.rm=T),digits = 0)

TempQuant_Med <- round(median(MaxTC,na.rm=T),digits=0)

png("MDC_Stack_CurvesWx_Quant_VertLines_SHD.png", width = 500, height = 1400)

par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="SHD Mort Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="SHD MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="SHD MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="SHD MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="SHD MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="SHD NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

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

DiffTable_SHD <- data.frame(matrix(nrow = 5,ncol = 6))
rownames(DiffTable_SHD) <- c("5th","10th","50th","90th","95th")
colnames(DiffTable_SHD) <- c("Mort","MDC01","MDC04","MDC05","MDC19","NonBillable ICD")

DiffTable_SHD[1,] <- c(Mort_Diff_05,MDC01_Diff_05,MDC04_Diff_05,MDC05_Diff_05,MDC19_Diff_05,NonBillICD_Diff_05)
DiffTable_SHD[2,] <- c(Mort_Diff_10,MDC01_Diff_10,MDC04_Diff_10,MDC05_Diff_10,MDC19_Diff_10,NonBillICD_Diff_10)
DiffTable_SHD[3,] <- c(Mort_Diff_50,MDC01_Diff_50,MDC04_Diff_50,MDC05_Diff_50,MDC19_Diff_50,NonBillICD_Diff_50)
DiffTable_SHD[4,] <- c(Mort_Diff_90,MDC01_Diff_90,MDC04_Diff_90,MDC05_Diff_90,MDC19_Diff_90,NonBillICD_Diff_90)
DiffTable_SHD[5,] <- c(Mort_Diff_95,MDC01_Diff_95,MDC04_Diff_95,MDC05_Diff_95,MDC19_Diff_95,NonBillICD_Diff_95)

#Load in Data

#VJI

VJI <- read_excel("~/Desktop/VJI.Final.Weather.xlsx",na="NA")
VJI <- VJI[1:5844, ]
#Convert Data to numeric
VJI %>% mutate_if(is.character, as.numeric)
head(VJI)
for(i in 1:ncol(VJI)){
  assign(names(VJI)[i], VJI[[i]])
}
#Create Trend Term
Trend = seq(1,5844,1)
#Compute variables quickly from after loading df,
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing VJI$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you

Mort <- as.numeric(VJI$Mort)
MDC01 <- as.numeric(VJI$MDC.01)
MDC04 <- as.numeric(VJI$MDC.04)
MDC05 <- as.numeric(VJI$MDC.05)
MDC19 <- as.numeric(VJI$MDC.19)
NonBillICD <- as.numeric(VJI$Non.billable.ICD)
MaxTC <- as.numeric(VJI$`MaxT(C)`)
dow <- as.factor(VJI$dow)
ColdWavesModerate <- as.factor(VJI$ColdWavesModerate)
HeatWavesModerate <- as.factor(VJI$HeatWavesModerate)
#

#total Mort DLNM
# Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_Mort=data.frame(Mort=Mort, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_Mort$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_Mort=crossbasis(lagframe_Mort$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VJI Mort Lag RR", ylim = c(0.70,1.30),lwd=3)


# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VJI Mort")


# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC01=data.frame(MDC01=MDC01, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC01$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC01=crossbasis(lagframe_MDC01$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC01=glm(MDC01~cb1.MaxTC_MDC01+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC01)
summary(modelA1_MDC01)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC01=crosspred(cb1.MaxTC_MDC01,modelA1_MDC01,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VJI MDC01")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC04=data.frame(MDC04=MDC04, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC04$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC04=crossbasis(lagframe_MDC04$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC04=glm(MDC04~cb1.MaxTC_MDC04+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC04)
summary(modelA1_MDC04)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC04=crosspred(cb1.MaxTC_MDC04,modelA1_MDC04,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VJI MDC04")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC05=data.frame(MDC05=MDC05, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC05$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC05=crossbasis(lagframe_MDC05$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC05=glm(MDC05~cb1.MaxTC_MDC05+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC05)
summary(modelA1_MDC05)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC05=crosspred(cb1.MaxTC_MDC05,modelA1_MDC05,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VJI MDC05")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_MDC19=data.frame(MDC19=MDC19, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_MDC19$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_MDC19=crossbasis(lagframe_MDC19$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_MDC19=glm(MDC19~cb1.MaxTC_MDC19+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_MDC19)
summary(modelA1_MDC19)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_MDC19=crosspred(cb1.MaxTC_MDC19,modelA1_MDC19,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VJI MDC19")

# Create a data frame with lagged variables using all variables in "final" GAM
lagframe_NonBillICD=data.frame(NonBillICD=NonBillICD, Trend=Trend, MaxTC=MaxTC, ColdWavesModerate=ColdWavesModerate, HeatWavesModerate=HeatWavesModerate, dow=dow, datevar=datevar)

# Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe_NonBillICD$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=0)
#plot(time)
basis.MaxT_NonBillICD=crossbasis(lagframe_NonBillICD$MaxTC,vartype="ns",vardf=4,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

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
modelA1_NonBillICD=glm(NonBillICD~cb1.MaxTC_NonBillICD+ns(Trend, 16*3)+as.factor(HeatWavesModerate)+as.factor(ColdWavesModerate)+as.factor(dow),family=quasipoisson(),lagframe_NonBillICD)
summary(modelA1_NonBillICD)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_NonBillICD=crosspred(cb1.MaxTC_NonBillICD,modelA1_NonBillICD,by=1, cen = median(MaxTC,na.rm = T))

# 3-D plot
# plot(pred1.MaxTC,xlab="MaxTC",zlab="RR",theta=200,phi=40,lphi=30,main="VJI NonBillICD")

TempQuant_All <- quantile(MaxTC, probs = c(0.05,0.10,0.50,0.90,0.95),na.rm=T)

TempQuant_05 <- round(quantile(MaxTC, probs = c(0.05),na.rm=T),digits = 0)
TempQuant_10 <- round(quantile(MaxTC, probs = c(0.10),na.rm=T),digits = 0)
TempQuant_50 <- round(quantile(MaxTC, probs = c(0.50),na.rm=T),digits = 0)
TempQuant_90 <- round(quantile(MaxTC, probs = c(0.90),na.rm=T),digits = 0)
TempQuant_95 <- round(quantile(MaxTC, probs = c(0.95),na.rm=T),digits = 0)

TempQuant_Med <- round(median(MaxTC,na.rm=T),digits=0)

png("MDC_Stack_CurvesWx_Quant_VertLines_VJI.png", width = 500, height = 1400)

par(mfrow=c(6,1))

plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VJI Mort Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC01,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VJI MDC01 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC04,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VJI MDC04 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC05,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VJI MDC05 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_MDC19,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VJI MDC19 Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

plot(pred1.MaxTC_NonBillICD,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VJI NonBillICD Lag RR", ylim = c(0.70,1.30),lwd=3)
abline(v = TempQuant_All,lty = 2)

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

DiffTable_VJI <- data.frame(matrix(nrow = 5,ncol = 6))
rownames(DiffTable_VJI) <- c("5th","10th","50th","90th","95th")
colnames(DiffTable_VJI) <- c("Mort","MDC01","MDC04","MDC05","MDC19","NonBillable ICD")

DiffTable_VJI[1,] <- c(Mort_Diff_05,MDC01_Diff_05,MDC04_Diff_05,MDC05_Diff_05,MDC19_Diff_05,NonBillICD_Diff_05)
DiffTable_VJI[2,] <- c(Mort_Diff_10,MDC01_Diff_10,MDC04_Diff_10,MDC05_Diff_10,MDC19_Diff_10,NonBillICD_Diff_10)
DiffTable_VJI[3,] <- c(Mort_Diff_50,MDC01_Diff_50,MDC04_Diff_50,MDC05_Diff_50,MDC19_Diff_50,NonBillICD_Diff_50)
DiffTable_VJI[4,] <- c(Mort_Diff_90,MDC01_Diff_90,MDC04_Diff_90,MDC05_Diff_90,MDC19_Diff_90,NonBillICD_Diff_90)
DiffTable_VJI[5,] <- c(Mort_Diff_95,MDC01_Diff_95,MDC04_Diff_95,MDC05_Diff_95,MDC19_Diff_95,NonBillICD_Diff_95)




