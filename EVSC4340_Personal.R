#Personal Research
#Things to do: Chi Square Test
# CaseCross for Raw MDCs'

library(readxl)
library(dplyr)
library(zoo)
library(season)
library(splines)
library(dlnm)

CHO_Final_Weather <- read_excel("~/Desktop/CHO.Final.Weather.xlsx")
CHO <- CHO_Final_Weather

CHO %>% mutate_if(is.character, as.numeric)

CHOProfileMort <- CHO[ ,107:149] 
CHOProfileMDC <- CHOProfileMort[ ,1:27]
CHOProfileHotCold <- CHO[ , 92:103]
CHO$date <- as.Date(CHO$Date)
CHOselect <- cbind(CHOProfileHotCold,CHOProfileMort)

#Heat General Profile

CHOHotDaysModerate <- CHOselect[CHOselect$HotDaysModerate == 1, ]
CHOHotDaysModerate[as.numeric(length((CHOHotDaysModerate$HotDaysModerate)+1)), ] = colSums(CHOHotDaysModerate)
CHOHotDaysModerate[as.numeric(length((CHOHotDaysModerate$HotDaysModerate)+1)), ]

tableprofileHotDaysModerate = data.frame(colSums(CHOHotDaysModerate))
tableprofileHotDaysModerate$Proportion = NA
minitableHotDaysModerate <- tableprofileHotDaysModerate[13:39, ]
minitableHotDaysModerate[,2] <- (minitableHotDaysModerate[,1] / sum(minitableHotDaysModerate$colSums.CHOHotDaysModerate.)) * 100
minitableHotDaysModerate$Proportion <- round(minitableHotDaysModerate$Proportion, digits = 2)
sum(minitableHotDaysModerate$Proportion)
#sum(minitable$colSums.CHOHotDaysModerate.)

#Cold General Profile

CHOColdDaysModerate <- CHOselect[CHOselect$ColdDaysModerate == 1, ]
CHOColdDaysModerate[as.numeric(length((CHOColdDaysModerate$ColdDaysModerate)+1)), ] = colSums(CHOColdDaysModerate)

tableprofileColdDaysModerate = data.frame(colSums(CHOColdDaysModerate))
tableprofileColdDaysModerate$Proportion = NA
minitableColdDaysModerate <- tableprofileColdDaysModerate[13:39, ]
minitableColdDaysModerate[,2] <- (minitableColdDaysModerate[,1] / sum(minitableColdDaysModerate$colSums.CHOColdDaysModerate.)) * 100
minitableColdDaysModerate$Proportion <- round(minitableColdDaysModerate$Proportion, digits = 2)
sum(minitableColdDaysModerate$Proportion)

#No stress General Profile

CHONoStress <- CHOselect %>%
  filter(ColdDaysModerate != 1 & HotDaysModerate != 1)
sum(CHONoStress$HotDaysModerate)

tableprofileNoStress = data.frame(colSums(CHONoStress))
tableprofileNoStress$Proportion = NA
minitableNoStress <- tableprofileNoStress[13:39, ]
minitableNoStress[,2] <- (minitableNoStress[,1] / sum(minitableNoStress$colSums.CHONoStress.)) * 100
minitableNoStress$Proportion <- round(minitableNoStress$Proportion, digits = 2)
sum(minitableNoStress$Proportion)

#Daily Proportions

CHOProfileMDC$Daily <- rowSums(CHOProfileMDC)
CHOProfileMDC <- data.frame(CHOProfileMDC)

DailyProportionsCHO <- (CHOProfileMDC/CHOProfileMDC[,28])*100
DailyProportionsCHO <- round(DailyProportionsCHO, digits = 2)

CHOStress <- CHOselect %>%
  filter(ColdDaysModerate == 1 | HotDaysModerate == 1)

plot(DailyProportionsCHO$MDC.04, type='l', col='grey',xlim=c(0,720))
abline(v = which(CHO$HotDaysModerate == 1), col = 'pink')
abline(v = which(CHO$ColdDaysModerate == 1), col = 'light blue')

lines(DailyProportionsCHO$MDC.04, type='l', col='dark grey')
abline(h=mean(DailyProportionsCHO$MDC.04), col='green',lwd=4)
abline(h=quantile(DailyProportionsCHO$MDC.04, c(.10, .90)),col='green')

lines(rollmean(rollmean(DailyProportionsCHO$MDC.04,28),28),col='orange',lwd=2)
lines(rollmean(DailyProportionsCHO$MDC.04,7),col='blue',lwd=2)
abline(h=mean(rollmean(DailyProportionsCHO$MDC.04,7)), col='purple',lwd=4)
abline(h=quantile(rollmean(DailyProportionsCHO$MDC.04,7), c(.10, .90)),col='purple')

#Calculate expected proportion per day

DailyProportionsCHO$doy <- CHO$doy

YearlyMDCProportionsCHOMDC1 <- DailyProportionsCHO %>%
  group_by(doy) %>%
  summarize(CHO_mean_MDC1 = mean(MDC.01))

YearlyMDCProportionsCHOMDC4 <- DailyProportionsCHO %>%
  group_by(doy) %>%
  summarize(CHO_mean_MDC4 = mean(MDC.04))

YearlyMDCProportionsCHOMDC5 <- DailyProportionsCHO %>%
  group_by(doy) %>%
  summarize(CHO_mean_MDC5 = mean(MDC.05))

plot(YearlyMDCProportionsCHOMDC5$CHO_mean_MDC5)

plot(YearlyMDCProportionsCHOMDC1$CHO_mean_MDC1,type='l',main="CHO MDC 1")
#abline(h=mean(YearlyMDCProportions$mean_MDC4))
abline(h=quantile(YearlyMDCProportionsCHOMDC1$CHO_mean_MDC1),col='blue')
lines(rollmean(YearlyMDCProportionsCHOMDC1$CHO_mean_MDC1,7),col='blue',lwd=2)
abline(v=seq(1,365,30))

plot(YearlyMDCProportionsCHOMDC4$CHO_mean_MDC4,type='l',main="CHO MDC 4")
#abline(h=mean(YearlyMDCProportions$mean_MDC4))
abline(h=quantile(YearlyMDCProportionsCHOMDC4$CHO_mean_MDC4),col='blue')
lines(rollmean(YearlyMDCProportionsCHOMDC4$CHO_mean_MDC4,7),col='blue',lwd=2)
abline(v=seq(1,365,30))

plot(YearlyMDCProportionsCHOMDC5$CHO_mean_MDC5,type='l',main="CHO MDC 5")
#abline(h=mean(YearlyMDCProportions$mean_MDC4))
abline(h=quantile(YearlyMDCProportionsCHOMDC5$CHO_mean_MDC5),col='blue')
lines(rollmean(YearlyMDCProportionsCHOMDC5$CHO_mean_MDC5,7),col='blue',lwd=2)
abline(v=seq(1,365,30))

#Big Three Combo

YearlyBigThreeCHO = YearlyMDCProportionsCHOMDC1 + YearlyMDCProportionsCHOMDC4 + YearlyMDCProportionsCHOMDC5
YearlyBigThreeCHO$doy <- YearlyMDCProportionsCHOMDC1$doy
plot(YearlyBigThreeCHO$CHO_mean_MDC1,type='l',main="CHO Average Daily Proportions Big Three MDC", ylim = c(0,75))
#abline(h=mean(YearlyMDCProportions$mean_MDC4))
abline(h=quantile(YearlyBigThreeCHO$CHO_mean_MDC1),col='blue')
lines(rollmean(YearlyBigThreeCHO$CHO_mean_MDC1,7),col='blue',lwd=2)
lines(rollmean(YearlyMDCProportionsCHOMDC1$CHO_mean_MDC1,7),col='blue',lwd=2)
lines(rollmean(YearlyMDCProportionsCHOMDC4$CHO_mean_MDC4,7),col='orange',lwd=2)
lines(rollmean(YearlyMDCProportionsCHOMDC5$CHO_mean_MDC5,7),col='green',lwd=2)
abline(v=seq(1,365,30))

#Statistical Analysis

#Seeing whether the proportions are different from general profile
t.test(minitableNoStress$Proportion, minitableHotDaysModerate$Proportion, paired = TRUE)
chisq.test(minitableNoStress$Proportion, minitableHotDaysModerate$Proportion, correct=FALSE)
chisq.test(minitableNoStress$Proportion, minitableColdDaysModerate$Proportion, correct=FALSE)
chisq.test(minitableHotDaysModerate$Proportion, minitableColdDaysModerate$Proportion, correct=FALSE)


#Run casecross for different MDC categories

model0 = casecross(CHO$MDC.04 ~ HotDaysModerate+OzoneREVISED+PM2.5REVISED, data=CHO)
model1 = casecross(CHO$MDC.04 ~ ColdDaysModerate+OzoneREVISED+PM2.5REVISED, data=CHO)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[7]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="CHO MDC.04 OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")

#MDC05
model0 = casecross(CHO$MDC.05 ~ HotDaysModerate+OzoneREVISED+PM2.5REVISED, data=CHO)
model1 = casecross(CHO$MDC.05 ~ ColdDaysModerate+OzoneREVISED+PM2.5REVISED, data=CHO)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[7]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="CHO MDC.05 OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")

#MDC01
model0 = casecross(CHO$MDC.01 ~ HotDaysModerate+OzoneREVISED+PM2.5REVISED, data=CHO)
model1 = casecross(CHO$MDC.01 ~ ColdDaysModerate+OzoneREVISED+PM2.5REVISED, data=CHO)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[7]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="CHO MDC.01 OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")

#Mort
model0 = casecross(CHO$Mort ~ HotDaysModerate+OzoneREVISED+PM2.5REVISED, data=CHO)
model1 = casecross(CHO$Mort ~ ColdDaysModerate+OzoneREVISED+PM2.5REVISED, data=CHO)

sum0 <- summary(model0)
coef0 = sum0[1]
se0 = sum0[7]

sum1 <- summary(model1)
coef1 = sum1[1]
se1 = sum1[7]

myORtable <- data.frame()
myORtable[1,1] = 0
myORtable[1,2] = sum0[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef0+(1.96*se0))
myORtable[1,4] = exp(coef0-(1.96*se0))
myORtable[2,1] = 1
myORtable[2,2] = sum1[1,2] #sum[3] = OR
myORtable[2,3] = exp(coef1+(1.96*se1))
myORtable[2,4] = exp(coef1-(1.96*se1))
colnames(myORtable) <- c("Model","OR","UpperCI","LowerCI")

ggplot(myORtable) +
  geom_point(aes(x=Model, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=Model, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="CHO Mort OR",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")+
  annotate(geom="text", x=0, y=1.08, label="Hot Days",color="black")+
  annotate(geom="text", x=1, y=1.08, label="Cold Days",color="black")


###### Create DLNMs for the MDCs
CHO$Trend <- seq(1,5844,1)
for(i in 1:ncol(CHO)){
  assign(names(CHO)[i], CHO[[i]])
}
ATC1pm <- as.numeric(CHO$`AT(C)1pm`)
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

#plot(pred1.ATC1pm,xlab="ATC1pm",zlab="RR",theta=200,phi=30,lphi=30,main="CHO MDC.04 Mortality")

# Plot of "heat map"
noeff=1
levels=pretty(pred1.ATC1pm$matRRfit,50)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.ATC1pm$predvar,y=seq(0,21,1),z=pred1.ATC1pm$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="ATC1pm" ,ylab="Lag (Days)",main="CHO MDC.04: ATC1pm"))

#plot(pred1.ATC1pm,"contour",key.title=title("RR"),plot.title=title("CHO MDC.04 Mortality",xlab="ATC1pm",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

plot(pred1.ATC1pm,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO MDC.04: RR vs. Lag (days) at 0°C (ATC1pm)",ylim=c(.8,1.1))
plot(pred1.ATC1pm,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO MDC.04: RR vs. Lag (days) at 40°C (ATC1pm)",ylim=c(.8,1.1))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.ATC1pm,"slices",var=c(0,10,25,40),lag=c(0,3,14,21),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.ATC1pm,"overall",xlab="Apparent Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="CHO MDC.04 Overall Cumulative Association")


###DLNM for MDC.05
modelMDC.05=glm(MDC.05~cb1.ATC1pm+ns(OzoneREVISED, 3)+ns(PM2.5REVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.05)
#plot(modelMDC.05)
#dispersiontest(modelMDC.05)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.ATC1pm=crosspred(cb1.ATC1pm,modelMDC.05,by=1,cen=(mean(ATC1pm,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.ATC1pm,xlab="ATC1pm",zlab="RR",theta=200,phi=30,lphi=30,main="CHO MDC.05 Mortality")

# Plot of "heat map"

noeff=1
levels=pretty(pred1.ATC1pm$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.ATC1pm$predvar,y=seq(0,21,1),z=pred1.ATC1pm$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="ATC1pm" ,ylab="Lag (Days)",main="CHO MDC.05: ATC1pm"))

#plot(pred1.ATC1pm,"contour",key.title=title("RR"),plot.title=title("CHO MDC.05 Mortality",xlab="ATC1pm",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.ATC1pm,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO: RR vs. Lag (days) at 10°C (ATC1pm)")
#plot(pred1.ATC1pm,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO: RR vs. Lag (days) at 30°C (ATC1pm)")

plot(pred1.ATC1pm,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO MDC.05: RR vs. Lag (days) at 0°C (ATC1pm)",ylim=c(.8,1.1))
plot(pred1.ATC1pm,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO MDC.05: RR vs. Lag (days) at 40°C (ATC1pm)",ylim=c(.8,1.1))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.ATC1pm,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.ATC1pm,"overall",xlab="Apparent Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="CHO MDC.05 Overall Cumulative Association")

###DLNM for MDC.01
modelMDC.01=glm(MDC.01~cb1.ATC1pm+ns(OzoneREVISED, 3)+ns(PM2.5REVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMDC.01)
#plot(modelMDC.01)
#dispersiontest(modelMDC.01)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.ATC1pm=crosspred(cb1.ATC1pm,modelMDC.01,by=1,cen=(mean(ATC1pm,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.ATC1pm,xlab="ATC1pm",zlab="RR",theta=200,phi=30,lphi=30,main="CHO MDC.01 Mortality")

# Plot of "heat map"

noeff=1
levels=pretty(pred1.ATC1pm$matRRfit,50)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.ATC1pm$predvar,y=seq(0,21,1),z=pred1.ATC1pm$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="ATC1pm" ,ylab="Lag (Days)",main="CHO MDC.01: ATC1pm"))

#plot(pred1.ATC1pm,"contour",key.title=title("RR"),plot.title=title("CHO MDC.01 Mortality",xlab="ATC1pm",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.ATC1pm,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO: RR vs. Lag (days) at 10°C (ATC1pm)")
#plot(pred1.ATC1pm,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO: RR vs. Lag (days) at 30°C (ATC1pm)")

plot(pred1.ATC1pm,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO MDC.01: RR vs. Lag (days) at 0°C (ATC1pm)",ylim=c(.8,1.2))
plot(pred1.ATC1pm,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO MDC.01: RR vs. Lag (days) at 40°C (ATC1pm)",ylim=c(.8,1.2))


# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.ATC1pm,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.ATC1pm,"overall",xlab="Apparent Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="CHO MDC.01 Overall Cumulative Association")


###DLNM for Mort
modelMort=glm(Mort~cb1.ATC1pm+ns(OzoneREVISED, 3)+ns(PM2.5REVISED, 3)+ns(Trend, 16*4),family=poisson(),lagframe)
summary(modelMort)
#plot(modelMort)
#dispersiontest(modelMort)

# Generate predictions from DLNM and plot results.  "cen" is the value of Max T used to center the plots, and "by" is the lag time step.

pred1.ATC1pm=crosspred(cb1.ATC1pm,modelMort,by=1,cen=(mean(ATC1pm,na.rm=TRUE)))

# 3-D plot (default.  See ?persp for details about plotting parameters)

#plot(pred1.ATC1pm,xlab="ATC1pm",zlab="RR",theta=200,phi=30,lphi=30,main="CHO Mort Mortality")

# Plot of "heat map"

noeff=1
levels=pretty(pred1.ATC1pm$matRRfit,100)
col1 <- colorRampPalette(c("blue","white"))
col2 <- colorRampPalette(c("white","red"))
col <- c(col1(sum(levels<noeff)),col2(sum(levels>noeff)))
filled.contour(x=pred1.ATC1pm$predvar,y=seq(0,21,1),z=pred1.ATC1pm$matRRfit,col=col,levels=levels,key.title=title("RR"),plot.title=title(xlab="ATC1pm" ,ylab="Lag (Days)",main="CHO Total Mortality"))

#plot(pred1.ATC1pm,"contour",key.title=title("RR"),plot.title=title("CHO Mort Mortality",xlab="ATC1pm",ylab="Lag (days)"))

# Plot of lag effects at selected values ("var=??")

#plot(pred1.ATC1pm,ptype="slices",var=c(10),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO: RR vs. Lag (days) at 10°C (ATC1pm)")
#plot(pred1.ATC1pm,ptype="slices",var=c(30),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO: RR vs. Lag (days) at 30°C (ATC1pm)")

plot(pred1.ATC1pm,ptype="slices",var=c(0),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO Mortality Total: RR vs. Lag (days) at 0°C (ATC1pm)",ylim=c(.8,1.2))
plot(pred1.ATC1pm,ptype="slices",var=c(40),col='black',xlab='lag (days)',ylab="Relative Risk",main="CHO Mortality Total: RR vs. Lag (days) at 40°C (ATC1pm)",ylim=c(.8,1.2))

# You can create a multi-panel figure showing slice by selected values of lag and of your basis variable.
#plot(pred1.ATC1pm,"slices",var=c(5,15,25,35),lag=c(0,7,14,20),ci.level=0.95)

# Plot of overall relationship for your basis variable totalled across all lags.  This is essentially a summary of the 3-D plot and heat map with the lag information collapsed.

plot(pred1.ATC1pm,"overall",xlab="Apparent Temperature (°C)",ylab="Relative Risk",lwd=3,ylim=c(0.5,1.5),main="CHO Total Mortality Overall Cumulative Association")



