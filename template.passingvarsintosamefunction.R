#Load in Data
library(splines)
library(dlnm)
library(lubridate)
library(readxl)
library(mgcv)
library(ggplot2)
library(dplyr)
RIC <- read_excel("~/Desktop/RIC.gam.test.xlsx", na="NA")
#RIC <- select(RIC,-MinT)

#Convert Data to numeric
RIC %>% mutate_if(is.character, as.numeric)
head(RIC)

#Create Trend Term
Trend = seq(1,5844,1)

#Compute variables quickly from after loading df, 
#this step is not necessary because our new model check will allow you to create a variable to use quite easily in the DLNM by doing RIC$DTR
#However, if you want to save yourself the time you can always just convert all the columns into variables using this below command
#Again, the below command is completely up to you
for(i in 1:ncol(RIC)){
  assign(names(RIC)[i], RIC[[i]])
}

#Test number of splines for Trend
modeltrend1=gam(mort~s(Trend,k=16*2),family="quasipoisson")
A=summary(modeltrend1)
plot(modeltrend1)
plot.gam(modeltrend1)

modeltrend2=gam(mort~s(Trend,k=16*3),family="quasipoisson")
B=summary(modeltrend2)
gam.check(modeltrend2)
plot(modeltrend2)

modeltrend3=gam(mort~s(Trend,k=16*4),family="quasipoisson")
C=summary(modeltrend3)
gam.check(modeltrend3)
plot(modeltrend3)

modeltrend4=gam(mort~s(Trend,k=16*5),family="quasipoisson")
D=summary(modeltrend4)
gam.check(modeltrend4)
plot(modeltrend4)

modeltrend5=gam(mort~s(Trend,k=16*7),family="quasipoisson")
E=summary(modeltrend5)
gam.check(modeltrend5)
plot(modeltrend5)

GAMbase <-matrix(c(A$sp.criterion,B$sp.criterion,C$sp.criterion,D$sp.criterion,E$sp.criterion,A$dev.expl,B$dev.expl,C$dev.expl,D$dev.expl,E$dev.expl),ncol=5,byrow=TRUE)
colnames(GAMbase)<-c('Trend k=2','k=3','k=3','k=5','k=7')
rownames(GAMbase)<-c('GCV','Dev.Exp.')
GAMbase<-as.table(GAMbase) 
head(GAMbase)


#Run every column of dataset through the model
f <- function(col,RIC) {
  form = as.formula(paste0("mort~s(Trend,k=3)+s(",col,",k=3)"))
  data.frame(col = col, gcv = gam(form, data = RIC, family="quasipoisson")$gcv)
}

#Compile outputs of model table into list of GCV values
t <- do.call(rbind, lapply(c("MaxT","MinT", "MaxTDepF","MinTDepF","TwF1am","Speedkts1am","ehPa1am","DTR"), f, RIC=RIC))
#t <- do.call(rbind, lapply(c("MaxTF","MinTF","MaxTDepF","MinTDepF","DTRF","TF1am","TwF1am","TdF1am","RH1am","Speedkts1am","SLPhPa1am","ehPa1am","esubshPa1am","ATF1am","THIF1am","HxF1am","WCF1am","TF7am","TwF7am","TdF7am","RH7am","Speedkts7am","SLPhPa7am","ehPa7am","esubshPa7am","ATF7am","THIF7am","HxF7am","WCF7am","TF1pm","TwF1pm","TdF1pm","RH1pm","Speedkts1pm","SLPhPa1pm","ehPa1pm","esubshPa1pm","ATF1pm","THIF1pm","HxF1pm","WCF1pm","TF7pm","TwF7pm","TdF7pm","RH7pm","Speedkts7pm","SLPhPa7pm","ehPa7pm","esubshPa7pm","ATF7pm","THIF7pm","HxF7pm","WCF7pm","mort","Ozone","PM2.5"), f, RIC=RIC))


#Find minimum GCV and variable associated with it
UseThisVariable <- t[which.min(t$gcv),]
View(UseThisVariable)

