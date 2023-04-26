#Load in Data
library(splines)
library(dlnm)
library(lubridate)
library(readxl)
library(mgcv)
library(ggplot2)
library(stringr)
library(dplyr)
library(padr)

PHFF <- read_excel("~/Desktop/PHF.Working.Complete.xlsx",sheet = "Sheet1",na="NA")

#Compute variables quickly from after loading df,
for(i in 1:ncol(PHFF)){
  assign(names(PHFF)[i], PHFF[[i]])
}

#Remove Unnecessary for Variables
PHFfactors <- dplyr::select(PHFF,ColdDaysExtreme,ColdDaysModerate,ColdDaysSevere,ColdWavesExtreme,ColdWavesModerate,ColdWavesSevere,HotDaysExtreme,HotDaysModerate,HotDaysSevere,HeatWavesExtreme,HeatWavesModerate,HeatWavesSevere,holidays,dow,WinterWeather)
PHF <- dplyr::select(PHFF,-Station,-Date,-Year,-Month,-Day,-`MaxT(C)`,-`MaxTDep(C)`,-`MinT(C)`,-`MinTDep(C)`,-`DTR(C)`,-`T(C)1am`,-`Td(C)1am`,-`Tw(C)1am`,-`AT(C)1am`,-`THI(C)1am`,-`Hx(C)1am`,-`WC(C)1am`,-`T(C)7am`,-`Td(C)7am`,-`Tw(C)7am`,-`AT(C)7am`,-`THI(C)7am`,-`Hx(C)7am`,-`WC(C)7am`,-`T(C)1pm`,-`Td(C)1pm`,-`Tw(C)1pm`,-`AT(C)1pm`,-`THI(C)1pm`,-`Hx(C)1pm`,-`WC(C)1pm`,-`T(C)7pm`,-`Td(C)7pm`,-`Tw(C)7pm`,-`AT(C)7pm`,-`THI(C)7pm`,-`Hx(C)7pm`,-`WC(C)7pm`,-ColdDaysExtreme,-ColdDaysModerate,-ColdDaysSevere,-ColdWavesExtreme,-ColdWavesModerate,-ColdWavesSevere,-HotDaysExtreme,-HotDaysModerate,-HotDaysSevere,-HeatWavesExtreme,-HeatWavesModerate,-HeatWavesSevere,-holidays,-dow,-WinterWeather,-AdjustedMort,-Lag1,-Lag2,-Speedkts1am,-Speedkts1pm,-Speedkts7am,-Speedkts7pm,-WCF1am,-WCF1pm,-WCF7am,-WCF7pm,-ehPa1am,-ehPa1pm,-ehPa7am,-ehPa7pm,-esubshPa1am,-esubshPa1pm,-esubshPa7am,-esubshPa7pm)#,-SLPhPa1am,-SLPhPa1pm,-SLPhPa7am,-SLPhPa7p,#-Ozone,-PM2.5,-OzoneREVISED,-PM2.5REVISED)
PHFsplines <- dplyr::select(PHFF,RH1am,RH7am,RH1pm,RH7pm,TdF1am,TdF7am,TdF1pm,TdF7pm,TwF1am,TwF7am,TwF1pm,TwF7pm,TF1am,TF7am,TF1pm,TF7pm,HxF1am,HxF7am,HxF1pm,HxF7pm,ATF1am,ATF7am,ATF1pm,ATF7pm,Speedkts1am,Speedkts1pm,Speedkts7am,Speedkts7pm,WCF1am,WCF1pm,WCF7am,WCF7pm,ehPa1am,ehPa1pm,ehPa7am,ehPa7pm,esubshPa1am,esubshPa1pm,esubshPa7am,esubshPa7pm)#,SLPhPa1am,SLPhPa1pm,SLPhPa7am,SLPhPa7pm)#OzoneREVISED,PM2.5REVISED)

#Convert Data to numeric
PHF %>% mutate_if(is.character, as.numeric)
head(PHF)

#Customize your mortality

#Load in and prepare filter for dataset
BigMort_RACE_Full <- read_excel("~/Desktop/BigMort3.xlsx")
BigMortR <- BigMort_RACE_Full

BigMortR$Date = as.Date(paste(BigMortR$DOD_YR,BigMortR$DOD_MO,BigMortR$DOD_DY,sep="-"))
BigMortR$`Station ID` <- str_replace_all(BigMortR$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))
BigMortR$Count <- 1

#Single Station Filter command
PHFmortdf <- BigMortR %>%
  filter(BigMortR$`Station ID` == "PHF")

PHFmortdf <- dplyr::select(PHFmortdf,Count,Date,RACE,AGE_GROUPS)


PHFmortdf$RACE <- str_replace_all(PHFmortdf$RACE, c("Asian/PacificIslander"="Other",
                                                    "AmerInd/AlaskNat"="Other",
                                                    "Other"="Other"))

#Produce df for how many of each age group have died, look at station general mortalities
PHFdfT <- PHFmortdf %>%
  group_by(Date)%>%
  summarise(Total=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

PHFmort <- PHFdfT$Total

##Filter by Race, Make each df the proper length

PHFdf1 <- PHFmortdf %>%
  filter(RACE == 'White')%>%
  group_by(Date) %>%
  summarise(White=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))


PHFdf2 <- PHFmortdf %>%
  filter(RACE == 'Black') %>%
  group_by(Date) %>%
  summarise(Black=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

PHFdf3 <- PHFmortdf %>%
  filter(RACE == 'Other') %>%
  group_by(Date) %>%
  summarise(Other=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

PHFdf4 <- PHFmortdf %>%
  filter(AGE_GROUPS == '75+') %>%
  group_by(Date) %>%
  summarise(PHFmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

PHFdfMerged <- do.call("cbind", list(PHFdfT,PHFdf1,PHFdf2,PHFdf3,PHFdf4))
PHFdfwide <- PHFdfMerged[!duplicated(as.list(PHFdfMerged))]
PHFdfwide[is.na(PHFdfwide)] <- 0

#Set your mort (Total is column 2, White in 3, Black in 4, Others in 5, Elderly in 6)
mort <- PHFdfwide[3]
colnames(mort) <- "mort"

assign(names(mort)[1], mort[[1]])

#Create Trend Term
Trend = seq(1,5844,1)

#DLNM component
#Create a date variable needed in model ("datevar" is a "lubridate" function)
datevar=make_date(Year,Month,Day)

#Create a data frame with lagged variables using all variables in "final" GAM
lagframe=data.frame(mort=mort, Trend=Trend, MaxTF=MaxTF, datevar=datevar, OzoneREVISED=OzoneREVISED,PM2.5REVISED=PM2.5REVISED, holidays=holidays, ColdWavesSevere=ColdWavesSevere, HeatWavesSevere=HeatWavesSevere)

#Generate basis matrix for predictors and lags
df.time=4
time=crossbasis(as.numeric(as.Date(lagframe$datevar)),vartype="ns",vardf=df.time,cen=T,maxlag=00)
plot(time)

basis.MaxTF=crossbasis(lagframe$MaxTF,vartype="ns",vardf=3,cen=T,maxlag=21,lagtype="ns",lagdf=3,cenvalue=20)

#Define the position of knots for spline functions
varknotsMaxTF=equalknots(lagframe$MaxTF,fun="bs",df=3,degree=2)
varknotsMaxTF
lagknotsMaxTF=logknots(21,2)
lagknotsMaxTF

# Create the basis matrix; set number of lag ("lag=10 (days)"); cen = centering value
cb1.MaxTF=crossbasis(lagframe$MaxTF,lag=21,argvar=list(fun="bs",knots=varknotsMaxTF),arglag=list(knots=lagknotsMaxTF))
summary(cb1.MaxTF)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
modelA1=glm(mort~cb1.MaxTF+ns(PM2.5REVISED, 3)+ns(OzoneREVISED, 3)+ns(Trend, 16*3)+as.factor(holidays)+as.factor(ColdWavesSevere)+as.factor(HeatWavesSevere),family=quasipoisson(),lagframe)
summary(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTF=crosspred(cb1.MaxTF,modelA1,by=1)

#OVERALL PLOT
plot(pred1.MaxTF,"overall",xlab="MaxTF",y="Relative Risk",lwd=3,ylim=c(0.7,1.3),main="PHF White Overall Cumulative Association")

# Plot of "heat map"
plot(pred1.MaxTF,"contour",xlab="MaxTF",key.title=title("RR"),plot.title=title("PHF White Mortality",xlab="MaxTF",ylab="Lag (days)"))

