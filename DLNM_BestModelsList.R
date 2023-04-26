### Best Models List

#EMV
#Black
model1d=gam(Mort~s(Trend,k=16*3)+s(MinTC,k=3)+as.factor(dow)+as.factor(HeatWavesModerate)+s(RH1pm,k=3),family="quasipoisson")
model1d=gam(Mort~s(Trend,k=16*3)+s(MinTC,k=3)+as.factor(dow)+as.factor(HeatWavesModerate)+s(RH1pm,k=3),family="poisson")
dispersiontest(model1d) #poisson
AIC(model1d)

#White
model1d=gam(Mort~s(Trend,k=16*2)+s(MinTC,k=3)+as.factor(dow)+as.factor(holidays)+as.factor(WinterWeather)+s(RH1pm,k=3),family="quasipoisson")
summary(model1d)
model1d=gam(Mort~s(Trend,k=16*2)+s(MinTC,k=3)+as.factor(dow)+as.factor(holidays)+as.factor(WinterWeather)+s(RH1pm,k=3),family="poisson")
dispersiontest(model1d) #quasipoisson

##EZF
#Black
model1a=gam(Mort~s(Trend,k=16*1)+s(MinTC,k=3)+as.factor(dow)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")
summary(model1a)
model1a=gam(Mort~s(Trend,k=16*1)+s(MinTC,k=3)+as.factor(dow)+as.factor(holidays)+as.factor(WinterWeather),family="poisson")
summary(model1a)
dispersiontest(model1a)
AIC(model1a)

#White
model1a=gam(Mort~s(Trend,k=16*3)+s(MinTC,k=3)+as.factor(dow)+as.factor(HeatWavesModerate),family="quasipoisson")
summary(model1a)
model1a=gam(Mort~s(Trend,k=16*3)+s(MinTC,k=3)+as.factor(dow)+as.factor(HeatWavesModerate),family="poisson")
summary(model1a)
dispersiontest(model1a)
AIC(model1a)

#PHF
#Black
model1i=gam(Mort~s(Trend,k=16*3)+as.factor(PHF$AccountChange2015)+s(ATC1pm,k=3)+s(PM2.5REVISED,k=3),family="quasipoisson")
model1i=gam(Mort~s(Trend,k=16*3)+as.factor(PHF$AccountChange2015)+s(ATC1pm,k=3)+s(PM2.5REVISED,k=3),family="poisson")
dispersiontest(model1i)
AIC(model1i)
summary(model1i)

#White
model1i=gam(Mort~s(Trend,k=16*2)+as.factor(PHF$AccountChange2015)+s(MinTC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate)+s(PM2.5REVISED,k=3),family="quasipoisson")
model1i=gam(Mort~s(Trend,k=16*2)+as.factor(PHF$AccountChange2015)+s(MinTC,k=3)+as.factor(dow)+as.factor(ColdWavesModerate)+s(PM2.5REVISED,k=3),family="poisson")
dispersiontest(model1i)
AIC(model1i)
summary(model1i)

# RIC
#Black
model1i=gam(Mort~s(Trend,k=16*3)+as.factor(RIC$AccountChange2015)+s(TC7pm,k=3)+s(PM2.5REVISED,k=3),family="quasipoisson")
model1i=gam(Mort~s(Trend,k=16*3)+as.factor(RIC$AccountChange2015)+s(TC7pm,k=3)+s(PM2.5REVISED,k=3),family="poisson")
dispersiontest(model1i)

#White
model1i=gam(Mort~s(Trend,k=16*3)+as.factor(RIC$AccountChange2015)+s(ATC1pm,k=3)+s(RH1pm,k=3)+as.factor(ColdWavesModerate)+as.factor(WinterWeather),family="quasipoisson")

#SHD
#Black
model1a=gam(Mort~s(Trend,k=16*3)+as.factor(SHD$AccountChange2015)+s(MaxTC,k=3),family="quasipoisson")
model1a=gam(Mort~s(Trend,k=16*3)+as.factor(SHD$AccountChange2015)+s(MaxTC,k=3),family="poisson")
dispersiontest(model1a) #Failed to reject null
AIC(model1a)
summary(model1a)

#White
model1a=gam(Mort~s(Trend,k=16*3)+as.factor(SHD$AccountChange2015)+s(MaxTC,k=3),family="quasipoisson")
model1a=gam(Mort~s(Trend,k=16*3)+as.factor(SHD$AccountChange2015)+s(MaxTC,k=3),family="poisson")
dispersiontest(model1a) #Failed to reject null
AIC(model1a)
summary(model1a)

#VJI
#Black
model1a=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3),family="quasipoisson")
model1a=gam(Mort~s(Trend,k=16*3)+as.factor(VJI$AccountChange2015)+s(MaxTC,k=3),family="poisson")
dispersiontest(model1a)
AIC(model1a)
summary(model1a)


#White
model1d=gam(Mort~s(Trend,k=16*2)+as.factor(VJI$AccountChange2015)+s(TC7pm,k=3)+as.factor(ColdWavesModerate)+s(RH1pm,k=3),family="quasipoisson")
model1d=gam(Mort~s(Trend,k=16*2)+as.factor(VJI$AccountChange2015)+s(TC7pm,k=3)+as.factor(ColdWavesModerate)+s(RH1pm,k=3),family="poisson")
dispersiontest(model1d)
AIC(model1d)
summary(model1d)

#Virginia (State Data)
#Black
model=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+s(RH1pm,k=3)+as.factor(dow)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")

#White
model=gam(Mort~s(Trend,k=16*3)+s(DTRC,k=3)+as.factor(dow)+as.factor(holidays)+as.factor(WinterWeather),family="quasipoisson")


