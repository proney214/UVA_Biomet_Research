# Run GAMs to test degrees of freedom in trend term
model1a=gam(mort~s(Trend,k=16*3)+s(MaxT,k=3),family="quasipoisson")
AA=summary(model1a)
plot(model1a)

model1b=gam(mort~s(Trend,k=16*3)+s(AT1300,k=3),family="quasipoisson")
BB=summary(model1b)
plot(model1b)

model1c=gam(mort~s(Trend,k=16*3)+s(T1300,k=3)+s(V1300,k=3),family="quasipoisson")
CC=summary(model1c)
plot(model1c)

model1d=gam(mort~s(Trend,k=16*3)+s(T1300,k=3)+s(RH1300,k=3),family="quasipoisson")
DD=summary(model1d)
plot(model1d)

model1e=gam(mort~s(Trend,k=16*3)+s(Hx1300,k=3),family="quasipoisson")
EE=summary(model1e)
plot(model1e)

model1f=gam(mort~s(Trend,k=16*3)+s(DTR,k=3),family="quasipoisson")
FF=summary(model1f)
plot(model1f)

model1g=gam(mort~s(Trend,k=16*3)+s(MinT,k=3),family="quasipoisson")
GG=summary(model1g)
plot(model1g)

model1h=gam(mort~s(Trend,k=16*3)+s(T1300,k=3)+s(Tw1300,k=3),family="quasipoisson")
HH=summary(model1h)
plot(model1h)

model1L=gam(mort~s(Trend,k=16*3)+s(WC1300,k=3),family="quasipoisson")
LL=summary(model1L)
plot(model1L)

GAMdof <-matrix(c(AA$sp.criterion,BB$sp.criterion,CC$sp.criterion,DD$sp.criterion,EE$sp.criterion,FF$sp.criterion,GG$sp.criterion,HH$sp.criterion,LL$sp.criterion,AA$dev.expl,BB$dev.expl,CC$dev.expl,DD$dev.expl,EE$dev.expl,FF$dev.expl,GG$dev.expl,HH$dev.expl,LL$dev.expl),ncol=9,byrow=TRUE)
colnames(GAMdof)<-c('MaxT','AT1300','T1300,V1300','T1300,RH1300','HX1300','DTR','MinT','T1300,Tw1300','WC1300')
rownames(GAMdof)<-c('GCV','Dev.Exp.')
GAMdof<-as.table(GAMdof) 
head(GAMdof)