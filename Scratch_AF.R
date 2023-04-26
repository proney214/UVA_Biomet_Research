#total Mort DLNM

 argvar_test <- list(fun="ns", knots = quantile(VA$`MaxT(C)`,c(10,75,90)/100, na.rm=T),
                Bound=range(quantile(VA$`MaxT(C)`,na.rm=T)))

argvar_P=list(fun="bs",knots=varknotsMaxTC_Mort)

# Now run best GAM model as GLM (DLNM uses GLM rather than GAM)
# This is the "best" model from GAMs
modelA1_Mort=glm(Mort~cb1.MaxTC_Mort+ns(Trend, 16*3)+as.factor(dow),family=quasipoisson(),lagframe_Mort)
summary(modelA1_Mort)
#plot(modelA1)

# Generate predictions from DLNM and plot results
pred1.MaxTC_Mort=crosspred(cb1.MaxTC_Mort,modelA1_Mort,by=1, cen = median(MaxTC,na.rm = T)) # set to median

#plot(pred1.MaxTC_Mort,"overall",col='black',xlab="Maximum Temperature (\u00B0C)",ylab="Relative Risk",main="VA Mort Lag RR", ylim = c(0.70,1.30),lwd=3)

red_P = crossreduce(cb1.MaxTC_Mort,pred1.MaxTC_Mort, cen = median(MaxTC,na.rm = T))
coef_P <- coef(red_P)

bvar_P = onebasis(x=MaxTC,fun="bs")
cenvec_P <- onebasis(x=median(MaxTC,na.rm = T),fun="bs")
bvarcen_P = scale(bvar_P,center = median(MaxTC,na.rm = T), scale = F)
length(cenvec_P) #3
length(bvar_P) #4

z_AF = 1 - exp(-bvarcen_P%*%coef_P)

z_an = (1 - exp(-bvarcen_P%*%coef_P)) * MortMortVA

test <- onebasis(x=tmeanproj[,j+1],argvar)

bvar <- do.call(onebasis,c(list(x=tmeanproj[,j+1]),argvar))
cenvec <- do.call(onebasis,c(list(x=cen),argvar))
bvarcen <- scale(bvar,center=cenvec,scale=F)
length(cenvec) #4
length(bvar) #4
