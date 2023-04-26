library(season)
# cardiovascular disease data
data(CVDdaily)
CVDdaily = subset(CVDdaily, date<=as.Date('1987-12-31')) # subset for example

CVDdaily$o3mean.10 <- CVDdaily$o3mean/10
CVDdaily$tmpd.5 <- CVDdaily$tmpd/5

model <- casecross(cvd ~ o3mean.10 + tmpd.5 + Mon + Tue + Wed + Thu + Fri + Sat, data = CVDdaily, stratalength = 28, exclusion = 4)
model
summary(model)

model1 <- casecross(cvd ~ o3mean.10 + tmpd.5, data = CVDdaily, matchdow = T, stratalength = 28, exclusion = 4)
model1
summary(model1)

model2 <- casecross(cvd ~ o3mean.10 + Mon + Tue + Wed + Thu + Fri + Sat, matchconf = 'tmpd', confrange = 1, data = CVDdaily, stratalength = 28, exclusion = 4)
model2
summary(model2)

model3 <- casecross(cvd ~ o3mean.10 + winter + spring + summer + autumn + Mon + Tue + Wed + Thu + Fri + Sat , data = CVDdaily, stratalength = 28, exclusion = 4)
model3
summary(model3)


# Effect of ozone on CVD death
model1 = casecross(cvd ~ o3mean+tmpd+Mon+Tue+Wed+Thu+Fri+Sat, data=CVDdaily)
summary(model1)
sum <- summary(model1)
coef = sum[1]
se = sum[3]

myORtable <- data.frame()
myORtable[1,1] = 1
myORtable[1,2] = sum[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef+(1.96*se))
myORtable[1,4] = exp(coef-(1.96*se))
colnames(myORtable) <- c("StrataLength","ORex0","UpperCIex0","LowerCIex0")

# ggplot(myORtable) +
#   geom_point(aes(x=StrataLength, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
#   geom_errorbar(aes(x=StrataLength, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
#   labs(title="SHD Cold Waves Extreme",y="Odds Ratio")+
#   geom_hline(yintercept=1, linetype="dashed", color = "red")

# match on day of the week
model2 = casecross(cvd ~ o3mean+tmpd, matchdow=TRUE, data=CVDdaily)
summary(model2)
# match on temperature to within a degree
model3 = casecross(cvd ~ o3mean+Mon+Tue+Wed+Thu+Fri+Sat, data=CVDdaily,
                   matchconf='tmpd', confrange=1)
summary(model3)
sum <- summary(model3)
coef = sum[1]
se = sum[3]

myORtable <- data.frame()
myORtable[1,1] = 1
myORtable[1,2] = sum[1,2] #sum[3] = OR
myORtable[1,3] = exp(coef+se)
myORtable[1,4] = exp(coef-se)
colnames(myORtable) <- c("StrataLength","ORex0","UpperCIex0","LowerCIex0")

ggplot(myORtable) +
  geom_point(aes(x=StrataLength, y=myORtable[,2]), stat="identity", fill="skyblue", alpha=1, size=2) +
  geom_errorbar(aes(x=StrataLength, ymin=myORtable[,3], ymax=myORtable[,4]), width=0.4, colour="blue", alpha=0.5, size=1.0)+
  labs(title="SHD Cold Waves Extreme",y="Odds Ratio")+
  geom_hline(yintercept=1, linetype="dashed", color = "red")
