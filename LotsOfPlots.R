library(readxl)
library(dplyr)
library(lubridate)
#library(boot)

#MaxT

hist(CHOMortGreat$`MaxT(C)`,breaks = (max(CHOMortGreat$`MaxT(C)`,na.rm = TRUE)-min(CHOMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(EZFMortGreat$`MaxT(C)`,breaks = (max(EZFMortGreat$`MaxT(C)`,na.rm = TRUE)-min(EZFMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(EMVMortGreat$`MaxT(C)`,breaks = (max(EMVMortGreat$`MaxT(C)`,na.rm = TRUE)-min(EMVMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(IADMortGreat$`MaxT(C)`,breaks = (max(IADMortGreat$`MaxT(C)`,na.rm = TRUE)-min(IADMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(LYHMortGreat$`MaxT(C)`,breaks = (max(LYHMortGreat$`MaxT(C)`,na.rm = TRUE)-min(LYHMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(OKVMortGreat$`MaxT(C)`,breaks = (max(OKVMortGreat$`MaxT(C)`,na.rm = TRUE)-min(OKVMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(ORFMortGreat$`MaxT(C)`,breaks = (max(ORFMortGreat$`MaxT(C)`,na.rm = TRUE)-min(ORFMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(PHFMortGreat$`MaxT(C)`,breaks = (max(PHFMortGreat$`MaxT(C)`,na.rm = TRUE)-min(PHFMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(RICMortGreat$`MaxT(C)`,breaks = (max(RICMortGreat$`MaxT(C)`,na.rm = TRUE)-min(RICMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(ROAMortGreat$`MaxT(C)`,breaks = (max(ROAMortGreat$`MaxT(C)`,na.rm = TRUE)-min(ROAMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(SHDMortGreat$`MaxT(C)`,breaks = (max(SHDMortGreat$`MaxT(C)`,na.rm = TRUE)-min(SHDMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(VJIMortGreat$`MaxT(C)`,breaks = (max(VJIMortGreat$`MaxT(C)`,na.rm = TRUE)-min(VJIMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))



hist(CHOMortGreat$`MinT(C)`,breaks = (max(CHOMortGreat$`MinT(C)`,na.rm = TRUE)-min(CHOMortGreat$`MinT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(EZFMortGreat$`MinT(C)`,breaks = (max(EZFMortGreat$`MinT(C)`,na.rm = TRUE)-min(EZFMortGreat$`MinT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(EMVMortGreat$`MinT(C)`,breaks = (max(EMVMortGreat$`MinT(C)`,na.rm = TRUE)-min(EMVMortGreat$`MinT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(IADMortGreat$`MinT(C)`,breaks = (max(IADMortGreat$`MinT(C)`,na.rm = TRUE)-min(IADMortGreat$`MinT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(LYHMortGreat$`MinT(C)`,breaks = (max(LYHMortGreat$`MinT(C)`,na.rm = TRUE)-min(LYHMortGreat$`MinT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(OKVMortGreat$`MinT(C)`,breaks = (max(OKVMortGreat$`MinT(C)`,na.rm = TRUE)-min(OKVMortGreat$`MinT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(ORFMortGreat$`MinT(C)`,breaks = (max(ORFMortGreat$`MinT(C)`,na.rm = TRUE)-min(ORFMortGreat$`MinT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(PHFMortGreat$`MinT(C)`,breaks = (max(PHFMortGreat$`MinT(C)`,na.rm = TRUE)-min(PHFMortGreat$`MinT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(RICMortGreat$`MinT(C)`,breaks = (max(RICMortGreat$`MinT(C)`,na.rm = TRUE)-min(RICMortGreat$`MinT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(ROAMortGreat$`MinT(C)`,breaks = (max(ROAMortGreat$`MinT(C)`,na.rm = TRUE)-min(ROAMortGreat$`MinT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(SHDMortGreat$`MinT(C)`,breaks = (max(SHDMortGreat$`MinT(C)`,na.rm = TRUE)-min(SHDMortGreat$`MinT(C)`,na.rm = TRUE)),xlim = c(-15,45))
hist(VJIMortGreat$`MinT(C)`,breaks = (max(VJIMortGreat$`MinT(C)`,na.rm = TRUE)-min(VJIMortGreat$`MinT(C)`,na.rm = TRUE)),xlim = c(-15,45))


hist(CHOMortGreat$`MaxTDep(C)`,breaks = (max(CHOMortGreat$`MaxTDep(C)`,na.rm = TRUE)-min(CHOMortGreat$`MaxTDep(C)`,na.rm = TRUE)),xlim = c(-20,20))
hist(EZFMortGreat$`MaxTDep(C)`,breaks = (max(EZFMortGreat$`MaxTDep(C)`,na.rm = TRUE)-min(EZFMortGreat$`MaxTDep(C)`,na.rm = TRUE)),xlim = c(-20,20))
hist(EMVMortGreat$`MaxTDep(C)`,breaks = (max(EMVMortGreat$`MaxTDep(C)`,na.rm = TRUE)-min(EMVMortGreat$`MaxTDep(C)`,na.rm = TRUE)),xlim = c(-20,20))
hist(IADMortGreat$`MaxTDep(C)`,breaks = (max(IADMortGreat$`MaxTDep(C)`,na.rm = TRUE)-min(IADMortGreat$`MaxTDep(C)`,na.rm = TRUE)),xlim = c(-20,20))
hist(LYHMortGreat$`MaxTDep(C)`,breaks = (max(LYHMortGreat$`MaxTDep(C)`,na.rm = TRUE)-min(LYHMortGreat$`MaxTDep(C)`,na.rm = TRUE)),xlim = c(-20,20))
hist(OKVMortGreat$`MaxTDep(C)`,breaks = (max(OKVMortGreat$`MaxTDep(C)`,na.rm = TRUE)-min(OKVMortGreat$`MaxTDep(C)`,na.rm = TRUE)),xlim = c(-20,20))
hist(ORFMortGreat$`MaxTDep(C)`,breaks = (max(ORFMortGreat$`MaxTDep(C)`,na.rm = TRUE)-min(ORFMortGreat$`MaxTDep(C)`,na.rm = TRUE)),xlim = c(-20,20))
hist(PHFMortGreat$`MaxTDep(C)`,breaks = (max(PHFMortGreat$`MaxTDep(C)`,na.rm = TRUE)-min(PHFMortGreat$`MaxTDep(C)`,na.rm = TRUE)),xlim = c(-20,20))
hist(RICMortGreat$`MaxTDep(C)`,breaks = (max(RICMortGreat$`MaxTDep(C)`,na.rm = TRUE)-min(RICMortGreat$`MaxTDep(C)`,na.rm = TRUE)),xlim = c(-20,20))
hist(ROAMortGreat$`MaxTDep(C)`,breaks = (max(ROAMortGreat$`MaxTDep(C)`,na.rm = TRUE)-min(ROAMortGreat$`MaxTDep(C)`,na.rm = TRUE)),xlim = c(-20,20))
hist(SHDMortGreat$`MaxTDep(C)`,breaks = (max(SHDMortGreat$`MaxTDep(C)`,na.rm = TRUE)-min(SHDMortGreat$`MaxTDep(C)`,na.rm = TRUE)),xlim = c(-20,20))
hist(VJIMortGreat$`MaxTDep(C)`,breaks = (max(VJIMortGreat$`MaxTDep(C)`,na.rm = TRUE)-min(VJIMortGreat$`MaxTDep(C)`,na.rm = TRUE)),xlim = c(-20,20))


#Mort
hist(CHO$Mort,breaks = (max(CHO$Mort,na.rm = TRUE)-min(CHO$Mort,na.rm = TRUE)))
hist(EZF$Mort,breaks = (max(EZF$Mort,na.rm = TRUE)-min(EZF$Mort,na.rm = TRUE)))
hist(EMV$Mort,breaks = (max(EMV$Mort,na.rm = TRUE)-min(EMV$Mort,na.rm = TRUE)))
hist(IAD$Mort,breaks = (max(IAD$Mort,na.rm = TRUE)-min(IAD$Mort,na.rm = TRUE)))
hist(LYH$Mort,breaks = (max(LYH$Mort,na.rm = TRUE)-min(LYH$Mort,na.rm = TRUE)))
hist(OKV$Mort,breaks = (max(OKV$Mort,na.rm = TRUE)-min(OKV$Mort,na.rm = TRUE)))
hist(ORF$Mort,breaks = (max(ORF$Mort,na.rm = TRUE)-min(ORF$Mort,na.rm = TRUE)))
hist(PHF$Mort,breaks = (max(PHF$Mort,na.rm = TRUE)-min(PHF$Mort,na.rm = TRUE)))
hist(RIC$Mort,breaks = (max(RIC$Mort,na.rm = TRUE)-min(RIC$Mort,na.rm = TRUE)))
hist(ROA$Mort,breaks = (max(ROA$Mort,na.rm = TRUE)-min(ROA$Mort,na.rm = TRUE)))
hist(SHD$Mort,breaks = (max(SHD$Mort,na.rm = TRUE)-min(SHD$Mort,na.rm = TRUE)))
hist(VJI$Mort,breaks = (max(VJI$Mort,na.rm = TRUE)-min(VJI$Mort,na.rm = TRUE)))

quantile(CHO$Mort, probs = c(.8,.9,.95))
quantile(EMV$Mort, probs = c(.8,.9,.95))
quantile(EZF$Mort, probs = c(.8,.9,.95))
quantile(IAD$Mort, probs = c(.8,.9,.95))
quantile(LYH$Mort, probs = c(.8,.9,.95))
quantile(OKV$Mort, probs = c(.8,.9,.95))
quantile(ORF$Mort, probs = c(.8,.9,.95))
quantile(PHF$Mort, probs = c(.8,.9,.95))
quantile(RIC$Mort, probs = c(.8,.9,.95))
quantile(ROA$Mort, probs = c(.8,.9,.95))
quantile(SHD$Mort, probs = c(.8,.9,.95))
quantile(VJI$Mort, probs = c(.8,.9,.95))


# create a 2X2 grid
par(mfrow= c(4,3))

plot(CHO$Black, type='l',main="CHO Black Mortality")
abline(v = 3653, col="red")
plot(EMV$Black, type='l',main="EMV Black Mortality")
abline(v = 3653, col="red")
plot(EZF$Black, type='l',main="EZF Black Mortality")
abline(v = 3653, col="red")
plot(IAD$Black, type='l',main="IAD Black Mortality")
abline(v = 3653, col="red")
plot(LYH$Black, type='l',main="LYH Black Mortality")
abline(v = 3653, col="red")
plot(OKV$Black, type='l',main="OKV Black Mortality")
abline(v = 3653, col="red")
plot(ORF$Black, type='l',main="ORF Black Mortality")
abline(v = 3653, col="red")
plot(PHF$Black, type='l',main="PHF Black Mortality")
abline(v = 3653, col="red")
plot(RIC$Black, type='l',main="RIC Black Mortality")
abline(v = 3653, col="red")
plot(ROA$Black, type='l',main="ROA Black Mortality")
abline(v = 3653, col="red")
plot(SHD$Black, type='l',main="SHD Black Mortality")
abline(v = 3653, col="red")
plot(VJI$Black, type='l',main="VJI Black Mortality")
abline(v = 3653, col="red")

# create a 2X2 grid
par(mfrow= c(4,3))

plot(CHO$White, type='l',main="CHO White Mortality")
abline(v = 3653, col="red")
plot(EMV$White, type='l',main="EMV White Mortality")
abline(v = 3653, col="red")
plot(EZF$White, type='l',main="EZF White Mortality")
abline(v = 3653, col="red")
plot(IAD$White, type='l',main="IAD White Mortality")
abline(v = 3653, col="red")
plot(LYH$White, type='l',main="LYH White Mortality")
abline(v = 3653, col="red")
plot(OKV$White, type='l',main="OKV White Mortality") #
abline(v = 3653, col="red")
plot(ORF$White, type='l',main="ORF White Mortality") #
abline(v = 3653, col="red")
plot(PHF$White, type='l',main="PHF White Mortality")
abline(v = 3653, col="red")
plot(RIC$White, type='l',main="RIC White Mortality")
abline(v = 3653, col="red")
plot(ROA$White, type='l',main="ROA White Mortality")
abline(v = 3653, col="red")
plot(SHD$White, type='l',main="SHD White Mortality")
abline(v = 3653, col="red")
plot(VJI$White, type='l',main="VJI White Mortality") #
abline(v = 3653, col="red")

# create a 2X2 grid
par(mfrow= c(4,3))

plot(CHO$AsianPacificIslander, type='l',main="CHO AsianPacificIslander Mortality")
abline(v = 3653, col="red")
plot(EMV$AsianPacificIslander, type='l',main="EMV AsianPacificIslander Mortality")
abline(v = 3653, col="red")
plot(EZF$AsianPacificIslander, type='l',main="EZF AsianPacificIslander Mortality")
abline(v = 3653, col="red")
plot(IAD$AsianPacificIslander, type='l',main="IAD AsianPacificIslander Mortality")
abline(v = 3653, col="red")
plot(LYH$AsianPacificIslander, type='l',main="LYH AsianPacificIslander Mortality")
abline(v = 3653, col="red")
plot(OKV$AsianPacificIslander, type='l',main="OKV AsianPacificIslander Mortality") #
abline(v = 3653, col="red")
plot(ORF$AsianPacificIslander, type='l',main="ORF AsianPacificIslander Mortality") #
abline(v = 3653, col="red")
plot(PHF$AsianPacificIslander, type='l',main="PHF AsianPacificIslander Mortality")
abline(v = 3653, col="red")
plot(RIC$AsianPacificIslander, type='l',main="RIC AsianPacificIslander Mortality")
abline(v = 3653, col="red")
plot(ROA$AsianPacificIslander, type='l',main="ROA AsianPacificIslander Mortality")
abline(v = 3653, col="red")
plot(SHD$AsianPacificIslander, type='l',main="SHD AsianPacificIslander Mortality")
abline(v = 3653, col="red")
plot(VJI$AsianPacificIslander, type='l',main="VJI AsianPacificIslander Mortality") #
abline(v = 3653, col="red")


# create a 2X2 grid
par(mfrow= c(4,3))

plot(CHO$AmericanIndian.AlaskanNative, type='l',main="CHO AmericanIndian.AlaskanNative Mortality")
abline(v = 3653, col="red")
plot(EMV$AmericanIndian.AlaskanNative, type='l',main="EMV AmericanIndian.AlaskanNative Mortality")
abline(v = 3653, col="red")
plot(EZF$AmericanIndian.AlaskanNative, type='l',main="EZF AmericanIndian.AlaskanNative Mortality")
abline(v = 3653, col="red")
plot(IAD$AmericanIndian.AlaskanNative, type='l',main="IAD AmericanIndian.AlaskanNative Mortality")
abline(v = 3653, col="red")
plot(LYH$AmericanIndian.AlaskanNative, type='l',main="LYH AmericanIndian.AlaskanNative Mortality")
abline(v = 3653, col="red")
plot(OKV$AmericanIndian.AlaskanNative, type='l',main="OKV AmericanIndian.AlaskanNative Mortality") #
abline(v = 3653, col="red")
plot(ORF$AmericanIndian.AlaskanNative, type='l',main="ORF AmericanIndian.AlaskanNative Mortality") #
abline(v = 3653, col="red")
plot(PHF$AmericanIndian.AlaskanNative, type='l',main="PHF AmericanIndian.AlaskanNative Mortality")
abline(v = 3653, col="red")
plot(RIC$AmericanIndian.AlaskanNative, type='l',main="RIC AmericanIndian.AlaskanNative Mortality")
abline(v = 3653, col="red")
plot(ROA$AmericanIndian.AlaskanNative, type='l',main="ROA AmericanIndian.AlaskanNative Mortality")
abline(v = 3653, col="red")
plot(SHD$AmericanIndian.AlaskanNative, type='l',main="SHD AmericanIndian.AlaskanNative Mortality")
abline(v = 3653, col="red")
plot(VJI$AmericanIndian.AlaskanNative, type='l',main="VJI AmericanIndian.AlaskanNative Mortality") #
abline(v = 3653, col="red")



# create a 2X2 grid
par(mfrow= c(4,3))

plot(CHO$Unknown...146, type='l',main="CHO Mortality Unknown")
abline(v = 3653, col="red")
plot(EMV$Unknown...146, type='l',main="EMV Mortality Unknown")
abline(v = 3653, col="red")
plot(EZF$Unknown...146, type='l',main="EZF Mortality Unknown")
abline(v = 3653, col="red")
plot(IAD$Unknown...146, type='l',main="IAD Mortality Unknown")
abline(v = 3653, col="red")
plot(LYH$Unknown...146, type='l',main="LYH Mortality Unknown")
abline(v = 3653, col="red")
plot(OKV$Unknown...146, type='l',main="OKV Mortality Unknown") #
abline(v = 3653, col="red")
plot(ORF$Unknown...146, type='l',main="ORF Mortality Unknown") #
abline(v = 3653, col="red")
plot(PHF$Unknown...146, type='l',main="PHF Mortality Unknown")
abline(v = 3653, col="red")
plot(RIC$Unknown...146, type='l',main="RIC Mortality Unknown")
abline(v = 3653, col="red")
plot(ROA$Unknown...146, type='l',main="ROA Mortality Unknown")
abline(v = 3653, col="red")
plot(SHD$Unknown...146, type='l',main="SHD Mortality Unknown")
abline(v = 3653, col="red")
plot(VJI$Unknown...146, type='l',main="VJI Mortality Unknown") #
abline(v = 3653, col="red")

