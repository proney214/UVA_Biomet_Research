#Personal Research
#Things to do: Chi Square Test
# CaseCross for Raw MDCs'

library(readxl)
library(dplyr)
library(zoo)
library(season)
library(splines)
library(dlnm)

VJI_Final_Weather <- read_excel("~/Desktop/VJI.Final.Weather.xlsx")
VJI <- VJI_Final_Weather[1:5844,]

VJI %>% mutate_if(is.character, as.numeric)
VJI$HotDaysExtreme <- as.numeric(VJI$HotDaysExtreme)

VJIProfileMort <- VJI[ ,107:149] 
VJIProfileMDC <- VJIProfileMort[ ,1:27]
VJIProfileHotCold <- VJI[ , 92:103]
VJI$date <- as.Date(VJI$Date)
VJIselect <- cbind(VJIProfileHotCold,VJIProfileMort)

#Heat General Profile

VJIHotDaysModerate <- VJIselect[VJIselect$HotDaysModerate == 1, ]
VJIHotDaysModerate[as.numeric(length((VJIHotDaysModerate$HotDaysModerate)+1)), ] = colSums(VJIHotDaysModerate,na.rm=TRUE)
VJIHotDaysModerate[as.numeric(length((VJIHotDaysModerate$HotDaysModerate)+1)), ]

tableprofileHotDaysModerateVJI = data.frame(colSums(VJIHotDaysModerate,na.rm = TRUE))
tableprofileHotDaysModerateVJI$Proportion = NA
minitableHotDaysModerateVJI <- tableprofileHotDaysModerateVJI[13:39, ]
minitableHotDaysModerateVJI[,2] <- (minitableHotDaysModerateVJI[,1] / sum(minitableHotDaysModerateVJI$colSums.VJIHotDaysModerate.)) * 100
minitableHotDaysModerateVJI$Proportion <- round(minitableHotDaysModerateVJI$Proportion, digits = 2)
sum(minitableHotDaysModerateVJI$Proportion)
#sum(minitable$colSums.VJIHotDaysModerate.)

#Cold General Profile

VJIColdDaysModerate <- VJIselect[VJIselect$ColdDaysModerate == 1, ]
VJIColdDaysModerate[as.numeric(length((VJIColdDaysModerate$ColdDaysModerate)+1)), ] = colSums(VJIColdDaysModerate)

tableprofileColdDaysModerateVJI = data.frame(colSums(VJIColdDaysModerate,na.rm = TRUE))
tableprofileColdDaysModerateVJI$Proportion = NA
minitableColdDaysModerateVJI <- tableprofileColdDaysModerateVJI[13:39, ]
minitableColdDaysModerateVJI[,2] <- (minitableColdDaysModerateVJI[,1] / sum(minitableColdDaysModerateVJI$colSums.VJIColdDaysModerate.)) * 100
minitableColdDaysModerateVJI$Proportion <- round(minitableColdDaysModerateVJI$Proportion, digits = 2)
sum(minitableColdDaysModerateVJI$Proportion)

#No stress General Profile

VJINoStress <- VJIselect %>%
  filter(ColdDaysModerate != 1 & HotDaysModerate != 1)
sum(VJINoStress$HotDaysModerate)

tableprofileNoStressVJI = data.frame(colSums(VJINoStress))
tableprofileNoStressVJI$Proportion = NA
minitableNoStressVJI <- tableprofileNoStressVJI[13:39, ]
minitableNoStressVJI[,2] <- (minitableNoStressVJI[,1] / sum(minitableNoStressVJI$colSums.VJINoStress.)) * 100
minitableNoStressVJI$Proportion <- round(minitableNoStressVJI$Proportion, digits = 2)
sum(minitableNoStressVJI$Proportion)

#Daily Proportions

VJIProfileMDC$Daily <- rowSums(VJIProfileMDC)
VJIProfileMDC <- data.frame(VJIProfileMDC)

DailyProportionsVJI <- (VJIProfileMDC/VJIProfileMDC[,28])*100
DailyProportionsVJI <- round(DailyProportionsVJI, digits = 2)

VJIStress <- VJIselect %>%
  filter(ColdDaysModerate == 1 | HotDaysModerate == 1)

#Statistical Analysis

#Seeing whether the proportions are different from general profile
#t.test(minitableNoStress$Proportion, minitableHotDaysModerate$Proportion, paired = TRUE)
chisq.test(minitableNoStressVJI$Proportion, minitableHotDaysModerateVJI$Proportion, correct=FALSE)
chisq.test(minitableNoStressVJI$Proportion, minitableColdDaysModerateVJI$Proportion, correct=FALSE)
chisq.test(minitableHotDaysModerateVJI$Proportion, minitableColdDaysModerateVJI$Proportion, correct=FALSE)

