# Davis Research Idea

library(readxl)
library(dplyr)

CHO <- read_excel("~/Desktop/CHO.Final.Weather.xlsx",na="NA")
CHO <- CHO[1:5844, ]

###### Investigation

CHO %>% mutate_if(is.character, as.numeric)
CHO$HotDaysExtreme <- as.numeric(CHO$HotDaysExtreme)

CHOProfileMortleft <- CHO[ ,107:149]
CHOProfileMortright <- CHO[ ,155:157]
CHOProfileMort <- cbind(CHOProfileMortleft,CHOProfileMortright)
CHOProfileMort$Date <- CHO$Date 

CHOProfileMDC <- CHOProfileMort[ ,1:27]
CHOProfileHotCold <- CHO[ , 92:103]
CHO$Date <- as.Date(CHO$Date)
CHOselect <- cbind(CHOProfileHotCold,CHOProfileMort)

#Heat General Profile

CHOHotDaysModerate_lag0 <- CHOselect[CHOselect$HotDaysModerate == 1, ]
CHOHotDaysModerate_lag1 <- CHOselect[CHOselect$Lag1HotDayModerate == 1, ]
CHOHotDaysModerate_LagCombine <- rbind(CHOHotDaysModerate_lag0,CHOHotDaysModerate_lag1)
CHOHotDaysModerate_LagCombine_DupsRem <- CHOHotDaysModerate_LagCombine[!duplicated(CHOHotDaysModerate_LagCombine), ]
CHOHotDaysModerate_FinalRaw <- CHOHotDaysModerate_LagCombine_DupsRem %>% arrange(Date)

CHOHotDaysModerate <- CHOHotDaysModerate_FinalRaw[,13:39]

CHOHotDaysModerate[as.numeric(length((CHOHotDaysModerate$MDC.01)+1)), ] = colSums(CHOHotDaysModerate,na.rm=TRUE)
CHOHotDaysModerate[as.numeric(length((CHOHotDaysModerate$MDC.01)+1)), ]

tableprofileHotDaysModerateCHO = data.frame(colSums(CHOHotDaysModerate,na.rm = TRUE))
tableprofileHotDaysModerateCHO$Proportion = NA
minitableHotDaysModerateCHO <- tableprofileHotDaysModerateCHO[1:27, ]
minitableHotDaysModerateCHO[,2] <- (minitableHotDaysModerateCHO[,1] / sum(minitableHotDaysModerateCHO$colSums.CHOHotDaysModerate.)) * 100
minitableHotDaysModerateCHO$Proportion <- round(minitableHotDaysModerateCHO$Proportion, digits = 2)
sum(minitableHotDaysModerateCHO$Proportion)
#sum(minitable$colSums.CHOHotDaysModerate.)

#Cold General Profile

CHOColdDaysModerate <- CHOselect[CHOselect$ColdDaysModerate == 1, ]
CHOColdDaysModerate[as.numeric(length((CHOColdDaysModerate$ColdDaysModerate)+1)), ] = colSums(CHOColdDaysModerate)

tableprofileColdDaysModerateCHO = data.frame(colSums(CHOColdDaysModerate,na.rm = TRUE))
tableprofileColdDaysModerateCHO$Proportion = NA
minitableColdDaysModerateCHO <- tableprofileColdDaysModerateCHO[13:39, ]
minitableColdDaysModerateCHO[,2] <- (minitableColdDaysModerateCHO[,1] / sum(minitableColdDaysModerateCHO$colSums.CHOColdDaysModerate.)) * 100
minitableColdDaysModerateCHO$Proportion <- round(minitableColdDaysModerateCHO$Proportion, digits = 2)
sum(minitableColdDaysModerateCHO$Proportion)

#No stress General Profile

CHONoStress <- CHOselect %>%
  filter(ColdDaysModerate != 1 & HotDaysModerate != 1)
sum(CHONoStress$HotDaysModerate)

tableprofileNoStressCHO = data.frame(colSums(CHONoStress))
tableprofileNoStressCHO$Proportion = NA
minitableNoStressCHO <- tableprofileNoStressCHO[13:39, ]
minitableNoStressCHO[,2] <- (minitableNoStressCHO[,1] / sum(minitableNoStressCHO$colSums.CHONoStress.)) * 100
minitableNoStressCHO$Proportion <- round(minitableNoStressCHO$Proportion, digits = 2)
sum(minitableNoStressCHO$Proportion)

#Daily Proportions

CHOProfileMDC$Daily <- rowSums(CHOProfileMDC)
CHOProfileMDC <- data.frame(CHOProfileMDC)

DailyProportionsCHO <- (CHOProfileMDC/CHOProfileMDC[,28])*100
DailyProportionsCHO <- round(DailyProportionsCHO, digits = 2)

CHOStress <- CHOselect %>%
  filter(ColdDaysModerate == 1 | HotDaysModerate == 1)

#Statistical Analysis

#Seeing whether the proportions are different from general profile
#t.test(minitableNoStress$Proportion, minitableHotDaysModerate$Proportion, paired = TRUE)
chisq.test(minitableNoStressCHO$Proportion, minitableHotDaysModerateCHO$Proportion, correct=FALSE)
chisq.test(minitableNoStressCHO$Proportion, minitableColdDaysModerateCHO$Proportion, correct=FALSE)
chisq.test(minitableHotDaysModerateCHO$Proportion, minitableColdDaysModerateCHO$Proportion, correct=FALSE)




