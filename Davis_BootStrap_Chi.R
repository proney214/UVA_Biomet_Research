# Davis Research Idea Extend

library(readxl)
library(dplyr)
library(boot)

PHF <- read_excel("~/Desktop/PHF.Final.Weather.xlsx",na="NA")
PHF <- PHF[1:5844, ]

# Generate One Day Lag


###### Investigation

PHF %>% mutate_if(is.character, as.numeric)
PHF$HotDaysExtreme <- as.numeric(PHF$HotDaysExtreme)

PHFProfileMortleft <- PHF[ ,107:149]
PHFProfileMortright <- PHF[ ,155:160]
PHFProfileMort <- cbind(PHFProfileMortleft,PHFProfileMortright)
PHFProfileMort$Date <- PHF$Date 

PHFProfileMDC <- PHFProfileMort[ ,1:27]
PHFProfileHotCold <- PHF[ , 92:103]
PHF$Date <- as.Date(PHF$Date)
PHFselect <- cbind(PHFProfileHotCold,PHFProfileMort)

#Heat General Profile

PHFHotDaysSevere_lag0 <- PHFselect[PHFselect$HotDaysSevere == 1, ]
PHFHotDaysSevere_lag1 <- PHFselect[PHFselect$Lag1HotDaySevere == 1, ]
PHFHotDaysSevere_LagCombine <- rbind(PHFHotDaysSevere_lag0,PHFHotDaysSevere_lag1)
PHFHotDaysSevere_LagCombine_DupsRem <- PHFHotDaysSevere_LagCombine[!duplicated(PHFHotDaysSevere_LagCombine), ]
PHFHotDaysSevere_FinalRaw <- PHFHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

PHFHotDaysSevere <- PHFHotDaysSevere_FinalRaw[,13:39]

PHFHotDaysSevere[as.numeric(length((PHFHotDaysSevere$MDC.01)+1)), ] = colSums(PHFHotDaysSevere,na.rm=TRUE)
PHFHotDaysSevere[as.numeric(length((PHFHotDaysSevere$MDC.01)+1)), ]

tableprofileHotDaysSeverePHF = data.frame(colSums(PHFHotDaysSevere,na.rm = TRUE))
tableprofileHotDaysSeverePHF$Proportion = NA
minitableHotDaysSeverePHF <- tableprofileHotDaysSeverePHF[1:27, ]
minitableHotDaysSeverePHF[,2] <- (minitableHotDaysSeverePHF[,1] / sum(minitableHotDaysSeverePHF$colSums.PHFHotDaysSevere.)) * 100
minitableHotDaysSeverePHF$Proportion <- round(minitableHotDaysSeverePHF$Proportion, digits = 2)
sum(minitableHotDaysSeverePHF$Proportion)
#sum(minitable$colSums.PHFHotDaysSevere.)

#Cold General Profile

PHFColdDaysSevere_lag0 <- PHFselect[PHFselect$ColdDaysSevere == 1, ]
PHFColdDaysSevere_lag1 <- PHFselect[PHFselect$Lag1ColdDaySevere == 1, ]
PHFColdDaysSevere_LagCombine <- rbind(PHFColdDaysSevere_lag0,PHFColdDaysSevere_lag1)
PHFColdDaysSevere_LagCombine_DupsRem <- PHFColdDaysSevere_LagCombine[!duplicated(PHFColdDaysSevere_LagCombine), ]
PHFColdDaysSevere_FinalRaw <- PHFColdDaysSevere_LagCombine_DupsRem %>% arrange(Date)

PHFColdDaysSevere <- PHFColdDaysSevere_FinalRaw[,13:39]

PHFColdDaysSevere[as.numeric(length((PHFColdDaysSevere$MDC.01)+1)), ] = colSums(PHFColdDaysSevere,na.rm=TRUE)
PHFColdDaysSevere[as.numeric(length((PHFColdDaysSevere$MDC.01)+1)), ]

tableprofileColdDaysSeverePHF = data.frame(colSums(PHFColdDaysSevere,na.rm = TRUE))
tableprofileColdDaysSeverePHF$Proportion = NA
minitableColdDaysSeverePHF <- tableprofileColdDaysSeverePHF[1:27, ]
minitableColdDaysSeverePHF[,2] <- (minitableColdDaysSeverePHF[,1] / sum(minitableColdDaysSeverePHF$colSums.PHFColdDaysSevere.)) * 100
minitableColdDaysSeverePHF$Proportion <- round(minitableColdDaysSeverePHF$Proportion, digits = 2)
sum(minitableColdDaysSeverePHF$Proportion)
#sum(minitable$colSums.PHFColdDaysSevere.)

#No stress General Profile

PHFNoStress_FinalRaw <- PHFselect %>%
  filter(ColdDaysSevere != 1 & Lag1ColdDaySevere != 1 & HotDaysSevere != 1 & Lag1HotDaySevere != 1)
sum(PHFNoStress_FinalRaw$HotDaysSevere)
sum(PHFNoStress_FinalRaw$ColdDaysSevere)
sum(PHFNoStress_FinalRaw$Lag1ColdDaySevere)
sum(PHFNoStress_FinalRaw$Lag1HotDaySevere)

PHFNoStress <- PHFNoStress_FinalRaw[,13:39]

PHFNoStress[as.numeric(length((PHFNoStress$MDC.01)+1)), ] = colSums(PHFNoStress,na.rm=TRUE)
PHFNoStress[as.numeric(length((PHFNoStress$MDC.01)+1)), ]

tableprofileNoStressPHF = data.frame(colSums(PHFNoStress,na.rm = TRUE))
tableprofileNoStressPHF$Proportion = NA
minitableNoStressPHF <- tableprofileNoStressPHF[1:27, ]
minitableNoStressPHF[,2] <- (minitableNoStressPHF[,1] / sum(minitableNoStressPHF$colSums.PHFNoStress.,na.rm = TRUE)) * 100
minitableNoStressPHF$Proportion <- round(minitableNoStressPHF$Proportion, digits = 2)
sum(minitableNoStressPHF$Proportion)

####### BootStrapping

#Compute Tables
PHFBootTable <- data.frame(matrix(data = NA, nrow = 27, ncol = 1))
colnames(PHFBootTable) <- c("Bootstrap Means")
for (j in 1:27) {
  print(j)
  bootFunc <- function(PHFNoStress, i){
    df <- PHFNoStress[i, ]
    mean(df[, j])
    # )
  }
  b <- boot(PHFNoStress, bootFunc, R = 1000)
  PHFBootTable[j, 1] = mean(b$t)
  print(mean(b$t))
}

RawTable <- data.frame(matrix(data = NA, nrow = 27, ncol = 1))
colnames(RawTable) <- c("Raw Means")
for (j in 1:27) {
  print(j)
  RawTable[j,1] <- mean(PHFNoStress[,j],na.rm=TRUE)
}


check <- cbind(PHFBootTable,RawTable)
rownames(check) <- colnames(PHFNoStress)

PHFNoStressBoot <- PHFBootTable
for (i in (1:27)) {
  PHFNoStressBoot[i,2] <- ((PHFNoStressBoot[i,] / sum(PHFNoStressBoot$`Bootstrap Means`)) * 100)
}
colnames(PHFNoStressBoot) <- c("BootValues","BootProportion")
PHFNoStressBoot$BootProportion <- round(PHFNoStressBoot$BootProportion, digits = 2)
sum(PHFNoStressBoot$BootProportion)


# PHFNoStressBoot[as.numeric(length((PHFNoStressBoot$MDC.01)+1)), ] = colSums(PHFNoStressBoot,na.rm=TRUE)
# PHFNoStressBoot[as.numeric(length((PHFNoStressBoot$MDC.01)+1)), ]
# 
# tableprofileNoStressPHFBoot = data.frame(colSums(PHFNoStressBoot,na.rm = TRUE))
# tableprofileNoStressPHFBoot$Proportion = NA
# minitableNoStressPHFBoot <- tableprofileNoStressPHFBoot[1:27, ]
# minitableNoStressPHFBoot[,2] <- (minitableNoStressPHFBoot[,1] / sum(minitableNoStressPHFBoot$colSums.PHFNoStressBoot.,na.rm = TRUE)) * 100
# minitableNoStressPHFBoot$Proportion <- round(minitableNoStressPHFBoot$Proportion, digits = 2)
# sum(minitableNoStressPHFBoot$Proportion)



#Daily Proportions

PHFProfileMDC$Daily <- rowSums(PHFProfileMDC)
PHFProfileMDC <- data.frame(PHFProfileMDC)

DailyProportionsPHF <- (PHFProfileMDC/PHFProfileMDC[,28])*100
DailyProportionsPHF <- round(DailyProportionsPHF, digits = 2)

PHFStress <- PHFselect %>%
  filter(ColdDaysSevere == 1 | HotDaysSevere == 1)

#Statistical Analysis

#Seeing whether the proportions are different from general profile
#t.test(minitableNoStress$Proportion, minitableHotDaysSevere$Proportion, paired = TRUE)
PHFNvH <- chisq.test(PHFNoStressBoot$BootProportion, minitableHotDaysSeverePHF$Proportion, correct=FALSE)
PHFNvC <- chisq.test(PHFNoStressBoot$BootProportion, minitableColdDaysSeverePHF$Proportion, correct=FALSE)
PHFHvC <- chisq.test(minitableHotDaysSeverePHF$Proportion, minitableColdDaysSeverePHF$Proportion, correct=FALSE)

PHFNvH$p.value
PHFNvC$p.value
PHFHvC$p.value

#SAVING RESULTS
# Proportions
# ResultsTableNoStress <- data.frame(matrix(nrow=27,ncol=1))
# ResultsTableHotStress <- data.frame(matrix(nrow=27,ncol=1))
# ResultsTableColdStress <- data.frame(matrix(nrow=27,ncol=1))

ResultsTableNoStress$PHF <- minitableNoStressPHF$Proportion
ResultsTableHotStress$PHF <- minitableHotDaysSeverePHF$Proportion
ResultsTableColdStress$PHF <- minitableColdDaysSeverePHF$Proportion

PHFallproportionTable <- data.frame(cbind(minitableNoStressPHF$Proportion,minitableHotDaysSeverePHF$Proportion,minitableColdDaysSeverePHF$Proportion))
colnames(PHFallproportionTable) <- c("NoStressBoot", "HotStress","ColdStress")
rownames(PHFallproportionTable) <- rownames(minitableNoStressPHF)
PHFallproportionTable$DiffHotNoStress = PHFallproportionTable$HotStress - PHFallproportionTable$NoStressBoot
PHFallproportionTable$DiffNoStressCold = PHFallproportionTable$ColdStress - PHFallproportionTable$NoStressBoot
PHFallproportionTable$DiffHotCold = PHFallproportionTable$HotStress - PHFallproportionTable$ColdStress

#Chi-Square Values
# ChiResultsTableNoHotStress <- data.frame(matrix(nrow=1,ncol=1))
# ChiResultsTableNoColdStress <- data.frame(matrix(nrow=1,ncol=1))
# ChiResultsTableHotColdStress <- data.frame(matrix(nrow=1,ncol=1))

ChiResultsTableNoHotStress$PHF <- PHFNvH$p.value
ChiResultsTableNoColdStress$PHF <- PHFNvC$p.value
ChiResultsTableHotColdStress$PHF <- PHFHvC$p.value

