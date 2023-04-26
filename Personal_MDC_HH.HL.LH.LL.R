# Davis FRESH START Research Idea Extend HH,HL,LH,LL

library(readxl)
library(dplyr)
library(lubridate)
#library(boot)

CHO <- read_excel("~/Desktop/CHO.Final.Weather.xlsx",na="NA")
CHO <- CHO[1:5844, ]

# Generate One Day Lag in excel


###### Investigation

CHO %>% mutate_if(is.character, as.numeric)
CHO$HotDaysExtreme <- as.numeric(CHO$HotDaysExtreme)

CHOProfileMortleft <- CHO[ ,107:149]
CHOProfileMortright <- CHO[ ,161:165]
CHOProfileMort <- cbind(CHOProfileMortleft,CHOProfileMortright)
CHOProfileMort$Date <- CHO$Date 
rm(CHOProfileMortleft,CHOProfileMortright)

CHOProfileMDC <- CHOProfileMort[ ,1:27]
CHOProfileHotCold <- CHO[ , 161:165]
CHO$Date <- as.Date(CHO$Date)
#CHOselect <- cbind(CHOProfileHotCold,CHOProfileMort)
CHOselect <- CHOProfileMort
rm(CHOProfileHotCold,CHOProfileMort)

#High Heat High Mortality

CHOHotDaysSevere_lag0 <- CHOselect[CHOselect$HeatLag0Update == 1, ]
CHOHotDaysSevere_lag1 <- CHOselect[CHOselect$HeatLag1Update == 1, ]
CHOHotDaysSevere_MortModerate <- CHOselect[CHOselect$MortModerate == 1, ] #THIS IS NEW
CHOHotDaysSevere_MortSevere <- CHOselect[CHOselect$MortSevere == 1, ] #THIS IS NEW
CHOHotDaysSevere_MortExtreme <- CHOselect[CHOselect$MortExtreme == 1, ] #THIS IS NEW
CHOHotDaysSevere_LagCombine <- rbind(CHOHotDaysSevere_lag0,CHOHotDaysSevere_lag1,CHOHotDaysSevere_MortModerate,CHOHotDaysSevere_MortSevere,CHOHotDaysSevere_MortExtreme)
CHOHotDaysSevere_LagCombine_DupsRem <- CHOHotDaysSevere_LagCombine[!duplicated(CHOHotDaysSevere_LagCombine), ]
CHOHotDaysUpdate <- CHOHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

rm(CHOHotDaysSevere_lag0,CHOHotDaysSevere_lag1,CHOHotDaysSevere_LagCombine,CHOHotDaysSevere_LagCombine_DupsRem,CHOHotDaysSevere_MortModerate,CHOHotDaysSevere_MortSevere,CHOHotDaysSevere_MortExtreme)


CHOHotDaysSevere <- CHOHotDaysUpdate[,c(1:27,44:49)]

CHOHotDaysSevere_SameMonths <- CHOHotDaysSevere %>%
  filter(month(Date) %in% c(4,5,6, 7, 8, 9))

CHOHotDaysSevere_SameMonths$HighMort_HighHeat_Lag0 <- ifelse(CHOHotDaysSevere_SameMonths$HeatLag0Update == 1 & CHOHotDaysSevere_SameMonths$MortModerate == 1, 1, 0)
CHOHotDaysSevere_SameMonths$HighMort_HighHeat_Lag1 <- ifelse(CHOHotDaysSevere_SameMonths$HeatLag1Update == 1 & CHOHotDaysSevere_SameMonths$MortModerate == 1, 1, 0)
CHOHotDaysSevere_SameMonths$HighMort_HighHeat_Final <- ifelse(CHOHotDaysSevere_SameMonths$HighMort_HighHeat_Lag0 == 1 | CHOHotDaysSevere_SameMonths$HighMort_HighHeat_Lag1 == 1, 1, 0)

CHOHotDaysSevere_SameMonths$HighMort_NoHeat_Lag0 <- ifelse(CHOHotDaysSevere_SameMonths$HeatLag0Update == 0 & CHOHotDaysSevere_SameMonths$MortModerate == 1, 1, 0)
CHOHotDaysSevere_SameMonths$HighMort_NoHeat_Lag1 <- ifelse(CHOHotDaysSevere_SameMonths$HeatLag1Update == 0 & CHOHotDaysSevere_SameMonths$MortModerate == 1, 1, 0)
CHOHotDaysSevere_SameMonths$HighMort_NoHeat_Final <- ifelse(CHOHotDaysSevere_SameMonths$HighMort_NoHeat_Lag0 == 1 | CHOHotDaysSevere_SameMonths$HighMort_NoHeat_Lag0 == 1, 1, 0)


CHOHotDaysSevere_SameMonths$ModMort_HighHeat_Lag0 <- ifelse(CHOHotDaysSevere_SameMonths$HeatLag0Update == 1 & CHOHotDaysSevere_SameMonths$MortModerate == 0, 1, 0)
CHOHotDaysSevere_SameMonths$ModMort_HighHeat_Lag1 <- ifelse(CHOHotDaysSevere_SameMonths$HeatLag1Update == 1 & CHOHotDaysSevere_SameMonths$MortModerate == 0, 1, 0)
CHOHotDaysSevere_SameMonths$ModMort_HighHeat_Final <- ifelse(CHOHotDaysSevere_SameMonths$ModMort_HighHeat_Lag0 == 1 | CHOHotDaysSevere_SameMonths$ModMort_HighHeat_Lag1 == 1, 1, 0)

CHOHotDaysSevere_SameMonths$ModMort_NoHeat_Lag0 <- ifelse(CHOHotDaysSevere_SameMonths$HeatLag0Update == 0 & CHOHotDaysSevere_SameMonths$MortModerate == 0, 1, 0)
CHOHotDaysSevere_SameMonths$ModMort_NoHeat_Lag1 <- ifelse(CHOHotDaysSevere_SameMonths$HeatLag1Update == 0 & CHOHotDaysSevere_SameMonths$MortModerate == 0, 1, 0)
CHOHotDaysSevere_SameMonths$ModMort_NoHeat_Final <- ifelse(CHOHotDaysSevere_SameMonths$ModMort_NoHeat_Lag0 == 1 | CHOHotDaysSevere_SameMonths$ModMort_NoHeat_Lag0 == 1, 1, 0)


CHOHotDay_HighMort_HighHeat_ShortList <- CHOHotDaysSevere_SameMonths[CHOHotDaysSevere_SameMonths$HighMort_HighHeat_Final == 1, ]
CHOHotDay_HighMort_NoHeat_ShortList <- CHOHotDaysSevere_SameMonths[CHOHotDaysSevere_SameMonths$HighMort_NoHeat_Final == 1, ]
CHOHotDay_ModMort_HighHeat_ShortList <- CHOHotDaysSevere_SameMonths[CHOHotDaysSevere_SameMonths$ModMort_HighHeat_Final == 1, ]
CHOHotDay_ModMort_NoHeat_ShortList <- CHOHotDaysSevere_SameMonths[CHOHotDaysSevere_SameMonths$ModMort_NoHeat_Final == 1, ]

CHOMDCCounts_Heat_ModMortTable <- data.frame(colSums(CHOHotDay_HighMort_HighHeat_ShortList[,1:27],na.rm=TRUE),
                                          colSums(CHOHotDay_HighMort_NoHeat_ShortList[,1:27],na.rm=TRUE),
                                          colSums(CHOHotDay_ModMort_HighHeat_ShortList[,1:27],na.rm=TRUE),
                                          colSums(CHOHotDay_ModMort_NoHeat_ShortList[,1:27],na.rm=TRUE))
colnames(CHOMDCCounts_Heat_ModMortTable) <- c("HighMort_HighHeat","HighMort_NoHeat","ModMort_HighHeat","ModMort_NoHeat")


CHOMDCCounts_Heat_ModMortTable_RATE <- data.frame(colSums(CHOHotDay_HighMort_HighHeat_ShortList[,1:27]/length(CHOHotDay_HighMort_HighHeat_ShortList$MDC.01),na.rm=TRUE),
                                                 colSums(CHOHotDay_HighMort_NoHeat_ShortList[,1:27]/length(CHOHotDay_HighMort_NoHeat_ShortList$MDC.01),na.rm=TRUE),
                                                 colSums(CHOHotDay_ModMort_HighHeat_ShortList[,1:27]/length(CHOHotDay_ModMort_HighHeat_ShortList$MDC.01),na.rm=TRUE),
                                                 colSums(CHOHotDay_ModMort_NoHeat_ShortList[,1:27]/length(CHOHotDay_ModMort_NoHeat_ShortList$MDC.01),na.rm=TRUE))
colnames(CHOMDCCounts_Heat_ModMortTable_RATE) <- c("HighMort_HighHeat","HighMort_NoHeat","ModMort_HighHeat","ModMort_NoHeat")
CHOMDCCounts_Heat_ModMortTable_RATE <- round(CHOMDCCounts_Heat_ModMortTable_RATE,digits = 1)

# library(tidyr)
# test <- data.frame(t(CHOMDCCounts_Heat_MortTable))
# CHOMDCCounts_ggplotready <- pivot_longer(test, 
#                                          cols = c(1:27),
#                                          names_to = "Group",
#                                          values_to = "Count")
# ggplot(data=CHOMDCCounts_Heat_MortTable, aes(x=HighMort_HighHeat, y=HighMort_HighHeat)) +
#   geom_bar(stat="identity")
# 
# library(reshape2)
# CHOMDCCounts_Heat_MortTable$row.names<-rownames(CHOMDCCounts_Heat_MortTable)
# long.CHOMDCCounts_Heat_MortTable<-melt(CHOMDCCounts_Heat_MortTable,id=c("row.names"))
# ggplot(long.CHOMDCCounts_Heat_MortTable,aes(x=row.names,y=variable,color=value))+geom_point()
# 
# 
