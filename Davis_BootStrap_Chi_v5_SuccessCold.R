#ULTIMATE FINAL BOOTSTRAP CHI-SQUARE SCRIPT for COLD WEATHER

library(readxl)
library(dplyr)
library(lubridate)

MDC_FullFinal_NAs <- read.csv("Personal_MDC_ChiSqMortData.csv")
MDC_FullFinal <- MDC_FullFinal_NAs[!is.na(MDC_FullFinal_NAs$MDC_ACME),]
rm(MDC_FullFinal_NAs)
# NAOnly <- subset(MDC_FullFinal, MDC_FullFinal$MDC_ACME == NA) #0 obs. so yay!
# NAOnly <- MDC_FullFinal[is.na(MDC_FullFinal$MDC_ACME), ]

#CHO

CHO <- read_excel("~/Desktop/CHO.Final.Weather.xlsx",na="NA")
CHOselect <- CHO[1:5844, ]

CHOColdDaysSevere_lag0 <- CHOselect[CHOselect$ColdLag0 == 1, ]
CHOColdDaysSevere_lag1 <- CHOselect[CHOselect$ColdLag1 == 1, ]
CHOColdDaysSevere_lag2 <- CHOselect[CHOselect$ColdLag2 == 1, ]
CHOColdDaysSevere_lag3 <- CHOselect[CHOselect$ColdLag3 == 1, ]
CHOColdDaysSevere_lag4 <- CHOselect[CHOselect$ColdLag4 == 1, ]
CHOColdDaysSevere_lag5 <- CHOselect[CHOselect$ColdLag5 == 1, ]
CHOColdDaysSevere_lag6 <- CHOselect[CHOselect$ColdLag6 == 1, ]

CHOColdDaysSevere_LagCombine <- rbind(CHOColdDaysSevere_lag0,CHOColdDaysSevere_lag1,CHOColdDaysSevere_lag2,CHOColdDaysSevere_lag3,CHOColdDaysSevere_lag4,CHOColdDaysSevere_lag5,CHOColdDaysSevere_lag6)
CHOColdDaysSevere_LagCombine_DupsRem <- CHOColdDaysSevere_LagCombine[!duplicated(CHOColdDaysSevere_LagCombine), ]
CHOColdDays <- CHOColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

CHOColdDays_Winter <- CHOColdDays[!(3 < month(CHOColdDays$Date) & month(CHOColdDays$Date) < 10), ]
CHOColdDays_Winter$Date_Update <- format(as.Date(CHOColdDays_Winter$Date), "%Y-%m-%d")

rm(CHOColdDaysSevere_lag0,CHOColdDaysSevere_lag1,CHOColdDaysSevere_lag2,CHOColdDaysSevere_lag3,CHOColdDaysSevere_lag4,CHOColdDaysSevere_lag5,CHOColdDaysSevere_lag6,CHOColdDaysSevere_LagCombine,CHOColdDaysSevere_LagCombine_DupsRem,CHOColdDays)

CHO_MDC <- MDC_FullFinal %>%
  filter(Wx == "CHO")

CHO_MDC_Cold_NAs <- left_join(CHOColdDays_Winter, 
                             CHO_MDC,
                             by = c("Date_Update" = "Date"))
CHO_MDC_Cold <- CHO_MDC_Cold_NAs[!is.na(CHO_MDC_Cold_NAs$MDC_ACME),]
#NAOnly_Cold <- CHO_MDC_Cold[is.na(CHO_MDC_Cold$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(CHO_MDC_Cold$ColdLag0 == 0 & CHO_MDC_Cold$ColdLag1 == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(CHO_MDC_Cold$Station) / length(CHO_MDC$Date) #  8.74% of deaths happen on Cold Days
length(CHOColdDays_Winter$Station) / length(CHO$Station) # 8.322% of days are Cold day lag0 or lag1

####### BootStrapping CHO

CHONotColdDaysSevere_lag0 <- CHOselect[!(CHOselect$ColdLag0 == 1), ]
CHONotColdDaysSevere_lag1 <- CHOselect[!(CHOselect$ColdLag1 == 1), ]
CHONotColdDaysSevere_lag2 <- CHOselect[!(CHOselect$ColdLag2 == 1), ]
CHONotColdDaysSevere_lag3 <- CHOselect[!(CHOselect$ColdLag3 == 1), ]
CHONotColdDaysSevere_lag4 <- CHOselect[!(CHOselect$ColdLag4 == 1), ]
CHONotColdDaysSevere_lag5 <- CHOselect[!(CHOselect$ColdLag5 == 1), ]
CHONotColdDaysSevere_lag6 <- CHOselect[!(CHOselect$ColdLag6 == 1), ]

CHONotColdDaysSevere_LagCombine <- rbind(CHONotColdDaysSevere_lag0,CHONotColdDaysSevere_lag1,CHONotColdDaysSevere_lag2,CHONotColdDaysSevere_lag3,CHONotColdDaysSevere_lag4,CHONotColdDaysSevere_lag5,CHONotColdDaysSevere_lag6)
CHONotColdDaysSevere_LagCombine_DupsRem <- CHONotColdDaysSevere_LagCombine[!duplicated(CHONotColdDaysSevere_LagCombine), ]
CHONotColdDays <- CHONotColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

CHONotColdDays_Winter <- CHONotColdDays[!(3 < month(CHONotColdDays$Date) & month(CHONotColdDays$Date) < 10), ]
CHONotColdDays_Winter$Date_Update <- format(as.Date(CHONotColdDays_Winter$Date), "%Y-%m-%d")

rm(CHONotColdDaysSevere_lag0,CHONotColdDaysSevere_lag1,CHONotColdDaysSevere_lag2,CHONotColdDaysSevere_lag3,CHONotColdDaysSevere_lag4,CHONotColdDaysSevere_lag5,CHONotColdDaysSevere_lag6,CHONotColdDaysSevere_LagCombine,CHONotColdDaysSevere_LagCombine_DupsRem,CHONotColdDays)

#test <- CHONotColdDays_Winter[CHONotColdDays_Winter$Month == 3, ]

CHO_MDC_NotCold_NAs <- left_join(CHONotColdDays_Winter, 
                                CHO_MDC,
                                by = c("Date_Update" = "Date"))
CHO_MDC_NotCold <- CHO_MDC_NotCold_NAs[!is.na(CHO_MDC_NotCold_NAs$MDC_ACME),]
#NAOnly_NotCold <- CHO_MDC_NotCold[is.na(CHO_MDC_NotCold$MDC_ACME), ] #0 obs. so yay!


CHO_MDC_NotCold_Slim <- CHO_MDC_NotCold %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotCold_Slim <- CHO_MDC_NotCold_Slim[is.na(CHO_MDC_NotCold_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
 # for (i in (1:1000)) {
 #   print(i)
 #   CHONoStressRandom <- CHO_MDC_NotCold_Slim %>%
 #     sample_n(length(CHO_MDC_Cold$Month),replace = TRUE)
 #   write.csv(CHONoStressRandom,paste0("~/Desktop/NoStressRandomWx/CHO_v3_Cold/CHO",i))
 # }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

CHO1 <- read.csv("~/Desktop/NoStressRandomWx/CHO_v3_Cold/CHO1")
CHO2 <- read.csv("~/Desktop/NoStressRandomWx/CHO_v3_Cold/CHO2")
ifelse(length(CHO1$X) == length(CHO_MDC_Cold$Month),1,0)
rm(CHO1,CHO2)

CHO_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/CHO_v3_Cold/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#CHO_data_all                                            # Print data to RStudio console

# Group by MDC

CHO_MDC_NotCold_Counts_InitialGrouping <- CHO_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
CHO_MDC_NotCold_Counts_InitialGrouping$Adjusted <- CHO_MDC_NotCold_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/CHO_v3_Cold/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
CHO_MDC_NotCold_Counts <- data.frame(cbind(CHO_MDC_NotCold_Counts_InitialGrouping$MDC_ACME,CHO_MDC_NotCold_Counts_InitialGrouping$Adjusted))

colnames(CHO_MDC_NotCold_Counts) <- c("Boot_MDC_ACME","Boot_Count")
CHO_MDC_NotCold_Counts$Boot_Count <- as.numeric(CHO_MDC_NotCold_Counts$Boot_Count)
sum(CHO_MDC_NotCold_Counts$Boot_Count)

#CHO_MDC_NotCold_Counts[CHO_MDC_NotCold_Counts$MDCAcme == "MDC22", 2] <- 0

CHO_MDC_Cold_Counts <- CHO_MDC_Cold %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# CHO_MDC_Cold_Counts[(length(CHO_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# CHO_MDC_Cold_Counts[(length(CHO_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# CHO_MDC_Cold_Counts[(length(CHO_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# CHO_MDC_Cold_Counts[(length(CHO_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# CHO_MDC_Cold_Counts[(length(CHO_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# CHO_MDC_Cold_Counts[(length(CHO_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# CHO_MDC_Cold_Counts[(length(CHO_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# CHO_MDC_Cold_Counts[(length(CHO_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)

rm(CHO_data_all,CHO_MDC_Cold_NAs,CHO_MDC_NotCold_NAs,CHO_MDC_NotCold_Slim)

#Final Prep for Comparison
CHO_Ordered_NotCold_Counts <- CHO_MDC_NotCold_Counts[order(CHO_MDC_NotCold_Counts$Boot_MDC_ACME), ]
CHO_Ordered_Cold_Counts <- CHO_MDC_Cold_Counts[order(CHO_MDC_Cold_Counts$MDC_ACME), ]

#CHO_Final_Cold <- CHO_Ordered_Cold_Counts[CHO_Ordered_Cold_Counts$count_by_siteyear > 5, ]
CHO_Ordered_NotCold_Expected_Counts <- CHO_Ordered_NotCold_Counts[CHO_Ordered_NotCold_Counts$Boot_Count > 5, ]

# ifelse(length(CHO_NotCold_Counts_Ordered$Boot_MDC_ACME) == length(CHO_Cold_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(CHO_NotCold_Counts_Ordered$Boot_Count) == sum(CHO_Cold_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackCHO <- cbind(CHO_NotCold_Counts_Ordered,CHO_Cold_Counts_Ordered)
CountsOnlyCHO <- right_join(CHO_Ordered_Cold_Counts , CHO_Ordered_NotCold_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackCHO <- CountsOnlyCHO[!is.na(CountsOnlyCHO$Boot_Count) | !is.na(CountsOnlyCHO$count_by_siteyear),]
rm(CountsOnlyCHO)

sum(BackToBackCHO$count_by_siteyear)
sum(BackToBackCHO$Boot_Count)

colnames(BackToBackCHO) <- c("MDC_ACME","Cold","Boot")

BackToBackCHO$Boot <- as.numeric(BackToBackCHO$Boot)
sum(BackToBackCHO$Boot)
sum(BackToBackCHO$Cold)

BackToBackCHO_GreaterThanZero_Cold <- BackToBackCHO[BackToBackCHO$Boot > 0, ]

sum(BackToBackCHO_GreaterThanZero_Cold$Cold)
sum(BackToBackCHO_GreaterThanZero_Cold$Boot)

EO2_CHO <- (BackToBackCHO_GreaterThanZero_Cold$Cold - BackToBackCHO_GreaterThanZero_Cold$Boot)^2
E_CHO <- BackToBackCHO_GreaterThanZero_Cold$Boot
EO2E_CHO <- EO2_CHO/E_CHO
sumEO2E_CHO <- round(sum(EO2E_CHO),digits = 2)
sum(EO2E_CHO)
df_CHO <- length(EO2_CHO) - 1
#CritValue_df18 = 28.869
#CritValue_df19 = 30.144
#CritValue_df20 = 31.410
#CritValue_df21 = 32.671

# CSByHandTable <- data.frame(matrix(nrow=1,ncol=1))
# CSByHandTable$CHOCSValue <- sumEO2E_CHO

EO_Diff_CHO <- (BackToBackCHO_GreaterThanZero_Cold$Cold - BackToBackCHO_GreaterThanZero_Cold$Boot)
STD_RES_CHO <- EO_Diff_CHO / (sqrt(BackToBackCHO_GreaterThanZero_Cold$Boot))
BackToBackCHO_GreaterThanZero_Cold$Res <- round(STD_RES_CHO,digits = 3)
BackToBackCHO_GreaterThanZero_Cold$ResSig <- ifelse(abs(BackToBackCHO_GreaterThanZero_Cold$Res) > 3.8, TRUE, print("F"))

#EMV

EMV <- read_excel("~/Desktop/EMV.Final.Weather.xlsx",na="NA")
EMVselect <- EMV[1:5844, ]

EMVColdDaysSevere_lag2 <- EMVselect[EMVselect$ColdLag2 == 1, ]
EMVColdDaysSevere_lag3 <- EMVselect[EMVselect$ColdLag3 == 1, ]
EMVColdDaysSevere_lag4 <- EMVselect[EMVselect$ColdLag4 == 1, ]
EMVColdDaysSevere_lag5 <- EMVselect[EMVselect$ColdLag5 == 1, ]
EMVColdDaysSevere_lag6 <- EMVselect[EMVselect$ColdLag6 == 1, ]

EMVColdDaysSevere_LagCombine <- rbind(EMVColdDaysSevere_lag2,EMVColdDaysSevere_lag3,EMVColdDaysSevere_lag4,EMVColdDaysSevere_lag5,EMVColdDaysSevere_lag6)
EMVColdDaysSevere_LagCombine_DupsRem <- EMVColdDaysSevere_LagCombine[!duplicated(EMVColdDaysSevere_LagCombine), ]
EMVColdDays <- EMVColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

EMVColdDays_Winter <- EMVColdDays[!(3 < month(EMVColdDays$Date) & month(EMVColdDays$Date) < 10), ]
EMVColdDays_Winter$Date_Update <- format(as.Date(EMVColdDays_Winter$Date), "%Y-%m-%d")

rm(EMVColdDaysSevere_lag2,EMVColdDaysSevere_lag3,EMVColdDaysSevere_lag4,EMVColdDaysSevere_lag5,EMVColdDaysSevere_lag6,EMVColdDaysSevere_LagCombine,EMVColdDaysSevere_LagCombine_DupsRem,EMVColdDays)

EMV_MDC <- MDC_FullFinal %>%
  filter(Wx == "EMV")

EMV_MDC_Cold_NAs <- left_join(EMVColdDays_Winter, 
                              EMV_MDC,
                              by = c("Date_Update" = "Date"))
EMV_MDC_Cold <- EMV_MDC_Cold_NAs[!is.na(EMV_MDC_Cold_NAs$MDC_ACME),]
#NAOnly_Cold <- EMV_MDC_Cold[is.na(EMV_MDC_Cold$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(EMV_MDC_Cold$ColdLag0 == 0 & EMV_MDC_Cold$ColdLag1 == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(EMV_MDC_Cold$Station) / length(EMV_MDC$Date) #  8.74% of deaths happen on Cold Days
length(EMVColdDays_Winter$Station) / length(EMV$Station) # 8.322% of days are Cold day lag0 or lag1

####### BootStrapping EMV

EMVNotColdDaysSevere_lag2 <- EMVselect[!(EMVselect$ColdLag2 == 1), ]
EMVNotColdDaysSevere_lag3 <- EMVselect[!(EMVselect$ColdLag3 == 1), ]
EMVNotColdDaysSevere_lag4 <- EMVselect[!(EMVselect$ColdLag4 == 1), ]
EMVNotColdDaysSevere_lag5 <- EMVselect[!(EMVselect$ColdLag5 == 1), ]
EMVNotColdDaysSevere_lag6 <- EMVselect[!(EMVselect$ColdLag6 == 1), ]

EMVNotColdDaysSevere_LagCombine <- rbind(EMVNotColdDaysSevere_lag2,EMVNotColdDaysSevere_lag3,EMVNotColdDaysSevere_lag4,EMVNotColdDaysSevere_lag5,EMVNotColdDaysSevere_lag6)
EMVNotColdDaysSevere_LagCombine_DupsRem <- EMVNotColdDaysSevere_LagCombine[!duplicated(EMVNotColdDaysSevere_LagCombine), ]
EMVNotColdDays <- EMVNotColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

EMVNotColdDays_Winter <- EMVNotColdDays[!(3 < month(EMVNotColdDays$Date) & month(EMVNotColdDays$Date) < 10), ]
EMVNotColdDays_Winter$Date_Update <- format(as.Date(EMVNotColdDays_Winter$Date), "%Y-%m-%d")

rm(EMVNotColdDaysSevere_lag2,EMVNotColdDaysSevere_lag3,EMVNotColdDaysSevere_lag4,EMVNotColdDaysSevere_lag5,EMVNotColdDaysSevere_lag6,EMVNotColdDaysSevere_LagCombine,EMVNotColdDaysSevere_LagCombine_DupsRem,EMVNotColdDays)

#test <- EMVNotColdDays_Winter[EMVNotColdDays_Winter$Month == 3, ]

EMV_MDC_NotCold_NAs <- left_join(EMVNotColdDays_Winter, 
                                 EMV_MDC,
                                 by = c("Date_Update" = "Date"))
EMV_MDC_NotCold <- EMV_MDC_NotCold_NAs[!is.na(EMV_MDC_NotCold_NAs$MDC_ACME),]
#NAOnly_NotCold <- EMV_MDC_NotCold[is.na(EMV_MDC_NotCold$MDC_ACME), ] #0 obs. so yay!


EMV_MDC_NotCold_Slim <- EMV_MDC_NotCold %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotCold_Slim <- EMV_MDC_NotCold_Slim[is.na(EMV_MDC_NotCold_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   EMVNoStressRandom <- EMV_MDC_NotCold_Slim %>%
#     sample_n(length(EMV_MDC_Cold$Month),replace = TRUE)
#   write.csv(EMVNoStressRandom,paste0("~/Desktop/NoStressRandomWx/EMV_v3_Cold/EMV",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

EMV1 <- read.csv("~/Desktop/NoStressRandomWx/EMV_v3_Cold/EMV1")
EMV2 <- read.csv("~/Desktop/NoStressRandomWx/EMV_v3_Cold/EMV2")
ifelse(length(EMV1$X) == length(EMV_MDC_Cold$Month),1,0)
rm(EMV1,EMV2)

EMV_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/EMV_v3_Cold/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#EMV_data_all                                            # Print data to RStudio console

# Group by MDC

EMV_MDC_NotCold_Counts_InitialGrouping <- EMV_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
EMV_MDC_NotCold_Counts_InitialGrouping$Adjusted <- EMV_MDC_NotCold_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/EMV_v3_Cold/",  # Identify all CSV files
                                                                                                                                 pattern = "*", full.names = TRUE))) # Divide by number i
EMV_MDC_NotCold_Counts <- data.frame(cbind(EMV_MDC_NotCold_Counts_InitialGrouping$MDC_ACME,EMV_MDC_NotCold_Counts_InitialGrouping$Adjusted))

colnames(EMV_MDC_NotCold_Counts) <- c("Boot_MDC_ACME","Boot_Count")
EMV_MDC_NotCold_Counts$Boot_Count <- as.numeric(EMV_MDC_NotCold_Counts$Boot_Count)
sum(EMV_MDC_NotCold_Counts$Boot_Count)

#EMV_MDC_NotCold_Counts[EMV_MDC_NotCold_Counts$MDCAcme == "MDC22", 2] <- 0

EMV_MDC_Cold_Counts <- EMV_MDC_Cold %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# EMV_MDC_Cold_Counts[(length(EMV_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# EMV_MDC_Cold_Counts[(length(EMV_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# EMV_MDC_Cold_Counts[(length(EMV_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# EMV_MDC_Cold_Counts[(length(EMV_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# EMV_MDC_Cold_Counts[(length(EMV_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# EMV_MDC_Cold_Counts[(length(EMV_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# EMV_MDC_Cold_Counts[(length(EMV_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# EMV_MDC_Cold_Counts[(length(EMV_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)

rm(EMV_data_all,EMV_MDC_Cold_NAs,EMV_MDC_NotCold_NAs,EMV_MDC_NotCold_Slim)

#Final Prep for Comparison
EMV_Ordered_NotCold_Counts <- EMV_MDC_NotCold_Counts[order(EMV_MDC_NotCold_Counts$Boot_MDC_ACME), ]
EMV_Ordered_Cold_Counts <- EMV_MDC_Cold_Counts[order(EMV_MDC_Cold_Counts$MDC_ACME), ]

#EMV_Final_Cold <- EMV_Ordered_Cold_Counts[EMV_Ordered_Cold_Counts$count_by_siteyear > 5, ]
EMV_Ordered_NotCold_Expected_Counts <- EMV_Ordered_NotCold_Counts[EMV_Ordered_NotCold_Counts$Boot_Count > 5, ]

# ifelse(length(EMV_NotCold_Counts_Ordered$Boot_MDC_ACME) == length(EMV_Cold_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(EMV_NotCold_Counts_Ordered$Boot_Count) == sum(EMV_Cold_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackEMV <- cbind(EMV_NotCold_Counts_Ordered,EMV_Cold_Counts_Ordered)
CountsOnlyEMV <- right_join(EMV_Ordered_Cold_Counts , EMV_Ordered_NotCold_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackEMV <- CountsOnlyEMV[!is.na(CountsOnlyEMV$Boot_Count) | !is.na(CountsOnlyEMV$count_by_siteyear),]
rm(CountsOnlyEMV)

sum(BackToBackEMV$count_by_siteyear)
sum(BackToBackEMV$Boot_Count)

colnames(BackToBackEMV) <- c("MDC_ACME","Cold","Boot")

BackToBackEMV$Boot <- as.numeric(BackToBackEMV$Boot)
sum(BackToBackEMV$Boot)
sum(BackToBackEMV$Cold)

BackToBackEMV_GreaterThanZero_Cold <- BackToBackEMV[BackToBackEMV$Boot > 0, ]

sum(BackToBackEMV_GreaterThanZero_Cold$Cold)
sum(BackToBackEMV_GreaterThanZero_Cold$Boot)

EO2_EMV <- (BackToBackEMV_GreaterThanZero_Cold$Cold - BackToBackEMV_GreaterThanZero_Cold$Boot)^2
E_EMV <- BackToBackEMV_GreaterThanZero_Cold$Boot
EO2E_EMV <- EO2_EMV/E_EMV
sumEO2E_EMV <- round(sum(EO2E_EMV),digits = 2)
sum(EO2E_EMV)
df_EMV <- length(EO2_EMV) - 1
#CritValue_df18 = 28.869
#CritValue_df19 = 30.144
#CritValue_df20 = 31.410
#CritValue_df21 = 32.671

# CSByHandTable <- data.frame(matrix(nrow=1,ncol=1))
# CSByHandTable$EMVCSValue <- sumEO2E_EMV

EO_Diff_EMV <- (BackToBackEMV_GreaterThanZero_Cold$Cold - BackToBackEMV_GreaterThanZero_Cold$Boot)
STD_RES_EMV <- EO_Diff_EMV / (sqrt(BackToBackEMV_GreaterThanZero_Cold$Boot))
BackToBackEMV_GreaterThanZero_Cold$Res <- round(STD_RES_EMV,digits = 3)
BackToBackEMV_GreaterThanZero_Cold$ResSig <- ifelse(abs(BackToBackEMV_GreaterThanZero_Cold$Res) > 3.8, TRUE, print("F"))


#EZF

EZF <- read_excel("~/Desktop/EZF.Final.Weather.xlsx",na="NA")
EZFselect <- EZF[1:5844, ]

EZFColdDaysSevere_lag0 <- EZFselect[EZFselect$ColdLag0 == 1, ]
EZFColdDaysSevere_lag1 <- EZFselect[EZFselect$ColdLag1 == 1, ]

EZFColdDaysSevere_LagCombine <- rbind(EZFColdDaysSevere_lag0,EZFColdDaysSevere_lag1)
EZFColdDaysSevere_LagCombine_DupsRem <- EZFColdDaysSevere_LagCombine[!duplicated(EZFColdDaysSevere_LagCombine), ]
EZFColdDays <- EZFColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

EZFColdDays_Winter <- EZFColdDays[!(3 < month(EZFColdDays$Date) & month(EZFColdDays$Date) < 10), ]
EZFColdDays_Winter$Date_Update <- format(as.Date(EZFColdDays_Winter$Date), "%Y-%m-%d")

rm(EZFColdDaysSevere_lag0,EZFColdDaysSevere_lag1,EZFColdDaysSevere_LagCombine,EZFColdDaysSevere_LagCombine_DupsRem,EZFColdDays)

EZF_MDC <- MDC_FullFinal %>%
  filter(Wx == "EZF")

EZF_MDC_Cold_NAs <- left_join(EZFColdDays_Winter, 
                              EZF_MDC,
                              by = c("Date_Update" = "Date"))
EZF_MDC_Cold <- EZF_MDC_Cold_NAs[!is.na(EZF_MDC_Cold_NAs$MDC_ACME),]
#NAOnly_Cold <- EZF_MDC_Cold[is.na(EZF_MDC_Cold$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(EZF_MDC_Cold$ColdLag0 == 0 & EZF_MDC_Cold$ColdLag1 == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(EZF_MDC_Cold$Station) / length(EZF_MDC$Date) #  8.74% of deaths happen on Cold Days
length(EZFColdDays_Winter$Station) / length(EZF$Station) # 8.322% of days are Cold day lag0 or lag1

####### BootStrapping EZF

EZFNotColdDaysSevere_lag0 <- EZFselect[!(EZFselect$ColdLag0 == 1), ]
EZFNotColdDaysSevere_lag1 <- EZFselect[!(EZFselect$ColdLag1 == 1), ]

EZFNotColdDaysSevere_LagCombine <- rbind(EZFNotColdDaysSevere_lag0,EZFNotColdDaysSevere_lag1)
EZFNotColdDaysSevere_LagCombine_DupsRem <- EZFNotColdDaysSevere_LagCombine[!duplicated(EZFNotColdDaysSevere_LagCombine), ]
EZFNotColdDays <- EZFNotColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

EZFNotColdDays_Winter <- EZFNotColdDays[!(3 < month(EZFNotColdDays$Date) & month(EZFNotColdDays$Date) < 10), ]
EZFNotColdDays_Winter$Date_Update <- format(as.Date(EZFNotColdDays_Winter$Date), "%Y-%m-%d")

rm(EZFNotColdDaysSevere_lag0,EZFNotColdDaysSevere_lag1,EZFNotColdDaysSevere_LagCombine,EZFNotColdDaysSevere_LagCombine_DupsRem,EZFNotColdDays)

#test <- EZFNotColdDays_Winter[EZFNotColdDays_Winter$Month == 3, ]

EZF_MDC_NotCold_NAs <- left_join(EZFNotColdDays_Winter, 
                                 EZF_MDC,
                                 by = c("Date_Update" = "Date"))
EZF_MDC_NotCold <- EZF_MDC_NotCold_NAs[!is.na(EZF_MDC_NotCold_NAs$MDC_ACME),]
#NAOnly_NotCold <- EZF_MDC_NotCold[is.na(EZF_MDC_NotCold$MDC_ACME), ] #0 obs. so yay!


EZF_MDC_NotCold_Slim <- EZF_MDC_NotCold %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotCold_Slim <- EZF_MDC_NotCold_Slim[is.na(EZF_MDC_NotCold_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   EZFNoStressRandom <- EZF_MDC_NotCold_Slim %>%
#     sample_n(length(EZF_MDC_Cold$Month),replace = TRUE)
#   write.csv(EZFNoStressRandom,paste0("~/Desktop/NoStressRandomWx/EZF_v3_Cold/EZF",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

EZF1 <- read.csv("~/Desktop/NoStressRandomWx/EZF_v3_Cold/EZF1")
EZF2 <- read.csv("~/Desktop/NoStressRandomWx/EZF_v3_Cold/EZF2")
ifelse(length(EZF1$X) == length(EZF_MDC_Cold$Month),1,0)
rm(EZF1,EZF2)

EZF_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/EZF_v3_Cold/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#EZF_data_all                                            # Print data to RStudio console

# Group by MDC

EZF_MDC_NotCold_Counts_InitialGrouping <- EZF_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
EZF_MDC_NotCold_Counts_InitialGrouping$Adjusted <- EZF_MDC_NotCold_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/EZF_v3_Cold/",  # Identify all CSV files
                                                                                                                                 pattern = "*", full.names = TRUE))) # Divide by number i
EZF_MDC_NotCold_Counts <- data.frame(cbind(EZF_MDC_NotCold_Counts_InitialGrouping$MDC_ACME,EZF_MDC_NotCold_Counts_InitialGrouping$Adjusted))

colnames(EZF_MDC_NotCold_Counts) <- c("Boot_MDC_ACME","Boot_Count")
EZF_MDC_NotCold_Counts$Boot_Count <- as.numeric(EZF_MDC_NotCold_Counts$Boot_Count)
sum(EZF_MDC_NotCold_Counts$Boot_Count)

#EZF_MDC_NotCold_Counts[EZF_MDC_NotCold_Counts$MDCAcme == "MDC22", 2] <- 0

EZF_MDC_Cold_Counts <- EZF_MDC_Cold %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# EZF_MDC_Cold_Counts[(length(EZF_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# EZF_MDC_Cold_Counts[(length(EZF_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# EZF_MDC_Cold_Counts[(length(EZF_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# EZF_MDC_Cold_Counts[(length(EZF_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# EZF_MDC_Cold_Counts[(length(EZF_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# EZF_MDC_Cold_Counts[(length(EZF_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# EZF_MDC_Cold_Counts[(length(EZF_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# EZF_MDC_Cold_Counts[(length(EZF_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)

rm(EZF_data_all,EZF_MDC_Cold_NAs,EZF_MDC_NotCold_NAs,EZF_MDC_NotCold_Slim)

#Final Prep for Comparison
EZF_Ordered_NotCold_Counts <- EZF_MDC_NotCold_Counts[order(EZF_MDC_NotCold_Counts$Boot_MDC_ACME), ]
EZF_Ordered_Cold_Counts <- EZF_MDC_Cold_Counts[order(EZF_MDC_Cold_Counts$MDC_ACME), ]

#EZF_Final_Cold <- EZF_Ordered_Cold_Counts[EZF_Ordered_Cold_Counts$count_by_siteyear > 5, ]
EZF_Ordered_NotCold_Expected_Counts <- EZF_Ordered_NotCold_Counts[EZF_Ordered_NotCold_Counts$Boot_Count > 5, ]

# ifelse(length(EZF_NotCold_Counts_Ordered$Boot_MDC_ACME) == length(EZF_Cold_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(EZF_NotCold_Counts_Ordered$Boot_Count) == sum(EZF_Cold_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackEZF <- cbind(EZF_NotCold_Counts_Ordered,EZF_Cold_Counts_Ordered)
CountsOnlyEZF <- right_join(EZF_Ordered_Cold_Counts , EZF_Ordered_NotCold_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackEZF <- CountsOnlyEZF[!is.na(CountsOnlyEZF$Boot_Count) | !is.na(CountsOnlyEZF$count_by_siteyear),]
rm(CountsOnlyEZF)

sum(BackToBackEZF$count_by_siteyear)
sum(BackToBackEZF$Boot_Count)

colnames(BackToBackEZF) <- c("MDC_ACME","Cold","Boot")

BackToBackEZF$Boot <- as.numeric(BackToBackEZF$Boot)
sum(BackToBackEZF$Boot)
sum(BackToBackEZF$Cold)

BackToBackEZF_GreaterThanZero_Cold <- BackToBackEZF[BackToBackEZF$Boot > 0, ]

sum(BackToBackEZF_GreaterThanZero_Cold$Cold)
sum(BackToBackEZF_GreaterThanZero_Cold$Boot)

EO2_EZF <- (BackToBackEZF_GreaterThanZero_Cold$Cold - BackToBackEZF_GreaterThanZero_Cold$Boot)^2
E_EZF <- BackToBackEZF_GreaterThanZero_Cold$Boot
EO2E_EZF <- EO2_EZF/E_EZF
sumEO2E_EZF <- round(sum(EO2E_EZF),digits = 2)
sum(EO2E_EZF)
df_EZF <- length(EO2_EZF) - 1
#CritValue_df18 = 28.869
#CritValue_df19 = 30.144
#CritValue_df20 = 31.410
#CritValue_df21 = 32.671

# CSByHandTable <- data.frame(matrix(nrow=1,ncol=1))
# CSByHandTable$EZFCSValue <- sumEO2E_EZF

EO_Diff_EZF <- (BackToBackEZF_GreaterThanZero_Cold$Cold - BackToBackEZF_GreaterThanZero_Cold$Boot)
STD_RES_EZF <- EO_Diff_EZF / (sqrt(BackToBackEZF_GreaterThanZero_Cold$Boot))
BackToBackEZF_GreaterThanZero_Cold$Res <- round(STD_RES_EZF,digits = 3)
BackToBackEZF_GreaterThanZero_Cold$ResSig <- ifelse(abs(BackToBackEZF_GreaterThanZero_Cold$Res) > 3.8, TRUE, print("F"))

#IAD

IAD <- read_excel("~/Desktop/IAD.Final.Weather.xlsx",na="NA")
IADselect <- IAD[1:5844, ]

IADColdDaysSevere_lag7 <- IADselect[IADselect$ColdLag7 == 1, ]
IADColdDaysSevere_lag8 <- IADselect[IADselect$ColdLag8 == 1, ]
IADColdDaysSevere_lag9 <- IADselect[IADselect$ColdLag9 == 1, ]
IADColdDaysSevere_lag10 <- IADselect[IADselect$ColdLag10 == 1, ]
IADColdDaysSevere_lag11 <- IADselect[IADselect$ColdLag11 == 1, ]
IADColdDaysSevere_lag12 <- IADselect[IADselect$ColdLag12 == 1, ]
IADColdDaysSevere_lag13 <- IADselect[IADselect$ColdLag13 == 1, ]

IADColdDaysSevere_LagCombine <- rbind(IADColdDaysSevere_lag7,IADColdDaysSevere_lag8,IADColdDaysSevere_lag9,IADColdDaysSevere_lag10,IADColdDaysSevere_lag11,IADColdDaysSevere_lag12,IADColdDaysSevere_lag13)
IADColdDaysSevere_LagCombine_DupsRem <- IADColdDaysSevere_LagCombine[!duplicated(IADColdDaysSevere_LagCombine), ]
IADColdDays <- IADColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

IADColdDays_Winter <- IADColdDays[!(3 < month(IADColdDays$Date) & month(IADColdDays$Date) < 10), ]
IADColdDays_Winter$Date_Update <- format(as.Date(IADColdDays_Winter$Date), "%Y-%m-%d")

rm(IADColdDaysSevere_lag7,IADColdDaysSevere_lag8,IADColdDaysSevere_lag9,IADColdDaysSevere_lag10,IADColdDaysSevere_lag11,IADColdDaysSevere_lag12,IADColdDaysSevere_lag13,IADColdDaysSevere_LagCombine,IADColdDaysSevere_LagCombine_DupsRem,IADColdDays)

IAD_MDC <- MDC_FullFinal %>%
  filter(Wx == "IAD")

IAD_MDC_Cold_NAs <- left_join(IADColdDays_Winter, 
                              IAD_MDC,
                              by = c("Date_Update" = "Date"))
IAD_MDC_Cold <- IAD_MDC_Cold_NAs[!is.na(IAD_MDC_Cold_NAs$MDC_ACME),]
#NAOnly_Cold <- IAD_MDC_Cold[is.na(IAD_MDC_Cold$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(IAD_MDC_Cold$ColdLag0 == 0 & IAD_MDC_Cold$ColdLag1 == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(IAD_MDC_Cold$Station) / length(IAD_MDC$Date) #  8.74% of deaths happen on Cold Days
length(IADColdDays_Winter$Station) / length(IAD$Station) # 8.322% of days are Cold day lag7 or lag8

####### BootStrapping IAD

IADNotColdDaysSevere_lag7 <- IADselect[!(IADselect$ColdLag7 == 1), ]
IADNotColdDaysSevere_lag8 <- IADselect[!(IADselect$ColdLag8 == 1), ]
IADNotColdDaysSevere_lag9 <- IADselect[!(IADselect$ColdLag9 == 1), ]
IADNotColdDaysSevere_lag10 <- IADselect[!(IADselect$ColdLag10 == 1), ]
IADNotColdDaysSevere_lag11 <- IADselect[!(IADselect$ColdLag11 == 1), ]
IADNotColdDaysSevere_lag12 <- IADselect[!(IADselect$ColdLag12 == 1), ]
IADNotColdDaysSevere_lag13 <- IADselect[!(IADselect$ColdLag13 == 1), ]

IADNotColdDaysSevere_LagCombine <- rbind(IADNotColdDaysSevere_lag7,IADNotColdDaysSevere_lag8,IADNotColdDaysSevere_lag9,IADNotColdDaysSevere_lag10,IADNotColdDaysSevere_lag11,IADNotColdDaysSevere_lag12,IADNotColdDaysSevere_lag13)
IADNotColdDaysSevere_LagCombine_DupsRem <- IADNotColdDaysSevere_LagCombine[!duplicated(IADNotColdDaysSevere_LagCombine), ]
IADNotColdDays <- IADNotColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

IADNotColdDays_Winter <- IADNotColdDays[!(3 < month(IADNotColdDays$Date) & month(IADNotColdDays$Date) < 10), ]
IADNotColdDays_Winter$Date_Update <- format(as.Date(IADNotColdDays_Winter$Date), "%Y-%m-%d")

rm(IADNotColdDaysSevere_lag7,IADNotColdDaysSevere_lag8,IADNotColdDaysSevere_lag9,IADNotColdDaysSevere_lag10,IADNotColdDaysSevere_lag11,IADNotColdDaysSevere_lag12,IADNotColdDaysSevere_lag13,IADNotColdDaysSevere_LagCombine,IADNotColdDaysSevere_LagCombine_DupsRem,IADNotColdDays)

#test <- IADNotColdDays_Winter[IADNotColdDays_Winter$Month == 3, ]

IAD_MDC_NotCold_NAs <- left_join(IADNotColdDays_Winter, 
                                 IAD_MDC,
                                 by = c("Date_Update" = "Date"))
IAD_MDC_NotCold <- IAD_MDC_NotCold_NAs[!is.na(IAD_MDC_NotCold_NAs$MDC_ACME),]
#NAOnly_NotCold <- IAD_MDC_NotCold[is.na(IAD_MDC_NotCold$MDC_ACME), ] #0 obs. so yay!


IAD_MDC_NotCold_Slim <- IAD_MDC_NotCold %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotCold_Slim <- IAD_MDC_NotCold_Slim[is.na(IAD_MDC_NotCold_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   IADNoStressRandom <- IAD_MDC_NotCold_Slim %>%
#     sample_n(length(IAD_MDC_Cold$Month),replace = TRUE)
#   write.csv(IADNoStressRandom,paste0("~/Desktop/NoStressRandomWx/IAD_v3_Cold/IAD",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

IAD1 <- read.csv("~/Desktop/NoStressRandomWx/IAD_v3_Cold/IAD1")
IAD2 <- read.csv("~/Desktop/NoStressRandomWx/IAD_v3_Cold/IAD2")
ifelse(length(IAD1$X) == length(IAD_MDC_Cold$Month),1,0)
rm(IAD1,IAD2)

IAD_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/IAD_v3_Cold/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#IAD_data_all                                            # Print data to RStudio console

# Group by MDC

IAD_MDC_NotCold_Counts_InitialGrouping <- IAD_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
IAD_MDC_NotCold_Counts_InitialGrouping$Adjusted <- IAD_MDC_NotCold_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/IAD_v3_Cold/",  # Identify all CSV files
                                                                                                                                 pattern = "*", full.names = TRUE))) # Divide by number i
IAD_MDC_NotCold_Counts <- data.frame(cbind(IAD_MDC_NotCold_Counts_InitialGrouping$MDC_ACME,IAD_MDC_NotCold_Counts_InitialGrouping$Adjusted))

colnames(IAD_MDC_NotCold_Counts) <- c("Boot_MDC_ACME","Boot_Count")
IAD_MDC_NotCold_Counts$Boot_Count <- as.numeric(IAD_MDC_NotCold_Counts$Boot_Count)
sum(IAD_MDC_NotCold_Counts$Boot_Count)

#IAD_MDC_NotCold_Counts[IAD_MDC_NotCold_Counts$MDCAcme == "MDC22", 2] <- 0

IAD_MDC_Cold_Counts <- IAD_MDC_Cold %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# IAD_MDC_Cold_Counts[(length(IAD_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# IAD_MDC_Cold_Counts[(length(IAD_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# IAD_MDC_Cold_Counts[(length(IAD_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# IAD_MDC_Cold_Counts[(length(IAD_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# IAD_MDC_Cold_Counts[(length(IAD_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# IAD_MDC_Cold_Counts[(length(IAD_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# IAD_MDC_Cold_Counts[(length(IAD_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# IAD_MDC_Cold_Counts[(length(IAD_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)

rm(IAD_data_all,IAD_MDC_Cold_NAs,IAD_MDC_NotCold_NAs,IAD_MDC_NotCold_Slim)

#Final Prep for Comparison
IAD_Ordered_NotCold_Counts <- IAD_MDC_NotCold_Counts[order(IAD_MDC_NotCold_Counts$Boot_MDC_ACME), ]
IAD_Ordered_Cold_Counts <- IAD_MDC_Cold_Counts[order(IAD_MDC_Cold_Counts$MDC_ACME), ]

#IAD_Final_Cold <- IAD_Ordered_Cold_Counts[IAD_Ordered_Cold_Counts$count_by_siteyear > 5, ]
IAD_Ordered_NotCold_Expected_Counts <- IAD_Ordered_NotCold_Counts[IAD_Ordered_NotCold_Counts$Boot_Count > 5, ]

# ifelse(length(IAD_NotCold_Counts_Ordered$Boot_MDC_ACME) == length(IAD_Cold_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(IAD_NotCold_Counts_Ordered$Boot_Count) == sum(IAD_Cold_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackIAD <- cbind(IAD_NotCold_Counts_Ordered,IAD_Cold_Counts_Ordered)
CountsOnlyIAD <- right_join(IAD_Ordered_Cold_Counts , IAD_Ordered_NotCold_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackIAD <- CountsOnlyIAD[!is.na(CountsOnlyIAD$Boot_Count) | !is.na(CountsOnlyIAD$count_by_siteyear),]
rm(CountsOnlyIAD)

sum(BackToBackIAD$count_by_siteyear)
sum(BackToBackIAD$Boot_Count)

colnames(BackToBackIAD) <- c("MDC_ACME","Cold","Boot")

BackToBackIAD$Boot <- as.numeric(BackToBackIAD$Boot)
sum(BackToBackIAD$Boot)
sum(BackToBackIAD$Cold)

BackToBackIAD_GreaterThanZero_Cold <- BackToBackIAD[BackToBackIAD$Boot > 0, ]

sum(BackToBackIAD_GreaterThanZero_Cold$Cold)
sum(BackToBackIAD_GreaterThanZero_Cold$Boot)

EO2_IAD <- (BackToBackIAD_GreaterThanZero_Cold$Cold - BackToBackIAD_GreaterThanZero_Cold$Boot)^2
E_IAD <- BackToBackIAD_GreaterThanZero_Cold$Boot
EO2E_IAD <- EO2_IAD/E_IAD
sumEO2E_IAD <- round(sum(EO2E_IAD),digits = 2)
sum(EO2E_IAD)
df_IAD <- length(EO2_IAD) - 1
#CritValue_df18 = 28.869
#CritValue_df19 = 30.144
#CritValue_df20 = 31.410
#CritValue_df21 = 32.671

# CSByHandTable <- data.frame(matrix(nrow=1,ncol=1))
# CSByHandTable$IADCSValue <- sumEO2E_IAD

EO_Diff_IAD <- (BackToBackIAD_GreaterThanZero_Cold$Cold - BackToBackIAD_GreaterThanZero_Cold$Boot)
STD_RES_IAD <- EO_Diff_IAD / (sqrt(BackToBackIAD_GreaterThanZero_Cold$Boot))
BackToBackIAD_GreaterThanZero_Cold$Res <- round(STD_RES_IAD,digits = 3)
BackToBackIAD_GreaterThanZero_Cold$ResSig <- ifelse(abs(BackToBackIAD_GreaterThanZero_Cold$Res) > 3.8, TRUE, print("F"))


#LYH

LYH <- read_excel("~/Desktop/LYH.Final.Weather.xlsx",na="NA")
LYHselect <- LYH[1:5844, ]

LYHColdDaysSevere_lag0 <- LYHselect[LYHselect$ColdLag0 == 1, ]
LYHColdDaysSevere_lag1 <- LYHselect[LYHselect$ColdLag1 == 1, ]
LYHColdDaysSevere_lag2 <- LYHselect[LYHselect$ColdLag2 == 1, ]


LYHColdDaysSevere_LagCombine <- rbind(LYHColdDaysSevere_lag0,LYHColdDaysSevere_lag1,LYHColdDaysSevere_lag2)
LYHColdDaysSevere_LagCombine_DupsRem <- LYHColdDaysSevere_LagCombine[!duplicated(LYHColdDaysSevere_LagCombine), ]
LYHColdDays <- LYHColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

LYHColdDays_Winter <- LYHColdDays[!(3 < month(LYHColdDays$Date) & month(LYHColdDays$Date) < 10), ]
LYHColdDays_Winter$Date_Update <- format(as.Date(LYHColdDays_Winter$Date), "%Y-%m-%d")

rm(LYHColdDaysSevere_lag0,LYHColdDaysSevere_lag1,LYHColdDaysSevere_lag2,LYHColdDaysSevere_LagCombine,LYHColdDaysSevere_LagCombine_DupsRem,LYHColdDays)

LYH_MDC <- MDC_FullFinal %>%
  filter(Wx == "LYH")

LYH_MDC_Cold_NAs <- left_join(LYHColdDays_Winter, 
                              LYH_MDC,
                              by = c("Date_Update" = "Date"))
LYH_MDC_Cold <- LYH_MDC_Cold_NAs[!is.na(LYH_MDC_Cold_NAs$MDC_ACME),]
#NAOnly_Cold <- LYH_MDC_Cold[is.na(LYH_MDC_Cold$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(LYH_MDC_Cold$ColdLag0 == 0 & LYH_MDC_Cold$ColdLag1 == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(LYH_MDC_Cold$Station) / length(LYH_MDC$Date) #  8.74% of deaths happen on Cold Days
length(LYHColdDays_Winter$Station) / length(LYH$Station) # 8.322% of days are Cold day lag0 or lag1

####### BootStrapping LYH

LYHNotColdDaysSevere_lag0 <- LYHselect[!(LYHselect$ColdLag0 == 1), ]
LYHNotColdDaysSevere_lag1 <- LYHselect[!(LYHselect$ColdLag1 == 1), ]
LYHNotColdDaysSevere_lag2 <- LYHselect[!(LYHselect$ColdLag2 == 1), ]

LYHNotColdDaysSevere_LagCombine <- rbind(LYHNotColdDaysSevere_lag0,LYHNotColdDaysSevere_lag1,LYHNotColdDaysSevere_lag2)
LYHNotColdDaysSevere_LagCombine_DupsRem <- LYHNotColdDaysSevere_LagCombine[!duplicated(LYHNotColdDaysSevere_LagCombine), ]
LYHNotColdDays <- LYHNotColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

LYHNotColdDays_Winter <- LYHNotColdDays[!(3 < month(LYHNotColdDays$Date) & month(LYHNotColdDays$Date) < 10), ]
LYHNotColdDays_Winter$Date_Update <- format(as.Date(LYHNotColdDays_Winter$Date), "%Y-%m-%d")

rm(LYHNotColdDaysSevere_lag0,LYHNotColdDaysSevere_lag1,LYHNotColdDaysSevere_lag2,LYHNotColdDaysSevere_LagCombine,LYHNotColdDaysSevere_LagCombine_DupsRem,LYHNotColdDays)

#test <- LYHNotColdDays_Winter[LYHNotColdDays_Winter$Month == 3, ]

LYH_MDC_NotCold_NAs <- left_join(LYHNotColdDays_Winter, 
                                 LYH_MDC,
                                 by = c("Date_Update" = "Date"))
LYH_MDC_NotCold <- LYH_MDC_NotCold_NAs[!is.na(LYH_MDC_NotCold_NAs$MDC_ACME),]
#NAOnly_NotCold <- LYH_MDC_NotCold[is.na(LYH_MDC_NotCold$MDC_ACME), ] #0 obs. so yay!


LYH_MDC_NotCold_Slim <- LYH_MDC_NotCold %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotCold_Slim <- LYH_MDC_NotCold_Slim[is.na(LYH_MDC_NotCold_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   LYHNoStressRandom <- LYH_MDC_NotCold_Slim %>%
#     sample_n(length(LYH_MDC_Cold$Month),replace = TRUE)
#   write.csv(LYHNoStressRandom,paste0("~/Desktop/NoStressRandomWx/LYH_v3_Cold/LYH",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

LYH1 <- read.csv("~/Desktop/NoStressRandomWx/LYH_v3_Cold/LYH1")
LYH2 <- read.csv("~/Desktop/NoStressRandomWx/LYH_v3_Cold/LYH2")
ifelse(length(LYH1$X) == length(LYH_MDC_Cold$Month),1,0)
rm(LYH1,LYH2)

LYH_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/LYH_v3_Cold/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#LYH_data_all                                            # Print data to RStudio console

# Group by MDC

LYH_MDC_NotCold_Counts_InitialGrouping <- LYH_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
LYH_MDC_NotCold_Counts_InitialGrouping$Adjusted <- LYH_MDC_NotCold_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/LYH_v3_Cold/",  # Identify all CSV files
                                                                                                                                 pattern = "*", full.names = TRUE))) # Divide by number i
LYH_MDC_NotCold_Counts <- data.frame(cbind(LYH_MDC_NotCold_Counts_InitialGrouping$MDC_ACME,LYH_MDC_NotCold_Counts_InitialGrouping$Adjusted))

colnames(LYH_MDC_NotCold_Counts) <- c("Boot_MDC_ACME","Boot_Count")
LYH_MDC_NotCold_Counts$Boot_Count <- as.numeric(LYH_MDC_NotCold_Counts$Boot_Count)
sum(LYH_MDC_NotCold_Counts$Boot_Count)

#LYH_MDC_NotCold_Counts[LYH_MDC_NotCold_Counts$MDCAcme == "MDC22", 2] <- 0

LYH_MDC_Cold_Counts <- LYH_MDC_Cold %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# LYH_MDC_Cold_Counts[(length(LYH_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# LYH_MDC_Cold_Counts[(length(LYH_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# LYH_MDC_Cold_Counts[(length(LYH_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# LYH_MDC_Cold_Counts[(length(LYH_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# LYH_MDC_Cold_Counts[(length(LYH_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# LYH_MDC_Cold_Counts[(length(LYH_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# LYH_MDC_Cold_Counts[(length(LYH_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# LYH_MDC_Cold_Counts[(length(LYH_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)

rm(LYH_data_all,LYH_MDC_Cold_NAs,LYH_MDC_NotCold_NAs,LYH_MDC_NotCold_Slim)

#Final Prep for Comparison
LYH_Ordered_NotCold_Counts <- LYH_MDC_NotCold_Counts[order(LYH_MDC_NotCold_Counts$Boot_MDC_ACME), ]
LYH_Ordered_Cold_Counts <- LYH_MDC_Cold_Counts[order(LYH_MDC_Cold_Counts$MDC_ACME), ]

#LYH_Final_Cold <- LYH_Ordered_Cold_Counts[LYH_Ordered_Cold_Counts$count_by_siteyear > 5, ]
LYH_Ordered_NotCold_Expected_Counts <- LYH_Ordered_NotCold_Counts[LYH_Ordered_NotCold_Counts$Boot_Count > 5, ]

# ifelse(length(LYH_NotCold_Counts_Ordered$Boot_MDC_ACME) == length(LYH_Cold_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(LYH_NotCold_Counts_Ordered$Boot_Count) == sum(LYH_Cold_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackLYH <- cbind(LYH_NotCold_Counts_Ordered,LYH_Cold_Counts_Ordered)
CountsOnlyLYH <- right_join(LYH_Ordered_Cold_Counts , LYH_Ordered_NotCold_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackLYH <- CountsOnlyLYH[!is.na(CountsOnlyLYH$Boot_Count) | !is.na(CountsOnlyLYH$count_by_siteyear),]
rm(CountsOnlyLYH)

sum(BackToBackLYH$count_by_siteyear)
sum(BackToBackLYH$Boot_Count)

colnames(BackToBackLYH) <- c("MDC_ACME","Cold","Boot")

BackToBackLYH$Boot <- as.numeric(BackToBackLYH$Boot)
sum(BackToBackLYH$Boot)
sum(BackToBackLYH$Cold)

BackToBackLYH_GreaterThanZero_Cold <- BackToBackLYH[BackToBackLYH$Boot > 0, ]

sum(BackToBackLYH_GreaterThanZero_Cold$Cold)
sum(BackToBackLYH_GreaterThanZero_Cold$Boot)

EO2_LYH <- (BackToBackLYH_GreaterThanZero_Cold$Cold - BackToBackLYH_GreaterThanZero_Cold$Boot)^2
E_LYH <- BackToBackLYH_GreaterThanZero_Cold$Boot
EO2E_LYH <- EO2_LYH/E_LYH
sumEO2E_LYH <- round(sum(EO2E_LYH),digits = 2)
sum(EO2E_LYH)
df_LYH <- length(EO2_LYH) - 1
#CritValue_df18 = 28.869
#CritValue_df19 = 30.144
#CritValue_df20 = 31.410
#CritValue_df21 = 32.671

# CSByHandTable <- data.frame(matrix(nrow=1,ncol=1))
# CSByHandTable$LYHCSValue <- sumEO2E_LYH

EO_Diff_LYH <- (BackToBackLYH_GreaterThanZero_Cold$Cold - BackToBackLYH_GreaterThanZero_Cold$Boot)
STD_RES_LYH <- EO_Diff_LYH / (sqrt(BackToBackLYH_GreaterThanZero_Cold$Boot))
BackToBackLYH_GreaterThanZero_Cold$Res <- round(STD_RES_LYH,digits = 3)
BackToBackLYH_GreaterThanZero_Cold$ResSig <- ifelse(abs(BackToBackLYH_GreaterThanZero_Cold$Res) > 3.8, TRUE, print("F"))

#OKV

OKV <- read_excel("~/Desktop/OKV.Final.Weather.xlsx",na="NA")
OKVselect <- OKV[1:5844, ]

OKVColdDaysSevere_lag0 <- OKVselect[OKVselect$ColdLag0 == 1, ]
OKVColdDaysSevere_lag1 <- OKVselect[OKVselect$ColdLag1 == 1, ]
OKVColdDaysSevere_lag2 <- OKVselect[OKVselect$ColdLag2 == 1, ]
OKVColdDaysSevere_lag3 <- OKVselect[OKVselect$ColdLag3 == 1, ]

OKVColdDaysSevere_LagCombine <- rbind(OKVColdDaysSevere_lag0,OKVColdDaysSevere_lag1,OKVColdDaysSevere_lag2,OKVColdDaysSevere_lag3)
OKVColdDaysSevere_LagCombine_DupsRem <- OKVColdDaysSevere_LagCombine[!duplicated(OKVColdDaysSevere_LagCombine), ]
OKVColdDays <- OKVColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

OKVColdDays_Winter <- OKVColdDays[!(3 < month(OKVColdDays$Date) & month(OKVColdDays$Date) < 10), ]
OKVColdDays_Winter$Date_Update <- format(as.Date(OKVColdDays_Winter$Date), "%Y-%m-%d")

rm(OKVColdDaysSevere_lag0,OKVColdDaysSevere_lag1,OKVColdDaysSevere_lag2,OKVColdDaysSevere_lag3,OKVColdDaysSevere_LagCombine,OKVColdDaysSevere_LagCombine_DupsRem,OKVColdDays)

OKV_MDC <- MDC_FullFinal %>%
  filter(Wx == "OKV")

OKV_MDC_Cold_NAs <- left_join(OKVColdDays_Winter, 
                              OKV_MDC,
                              by = c("Date_Update" = "Date"))
OKV_MDC_Cold <- OKV_MDC_Cold_NAs[!is.na(OKV_MDC_Cold_NAs$MDC_ACME),]
#NAOnly_Cold <- OKV_MDC_Cold[is.na(OKV_MDC_Cold$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(OKV_MDC_Cold$ColdLag0 == 0 & OKV_MDC_Cold$ColdLag1 == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(OKV_MDC_Cold$Station) / length(OKV_MDC$Date) #  8.74% of deaths happen on Cold Days
length(OKVColdDays_Winter$Station) / length(OKV$Station) # 8.322% of days are Cold day lag0 or lag1

####### BootStrapping OKV

OKVNotColdDaysSevere_lag0 <- OKVselect[!(OKVselect$ColdLag0 == 1), ]
OKVNotColdDaysSevere_lag1 <- OKVselect[!(OKVselect$ColdLag1 == 1), ]
OKVNotColdDaysSevere_lag2 <- OKVselect[!(OKVselect$ColdLag2 == 1), ]
OKVNotColdDaysSevere_lag3 <- OKVselect[!(OKVselect$ColdLag3 == 1), ]

OKVNotColdDaysSevere_LagCombine <- rbind(OKVNotColdDaysSevere_lag0,OKVNotColdDaysSevere_lag1,OKVNotColdDaysSevere_lag2,OKVNotColdDaysSevere_lag3)
OKVNotColdDaysSevere_LagCombine_DupsRem <- OKVNotColdDaysSevere_LagCombine[!duplicated(OKVNotColdDaysSevere_LagCombine), ]
OKVNotColdDays <- OKVNotColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

OKVNotColdDays_Winter <- OKVNotColdDays[!(3 < month(OKVNotColdDays$Date) & month(OKVNotColdDays$Date) < 10), ]
OKVNotColdDays_Winter$Date_Update <- format(as.Date(OKVNotColdDays_Winter$Date), "%Y-%m-%d")

rm(OKVNotColdDaysSevere_lag0,OKVNotColdDaysSevere_lag1,OKVNotColdDaysSevere_lag2,OKVNotColdDaysSevere_lag3,OKVNotColdDaysSevere_LagCombine,OKVNotColdDaysSevere_LagCombine_DupsRem,OKVNotColdDays)

#test <- OKVNotColdDays_Winter[OKVNotColdDays_Winter$Month == 3, ]

OKV_MDC_NotCold_NAs <- left_join(OKVNotColdDays_Winter, 
                                 OKV_MDC,
                                 by = c("Date_Update" = "Date"))
OKV_MDC_NotCold <- OKV_MDC_NotCold_NAs[!is.na(OKV_MDC_NotCold_NAs$MDC_ACME),]
#NAOnly_NotCold <- OKV_MDC_NotCold[is.na(OKV_MDC_NotCold$MDC_ACME), ] #0 obs. so yay!


OKV_MDC_NotCold_Slim <- OKV_MDC_NotCold %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotCold_Slim <- OKV_MDC_NotCold_Slim[is.na(OKV_MDC_NotCold_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   OKVNoStressRandom <- OKV_MDC_NotCold_Slim %>%
#     sample_n(length(OKV_MDC_Cold$Month),replace = TRUE)
#   write.csv(OKVNoStressRandom,paste0("~/Desktop/NoStressRandomWx/OKV_v3_Cold/OKV",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

OKV1 <- read.csv("~/Desktop/NoStressRandomWx/OKV_v3_Cold/OKV1")
OKV2 <- read.csv("~/Desktop/NoStressRandomWx/OKV_v3_Cold/OKV2")
ifelse(length(OKV1$X) == length(OKV_MDC_Cold$Month),1,0)
rm(OKV1,OKV2)

OKV_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/OKV_v3_Cold/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#OKV_data_all                                            # Print data to RStudio console

# Group by MDC

OKV_MDC_NotCold_Counts_InitialGrouping <- OKV_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
OKV_MDC_NotCold_Counts_InitialGrouping$Adjusted <- OKV_MDC_NotCold_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/OKV_v3_Cold/",  # Identify all CSV files
                                                                                                                                 pattern = "*", full.names = TRUE))) # Divide by number i
OKV_MDC_NotCold_Counts <- data.frame(cbind(OKV_MDC_NotCold_Counts_InitialGrouping$MDC_ACME,OKV_MDC_NotCold_Counts_InitialGrouping$Adjusted))

colnames(OKV_MDC_NotCold_Counts) <- c("Boot_MDC_ACME","Boot_Count")
OKV_MDC_NotCold_Counts$Boot_Count <- as.numeric(OKV_MDC_NotCold_Counts$Boot_Count)
sum(OKV_MDC_NotCold_Counts$Boot_Count)

#OKV_MDC_NotCold_Counts[OKV_MDC_NotCold_Counts$MDCAcme == "MDC22", 2] <- 0

OKV_MDC_Cold_Counts <- OKV_MDC_Cold %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# OKV_MDC_Cold_Counts[(length(OKV_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# OKV_MDC_Cold_Counts[(length(OKV_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# OKV_MDC_Cold_Counts[(length(OKV_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# OKV_MDC_Cold_Counts[(length(OKV_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# OKV_MDC_Cold_Counts[(length(OKV_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# OKV_MDC_Cold_Counts[(length(OKV_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# OKV_MDC_Cold_Counts[(length(OKV_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# OKV_MDC_Cold_Counts[(length(OKV_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)

rm(OKV_data_all,OKV_MDC_Cold_NAs,OKV_MDC_NotCold_NAs,OKV_MDC_NotCold_Slim)

#Final Prep for Comparison
OKV_Ordered_NotCold_Counts <- OKV_MDC_NotCold_Counts[order(OKV_MDC_NotCold_Counts$Boot_MDC_ACME), ]
OKV_Ordered_Cold_Counts <- OKV_MDC_Cold_Counts[order(OKV_MDC_Cold_Counts$MDC_ACME), ]

#OKV_Final_Cold <- OKV_Ordered_Cold_Counts[OKV_Ordered_Cold_Counts$count_by_siteyear > 5, ]
OKV_Ordered_NotCold_Expected_Counts <- OKV_Ordered_NotCold_Counts[OKV_Ordered_NotCold_Counts$Boot_Count > 5, ]

# ifelse(length(OKV_NotCold_Counts_Ordered$Boot_MDC_ACME) == length(OKV_Cold_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(OKV_NotCold_Counts_Ordered$Boot_Count) == sum(OKV_Cold_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackOKV <- cbind(OKV_NotCold_Counts_Ordered,OKV_Cold_Counts_Ordered)
CountsOnlyOKV <- right_join(OKV_Ordered_Cold_Counts , OKV_Ordered_NotCold_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackOKV <- CountsOnlyOKV[!is.na(CountsOnlyOKV$Boot_Count) | !is.na(CountsOnlyOKV$count_by_siteyear),]
rm(CountsOnlyOKV)

sum(BackToBackOKV$count_by_siteyear)
sum(BackToBackOKV$Boot_Count)

colnames(BackToBackOKV) <- c("MDC_ACME","Cold","Boot")

BackToBackOKV$Boot <- as.numeric(BackToBackOKV$Boot)
sum(BackToBackOKV$Boot)
sum(BackToBackOKV$Cold)

BackToBackOKV_GreaterThanZero_Cold <- BackToBackOKV[BackToBackOKV$Boot > 0, ]

sum(BackToBackOKV_GreaterThanZero_Cold$Cold)
sum(BackToBackOKV_GreaterThanZero_Cold$Boot)

EO2_OKV <- (BackToBackOKV_GreaterThanZero_Cold$Cold - BackToBackOKV_GreaterThanZero_Cold$Boot)^2
E_OKV <- BackToBackOKV_GreaterThanZero_Cold$Boot
EO2E_OKV <- EO2_OKV/E_OKV
sumEO2E_OKV <- round(sum(EO2E_OKV),digits = 2)
sum(EO2E_OKV)
df_OKV <- length(EO2_OKV) - 1
#CritValue_df18 = 28.869
#CritValue_df19 = 30.144
#CritValue_df20 = 31.410
#CritValue_df21 = 32.671

# CSByHandTable <- data.frame(matrix(nrow=1,ncol=1))
# CSByHandTable$OKVCSValue <- sumEO2E_OKV

EO_Diff_OKV <- (BackToBackOKV_GreaterThanZero_Cold$Cold - BackToBackOKV_GreaterThanZero_Cold$Boot)
STD_RES_OKV <- EO_Diff_OKV / (sqrt(BackToBackOKV_GreaterThanZero_Cold$Boot))
BackToBackOKV_GreaterThanZero_Cold$Res <- round(STD_RES_OKV,digits = 3)
BackToBackOKV_GreaterThanZero_Cold$ResSig <- ifelse(abs(BackToBackOKV_GreaterThanZero_Cold$Res) > 3.8, TRUE, print("F"))

#ORF

ORF <- read_excel("~/Desktop/ORF.Final.Weather.xlsx",na="NA")
ORFselect <- ORF[1:5844, ]

ORFColdDaysSevere_lag0 <- ORFselect[ORFselect$ColdLag0 == 1, ]
ORFColdDaysSevere_lag1 <- ORFselect[ORFselect$ColdLag1 == 1, ]
ORFColdDaysSevere_lag2 <- ORFselect[ORFselect$ColdLag2 == 1, ]
ORFColdDaysSevere_lag3 <- ORFselect[ORFselect$ColdLag3 == 1, ]
ORFColdDaysSevere_lag4 <- ORFselect[ORFselect$ColdLag4 == 1, ]
ORFColdDaysSevere_lag5 <- ORFselect[ORFselect$ColdLag5 == 1, ]
ORFColdDaysSevere_lag6 <- ORFselect[ORFselect$ColdLag6 == 1, ]

ORFColdDaysSevere_LagCombine <- rbind(ORFColdDaysSevere_lag0,ORFColdDaysSevere_lag1,ORFColdDaysSevere_lag2,ORFColdDaysSevere_lag3,ORFColdDaysSevere_lag4,ORFColdDaysSevere_lag5,ORFColdDaysSevere_lag6)
ORFColdDaysSevere_LagCombine_DupsRem <- ORFColdDaysSevere_LagCombine[!duplicated(ORFColdDaysSevere_LagCombine), ]
ORFColdDays <- ORFColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

ORFColdDays_Winter <- ORFColdDays[!(3 < month(ORFColdDays$Date) & month(ORFColdDays$Date) < 10), ]
ORFColdDays_Winter$Date_Update <- format(as.Date(ORFColdDays_Winter$Date), "%Y-%m-%d")

rm(ORFColdDaysSevere_lag0,ORFColdDaysSevere_lag1,ORFColdDaysSevere_lag2,ORFColdDaysSevere_lag3,ORFColdDaysSevere_lag4,ORFColdDaysSevere_lag5,ORFColdDaysSevere_lag6,ORFColdDaysSevere_LagCombine,ORFColdDaysSevere_LagCombine_DupsRem,ORFColdDays)

ORF_MDC <- MDC_FullFinal %>%
  filter(Wx == "ORF")

ORF_MDC_Cold_NAs <- left_join(ORFColdDays_Winter, 
                              ORF_MDC,
                              by = c("Date_Update" = "Date"))
ORF_MDC_Cold <- ORF_MDC_Cold_NAs[!is.na(ORF_MDC_Cold_NAs$MDC_ACME),]
#NAOnly_Cold <- ORF_MDC_Cold[is.na(ORF_MDC_Cold$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(ORF_MDC_Cold$ColdLag0 == 0 & ORF_MDC_Cold$ColdLag1 == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(ORF_MDC_Cold$Station) / length(ORF_MDC$Date) #  8.74% of deaths happen on Cold Days
length(ORFColdDays_Winter$Station) / length(ORF$Station) # 8.322% of days are Cold day lag0 or lag1

####### BootStrapping ORF

ORFNotColdDaysSevere_lag0 <- ORFselect[!(ORFselect$ColdLag0 == 1), ]
ORFNotColdDaysSevere_lag1 <- ORFselect[!(ORFselect$ColdLag1 == 1), ]
ORFNotColdDaysSevere_lag2 <- ORFselect[!(ORFselect$ColdLag2 == 1), ]
ORFNotColdDaysSevere_lag3 <- ORFselect[!(ORFselect$ColdLag3 == 1), ]
ORFNotColdDaysSevere_lag4 <- ORFselect[!(ORFselect$ColdLag4 == 1), ]
ORFNotColdDaysSevere_lag5 <- ORFselect[!(ORFselect$ColdLag5 == 1), ]
ORFNotColdDaysSevere_lag6 <- ORFselect[!(ORFselect$ColdLag6 == 1), ]

ORFNotColdDaysSevere_LagCombine <- rbind(ORFNotColdDaysSevere_lag0,ORFNotColdDaysSevere_lag1,ORFNotColdDaysSevere_lag2,ORFNotColdDaysSevere_lag3,ORFNotColdDaysSevere_lag4,ORFNotColdDaysSevere_lag5,ORFNotColdDaysSevere_lag6)
ORFNotColdDaysSevere_LagCombine_DupsRem <- ORFNotColdDaysSevere_LagCombine[!duplicated(ORFNotColdDaysSevere_LagCombine), ]
ORFNotColdDays <- ORFNotColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

ORFNotColdDays_Winter <- ORFNotColdDays[!(3 < month(ORFNotColdDays$Date) & month(ORFNotColdDays$Date) < 10), ]
ORFNotColdDays_Winter$Date_Update <- format(as.Date(ORFNotColdDays_Winter$Date), "%Y-%m-%d")

rm(ORFNotColdDaysSevere_lag0,ORFNotColdDaysSevere_lag1,ORFNotColdDaysSevere_lag2,ORFNotColdDaysSevere_lag3,ORFNotColdDaysSevere_lag4,ORFNotColdDaysSevere_lag5,ORFNotColdDaysSevere_lag6,ORFNotColdDaysSevere_LagCombine,ORFNotColdDaysSevere_LagCombine_DupsRem,ORFNotColdDays)

#test <- ORFNotColdDays_Winter[ORFNotColdDays_Winter$Month == 3, ]

ORF_MDC_NotCold_NAs <- left_join(ORFNotColdDays_Winter, 
                                 ORF_MDC,
                                 by = c("Date_Update" = "Date"))
ORF_MDC_NotCold <- ORF_MDC_NotCold_NAs[!is.na(ORF_MDC_NotCold_NAs$MDC_ACME),]
#NAOnly_NotCold <- ORF_MDC_NotCold[is.na(ORF_MDC_NotCold$MDC_ACME), ] #0 obs. so yay!


ORF_MDC_NotCold_Slim <- ORF_MDC_NotCold %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotCold_Slim <- ORF_MDC_NotCold_Slim[is.na(ORF_MDC_NotCold_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   ORFNoStressRandom <- ORF_MDC_NotCold_Slim %>%
#     sample_n(length(ORF_MDC_Cold$Month),replace = TRUE)
#   write.csv(ORFNoStressRandom,paste0("~/Desktop/NoStressRandomWx/ORF_v3_Cold/ORF",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

ORF1 <- read.csv("~/Desktop/NoStressRandomWx/ORF_v3_Cold/ORF1")
ORF2 <- read.csv("~/Desktop/NoStressRandomWx/ORF_v3_Cold/ORF2")
ifelse(length(ORF1$X) == length(ORF_MDC_Cold$Month),1,0)
rm(ORF1,ORF2)

ORF_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/ORF_v3_Cold/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#ORF_data_all                                            # Print data to RStudio console

# Group by MDC

ORF_MDC_NotCold_Counts_InitialGrouping <- ORF_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
ORF_MDC_NotCold_Counts_InitialGrouping$Adjusted <- ORF_MDC_NotCold_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/ORF_v3_Cold/",  # Identify all CSV files
                                                                                                                                 pattern = "*", full.names = TRUE))) # Divide by number i
ORF_MDC_NotCold_Counts <- data.frame(cbind(ORF_MDC_NotCold_Counts_InitialGrouping$MDC_ACME,ORF_MDC_NotCold_Counts_InitialGrouping$Adjusted))

colnames(ORF_MDC_NotCold_Counts) <- c("Boot_MDC_ACME","Boot_Count")
ORF_MDC_NotCold_Counts$Boot_Count <- as.numeric(ORF_MDC_NotCold_Counts$Boot_Count)
sum(ORF_MDC_NotCold_Counts$Boot_Count)

#ORF_MDC_NotCold_Counts[ORF_MDC_NotCold_Counts$MDCAcme == "MDC22", 2] <- 0

ORF_MDC_Cold_Counts <- ORF_MDC_Cold %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# ORF_MDC_Cold_Counts[(length(ORF_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# ORF_MDC_Cold_Counts[(length(ORF_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# ORF_MDC_Cold_Counts[(length(ORF_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# ORF_MDC_Cold_Counts[(length(ORF_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# ORF_MDC_Cold_Counts[(length(ORF_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# ORF_MDC_Cold_Counts[(length(ORF_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# ORF_MDC_Cold_Counts[(length(ORF_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# ORF_MDC_Cold_Counts[(length(ORF_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)

rm(ORF_data_all,ORF_MDC_Cold_NAs,ORF_MDC_NotCold_NAs,ORF_MDC_NotCold_Slim)

#Final Prep for Comparison
ORF_Ordered_NotCold_Counts <- ORF_MDC_NotCold_Counts[order(ORF_MDC_NotCold_Counts$Boot_MDC_ACME), ]
ORF_Ordered_Cold_Counts <- ORF_MDC_Cold_Counts[order(ORF_MDC_Cold_Counts$MDC_ACME), ]

#ORF_Final_Cold <- ORF_Ordered_Cold_Counts[ORF_Ordered_Cold_Counts$count_by_siteyear > 5, ]
ORF_Ordered_NotCold_Expected_Counts <- ORF_Ordered_NotCold_Counts[ORF_Ordered_NotCold_Counts$Boot_Count > 5, ]

# ifelse(length(ORF_NotCold_Counts_Ordered$Boot_MDC_ACME) == length(ORF_Cold_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(ORF_NotCold_Counts_Ordered$Boot_Count) == sum(ORF_Cold_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackORF <- cbind(ORF_NotCold_Counts_Ordered,ORF_Cold_Counts_Ordered)
CountsOnlyORF <- right_join(ORF_Ordered_Cold_Counts , ORF_Ordered_NotCold_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackORF <- CountsOnlyORF[!is.na(CountsOnlyORF$Boot_Count) | !is.na(CountsOnlyORF$count_by_siteyear),]
rm(CountsOnlyORF)

sum(BackToBackORF$count_by_siteyear)
sum(BackToBackORF$Boot_Count)

colnames(BackToBackORF) <- c("MDC_ACME","Cold","Boot")

BackToBackORF$Boot <- as.numeric(BackToBackORF$Boot)
sum(BackToBackORF$Boot)
sum(BackToBackORF$Cold)

BackToBackORF_GreaterThanZero_Cold <- BackToBackORF[BackToBackORF$Boot > 0, ]

sum(BackToBackORF_GreaterThanZero_Cold$Cold)
sum(BackToBackORF_GreaterThanZero_Cold$Boot)

EO2_ORF <- (BackToBackORF_GreaterThanZero_Cold$Cold - BackToBackORF_GreaterThanZero_Cold$Boot)^2
E_ORF <- BackToBackORF_GreaterThanZero_Cold$Boot
EO2E_ORF <- EO2_ORF/E_ORF
sumEO2E_ORF <- round(sum(EO2E_ORF),digits = 2)
sum(EO2E_ORF)
df_ORF <- length(EO2_ORF) - 1
#CritValue_df18 = 28.869
#CritValue_df19 = 30.144
#CritValue_df20 = 31.410
#CritValue_df21 = 32.671

# CSByHandTable <- data.frame(matrix(nrow=1,ncol=1))
# CSByHandTable$ORFCSValue <- sumEO2E_ORF

EO_Diff_ORF <- (BackToBackORF_GreaterThanZero_Cold$Cold - BackToBackORF_GreaterThanZero_Cold$Boot)
STD_RES_ORF <- EO_Diff_ORF / (sqrt(BackToBackORF_GreaterThanZero_Cold$Boot))
BackToBackORF_GreaterThanZero_Cold$Res <- round(STD_RES_ORF,digits = 3)
BackToBackORF_GreaterThanZero_Cold$ResSig <- ifelse(abs(BackToBackORF_GreaterThanZero_Cold$Res) > 3.8, TRUE, print("F"))

#PHF

PHF <- read_excel("~/Desktop/PHF.Final.Weather.xlsx",na="NA")
PHFselect <- PHF[1:5844, ]

PHFColdDaysSevere_lag0 <- PHFselect[PHFselect$ColdLag0 == 1, ]
PHFColdDaysSevere_lag1 <- PHFselect[PHFselect$ColdLag1 == 1, ]
PHFColdDaysSevere_lag2 <- PHFselect[PHFselect$ColdLag2 == 1, ]
PHFColdDaysSevere_lag3 <- PHFselect[PHFselect$ColdLag3 == 1, ]
PHFColdDaysSevere_lag4 <- PHFselect[PHFselect$ColdLag4 == 1, ]
PHFColdDaysSevere_lag5 <- PHFselect[PHFselect$ColdLag5 == 1, ]
PHFColdDaysSevere_lag6 <- PHFselect[PHFselect$ColdLag6 == 1, ]
PHFColdDaysSevere_lag7 <- PHFselect[PHFselect$ColdLag7 == 1, ]
PHFColdDaysSevere_lag8 <- PHFselect[PHFselect$ColdLag8 == 1, ]

PHFColdDaysSevere_LagCombine <- rbind(PHFColdDaysSevere_lag0,PHFColdDaysSevere_lag1,PHFColdDaysSevere_lag2,PHFColdDaysSevere_lag3,PHFColdDaysSevere_lag4,PHFColdDaysSevere_lag5,PHFColdDaysSevere_lag6,PHFColdDaysSevere_lag7,PHFColdDaysSevere_lag8)
PHFColdDaysSevere_LagCombine_DupsRem <- PHFColdDaysSevere_LagCombine[!duplicated(PHFColdDaysSevere_LagCombine), ]
PHFColdDays <- PHFColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

PHFColdDays_Winter <- PHFColdDays[!(3 < month(PHFColdDays$Date) & month(PHFColdDays$Date) < 10), ]
PHFColdDays_Winter$Date_Update <- format(as.Date(PHFColdDays_Winter$Date), "%Y-%m-%d")

rm(PHFColdDaysSevere_lag0,PHFColdDaysSevere_lag1,PHFColdDaysSevere_lag2,PHFColdDaysSevere_lag3,PHFColdDaysSevere_lag4,PHFColdDaysSevere_lag5,PHFColdDaysSevere_lag6,PHFColdDaysSevere_lag7,PHFColdDaysSevere_lag8,PHFColdDaysSevere_LagCombine,PHFColdDaysSevere_LagCombine_DupsRem,PHFColdDays)

PHF_MDC <- MDC_FullFinal %>%
  filter(Wx == "PHF")

PHF_MDC_Cold_NAs <- left_join(PHFColdDays_Winter, 
                              PHF_MDC,
                              by = c("Date_Update" = "Date"))
PHF_MDC_Cold <- PHF_MDC_Cold_NAs[!is.na(PHF_MDC_Cold_NAs$MDC_ACME),]
#NAOnly_Cold <- PHF_MDC_Cold[is.na(PHF_MDC_Cold$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(PHF_MDC_Cold$ColdLag0 == 0 & PHF_MDC_Cold$ColdLag1 == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(PHF_MDC_Cold$Station) / length(PHF_MDC$Date) #  8.74% of deaths happen on Cold Days
length(PHFColdDays_Winter$Station) / length(PHF$Station) # 8.322% of days are Cold day lag0 or lag1

####### BootStrapping PHF

PHFNotColdDaysSevere_lag0 <- PHFselect[!(PHFselect$ColdLag0 == 1), ]
PHFNotColdDaysSevere_lag1 <- PHFselect[!(PHFselect$ColdLag1 == 1), ]
PHFNotColdDaysSevere_lag2 <- PHFselect[!(PHFselect$ColdLag2 == 1), ]
PHFNotColdDaysSevere_lag3 <- PHFselect[!(PHFselect$ColdLag3 == 1), ]
PHFNotColdDaysSevere_lag4 <- PHFselect[!(PHFselect$ColdLag4 == 1), ]
PHFNotColdDaysSevere_lag5 <- PHFselect[!(PHFselect$ColdLag5 == 1), ]
PHFNotColdDaysSevere_lag6 <- PHFselect[!(PHFselect$ColdLag6 == 1), ]
PHFNotColdDaysSevere_lag7 <- PHFselect[!(PHFselect$ColdLag7 == 1), ]
PHFNotColdDaysSevere_lag8 <- PHFselect[!(PHFselect$ColdLag8 == 1), ]

PHFNotColdDaysSevere_LagCombine <- rbind(PHFNotColdDaysSevere_lag0,PHFNotColdDaysSevere_lag1,PHFNotColdDaysSevere_lag2,PHFNotColdDaysSevere_lag3,PHFNotColdDaysSevere_lag4,PHFNotColdDaysSevere_lag5,PHFNotColdDaysSevere_lag6,PHFNotColdDaysSevere_lag7,PHFNotColdDaysSevere_lag8)
PHFNotColdDaysSevere_LagCombine_DupsRem <- PHFNotColdDaysSevere_LagCombine[!duplicated(PHFNotColdDaysSevere_LagCombine), ]
PHFNotColdDays <- PHFNotColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

PHFNotColdDays_Winter <- PHFNotColdDays[!(3 < month(PHFNotColdDays$Date) & month(PHFNotColdDays$Date) < 10), ]
PHFNotColdDays_Winter$Date_Update <- format(as.Date(PHFNotColdDays_Winter$Date), "%Y-%m-%d")

rm(PHFNotColdDaysSevere_lag0,PHFNotColdDaysSevere_lag1,PHFNotColdDaysSevere_lag2,PHFNotColdDaysSevere_lag3,PHFNotColdDaysSevere_lag4,PHFNotColdDaysSevere_lag5,PHFNotColdDaysSevere_lag6,PHFNotColdDaysSevere_lag7,PHFNotColdDaysSevere_lag8,PHFNotColdDaysSevere_LagCombine,PHFNotColdDaysSevere_LagCombine_DupsRem,PHFNotColdDays)

#test <- PHFNotColdDays_Winter[PHFNotColdDays_Winter$Month == 3, ]

PHF_MDC_NotCold_NAs <- left_join(PHFNotColdDays_Winter, 
                                 PHF_MDC,
                                 by = c("Date_Update" = "Date"))
PHF_MDC_NotCold <- PHF_MDC_NotCold_NAs[!is.na(PHF_MDC_NotCold_NAs$MDC_ACME),]
#NAOnly_NotCold <- PHF_MDC_NotCold[is.na(PHF_MDC_NotCold$MDC_ACME), ] #0 obs. so yay!


PHF_MDC_NotCold_Slim <- PHF_MDC_NotCold %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotCold_Slim <- PHF_MDC_NotCold_Slim[is.na(PHF_MDC_NotCold_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   PHFNoStressRandom <- PHF_MDC_NotCold_Slim %>%
#     sample_n(length(PHF_MDC_Cold$Month),replace = TRUE)
#   write.csv(PHFNoStressRandom,paste0("~/Desktop/NoStressRandomWx/PHF_v3_Cold/PHF",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

PHF1 <- read.csv("~/Desktop/NoStressRandomWx/PHF_v3_Cold/PHF1")
PHF2 <- read.csv("~/Desktop/NoStressRandomWx/PHF_v3_Cold/PHF2")
ifelse(length(PHF1$X) == length(PHF_MDC_Cold$Month),1,0)
rm(PHF1,PHF2)

PHF_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/PHF_v3_Cold/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#PHF_data_all                                            # Print data to RStudio console

# Group by MDC

PHF_MDC_NotCold_Counts_InitialGrouping <- PHF_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
PHF_MDC_NotCold_Counts_InitialGrouping$Adjusted <- PHF_MDC_NotCold_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/PHF_v3_Cold/",  # Identify all CSV files
                                                                                                                                 pattern = "*", full.names = TRUE))) # Divide by number i
PHF_MDC_NotCold_Counts <- data.frame(cbind(PHF_MDC_NotCold_Counts_InitialGrouping$MDC_ACME,PHF_MDC_NotCold_Counts_InitialGrouping$Adjusted))

colnames(PHF_MDC_NotCold_Counts) <- c("Boot_MDC_ACME","Boot_Count")
PHF_MDC_NotCold_Counts$Boot_Count <- as.numeric(PHF_MDC_NotCold_Counts$Boot_Count)
sum(PHF_MDC_NotCold_Counts$Boot_Count)

#PHF_MDC_NotCold_Counts[PHF_MDC_NotCold_Counts$MDCAcme == "MDC22", 2] <- 0

PHF_MDC_Cold_Counts <- PHF_MDC_Cold %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# PHF_MDC_Cold_Counts[(length(PHF_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# PHF_MDC_Cold_Counts[(length(PHF_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# PHF_MDC_Cold_Counts[(length(PHF_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# PHF_MDC_Cold_Counts[(length(PHF_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# PHF_MDC_Cold_Counts[(length(PHF_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# PHF_MDC_Cold_Counts[(length(PHF_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# PHF_MDC_Cold_Counts[(length(PHF_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# PHF_MDC_Cold_Counts[(length(PHF_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)

rm(PHF_data_all,PHF_MDC_Cold_NAs,PHF_MDC_NotCold_NAs,PHF_MDC_NotCold_Slim)

#Final Prep for Comparison
PHF_Ordered_NotCold_Counts <- PHF_MDC_NotCold_Counts[order(PHF_MDC_NotCold_Counts$Boot_MDC_ACME), ]
PHF_Ordered_Cold_Counts <- PHF_MDC_Cold_Counts[order(PHF_MDC_Cold_Counts$MDC_ACME), ]

#PHF_Final_Cold <- PHF_Ordered_Cold_Counts[PHF_Ordered_Cold_Counts$count_by_siteyear > 5, ]
PHF_Ordered_NotCold_Expected_Counts <- PHF_Ordered_NotCold_Counts[PHF_Ordered_NotCold_Counts$Boot_Count > 5, ]

# ifelse(length(PHF_NotCold_Counts_Ordered$Boot_MDC_ACME) == length(PHF_Cold_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(PHF_NotCold_Counts_Ordered$Boot_Count) == sum(PHF_Cold_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackPHF <- cbind(PHF_NotCold_Counts_Ordered,PHF_Cold_Counts_Ordered)
CountsOnlyPHF <- right_join(PHF_Ordered_Cold_Counts , PHF_Ordered_NotCold_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackPHF <- CountsOnlyPHF[!is.na(CountsOnlyPHF$Boot_Count) | !is.na(CountsOnlyPHF$count_by_siteyear),]
rm(CountsOnlyPHF)

sum(BackToBackPHF$count_by_siteyear)
sum(BackToBackPHF$Boot_Count)

colnames(BackToBackPHF) <- c("MDC_ACME","Cold","Boot")

BackToBackPHF$Boot <- as.numeric(BackToBackPHF$Boot)
sum(BackToBackPHF$Boot)
sum(BackToBackPHF$Cold)

BackToBackPHF_GreaterThanZero_Cold <- BackToBackPHF[BackToBackPHF$Boot > 0, ]

sum(BackToBackPHF_GreaterThanZero_Cold$Cold)
sum(BackToBackPHF_GreaterThanZero_Cold$Boot)

EO2_PHF <- (BackToBackPHF_GreaterThanZero_Cold$Cold - BackToBackPHF_GreaterThanZero_Cold$Boot)^2
E_PHF <- BackToBackPHF_GreaterThanZero_Cold$Boot
EO2E_PHF <- EO2_PHF/E_PHF
sumEO2E_PHF <- round(sum(EO2E_PHF),digits = 2)
sum(EO2E_PHF)
df_PHF <- length(EO2_PHF) - 1
#CritValue_df18 = 28.869
#CritValue_df19 = 30.144
#CritValue_df20 = 31.410
#CritValue_df21 = 32.671

# CSByHandTable <- data.frame(matrix(nrow=1,ncol=1))
# CSByHandTable$PHFCSValue <- sumEO2E_PHF

EO_Diff_PHF <- (BackToBackPHF_GreaterThanZero_Cold$Cold - BackToBackPHF_GreaterThanZero_Cold$Boot)
STD_RES_PHF <- EO_Diff_PHF / (sqrt(BackToBackPHF_GreaterThanZero_Cold$Boot))
BackToBackPHF_GreaterThanZero_Cold$Res <- round(STD_RES_PHF,digits = 3)
BackToBackPHF_GreaterThanZero_Cold$ResSig <- ifelse(abs(BackToBackPHF_GreaterThanZero_Cold$Res) > 3.8, TRUE, print("F"))

#RIC

RIC <- read_excel("~/Desktop/RIC.Final.Weather.xlsx",na="NA")
RICselect <- RIC[1:5844, ]

RICColdDaysSevere_lag0 <- RICselect[RICselect$ColdLag0 == 1, ]
RICColdDaysSevere_lag1 <- RICselect[RICselect$ColdLag1 == 1, ]
RICColdDaysSevere_lag2 <- RICselect[RICselect$ColdLag2 == 1, ]
RICColdDaysSevere_lag3 <- RICselect[RICselect$ColdLag3 == 1, ]
RICColdDaysSevere_lag4 <- RICselect[RICselect$ColdLag4 == 1, ]
RICColdDaysSevere_lag5 <- RICselect[RICselect$ColdLag5 == 1, ]
RICColdDaysSevere_lag6 <- RICselect[RICselect$ColdLag6 == 1, ]
RICColdDaysSevere_lag7 <- RICselect[RICselect$ColdLag7 == 1, ]
RICColdDaysSevere_lag8 <- RICselect[RICselect$ColdLag8 == 1, ]
RICColdDaysSevere_lag9 <- RICselect[RICselect$ColdLag9 == 1, ]
RICColdDaysSevere_lag10 <- RICselect[RICselect$ColdLag10 == 1, ]
RICColdDaysSevere_lag11 <- RICselect[RICselect$ColdLag11 == 1, ]
RICColdDaysSevere_lag12 <- RICselect[RICselect$ColdLag12 == 1, ]
RICColdDaysSevere_lag13 <- RICselect[RICselect$ColdLag13 == 1, ]

RICColdDaysSevere_LagCombine <- rbind(RICColdDaysSevere_lag0,RICColdDaysSevere_lag1,RICColdDaysSevere_lag2,RICColdDaysSevere_lag3,RICColdDaysSevere_lag4,RICColdDaysSevere_lag5,RICColdDaysSevere_lag6,RICColdDaysSevere_lag7,RICColdDaysSevere_lag8,RICColdDaysSevere_lag9,RICColdDaysSevere_lag10,RICColdDaysSevere_lag11,RICColdDaysSevere_lag12,RICColdDaysSevere_lag13)
RICColdDaysSevere_LagCombine_DupsRem <- RICColdDaysSevere_LagCombine[!duplicated(RICColdDaysSevere_LagCombine), ]
RICColdDays <- RICColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

RICColdDays_Winter <- RICColdDays[!(3 < month(RICColdDays$Date) & month(RICColdDays$Date) < 10), ]
RICColdDays_Winter$Date_Update <- format(as.Date(RICColdDays_Winter$Date), "%Y-%m-%d")

rm(RICColdDaysSevere_lag0,RICColdDaysSevere_lag1,RICColdDaysSevere_lag2,RICColdDaysSevere_lag3,RICColdDaysSevere_lag4,RICColdDaysSevere_lag5,RICColdDaysSevere_lag6,RICColdDaysSevere_lag7,RICColdDaysSevere_lag8,RICColdDaysSevere_lag9,RICColdDaysSevere_lag10,RICColdDaysSevere_lag11,RICColdDaysSevere_lag12,RICColdDaysSevere_lag13,RICColdDaysSevere_LagCombine,RICColdDaysSevere_LagCombine_DupsRem,RICColdDays)

RIC_MDC <- MDC_FullFinal %>%
  filter(Wx == "RIC")

RIC_MDC_Cold_NAs <- left_join(RICColdDays_Winter, 
                              RIC_MDC,
                              by = c("Date_Update" = "Date"))
RIC_MDC_Cold <- RIC_MDC_Cold_NAs[!is.na(RIC_MDC_Cold_NAs$MDC_ACME),]
#NAOnly_Cold <- RIC_MDC_Cold[is.na(RIC_MDC_Cold$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(RIC_MDC_Cold$ColdLag0 == 0 & RIC_MDC_Cold$ColdLag1 == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(RIC_MDC_Cold$Station) / length(RIC_MDC$Date) #  8.74% of deaths happen on Cold Days
length(RICColdDays_Winter$Station) / length(RIC$Station) # 8.322% of days are Cold day lag0 or lag1

####### BootStrapping RIC

RICNotColdDaysSevere_lag0 <- RICselect[!(RICselect$ColdLag0 == 1), ]
RICNotColdDaysSevere_lag1 <- RICselect[!(RICselect$ColdLag1 == 1), ]
RICNotColdDaysSevere_lag2 <- RICselect[!(RICselect$ColdLag2 == 1), ]
RICNotColdDaysSevere_lag3 <- RICselect[!(RICselect$ColdLag3 == 1), ]
RICNotColdDaysSevere_lag4 <- RICselect[!(RICselect$ColdLag4 == 1), ]
RICNotColdDaysSevere_lag5 <- RICselect[!(RICselect$ColdLag5 == 1), ]
RICNotColdDaysSevere_lag6 <- RICselect[!(RICselect$ColdLag6 == 1), ]
RICNotColdDaysSevere_lag7 <- RICselect[!(RICselect$ColdLag7 == 1), ]
RICNotColdDaysSevere_lag8 <- RICselect[!(RICselect$ColdLag8 == 1), ]
RICNotColdDaysSevere_lag9 <- RICselect[!(RICselect$ColdLag9 == 1), ]
RICNotColdDaysSevere_lag10 <- RICselect[!(RICselect$ColdLag10 == 1), ]
RICNotColdDaysSevere_lag11 <- RICselect[!(RICselect$ColdLag11 == 1), ]
RICNotColdDaysSevere_lag12 <- RICselect[!(RICselect$ColdLag12 == 1), ]
RICNotColdDaysSevere_lag13 <- RICselect[!(RICselect$ColdLag13 == 1), ]

RICNotColdDaysSevere_LagCombine <- rbind(RICNotColdDaysSevere_lag0,RICNotColdDaysSevere_lag1,RICNotColdDaysSevere_lag2,RICNotColdDaysSevere_lag3,RICNotColdDaysSevere_lag4,RICNotColdDaysSevere_lag5,RICNotColdDaysSevere_lag6,RICNotColdDaysSevere_lag7,RICNotColdDaysSevere_lag8,RICNotColdDaysSevere_lag9,RICNotColdDaysSevere_lag10,RICNotColdDaysSevere_lag11,RICNotColdDaysSevere_lag12,RICNotColdDaysSevere_lag13)
RICNotColdDaysSevere_LagCombine_DupsRem <- RICNotColdDaysSevere_LagCombine[!duplicated(RICNotColdDaysSevere_LagCombine), ]
RICNotColdDays <- RICNotColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

RICNotColdDays_Winter <- RICNotColdDays[!(3 < month(RICNotColdDays$Date) & month(RICNotColdDays$Date) < 10), ]
RICNotColdDays_Winter$Date_Update <- format(as.Date(RICNotColdDays_Winter$Date), "%Y-%m-%d")

rm(RICNotColdDaysSevere_lag0,RICNotColdDaysSevere_lag1,RICNotColdDaysSevere_lag2,RICNotColdDaysSevere_lag3,RICNotColdDaysSevere_lag4,RICNotColdDaysSevere_lag5,RICNotColdDaysSevere_lag6,RICNotColdDaysSevere_lag7,RICNotColdDaysSevere_lag8,RICNotColdDaysSevere_lag9,RICNotColdDaysSevere_lag10,RICNotColdDaysSevere_lag11,RICNotColdDaysSevere_lag12,RICNotColdDaysSevere_lag13,RICNotColdDaysSevere_LagCombine,RICNotColdDaysSevere_LagCombine_DupsRem,RICNotColdDays)

#test <- RICNotColdDays_Winter[RICNotColdDays_Winter$Month == 3, ]

RIC_MDC_NotCold_NAs <- left_join(RICNotColdDays_Winter, 
                                 RIC_MDC,
                                 by = c("Date_Update" = "Date"))
RIC_MDC_NotCold <- RIC_MDC_NotCold_NAs[!is.na(RIC_MDC_NotCold_NAs$MDC_ACME),]
#NAOnly_NotCold <- RIC_MDC_NotCold[is.na(RIC_MDC_NotCold$MDC_ACME), ] #0 obs. so yay!


RIC_MDC_NotCold_Slim <- RIC_MDC_NotCold %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotCold_Slim <- RIC_MDC_NotCold_Slim[is.na(RIC_MDC_NotCold_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   RICNoStressRandom <- RIC_MDC_NotCold_Slim %>%
#     sample_n(length(RIC_MDC_Cold$Month),replace = TRUE)
#   write.csv(RICNoStressRandom,paste0("~/Desktop/NoStressRandomWx/RIC_v3_Cold/RIC",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

RIC1 <- read.csv("~/Desktop/NoStressRandomWx/RIC_v3_Cold/RIC1")
RIC2 <- read.csv("~/Desktop/NoStressRandomWx/RIC_v3_Cold/RIC2")
ifelse(length(RIC1$X) == length(RIC_MDC_Cold$Month),1,0)
rm(RIC1,RIC2)

RIC_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/RIC_v3_Cold/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#RIC_data_all                                            # Print data to RStudio console

# Group by MDC

RIC_MDC_NotCold_Counts_InitialGrouping <- RIC_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
RIC_MDC_NotCold_Counts_InitialGrouping$Adjusted <- RIC_MDC_NotCold_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/RIC_v3_Cold/",  # Identify all CSV files
                                                                                                                                 pattern = "*", full.names = TRUE))) # Divide by number i
RIC_MDC_NotCold_Counts <- data.frame(cbind(RIC_MDC_NotCold_Counts_InitialGrouping$MDC_ACME,RIC_MDC_NotCold_Counts_InitialGrouping$Adjusted))

colnames(RIC_MDC_NotCold_Counts) <- c("Boot_MDC_ACME","Boot_Count")
RIC_MDC_NotCold_Counts$Boot_Count <- as.numeric(RIC_MDC_NotCold_Counts$Boot_Count)
sum(RIC_MDC_NotCold_Counts$Boot_Count)

#RIC_MDC_NotCold_Counts[RIC_MDC_NotCold_Counts$MDCAcme == "MDC22", 2] <- 0

RIC_MDC_Cold_Counts <- RIC_MDC_Cold %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# RIC_MDC_Cold_Counts[(length(RIC_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# RIC_MDC_Cold_Counts[(length(RIC_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# RIC_MDC_Cold_Counts[(length(RIC_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# RIC_MDC_Cold_Counts[(length(RIC_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# RIC_MDC_Cold_Counts[(length(RIC_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# RIC_MDC_Cold_Counts[(length(RIC_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# RIC_MDC_Cold_Counts[(length(RIC_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# RIC_MDC_Cold_Counts[(length(RIC_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)

rm(RIC_data_all,RIC_MDC_Cold_NAs,RIC_MDC_NotCold_NAs,RIC_MDC_NotCold_Slim)

#Final Prep for Comparison
RIC_Ordered_NotCold_Counts <- RIC_MDC_NotCold_Counts[order(RIC_MDC_NotCold_Counts$Boot_MDC_ACME), ]
RIC_Ordered_Cold_Counts <- RIC_MDC_Cold_Counts[order(RIC_MDC_Cold_Counts$MDC_ACME), ]

#RIC_Final_Cold <- RIC_Ordered_Cold_Counts[RIC_Ordered_Cold_Counts$count_by_siteyear > 5, ]
RIC_Ordered_NotCold_Expected_Counts <- RIC_Ordered_NotCold_Counts[RIC_Ordered_NotCold_Counts$Boot_Count > 5, ]

# ifelse(length(RIC_NotCold_Counts_Ordered$Boot_MDC_ACME) == length(RIC_Cold_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(RIC_NotCold_Counts_Ordered$Boot_Count) == sum(RIC_Cold_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackRIC <- cbind(RIC_NotCold_Counts_Ordered,RIC_Cold_Counts_Ordered)
CountsOnlyRIC <- right_join(RIC_Ordered_Cold_Counts , RIC_Ordered_NotCold_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackRIC <- CountsOnlyRIC[!is.na(CountsOnlyRIC$Boot_Count) | !is.na(CountsOnlyRIC$count_by_siteyear),]
rm(CountsOnlyRIC)

sum(BackToBackRIC$count_by_siteyear)
sum(BackToBackRIC$Boot_Count)

colnames(BackToBackRIC) <- c("MDC_ACME","Cold","Boot")

BackToBackRIC$Boot <- as.numeric(BackToBackRIC$Boot)
sum(BackToBackRIC$Boot)
sum(BackToBackRIC$Cold)

BackToBackRIC_GreaterThanZero_Cold <- BackToBackRIC[BackToBackRIC$Boot > 0, ]

sum(BackToBackRIC_GreaterThanZero_Cold$Cold)
sum(BackToBackRIC_GreaterThanZero_Cold$Boot)

EO2_RIC <- (BackToBackRIC_GreaterThanZero_Cold$Cold - BackToBackRIC_GreaterThanZero_Cold$Boot)^2
E_RIC <- BackToBackRIC_GreaterThanZero_Cold$Boot
EO2E_RIC <- EO2_RIC/E_RIC
sumEO2E_RIC <- round(sum(EO2E_RIC),digits = 2)
sum(EO2E_RIC)
df_RIC <- length(EO2_RIC) - 1
#CritValue_df18 = 28.869
#CritValue_df19 = 30.144
#CritValue_df20 = 31.410
#CritValue_df21 = 32.671

# CSByHandTable <- data.frame(matrix(nrow=1,ncol=1))
# CSByHandTable$RICCSValue <- sumEO2E_RIC

EO_Diff_RIC <- (BackToBackRIC_GreaterThanZero_Cold$Cold - BackToBackRIC_GreaterThanZero_Cold$Boot)
STD_RES_RIC <- EO_Diff_RIC / (sqrt(BackToBackRIC_GreaterThanZero_Cold$Boot))
BackToBackRIC_GreaterThanZero_Cold$Res <- round(STD_RES_RIC,digits = 3)
BackToBackRIC_GreaterThanZero_Cold$ResSig <- ifelse(abs(BackToBackRIC_GreaterThanZero_Cold$Res) > 3.8, TRUE, print("F"))

#ROA

ROA <- read_excel("~/Desktop/ROA.Final.Weather.xlsx",na="NA")
ROAselect <- ROA[1:5844, ]

ROAColdDaysSevere_lag0 <- ROAselect[ROAselect$ColdLag0 == 1, ]
ROAColdDaysSevere_lag1 <- ROAselect[ROAselect$ColdLag1 == 1, ]
ROAColdDaysSevere_lag2 <- ROAselect[ROAselect$ColdLag2 == 1, ]
ROAColdDaysSevere_lag3 <- ROAselect[ROAselect$ColdLag3 == 1, ]
ROAColdDaysSevere_lag4 <- ROAselect[ROAselect$ColdLag4 == 1, ]
ROAColdDaysSevere_lag5 <- ROAselect[ROAselect$ColdLag5 == 1, ]
ROAColdDaysSevere_lag6 <- ROAselect[ROAselect$ColdLag6 == 1, ]
ROAColdDaysSevere_lag7 <- ROAselect[ROAselect$ColdLag7 == 1, ]
ROAColdDaysSevere_lag8 <- ROAselect[ROAselect$ColdLag8 == 1, ]
ROAColdDaysSevere_lag9 <- ROAselect[ROAselect$ColdLag9 == 1, ]
ROAColdDaysSevere_lag10 <- ROAselect[ROAselect$ColdLag10 == 1, ]

ROAColdDaysSevere_LagCombine <- rbind(ROAColdDaysSevere_lag0,ROAColdDaysSevere_lag1,ROAColdDaysSevere_lag2,ROAColdDaysSevere_lag3,ROAColdDaysSevere_lag4,ROAColdDaysSevere_lag5,ROAColdDaysSevere_lag6,ROAColdDaysSevere_lag7,ROAColdDaysSevere_lag8,ROAColdDaysSevere_lag9,ROAColdDaysSevere_lag10)
ROAColdDaysSevere_LagCombine_DupsRem <- ROAColdDaysSevere_LagCombine[!duplicated(ROAColdDaysSevere_LagCombine), ]
ROAColdDays <- ROAColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

ROAColdDays_Winter <- ROAColdDays[!(3 < month(ROAColdDays$Date) & month(ROAColdDays$Date) < 10), ]
ROAColdDays_Winter$Date_Update <- format(as.Date(ROAColdDays_Winter$Date), "%Y-%m-%d")

rm(ROAColdDaysSevere_lag0,ROAColdDaysSevere_lag1,ROAColdDaysSevere_lag2,ROAColdDaysSevere_lag3,ROAColdDaysSevere_lag4,ROAColdDaysSevere_lag5,ROAColdDaysSevere_lag6,ROAColdDaysSevere_lag7,ROAColdDaysSevere_lag8,ROAColdDaysSevere_lag9,ROAColdDaysSevere_lag10,ROAColdDaysSevere_LagCombine,ROAColdDaysSevere_LagCombine_DupsRem,ROAColdDays)

ROA_MDC <- MDC_FullFinal %>%
  filter(Wx == "ROA")

ROA_MDC_Cold_NAs <- left_join(ROAColdDays_Winter, 
                              ROA_MDC,
                              by = c("Date_Update" = "Date"))
ROA_MDC_Cold <- ROA_MDC_Cold_NAs[!is.na(ROA_MDC_Cold_NAs$MDC_ACME),]
#NAOnly_Cold <- ROA_MDC_Cold[is.na(ROA_MDC_Cold$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(ROA_MDC_Cold$ColdLag0 == 0 & ROA_MDC_Cold$ColdLag1 == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(ROA_MDC_Cold$Station) / length(ROA_MDC$Date) #  8.74% of deaths happen on Cold Days
length(ROAColdDays_Winter$Station) / length(ROA$Station) # 8.322% of days are Cold day lag0 or lag1

####### BootStrapping ROA

ROANotColdDaysSevere_lag0 <- ROAselect[!(ROAselect$ColdLag0 == 1), ]
ROANotColdDaysSevere_lag1 <- ROAselect[!(ROAselect$ColdLag1 == 1), ]
ROANotColdDaysSevere_lag2 <- ROAselect[!(ROAselect$ColdLag2 == 1), ]
ROANotColdDaysSevere_lag3 <- ROAselect[!(ROAselect$ColdLag3 == 1), ]
ROANotColdDaysSevere_lag4 <- ROAselect[!(ROAselect$ColdLag4 == 1), ]
ROANotColdDaysSevere_lag5 <- ROAselect[!(ROAselect$ColdLag5 == 1), ]
ROANotColdDaysSevere_lag6 <- ROAselect[!(ROAselect$ColdLag6 == 1), ]
ROANotColdDaysSevere_lag7 <- ROAselect[!(ROAselect$ColdLag7 == 1), ]
ROANotColdDaysSevere_lag8 <- ROAselect[!(ROAselect$ColdLag8 == 1), ]
ROANotColdDaysSevere_lag9 <- ROAselect[!(ROAselect$ColdLag9 == 1), ]
ROANotColdDaysSevere_lag10 <- ROAselect[!(ROAselect$ColdLag10 == 1), ]

ROANotColdDaysSevere_LagCombine <- rbind(ROANotColdDaysSevere_lag0,ROANotColdDaysSevere_lag1,ROANotColdDaysSevere_lag2,ROANotColdDaysSevere_lag3,ROANotColdDaysSevere_lag4,ROANotColdDaysSevere_lag5,ROANotColdDaysSevere_lag6,ROANotColdDaysSevere_lag7,ROANotColdDaysSevere_lag8,ROANotColdDaysSevere_lag9,ROANotColdDaysSevere_lag10)
ROANotColdDaysSevere_LagCombine_DupsRem <- ROANotColdDaysSevere_LagCombine[!duplicated(ROANotColdDaysSevere_LagCombine), ]
ROANotColdDays <- ROANotColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

ROANotColdDays_Winter <- ROANotColdDays[!(3 < month(ROANotColdDays$Date) & month(ROANotColdDays$Date) < 10), ]
ROANotColdDays_Winter$Date_Update <- format(as.Date(ROANotColdDays_Winter$Date), "%Y-%m-%d")

rm(ROANotColdDaysSevere_lag0,ROANotColdDaysSevere_lag1,ROANotColdDaysSevere_lag2,ROANotColdDaysSevere_lag3,ROANotColdDaysSevere_lag4,ROANotColdDaysSevere_lag5,ROANotColdDaysSevere_lag6,ROANotColdDaysSevere_lag7,ROANotColdDaysSevere_lag8,ROANotColdDaysSevere_lag9,ROANotColdDaysSevere_lag10,ROANotColdDaysSevere_LagCombine,ROANotColdDaysSevere_LagCombine_DupsRem,ROANotColdDays)

#test <- ROANotColdDays_Winter[ROANotColdDays_Winter$Month == 3, ]

ROA_MDC_NotCold_NAs <- left_join(ROANotColdDays_Winter, 
                                 ROA_MDC,
                                 by = c("Date_Update" = "Date"))
ROA_MDC_NotCold <- ROA_MDC_NotCold_NAs[!is.na(ROA_MDC_NotCold_NAs$MDC_ACME),]
#NAOnly_NotCold <- ROA_MDC_NotCold[is.na(ROA_MDC_NotCold$MDC_ACME), ] #0 obs. so yay!


ROA_MDC_NotCold_Slim <- ROA_MDC_NotCold %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotCold_Slim <- ROA_MDC_NotCold_Slim[is.na(ROA_MDC_NotCold_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   ROANoStressRandom <- ROA_MDC_NotCold_Slim %>%
#     sample_n(length(ROA_MDC_Cold$Month),replace = TRUE)
#   write.csv(ROANoStressRandom,paste0("~/Desktop/NoStressRandomWx/ROA_v3_Cold/ROA",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

ROA1 <- read.csv("~/Desktop/NoStressRandomWx/ROA_v3_Cold/ROA1")
ROA2 <- read.csv("~/Desktop/NoStressRandomWx/ROA_v3_Cold/ROA2")
ifelse(length(ROA1$X) == length(ROA_MDC_Cold$Month),1,0)
rm(ROA1,ROA2)

ROA_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/ROA_v3_Cold/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#ROA_data_all                                            # Print data to RStudio console

# Group by MDC

ROA_MDC_NotCold_Counts_InitialGrouping <- ROA_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
ROA_MDC_NotCold_Counts_InitialGrouping$Adjusted <- ROA_MDC_NotCold_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/ROA_v3_Cold/",  # Identify all CSV files
                                                                                                                                 pattern = "*", full.names = TRUE))) # Divide by number i
ROA_MDC_NotCold_Counts <- data.frame(cbind(ROA_MDC_NotCold_Counts_InitialGrouping$MDC_ACME,ROA_MDC_NotCold_Counts_InitialGrouping$Adjusted))

colnames(ROA_MDC_NotCold_Counts) <- c("Boot_MDC_ACME","Boot_Count")
ROA_MDC_NotCold_Counts$Boot_Count <- as.numeric(ROA_MDC_NotCold_Counts$Boot_Count)
sum(ROA_MDC_NotCold_Counts$Boot_Count)

#ROA_MDC_NotCold_Counts[ROA_MDC_NotCold_Counts$MDCAcme == "MDC22", 2] <- 0

ROA_MDC_Cold_Counts <- ROA_MDC_Cold %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# ROA_MDC_Cold_Counts[(length(ROA_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# ROA_MDC_Cold_Counts[(length(ROA_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# ROA_MDC_Cold_Counts[(length(ROA_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# ROA_MDC_Cold_Counts[(length(ROA_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# ROA_MDC_Cold_Counts[(length(ROA_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# ROA_MDC_Cold_Counts[(length(ROA_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# ROA_MDC_Cold_Counts[(length(ROA_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# ROA_MDC_Cold_Counts[(length(ROA_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)

rm(ROA_data_all,ROA_MDC_Cold_NAs,ROA_MDC_NotCold_NAs,ROA_MDC_NotCold_Slim)

#Final Prep for Comparison
ROA_Ordered_NotCold_Counts <- ROA_MDC_NotCold_Counts[order(ROA_MDC_NotCold_Counts$Boot_MDC_ACME), ]
ROA_Ordered_Cold_Counts <- ROA_MDC_Cold_Counts[order(ROA_MDC_Cold_Counts$MDC_ACME), ]

#ROA_Final_Cold <- ROA_Ordered_Cold_Counts[ROA_Ordered_Cold_Counts$count_by_siteyear > 5, ]
ROA_Ordered_NotCold_Expected_Counts <- ROA_Ordered_NotCold_Counts[ROA_Ordered_NotCold_Counts$Boot_Count > 5, ]

# ifelse(length(ROA_NotCold_Counts_Ordered$Boot_MDC_ACME) == length(ROA_Cold_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(ROA_NotCold_Counts_Ordered$Boot_Count) == sum(ROA_Cold_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackROA <- cbind(ROA_NotCold_Counts_Ordered,ROA_Cold_Counts_Ordered)
CountsOnlyROA <- right_join(ROA_Ordered_Cold_Counts , ROA_Ordered_NotCold_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackROA <- CountsOnlyROA[!is.na(CountsOnlyROA$Boot_Count) | !is.na(CountsOnlyROA$count_by_siteyear),]
rm(CountsOnlyROA)

sum(BackToBackROA$count_by_siteyear)
sum(BackToBackROA$Boot_Count)

colnames(BackToBackROA) <- c("MDC_ACME","Cold","Boot")

BackToBackROA$Boot <- as.numeric(BackToBackROA$Boot)
sum(BackToBackROA$Boot)
sum(BackToBackROA$Cold)

BackToBackROA_GreaterThanZero_Cold <- BackToBackROA[BackToBackROA$Boot > 0, ]

sum(BackToBackROA_GreaterThanZero_Cold$Cold)
sum(BackToBackROA_GreaterThanZero_Cold$Boot)

EO2_ROA <- (BackToBackROA_GreaterThanZero_Cold$Cold - BackToBackROA_GreaterThanZero_Cold$Boot)^2
E_ROA <- BackToBackROA_GreaterThanZero_Cold$Boot
EO2E_ROA <- EO2_ROA/E_ROA
sumEO2E_ROA <- round(sum(EO2E_ROA),digits = 2)
sum(EO2E_ROA)
df_ROA <- length(EO2_ROA) - 1
#CritValue_df18 = 28.869
#CritValue_df19 = 30.144
#CritValue_df20 = 31.410
#CritValue_df21 = 32.671

# CSByHandTable <- data.frame(matrix(nrow=1,ncol=1))
# CSByHandTable$ROACSValue <- sumEO2E_ROA

EO_Diff_ROA <- (BackToBackROA_GreaterThanZero_Cold$Cold - BackToBackROA_GreaterThanZero_Cold$Boot)
STD_RES_ROA <- EO_Diff_ROA / (sqrt(BackToBackROA_GreaterThanZero_Cold$Boot))
BackToBackROA_GreaterThanZero_Cold$Res <- round(STD_RES_ROA,digits = 3)
BackToBackROA_GreaterThanZero_Cold$ResSig <- ifelse(abs(BackToBackROA_GreaterThanZero_Cold$Res) > 3.8, TRUE, print("F"))

#SHD

SHD <- read_excel("~/Desktop/SHD.Final.Weather.xlsx",na="NA")
SHDselect <- SHD[1:5844, ]

SHDColdDaysSevere_lag0 <- SHDselect[SHDselect$ColdLag0 == 1, ]
SHDColdDaysSevere_lag1 <- SHDselect[SHDselect$ColdLag1 == 1, ]
SHDColdDaysSevere_lag2 <- SHDselect[SHDselect$ColdLag2 == 1, ]
SHDColdDaysSevere_lag3 <- SHDselect[SHDselect$ColdLag3 == 1, ]
SHDColdDaysSevere_lag4 <- SHDselect[SHDselect$ColdLag4 == 1, ]

SHDColdDaysSevere_LagCombine <- rbind(SHDColdDaysSevere_lag0,SHDColdDaysSevere_lag1,SHDColdDaysSevere_lag2,SHDColdDaysSevere_lag3,SHDColdDaysSevere_lag4)
SHDColdDaysSevere_LagCombine_DupsRem <- SHDColdDaysSevere_LagCombine[!duplicated(SHDColdDaysSevere_LagCombine), ]
SHDColdDays <- SHDColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

SHDColdDays_Winter <- SHDColdDays[!(3 < month(SHDColdDays$Date) & month(SHDColdDays$Date) < 10), ]
SHDColdDays_Winter$Date_Update <- format(as.Date(SHDColdDays_Winter$Date), "%Y-%m-%d")

rm(SHDColdDaysSevere_lag0,SHDColdDaysSevere_lag1,SHDColdDaysSevere_lag2,SHDColdDaysSevere_lag3,SHDColdDaysSevere_lag4,SHDColdDaysSevere_LagCombine,SHDColdDaysSevere_LagCombine_DupsRem,SHDColdDays)

SHD_MDC <- MDC_FullFinal %>%
  filter(Wx == "SHD")

SHD_MDC_Cold_NAs <- left_join(SHDColdDays_Winter, 
                              SHD_MDC,
                              by = c("Date_Update" = "Date"))
SHD_MDC_Cold <- SHD_MDC_Cold_NAs[!is.na(SHD_MDC_Cold_NAs$MDC_ACME),]
#NAOnly_Cold <- SHD_MDC_Cold[is.na(SHD_MDC_Cold$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(SHD_MDC_Cold$ColdLag0 == 0 & SHD_MDC_Cold$ColdLag1 == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(SHD_MDC_Cold$Station) / length(SHD_MDC$Date) #  8.74% of deaths happen on Cold Days
length(SHDColdDays_Winter$Station) / length(SHD$Station) # 8.322% of days are Cold day lag0 or lag1

####### BootStrapping SHD

SHDNotColdDaysSevere_lag0 <- SHDselect[!(SHDselect$ColdLag0 == 1), ]
SHDNotColdDaysSevere_lag1 <- SHDselect[!(SHDselect$ColdLag1 == 1), ]
SHDNotColdDaysSevere_lag2 <- SHDselect[!(SHDselect$ColdLag2 == 1), ]
SHDNotColdDaysSevere_lag3 <- SHDselect[!(SHDselect$ColdLag3 == 1), ]
SHDNotColdDaysSevere_lag4 <- SHDselect[!(SHDselect$ColdLag4 == 1), ]

SHDNotColdDaysSevere_LagCombine <- rbind(SHDNotColdDaysSevere_lag0,SHDNotColdDaysSevere_lag1,SHDNotColdDaysSevere_lag2,SHDNotColdDaysSevere_lag3,SHDNotColdDaysSevere_lag4)
SHDNotColdDaysSevere_LagCombine_DupsRem <- SHDNotColdDaysSevere_LagCombine[!duplicated(SHDNotColdDaysSevere_LagCombine), ]
SHDNotColdDays <- SHDNotColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

SHDNotColdDays_Winter <- SHDNotColdDays[!(3 < month(SHDNotColdDays$Date) & month(SHDNotColdDays$Date) < 10), ]
SHDNotColdDays_Winter$Date_Update <- format(as.Date(SHDNotColdDays_Winter$Date), "%Y-%m-%d")

rm(SHDNotColdDaysSevere_lag0,SHDNotColdDaysSevere_lag1,SHDNotColdDaysSevere_lag2,SHDNotColdDaysSevere_lag3,SHDNotColdDaysSevere_lag4,SHDNotColdDaysSevere_LagCombine,SHDNotColdDaysSevere_LagCombine_DupsRem,SHDNotColdDays)

#test <- SHDNotColdDays_Winter[SHDNotColdDays_Winter$Month == 3, ]

SHD_MDC_NotCold_NAs <- left_join(SHDNotColdDays_Winter, 
                                 SHD_MDC,
                                 by = c("Date_Update" = "Date"))
SHD_MDC_NotCold <- SHD_MDC_NotCold_NAs[!is.na(SHD_MDC_NotCold_NAs$MDC_ACME),]
#NAOnly_NotCold <- SHD_MDC_NotCold[is.na(SHD_MDC_NotCold$MDC_ACME), ] #0 obs. so yay!


SHD_MDC_NotCold_Slim <- SHD_MDC_NotCold %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotCold_Slim <- SHD_MDC_NotCold_Slim[is.na(SHD_MDC_NotCold_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   SHDNoStressRandom <- SHD_MDC_NotCold_Slim %>%
#     sample_n(length(SHD_MDC_Cold$Month),replace = TRUE)
#   write.csv(SHDNoStressRandom,paste0("~/Desktop/NoStressRandomWx/SHD_v3_Cold/SHD",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

SHD1 <- read.csv("~/Desktop/NoStressRandomWx/SHD_v3_Cold/SHD1")
SHD2 <- read.csv("~/Desktop/NoStressRandomWx/SHD_v3_Cold/SHD2")
ifelse(length(SHD1$X) == length(SHD_MDC_Cold$Month),1,0)
rm(SHD1,SHD2)

SHD_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/SHD_v3_Cold/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#SHD_data_all                                            # Print data to RStudio console

# Group by MDC

SHD_MDC_NotCold_Counts_InitialGrouping <- SHD_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
SHD_MDC_NotCold_Counts_InitialGrouping$Adjusted <- SHD_MDC_NotCold_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/SHD_v3_Cold/",  # Identify all CSV files
                                                                                                                                 pattern = "*", full.names = TRUE))) # Divide by number i
SHD_MDC_NotCold_Counts <- data.frame(cbind(SHD_MDC_NotCold_Counts_InitialGrouping$MDC_ACME,SHD_MDC_NotCold_Counts_InitialGrouping$Adjusted))

colnames(SHD_MDC_NotCold_Counts) <- c("Boot_MDC_ACME","Boot_Count")
SHD_MDC_NotCold_Counts$Boot_Count <- as.numeric(SHD_MDC_NotCold_Counts$Boot_Count)
sum(SHD_MDC_NotCold_Counts$Boot_Count)

#SHD_MDC_NotCold_Counts[SHD_MDC_NotCold_Counts$MDCAcme == "MDC22", 2] <- 0

SHD_MDC_Cold_Counts <- SHD_MDC_Cold %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# SHD_MDC_Cold_Counts[(length(SHD_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# SHD_MDC_Cold_Counts[(length(SHD_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# SHD_MDC_Cold_Counts[(length(SHD_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# SHD_MDC_Cold_Counts[(length(SHD_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# SHD_MDC_Cold_Counts[(length(SHD_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# SHD_MDC_Cold_Counts[(length(SHD_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# SHD_MDC_Cold_Counts[(length(SHD_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# SHD_MDC_Cold_Counts[(length(SHD_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)

rm(SHD_data_all,SHD_MDC_Cold_NAs,SHD_MDC_NotCold_NAs,SHD_MDC_NotCold_Slim)

#Final Prep for Comparison
SHD_Ordered_NotCold_Counts <- SHD_MDC_NotCold_Counts[order(SHD_MDC_NotCold_Counts$Boot_MDC_ACME), ]
SHD_Ordered_Cold_Counts <- SHD_MDC_Cold_Counts[order(SHD_MDC_Cold_Counts$MDC_ACME), ]

#SHD_Final_Cold <- SHD_Ordered_Cold_Counts[SHD_Ordered_Cold_Counts$count_by_siteyear > 5, ]
SHD_Ordered_NotCold_Expected_Counts <- SHD_Ordered_NotCold_Counts[SHD_Ordered_NotCold_Counts$Boot_Count > 5, ]

# ifelse(length(SHD_NotCold_Counts_Ordered$Boot_MDC_ACME) == length(SHD_Cold_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(SHD_NotCold_Counts_Ordered$Boot_Count) == sum(SHD_Cold_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackSHD <- cbind(SHD_NotCold_Counts_Ordered,SHD_Cold_Counts_Ordered)
CountsOnlySHD <- right_join(SHD_Ordered_Cold_Counts , SHD_Ordered_NotCold_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackSHD <- CountsOnlySHD[!is.na(CountsOnlySHD$Boot_Count) | !is.na(CountsOnlySHD$count_by_siteyear),]
rm(CountsOnlySHD)

sum(BackToBackSHD$count_by_siteyear)
sum(BackToBackSHD$Boot_Count)

colnames(BackToBackSHD) <- c("MDC_ACME","Cold","Boot")

BackToBackSHD$Boot <- as.numeric(BackToBackSHD$Boot)
sum(BackToBackSHD$Boot)
sum(BackToBackSHD$Cold)

BackToBackSHD_GreaterThanZero_Cold <- BackToBackSHD[BackToBackSHD$Boot > 0, ]

sum(BackToBackSHD_GreaterThanZero_Cold$Cold)
sum(BackToBackSHD_GreaterThanZero_Cold$Boot)

EO2_SHD <- (BackToBackSHD_GreaterThanZero_Cold$Cold - BackToBackSHD_GreaterThanZero_Cold$Boot)^2
E_SHD <- BackToBackSHD_GreaterThanZero_Cold$Boot
EO2E_SHD <- EO2_SHD/E_SHD
sumEO2E_SHD <- round(sum(EO2E_SHD),digits = 2)
sum(EO2E_SHD)
df_SHD <- length(EO2_SHD) - 1
#CritValue_df18 = 28.869
#CritValue_df19 = 30.144
#CritValue_df20 = 31.410
#CritValue_df21 = 32.671

# CSByHandTable <- data.frame(matrix(nrow=1,ncol=1))
# CSByHandTable$SHDCSValue <- sumEO2E_SHD

EO_Diff_SHD <- (BackToBackSHD_GreaterThanZero_Cold$Cold - BackToBackSHD_GreaterThanZero_Cold$Boot)
STD_RES_SHD <- EO_Diff_SHD / (sqrt(BackToBackSHD_GreaterThanZero_Cold$Boot))
BackToBackSHD_GreaterThanZero_Cold$Res <- round(STD_RES_SHD,digits = 3)
BackToBackSHD_GreaterThanZero_Cold$ResSig <- ifelse(abs(BackToBackSHD_GreaterThanZero_Cold$Res) > 3.8, TRUE, print("F"))

#VJI

VJI <- read_excel("~/Desktop/VJI.Final.Weather.xlsx",na="NA")
VJIselect <- VJI[1:5844, ]

VJIColdDaysSevere_lag1 <- VJIselect[VJIselect$ColdLag1 == 1, ]
VJIColdDaysSevere_lag2 <- VJIselect[VJIselect$ColdLag2 == 1, ]
VJIColdDaysSevere_lag3 <- VJIselect[VJIselect$ColdLag3 == 1, ]
VJIColdDaysSevere_lag4 <- VJIselect[VJIselect$ColdLag4 == 1, ]
VJIColdDaysSevere_lag5 <- VJIselect[VJIselect$ColdLag5 == 1, ]
VJIColdDaysSevere_lag6 <- VJIselect[VJIselect$ColdLag6 == 1, ]
VJIColdDaysSevere_lag7 <- VJIselect[VJIselect$ColdLag7 == 1, ]
VJIColdDaysSevere_lag8 <- VJIselect[VJIselect$ColdLag8 == 1, ]
VJIColdDaysSevere_lag9 <- VJIselect[VJIselect$ColdLag9 == 1, ]
VJIColdDaysSevere_lag10 <- VJIselect[VJIselect$ColdLag10 == 1, ]
VJIColdDaysSevere_lag11 <- VJIselect[VJIselect$ColdLag11 == 1, ]
VJIColdDaysSevere_lag12 <- VJIselect[VJIselect$ColdLag12 == 1, ]
VJIColdDaysSevere_lag13 <- VJIselect[VJIselect$ColdLag13 == 1, ]
VJIColdDaysSevere_lag14 <- VJIselect[VJIselect$ColdLag14 == 1, ]
VJIColdDaysSevere_lag15 <- VJIselect[VJIselect$ColdLag15 == 1, ]

VJIColdDaysSevere_LagCombine <- rbind(VJIColdDaysSevere_lag1,VJIColdDaysSevere_lag2,VJIColdDaysSevere_lag3,VJIColdDaysSevere_lag4,VJIColdDaysSevere_lag5,VJIColdDaysSevere_lag6,VJIColdDaysSevere_lag7,VJIColdDaysSevere_lag8,VJIColdDaysSevere_lag9,VJIColdDaysSevere_lag10,VJIColdDaysSevere_lag11,VJIColdDaysSevere_lag12,VJIColdDaysSevere_lag13,VJIColdDaysSevere_lag14,VJIColdDaysSevere_lag15)
VJIColdDaysSevere_LagCombine_DupsRem <- VJIColdDaysSevere_LagCombine[!duplicated(VJIColdDaysSevere_LagCombine), ]
VJIColdDays <- VJIColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

VJIColdDays_Winter <- VJIColdDays[!(3 < month(VJIColdDays$Date) & month(VJIColdDays$Date) < 10), ]
VJIColdDays_Winter$Date_Update <- format(as.Date(VJIColdDays_Winter$Date), "%Y-%m-%d")

rm(VJIColdDaysSevere_lag1,VJIColdDaysSevere_lag2,VJIColdDaysSevere_lag3,VJIColdDaysSevere_lag4,VJIColdDaysSevere_lag5,VJIColdDaysSevere_lag6,VJIColdDaysSevere_lag7,VJIColdDaysSevere_lag8,VJIColdDaysSevere_lag9,VJIColdDaysSevere_lag10,VJIColdDaysSevere_lag11,VJIColdDaysSevere_lag12,VJIColdDaysSevere_lag13,VJIColdDaysSevere_lag14,VJIColdDaysSevere_lag15,VJIColdDaysSevere_LagCombine,VJIColdDaysSevere_LagCombine_DupsRem,VJIColdDays)

VJI_MDC <- MDC_FullFinal %>%
  filter(Wx == "VJI")

VJI_MDC_Cold_NAs <- left_join(VJIColdDays_Winter, 
                              VJI_MDC,
                              by = c("Date_Update" = "Date"))
VJI_MDC_Cold <- VJI_MDC_Cold_NAs[!is.na(VJI_MDC_Cold_NAs$MDC_ACME),]
#NAOnly_Cold <- VJI_MDC_Cold[is.na(VJI_MDC_Cold$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(VJI_MDC_Cold$ColdLag0 == 0 & VJI_MDC_Cold$ColdLag1 == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(VJI_MDC_Cold$Station) / length(VJI_MDC$Date) #  8.74% of deaths happen on Cold Days
length(VJIColdDays_Winter$Station) / length(VJI$Station) # 8.322% of days are Cold day lag0 or lag1

####### BootStrapping VJI

VJINotColdDaysSevere_lag1 <- VJIselect[!(VJIselect$ColdLag1 == 1), ]
VJINotColdDaysSevere_lag2 <- VJIselect[!(VJIselect$ColdLag2 == 1), ]
VJINotColdDaysSevere_lag3 <- VJIselect[!(VJIselect$ColdLag3 == 1), ]
VJINotColdDaysSevere_lag4 <- VJIselect[!(VJIselect$ColdLag4 == 1), ]
VJINotColdDaysSevere_lag5 <- VJIselect[!(VJIselect$ColdLag5 == 1), ]
VJINotColdDaysSevere_lag6 <- VJIselect[!(VJIselect$ColdLag6 == 1), ]
VJINotColdDaysSevere_lag7 <- VJIselect[!(VJIselect$ColdLag7 == 1), ]
VJINotColdDaysSevere_lag8 <- VJIselect[!(VJIselect$ColdLag8 == 1), ]
VJINotColdDaysSevere_lag9 <- VJIselect[!(VJIselect$ColdLag9 == 1), ]
VJINotColdDaysSevere_lag10 <- VJIselect[!(VJIselect$ColdLag10 == 1), ]
VJINotColdDaysSevere_lag11 <- VJIselect[!(VJIselect$ColdLag11 == 1), ]
VJINotColdDaysSevere_lag12 <- VJIselect[!(VJIselect$ColdLag12 == 1), ]
VJINotColdDaysSevere_lag13 <- VJIselect[!(VJIselect$ColdLag13 == 1), ]
VJINotColdDaysSevere_lag14 <- VJIselect[!(VJIselect$ColdLag14 == 1), ]
VJINotColdDaysSevere_lag15 <- VJIselect[!(VJIselect$ColdLag15 == 1), ]

VJINotColdDaysSevere_LagCombine <- rbind(VJINotColdDaysSevere_lag1,VJINotColdDaysSevere_lag2,VJINotColdDaysSevere_lag3,VJINotColdDaysSevere_lag4,VJINotColdDaysSevere_lag5,VJINotColdDaysSevere_lag6,VJINotColdDaysSevere_lag7,VJINotColdDaysSevere_lag8,VJINotColdDaysSevere_lag9,VJINotColdDaysSevere_lag10,VJINotColdDaysSevere_lag11,VJINotColdDaysSevere_lag12,VJINotColdDaysSevere_lag13,VJINotColdDaysSevere_lag14,VJINotColdDaysSevere_lag15)
VJINotColdDaysSevere_LagCombine_DupsRem <- VJINotColdDaysSevere_LagCombine[!duplicated(VJINotColdDaysSevere_LagCombine), ]
VJINotColdDays <- VJINotColdDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

VJINotColdDays_Winter <- VJINotColdDays[!(3 < month(VJINotColdDays$Date) & month(VJINotColdDays$Date) < 10), ]
VJINotColdDays_Winter$Date_Update <- format(as.Date(VJINotColdDays_Winter$Date), "%Y-%m-%d")

rm(VJINotColdDaysSevere_lag1,VJINotColdDaysSevere_lag2,VJINotColdDaysSevere_lag3,VJINotColdDaysSevere_lag4,VJINotColdDaysSevere_lag5,VJINotColdDaysSevere_lag6,VJINotColdDaysSevere_lag7,VJINotColdDaysSevere_lag8,VJINotColdDaysSevere_lag9,VJINotColdDaysSevere_lag10,VJINotColdDaysSevere_lag11,VJINotColdDaysSevere_lag12,VJINotColdDaysSevere_lag13,VJINotColdDaysSevere_lag14,VJINotColdDaysSevere_lag15,VJINotColdDaysSevere_LagCombine,VJINotColdDaysSevere_LagCombine_DupsRem,VJINotColdDays)

#test <- VJINotColdDays_Winter[VJINotColdDays_Winter$Month == 3, ]

VJI_MDC_NotCold_NAs <- left_join(VJINotColdDays_Winter, 
                                 VJI_MDC,
                                 by = c("Date_Update" = "Date"))
VJI_MDC_NotCold <- VJI_MDC_NotCold_NAs[!is.na(VJI_MDC_NotCold_NAs$MDC_ACME),]
#NAOnly_NotCold <- VJI_MDC_NotCold[is.na(VJI_MDC_NotCold$MDC_ACME), ] #0 obs. so yay!


VJI_MDC_NotCold_Slim <- VJI_MDC_NotCold %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotCold_Slim <- VJI_MDC_NotCold_Slim[is.na(VJI_MDC_NotCold_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   VJINoStressRandom <- VJI_MDC_NotCold_Slim %>%
#     sample_n(length(VJI_MDC_Cold$Month),replace = TRUE)
#   write.csv(VJINoStressRandom,paste0("~/Desktop/NoStressRandomWx/VJI_v3_Cold/VJI",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

VJI1 <- read.csv("~/Desktop/NoStressRandomWx/VJI_v3_Cold/VJI1")
VJI2 <- read.csv("~/Desktop/NoStressRandomWx/VJI_v3_Cold/VJI2")
ifelse(length(VJI1$X) == length(VJI_MDC_Cold$Month),1,0)
rm(VJI1,VJI2)

VJI_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/VJI_v3_Cold/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#VJI_data_all                                            # Print data to RStudio console

# Group by MDC

VJI_MDC_NotCold_Counts_InitialGrouping <- VJI_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
VJI_MDC_NotCold_Counts_InitialGrouping$Adjusted <- VJI_MDC_NotCold_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/VJI_v3_Cold/",  # Identify all CSV files
                                                                                                                                 pattern = "*", full.names = TRUE))) # Divide by number i
VJI_MDC_NotCold_Counts <- data.frame(cbind(VJI_MDC_NotCold_Counts_InitialGrouping$MDC_ACME,VJI_MDC_NotCold_Counts_InitialGrouping$Adjusted))

colnames(VJI_MDC_NotCold_Counts) <- c("Boot_MDC_ACME","Boot_Count")
VJI_MDC_NotCold_Counts$Boot_Count <- as.numeric(VJI_MDC_NotCold_Counts$Boot_Count)
sum(VJI_MDC_NotCold_Counts$Boot_Count)

#VJI_MDC_NotCold_Counts[VJI_MDC_NotCold_Counts$MDCAcme == "MDC22", 2] <- 0

VJI_MDC_Cold_Counts <- VJI_MDC_Cold %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# VJI_MDC_Cold_Counts[(length(VJI_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# VJI_MDC_Cold_Counts[(length(VJI_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# VJI_MDC_Cold_Counts[(length(VJI_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# VJI_MDC_Cold_Counts[(length(VJI_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# VJI_MDC_Cold_Counts[(length(VJI_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# VJI_MDC_Cold_Counts[(length(VJI_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)
# VJI_MDC_Cold_Counts[(length(VJI_MDC_Cold_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# VJI_MDC_Cold_Counts[(length(VJI_MDC_Cold_Counts$MDC_ACME)), 2] <- c(0)

rm(VJI_data_all,VJI_MDC_Cold_NAs,VJI_MDC_NotCold_NAs,VJI_MDC_NotCold_Slim)

#Final Prep for Comparison
VJI_Ordered_NotCold_Counts <- VJI_MDC_NotCold_Counts[order(VJI_MDC_NotCold_Counts$Boot_MDC_ACME), ]
VJI_Ordered_Cold_Counts <- VJI_MDC_Cold_Counts[order(VJI_MDC_Cold_Counts$MDC_ACME), ]

#VJI_Final_Cold <- VJI_Ordered_Cold_Counts[VJI_Ordered_Cold_Counts$count_by_siteyear > 5, ]
VJI_Ordered_NotCold_Expected_Counts <- VJI_Ordered_NotCold_Counts[VJI_Ordered_NotCold_Counts$Boot_Count > 5, ]

# ifelse(length(VJI_NotCold_Counts_Ordered$Boot_MDC_ACME) == length(VJI_Cold_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(VJI_NotCold_Counts_Ordered$Boot_Count) == sum(VJI_Cold_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackVJI <- cbind(VJI_NotCold_Counts_Ordered,VJI_Cold_Counts_Ordered)
CountsOnlyVJI <- right_join(VJI_Ordered_Cold_Counts , VJI_Ordered_NotCold_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackVJI <- CountsOnlyVJI[!is.na(CountsOnlyVJI$Boot_Count) | !is.na(CountsOnlyVJI$count_by_siteyear),]
rm(CountsOnlyVJI)

sum(BackToBackVJI$count_by_siteyear)
sum(BackToBackVJI$Boot_Count)

colnames(BackToBackVJI) <- c("MDC_ACME","Cold","Boot")

BackToBackVJI$Boot <- as.numeric(BackToBackVJI$Boot)
sum(BackToBackVJI$Boot)
sum(BackToBackVJI$Cold)

BackToBackVJI_GreaterThanZero_Cold <- BackToBackVJI[BackToBackVJI$Boot > 0, ]

sum(BackToBackVJI_GreaterThanZero_Cold$Cold)
sum(BackToBackVJI_GreaterThanZero_Cold$Boot)

EO2_VJI <- (BackToBackVJI_GreaterThanZero_Cold$Cold - BackToBackVJI_GreaterThanZero_Cold$Boot)^2
E_VJI <- BackToBackVJI_GreaterThanZero_Cold$Boot
EO2E_VJI <- EO2_VJI/E_VJI
sumEO2E_VJI <- round(sum(EO2E_VJI),digits = 2)
sum(EO2E_VJI)
df_VJI <- length(EO2_VJI) - 1
#CritValue_df18 = 28.869
#CritValue_df19 = 30.144
#CritValue_df20 = 31.410
#CritValue_df21 = 32.671

# CSByHandTable <- data.frame(matrix(nrow=1,ncol=1))
# CSByHandTable$VJICSValue <- sumEO2E_VJI

EO_Diff_VJI <- (BackToBackVJI_GreaterThanZero_Cold$Cold - BackToBackVJI_GreaterThanZero_Cold$Boot)
STD_RES_VJI <- EO_Diff_VJI / (sqrt(BackToBackVJI_GreaterThanZero_Cold$Boot))
BackToBackVJI_GreaterThanZero_Cold$Res <- round(STD_RES_VJI,digits = 3)
BackToBackVJI_GreaterThanZero_Cold$ResSig <- ifelse(abs(BackToBackVJI_GreaterThanZero_Cold$Res) > 3.8, TRUE, print("F"))

