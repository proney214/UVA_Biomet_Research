#ULTIMATE FINAL BOOTSTRAP CHI-SQUARE SCRIPT for warm 
#Load Davis_Scratch_Warm script as well

library(readxl)
library(dplyr)
library(lubridate)

MDC_FullFinal_NAs <- read.csv("Personal_MDC_ChiSqMortData.csv")
MDC_FullFinal <- MDC_FullFinal_NAs[!is.na(MDC_FullFinal_NAs$MDC_ACME),]
rm(MDC_FullFinal_NAs)
# NAOnly <- subset(MDC_FullFinal, MDC_FullFinal$MDC_ACME == NA) #0 obs. so yay!
# NAOnly <- MDC_FullFinal[is.na(MDC_FullFinal$MDC_ACME), ]

CHO <- read_excel("~/Desktop/CHO.Final.Weather.xlsx",na="NA")
CHOselect <- CHO[1:5844, ]

CHOHotDaysSevere_lag0 <- CHOselect[CHOselect$HeatLag0Update == 1, ]
CHOHotDaysSevere_lag1 <- CHOselect[CHOselect$HeatLag1Update == 1, ]
CHOHotDaysSevere_LagCombine <- rbind(CHOHotDaysSevere_lag0,CHOHotDaysSevere_lag1)
CHOHotDaysSevere_LagCombine_DupsRem <- CHOHotDaysSevere_LagCombine[!duplicated(CHOHotDaysSevere_LagCombine), ]
CHOHotDaysUpdate <- CHOHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

CHOHotDaysUpdate_Summer <- CHOHotDaysUpdate[3 < month(CHOHotDaysUpdate$Date) & month(CHOHotDaysUpdate$Date) < 10, ]
CHOHotDaysUpdate_Summer$Date_Update <- format(as.Date(CHOHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(CHOHotDaysSevere_lag0,CHOHotDaysSevere_lag1,CHOHotDaysSevere_LagCombine,CHOHotDaysSevere_LagCombine_DupsRem,CHOHotDaysUpdate)

CHO_MDC <- MDC_FullFinal %>%
  filter(Wx == "CHO")

CHO_MDC_Hot_NAs <- left_join(CHOHotDaysUpdate_Summer, 
                         CHO_MDC,
                         by = c("Date_Update" = "Date"))
CHO_MDC_Hot <- CHO_MDC_Hot_NAs[!is.na(CHO_MDC_Hot_NAs$MDC_ACME),]
#NAOnly_Hot <- CHO_MDC_Hot[is.na(CHO_MDC_Hot$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(CHO_MDC_Hot$HeatLag0Update == 0 & CHO_MDC_Hot$HeatLag1Update == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(CHO_MDC_Hot$Station) / length(CHO_MDC$Date) #  13.49% of deaths happen on Hot Days
length(CHOHotDaysUpdate_Summer$Station) / length(CHO$Station) # 13.92% of days are hot day lag0 or lag1

####### BootStrapping CHO

CHONotHotDaysSevere_lag0 <- CHOselect[!(CHOselect$HeatLag0Update == 1), ]
CHONotHotDaysSevere_lag1 <- CHOselect[!(CHOselect$HeatLag1Update == 1), ]
CHONotHotDaysSevere_LagCombine <- rbind(CHONotHotDaysSevere_lag0,CHONotHotDaysSevere_lag1)
CHONotHotDaysSevere_LagCombine_DupsRem <- CHONotHotDaysSevere_LagCombine[!duplicated(CHONotHotDaysSevere_LagCombine), ]
CHONotHotDaysUpdate <- CHONotHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

CHONotHotDaysUpdate_Summer <- CHONotHotDaysUpdate[3 < month(CHONotHotDaysUpdate$Date) & month(CHONotHotDaysUpdate$Date) < 10, ]
CHONotHotDaysUpdate_Summer$Date_Update <- format(as.Date(CHONotHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(CHONotHotDaysSevere_lag0,CHONotHotDaysSevere_lag1,CHONotHotDaysSevere_LagCombine,CHONotHotDaysSevere_LagCombine_DupsRem,CHONotHotDaysUpdate)

#test <- CHONotHotDaysUpdate_Summer[CHONotHotDaysUpdate_Summer$Month == 3, ]

CHO_MDC_NotHot_NAs <- left_join(CHONotHotDaysUpdate_Summer, 
                            CHO_MDC,
                            by = c("Date_Update" = "Date"))
CHO_MDC_NotHot <- CHO_MDC_NotHot_NAs[!is.na(CHO_MDC_NotHot_NAs$MDC_ACME),]
#NAOnly_NotHot <- CHO_MDC_NotHot[is.na(CHO_MDC_NotHot$MDC_ACME), ] #0 obs. so yay!


CHO_MDC_NotHot_Slim <- CHO_MDC_NotHot %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotHot_Slim <- CHO_MDC_NotHot_Slim[is.na(CHO_MDC_NotHot_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   CHONoStressRandom <- CHO_MDC_NotHot_Slim %>%
#     sample_n(length(CHO_MDC_Hot$Month))
#   write.csv(CHONoStressRandom,paste0("~/Desktop/NoStressRandomWx/CHO_v3/CHO",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

CHO1 <- read.csv("~/Desktop/NoStressRandomWx/CHO_v3/CHO1")
CHO2 <- read.csv("~/Desktop/NoStressRandomWx/CHO_v3/CHO2")
ifelse(length(CHO1$X) == length(CHO_MDC_Hot$Month),1,0)
rm(CHO1,CHO2)

CHO_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/CHO_v3/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#CHO_data_all                                            # Print data to RStudio console

# Group by MDC

CHO_MDC_NotHot_Counts_InitialGrouping <- CHO_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
CHO_MDC_NotHot_Counts_InitialGrouping$Adjusted <- CHO_MDC_NotHot_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/CHO_v3/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
CHO_MDC_NotHot_Counts <- data.frame(cbind(CHO_MDC_NotHot_Counts_InitialGrouping$MDC_ACME,CHO_MDC_NotHot_Counts_InitialGrouping$Adjusted))

colnames(CHO_MDC_NotHot_Counts) <- c("Boot_MDC_ACME","Boot_Count")
CHO_MDC_NotHot_Counts$Boot_Count <- as.numeric(CHO_MDC_NotHot_Counts$Boot_Count)
sum(CHO_MDC_NotHot_Counts$Boot_Count)

#CHO_MDC_NotHot_Counts[CHO_MDC_NotHot_Counts$MDCAcme == "MDC22", 2] <- 0

CHO_MDC_Hot_Counts <- CHO_MDC_Hot %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)

rm(CHO_data_all,CHO_MDC_Hot_NAs,CHO_MDC_NotHot_NAs,CHO_MDC_NotHot_Slim)

#Final Prep for Comparison
CHO_Ordered_NotHot_Counts <- CHO_MDC_NotHot_Counts[order(CHO_MDC_NotHot_Counts$Boot_MDC_ACME), ]
CHO_Ordered_Hot_Counts <- CHO_MDC_Hot_Counts[order(CHO_MDC_Hot_Counts$MDC_ACME), ]

#CHO_Final_Hot <- CHO_Ordered_Hot_Counts[CHO_Ordered_Hot_Counts$count_by_siteyear > 5, ]
CHO_Ordered_NotHot_Expected_Counts <- CHO_Ordered_NotHot_Counts[CHO_Ordered_NotHot_Counts$Boot_Count > 5, ]

# ifelse(length(CHO_NotHot_Counts_Ordered$Boot_MDC_ACME) == length(CHO_Hot_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(CHO_NotHot_Counts_Ordered$Boot_Count) == sum(CHO_Hot_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackCHO <- cbind(CHO_NotHot_Counts_Ordered,CHO_Hot_Counts_Ordered)
CountsOnlyCHO <- right_join(CHO_Ordered_Hot_Counts , CHO_Ordered_NotHot_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackCHO <- CountsOnlyCHO[!is.na(CountsOnlyCHO$Boot_Count) | !is.na(CountsOnlyCHO$count_by_siteyear),]
rm(CountsOnlyCHO)

sum(BackToBackCHO$count_by_siteyear)
sum(BackToBackCHO$Boot_Count)

colnames(BackToBackCHO) <- c("MDC_ACME","Heat","Boot")

BackToBackCHO$Boot <- as.numeric(BackToBackCHO$Boot)
sum(BackToBackCHO$Boot)
sum(BackToBackCHO$Heat)

BackToBackCHO_GreaterThanZero_Heat <- BackToBackCHO[BackToBackCHO$Boot > 0, ]

sum(BackToBackCHO_GreaterThanZero_Heat$Heat)
sum(BackToBackCHO_GreaterThanZero_Heat$Boot)

EO2_CHO <- (BackToBackCHO_GreaterThanZero_Heat$Heat - BackToBackCHO_GreaterThanZero_Heat$Boot)^2
E_CHO <- BackToBackCHO_GreaterThanZero_Heat$Boot
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

EO_Diff_CHO <- (BackToBackCHO_GreaterThanZero_Heat$Heat - BackToBackCHO_GreaterThanZero_Heat$Boot)
STD_RES_CHO <- EO_Diff_CHO / (sqrt(BackToBackCHO_GreaterThanZero_Heat$Boot))
BackToBackCHO_GreaterThanZero_Heat$Res <- round(STD_RES_CHO,digits = 3)
BackToBackCHO_GreaterThanZero_Heat$ResSig <- ifelse(abs(BackToBackCHO_GreaterThanZero_Heat$Res) > 3.8, TRUE, print("F"))

#EMV

EMV <- read_excel("~/Desktop/EMV.Final.Weather.xlsx",na="NA")
EMVselect <- EMV[1:5844, ]

EMVHotDaysSevere_lag0 <- EMVselect[EMVselect$HeatLag0Update == 1, ]
EMVHotDaysSevere_lag1 <- EMVselect[EMVselect$HeatLag1Update == 1, ]
EMVHotDaysSevere_LagCombine <- rbind(EMVHotDaysSevere_lag0,EMVHotDaysSevere_lag1)
EMVHotDaysSevere_LagCombine_DupsRem <- EMVHotDaysSevere_LagCombine[!duplicated(EMVHotDaysSevere_LagCombine), ]
EMVHotDaysUpdate <- EMVHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

EMVHotDaysUpdate_Summer <- EMVHotDaysUpdate[3 < month(EMVHotDaysUpdate$Date) & month(EMVHotDaysUpdate$Date) < 10, ]
EMVHotDaysUpdate_Summer$Date_Update <- format(as.Date(EMVHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(EMVHotDaysSevere_lag0,EMVHotDaysSevere_lag1,EMVHotDaysSevere_LagCombine,EMVHotDaysSevere_LagCombine_DupsRem,EMVHotDaysUpdate)

EMV_MDC <- MDC_FullFinal %>%
  filter(Wx == "EMV")

EMV_MDC_Hot_NAs <- left_join(EMVHotDaysUpdate_Summer, 
                             EMV_MDC,
                             by = c("Date_Update" = "Date"))
EMV_MDC_Hot <- EMV_MDC_Hot_NAs[!is.na(EMV_MDC_Hot_NAs$MDC_ACME),]
#NAOnly_Hot <- EMV_MDC_Hot[is.na(EMV_MDC_Hot$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(EMV_MDC_Hot$HeatLag0Update == 0 & EMV_MDC_Hot$HeatLag1Update == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(EMV_MDC_Hot$Station) / length(EMV_MDC$Date) #  13.49% of deaths happen on Hot Days
length(EMVHotDaysUpdate_Summer$Station) / length(EMV$Station) # 13.92% of days are hot day lag0 or lag1

####### BootStrapping EMV

EMVNotHotDaysSevere_lag0 <- EMVselect[!(EMVselect$HeatLag0Update == 1), ]
EMVNotHotDaysSevere_lag1 <- EMVselect[!(EMVselect$HeatLag1Update == 1), ]
EMVNotHotDaysSevere_LagCombine <- rbind(EMVNotHotDaysSevere_lag0,EMVNotHotDaysSevere_lag1)
EMVNotHotDaysSevere_LagCombine_DupsRem <- EMVNotHotDaysSevere_LagCombine[!duplicated(EMVNotHotDaysSevere_LagCombine), ]
EMVNotHotDaysUpdate <- EMVNotHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

EMVNotHotDaysUpdate_Summer <- EMVNotHotDaysUpdate[3 < month(EMVNotHotDaysUpdate$Date) & month(EMVNotHotDaysUpdate$Date) < 10, ]
EMVNotHotDaysUpdate_Summer$Date_Update <- format(as.Date(EMVNotHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(EMVNotHotDaysSevere_lag0,EMVNotHotDaysSevere_lag1,EMVNotHotDaysSevere_LagCombine,EMVNotHotDaysSevere_LagCombine_DupsRem,EMVNotHotDaysUpdate)

#test <- EMVNotHotDaysUpdate_Summer[EMVNotHotDaysUpdate_Summer$Month == 3, ]

EMV_MDC_NotHot_NAs <- left_join(EMVNotHotDaysUpdate_Summer, 
                                EMV_MDC,
                                by = c("Date_Update" = "Date"))
EMV_MDC_NotHot <- EMV_MDC_NotHot_NAs[!is.na(EMV_MDC_NotHot_NAs$MDC_ACME),]
#NAOnly_NotHot <- EMV_MDC_NotHot[is.na(EMV_MDC_NotHot$MDC_ACME), ] #0 obs. so yay!


EMV_MDC_NotHot_Slim <- EMV_MDC_NotHot %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotHot_Slim <- EMV_MDC_NotHot_Slim[is.na(EMV_MDC_NotHot_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   EMVNoStressRandom <- EMV_MDC_NotHot_Slim %>%
#     sample_n(length(EMV_MDC_Hot$Month),replace = TRUE)
#   write.csv(EMVNoStressRandom,paste0("~/Desktop/NoStressRandomWx/EMV_v3/EMV",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

EMV1 <- read.csv("~/Desktop/NoStressRandomWx/EMV_v3/EMV1")
EMV2 <- read.csv("~/Desktop/NoStressRandomWx/EMV_v3/EMV2")
ifelse(length(EMV1$X) == length(EMV_MDC_Hot$Month),1,0)
rm(EMV1,EMV2)

EMV_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/EMV_v3/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#EMV_data_all                                            # Print data to RStudio console

# Group by MDC

EMV_MDC_NotHot_Counts_InitialGrouping <- EMV_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
EMV_MDC_NotHot_Counts_InitialGrouping$Adjusted <- EMV_MDC_NotHot_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/EMV_v3/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
EMV_MDC_NotHot_Counts <- data.frame(cbind(EMV_MDC_NotHot_Counts_InitialGrouping$MDC_ACME,EMV_MDC_NotHot_Counts_InitialGrouping$Adjusted))

colnames(EMV_MDC_NotHot_Counts) <- c("Boot_MDC_ACME","Boot_Count")
EMV_MDC_NotHot_Counts$Boot_Count <- as.numeric(EMV_MDC_NotHot_Counts$Boot_Count)
sum(EMV_MDC_NotHot_Counts$Boot_Count)

#EMV_MDC_NotHot_Counts[EMV_MDC_NotHot_Counts$MDCAcme == "MDC22", 2] <- 0

EMV_MDC_Hot_Counts <- EMV_MDC_Hot %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# EMV_MDC_Hot_Counts[(length(EMV_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# EMV_MDC_Hot_Counts[(length(EMV_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# EMV_MDC_Hot_Counts[(length(EMV_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# EMV_MDC_Hot_Counts[(length(EMV_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# EMV_MDC_Hot_Counts[(length(EMV_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# EMV_MDC_Hot_Counts[(length(EMV_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# EMV_MDC_Hot_Counts[(length(EMV_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# EMV_MDC_Hot_Counts[(length(EMV_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)

rm(EMV_data_all,EMV_MDC_Hot_NAs,EMV_MDC_NotHot_NAs,EMV_MDC_NotHot_Slim)

#Final Prep for Comparison
EMV_Ordered_NotHot_Counts <- EMV_MDC_NotHot_Counts[order(EMV_MDC_NotHot_Counts$Boot_MDC_ACME), ]
EMV_Ordered_Hot_Counts <- EMV_MDC_Hot_Counts[order(EMV_MDC_Hot_Counts$MDC_ACME), ]

#EMV_Final_Hot <- EMV_Ordered_Hot_Counts[EMV_Ordered_Hot_Counts$count_by_siteyear > 5, ]
EMV_Ordered_NotHot_Expected_Counts <- EMV_Ordered_NotHot_Counts[EMV_Ordered_NotHot_Counts$Boot_Count > 5, ]

# ifelse(length(EMV_NotHot_Counts_Ordered$Boot_MDC_ACME) == length(EMV_Hot_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(EMV_NotHot_Counts_Ordered$Boot_Count) == sum(EMV_Hot_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackEMV <- cbind(EMV_NotHot_Counts_Ordered,EMV_Hot_Counts_Ordered)
CountsOnlyEMV <- right_join(EMV_Ordered_Hot_Counts , EMV_Ordered_NotHot_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackEMV <- CountsOnlyEMV[!is.na(CountsOnlyEMV$Boot_Count) | !is.na(CountsOnlyEMV$count_by_siteyear),]
rm(CountsOnlyEMV)

sum(BackToBackEMV$count_by_siteyear)
sum(BackToBackEMV$Boot_Count)

colnames(BackToBackEMV) <- c("MDC_ACME","Heat","Boot")

BackToBackEMV$Boot <- as.numeric(BackToBackEMV$Boot)
sum(BackToBackEMV$Boot)
sum(BackToBackEMV$Heat)

BackToBackEMV_GreaterThanZero_Heat <- BackToBackEMV[BackToBackEMV$Boot > 0, ]

sum(BackToBackEMV_GreaterThanZero_Heat$Heat)
sum(BackToBackEMV_GreaterThanZero_Heat$Boot)

EO2_EMV <- (BackToBackEMV_GreaterThanZero_Heat$Heat - BackToBackEMV_GreaterThanZero_Heat$Boot)^2
E_EMV <- BackToBackEMV_GreaterThanZero_Heat$Boot
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

EO_Diff_EMV <- (BackToBackEMV_GreaterThanZero_Heat$Heat - BackToBackEMV_GreaterThanZero_Heat$Boot)
STD_RES_EMV <- EO_Diff_EMV / (sqrt(BackToBackEMV_GreaterThanZero_Heat$Boot))
BackToBackEMV_GreaterThanZero_Heat$Res <- round(STD_RES_EMV,digits = 3)
BackToBackEMV_GreaterThanZero_Heat$ResSig <- ifelse(abs(BackToBackEMV_GreaterThanZero_Heat$Res) > 3.8, TRUE, print("F"))

#EZF

EZF <- read_excel("~/Desktop/EZF.Final.Weather.xlsx",na="NA")
EZFselect <- EZF[1:5844, ]

EZFHotDaysSevere_lag0 <- EZFselect[EZFselect$HeatLag0Update == 1, ]
EZFHotDaysSevere_lag1 <- EZFselect[EZFselect$HeatLag1Update == 1, ]
EZFHotDaysSevere_LagCombine <- rbind(EZFHotDaysSevere_lag0,EZFHotDaysSevere_lag1)
EZFHotDaysSevere_LagCombine_DupsRem <- EZFHotDaysSevere_LagCombine[!duplicated(EZFHotDaysSevere_LagCombine), ]
EZFHotDaysUpdate <- EZFHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

EZFHotDaysUpdate_Summer <- EZFHotDaysUpdate[3 < month(EZFHotDaysUpdate$Date) & month(EZFHotDaysUpdate$Date) < 10, ]
EZFHotDaysUpdate_Summer$Date_Update <- format(as.Date(EZFHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(EZFHotDaysSevere_lag0,EZFHotDaysSevere_lag1,EZFHotDaysSevere_LagCombine,EZFHotDaysSevere_LagCombine_DupsRem,EZFHotDaysUpdate)

EZF_MDC <- MDC_FullFinal %>%
  filter(Wx == "EZF")

EZF_MDC_Hot_NAs <- left_join(EZFHotDaysUpdate_Summer, 
                             EZF_MDC,
                             by = c("Date_Update" = "Date"))
EZF_MDC_Hot <- EZF_MDC_Hot_NAs[!is.na(EZF_MDC_Hot_NAs$MDC_ACME),]
#NAOnly_Hot <- EZF_MDC_Hot[is.na(EZF_MDC_Hot$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(EZF_MDC_Hot$HeatLag0Update == 0 & EZF_MDC_Hot$HeatLag1Update == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(EZF_MDC_Hot$Station) / length(EZF_MDC$Date) #  13.49% of deaths happen on Hot Days
length(EZFHotDaysUpdate_Summer$Station) / length(EZF$Station) # 13.92% of days are hot day lag0 or lag1

####### BootStrapping EZF

EZFNotHotDaysSevere_lag0 <- EZFselect[!(EZFselect$HeatLag0Update == 1), ]
EZFNotHotDaysSevere_lag1 <- EZFselect[!(EZFselect$HeatLag1Update == 1), ]
EZFNotHotDaysSevere_LagCombine <- rbind(EZFNotHotDaysSevere_lag0,EZFNotHotDaysSevere_lag1)
EZFNotHotDaysSevere_LagCombine_DupsRem <- EZFNotHotDaysSevere_LagCombine[!duplicated(EZFNotHotDaysSevere_LagCombine), ]
EZFNotHotDaysUpdate <- EZFNotHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

EZFNotHotDaysUpdate_Summer <- EZFNotHotDaysUpdate[3 < month(EZFNotHotDaysUpdate$Date) & month(EZFNotHotDaysUpdate$Date) < 10, ]
EZFNotHotDaysUpdate_Summer$Date_Update <- format(as.Date(EZFNotHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(EZFNotHotDaysSevere_lag0,EZFNotHotDaysSevere_lag1,EZFNotHotDaysSevere_LagCombine,EZFNotHotDaysSevere_LagCombine_DupsRem,EZFNotHotDaysUpdate)

#test <- EZFNotHotDaysUpdate_Summer[EZFNotHotDaysUpdate_Summer$Month == 3, ]

EZF_MDC_NotHot_NAs <- left_join(EZFNotHotDaysUpdate_Summer, 
                                EZF_MDC,
                                by = c("Date_Update" = "Date"))
EZF_MDC_NotHot <- EZF_MDC_NotHot_NAs[!is.na(EZF_MDC_NotHot_NAs$MDC_ACME),]
#NAOnly_NotHot <- EZF_MDC_NotHot[is.na(EZF_MDC_NotHot$MDC_ACME), ] #0 obs. so yay!


EZF_MDC_NotHot_Slim <- EZF_MDC_NotHot %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotHot_Slim <- EZF_MDC_NotHot_Slim[is.na(EZF_MDC_NotHot_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   EZFNoStressRandom <- EZF_MDC_NotHot_Slim %>%
#     sample_n(length(EZF_MDC_Hot$Month),replace = TRUE)
#   write.csv(EZFNoStressRandom,paste0("~/Desktop/NoStressRandomWx/EZF_v3/EZF",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

EZF1 <- read.csv("~/Desktop/NoStressRandomWx/EZF_v3/EZF1")
EZF2 <- read.csv("~/Desktop/NoStressRandomWx/EZF_v3/EZF2")
ifelse(length(EZF1$X) == length(EZF_MDC_Hot$Month),1,0)
rm(EZF1,EZF2)

EZF_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/EZF_v3/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#EZF_data_all                                            # Print data to RStudio console

# Group by MDC

EZF_MDC_NotHot_Counts_InitialGrouping <- EZF_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
EZF_MDC_NotHot_Counts_InitialGrouping$Adjusted <- EZF_MDC_NotHot_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/EZF_v3/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
EZF_MDC_NotHot_Counts <- data.frame(cbind(EZF_MDC_NotHot_Counts_InitialGrouping$MDC_ACME,EZF_MDC_NotHot_Counts_InitialGrouping$Adjusted))

colnames(EZF_MDC_NotHot_Counts) <- c("Boot_MDC_ACME","Boot_Count")
EZF_MDC_NotHot_Counts$Boot_Count <- as.numeric(EZF_MDC_NotHot_Counts$Boot_Count)
sum(EZF_MDC_NotHot_Counts$Boot_Count)

#EZF_MDC_NotHot_Counts[EZF_MDC_NotHot_Counts$MDCAcme == "MDC22", 2] <- 0

EZF_MDC_Hot_Counts <- EZF_MDC_Hot %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# EZF_MDC_Hot_Counts[(length(EZF_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# EZF_MDC_Hot_Counts[(length(EZF_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# EZF_MDC_Hot_Counts[(length(EZF_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# EZF_MDC_Hot_Counts[(length(EZF_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# EZF_MDC_Hot_Counts[(length(EZF_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# EZF_MDC_Hot_Counts[(length(EZF_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# EZF_MDC_Hot_Counts[(length(EZF_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# EZF_MDC_Hot_Counts[(length(EZF_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)

rm(EZF_data_all,EZF_MDC_Hot_NAs,EZF_MDC_NotHot_NAs,EZF_MDC_NotHot_Slim)

#Final Prep for Comparison
EZF_Ordered_NotHot_Counts <- EZF_MDC_NotHot_Counts[order(EZF_MDC_NotHot_Counts$Boot_MDC_ACME), ]
EZF_Ordered_Hot_Counts <- EZF_MDC_Hot_Counts[order(EZF_MDC_Hot_Counts$MDC_ACME), ]

#EZF_Final_Hot <- EZF_Ordered_Hot_Counts[EZF_Ordered_Hot_Counts$count_by_siteyear > 5, ]
EZF_Ordered_NotHot_Expected_Counts <- EZF_Ordered_NotHot_Counts[EZF_Ordered_NotHot_Counts$Boot_Count > 5, ]

# ifelse(length(EZF_NotHot_Counts_Ordered$Boot_MDC_ACME) == length(EZF_Hot_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(EZF_NotHot_Counts_Ordered$Boot_Count) == sum(EZF_Hot_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackEZF <- cbind(EZF_NotHot_Counts_Ordered,EZF_Hot_Counts_Ordered)
CountsOnlyEZF <- right_join(EZF_Ordered_Hot_Counts , EZF_Ordered_NotHot_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackEZF <- CountsOnlyEZF[!is.na(CountsOnlyEZF$Boot_Count) | !is.na(CountsOnlyEZF$count_by_siteyear),]
rm(CountsOnlyEZF)

sum(BackToBackEZF$count_by_siteyear)
sum(BackToBackEZF$Boot_Count)

colnames(BackToBackEZF) <- c("MDC_ACME","Heat","Boot")

BackToBackEZF$Boot <- as.numeric(BackToBackEZF$Boot)
sum(BackToBackEZF$Boot)
sum(BackToBackEZF$Heat)

BackToBackEZF_GreaterThanZero_Heat <- BackToBackEZF[BackToBackEZF$Boot > 0, ]

sum(BackToBackEZF_GreaterThanZero_Heat$Heat)
sum(BackToBackEZF_GreaterThanZero_Heat$Boot)

EO2_EZF <- (BackToBackEZF_GreaterThanZero_Heat$Heat - BackToBackEZF_GreaterThanZero_Heat$Boot)^2
E_EZF <- BackToBackEZF_GreaterThanZero_Heat$Boot
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

EO_Diff_EZF <- (BackToBackEZF_GreaterThanZero_Heat$Heat - BackToBackEZF_GreaterThanZero_Heat$Boot)
STD_RES_EZF <- EO_Diff_EZF / (sqrt(BackToBackEZF_GreaterThanZero_Heat$Boot))
BackToBackEZF_GreaterThanZero_Heat$Res <- round(STD_RES_EZF,digits = 3)
BackToBackEZF_GreaterThanZero_Heat$ResSig <- ifelse(abs(BackToBackEZF_GreaterThanZero_Heat$Res) > 3.8, TRUE, print("F"))


#IAD

IAD <- read_excel("~/Desktop/IAD.Final.Weather.xlsx",na="NA")
IADselect <- IAD[1:5844, ]

IADHotDaysSevere_lag0 <- IADselect[IADselect$HeatLag0Update == 1, ]
IADHotDaysSevere_lag1 <- IADselect[IADselect$HeatLag1Update == 1, ]
IADHotDaysSevere_LagCombine <- rbind(IADHotDaysSevere_lag0,IADHotDaysSevere_lag1)
IADHotDaysSevere_LagCombine_DupsRem <- IADHotDaysSevere_LagCombine[!duplicated(IADHotDaysSevere_LagCombine), ]
IADHotDaysUpdate <- IADHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

IADHotDaysUpdate_Summer <- IADHotDaysUpdate[3 < month(IADHotDaysUpdate$Date) & month(IADHotDaysUpdate$Date) < 10, ]
IADHotDaysUpdate_Summer$Date_Update <- format(as.Date(IADHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(IADHotDaysSevere_lag0,IADHotDaysSevere_lag1,IADHotDaysSevere_LagCombine,IADHotDaysSevere_LagCombine_DupsRem,IADHotDaysUpdate)

IAD_MDC <- MDC_FullFinal %>%
  filter(Wx == "IAD")

IAD_MDC_Hot_NAs <- left_join(IADHotDaysUpdate_Summer, 
                             IAD_MDC,
                             by = c("Date_Update" = "Date"))
IAD_MDC_Hot <- IAD_MDC_Hot_NAs[!is.na(IAD_MDC_Hot_NAs$MDC_ACME),]
#NAOnly_Hot <- IAD_MDC_Hot[is.na(IAD_MDC_Hot$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(IAD_MDC_Hot$HeatLag0Update == 0 & IAD_MDC_Hot$HeatLag1Update == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(IAD_MDC_Hot$Station) / length(IAD_MDC$Date) #  13.49% of deaths happen on Hot Days
length(IADHotDaysUpdate_Summer$Station) / length(IAD$Station) # 13.92% of days are hot day lag0 or lag1

####### BootStrapping IAD

IADNotHotDaysSevere_lag0 <- IADselect[!(IADselect$HeatLag0Update == 1), ]
IADNotHotDaysSevere_lag1 <- IADselect[!(IADselect$HeatLag1Update == 1), ]
IADNotHotDaysSevere_LagCombine <- rbind(IADNotHotDaysSevere_lag0,IADNotHotDaysSevere_lag1)
IADNotHotDaysSevere_LagCombine_DupsRem <- IADNotHotDaysSevere_LagCombine[!duplicated(IADNotHotDaysSevere_LagCombine), ]
IADNotHotDaysUpdate <- IADNotHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

IADNotHotDaysUpdate_Summer <- IADNotHotDaysUpdate[3 < month(IADNotHotDaysUpdate$Date) & month(IADNotHotDaysUpdate$Date) < 10, ]
IADNotHotDaysUpdate_Summer$Date_Update <- format(as.Date(IADNotHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(IADNotHotDaysSevere_lag0,IADNotHotDaysSevere_lag1,IADNotHotDaysSevere_LagCombine,IADNotHotDaysSevere_LagCombine_DupsRem,IADNotHotDaysUpdate)

#test <- IADNotHotDaysUpdate_Summer[IADNotHotDaysUpdate_Summer$Month == 3, ]

IAD_MDC_NotHot_NAs <- left_join(IADNotHotDaysUpdate_Summer, 
                                IAD_MDC,
                                by = c("Date_Update" = "Date"))
IAD_MDC_NotHot <- IAD_MDC_NotHot_NAs[!is.na(IAD_MDC_NotHot_NAs$MDC_ACME),]
#NAOnly_NotHot <- IAD_MDC_NotHot[is.na(IAD_MDC_NotHot$MDC_ACME), ] #0 obs. so yay!


IAD_MDC_NotHot_Slim <- IAD_MDC_NotHot %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotHot_Slim <- IAD_MDC_NotHot_Slim[is.na(IAD_MDC_NotHot_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   IADNoStressRandom <- IAD_MDC_NotHot_Slim %>%
#     sample_n(length(IAD_MDC_Hot$Month),replace = TRUE)
#   write.csv(IADNoStressRandom,paste0("~/Desktop/NoStressRandomWx/IAD_v3/IAD",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

IAD1 <- read.csv("~/Desktop/NoStressRandomWx/IAD_v3/IAD1")
IAD2 <- read.csv("~/Desktop/NoStressRandomWx/IAD_v3/IAD2")
ifelse(length(IAD1$X) == length(IAD_MDC_Hot$Month),1,0)
rm(IAD1,IAD2)

IAD_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/IAD_v3/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#IAD_data_all                                            # Print data to RStudio console

# Group by MDC

IAD_MDC_NotHot_Counts_InitialGrouping <- IAD_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
IAD_MDC_NotHot_Counts_InitialGrouping$Adjusted <- IAD_MDC_NotHot_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/IAD_v3/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
IAD_MDC_NotHot_Counts <- data.frame(cbind(IAD_MDC_NotHot_Counts_InitialGrouping$MDC_ACME,IAD_MDC_NotHot_Counts_InitialGrouping$Adjusted))

colnames(IAD_MDC_NotHot_Counts) <- c("Boot_MDC_ACME","Boot_Count")
IAD_MDC_NotHot_Counts$Boot_Count <- as.numeric(IAD_MDC_NotHot_Counts$Boot_Count)
sum(IAD_MDC_NotHot_Counts$Boot_Count)

#IAD_MDC_NotHot_Counts[IAD_MDC_NotHot_Counts$MDCAcme == "MDC22", 2] <- 0

IAD_MDC_Hot_Counts <- IAD_MDC_Hot %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# IAD_MDC_Hot_Counts[(length(IAD_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# IAD_MDC_Hot_Counts[(length(IAD_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# IAD_MDC_Hot_Counts[(length(IAD_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# IAD_MDC_Hot_Counts[(length(IAD_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# IAD_MDC_Hot_Counts[(length(IAD_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# IAD_MDC_Hot_Counts[(length(IAD_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# IAD_MDC_Hot_Counts[(length(IAD_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# IAD_MDC_Hot_Counts[(length(IAD_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)

rm(IAD_data_all,IAD_MDC_Hot_NAs,IAD_MDC_NotHot_NAs,IAD_MDC_NotHot_Slim)

#Final Prep for Comparison
IAD_Ordered_NotHot_Counts <- IAD_MDC_NotHot_Counts[order(IAD_MDC_NotHot_Counts$Boot_MDC_ACME), ]
IAD_Ordered_Hot_Counts <- IAD_MDC_Hot_Counts[order(IAD_MDC_Hot_Counts$MDC_ACME), ]

#IAD_Final_Hot <- IAD_Ordered_Hot_Counts[IAD_Ordered_Hot_Counts$count_by_siteyear > 5, ]
IAD_Ordered_NotHot_Expected_Counts <- IAD_Ordered_NotHot_Counts[IAD_Ordered_NotHot_Counts$Boot_Count > 5, ]

# ifelse(length(IAD_NotHot_Counts_Ordered$Boot_MDC_ACME) == length(IAD_Hot_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(IAD_NotHot_Counts_Ordered$Boot_Count) == sum(IAD_Hot_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackIAD <- cbind(IAD_NotHot_Counts_Ordered,IAD_Hot_Counts_Ordered)
CountsOnlyIAD <- right_join(IAD_Ordered_Hot_Counts , IAD_Ordered_NotHot_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackIAD <- CountsOnlyIAD[!is.na(CountsOnlyIAD$Boot_Count) | !is.na(CountsOnlyIAD$count_by_siteyear),]
rm(CountsOnlyIAD)

sum(BackToBackIAD$count_by_siteyear)
sum(BackToBackIAD$Boot_Count)

colnames(BackToBackIAD) <- c("MDC_ACME","Heat","Boot")

BackToBackIAD$Boot <- as.numeric(BackToBackIAD$Boot)
sum(BackToBackIAD$Boot)
sum(BackToBackIAD$Heat)

BackToBackIAD_GreaterThanZero_Heat <- BackToBackIAD[BackToBackIAD$Boot > 0, ]

sum(BackToBackIAD_GreaterThanZero_Heat$Heat)
sum(BackToBackIAD_GreaterThanZero_Heat$Boot)

EO2_IAD <- (BackToBackIAD_GreaterThanZero_Heat$Heat - BackToBackIAD_GreaterThanZero_Heat$Boot)^2
E_IAD <- BackToBackIAD_GreaterThanZero_Heat$Boot
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

EO_Diff_IAD <- (BackToBackIAD_GreaterThanZero_Heat$Heat - BackToBackIAD_GreaterThanZero_Heat$Boot)
STD_RES_IAD <- EO_Diff_IAD / (sqrt(BackToBackIAD_GreaterThanZero_Heat$Boot))
BackToBackIAD_GreaterThanZero_Heat$Res <- round(STD_RES_IAD,digits = 3)
BackToBackIAD_GreaterThanZero_Heat$ResSig <- ifelse(abs(BackToBackIAD_GreaterThanZero_Heat$Res) > 3.8, TRUE, print("F"))

#LYH

LYH <- read_excel("~/Desktop/LYH.Final.Weather.xlsx",na="NA")
LYHselect <- LYH[1:5844, ]

LYHHotDaysSevere_lag0 <- LYHselect[LYHselect$HeatLag0Update == 1, ]
LYHHotDaysSevere_lag1 <- LYHselect[LYHselect$HeatLag1Update == 1, ]
LYHHotDaysSevere_LagCombine <- rbind(LYHHotDaysSevere_lag0,LYHHotDaysSevere_lag1)
LYHHotDaysSevere_LagCombine_DupsRem <- LYHHotDaysSevere_LagCombine[!duplicated(LYHHotDaysSevere_LagCombine), ]
LYHHotDaysUpdate <- LYHHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

LYHHotDaysUpdate_Summer <- LYHHotDaysUpdate[3 < month(LYHHotDaysUpdate$Date) & month(LYHHotDaysUpdate$Date) < 10, ]
LYHHotDaysUpdate_Summer$Date_Update <- format(as.Date(LYHHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(LYHHotDaysSevere_lag0,LYHHotDaysSevere_lag1,LYHHotDaysSevere_LagCombine,LYHHotDaysSevere_LagCombine_DupsRem,LYHHotDaysUpdate)

LYH_MDC <- MDC_FullFinal %>%
  filter(Wx == "LYH")

LYH_MDC_Hot_NAs <- left_join(LYHHotDaysUpdate_Summer, 
                             LYH_MDC,
                             by = c("Date_Update" = "Date"))
LYH_MDC_Hot <- LYH_MDC_Hot_NAs[!is.na(LYH_MDC_Hot_NAs$MDC_ACME),]
#NAOnly_Hot <- LYH_MDC_Hot[is.na(LYH_MDC_Hot$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(LYH_MDC_Hot$HeatLag0Update == 0 & LYH_MDC_Hot$HeatLag1Update == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(LYH_MDC_Hot$Station) / length(LYH_MDC$Date) #  13.49% of deaths happen on Hot Days
length(LYHHotDaysUpdate_Summer$Station) / length(LYH$Station) # 13.92% of days are hot day lag0 or lag1

####### BootStrapping LYH

LYHNotHotDaysSevere_lag0 <- LYHselect[!(LYHselect$HeatLag0Update == 1), ]
LYHNotHotDaysSevere_lag1 <- LYHselect[!(LYHselect$HeatLag1Update == 1), ]
LYHNotHotDaysSevere_LagCombine <- rbind(LYHNotHotDaysSevere_lag0,LYHNotHotDaysSevere_lag1)
LYHNotHotDaysSevere_LagCombine_DupsRem <- LYHNotHotDaysSevere_LagCombine[!duplicated(LYHNotHotDaysSevere_LagCombine), ]
LYHNotHotDaysUpdate <- LYHNotHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

LYHNotHotDaysUpdate_Summer <- LYHNotHotDaysUpdate[3 < month(LYHNotHotDaysUpdate$Date) & month(LYHNotHotDaysUpdate$Date) < 10, ]
LYHNotHotDaysUpdate_Summer$Date_Update <- format(as.Date(LYHNotHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(LYHNotHotDaysSevere_lag0,LYHNotHotDaysSevere_lag1,LYHNotHotDaysSevere_LagCombine,LYHNotHotDaysSevere_LagCombine_DupsRem,LYHNotHotDaysUpdate)

#test <- LYHNotHotDaysUpdate_Summer[LYHNotHotDaysUpdate_Summer$Month == 3, ]

LYH_MDC_NotHot_NAs <- left_join(LYHNotHotDaysUpdate_Summer, 
                                LYH_MDC,
                                by = c("Date_Update" = "Date"))
LYH_MDC_NotHot <- LYH_MDC_NotHot_NAs[!is.na(LYH_MDC_NotHot_NAs$MDC_ACME),]
#NAOnly_NotHot <- LYH_MDC_NotHot[is.na(LYH_MDC_NotHot$MDC_ACME), ] #0 obs. so yay!


LYH_MDC_NotHot_Slim <- LYH_MDC_NotHot %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotHot_Slim <- LYH_MDC_NotHot_Slim[is.na(LYH_MDC_NotHot_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   LYHNoStressRandom <- LYH_MDC_NotHot_Slim %>%
#     sample_n(length(LYH_MDC_Hot$Month),replace = TRUE)
#   write.csv(LYHNoStressRandom,paste0("~/Desktop/NoStressRandomWx/LYH_v3/LYH",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

LYH1 <- read.csv("~/Desktop/NoStressRandomWx/LYH_v3/LYH1")
LYH2 <- read.csv("~/Desktop/NoStressRandomWx/LYH_v3/LYH2")
ifelse(length(LYH1$X) == length(LYH_MDC_Hot$Month),1,0)
rm(LYH1,LYH2)

LYH_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/LYH_v3/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#LYH_data_all                                            # Print data to RStudio console

# Group by MDC

LYH_MDC_NotHot_Counts_InitialGrouping <- LYH_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
LYH_MDC_NotHot_Counts_InitialGrouping$Adjusted <- LYH_MDC_NotHot_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/LYH_v3/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
LYH_MDC_NotHot_Counts <- data.frame(cbind(LYH_MDC_NotHot_Counts_InitialGrouping$MDC_ACME,LYH_MDC_NotHot_Counts_InitialGrouping$Adjusted))

colnames(LYH_MDC_NotHot_Counts) <- c("Boot_MDC_ACME","Boot_Count")
LYH_MDC_NotHot_Counts$Boot_Count <- as.numeric(LYH_MDC_NotHot_Counts$Boot_Count)
sum(LYH_MDC_NotHot_Counts$Boot_Count)

#LYH_MDC_NotHot_Counts[LYH_MDC_NotHot_Counts$MDCAcme == "MDC22", 2] <- 0

LYH_MDC_Hot_Counts <- LYH_MDC_Hot %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# LYH_MDC_Hot_Counts[(length(LYH_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# LYH_MDC_Hot_Counts[(length(LYH_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# LYH_MDC_Hot_Counts[(length(LYH_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# LYH_MDC_Hot_Counts[(length(LYH_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# LYH_MDC_Hot_Counts[(length(LYH_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# LYH_MDC_Hot_Counts[(length(LYH_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# LYH_MDC_Hot_Counts[(length(LYH_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# LYH_MDC_Hot_Counts[(length(LYH_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)

rm(LYH_data_all,LYH_MDC_Hot_NAs,LYH_MDC_NotHot_NAs,LYH_MDC_NotHot_Slim)

#Final Prep for Comparison
LYH_Ordered_NotHot_Counts <- LYH_MDC_NotHot_Counts[order(LYH_MDC_NotHot_Counts$Boot_MDC_ACME), ]
LYH_Ordered_Hot_Counts <- LYH_MDC_Hot_Counts[order(LYH_MDC_Hot_Counts$MDC_ACME), ]

#LYH_Final_Hot <- LYH_Ordered_Hot_Counts[LYH_Ordered_Hot_Counts$count_by_siteyear > 5, ]
LYH_Ordered_NotHot_Expected_Counts <- LYH_Ordered_NotHot_Counts[LYH_Ordered_NotHot_Counts$Boot_Count > 5, ]

# ifelse(length(LYH_NotHot_Counts_Ordered$Boot_MDC_ACME) == length(LYH_Hot_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(LYH_NotHot_Counts_Ordered$Boot_Count) == sum(LYH_Hot_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackLYH <- cbind(LYH_NotHot_Counts_Ordered,LYH_Hot_Counts_Ordered)
CountsOnlyLYH <- right_join(LYH_Ordered_Hot_Counts , LYH_Ordered_NotHot_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackLYH <- CountsOnlyLYH[!is.na(CountsOnlyLYH$Boot_Count) | !is.na(CountsOnlyLYH$count_by_siteyear),]
rm(CountsOnlyLYH)

sum(BackToBackLYH$count_by_siteyear)
sum(BackToBackLYH$Boot_Count)

colnames(BackToBackLYH) <- c("MDC_ACME","Heat","Boot")

BackToBackLYH$Boot <- as.numeric(BackToBackLYH$Boot)
sum(BackToBackLYH$Boot)
sum(BackToBackLYH$Heat)

BackToBackLYH_GreaterThanZero_Heat <- BackToBackLYH[BackToBackLYH$Boot > 0, ]

sum(BackToBackLYH_GreaterThanZero_Heat$Heat)
sum(BackToBackLYH_GreaterThanZero_Heat$Boot)

EO2_LYH <- (BackToBackLYH_GreaterThanZero_Heat$Heat - BackToBackLYH_GreaterThanZero_Heat$Boot)^2
E_LYH <- BackToBackLYH_GreaterThanZero_Heat$Boot
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

EO_Diff_LYH <- (BackToBackLYH_GreaterThanZero_Heat$Heat - BackToBackLYH_GreaterThanZero_Heat$Boot)
STD_RES_LYH <- EO_Diff_LYH / (sqrt(BackToBackLYH_GreaterThanZero_Heat$Boot))
BackToBackLYH_GreaterThanZero_Heat$Res <- round(STD_RES_LYH,digits = 3)
BackToBackLYH_GreaterThanZero_Heat$ResSig <- ifelse(abs(BackToBackLYH_GreaterThanZero_Heat$Res) > 3.8, TRUE, print("F"))

#OKV

OKV <- read_excel("~/Desktop/OKV.Final.Weather.xlsx",na="NA")
OKVselect <- OKV[1:5844, ]

OKVHotDaysSevere_lag0 <- OKVselect[OKVselect$HeatLag0Update == 1, ]
OKVHotDaysSevere_lag1 <- OKVselect[OKVselect$HeatLag1Update == 1, ]
OKVHotDaysSevere_LagCombine <- rbind(OKVHotDaysSevere_lag0,OKVHotDaysSevere_lag1)
OKVHotDaysSevere_LagCombine_DupsRem <- OKVHotDaysSevere_LagCombine[!duplicated(OKVHotDaysSevere_LagCombine), ]
OKVHotDaysUpdate <- OKVHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

OKVHotDaysUpdate_Summer <- OKVHotDaysUpdate[3 < month(OKVHotDaysUpdate$Date) & month(OKVHotDaysUpdate$Date) < 10, ]
OKVHotDaysUpdate_Summer$Date_Update <- format(as.Date(OKVHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(OKVHotDaysSevere_lag0,OKVHotDaysSevere_lag1,OKVHotDaysSevere_LagCombine,OKVHotDaysSevere_LagCombine_DupsRem,OKVHotDaysUpdate)

OKV_MDC <- MDC_FullFinal %>%
  filter(Wx == "OKV")

OKV_MDC_Hot_NAs <- left_join(OKVHotDaysUpdate_Summer, 
                             OKV_MDC,
                             by = c("Date_Update" = "Date"))
OKV_MDC_Hot <- OKV_MDC_Hot_NAs[!is.na(OKV_MDC_Hot_NAs$MDC_ACME),]
#NAOnly_Hot <- OKV_MDC_Hot[is.na(OKV_MDC_Hot$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(OKV_MDC_Hot$HeatLag0Update == 0 & OKV_MDC_Hot$HeatLag1Update == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(OKV_MDC_Hot$Station) / length(OKV_MDC$Date) #  13.49% of deaths happen on Hot Days
length(OKVHotDaysUpdate_Summer$Station) / length(OKV$Station) # 13.92% of days are hot day lag0 or lag1

####### BootStrapping OKV

OKVNotHotDaysSevere_lag0 <- OKVselect[!(OKVselect$HeatLag0Update == 1), ]
OKVNotHotDaysSevere_lag1 <- OKVselect[!(OKVselect$HeatLag1Update == 1), ]
OKVNotHotDaysSevere_LagCombine <- rbind(OKVNotHotDaysSevere_lag0,OKVNotHotDaysSevere_lag1)
OKVNotHotDaysSevere_LagCombine_DupsRem <- OKVNotHotDaysSevere_LagCombine[!duplicated(OKVNotHotDaysSevere_LagCombine), ]
OKVNotHotDaysUpdate <- OKVNotHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

OKVNotHotDaysUpdate_Summer <- OKVNotHotDaysUpdate[3 < month(OKVNotHotDaysUpdate$Date) & month(OKVNotHotDaysUpdate$Date) < 10, ]
OKVNotHotDaysUpdate_Summer$Date_Update <- format(as.Date(OKVNotHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(OKVNotHotDaysSevere_lag0,OKVNotHotDaysSevere_lag1,OKVNotHotDaysSevere_LagCombine,OKVNotHotDaysSevere_LagCombine_DupsRem,OKVNotHotDaysUpdate)

#test <- OKVNotHotDaysUpdate_Summer[OKVNotHotDaysUpdate_Summer$Month == 3, ]

OKV_MDC_NotHot_NAs <- left_join(OKVNotHotDaysUpdate_Summer, 
                                OKV_MDC,
                                by = c("Date_Update" = "Date"))
OKV_MDC_NotHot <- OKV_MDC_NotHot_NAs[!is.na(OKV_MDC_NotHot_NAs$MDC_ACME),]
#NAOnly_NotHot <- OKV_MDC_NotHot[is.na(OKV_MDC_NotHot$MDC_ACME), ] #0 obs. so yay!


OKV_MDC_NotHot_Slim <- OKV_MDC_NotHot %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotHot_Slim <- OKV_MDC_NotHot_Slim[is.na(OKV_MDC_NotHot_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   OKVNoStressRandom <- OKV_MDC_NotHot_Slim %>%
#     sample_n(length(OKV_MDC_Hot$Month),replace = TRUE)
#   write.csv(OKVNoStressRandom,paste0("~/Desktop/NoStressRandomWx/OKV_v3/OKV",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

OKV1 <- read.csv("~/Desktop/NoStressRandomWx/OKV_v3/OKV1")
OKV2 <- read.csv("~/Desktop/NoStressRandomWx/OKV_v3/OKV2")
ifelse(length(OKV1$X) == length(OKV_MDC_Hot$Month),1,0)
rm(OKV1,OKV2)

OKV_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/OKV_v3/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#OKV_data_all                                            # Print data to RStudio console

# Group by MDC

OKV_MDC_NotHot_Counts_InitialGrouping <- OKV_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
OKV_MDC_NotHot_Counts_InitialGrouping$Adjusted <- OKV_MDC_NotHot_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/OKV_v3/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
OKV_MDC_NotHot_Counts <- data.frame(cbind(OKV_MDC_NotHot_Counts_InitialGrouping$MDC_ACME,OKV_MDC_NotHot_Counts_InitialGrouping$Adjusted))

colnames(OKV_MDC_NotHot_Counts) <- c("Boot_MDC_ACME","Boot_Count")
OKV_MDC_NotHot_Counts$Boot_Count <- as.numeric(OKV_MDC_NotHot_Counts$Boot_Count)
sum(OKV_MDC_NotHot_Counts$Boot_Count)

#OKV_MDC_NotHot_Counts[OKV_MDC_NotHot_Counts$MDCAcme == "MDC22", 2] <- 0

OKV_MDC_Hot_Counts <- OKV_MDC_Hot %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# OKV_MDC_Hot_Counts[(length(OKV_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# OKV_MDC_Hot_Counts[(length(OKV_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# OKV_MDC_Hot_Counts[(length(OKV_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# OKV_MDC_Hot_Counts[(length(OKV_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# OKV_MDC_Hot_Counts[(length(OKV_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# OKV_MDC_Hot_Counts[(length(OKV_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# OKV_MDC_Hot_Counts[(length(OKV_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# OKV_MDC_Hot_Counts[(length(OKV_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)

rm(OKV_data_all,OKV_MDC_Hot_NAs,OKV_MDC_NotHot_NAs,OKV_MDC_NotHot_Slim)

#Final Prep for Comparison
OKV_Ordered_NotHot_Counts <- OKV_MDC_NotHot_Counts[order(OKV_MDC_NotHot_Counts$Boot_MDC_ACME), ]
OKV_Ordered_Hot_Counts <- OKV_MDC_Hot_Counts[order(OKV_MDC_Hot_Counts$MDC_ACME), ]

#OKV_Final_Hot <- OKV_Ordered_Hot_Counts[OKV_Ordered_Hot_Counts$count_by_siteyear > 5, ]
OKV_Ordered_NotHot_Expected_Counts <- OKV_Ordered_NotHot_Counts[OKV_Ordered_NotHot_Counts$Boot_Count > 5, ]

# ifelse(length(OKV_NotHot_Counts_Ordered$Boot_MDC_ACME) == length(OKV_Hot_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(OKV_NotHot_Counts_Ordered$Boot_Count) == sum(OKV_Hot_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackOKV <- cbind(OKV_NotHot_Counts_Ordered,OKV_Hot_Counts_Ordered)
CountsOnlyOKV <- right_join(OKV_Ordered_Hot_Counts , OKV_Ordered_NotHot_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackOKV <- CountsOnlyOKV[!is.na(CountsOnlyOKV$Boot_Count) | !is.na(CountsOnlyOKV$count_by_siteyear),]
rm(CountsOnlyOKV)

sum(BackToBackOKV$count_by_siteyear)
sum(BackToBackOKV$Boot_Count)

colnames(BackToBackOKV) <- c("MDC_ACME","Heat","Boot")

BackToBackOKV$Boot <- as.numeric(BackToBackOKV$Boot)
sum(BackToBackOKV$Boot)
sum(BackToBackOKV$Heat)

BackToBackOKV_GreaterThanZero_Heat <- BackToBackOKV[BackToBackOKV$Boot > 0, ]

sum(BackToBackOKV_GreaterThanZero_Heat$Heat)
sum(BackToBackOKV_GreaterThanZero_Heat$Boot)

EO2_OKV <- (BackToBackOKV_GreaterThanZero_Heat$Heat - BackToBackOKV_GreaterThanZero_Heat$Boot)^2
E_OKV <- BackToBackOKV_GreaterThanZero_Heat$Boot
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

EO_Diff_OKV <- (BackToBackOKV_GreaterThanZero_Heat$Heat - BackToBackOKV_GreaterThanZero_Heat$Boot)
STD_RES_OKV <- EO_Diff_OKV / (sqrt(BackToBackOKV_GreaterThanZero_Heat$Boot))
BackToBackOKV_GreaterThanZero_Heat$Res <- round(STD_RES_OKV,digits = 3)
BackToBackOKV_GreaterThanZero_Heat$ResSig <- ifelse(abs(BackToBackOKV_GreaterThanZero_Heat$Res) > 3.8, TRUE, print("F"))

#ORF

ORF <- read_excel("~/Desktop/ORF.Final.Weather.xlsx",na="NA")
ORFselect <- ORF[1:5844, ]

ORFHotDaysSevere_lag0 <- ORFselect[ORFselect$HeatLag0Update == 1, ]
ORFHotDaysSevere_lag1 <- ORFselect[ORFselect$HeatLag1Update == 1, ]
ORFHotDaysSevere_LagCombine <- rbind(ORFHotDaysSevere_lag0,ORFHotDaysSevere_lag1)
ORFHotDaysSevere_LagCombine_DupsRem <- ORFHotDaysSevere_LagCombine[!duplicated(ORFHotDaysSevere_LagCombine), ]
ORFHotDaysUpdate <- ORFHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

ORFHotDaysUpdate_Summer <- ORFHotDaysUpdate[3 < month(ORFHotDaysUpdate$Date) & month(ORFHotDaysUpdate$Date) < 10, ]
ORFHotDaysUpdate_Summer$Date_Update <- format(as.Date(ORFHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(ORFHotDaysSevere_lag0,ORFHotDaysSevere_lag1,ORFHotDaysSevere_LagCombine,ORFHotDaysSevere_LagCombine_DupsRem,ORFHotDaysUpdate)

ORF_MDC <- MDC_FullFinal %>%
  filter(Wx == "ORF")

ORF_MDC_Hot_NAs <- left_join(ORFHotDaysUpdate_Summer, 
                             ORF_MDC,
                             by = c("Date_Update" = "Date"))
ORF_MDC_Hot <- ORF_MDC_Hot_NAs[!is.na(ORF_MDC_Hot_NAs$MDC_ACME),]
#NAOnly_Hot <- ORF_MDC_Hot[is.na(ORF_MDC_Hot$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(ORF_MDC_Hot$HeatLag0Update == 0 & ORF_MDC_Hot$HeatLag1Update == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(ORF_MDC_Hot$Station) / length(ORF_MDC$Date) #  13.49% of deaths happen on Hot Days
length(ORFHotDaysUpdate_Summer$Station) / length(ORF$Station) # 13.92% of days are hot day lag0 or lag1

####### BootStrapping ORF

ORFNotHotDaysSevere_lag0 <- ORFselect[!(ORFselect$HeatLag0Update == 1), ]
ORFNotHotDaysSevere_lag1 <- ORFselect[!(ORFselect$HeatLag1Update == 1), ]
ORFNotHotDaysSevere_LagCombine <- rbind(ORFNotHotDaysSevere_lag0,ORFNotHotDaysSevere_lag1)
ORFNotHotDaysSevere_LagCombine_DupsRem <- ORFNotHotDaysSevere_LagCombine[!duplicated(ORFNotHotDaysSevere_LagCombine), ]
ORFNotHotDaysUpdate <- ORFNotHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

ORFNotHotDaysUpdate_Summer <- ORFNotHotDaysUpdate[3 < month(ORFNotHotDaysUpdate$Date) & month(ORFNotHotDaysUpdate$Date) < 10, ]
ORFNotHotDaysUpdate_Summer$Date_Update <- format(as.Date(ORFNotHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(ORFNotHotDaysSevere_lag0,ORFNotHotDaysSevere_lag1,ORFNotHotDaysSevere_LagCombine,ORFNotHotDaysSevere_LagCombine_DupsRem,ORFNotHotDaysUpdate)

#test <- ORFNotHotDaysUpdate_Summer[ORFNotHotDaysUpdate_Summer$Month == 3, ]

ORF_MDC_NotHot_NAs <- left_join(ORFNotHotDaysUpdate_Summer, 
                                ORF_MDC,
                                by = c("Date_Update" = "Date"))
ORF_MDC_NotHot <- ORF_MDC_NotHot_NAs[!is.na(ORF_MDC_NotHot_NAs$MDC_ACME),]
#NAOnly_NotHot <- ORF_MDC_NotHot[is.na(ORF_MDC_NotHot$MDC_ACME), ] #0 obs. so yay!


ORF_MDC_NotHot_Slim <- ORF_MDC_NotHot %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotHot_Slim <- ORF_MDC_NotHot_Slim[is.na(ORF_MDC_NotHot_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   ORFNoStressRandom <- ORF_MDC_NotHot_Slim %>%
#     sample_n(length(ORF_MDC_Hot$Month),replace = TRUE)
#   write.csv(ORFNoStressRandom,paste0("~/Desktop/NoStressRandomWx/ORF_v3/ORF",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

ORF1 <- read.csv("~/Desktop/NoStressRandomWx/ORF_v3/ORF1")
ORF2 <- read.csv("~/Desktop/NoStressRandomWx/ORF_v3/ORF2")
ifelse(length(ORF1$X) == length(ORF_MDC_Hot$Month),1,0)
rm(ORF1,ORF2)

ORF_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/ORF_v3/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#ORF_data_all                                            # Print data to RStudio console

# Group by MDC

ORF_MDC_NotHot_Counts_InitialGrouping <- ORF_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
ORF_MDC_NotHot_Counts_InitialGrouping$Adjusted <- ORF_MDC_NotHot_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/ORF_v3/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
ORF_MDC_NotHot_Counts <- data.frame(cbind(ORF_MDC_NotHot_Counts_InitialGrouping$MDC_ACME,ORF_MDC_NotHot_Counts_InitialGrouping$Adjusted))

colnames(ORF_MDC_NotHot_Counts) <- c("Boot_MDC_ACME","Boot_Count")
ORF_MDC_NotHot_Counts$Boot_Count <- as.numeric(ORF_MDC_NotHot_Counts$Boot_Count)
sum(ORF_MDC_NotHot_Counts$Boot_Count)

#ORF_MDC_NotHot_Counts[ORF_MDC_NotHot_Counts$MDCAcme == "MDC22", 2] <- 0

ORF_MDC_Hot_Counts <- ORF_MDC_Hot %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# ORF_MDC_Hot_Counts[(length(ORF_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# ORF_MDC_Hot_Counts[(length(ORF_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# ORF_MDC_Hot_Counts[(length(ORF_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# ORF_MDC_Hot_Counts[(length(ORF_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# ORF_MDC_Hot_Counts[(length(ORF_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# ORF_MDC_Hot_Counts[(length(ORF_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# ORF_MDC_Hot_Counts[(length(ORF_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# ORF_MDC_Hot_Counts[(length(ORF_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)

rm(ORF_data_all,ORF_MDC_Hot_NAs,ORF_MDC_NotHot_NAs,ORF_MDC_NotHot_Slim)

#Final Prep for Comparison
ORF_Ordered_NotHot_Counts <- ORF_MDC_NotHot_Counts[order(ORF_MDC_NotHot_Counts$Boot_MDC_ACME), ]
ORF_Ordered_Hot_Counts <- ORF_MDC_Hot_Counts[order(ORF_MDC_Hot_Counts$MDC_ACME), ]

#ORF_Final_Hot <- ORF_Ordered_Hot_Counts[ORF_Ordered_Hot_Counts$count_by_siteyear > 5, ]
ORF_Ordered_NotHot_Expected_Counts <- ORF_Ordered_NotHot_Counts[ORF_Ordered_NotHot_Counts$Boot_Count > 5, ]

# ifelse(length(ORF_NotHot_Counts_Ordered$Boot_MDC_ACME) == length(ORF_Hot_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(ORF_NotHot_Counts_Ordered$Boot_Count) == sum(ORF_Hot_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackORF <- cbind(ORF_NotHot_Counts_Ordered,ORF_Hot_Counts_Ordered)
CountsOnlyORF <- right_join(ORF_Ordered_Hot_Counts , ORF_Ordered_NotHot_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackORF <- CountsOnlyORF[!is.na(CountsOnlyORF$Boot_Count) | !is.na(CountsOnlyORF$count_by_siteyear),]
rm(CountsOnlyORF)

sum(BackToBackORF$count_by_siteyear)
sum(BackToBackORF$Boot_Count)

colnames(BackToBackORF) <- c("MDC_ACME","Heat","Boot")

BackToBackORF$Boot <- as.numeric(BackToBackORF$Boot)
sum(BackToBackORF$Boot)
sum(BackToBackORF$Heat)

BackToBackORF_GreaterThanZero_Heat <- BackToBackORF[BackToBackORF$Boot > 0, ]

sum(BackToBackORF_GreaterThanZero_Heat$Heat)
sum(BackToBackORF_GreaterThanZero_Heat$Boot)

EO2_ORF <- (BackToBackORF_GreaterThanZero_Heat$Heat - BackToBackORF_GreaterThanZero_Heat$Boot)^2
E_ORF <- BackToBackORF_GreaterThanZero_Heat$Boot
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

EO_Diff_ORF <- (BackToBackORF_GreaterThanZero_Heat$Heat - BackToBackORF_GreaterThanZero_Heat$Boot)
STD_RES_ORF <- EO_Diff_ORF / (sqrt(BackToBackORF_GreaterThanZero_Heat$Boot))
BackToBackORF_GreaterThanZero_Heat$Res <- round(STD_RES_ORF,digits = 3)
BackToBackORF_GreaterThanZero_Heat$ResSig <- ifelse(abs(BackToBackORF_GreaterThanZero_Heat$Res) > 3.8, TRUE, print("F"))

#PHF

PHF <- read_excel("~/Desktop/PHF.Final.Weather.xlsx",na="NA")
PHFselect <- PHF[1:5844, ]

PHFHotDaysSevere_lag0 <- PHFselect[PHFselect$HeatLag0Update == 1, ]
PHFHotDaysSevere_lag1 <- PHFselect[PHFselect$HeatLag1Update == 1, ]
PHFHotDaysSevere_LagCombine <- rbind(PHFHotDaysSevere_lag0,PHFHotDaysSevere_lag1)
PHFHotDaysSevere_LagCombine_DupsRem <- PHFHotDaysSevere_LagCombine[!duplicated(PHFHotDaysSevere_LagCombine), ]
PHFHotDaysUpdate <- PHFHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

PHFHotDaysUpdate_Summer <- PHFHotDaysUpdate[3 < month(PHFHotDaysUpdate$Date) & month(PHFHotDaysUpdate$Date) < 10, ]
PHFHotDaysUpdate_Summer$Date_Update <- format(as.Date(PHFHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(PHFHotDaysSevere_lag0,PHFHotDaysSevere_lag1,PHFHotDaysSevere_LagCombine,PHFHotDaysSevere_LagCombine_DupsRem,PHFHotDaysUpdate)

PHF_MDC <- MDC_FullFinal %>%
  filter(Wx == "PHF")

PHF_MDC_Hot_NAs <- left_join(PHFHotDaysUpdate_Summer, 
                             PHF_MDC,
                             by = c("Date_Update" = "Date"))
PHF_MDC_Hot <- PHF_MDC_Hot_NAs[!is.na(PHF_MDC_Hot_NAs$MDC_ACME),]
#NAOnly_Hot <- PHF_MDC_Hot[is.na(PHF_MDC_Hot$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(PHF_MDC_Hot$HeatLag0Update == 0 & PHF_MDC_Hot$HeatLag1Update == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(PHF_MDC_Hot$Station) / length(PHF_MDC$Date) #  13.49% of deaths happen on Hot Days
length(PHFHotDaysUpdate_Summer$Station) / length(PHF$Station) # 13.92% of days are hot day lag0 or lag1

####### BootStrapping PHF

PHFNotHotDaysSevere_lag0 <- PHFselect[!(PHFselect$HeatLag0Update == 1), ]
PHFNotHotDaysSevere_lag1 <- PHFselect[!(PHFselect$HeatLag1Update == 1), ]
PHFNotHotDaysSevere_LagCombine <- rbind(PHFNotHotDaysSevere_lag0,PHFNotHotDaysSevere_lag1)
PHFNotHotDaysSevere_LagCombine_DupsRem <- PHFNotHotDaysSevere_LagCombine[!duplicated(PHFNotHotDaysSevere_LagCombine), ]
PHFNotHotDaysUpdate <- PHFNotHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

PHFNotHotDaysUpdate_Summer <- PHFNotHotDaysUpdate[3 < month(PHFNotHotDaysUpdate$Date) & month(PHFNotHotDaysUpdate$Date) < 10, ]
PHFNotHotDaysUpdate_Summer$Date_Update <- format(as.Date(PHFNotHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(PHFNotHotDaysSevere_lag0,PHFNotHotDaysSevere_lag1,PHFNotHotDaysSevere_LagCombine,PHFNotHotDaysSevere_LagCombine_DupsRem,PHFNotHotDaysUpdate)

#test <- PHFNotHotDaysUpdate_Summer[PHFNotHotDaysUpdate_Summer$Month == 3, ]

PHF_MDC_NotHot_NAs <- left_join(PHFNotHotDaysUpdate_Summer, 
                                PHF_MDC,
                                by = c("Date_Update" = "Date"))
PHF_MDC_NotHot <- PHF_MDC_NotHot_NAs[!is.na(PHF_MDC_NotHot_NAs$MDC_ACME),]
#NAOnly_NotHot <- PHF_MDC_NotHot[is.na(PHF_MDC_NotHot$MDC_ACME), ] #0 obs. so yay!


PHF_MDC_NotHot_Slim <- PHF_MDC_NotHot %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotHot_Slim <- PHF_MDC_NotHot_Slim[is.na(PHF_MDC_NotHot_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   PHFNoStressRandom <- PHF_MDC_NotHot_Slim %>%
#     sample_n(length(PHF_MDC_Hot$Month),replace = TRUE)
#   write.csv(PHFNoStressRandom,paste0("~/Desktop/NoStressRandomWx/PHF_v3/PHF",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

PHF1 <- read.csv("~/Desktop/NoStressRandomWx/PHF_v3/PHF1")
PHF2 <- read.csv("~/Desktop/NoStressRandomWx/PHF_v3/PHF2")
ifelse(length(PHF1$X) == length(PHF_MDC_Hot$Month),1,0)
rm(PHF1,PHF2)

PHF_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/PHF_v3/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#PHF_data_all                                            # Print data to RStudio console

# Group by MDC

PHF_MDC_NotHot_Counts_InitialGrouping <- PHF_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
PHF_MDC_NotHot_Counts_InitialGrouping$Adjusted <- PHF_MDC_NotHot_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/PHF_v3/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
PHF_MDC_NotHot_Counts <- data.frame(cbind(PHF_MDC_NotHot_Counts_InitialGrouping$MDC_ACME,PHF_MDC_NotHot_Counts_InitialGrouping$Adjusted))

colnames(PHF_MDC_NotHot_Counts) <- c("Boot_MDC_ACME","Boot_Count")
PHF_MDC_NotHot_Counts$Boot_Count <- as.numeric(PHF_MDC_NotHot_Counts$Boot_Count)
sum(PHF_MDC_NotHot_Counts$Boot_Count)

#PHF_MDC_NotHot_Counts[PHF_MDC_NotHot_Counts$MDCAcme == "MDC22", 2] <- 0

PHF_MDC_Hot_Counts <- PHF_MDC_Hot %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# PHF_MDC_Hot_Counts[(length(PHF_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# PHF_MDC_Hot_Counts[(length(PHF_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# PHF_MDC_Hot_Counts[(length(PHF_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# PHF_MDC_Hot_Counts[(length(PHF_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# PHF_MDC_Hot_Counts[(length(PHF_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# PHF_MDC_Hot_Counts[(length(PHF_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# PHF_MDC_Hot_Counts[(length(PHF_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# PHF_MDC_Hot_Counts[(length(PHF_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)

rm(PHF_data_all,PHF_MDC_Hot_NAs,PHF_MDC_NotHot_NAs,PHF_MDC_NotHot_Slim)

#Final Prep for Comparison
PHF_Ordered_NotHot_Counts <- PHF_MDC_NotHot_Counts[order(PHF_MDC_NotHot_Counts$Boot_MDC_ACME), ]
PHF_Ordered_Hot_Counts <- PHF_MDC_Hot_Counts[order(PHF_MDC_Hot_Counts$MDC_ACME), ]

#PHF_Final_Hot <- PHF_Ordered_Hot_Counts[PHF_Ordered_Hot_Counts$count_by_siteyear > 5, ]
PHF_Ordered_NotHot_Expected_Counts <- PHF_Ordered_NotHot_Counts[PHF_Ordered_NotHot_Counts$Boot_Count > 5, ]

# ifelse(length(PHF_NotHot_Counts_Ordered$Boot_MDC_ACME) == length(PHF_Hot_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(PHF_NotHot_Counts_Ordered$Boot_Count) == sum(PHF_Hot_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackPHF <- cbind(PHF_NotHot_Counts_Ordered,PHF_Hot_Counts_Ordered)
CountsOnlyPHF <- right_join(PHF_Ordered_Hot_Counts , PHF_Ordered_NotHot_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackPHF <- CountsOnlyPHF[!is.na(CountsOnlyPHF$Boot_Count) | !is.na(CountsOnlyPHF$count_by_siteyear),]
rm(CountsOnlyPHF)

sum(BackToBackPHF$count_by_siteyear)
sum(BackToBackPHF$Boot_Count)

colnames(BackToBackPHF) <- c("MDC_ACME","Heat","Boot")

BackToBackPHF$Boot <- as.numeric(BackToBackPHF$Boot)
sum(BackToBackPHF$Boot)
sum(BackToBackPHF$Heat)

BackToBackPHF_GreaterThanZero_Heat <- BackToBackPHF[BackToBackPHF$Boot > 0, ]

sum(BackToBackPHF_GreaterThanZero_Heat$Heat)
sum(BackToBackPHF_GreaterThanZero_Heat$Boot)

EO2_PHF <- (BackToBackPHF_GreaterThanZero_Heat$Heat - BackToBackPHF_GreaterThanZero_Heat$Boot)^2
E_PHF <- BackToBackPHF_GreaterThanZero_Heat$Boot
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

EO_Diff_PHF <- (BackToBackPHF_GreaterThanZero_Heat$Heat - BackToBackPHF_GreaterThanZero_Heat$Boot)
STD_RES_PHF <- EO_Diff_PHF / (sqrt(BackToBackPHF_GreaterThanZero_Heat$Boot))
BackToBackPHF_GreaterThanZero_Heat$Res <- round(STD_RES_PHF,digits = 3)
BackToBackPHF_GreaterThanZero_Heat$ResSig <- ifelse(abs(BackToBackPHF_GreaterThanZero_Heat$Res) > 3.8, TRUE, print("F"))

#RIC

RIC <- read_excel("~/Desktop/RIC.Final.Weather.xlsx",na="NA")
RICselect <- RIC[1:5844, ]

RICHotDaysSevere_lag0 <- RICselect[RICselect$HeatLag0Update == 1, ]
RICHotDaysSevere_lag1 <- RICselect[RICselect$HeatLag1Update == 1, ]
RICHotDaysSevere_LagCombine <- rbind(RICHotDaysSevere_lag0,RICHotDaysSevere_lag1)
RICHotDaysSevere_LagCombine_DupsRem <- RICHotDaysSevere_LagCombine[!duplicated(RICHotDaysSevere_LagCombine), ]
RICHotDaysUpdate <- RICHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

RICHotDaysUpdate_Summer <- RICHotDaysUpdate[3 < month(RICHotDaysUpdate$Date) & month(RICHotDaysUpdate$Date) < 10, ]
RICHotDaysUpdate_Summer$Date_Update <- format(as.Date(RICHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(RICHotDaysSevere_lag0,RICHotDaysSevere_lag1,RICHotDaysSevere_LagCombine,RICHotDaysSevere_LagCombine_DupsRem,RICHotDaysUpdate)

RIC_MDC <- MDC_FullFinal %>%
  filter(Wx == "RIC")

RIC_MDC_Hot_NAs <- left_join(RICHotDaysUpdate_Summer, 
                             RIC_MDC,
                             by = c("Date_Update" = "Date"))
RIC_MDC_Hot <- RIC_MDC_Hot_NAs[!is.na(RIC_MDC_Hot_NAs$MDC_ACME),]
#NAOnly_Hot <- RIC_MDC_Hot[is.na(RIC_MDC_Hot$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(RIC_MDC_Hot$HeatLag0Update == 0 & RIC_MDC_Hot$HeatLag1Update == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(RIC_MDC_Hot$Station) / length(RIC_MDC$Date) #  13.49% of deaths happen on Hot Days
length(RICHotDaysUpdate_Summer$Station) / length(RIC$Station) # 13.92% of days are hot day lag0 or lag1

####### BootStrapping RIC

RICNotHotDaysSevere_lag0 <- RICselect[!(RICselect$HeatLag0Update == 1), ]
RICNotHotDaysSevere_lag1 <- RICselect[!(RICselect$HeatLag1Update == 1), ]
RICNotHotDaysSevere_LagCombine <- rbind(RICNotHotDaysSevere_lag0,RICNotHotDaysSevere_lag1)
RICNotHotDaysSevere_LagCombine_DupsRem <- RICNotHotDaysSevere_LagCombine[!duplicated(RICNotHotDaysSevere_LagCombine), ]
RICNotHotDaysUpdate <- RICNotHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

RICNotHotDaysUpdate_Summer <- RICNotHotDaysUpdate[3 < month(RICNotHotDaysUpdate$Date) & month(RICNotHotDaysUpdate$Date) < 10, ]
RICNotHotDaysUpdate_Summer$Date_Update <- format(as.Date(RICNotHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(RICNotHotDaysSevere_lag0,RICNotHotDaysSevere_lag1,RICNotHotDaysSevere_LagCombine,RICNotHotDaysSevere_LagCombine_DupsRem,RICNotHotDaysUpdate)

#test <- RICNotHotDaysUpdate_Summer[RICNotHotDaysUpdate_Summer$Month == 3, ]

RIC_MDC_NotHot_NAs <- left_join(RICNotHotDaysUpdate_Summer, 
                                RIC_MDC,
                                by = c("Date_Update" = "Date"))
RIC_MDC_NotHot <- RIC_MDC_NotHot_NAs[!is.na(RIC_MDC_NotHot_NAs$MDC_ACME),]
#NAOnly_NotHot <- RIC_MDC_NotHot[is.na(RIC_MDC_NotHot$MDC_ACME), ] #0 obs. so yay!


RIC_MDC_NotHot_Slim <- RIC_MDC_NotHot %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotHot_Slim <- RIC_MDC_NotHot_Slim[is.na(RIC_MDC_NotHot_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   RICNoStressRandom <- RIC_MDC_NotHot_Slim %>%
#     sample_n(length(RIC_MDC_Hot$Month),replace = TRUE)
#   write.csv(RICNoStressRandom,paste0("~/Desktop/NoStressRandomWx/RIC_v3/RIC",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

RIC1 <- read.csv("~/Desktop/NoStressRandomWx/RIC_v3/RIC1")
RIC2 <- read.csv("~/Desktop/NoStressRandomWx/RIC_v3/RIC2")
ifelse(length(RIC1$X) == length(RIC_MDC_Hot$Month),1,0)
rm(RIC1,RIC2)

RIC_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/RIC_v3/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#RIC_data_all                                            # Print data to RStudio console

# Group by MDC

RIC_MDC_NotHot_Counts_InitialGrouping <- RIC_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
RIC_MDC_NotHot_Counts_InitialGrouping$Adjusted <- RIC_MDC_NotHot_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/RIC_v3/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
RIC_MDC_NotHot_Counts <- data.frame(cbind(RIC_MDC_NotHot_Counts_InitialGrouping$MDC_ACME,RIC_MDC_NotHot_Counts_InitialGrouping$Adjusted))

colnames(RIC_MDC_NotHot_Counts) <- c("Boot_MDC_ACME","Boot_Count")
RIC_MDC_NotHot_Counts$Boot_Count <- as.numeric(RIC_MDC_NotHot_Counts$Boot_Count)
sum(RIC_MDC_NotHot_Counts$Boot_Count)

#RIC_MDC_NotHot_Counts[RIC_MDC_NotHot_Counts$MDCAcme == "MDC22", 2] <- 0

RIC_MDC_Hot_Counts <- RIC_MDC_Hot %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# RIC_MDC_Hot_Counts[(length(RIC_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# RIC_MDC_Hot_Counts[(length(RIC_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# RIC_MDC_Hot_Counts[(length(RIC_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# RIC_MDC_Hot_Counts[(length(RIC_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# RIC_MDC_Hot_Counts[(length(RIC_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# RIC_MDC_Hot_Counts[(length(RIC_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# RIC_MDC_Hot_Counts[(length(RIC_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# RIC_MDC_Hot_Counts[(length(RIC_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)

rm(RIC_data_all,RIC_MDC_Hot_NAs,RIC_MDC_NotHot_NAs,RIC_MDC_NotHot_Slim)

#Final Prep for Comparison
RIC_Ordered_NotHot_Counts <- RIC_MDC_NotHot_Counts[order(RIC_MDC_NotHot_Counts$Boot_MDC_ACME), ]
RIC_Ordered_Hot_Counts <- RIC_MDC_Hot_Counts[order(RIC_MDC_Hot_Counts$MDC_ACME), ]

#RIC_Final_Hot <- RIC_Ordered_Hot_Counts[RIC_Ordered_Hot_Counts$count_by_siteyear > 5, ]
RIC_Ordered_NotHot_Expected_Counts <- RIC_Ordered_NotHot_Counts[RIC_Ordered_NotHot_Counts$Boot_Count > 5, ]

# ifelse(length(RIC_NotHot_Counts_Ordered$Boot_MDC_ACME) == length(RIC_Hot_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(RIC_NotHot_Counts_Ordered$Boot_Count) == sum(RIC_Hot_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackRIC <- cbind(RIC_NotHot_Counts_Ordered,RIC_Hot_Counts_Ordered)
CountsOnlyRIC <- right_join(RIC_Ordered_Hot_Counts , RIC_Ordered_NotHot_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackRIC <- CountsOnlyRIC[!is.na(CountsOnlyRIC$Boot_Count) | !is.na(CountsOnlyRIC$count_by_siteyear),]
rm(CountsOnlyRIC)

sum(BackToBackRIC$count_by_siteyear)
sum(BackToBackRIC$Boot_Count)

colnames(BackToBackRIC) <- c("MDC_ACME","Heat","Boot")

BackToBackRIC$Boot <- as.numeric(BackToBackRIC$Boot)
sum(BackToBackRIC$Boot)
sum(BackToBackRIC$Heat)

BackToBackRIC_GreaterThanZero_Heat <- BackToBackRIC[BackToBackRIC$Boot > 0, ]

sum(BackToBackRIC_GreaterThanZero_Heat$Heat)
sum(BackToBackRIC_GreaterThanZero_Heat$Boot)

EO2_RIC <- (BackToBackRIC_GreaterThanZero_Heat$Heat - BackToBackRIC_GreaterThanZero_Heat$Boot)^2
E_RIC <- BackToBackRIC_GreaterThanZero_Heat$Boot
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

EO_Diff_RIC <- (BackToBackRIC_GreaterThanZero_Heat$Heat - BackToBackRIC_GreaterThanZero_Heat$Boot)
STD_RES_RIC <- EO_Diff_RIC / (sqrt(BackToBackRIC_GreaterThanZero_Heat$Boot))
BackToBackRIC_GreaterThanZero_Heat$Res <- round(STD_RES_RIC,digits = 3)
BackToBackRIC_GreaterThanZero_Heat$ResSig <- ifelse(abs(BackToBackRIC_GreaterThanZero_Heat$Res) > 3.8, TRUE, print("F"))

#ROA

ROA <- read_excel("~/Desktop/ROA.Final.Weather.xlsx",na="NA")
ROAselect <- ROA[1:5844, ]

ROAHotDaysSevere_lag0 <- ROAselect[ROAselect$HeatLag0Update == 1, ]
ROAHotDaysSevere_lag1 <- ROAselect[ROAselect$HeatLag1Update == 1, ]
ROAHotDaysSevere_LagCombine <- rbind(ROAHotDaysSevere_lag0,ROAHotDaysSevere_lag1)
ROAHotDaysSevere_LagCombine_DupsRem <- ROAHotDaysSevere_LagCombine[!duplicated(ROAHotDaysSevere_LagCombine), ]
ROAHotDaysUpdate <- ROAHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

ROAHotDaysUpdate_Summer <- ROAHotDaysUpdate[3 < month(ROAHotDaysUpdate$Date) & month(ROAHotDaysUpdate$Date) < 10, ]
ROAHotDaysUpdate_Summer$Date_Update <- format(as.Date(ROAHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(ROAHotDaysSevere_lag0,ROAHotDaysSevere_lag1,ROAHotDaysSevere_LagCombine,ROAHotDaysSevere_LagCombine_DupsRem,ROAHotDaysUpdate)

ROA_MDC <- MDC_FullFinal %>%
  filter(Wx == "ROA")

ROA_MDC_Hot_NAs <- left_join(ROAHotDaysUpdate_Summer, 
                             ROA_MDC,
                             by = c("Date_Update" = "Date"))
ROA_MDC_Hot <- ROA_MDC_Hot_NAs[!is.na(ROA_MDC_Hot_NAs$MDC_ACME),]
#NAOnly_Hot <- ROA_MDC_Hot[is.na(ROA_MDC_Hot$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(ROA_MDC_Hot$HeatLag0Update == 0 & ROA_MDC_Hot$HeatLag1Update == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(ROA_MDC_Hot$Station) / length(ROA_MDC$Date) #  13.49% of deaths happen on Hot Days
length(ROAHotDaysUpdate_Summer$Station) / length(ROA$Station) # 13.92% of days are hot day lag0 or lag1

####### BootStrapping ROA

ROANotHotDaysSevere_lag0 <- ROAselect[!(ROAselect$HeatLag0Update == 1), ]
ROANotHotDaysSevere_lag1 <- ROAselect[!(ROAselect$HeatLag1Update == 1), ]
ROANotHotDaysSevere_LagCombine <- rbind(ROANotHotDaysSevere_lag0,ROANotHotDaysSevere_lag1)
ROANotHotDaysSevere_LagCombine_DupsRem <- ROANotHotDaysSevere_LagCombine[!duplicated(ROANotHotDaysSevere_LagCombine), ]
ROANotHotDaysUpdate <- ROANotHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

ROANotHotDaysUpdate_Summer <- ROANotHotDaysUpdate[3 < month(ROANotHotDaysUpdate$Date) & month(ROANotHotDaysUpdate$Date) < 10, ]
ROANotHotDaysUpdate_Summer$Date_Update <- format(as.Date(ROANotHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(ROANotHotDaysSevere_lag0,ROANotHotDaysSevere_lag1,ROANotHotDaysSevere_LagCombine,ROANotHotDaysSevere_LagCombine_DupsRem,ROANotHotDaysUpdate)

#test <- ROANotHotDaysUpdate_Summer[ROANotHotDaysUpdate_Summer$Month == 3, ]

ROA_MDC_NotHot_NAs <- left_join(ROANotHotDaysUpdate_Summer, 
                                ROA_MDC,
                                by = c("Date_Update" = "Date"))
ROA_MDC_NotHot <- ROA_MDC_NotHot_NAs[!is.na(ROA_MDC_NotHot_NAs$MDC_ACME),]
#NAOnly_NotHot <- ROA_MDC_NotHot[is.na(ROA_MDC_NotHot$MDC_ACME), ] #0 obs. so yay!


ROA_MDC_NotHot_Slim <- ROA_MDC_NotHot %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotHot_Slim <- ROA_MDC_NotHot_Slim[is.na(ROA_MDC_NotHot_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   ROANoStressRandom <- ROA_MDC_NotHot_Slim %>%
#     sample_n(length(ROA_MDC_Hot$Month),replace = TRUE)
#   write.csv(ROANoStressRandom,paste0("~/Desktop/NoStressRandomWx/ROA_v3/ROA",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

ROA1 <- read.csv("~/Desktop/NoStressRandomWx/ROA_v3/ROA1")
ROA2 <- read.csv("~/Desktop/NoStressRandomWx/ROA_v3/ROA2")
ifelse(length(ROA1$X) == length(ROA_MDC_Hot$Month),1,0)
rm(ROA1,ROA2)

ROA_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/ROA_v3/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#ROA_data_all                                            # Print data to RStudio console

# Group by MDC

ROA_MDC_NotHot_Counts_InitialGrouping <- ROA_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
ROA_MDC_NotHot_Counts_InitialGrouping$Adjusted <- ROA_MDC_NotHot_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/ROA_v3/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
ROA_MDC_NotHot_Counts <- data.frame(cbind(ROA_MDC_NotHot_Counts_InitialGrouping$MDC_ACME,ROA_MDC_NotHot_Counts_InitialGrouping$Adjusted))

colnames(ROA_MDC_NotHot_Counts) <- c("Boot_MDC_ACME","Boot_Count")
ROA_MDC_NotHot_Counts$Boot_Count <- as.numeric(ROA_MDC_NotHot_Counts$Boot_Count)
sum(ROA_MDC_NotHot_Counts$Boot_Count)

#ROA_MDC_NotHot_Counts[ROA_MDC_NotHot_Counts$MDCAcme == "MDC22", 2] <- 0

ROA_MDC_Hot_Counts <- ROA_MDC_Hot %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# ROA_MDC_Hot_Counts[(length(ROA_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# ROA_MDC_Hot_Counts[(length(ROA_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# ROA_MDC_Hot_Counts[(length(ROA_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# ROA_MDC_Hot_Counts[(length(ROA_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# ROA_MDC_Hot_Counts[(length(ROA_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# ROA_MDC_Hot_Counts[(length(ROA_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# ROA_MDC_Hot_Counts[(length(ROA_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# ROA_MDC_Hot_Counts[(length(ROA_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)

rm(ROA_data_all,ROA_MDC_Hot_NAs,ROA_MDC_NotHot_NAs,ROA_MDC_NotHot_Slim)

#Final Prep for Comparison
ROA_Ordered_NotHot_Counts <- ROA_MDC_NotHot_Counts[order(ROA_MDC_NotHot_Counts$Boot_MDC_ACME), ]
ROA_Ordered_Hot_Counts <- ROA_MDC_Hot_Counts[order(ROA_MDC_Hot_Counts$MDC_ACME), ]

#ROA_Final_Hot <- ROA_Ordered_Hot_Counts[ROA_Ordered_Hot_Counts$count_by_siteyear > 5, ]
ROA_Ordered_NotHot_Expected_Counts <- ROA_Ordered_NotHot_Counts[ROA_Ordered_NotHot_Counts$Boot_Count > 5, ]

# ifelse(length(ROA_NotHot_Counts_Ordered$Boot_MDC_ACME) == length(ROA_Hot_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(ROA_NotHot_Counts_Ordered$Boot_Count) == sum(ROA_Hot_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackROA <- cbind(ROA_NotHot_Counts_Ordered,ROA_Hot_Counts_Ordered)
CountsOnlyROA <- right_join(ROA_Ordered_Hot_Counts , ROA_Ordered_NotHot_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackROA <- CountsOnlyROA[!is.na(CountsOnlyROA$Boot_Count) | !is.na(CountsOnlyROA$count_by_siteyear),]
rm(CountsOnlyROA)

sum(BackToBackROA$count_by_siteyear)
sum(BackToBackROA$Boot_Count)

colnames(BackToBackROA) <- c("MDC_ACME","Heat","Boot")

BackToBackROA$Boot <- as.numeric(BackToBackROA$Boot)
sum(BackToBackROA$Boot)
sum(BackToBackROA$Heat)

BackToBackROA_GreaterThanZero_Heat <- BackToBackROA[BackToBackROA$Boot > 0, ]

sum(BackToBackROA_GreaterThanZero_Heat$Heat)
sum(BackToBackROA_GreaterThanZero_Heat$Boot)

EO2_ROA <- (BackToBackROA_GreaterThanZero_Heat$Heat - BackToBackROA_GreaterThanZero_Heat$Boot)^2
E_ROA <- BackToBackROA_GreaterThanZero_Heat$Boot
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

EO_Diff_ROA <- (BackToBackROA_GreaterThanZero_Heat$Heat - BackToBackROA_GreaterThanZero_Heat$Boot)
STD_RES_ROA <- EO_Diff_ROA / (sqrt(BackToBackROA_GreaterThanZero_Heat$Boot))
BackToBackROA_GreaterThanZero_Heat$Res <- round(STD_RES_ROA,digits = 3)
BackToBackROA_GreaterThanZero_Heat$ResSig <- ifelse(abs(BackToBackROA_GreaterThanZero_Heat$Res) > 3.8, TRUE, print("F"))

#SHD

SHD <- read_excel("~/Desktop/SHD.Final.Weather.xlsx",na="NA")
SHDselect <- SHD[1:5844, ]

SHDHotDaysSevere_lag0 <- SHDselect[SHDselect$HeatLag0Update == 1, ]
SHDHotDaysSevere_lag1 <- SHDselect[SHDselect$HeatLag1Update == 1, ]
SHDHotDaysSevere_LagCombine <- rbind(SHDHotDaysSevere_lag0,SHDHotDaysSevere_lag1)
SHDHotDaysSevere_LagCombine_DupsRem <- SHDHotDaysSevere_LagCombine[!duplicated(SHDHotDaysSevere_LagCombine), ]
SHDHotDaysUpdate <- SHDHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

SHDHotDaysUpdate_Summer <- SHDHotDaysUpdate[3 < month(SHDHotDaysUpdate$Date) & month(SHDHotDaysUpdate$Date) < 10, ]
SHDHotDaysUpdate_Summer$Date_Update <- format(as.Date(SHDHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(SHDHotDaysSevere_lag0,SHDHotDaysSevere_lag1,SHDHotDaysSevere_LagCombine,SHDHotDaysSevere_LagCombine_DupsRem,SHDHotDaysUpdate)

SHD_MDC <- MDC_FullFinal %>%
  filter(Wx == "SHD")

SHD_MDC_Hot_NAs <- left_join(SHDHotDaysUpdate_Summer, 
                             SHD_MDC,
                             by = c("Date_Update" = "Date"))
SHD_MDC_Hot <- SHD_MDC_Hot_NAs[!is.na(SHD_MDC_Hot_NAs$MDC_ACME),]
#NAOnly_Hot <- SHD_MDC_Hot[is.na(SHD_MDC_Hot$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(SHD_MDC_Hot$HeatLag0Update == 0 & SHD_MDC_Hot$HeatLag1Update == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(SHD_MDC_Hot$Station) / length(SHD_MDC$Date) #  13.49% of deaths happen on Hot Days
length(SHDHotDaysUpdate_Summer$Station) / length(SHD$Station) # 13.92% of days are hot day lag0 or lag1

####### BootStrapping SHD

SHDNotHotDaysSevere_lag0 <- SHDselect[!(SHDselect$HeatLag0Update == 1), ]
SHDNotHotDaysSevere_lag1 <- SHDselect[!(SHDselect$HeatLag1Update == 1), ]
SHDNotHotDaysSevere_LagCombine <- rbind(SHDNotHotDaysSevere_lag0,SHDNotHotDaysSevere_lag1)
SHDNotHotDaysSevere_LagCombine_DupsRem <- SHDNotHotDaysSevere_LagCombine[!duplicated(SHDNotHotDaysSevere_LagCombine), ]
SHDNotHotDaysUpdate <- SHDNotHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

SHDNotHotDaysUpdate_Summer <- SHDNotHotDaysUpdate[3 < month(SHDNotHotDaysUpdate$Date) & month(SHDNotHotDaysUpdate$Date) < 10, ]
SHDNotHotDaysUpdate_Summer$Date_Update <- format(as.Date(SHDNotHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(SHDNotHotDaysSevere_lag0,SHDNotHotDaysSevere_lag1,SHDNotHotDaysSevere_LagCombine,SHDNotHotDaysSevere_LagCombine_DupsRem,SHDNotHotDaysUpdate)

#test <- SHDNotHotDaysUpdate_Summer[SHDNotHotDaysUpdate_Summer$Month == 3, ]

SHD_MDC_NotHot_NAs <- left_join(SHDNotHotDaysUpdate_Summer, 
                                SHD_MDC,
                                by = c("Date_Update" = "Date"))
SHD_MDC_NotHot <- SHD_MDC_NotHot_NAs[!is.na(SHD_MDC_NotHot_NAs$MDC_ACME),]
#NAOnly_NotHot <- SHD_MDC_NotHot[is.na(SHD_MDC_NotHot$MDC_ACME), ] #0 obs. so yay!


SHD_MDC_NotHot_Slim <- SHD_MDC_NotHot %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotHot_Slim <- SHD_MDC_NotHot_Slim[is.na(SHD_MDC_NotHot_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   SHDNoStressRandom <- SHD_MDC_NotHot_Slim %>%
#     sample_n(length(SHD_MDC_Hot$Month),replace = TRUE)
#   write.csv(SHDNoStressRandom,paste0("~/Desktop/NoStressRandomWx/SHD_v3/SHD",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

SHD1 <- read.csv("~/Desktop/NoStressRandomWx/SHD_v3/SHD1")
SHD2 <- read.csv("~/Desktop/NoStressRandomWx/SHD_v3/SHD2")
ifelse(length(SHD1$X) == length(SHD_MDC_Hot$Month),1,0)
rm(SHD1,SHD2)

SHD_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/SHD_v3/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#SHD_data_all                                            # Print data to RStudio console

# Group by MDC

SHD_MDC_NotHot_Counts_InitialGrouping <- SHD_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
SHD_MDC_NotHot_Counts_InitialGrouping$Adjusted <- SHD_MDC_NotHot_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/SHD_v3/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
SHD_MDC_NotHot_Counts <- data.frame(cbind(SHD_MDC_NotHot_Counts_InitialGrouping$MDC_ACME,SHD_MDC_NotHot_Counts_InitialGrouping$Adjusted))

colnames(SHD_MDC_NotHot_Counts) <- c("Boot_MDC_ACME","Boot_Count")
SHD_MDC_NotHot_Counts$Boot_Count <- as.numeric(SHD_MDC_NotHot_Counts$Boot_Count)
sum(SHD_MDC_NotHot_Counts$Boot_Count)

#SHD_MDC_NotHot_Counts[SHD_MDC_NotHot_Counts$MDCAcme == "MDC22", 2] <- 0

SHD_MDC_Hot_Counts <- SHD_MDC_Hot %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# SHD_MDC_Hot_Counts[(length(SHD_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# SHD_MDC_Hot_Counts[(length(SHD_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# SHD_MDC_Hot_Counts[(length(SHD_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# SHD_MDC_Hot_Counts[(length(SHD_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# SHD_MDC_Hot_Counts[(length(SHD_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# SHD_MDC_Hot_Counts[(length(SHD_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# SHD_MDC_Hot_Counts[(length(SHD_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# SHD_MDC_Hot_Counts[(length(SHD_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)

rm(SHD_data_all,SHD_MDC_Hot_NAs,SHD_MDC_NotHot_NAs,SHD_MDC_NotHot_Slim)

#Final Prep for Comparison
SHD_Ordered_NotHot_Counts <- SHD_MDC_NotHot_Counts[order(SHD_MDC_NotHot_Counts$Boot_MDC_ACME), ]
SHD_Ordered_Hot_Counts <- SHD_MDC_Hot_Counts[order(SHD_MDC_Hot_Counts$MDC_ACME), ]

#SHD_Final_Hot <- SHD_Ordered_Hot_Counts[SHD_Ordered_Hot_Counts$count_by_siteyear > 5, ]
SHD_Ordered_NotHot_Expected_Counts <- SHD_Ordered_NotHot_Counts[SHD_Ordered_NotHot_Counts$Boot_Count > 5, ]

# ifelse(length(SHD_NotHot_Counts_Ordered$Boot_MDC_ACME) == length(SHD_Hot_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(SHD_NotHot_Counts_Ordered$Boot_Count) == sum(SHD_Hot_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackSHD <- cbind(SHD_NotHot_Counts_Ordered,SHD_Hot_Counts_Ordered)
CountsOnlySHD <- right_join(SHD_Ordered_Hot_Counts , SHD_Ordered_NotHot_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackSHD <- CountsOnlySHD[!is.na(CountsOnlySHD$Boot_Count) | !is.na(CountsOnlySHD$count_by_siteyear),]
rm(CountsOnlySHD)

sum(BackToBackSHD$count_by_siteyear)
sum(BackToBackSHD$Boot_Count)

colnames(BackToBackSHD) <- c("MDC_ACME","Heat","Boot")

BackToBackSHD$Boot <- as.numeric(BackToBackSHD$Boot)
sum(BackToBackSHD$Boot)
sum(BackToBackSHD$Heat)

BackToBackSHD_GreaterThanZero_Heat <- BackToBackSHD[BackToBackSHD$Boot > 0, ]

sum(BackToBackSHD_GreaterThanZero_Heat$Heat)
sum(BackToBackSHD_GreaterThanZero_Heat$Boot)

EO2_SHD <- (BackToBackSHD_GreaterThanZero_Heat$Heat - BackToBackSHD_GreaterThanZero_Heat$Boot)^2
E_SHD <- BackToBackSHD_GreaterThanZero_Heat$Boot
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

EO_Diff_SHD <- (BackToBackSHD_GreaterThanZero_Heat$Heat - BackToBackSHD_GreaterThanZero_Heat$Boot)
STD_RES_SHD <- EO_Diff_SHD / (sqrt(BackToBackSHD_GreaterThanZero_Heat$Boot))
BackToBackSHD_GreaterThanZero_Heat$Res <- round(STD_RES_SHD,digits = 3)
BackToBackSHD_GreaterThanZero_Heat$ResSig <- ifelse(abs(BackToBackSHD_GreaterThanZero_Heat$Res) > 3.8, TRUE, print("F"))

#VJI

VJI <- read_excel("~/Desktop/VJI.Final.Weather.xlsx",na="NA")
VJIselect <- VJI[1:5844, ]

VJIHotDaysSevere_lag0 <- VJIselect[VJIselect$HeatLag0Update == 1, ]
VJIHotDaysSevere_lag1 <- VJIselect[VJIselect$HeatLag1Update == 1, ]
VJIHotDaysSevere_LagCombine <- rbind(VJIHotDaysSevere_lag0,VJIHotDaysSevere_lag1)
VJIHotDaysSevere_LagCombine_DupsRem <- VJIHotDaysSevere_LagCombine[!duplicated(VJIHotDaysSevere_LagCombine), ]
VJIHotDaysUpdate <- VJIHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

VJIHotDaysUpdate_Summer <- VJIHotDaysUpdate[3 < month(VJIHotDaysUpdate$Date) & month(VJIHotDaysUpdate$Date) < 10, ]
VJIHotDaysUpdate_Summer$Date_Update <- format(as.Date(VJIHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(VJIHotDaysSevere_lag0,VJIHotDaysSevere_lag1,VJIHotDaysSevere_LagCombine,VJIHotDaysSevere_LagCombine_DupsRem,VJIHotDaysUpdate)

VJI_MDC <- MDC_FullFinal %>%
  filter(Wx == "VJI")

VJI_MDC_Hot_NAs <- left_join(VJIHotDaysUpdate_Summer, 
                             VJI_MDC,
                             by = c("Date_Update" = "Date"))
VJI_MDC_Hot <- VJI_MDC_Hot_NAs[!is.na(VJI_MDC_Hot_NAs$MDC_ACME),]
#NAOnly_Hot <- VJI_MDC_Hot[is.na(VJI_MDC_Hot$MDC_ACME), ] #0 obs. so yay!

# test <- ifelse(VJI_MDC_Hot$HeatLag0Update == 0 & VJI_MDC_Hot$HeatLag1Update == 0,1,0)
# > sum(test)
# [1] NA
# > sum(test,na.rm = T)
# [1] 0

length(VJI_MDC_Hot$Station) / length(VJI_MDC$Date) #  13.49% of deaths happen on Hot Days
length(VJIHotDaysUpdate_Summer$Station) / length(VJI$Station) # 13.92% of days are hot day lag0 or lag1

####### BootStrapping VJI

VJINotHotDaysSevere_lag0 <- VJIselect[!(VJIselect$HeatLag0Update == 1), ]
VJINotHotDaysSevere_lag1 <- VJIselect[!(VJIselect$HeatLag1Update == 1), ]
VJINotHotDaysSevere_LagCombine <- rbind(VJINotHotDaysSevere_lag0,VJINotHotDaysSevere_lag1)
VJINotHotDaysSevere_LagCombine_DupsRem <- VJINotHotDaysSevere_LagCombine[!duplicated(VJINotHotDaysSevere_LagCombine), ]
VJINotHotDaysUpdate <- VJINotHotDaysSevere_LagCombine_DupsRem %>% arrange(Date) #826 obs

VJINotHotDaysUpdate_Summer <- VJINotHotDaysUpdate[3 < month(VJINotHotDaysUpdate$Date) & month(VJINotHotDaysUpdate$Date) < 10, ]
VJINotHotDaysUpdate_Summer$Date_Update <- format(as.Date(VJINotHotDaysUpdate_Summer$Date), "%Y-%m-%d")

rm(VJINotHotDaysSevere_lag0,VJINotHotDaysSevere_lag1,VJINotHotDaysSevere_LagCombine,VJINotHotDaysSevere_LagCombine_DupsRem,VJINotHotDaysUpdate)

#test <- VJINotHotDaysUpdate_Summer[VJINotHotDaysUpdate_Summer$Month == 3, ]

VJI_MDC_NotHot_NAs <- left_join(VJINotHotDaysUpdate_Summer, 
                                VJI_MDC,
                                by = c("Date_Update" = "Date"))
VJI_MDC_NotHot <- VJI_MDC_NotHot_NAs[!is.na(VJI_MDC_NotHot_NAs$MDC_ACME),]
#NAOnly_NotHot <- VJI_MDC_NotHot[is.na(VJI_MDC_NotHot$MDC_ACME), ] #0 obs. so yay!


VJI_MDC_NotHot_Slim <- VJI_MDC_NotHot %>%
  select(Date_Update,MDC_ACME)
#NAOnly_NotHot_Slim <- VJI_MDC_NotHot_Slim[is.na(VJI_MDC_NotHot_Slim$MDC_ACME), ] #0 obs. so yay!

#Compute Tables 
# for (i in (1:1000)) {
#   print(i)
#   VJINoStressRandom <- VJI_MDC_NotHot_Slim %>%
#     sample_n(length(VJI_MDC_Hot$Month),replace = TRUE)
#   write.csv(VJINoStressRandom,paste0("~/Desktop/NoStressRandomWx/VJI_v3/VJI",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")   #Identify file names
# data_files

VJI1 <- read.csv("~/Desktop/NoStressRandomWx/VJI_v3/VJI1")
VJI2 <- read.csv("~/Desktop/NoStressRandomWx/VJI_v3/VJI2")
ifelse(length(VJI1$X) == length(VJI_MDC_Hot$Month),1,0)
rm(VJI1,VJI2)

VJI_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/VJI_v3/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
#VJI_data_all                                            # Print data to RStudio console

# Group by MDC

VJI_MDC_NotHot_Counts_InitialGrouping <- VJI_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
VJI_MDC_NotHot_Counts_InitialGrouping$Adjusted <- VJI_MDC_NotHot_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/VJI_v3/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
VJI_MDC_NotHot_Counts <- data.frame(cbind(VJI_MDC_NotHot_Counts_InitialGrouping$MDC_ACME,VJI_MDC_NotHot_Counts_InitialGrouping$Adjusted))

colnames(VJI_MDC_NotHot_Counts) <- c("Boot_MDC_ACME","Boot_Count")
VJI_MDC_NotHot_Counts$Boot_Count <- as.numeric(VJI_MDC_NotHot_Counts$Boot_Count)
sum(VJI_MDC_NotHot_Counts$Boot_Count)

#VJI_MDC_NotHot_Counts[VJI_MDC_NotHot_Counts$MDCAcme == "MDC22", 2] <- 0

VJI_MDC_Hot_Counts <- VJI_MDC_Hot %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
# VJI_MDC_Hot_Counts[(length(VJI_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
# VJI_MDC_Hot_Counts[(length(VJI_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# VJI_MDC_Hot_Counts[(length(VJI_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
# VJI_MDC_Hot_Counts[(length(VJI_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# VJI_MDC_Hot_Counts[(length(VJI_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
# VJI_MDC_Hot_Counts[(length(VJI_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
# VJI_MDC_Hot_Counts[(length(VJI_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
# VJI_MDC_Hot_Counts[(length(VJI_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)

rm(VJI_data_all,VJI_MDC_Hot_NAs,VJI_MDC_NotHot_NAs,VJI_MDC_NotHot_Slim)

#Final Prep for Comparison
VJI_Ordered_NotHot_Counts <- VJI_MDC_NotHot_Counts[order(VJI_MDC_NotHot_Counts$Boot_MDC_ACME), ]
VJI_Ordered_Hot_Counts <- VJI_MDC_Hot_Counts[order(VJI_MDC_Hot_Counts$MDC_ACME), ]

#VJI_Final_Hot <- VJI_Ordered_Hot_Counts[VJI_Ordered_Hot_Counts$count_by_siteyear > 5, ]
VJI_Ordered_NotHot_Expected_Counts <- VJI_Ordered_NotHot_Counts[VJI_Ordered_NotHot_Counts$Boot_Count > 5, ]

# ifelse(length(VJI_NotHot_Counts_Ordered$Boot_MDC_ACME) == length(VJI_Hot_Counts_Ordered$MDC_ACME),TRUE,FALSE)
# ifelse(sum(VJI_NotHot_Counts_Ordered$Boot_Count) == sum(VJI_Hot_Counts_Ordered$count_by_siteyear),TRUE,FALSE)


#BackToBackVJI <- cbind(VJI_NotHot_Counts_Ordered,VJI_Hot_Counts_Ordered)
CountsOnlyVJI <- right_join(VJI_Ordered_Hot_Counts , VJI_Ordered_NotHot_Expected_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
BackToBackVJI <- CountsOnlyVJI[!is.na(CountsOnlyVJI$Boot_Count) | !is.na(CountsOnlyVJI$count_by_siteyear),]
rm(CountsOnlyVJI)

sum(BackToBackVJI$count_by_siteyear)
sum(BackToBackVJI$Boot_Count)

colnames(BackToBackVJI) <- c("MDC_ACME","Heat","Boot")

BackToBackVJI$Boot <- as.numeric(BackToBackVJI$Boot)
sum(BackToBackVJI$Boot)
sum(BackToBackVJI$Heat)

BackToBackVJI_GreaterThanZero_Heat <- BackToBackVJI[BackToBackVJI$Boot > 0, ]

sum(BackToBackVJI_GreaterThanZero_Heat$Heat)
sum(BackToBackVJI_GreaterThanZero_Heat$Boot)

EO2_VJI <- (BackToBackVJI_GreaterThanZero_Heat$Heat - BackToBackVJI_GreaterThanZero_Heat$Boot)^2
E_VJI <- BackToBackVJI_GreaterThanZero_Heat$Boot
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

EO_Diff_VJI <- (BackToBackVJI_GreaterThanZero_Heat$Heat - BackToBackVJI_GreaterThanZero_Heat$Boot)
STD_RES_VJI <- EO_Diff_VJI / (sqrt(BackToBackVJI_GreaterThanZero_Heat$Boot))
BackToBackVJI_GreaterThanZero_Heat$Res <- round(STD_RES_VJI,digits = 3)
BackToBackVJI_GreaterThanZero_Heat$ResSig <- ifelse(abs(BackToBackVJI_GreaterThanZero_Heat$Res) > 3.8, TRUE, print("F"))

#Change this 1.96 to 0.05/total comparisons (z-table)
                                               