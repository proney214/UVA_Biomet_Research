library(dplyr)
library(lubridate)
library(readxl)

MDC0514 <- read.csv("MDC_Station_Matching_0514.csv")
MDC1520 <- read.csv("MDC_Station_Matching_1520.csv")
MDC20 <- read.csv("MDC2020_GoodToGo.csv")

MDC0514_Work <- data.frame(cbind(MDC0514$Date,MDC0514$MDCAcme,MDC0514$ID))
MDC1520_Work <- data.frame(cbind(MDC1520$Date,MDC1520$MDCAcme,MDC1520$ID))

MDC_Work <- rbind(MDC0514_Work,MDC1520_Work)
colnames(MDC_Work) <- c("Date","MDC_ACME","Wx")

MDC_Bad2020Removed <- MDC_Work[year(MDC_Work$Date) < 2020, ]

MDC20_work <- data.frame(cbind(MDC20$Date,MDC20$MDC0ACME,MDC20$ID))
colnames(MDC20_work) <- c("Date","MDC_ACME","Wx")

MDC_FullFinal <- rbind(MDC_Bad2020Removed,MDC20_work)
rm(MDC_Bad2020Removed,MDC0514_Work,MDC1520_Work,MDC20_work,MDC_Work)

write.csv(MDC_FullFinal, "Personal_MDC_ChiSqMortData.csv")
#Generate Data 

#First run with observed being just high temperatures

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

CHO_MDC_Hot <- left_join(CHOHotDaysUpdate_Summer, 
                         CHO_MDC,
                         by = c("Date_Update" = "Date"))

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

CHO_MDC_NotHot <- left_join(CHONotHotDaysUpdate_Summer, 
                         CHO_MDC,
                         by = c("Date_Update" = "Date"))
CHO_MDC_NotHot_Slim <- CHO_MDC_NotHot %>%
  select(Date_Update,MDC_ACME)

#Compute Tables
for (i in (1:1000)) {
  print(i)
  CHONoStressRandom <- CHO_MDC_NotHot_Slim %>%
    sample_n(length(CHO_MDC_Hot$Month))
  write.csv(CHONoStressRandom,paste0("~/Desktop/NoStressRandomWx/CHO_v3/CHO",i))
}

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
CHO_data_all                                            # Print data to RStudio console


# Group by MDC

CHO_MDC_NotHot_Counts_InitialGrouping <- CHO_data_all %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
CHO_MDC_NotHot_Counts_InitialGrouping$Adjusted <- CHO_MDC_NotHot_Counts_InitialGrouping$count_by_siteyear / (length(list.files(path = "~/Desktop/NoStressRandomWx/CHO_v3/",  # Identify all CSV files
                                                                                                                               pattern = "*", full.names = TRUE))) # Divide by number i
CHO_MDC_NotHot_Counts <- data.frame(cbind(CHO_MDC_NotHot_Counts_InitialGrouping$MDC_ACME,CHO_MDC_NotHot_Counts_InitialGrouping$Adjusted))

colnames(CHO_MDC_NotHot_Counts) <- c("Boot_MDC_ACME","Boot_Count")
#CHO_MDC_NotHot_Counts[CHO_MDC_NotHot_Counts$MDCAcme == "MDC22", 2] <- 0

CHO_MDC_Hot_Counts <- CHO_MDC_Hot %>%
  group_by(MDC_ACME) %>%
  summarize(count_by_siteyear =  n()) 
CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 22")
CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 24")
CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 02")
CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)
CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)+1), 1] <- c("MDC 21")
CHO_MDC_Hot_Counts[(length(CHO_MDC_Hot_Counts$MDC_ACME)), 2] <- c(0)

#Final Prep for Comparison
CHO_NotHot_Counts_Ordered <- CHO_MDC_NotHot_Counts[order(CHO_MDC_NotHot_Counts$Boot_MDC_ACME), ]
CHO_Hot_Counts_Ordered <- CHO_MDC_Hot_Counts[order(CHO_MDC_Hot_Counts$MDC_ACME), ]

# View(CHO_NotHot_Counts_Ordered)
# View(CHO_Hot_Counts_Ordered)

#BackToBackCHO <- cbind(CHO_NotHot_Counts_Ordered,CHO_Hot_Counts_Ordered)
BackToBackCHO <- right_join(CHO_MDC_Hot_Counts, CHO_MDC_NotHot_Counts, by = c("MDC_ACME" = "Boot_MDC_ACME"))
CountsOnlyCHO <- BackToBackCHO[1:20, ]
colnames(CountsOnlyCHO) <- c("MDC_ACME","Heat","Boot")

CHOCountsOnlyCH$Boot <- as.integer(CountsOnlyCHO$Boot)
sum(CountsOnlyCHO$Boot)
sum(CountsOnlyCHO$Heat)

chisq.test(CountsOnlyCHO$Heat,CountsOnlyCHO$Boot)
