# Davis FRESH START Research Idea Extend

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
CHOProfileMortright <- CHO[ ,161:162]
CHOProfileMort <- cbind(CHOProfileMortleft,CHOProfileMortright)
CHOProfileMort$Date <- CHO$Date 
rm(CHOProfileMortleft,CHOProfileMortright)

CHOProfileMDC <- CHOProfileMort[ ,1:27]
CHOProfileHotCold <- CHO[ , 161:162]
CHO$Date <- as.Date(CHO$Date)
#CHOselect <- cbind(CHOProfileHotCold,CHOProfileMort)
CHOselect <- CHOProfileMort
rm(CHOProfileHotCold,CHOProfileMort)

#Heat General Profile

CHOHotDaysSevere_lag0 <- CHOselect[CHOselect$HeatLag0Update == 1, ]
CHOHotDaysSevere_lag1 <- CHOselect[CHOselect$HeatLag1Update == 1, ]
CHOHotDaysSevere_LagCombine <- rbind(CHOHotDaysSevere_lag0,CHOHotDaysSevere_lag1)
CHOHotDaysSevere_LagCombine_DupsRem <- CHOHotDaysSevere_LagCombine[!duplicated(CHOHotDaysSevere_LagCombine), ]
CHOHotDaysUpdate <- CHOHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

rm(CHOHotDaysSevere_lag0,CHOHotDaysSevere_lag1,CHOHotDaysSevere_LagCombine,CHOHotDaysSevere_LagCombine_DupsRem)


CHOHotDaysSevere <- CHOHotDaysUpdate[,c(1:27,44:46)]

CHOMDCCountsHeat <- data.frame(colSums(CHOHotDaysSevere[,1:27],na.rm=TRUE))
colnames(CHOMDCCountsHeat) <- c("HeatCount")

#No stress General Profile

CHONoStressFull <- CHOselect %>%
  filter(HeatLag0Update != 1 & HeatLag1Update != 1) %>%
  filter(month(Date) %in% c(4,5,6, 7, 8, 9))

CHONoStress <- CHONoStressFull[,c(1:27,44:46)]
rm(CHONoStressFull)

####### BootStrapping

#Compute Tables
# for (i in (1:1000)) {
#   print(i)
#   CHONoStressRandom <- CHONoStress %>%
#     sample_n(length(CHOHotDaysUpdate$MDC.02))
#   write.csv(CHONoStressRandom,paste0("~/Desktop/NoStressRandomWx/CHO/CHO",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")  # Identify file names
# data_files    
# 
# for(i in 1:5) {                              # Head of for-loop
#   assign(paste0("CHO", i),                                   # Read and store data frames
#          read.csv(paste0("~/Desktop/NoStressRandomWx/",
#                           CHO[i])))
# }

CHO1 <- read.csv("~/Desktop/NoStressRandomWx/CHO/CHO1")
CHO2 <- read.csv("~/Desktop/NoStressRandomWx/CHO/CHO2")

CHO_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/CHO/",  # Identify all CSV files
                       pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
CHO_data_all                                            # Print data to RStudio console

CHO_data_all_MDC <- CHO_data_all[,2:28]

CHOMDCCountsBoot <- data.frame(colSums(CHO_data_all_MDC)/1000)
colnames(CHOMDCCountsBoot) <- c("BootCount")

BacktoBackCHO <- cbind(CHOMDCCountsHeat,CHOMDCCountsBoot)

CHONvH <- chisq.test(CHOMDCCountsBoot$BootCount, CHOMDCCountsHeat$HeatCount, correct=FALSE)
CHONvH$p.value

ChiResultsTableNoHotStress <- data.frame(matrix(nrow=1,ncol=1))

ChiResultsTableNoHotStress$CHO <- CHONvH$p.value
rm(CHO,CHO_data_all,CHO_data_all_MDC,CHO1,CHO2,HotDaysSevere,CHOMDCCountsBoot,CHOMDCCountsHeat,CHONoStressRandom,CHOProfileMDC,CHOselect)


# EMV

# Davis FRESH START Research Idea Extend

library(readxl)
library(dplyr)
library(lubridate)
#library(boot)

EMV <- read_excel("~/Desktop/EMV.Final.Weather.xlsx",na="NA")
EMV <- EMV[1:5844, ]

# Generate One Day Lag in excel


###### Investigation

EMV %>% mutate_if(is.character, as.numeric)
EMV$HotDaysExtreme <- as.numeric(EMV$HotDaysExtreme)

EMVProfileMortleft <- EMV[ ,107:149]
EMVProfileMortright <- EMV[ ,161:162]
EMVProfileMort <- cbind(EMVProfileMortleft,EMVProfileMortright)
EMVProfileMort$Date <- EMV$Date 
rm(EMVProfileMortleft,EMVProfileMortright)

EMVProfileMDC <- EMVProfileMort[ ,1:27]
EMVProfileHotCold <- EMV[ , 161:162]
EMV$Date <- as.Date(EMV$Date)
#EMVselect <- cbind(EMVProfileHotCold,EMVProfileMort)
EMVselect <- EMVProfileMort
rm(EMVProfileHotCold,EMVProfileMort)

#Heat General Profile

EMVHotDaysSevere_lag0 <- EMVselect[EMVselect$HeatLag0Update == 1, ]
EMVHotDaysSevere_lag1 <- EMVselect[EMVselect$HeatLag1Update == 1, ]
EMVHotDaysSevere_LagCombine <- rbind(EMVHotDaysSevere_lag0,EMVHotDaysSevere_lag1)
EMVHotDaysSevere_LagCombine_DupsRem <- EMVHotDaysSevere_LagCombine[!duplicated(EMVHotDaysSevere_LagCombine), ]
EMVHotDaysUpdate <- EMVHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

rm(EMVHotDaysSevere_lag0,EMVHotDaysSevere_lag1,EMVHotDaysSevere_LagCombine,EMVHotDaysSevere_LagCombine_DupsRem)


EMVHotDaysSevere <- EMVHotDaysUpdate[,c(1:27,44:46)]

EMVMDCCountsHeat <- data.frame(colSums(EMVHotDaysSevere[,1:27],na.rm=TRUE))
colnames(EMVMDCCountsHeat) <- c("HeatCount")

#No stress General Profile

EMVNoStressFull <- EMVselect %>%
  filter(HeatLag0Update != 1 & HeatLag1Update != 1) %>%
  filter(month(Date) %in% c(4,5,6, 7, 8, 9))

EMVNoStress <- EMVNoStressFull[,c(1:27,44:46)]
rm(EMVNoStressFull)

####### BootStrapping

#Compute Tables
# for (i in (1:1000)) {
#   print(i)
#   EMVNoStressRandom <- EMVNoStress %>%
#     sample_n(length(EMVHotDaysUpdate$MDC.02),replace = TRUE)
#   write.csv(EMVNoStressRandom,paste0("~/Desktop/NoStressRandomWx/EMV/EMV",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")  # Identify file names
# data_files    
# 
# for(i in 1:5) {                              # Head of for-loop
#   assign(paste0("EMV", i),                                   # Read and store data frames
#          read.csv(paste0("~/Desktop/NoStressRandomWx/",
#                           EMV[i])))
# }

EMV1 <- read.csv("~/Desktop/NoStressRandomWx/EMV/EMV1")
EMV2 <- read.csv("~/Desktop/NoStressRandomWx/EMV/EMV2")

EMV_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/EMV/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
EMV_data_all                                            # Print data to RStudio console

EMV_data_all_MDC <- EMV_data_all[,2:28]

EMVMDCCountsBoot <- data.frame(colSums(EMV_data_all_MDC)/1000)
colnames(EMVMDCCountsBoot) <- c("BootCount")

BacktoBackEMV <- cbind(EMVMDCCountsHeat,EMVMDCCountsBoot)

EMVNvH <- chisq.test(EMVMDCCountsBoot$BootCount, EMVMDCCountsHeat$HeatCount, correct=FALSE)
EMVNvH$p.value

# ChiResultsTableNoHotStress <- data.frame(matrix(nrow=1,ncol=1))

ChiResultsTableNoHotStress$EMV <- EMVNvH$p.value
rm(EMV,EMV_data_all,EMV_data_all_MDC,EMV1,EMV2,HotDaysSevere,EMVMDCCountsBoot,EMVMDCCountsHeat,EMVNoStressRandom,EMVProfileMDC,EMVselect)


# EZF

EZF <- read_excel("~/Desktop/EZF.Final.Weather.xlsx",na="NA")
EZF <- EZF[1:5844, ]

# Generate One Day Lag in excel


###### Investigation

EZF %>% mutate_if(is.character, as.numeric)
EZF$HotDaysExtreme <- as.numeric(EZF$HotDaysExtreme)

EZFProfileMortleft <- EZF[ ,107:149]
EZFProfileMortright <- EZF[ ,161:162]
EZFProfileMort <- cbind(EZFProfileMortleft,EZFProfileMortright)
EZFProfileMort$Date <- EZF$Date 
rm(EZFProfileMortleft,EZFProfileMortright)

EZFProfileMDC <- EZFProfileMort[ ,1:27]
EZFProfileHotCold <- EZF[ , 161:162]
EZF$Date <- as.Date(EZF$Date)
#EZFselect <- cbind(EZFProfileHotCold,EZFProfileMort)
EZFselect <- EZFProfileMort
rm(EZFProfileHotCold,EZFProfileMort)

#Heat General Profile

EZFHotDaysSevere_lag0 <- EZFselect[EZFselect$HeatLag0Update == 1, ]
EZFHotDaysSevere_lag1 <- EZFselect[EZFselect$HeatLag1Update == 1, ]
EZFHotDaysSevere_LagCombine <- rbind(EZFHotDaysSevere_lag0,EZFHotDaysSevere_lag1)
EZFHotDaysSevere_LagCombine_DupsRem <- EZFHotDaysSevere_LagCombine[!duplicated(EZFHotDaysSevere_LagCombine), ]
EZFHotDaysUpdate <- EZFHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

rm(EZFHotDaysSevere_lag0,EZFHotDaysSevere_lag1,EZFHotDaysSevere_LagCombine,EZFHotDaysSevere_LagCombine_DupsRem)


EZFHotDaysSevere <- EZFHotDaysUpdate[,c(1:27,44:46)]

EZFMDCCountsHeat <- data.frame(colSums(EZFHotDaysSevere[,1:27],na.rm=TRUE))
colnames(EZFMDCCountsHeat) <- c("HeatCount")

#No stress General Profile

EZFNoStressFull <- EZFselect %>%
  filter(HeatLag0Update != 1 & HeatLag1Update != 1) %>%
  filter(month(Date) %in% c(4,5,6, 7, 8, 9))

EZFNoStress <- EZFNoStressFull[,c(1:27,44:46)]
#rm(EZFNoStressFull)

####### BootStrapping

#Compute Tables
# for (i in (1:1000)) {
#   print(i)
#   EZFNoStressRandom <- EZFNoStress %>%
#     sample_n(length(EZFHotDaysUpdate$MDC.02),replace = TRUE)
#   write.csv(EZFNoStressRandom,paste0("~/Desktop/NoStressRandomWx/EZF/EZF",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")  # Identify file names
# data_files    
# 
# for(i in 1:5) {                              # Head of for-loop
#   assign(paste0("EZF", i),                                   # Read and store data frames
#          read.csv(paste0("~/Desktop/NoStressRandomWx/",
#                           EZF[i])))
# }

EZF1 <- read.csv("~/Desktop/NoStressRandomWx/EZF/EZF1")
EZF2 <- read.csv("~/Desktop/NoStressRandomWx/EZF/EZF2")

EZF_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/EZF/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
EZF_data_all                                            # Print data to RStudio console

EZF_data_all_MDC <- EZF_data_all[,2:28]

EZFMDCCountsBoot <- data.frame(colSums(EZF_data_all_MDC)/1000)
colnames(EZFMDCCountsBoot) <- c("BootCount")

BacktoBackEZF <- cbind(EZFMDCCountsHeat,EZFMDCCountsBoot)

EZFNvH <- chisq.test(EZFMDCCountsBoot$BootCount, EZFMDCCountsHeat$HeatCount, correct=FALSE)
EZFNvH$p.value

# ChiResultsTableNoHotStress <- data.frame(matrix(nrow=1,ncol=1))

ChiResultsTableNoHotStress$EZF <- EZFNvH$p.value
rm(EZF,EZF_data_all,EZF_data_all_MDC,EZF1,EZF2,HotDaysSevere,EZFMDCCountsBoot,EZFMDCCountsHeat,EZFNoStressRandom,EZFProfileMDC,EZFselect)


# IAD

IAD <- read_excel("~/Desktop/IAD.Final.Weather.xlsx",na="NA")
IAD <- IAD[1:5844, ]

# Generate One Day Lag in excel


###### Investigation

IAD %>% mutate_if(is.character, as.numeric)
IAD$HotDaysExtreme <- as.numeric(IAD$HotDaysExtreme)

IADProfileMortleft <- IAD[ ,107:149]
IADProfileMortright <- IAD[ ,161:162]
IADProfileMort <- cbind(IADProfileMortleft,IADProfileMortright)
IADProfileMort$Date <- IAD$Date 
rm(IADProfileMortleft,IADProfileMortright)

IADProfileMDC <- IADProfileMort[ ,1:27]
IADProfileHotCold <- IAD[ , 161:162]
IAD$Date <- as.Date(IAD$Date)
#IADselect <- cbind(IADProfileHotCold,IADProfileMort)
IADselect <- IADProfileMort
rm(IADProfileHotCold,IADProfileMort)

#Heat General Profile

IADHotDaysSevere_lag0 <- IADselect[IADselect$HeatLag0Update == 1, ]
IADHotDaysSevere_lag1 <- IADselect[IADselect$HeatLag1Update == 1, ]
IADHotDaysSevere_LagCombine <- rbind(IADHotDaysSevere_lag0,IADHotDaysSevere_lag1)
IADHotDaysSevere_LagCombine_DupsRem <- IADHotDaysSevere_LagCombine[!duplicated(IADHotDaysSevere_LagCombine), ]
IADHotDaysUpdate <- IADHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

rm(IADHotDaysSevere_lag0,IADHotDaysSevere_lag1,IADHotDaysSevere_LagCombine,IADHotDaysSevere_LagCombine_DupsRem)


IADHotDaysSevere <- IADHotDaysUpdate[,c(1:27,44:46)]

IADMDCCountsHeat <- data.frame(colSums(IADHotDaysSevere[,1:27],na.rm=TRUE))
colnames(IADMDCCountsHeat) <- c("HeatCount")

#No stress General Profile

IADNoStressFull <- IADselect %>%
  filter(HeatLag0Update != 1 & HeatLag1Update != 1) %>%
  filter(month(Date) %in% c(4,5,6, 7, 8, 9))

IADNoStress <- IADNoStressFull[,c(1:27,44:46)]
#rm(IADNoStressFull)

####### BootStrapping

#Compute Tables
# for (i in (1:1000)) {
#   print(i)
#   IADNoStressRandom <- IADNoStress %>%
#     sample_n(length(IADHotDaysUpdate$MDC.02),replace = TRUE)
#   write.csv(IADNoStressRandom,paste0("~/Desktop/NoStressRandomWx/IAD/IAD",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")  # Identify file names
# data_files    
# 
# for(i in 1:5) {                              # Head of for-loop
#   assign(paste0("IAD", i),                                   # Read and store data frames
#          read.csv(paste0("~/Desktop/NoStressRandomWx/",
#                           IAD[i])))
# }

IAD1 <- read.csv("~/Desktop/NoStressRandomWx/IAD/IAD1")
IAD2 <- read.csv("~/Desktop/NoStressRandomWx/IAD/IAD2")

IAD_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/IAD/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
IAD_data_all                                            # Print data to RStudio console

IAD_data_all_MDC <- IAD_data_all[,2:28]

IADMDCCountsBoot <- data.frame(colSums(IAD_data_all_MDC)/1000)
colnames(IADMDCCountsBoot) <- c("BootCount")

BacktoBackIAD <- cbind(IADMDCCountsHeat,IADMDCCountsBoot)

IADNvH <- chisq.test(IADMDCCountsBoot$BootCount, IADMDCCountsHeat$HeatCount, correct=FALSE)
IADNvH$p.value

# ChiResultsTableNoHotStress <- data.frame(matrix(nrow=1,ncol=1))

ChiResultsTableNoHotStress$IAD <- IADNvH$p.value
rm(IAD,IAD_data_all,IAD_data_all_MDC,IAD1,IAD2,HotDaysSevere,IADMDCCountsBoot,IADMDCCountsHeat,IADNoStressRandom,IADProfileMDC,IADselect)

# LYH

LYH <- read_excel("~/Desktop/LYH.Final.Weather.xlsx",na="NA")
LYH <- LYH[1:5844, ]

# Generate One Day Lag in excel


###### Investigation

LYH %>% mutate_if(is.character, as.numeric)
LYH$HotDaysExtreme <- as.numeric(LYH$HotDaysExtreme)

LYHProfileMortleft <- LYH[ ,107:149]
LYHProfileMortright <- LYH[ ,161:162]
LYHProfileMort <- cbind(LYHProfileMortleft,LYHProfileMortright)
LYHProfileMort$Date <- LYH$Date 
rm(LYHProfileMortleft,LYHProfileMortright)

LYHProfileMDC <- LYHProfileMort[ ,1:27]
LYHProfileHotCold <- LYH[ , 161:162]
LYH$Date <- as.Date(LYH$Date)
#LYHselect <- cbind(LYHProfileHotCold,LYHProfileMort)
LYHselect <- LYHProfileMort
rm(LYHProfileHotCold,LYHProfileMort)

#Heat General Profile

LYHHotDaysSevere_lag0 <- LYHselect[LYHselect$HeatLag0Update == 1, ]
LYHHotDaysSevere_lag1 <- LYHselect[LYHselect$HeatLag1Update == 1, ]
LYHHotDaysSevere_LagCombine <- rbind(LYHHotDaysSevere_lag0,LYHHotDaysSevere_lag1)
LYHHotDaysSevere_LagCombine_DupsRem <- LYHHotDaysSevere_LagCombine[!duplicated(LYHHotDaysSevere_LagCombine), ]
LYHHotDaysUpdate <- LYHHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

rm(LYHHotDaysSevere_lag0,LYHHotDaysSevere_lag1,LYHHotDaysSevere_LagCombine,LYHHotDaysSevere_LagCombine_DupsRem)


LYHHotDaysSevere <- LYHHotDaysUpdate[,c(1:27,44:46)]

LYHMDCCountsHeat <- data.frame(colSums(LYHHotDaysSevere[,1:27],na.rm=TRUE))
colnames(LYHMDCCountsHeat) <- c("HeatCount")

#No stress General Profile

LYHNoStressFull <- LYHselect %>%
  filter(HeatLag0Update != 1 & HeatLag1Update != 1) %>%
  filter(month(Date) %in% c(4,5,6, 7, 8, 9))

LYHNoStress <- LYHNoStressFull[,c(1:27,44:46)]
#rm(LYHNoStressFull)

####### BootStrapping

#Compute Tables
# for (i in (1:1000)) {
#   print(i)
#   LYHNoStressRandom <- LYHNoStress %>%
#     sample_n(length(LYHHotDaysUpdate$MDC.02),replace = TRUE)
#   write.csv(LYHNoStressRandom,paste0("~/Desktop/NoStressRandomWx/LYH/LYH",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")  # Identify file names
# data_files    
# 
# for(i in 1:5) {                              # Head of for-loop
#   assign(paste0("LYH", i),                                   # Read and store data frames
#          read.csv(paste0("~/Desktop/NoStressRandomWx/",
#                           LYH[i])))
# }

LYH1 <- read.csv("~/Desktop/NoStressRandomWx/LYH/LYH1")
LYH2 <- read.csv("~/Desktop/NoStressRandomWx/LYH/LYH2")

LYH_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/LYH/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
LYH_data_all                                            # Print data to RStudio console

LYH_data_all_MDC <- LYH_data_all[,2:28]

LYHMDCCountsBoot <- data.frame(colSums(LYH_data_all_MDC)/1000)
colnames(LYHMDCCountsBoot) <- c("BootCount")

BacktoBackLYH <- cbind(LYHMDCCountsHeat,LYHMDCCountsBoot)

LYHNvH <- chisq.test(LYHMDCCountsBoot$BootCount, LYHMDCCountsHeat$HeatCount, correct=FALSE)
LYHNvH$p.value

# ChiResultsTableNoHotStress <- data.frame(matrix(nrow=1,ncol=1))

ChiResultsTableNoHotStress$LYH <- LYHNvH$p.value
rm(LYH,LYH_data_all,LYH_data_all_MDC,LYH1,LYH2,HotDaysSevere,LYHMDCCountsBoot,LYHMDCCountsHeat,LYHNoStressRandom,LYHProfileMDC,LYHselect)

# OKV

OKV <- read_excel("~/Desktop/OKV.Final.Weather.xlsx",na="NA")
OKV <- OKV[1:5844, ]

# Generate One Day Lag in excel


###### Investigation

OKV %>% mutate_if(is.character, as.numeric)
OKV$HotDaysExtreme <- as.numeric(OKV$HotDaysExtreme)

OKVProfileMortleft <- OKV[ ,107:149]
OKVProfileMortright <- OKV[ ,161:162]
OKVProfileMort <- cbind(OKVProfileMortleft,OKVProfileMortright)
OKVProfileMort$Date <- OKV$Date 
rm(OKVProfileMortleft,OKVProfileMortright)

OKVProfileMDC <- OKVProfileMort[ ,1:27]
OKVProfileHotCold <- OKV[ , 161:162]
OKV$Date <- as.Date(OKV$Date)
#OKVselect <- cbind(OKVProfileHotCold,OKVProfileMort)
OKVselect <- OKVProfileMort
rm(OKVProfileHotCold,OKVProfileMort)

#Heat General Profile

OKVHotDaysSevere_lag0 <- OKVselect[OKVselect$HeatLag0Update == 1, ]
OKVHotDaysSevere_lag1 <- OKVselect[OKVselect$HeatLag1Update == 1, ]
OKVHotDaysSevere_LagCombine <- rbind(OKVHotDaysSevere_lag0,OKVHotDaysSevere_lag1)
OKVHotDaysSevere_LagCombine_DupsRem <- OKVHotDaysSevere_LagCombine[!duplicated(OKVHotDaysSevere_LagCombine), ]
OKVHotDaysUpdate <- OKVHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

rm(OKVHotDaysSevere_lag0,OKVHotDaysSevere_lag1,OKVHotDaysSevere_LagCombine,OKVHotDaysSevere_LagCombine_DupsRem)


OKVHotDaysSevere <- OKVHotDaysUpdate[,c(1:27,44:46)]

OKVMDCCountsHeat <- data.frame(colSums(OKVHotDaysSevere[,1:27],na.rm=TRUE))
colnames(OKVMDCCountsHeat) <- c("HeatCount")

#No stress General Profile

OKVNoStressFull <- OKVselect %>%
  filter(HeatLag0Update != 1 & HeatLag1Update != 1) %>%
  filter(month(Date) %in% c(4,5,6, 7, 8, 9))

OKVNoStress <- OKVNoStressFull[,c(1:27,44:46)]
#rm(OKVNoStressFull)

####### BootStrapping

#Compute Tables
# for (i in (1:1000)) {
#   print(i)
#   OKVNoStressRandom <- OKVNoStress %>%
#     sample_n(length(OKVHotDaysUpdate$MDC.02),replace = TRUE)
#   write.csv(OKVNoStressRandom,paste0("~/Desktop/NoStressRandomWx/OKV/OKV",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")  # Identify file names
# data_files    
# 
# for(i in 1:5) {                              # Head of for-loop
#   assign(paste0("OKV", i),                                   # Read and store data frames
#          read.csv(paste0("~/Desktop/NoStressRandomWx/",
#                           OKV[i])))
# }

OKV1 <- read.csv("~/Desktop/NoStressRandomWx/OKV/OKV1")
OKV2 <- read.csv("~/Desktop/NoStressRandomWx/OKV/OKV2")

OKV_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/OKV/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
OKV_data_all                                            # Print data to RStudio console

OKV_data_all_MDC <- OKV_data_all[,2:28]

OKVMDCCountsBoot <- data.frame(colSums(OKV_data_all_MDC)/1000)
colnames(OKVMDCCountsBoot) <- c("BootCount")

BacktoBackOKV <- cbind(OKVMDCCountsHeat,OKVMDCCountsBoot)

OKVNvH <- chisq.test(OKVMDCCountsBoot$BootCount, OKVMDCCountsHeat$HeatCount, correct=FALSE)
OKVNvH$p.value

# ChiResultsTableNoHotStress <- data.frame(matrix(nrow=1,ncol=1))

ChiResultsTableNoHotStress$OKV <- OKVNvH$p.value
rm(OKV,OKV_data_all,OKV_data_all_MDC,OKV1,OKV2,HotDaysSevere,OKVMDCCountsBoot,OKVMDCCountsHeat,OKVNoStressRandom,OKVProfileMDC,OKVselect)

# ORF

ORF <- read_excel("~/Desktop/ORF.Final.Weather.xlsx",na="NA")
ORF <- ORF[1:5844, ]

# Generate One Day Lag in excel


###### Investigation

ORF %>% mutate_if(is.character, as.numeric)
ORF$HotDaysExtreme <- as.numeric(ORF$HotDaysExtreme)

ORFProfileMortleft <- ORF[ ,107:149]
ORFProfileMortright <- ORF[ ,161:162]
ORFProfileMort <- cbind(ORFProfileMortleft,ORFProfileMortright)
ORFProfileMort$Date <- ORF$Date 
rm(ORFProfileMortleft,ORFProfileMortright)

ORFProfileMDC <- ORFProfileMort[ ,1:27]
ORFProfileHotCold <- ORF[ , 161:162]
ORF$Date <- as.Date(ORF$Date)
#ORFselect <- cbind(ORFProfileHotCold,ORFProfileMort)
ORFselect <- ORFProfileMort
rm(ORFProfileHotCold,ORFProfileMort)

#Heat General Profile

ORFHotDaysSevere_lag0 <- ORFselect[ORFselect$HeatLag0Update == 1, ]
ORFHotDaysSevere_lag1 <- ORFselect[ORFselect$HeatLag1Update == 1, ]
ORFHotDaysSevere_LagCombine <- rbind(ORFHotDaysSevere_lag0,ORFHotDaysSevere_lag1)
ORFHotDaysSevere_LagCombine_DupsRem <- ORFHotDaysSevere_LagCombine[!duplicated(ORFHotDaysSevere_LagCombine), ]
ORFHotDaysUpdate <- ORFHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

rm(ORFHotDaysSevere_lag0,ORFHotDaysSevere_lag1,ORFHotDaysSevere_LagCombine,ORFHotDaysSevere_LagCombine_DupsRem)


ORFHotDaysSevere <- ORFHotDaysUpdate[,c(1:27,44:46)]

ORFMDCCountsHeat <- data.frame(colSums(ORFHotDaysSevere[,1:27],na.rm=TRUE))
colnames(ORFMDCCountsHeat) <- c("HeatCount")

#No stress General Profile

ORFNoStressFull <- ORFselect %>%
  filter(HeatLag0Update != 1 & HeatLag1Update != 1) %>%
  filter(month(Date) %in% c(4,5,6, 7, 8, 9))

ORFNoStress <- ORFNoStressFull[,c(1:27,44:46)]
#rm(ORFNoStressFull)

####### BootStrapping

#Compute Tables
# for (i in (1:1000)) {
#   print(i)
#   ORFNoStressRandom <- ORFNoStress %>%
#     sample_n(length(ORFHotDaysUpdate$MDC.02),replace = TRUE)
#   write.csv(ORFNoStressRandom,paste0("~/Desktop/NoStressRandomWx/ORF/ORF",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")  # Identify file names
# data_files    
# 
# for(i in 1:5) {                              # Head of for-loop
#   assign(paste0("ORF", i),                                   # Read and store data frames
#          read.csv(paste0("~/Desktop/NoStressRandomWx/",
#                           ORF[i])))
# }

ORF1 <- read.csv("~/Desktop/NoStressRandomWx/ORF/ORF1")
ORF2 <- read.csv("~/Desktop/NoStressRandomWx/ORF/ORF2")

ORF_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/ORF/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
ORF_data_all                                            # Print data to RStudio console

ORF_data_all_MDC <- ORF_data_all[,2:28]

ORFMDCCountsBoot <- data.frame(colSums(ORF_data_all_MDC)/1000)
colnames(ORFMDCCountsBoot) <- c("BootCount")

BacktoBackORF <- cbind(ORFMDCCountsHeat,ORFMDCCountsBoot)

ORFNvH <- chisq.test(ORFMDCCountsBoot$BootCount, ORFMDCCountsHeat$HeatCount, correct=FALSE)
ORFNvH$p.value

# ChiResultsTableNoHotStress <- data.frame(matrix(nrow=1,ncol=1))

ChiResultsTableNoHotStress$ORF <- ORFNvH$p.value
rm(ORF,ORF_data_all,ORF_data_all_MDC,ORF1,ORF2,HotDaysSevere,ORFMDCCountsBoot,ORFMDCCountsHeat,ORFNoStressRandom,ORFProfileMDC,ORFselect)

# PHF

PHF <- read_excel("~/Desktop/PHF.Final.Weather.xlsx",na="NA")
PHF <- PHF[1:5844, ]

# Generate One Day Lag in excel


###### Investigation

PHF %>% mutate_if(is.character, as.numeric)
PHF$HotDaysExtreme <- as.numeric(PHF$HotDaysExtreme)

PHFProfileMortleft <- PHF[ ,107:149]
PHFProfileMortright <- PHF[ ,161:162]
PHFProfileMort <- cbind(PHFProfileMortleft,PHFProfileMortright)
PHFProfileMort$Date <- PHF$Date 
rm(PHFProfileMortleft,PHFProfileMortright)

PHFProfileMDC <- PHFProfileMort[ ,1:27]
PHFProfileHotCold <- PHF[ , 161:162]
PHF$Date <- as.Date(PHF$Date)
#PHFselect <- cbind(PHFProfileHotCold,PHFProfileMort)
PHFselect <- PHFProfileMort
rm(PHFProfileHotCold,PHFProfileMort)

#Heat General Profile

PHFHotDaysSevere_lag0 <- PHFselect[PHFselect$HeatLag0Update == 1, ]
PHFHotDaysSevere_lag1 <- PHFselect[PHFselect$HeatLag1Update == 1, ]
PHFHotDaysSevere_LagCombine <- rbind(PHFHotDaysSevere_lag0,PHFHotDaysSevere_lag1)
PHFHotDaysSevere_LagCombine_DupsRem <- PHFHotDaysSevere_LagCombine[!duplicated(PHFHotDaysSevere_LagCombine), ]
PHFHotDaysUpdate <- PHFHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

rm(PHFHotDaysSevere_lag0,PHFHotDaysSevere_lag1,PHFHotDaysSevere_LagCombine,PHFHotDaysSevere_LagCombine_DupsRem)


PHFHotDaysSevere <- PHFHotDaysUpdate[,c(1:27,44:46)]

PHFMDCCountsHeat <- data.frame(colSums(PHFHotDaysSevere[,1:27],na.rm=TRUE))
colnames(PHFMDCCountsHeat) <- c("HeatCount")

#No stress General Profile

PHFNoStressFull <- PHFselect %>%
  filter(HeatLag0Update != 1 & HeatLag1Update != 1) %>%
  filter(month(Date) %in% c(4,5,6, 7, 8, 9))

PHFNoStress <- PHFNoStressFull[,c(1:27,44:46)]
#rm(PHFNoStressFull)

####### BootStrapping

#Compute Tables
# for (i in (1:1000)) {
#   print(i)
#   PHFNoStressRandom <- PHFNoStress %>%
#     sample_n(length(PHFHotDaysUpdate$MDC.02),replace = TRUE)
#   write.csv(PHFNoStressRandom,paste0("~/Desktop/NoStressRandomWx/PHF/PHF",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")  # Identify file names
# data_files    
# 
# for(i in 1:5) {                              # Head of for-loop
#   assign(paste0("PHF", i),                                   # Read and store data frames
#          read.csv(paste0("~/Desktop/NoStressRandomWx/",
#                           PHF[i])))
# }

PHF1 <- read.csv("~/Desktop/NoStressRandomWx/PHF/PHF1")
PHF2 <- read.csv("~/Desktop/NoStressRandomWx/PHF/PHF2")

PHF_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/PHF/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
PHF_data_all                                            # Print data to RStudio console

PHF_data_all_MDC <- PHF_data_all[,2:28]

PHFMDCCountsBoot <- data.frame(colSums(PHF_data_all_MDC)/1000)
colnames(PHFMDCCountsBoot) <- c("BootCount")

BacktoBackPHF <- cbind(PHFMDCCountsHeat,PHFMDCCountsBoot)

PHFNvH <- chisq.test(PHFMDCCountsBoot$BootCount, PHFMDCCountsHeat$HeatCount, correct=FALSE)
PHFNvH$p.value

# ChiResultsTableNoHotStress <- data.frame(matrix(nrow=1,ncol=1))

ChiResultsTableNoHotStress$PHF <- PHFNvH$p.value
rm(PHF,PHF_data_all,PHF_data_all_MDC,PHF1,PHF2,HotDaysSevere,PHFMDCCountsBoot,PHFMDCCountsHeat,PHFNoStressRandom,PHFProfileMDC,PHFselect)

# RIC

RIC <- read_excel("~/Desktop/RIC.Final.Weather.xlsx",na="NA")
RIC <- RIC[1:5844, ]

# Generate One Day Lag in excel


###### Investigation

RIC %>% mutate_if(is.character, as.numeric)
RIC$HotDaysExtreme <- as.numeric(RIC$HotDaysExtreme)

RICProfileMortleft <- RIC[ ,107:149]
RICProfileMortright <- RIC[ ,161:162]
RICProfileMort <- cbind(RICProfileMortleft,RICProfileMortright)
RICProfileMort$Date <- RIC$Date 
rm(RICProfileMortleft,RICProfileMortright)

RICProfileMDC <- RICProfileMort[ ,1:27]
RICProfileHotCold <- RIC[ , 161:162]
RIC$Date <- as.Date(RIC$Date)
#RICselect <- cbind(RICProfileHotCold,RICProfileMort)
RICselect <- RICProfileMort
rm(RICProfileHotCold,RICProfileMort)

#Heat General Profile

RICHotDaysSevere_lag0 <- RICselect[RICselect$HeatLag0Update == 1, ]
RICHotDaysSevere_lag1 <- RICselect[RICselect$HeatLag1Update == 1, ]
RICHotDaysSevere_LagCombine <- rbind(RICHotDaysSevere_lag0,RICHotDaysSevere_lag1)
RICHotDaysSevere_LagCombine_DupsRem <- RICHotDaysSevere_LagCombine[!duplicated(RICHotDaysSevere_LagCombine), ]
RICHotDaysUpdate <- RICHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

rm(RICHotDaysSevere_lag0,RICHotDaysSevere_lag1,RICHotDaysSevere_LagCombine,RICHotDaysSevere_LagCombine_DupsRem)


RICHotDaysSevere <- RICHotDaysUpdate[,c(1:27,44:46)]

RICMDCCountsHeat <- data.frame(colSums(RICHotDaysSevere[,1:27],na.rm=TRUE))
colnames(RICMDCCountsHeat) <- c("HeatCount")

#No stress General Profile

RICNoStressFull <- RICselect %>%
  filter(HeatLag0Update != 1 & HeatLag1Update != 1) %>%
  filter(month(Date) %in% c(4,5,6, 7, 8, 9))

RICNoStress <- RICNoStressFull[,c(1:27,44:46)]
#rm(RICNoStressFull)

####### BootStrapping

#Compute Tables
# for (i in (1:1000)) {
#   print(i)
#   RICNoStressRandom <- RICNoStress %>%
#     sample_n(length(RICHotDaysUpdate$MDC.02),replace = TRUE)
#   write.csv(RICNoStressRandom,paste0("~/Desktop/NoStressRandomWx/RIC/RIC",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")  # Identify file names
# data_files    
# 
# for(i in 1:5) {                              # Head of for-loop
#   assign(paste0("RIC", i),                                   # Read and store data frames
#          read.csv(paste0("~/Desktop/NoStressRandomWx/",
#                           RIC[i])))
# }

RIC1 <- read.csv("~/Desktop/NoStressRandomWx/RIC/RIC1")
RIC2 <- read.csv("~/Desktop/NoStressRandomWx/RIC/RIC2")

RIC_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/RIC/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
RIC_data_all                                            # Print data to RStudio console

RIC_data_all_MDC <- RIC_data_all[,2:28]

RICMDCCountsBoot <- data.frame(colSums(RIC_data_all_MDC)/1000)
colnames(RICMDCCountsBoot) <- c("BootCount")

BacktoBackRIC <- cbind(RICMDCCountsHeat,RICMDCCountsBoot)

RICNvH <- chisq.test(RICMDCCountsBoot$BootCount, RICMDCCountsHeat$HeatCount, correct=FALSE)
RICNvH$p.value

# ChiResultsTableNoHotStress <- data.frame(matrix(nrow=1,ncol=1))

ChiResultsTableNoHotStress$RIC <- RICNvH$p.value
rm(RIC,RIC_data_all,RIC_data_all_MDC,RIC1,RIC2,HotDaysSevere,RICMDCCountsBoot,RICMDCCountsHeat,RICNoStressRandom,RICProfileMDC,RICselect)

# ROA

ROA <- read_excel("~/Desktop/ROA.Final.Weather.xlsx",na="NA")
ROA <- ROA[1:5844, ]

# Generate One Day Lag in excel


###### Investigation

ROA %>% mutate_if(is.character, as.numeric)
ROA$HotDaysExtreme <- as.numeric(ROA$HotDaysExtreme)

ROAProfileMortleft <- ROA[ ,107:149]
ROAProfileMortright <- ROA[ ,161:162]
ROAProfileMort <- cbind(ROAProfileMortleft,ROAProfileMortright)
ROAProfileMort$Date <- ROA$Date 
rm(ROAProfileMortleft,ROAProfileMortright)

ROAProfileMDC <- ROAProfileMort[ ,1:27]
ROAProfileHotCold <- ROA[ , 161:162]
ROA$Date <- as.Date(ROA$Date)
#ROAselect <- cbind(ROAProfileHotCold,ROAProfileMort)
ROAselect <- ROAProfileMort
rm(ROAProfileHotCold,ROAProfileMort)

#Heat General Profile

ROAHotDaysSevere_lag0 <- ROAselect[ROAselect$HeatLag0Update == 1, ]
ROAHotDaysSevere_lag1 <- ROAselect[ROAselect$HeatLag1Update == 1, ]
ROAHotDaysSevere_LagCombine <- rbind(ROAHotDaysSevere_lag0,ROAHotDaysSevere_lag1)
ROAHotDaysSevere_LagCombine_DupsRem <- ROAHotDaysSevere_LagCombine[!duplicated(ROAHotDaysSevere_LagCombine), ]
ROAHotDaysUpdate <- ROAHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

rm(ROAHotDaysSevere_lag0,ROAHotDaysSevere_lag1,ROAHotDaysSevere_LagCombine,ROAHotDaysSevere_LagCombine_DupsRem)


ROAHotDaysSevere <- ROAHotDaysUpdate[,c(1:27,44:46)]

ROAMDCCountsHeat <- data.frame(colSums(ROAHotDaysSevere[,1:27],na.rm=TRUE))
colnames(ROAMDCCountsHeat) <- c("HeatCount")

#No stress General Profile

ROANoStressFull <- ROAselect %>%
  filter(HeatLag0Update != 1 & HeatLag1Update != 1) %>%
  filter(month(Date) %in% c(4,5,6, 7, 8, 9))

ROANoStress <- ROANoStressFull[,c(1:27,44:46)]
#rm(ROANoStressFull)

####### BootStrapping

#Compute Tables
# for (i in (1:1000)) {
#   print(i)
#   ROANoStressRandom <- ROANoStress %>%
#     sample_n(length(ROAHotDaysUpdate$MDC.02),replace = TRUE)
#   write.csv(ROANoStressRandom,paste0("~/Desktop/NoStressRandomWx/ROA/ROA",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")  # Identify file names
# data_files    
# 
# for(i in 1:5) {                              # Head of for-loop
#   assign(paste0("ROA", i),                                   # Read and store data frames
#          read.csv(paste0("~/Desktop/NoStressRandomWx/",
#                           ROA[i])))
# }

ROA1 <- read.csv("~/Desktop/NoStressRandomWx/ROA/ROA1")
ROA2 <- read.csv("~/Desktop/NoStressRandomWx/ROA/ROA2")

ROA_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/ROA/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
ROA_data_all                                            # Print data to RStudio console

ROA_data_all_MDC <- ROA_data_all[,2:28]

ROAMDCCountsBoot <- data.frame(colSums(ROA_data_all_MDC)/1000)
colnames(ROAMDCCountsBoot) <- c("BootCount")

BacktoBackROA <- cbind(ROAMDCCountsHeat,ROAMDCCountsBoot)

ROANvH <- chisq.test(ROAMDCCountsBoot$BootCount, ROAMDCCountsHeat$HeatCount, correct=FALSE)
ROANvH$p.value

# ChiResultsTableNoHotStress <- data.frame(matrix(nrow=1,ncol=1))

ChiResultsTableNoHotStress$ROA <- ROANvH$p.value
rm(ROA,ROA_data_all,ROA_data_all_MDC,ROA1,ROA2,HotDaysSevere,ROANoStressRandom,ROAProfileMDC,ROAselect)

# SHD

SHD <- read_excel("~/Desktop/SHD.Final.Weather.xlsx",na="NA")
SHD <- SHD[1:5844, ]

# Generate One Day Lag in excel


###### Investigation

SHD %>% mutate_if(is.character, as.numeric)
SHD$HotDaysExtreme <- as.numeric(SHD$HotDaysExtreme)

SHDProfileMortleft <- SHD[ ,107:149]
SHDProfileMortright <- SHD[ ,161:162]
SHDProfileMort <- cbind(SHDProfileMortleft,SHDProfileMortright)
SHDProfileMort$Date <- SHD$Date 
rm(SHDProfileMortleft,SHDProfileMortright)

SHDProfileMDC <- SHDProfileMort[ ,1:27]
SHDProfileHotCold <- SHD[ , 161:162]
SHD$Date <- as.Date(SHD$Date)
#SHDselect <- cbind(SHDProfileHotCold,SHDProfileMort)
SHDselect <- SHDProfileMort
rm(SHDProfileHotCold,SHDProfileMort)

#Heat General Profile

SHDHotDaysSevere_lag0 <- SHDselect[SHDselect$HeatLag0Update == 1, ]
SHDHotDaysSevere_lag1 <- SHDselect[SHDselect$HeatLag1Update == 1, ]
SHDHotDaysSevere_LagCombine <- rbind(SHDHotDaysSevere_lag0,SHDHotDaysSevere_lag1)
SHDHotDaysSevere_LagCombine_DupsRem <- SHDHotDaysSevere_LagCombine[!duplicated(SHDHotDaysSevere_LagCombine), ]
SHDHotDaysUpdate <- SHDHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

rm(SHDHotDaysSevere_lag0,SHDHotDaysSevere_lag1,SHDHotDaysSevere_LagCombine,SHDHotDaysSevere_LagCombine_DupsRem)


SHDHotDaysSevere <- SHDHotDaysUpdate[,c(1:27,44:46)]

SHDMDCCountsHeat <- data.frame(colSums(SHDHotDaysSevere[,1:27],na.rm=TRUE))
colnames(SHDMDCCountsHeat) <- c("HeatCount")

#No stress General Profile

SHDNoStressFull <- SHDselect %>%
  filter(HeatLag0Update != 1 & HeatLag1Update != 1) %>%
  filter(month(Date) %in% c(4,5,6, 7, 8, 9))

SHDNoStress <- SHDNoStressFull[,c(1:27,44:46)]
#rm(SHDNoStressFull)

####### BootStrapping

#Compute Tables
# for (i in (1:1000)) {
#   print(i)
#   SHDNoStressRandom <- SHDNoStress %>%
#     sample_n(length(SHDHotDaysUpdate$MDC.02),replace = TRUE)
#   write.csv(SHDNoStressRandom,paste0("~/Desktop/NoStressRandomWx/SHD/SHD",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")  # Identify file names
# data_files    
# 
# for(i in 1:5) {                              # Head of for-loop
#   assign(paste0("SHD", i),                                   # Read and store data frames
#          read.csv(paste0("~/Desktop/NoStressRandomWx/",
#                           SHD[i])))
# }

SHD1 <- read.csv("~/Desktop/NoStressRandomWx/SHD/SHD1")
SHD2 <- read.csv("~/Desktop/NoStressRandomWx/SHD/SHD2")

SHD_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/SHD/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
SHD_data_all                                            # Print data to RStudio console

SHD_data_all_MDC <- SHD_data_all[,2:28]

SHDMDCCountsBoot <- data.frame(colSums(SHD_data_all_MDC)/1000)
colnames(SHDMDCCountsBoot) <- c("BootCount")

BacktoBackSHD <- cbind(SHDMDCCountsHeat,SHDMDCCountsBoot)

SHDNvH <- chisq.test(SHDMDCCountsBoot$BootCount, SHDMDCCountsHeat$HeatCount, correct=FALSE)
SHDNvH$p.value

# ChiResultsTableNoHotStress <- data.frame(matrix(nrow=1,ncol=1))

ChiResultsTableNoHotStress$SHD <- SHDNvH$p.value
rm(SHD,SHD_data_all,SHD_data_all_MDC,SHD1,SHD2,HotDaysSevere,SHDMDCCountsBoot,SHDMDCCountsHeat,SHDNoStressRandom,SHDProfileMDC,SHDselect)

# VJI

VJI <- read_excel("~/Desktop/VJI.Final.Weather.xlsx",na="NA")
VJI <- VJI[1:5844, ]

# Generate One Day Lag in excel


###### Investigation

VJI %>% mutate_if(is.character, as.numeric)
VJI$HotDaysExtreme <- as.numeric(VJI$HotDaysExtreme)

VJIProfileMortleft <- VJI[ ,107:149]
VJIProfileMortright <- VJI[ ,161:162]
VJIProfileMort <- cbind(VJIProfileMortleft,VJIProfileMortright)
VJIProfileMort$Date <- VJI$Date 
rm(VJIProfileMortleft,VJIProfileMortright)

VJIProfileMDC <- VJIProfileMort[ ,1:27]
VJIProfileHotCold <- VJI[ , 161:162]
VJI$Date <- as.Date(VJI$Date)
#VJIselect <- cbind(VJIProfileHotCold,VJIProfileMort)
VJIselect <- VJIProfileMort
rm(VJIProfileHotCold,VJIProfileMort)

#Heat General Profile

VJIHotDaysSevere_lag0 <- VJIselect[VJIselect$HeatLag0Update == 1, ]
VJIHotDaysSevere_lag1 <- VJIselect[VJIselect$HeatLag1Update == 1, ]
VJIHotDaysSevere_LagCombine <- rbind(VJIHotDaysSevere_lag0,VJIHotDaysSevere_lag1)
VJIHotDaysSevere_LagCombine_DupsRem <- VJIHotDaysSevere_LagCombine[!duplicated(VJIHotDaysSevere_LagCombine), ]
VJIHotDaysUpdate <- VJIHotDaysSevere_LagCombine_DupsRem %>% arrange(Date)

rm(VJIHotDaysSevere_lag0,VJIHotDaysSevere_lag1,VJIHotDaysSevere_LagCombine,VJIHotDaysSevere_LagCombine_DupsRem)


VJIHotDaysSevere <- VJIHotDaysUpdate[,c(1:27,44:46)]

VJIMDCCountsHeat <- data.frame(colSums(VJIHotDaysSevere[,1:27],na.rm=TRUE))
colnames(VJIMDCCountsHeat) <- c("HeatCount")

#No stress General Profile

VJINoStressFull <- VJIselect %>%
  filter(HeatLag0Update != 1 & HeatLag1Update != 1) %>%
  filter(month(Date) %in% c(4,5,6, 7, 8, 9))

VJINoStress <- VJINoStressFull[,c(1:27,44:46)]
#rm(VJINoStressFull)

####### BootStrapping

#Compute Tables
# for (i in (1:1000)) {
#   print(i)
#   VJINoStressRandom <- VJINoStress %>%
#     sample_n(length(VJIHotDaysUpdate$MDC.02),replace = TRUE)
#   write.csv(VJINoStressRandom,paste0("~/Desktop/NoStressRandomWx/VJI/VJI",i))
# }

# data_files <- list.files("~/Desktop/NoStressRandomWx")  # Identify file names
# data_files    
# 
# for(i in 1:5) {                              # Head of for-loop
#   assign(paste0("VJI", i),                                   # Read and store data frames
#          read.csv(paste0("~/Desktop/NoStressRandomWx/",
#                           VJI[i])))
# }

VJI1 <- read.csv("~/Desktop/NoStressRandomWx/VJI/VJI1")
VJI2 <- read.csv("~/Desktop/NoStressRandomWx/VJI/VJI2")

VJI_data_all <- list.files(path = "~/Desktop/NoStressRandomWx/VJI/",  # Identify all CSV files
                           pattern = "*", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
VJI_data_all                                            # Print data to RStudio console

VJI_data_all_MDC <- VJI_data_all[,2:28]

VJIMDCCountsBoot <- data.frame(colSums(VJI_data_all_MDC)/1000)
colnames(VJIMDCCountsBoot) <- c("BootCount")

BacktoBackVJI <- cbind(VJIMDCCountsHeat,VJIMDCCountsBoot)

VJINvH <- chisq.test(VJIMDCCountsBoot$BootCount, VJIMDCCountsHeat$HeatCount, correct=FALSE)
VJINvH$p.value

# ChiResultsTableNoHotStress <- data.frame(matrix(nrow=1,ncol=1))

ChiResultsTableNoHotStress$VJI <- VJINvH$p.value
rm(VJI,VJI_data_all,VJI_data_all_MDC,VJI1,VJI2,HotDaysSevere,VJIMDCCountsBoot,VJIMDCCountsHeat,VJINoStressRandom,VJIProfileMDC,VJIselect)

ChiResultsTableRound <- round(ChiResultsTableNoHotStress,digits = 3)
ChiChiResultsTableRoundWx <- ChiResultsTableRound[,2:13]

library(rstatix)

OKVcontingency_table <- cbind(OKVMDCCountsHeat$HeatCount,OKVMDCCountsBoot$BootCount)
OKVcontingency_table[OKVcontingency_table==0] <- NA
OKVcontingency_table<-data.frame(OKVcontingency_table[complete.cases(OKVcontingency_table),])
OKVresult <- pairwise_prop_test(OKVcontingency_table)
unique(OKVresult$p.adj.signif)

ROAcontingency_table <- cbind(ROAMDCCountsHeat$HeatCount,ROAMDCCountsBoot$BootCount)
ROAcontingency_table[ROAcontingency_table==0] <- NA
ROAcontingency_table<-data.frame(ROAcontingency_table[complete.cases(ROAcontingency_table),])
ROAresult <- pairwise_prop_test(ROAcontingency_table)
unique(ROAresult$p.adj.signif)


########## SCRATCH BELOW

# CHO <- read_excel("~/Desktop/CHO.Final.Weather.xlsx",na="NA")
# CHO <- CHO[1:5844, ]
# 
# data <- cbind(CHO$Month,CHO$HeatLag0Update)
# data[data==0] <- NA
# data
# 
# data2<-data.frame(data[complete.cases(data),])
# hist(data2$X1,main="CHO Heat Months")
# sum(table(data2$X1)) #622
# 
# 
# EMV <- read_excel("~/Desktop/EMV.Final.Weather.xlsx",na="NA")
# EMV <- EMV[1:5844, ]
# 
# data <- cbind(EMV$Month,EMV$HeatLag0Update)
# data[data==0] <- NA
# data
# 
# data2<-data.frame(data[complete.cases(data),])
# hist(data2$X1,main="EMV Heat Months")
# sum(table(data2$X1)) #1865
# 
# EZF <- read_excel("~/Desktop/EZF.Final.Weather.xlsx",na="NA")
# EZF <- EZF[1:5844, ]
# 
# data <- cbind(EZF$Month,EZF$HeatLag0Update)
# data[data==0] <- NA
# data
# 
# data2<-data.frame(data[complete.cases(data),])
# hist(data2$X1,main="EZF Heat Months")
# sum(table(data2$X1)) #1626
# 
# IAD <- read_excel("~/Desktop/IAD.Final.Weather.xlsx",na="NA")
# IAD <- IAD[1:5844, ]
# 
# data <- cbind(IAD$Month,IAD$HeatLag0Update)
# data[data==0] <- NA
# data
# 
# data2<-data.frame(data[complete.cases(data),])
# hist(data2$X1,main="IAD Heat Months")
# sum(table(data2$X1)) #1246
# 
# LYH <- read_excel("~/Desktop/LYH.Final.Weather.xlsx",na="NA")
# LYH <- LYH[1:5844, ]
# 
# data <- cbind(LYH$Month,LYH$HeatLag0Update)
# data[data==0] <- NA
# data
# 
# data2<-data.frame(data[complete.cases(data),])
# hist(data2$X1,main="LYH Heat Months")
# sum(table(data2$X1)) #281
# 
# OKV <- read_excel("~/Desktop/OKV.Final.Weather.xlsx",na="NA")
# OKV <- OKV[1:5844, ]
# 
# data <- cbind(OKV$Month,OKV$HeatLag0Update)
# data[data==0] <- NA
# data
# 
# data2<-data.frame(data[complete.cases(data),])
# hist(data2$X1,main="OKV Heat Months")
# sum(table(data2$X1)) #748
# 
# ORF <- read_excel("~/Desktop/ORF.Final.Weather.xlsx",na="NA")
# ORF <- ORF[1:5844, ]
# 
# data <- cbind(ORF$Month,ORF$HeatLag0Update)
# data[data==0] <- NA
# data
# 
# data2<-data.frame(data[complete.cases(data),])
# hist(data2$X1,main="ORF Heat Months")
# sum(table(data2$X1)) #1026
# 
# PHF <- read_excel("~/Desktop/PHF.Final.Weather.xlsx",na="NA")
# PHF <- PHF[1:5844, ]
# 
# data <- cbind(PHF$Month,PHF$HeatLag0Update)
# data[data==0] <- NA
# data
# 
# data2<-data.frame(data[complete.cases(data),])
# hist(data2$X1,main="PHF Heat Months")
# sum(table(data2$X1)) #744
# 
# RIC <- read_excel("~/Desktop/RIC.Final.Weather.xlsx",na="NA")
# RIC <- RIC[1:5844, ]
# 
# data <- cbind(RIC$Month,RIC$HeatLag0Update)
# data[data==0] <- NA
# data
# 
# data2<-data.frame(data[complete.cases(data),])
# hist(data2$X1,main="RIC Heat Months")
# sum(table(data2$X1)) #1578
# 
# ROA <- read_excel("~/Desktop/ROA.Final.Weather.xlsx",na="NA")
# ROA <- ROA[1:5844, ]
# 
# data <- cbind(ROA$Month,ROA$HeatLag0Update)
# data[data==0] <- NA
# data
# 
# data2<-data.frame(data[complete.cases(data),])
# hist(data2$X1,main="ROA Heat Months")
# sum(table(data2$X1)) #1829
# 
# SHD <- read_excel("~/Desktop/SHD.Final.Weather.xlsx",na="NA")
# SHD <- SHD[1:5844, ]
# 
# data <- cbind(SHD$Month,SHD$HeatLag0Update)
# data[data==0] <- NA
# data
# 
# data2<-data.frame(data[complete.cases(data),])
# hist(data2$X1,main="SHD Heat Months")
# sum(table(data2$X1)) #1320
# 
# VJI <- read_excel("~/Desktop/VJI.Final.Weather.xlsx",na="NA")
# VJI <- VJI[1:5844, ]
# 
# data <- cbind(VJI$Month,VJI$HeatLag0Update)
# data[data==0] <- NA
# data
# 
# data2<-data.frame(data[complete.cases(data),])
# hist(data2$X1,main="VJI Heat Months")
# sum(table(data2$X1)) #1491
# 
# 
# 


# Creating a function to pass into boot() function
# bootFunc <- function(CHONoStress, i){
#   df <- CHONoStress[i, ]
#   # c(cor(df[, 2], df[, 3]),
#   #   median(df[, 2]),
#   mean(df[, 1])
#   # )
# }
# b1 <- boot(CHONoStress, bootFunc, R = 1000)
# print(b1)
# MDC1mean <- b1$t0
# 
# # Show all CI values
# #boot.ci(b, index = 1)
# #Compute for all columns
# 
# 
# #Compute Tables
# CHOBootTable <- data.frame(matrix(data = NA, nrow = 27, ncol = 1))
# colnames(CHOBootTable) <- c("Bootstrap Means")
# for (j in 1:27) {
#   print(j)
#   bootFunc <- function(CHONoStress, i){
#     df <- CHONoStress[i, ]
#     mean(df[, j])
#     # )
#   }
#   b <- boot(CHONoStress, bootFunc, R = 1000)
#   CHOBootTable[j, 1] = mean(b$t)
#   print(mean(b$t))
# }
# 
# RawTable <- data.frame(matrix(data = NA, nrow = 27, ncol = 1))
# colnames(RawTable) <- c("Raw Means")
# for (j in 1:27) {
#   print(j)
#   RawTable[j,1] <- mean(CHONoStress[,j],na.rm=TRUE)
# }
# 
# 
# check <- cbind(CHOBootTable,RawTable)
# rownames(check) <- colnames(CHONoStress)
# 
# CHONoStressBoot <- CHOBootTable
# for (i in (1:27)) {
#   CHONoStressBoot[i,2] <- ((CHONoStressBoot[i,] / sum(CHONoStressBoot$`Bootstrap Means`)) * 100)
# }
# colnames(CHONoStressBoot) <- c("BootValues","BootProportion")
# CHONoStressBoot$BootProportion <- round(CHONoStressBoot$BootProportion, digits = 2)
# sum(CHONoStressBoot$BootProportion)
# 
# CHOmort <- sum(CHO$Mort)
# EMVmort <- sum(EMV$Mort)
# EZFmort <- sum(EZF$Mort)
# IADmort <- sum(IAD$Mort)
# LYHmort <- sum(LYH$Mort)
# OKVmort <- sum(OKV$Mort)
# ORFmort <- sum(ORF$Mort)
# PHFmort <- sum(PHF$Mort)
# RICmort <- sum(RIC$Mort)
# ROAmort <- sum(ROA$Mort)
# SHDmort <- sum(SHD$Mort)
# VJImort <- sum(VJI$Mort)
# answermort <- CHOmort + EMVmort + EZFmort + IADmort + LYHmort + OKVmort + ORFmort + PHFmort + RICmort + ROAmort + SHDmort + VJImort
# 
# 
# answerallproportionTable <- (CHOallproportionTable + EMVallproportionTable + EZFallproportionTable + IADallproportionTable + LYHallproportionTable + OKVallproportionTable + ORFallproportionTable + PHFallproportionTable + RICallproportionTable + ROAallproportionTable + SHDallproportionTable + VJIallproportionTable)/12
# answerallproportionTableround <- round(answerallproportionTable,digits=2)
# 
# 
# write.csv(EMVallproportionTable,"EMVMDCProportionTable.csv")
# write.csv(EZFallproportionTable,"EZFMDCProportionTable.csv")
# write.csv(IADallproportionTable,"IADMDCProportionTable.csv")
# write.csv(LYHallproportionTable,"LYHMDCProportionTable.csv")
# write.csv(OKVallproportionTable,"OKVMDCProportionTable.csv")
# write.csv(ORFallproportionTable,"ORFMDCProportionTable.csv")
# write.csv(PHFallproportionTable,"PHFMDCProportionTable.csv")
# write.csv(RICallproportionTable,"RICMDCProportionTable.csv")
# write.csv(ROAallproportionTable,"ROAMDCProportionTable.csv")
# write.csv(SHDallproportionTable,"SHDMDCProportionTable.csv")
# write.csv(VJIallproportionTable,"VJIMDCProportionTable.csv")
# 
# write.csv(answerallproportionTableround,"allMDCProportionTable.csv")
# 
# 
