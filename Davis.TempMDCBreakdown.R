library(readxl)
library(lubridate)
library(dplyr)

MDC0514 <- read.csv("MDC_Station_Matching_0514.csv")
MDC1520_bad <- read.csv("MDC_Station_Matching_1520.csv")
MDC20 <- read.csv("MDC2020_GoodToGo.csv")

MDC_Bad2020Removed <- MDC1520_bad[year(MDC1520_bad$Date) < 2020, ]
rm(MDC1520_bad)

MDC0514$Date <- format(as.Date(MDC0514$Date),"%Y-%m-%d")
MDC_Bad2020Removed$Date <- format(as.Date(MDC_Bad2020Removed$Date),"%Y-%m-%d")
MDC20$Date <- format(as.Date(MDC20$Date),"%Y-%m-%d")

#CHO

CHO_Final_Weather <- read_excel("~/Desktop/CHO.Final.Weather.xlsx")
CHO_Final_Weather  <- CHO_Final_Weather[1:5844,]
CHO_Final_Weather$Date_Update <- format(as.Date(CHO_Final_Weather$Date), "%Y-%m-%d")

CHO_Final_Weather$MinTLoop <- CHO_Final_Weather$`MinT(C)`
CHO_Final_Weather$MinTLoop[is.na(CHO_Final_Weather$MinTLoop)] <- 10

CHO_Final_Weather$MinTColdLag00 <- NA
CHO_Final_Weather$MinTColdLag01 <- NA
CHO_Final_Weather$MinTColdLag02 <- NA
CHO_Final_Weather$MinTColdLag03 <- NA
CHO_Final_Weather$MinTColdLag04 <- NA
CHO_Final_Weather$MinTColdLag05 <- NA
CHO_Final_Weather$MinTColdLag06 <- NA
CHO_Final_Weather$MinTColdLag07 <- NA


for (i in (1:5844)) {
  print(i)
  if (CHO_Final_Weather$MinTLoop[i] <= quantile(CHO_Final_Weather$`MinT(C)`, probs = .01,na.rm = T)) {
    CHO_Final_Weather$MinTColdLag00[i] = 1
    CHO_Final_Weather$MinTColdLag01[i+1] = 1
    CHO_Final_Weather$MinTColdLag02[i+2] = 1
    CHO_Final_Weather$MinTColdLag03[i+3] = 1
    CHO_Final_Weather$MinTColdLag04[i+4] = 1
    CHO_Final_Weather$MinTColdLag05[i+5] = 1
    CHO_Final_Weather$MinTColdLag06[i+6] = 1
    CHO_Final_Weather$MinTColdLag07[i+7] = 1
  }
}

CHO_Final_Weather$MinTColdLag00[is.na(CHO_Final_Weather$MinTColdLag00)] <- 0
CHO_Final_Weather$MinTColdLag01[is.na(CHO_Final_Weather$MinTColdLag01)] <- 0
CHO_Final_Weather$MinTColdLag02[is.na(CHO_Final_Weather$MinTColdLag02)] <- 0
CHO_Final_Weather$MinTColdLag03[is.na(CHO_Final_Weather$MinTColdLag03)] <- 0
CHO_Final_Weather$MinTColdLag04[is.na(CHO_Final_Weather$MinTColdLag04)] <- 0
CHO_Final_Weather$MinTColdLag05[is.na(CHO_Final_Weather$MinTColdLag05)] <- 0
CHO_Final_Weather$MinTColdLag06[is.na(CHO_Final_Weather$MinTColdLag06)] <- 0
CHO_Final_Weather$MinTColdLag07[is.na(CHO_Final_Weather$MinTColdLag07)] <- 0


CHO_Final_Weather$MinTWarmLag00 <- NA
CHO_Final_Weather$MinTWarmLag01 <- NA
CHO_Final_Weather$MinTWarmLag02 <- NA

for (i in (1:5844)) {
  print(i)
  if (CHO_Final_Weather$MinTLoop[i] >= quantile(CHO_Final_Weather$`MinT(C)`, probs = .99,na.rm = T)) {
    CHO_Final_Weather$MinTWarmLag00[i] = 1
    CHO_Final_Weather$MinTWarmLag01[i+1] = 1
    CHO_Final_Weather$MinTWarmLag02[i+2] = 1
  }
}

CHO_Final_Weather$MinTWarmLag00[is.na(CHO_Final_Weather$MinTWarmLag00)] <- 0
CHO_Final_Weather$MinTWarmLag01[is.na(CHO_Final_Weather$MinTWarmLag01)] <- 0
CHO_Final_Weather$MinTWarmLag02[is.na(CHO_Final_Weather$MinTWarmLag02)] <- 0

CHO_Final_Weather$KeepTheseDaysCold <- NA

for (i in (1:5844)) {
  print(i)
  if (CHO_Final_Weather$MinTColdLag00[i] == 1 | CHO_Final_Weather$MinTColdLag01[i] == 1 | CHO_Final_Weather$MinTColdLag02[i] == 1 | CHO_Final_Weather$MinTColdLag03[i] == 1 | CHO_Final_Weather$MinTColdLag04[i] == 1 | CHO_Final_Weather$MinTColdLag05[i] == 1 | CHO_Final_Weather$MinTColdLag06[i] == 1 | CHO_Final_Weather$MinTColdLag07[i] == 1) {
    CHO_Final_Weather$KeepTheseDaysCold[i] = 1
  } else {
    CHO_Final_Weather$KeepTheseDaysCold[i] = 0
  }
}

CHO_Final_Weather$KeepTheseDaysWarm <- NA

for (i in (1:5844)) {
  print(i)
  if (CHO_Final_Weather$MinTWarmLag00[i] == 1 | CHO_Final_Weather$MinTWarmLag01[i] == 1 | CHO_Final_Weather$MinTWarmLag02[i] == 1) {
    CHO_Final_Weather$KeepTheseDaysWarm[i] = 1
  } else {
    CHO_Final_Weather$KeepTheseDaysWarm[i] = 0
  }
}

CHO_Final_Weather$KeepTheseDaysNoStress <- NA
CHO_Final_Weather$KeepTheseDaysNoStress <- ifelse(CHO_Final_Weather$KeepTheseDaysCold == 1 | CHO_Final_Weather$KeepTheseDaysWarm == 1, 0, 1)

CHO_Weather_ColdOnly <- CHO_Final_Weather[(CHO_Final_Weather$KeepTheseDaysCold == 1), ]
CHO_Weather_WarmOnly <- CHO_Final_Weather[(CHO_Final_Weather$KeepTheseDaysWarm == 1), ]
CHO_Weather_NormalOnly <- CHO_Final_Weather[(CHO_Final_Weather$KeepTheseDaysNoStress == 1), ]

CHO_MDC_0514 <- MDC0514 %>%
  filter(ID == "CHO")

CHO_MDC_1519 <- MDC_Bad2020Removed %>%
  filter(ID == "CHO")

CHO_MDC_20 <- MDC20 %>%
  filter(ID == "CHO")

CHO_MDC_Cold_0514_NAs <- left_join(CHO_Weather_ColdOnly, 
                         CHO_MDC_0514,
                         by = c("Date_Update" = "Date"))

CHO_MDC0514_Cold <- CHO_MDC_Cold_0514_NAs[!is.na(CHO_MDC_Cold_0514_NAs$MDCAcme),]

write.csv(CHO_MDC0514_Cold, "CHO_MDC0514_Cold.csv")

CHO_MDC_Cold_1519_NAs <- left_join(CHO_Weather_ColdOnly, 
                                   CHO_MDC_1519,
                                   by = c("Date_Update" = "Date"))

CHO_MDC1519_Cold <- CHO_MDC_Cold_1519_NAs[!is.na(CHO_MDC_Cold_1519_NAs$MDCAcme),]

write.csv(CHO_MDC1519_Cold, "CHO_MDC1519_Cold.csv")

CHO_MDC_Cold_20_NAs <- left_join(CHO_Weather_ColdOnly, 
                                   CHO_MDC_20,
                                   by = c("Date_Update" = "Date"))

CHO_MDC20_Cold <- CHO_MDC_Cold_20_NAs[!is.na(CHO_MDC_Cold_20_NAs$MDC0ACME),]

write.csv(CHO_MDC20_Cold, "CHO_MDC20_Cold.csv")


CHO_MDC_Warm_0514_NAs <- left_join(CHO_Weather_WarmOnly, 
                                   CHO_MDC_0514,
                                   by = c("Date_Update" = "Date"))

CHO_MDC0514_Warm <- CHO_MDC_Warm_0514_NAs[!is.na(CHO_MDC_Warm_0514_NAs$MDCAcme),]

write.csv(CHO_MDC0514_Warm, "CHO_MDC0514_Warm.csv")

CHO_MDC_Warm_1519_NAs <- left_join(CHO_Weather_WarmOnly, 
                                   CHO_MDC_1519,
                                   by = c("Date_Update" = "Date"))

CHO_MDC1519_Warm <- CHO_MDC_Warm_1519_NAs[!is.na(CHO_MDC_Warm_1519_NAs$MDCAcme),]

write.csv(CHO_MDC1519_Warm, "CHO_MDC1519_Warm.csv")

CHO_MDC_Warm_20_NAs <- left_join(CHO_Weather_WarmOnly, 
                                 CHO_MDC_20,
                                 by = c("Date_Update" = "Date"))

CHO_MDC20_Warm <- CHO_MDC_Warm_20_NAs[!is.na(CHO_MDC_Warm_20_NAs$MDC0ACME),]

write.csv(CHO_MDC20_Warm, "CHO_MDC20_Warm.csv")


CHO_MDC_Normal_0514_NAs <- left_join(CHO_Weather_NormalOnly, 
                                     CHO_MDC_0514,
                                     by = c("Date_Update" = "Date"))

CHO_MDC0514_Normal <- CHO_MDC_Normal_0514_NAs[!is.na(CHO_MDC_Normal_0514_NAs$MDCAcme),]

write.csv(CHO_MDC0514_Normal, "CHO_MDC0514_Normal.csv")

CHO_MDC_Normal_1519_NAs <- left_join(CHO_Weather_NormalOnly, 
                                     CHO_MDC_1519,
                                     by = c("Date_Update" = "Date"))

CHO_MDC1519_Normal <- CHO_MDC_Normal_1519_NAs[!is.na(CHO_MDC_Normal_1519_NAs$MDCAcme),]

write.csv(CHO_MDC1519_Normal, "CHO_MDC1519_Normal.csv")

CHO_MDC_Normal_20_NAs <- left_join(CHO_Weather_NormalOnly, 
                                   CHO_MDC_20,
                                   by = c("Date_Update" = "Date"))

CHO_MDC20_Normal <- CHO_MDC_Normal_20_NAs[!is.na(CHO_MDC_Normal_20_NAs$MDC0ACME),]

write.csv(CHO_MDC20_Normal, "CHO_MDC20_Normal.csv")

#EMV

EMV_Final_Weather <- read_excel("~/Desktop/EMV.Final.Weather.xlsx")
EMV_Final_Weather  <- EMV_Final_Weather[1:5844,]
EMV_Final_Weather$Date_Update <- format(as.Date(EMV_Final_Weather$Date), "%Y-%m-%d")

EMV_Final_Weather$MinTLoop <- EMV_Final_Weather$`MinT(C)`
EMV_Final_Weather$MinTLoop[is.na(EMV_Final_Weather$MinTLoop)] <- 10

EMV_Final_Weather$MinTColdLag00 <- NA
EMV_Final_Weather$MinTColdLag01 <- NA
EMV_Final_Weather$MinTColdLag02 <- NA
EMV_Final_Weather$MinTColdLag03 <- NA
EMV_Final_Weather$MinTColdLag04 <- NA
EMV_Final_Weather$MinTColdLag05 <- NA
EMV_Final_Weather$MinTColdLag06 <- NA
EMV_Final_Weather$MinTColdLag07 <- NA


for (i in (1:5844)) {
  print(i)
  if (EMV_Final_Weather$MinTLoop[i] <= quantile(EMV_Final_Weather$`MinT(C)`, probs = .01,na.rm = T)) {
    EMV_Final_Weather$MinTColdLag00[i] = 1
    EMV_Final_Weather$MinTColdLag01[i+1] = 1
    EMV_Final_Weather$MinTColdLag02[i+2] = 1
    EMV_Final_Weather$MinTColdLag03[i+3] = 1
    EMV_Final_Weather$MinTColdLag04[i+4] = 1
    EMV_Final_Weather$MinTColdLag05[i+5] = 1
    EMV_Final_Weather$MinTColdLag06[i+6] = 1
    EMV_Final_Weather$MinTColdLag07[i+7] = 1
  }
}

EMV_Final_Weather$MinTColdLag00[is.na(EMV_Final_Weather$MinTColdLag00)] <- 0
EMV_Final_Weather$MinTColdLag01[is.na(EMV_Final_Weather$MinTColdLag01)] <- 0
EMV_Final_Weather$MinTColdLag02[is.na(EMV_Final_Weather$MinTColdLag02)] <- 0
EMV_Final_Weather$MinTColdLag03[is.na(EMV_Final_Weather$MinTColdLag03)] <- 0
EMV_Final_Weather$MinTColdLag04[is.na(EMV_Final_Weather$MinTColdLag04)] <- 0
EMV_Final_Weather$MinTColdLag05[is.na(EMV_Final_Weather$MinTColdLag05)] <- 0
EMV_Final_Weather$MinTColdLag06[is.na(EMV_Final_Weather$MinTColdLag06)] <- 0
EMV_Final_Weather$MinTColdLag07[is.na(EMV_Final_Weather$MinTColdLag07)] <- 0


EMV_Final_Weather$MinTWarmLag00 <- NA
EMV_Final_Weather$MinTWarmLag01 <- NA
EMV_Final_Weather$MinTWarmLag02 <- NA

for (i in (1:5844)) {
  print(i)
  if (EMV_Final_Weather$MinTLoop[i] >= quantile(EMV_Final_Weather$`MinT(C)`, probs = .99,na.rm = T)) {
    EMV_Final_Weather$MinTWarmLag00[i] = 1
    EMV_Final_Weather$MinTWarmLag01[i+1] = 1
    EMV_Final_Weather$MinTWarmLag02[i+2] = 1
  }
}

EMV_Final_Weather$MinTWarmLag00[is.na(EMV_Final_Weather$MinTWarmLag00)] <- 0
EMV_Final_Weather$MinTWarmLag01[is.na(EMV_Final_Weather$MinTWarmLag01)] <- 0
EMV_Final_Weather$MinTWarmLag02[is.na(EMV_Final_Weather$MinTWarmLag02)] <- 0

EMV_Final_Weather$KeepTheseDaysCold <- NA

for (i in (1:5844)) {
  print(i)
  if (EMV_Final_Weather$MinTColdLag00[i] == 1 | EMV_Final_Weather$MinTColdLag01[i] == 1 | EMV_Final_Weather$MinTColdLag02[i] == 1 | EMV_Final_Weather$MinTColdLag03[i] == 1 | EMV_Final_Weather$MinTColdLag04[i] == 1 | EMV_Final_Weather$MinTColdLag05[i] == 1 | EMV_Final_Weather$MinTColdLag06[i] == 1 | EMV_Final_Weather$MinTColdLag07[i] == 1) {
    EMV_Final_Weather$KeepTheseDaysCold[i] = 1
  } else {
    EMV_Final_Weather$KeepTheseDaysCold[i] = 0
  }
}

EMV_Final_Weather$KeepTheseDaysWarm <- NA

for (i in (1:5844)) {
  print(i)
  if (EMV_Final_Weather$MinTWarmLag00[i] == 1 | EMV_Final_Weather$MinTWarmLag01[i] == 1 | EMV_Final_Weather$MinTWarmLag02[i] == 1) {
    EMV_Final_Weather$KeepTheseDaysWarm[i] = 1
  } else {
    EMV_Final_Weather$KeepTheseDaysWarm[i] = 0
  }
}

EMV_Final_Weather$KeepTheseDaysNoStress <- NA
EMV_Final_Weather$KeepTheseDaysNoStress <- ifelse(EMV_Final_Weather$KeepTheseDaysCold == 1 | EMV_Final_Weather$KeepTheseDaysWarm == 1, 0, 1)

EMV_Weather_ColdOnly <- EMV_Final_Weather[(EMV_Final_Weather$KeepTheseDaysCold == 1), ]
EMV_Weather_WarmOnly <- EMV_Final_Weather[(EMV_Final_Weather$KeepTheseDaysWarm == 1), ]
EMV_Weather_NormalOnly <- EMV_Final_Weather[(EMV_Final_Weather$KeepTheseDaysNoStress == 1), ]

EMV_MDC_0514 <- MDC0514 %>%
  filter(ID == "EMV")

EMV_MDC_1519 <- MDC_Bad2020Removed %>%
  filter(ID == "EMV")

EMV_MDC_20 <- MDC20 %>%
  filter(ID == "EMV")

EMV_MDC_Cold_0514_NAs <- left_join(EMV_Weather_ColdOnly, 
                                   EMV_MDC_0514,
                                   by = c("Date_Update" = "Date"))

EMV_MDC0514_Cold <- EMV_MDC_Cold_0514_NAs[!is.na(EMV_MDC_Cold_0514_NAs$MDCAcme),]

write.csv(EMV_MDC0514_Cold, "EMV_MDC0514_Cold.csv")

EMV_MDC_Cold_1519_NAs <- left_join(EMV_Weather_ColdOnly, 
                                   EMV_MDC_1519,
                                   by = c("Date_Update" = "Date"))

EMV_MDC1519_Cold <- EMV_MDC_Cold_1519_NAs[!is.na(EMV_MDC_Cold_1519_NAs$MDCAcme),]

write.csv(EMV_MDC1519_Cold, "EMV_MDC1519_Cold.csv")

EMV_MDC_Cold_20_NAs <- left_join(EMV_Weather_ColdOnly, 
                                 EMV_MDC_20,
                                 by = c("Date_Update" = "Date"))

EMV_MDC20_Cold <- EMV_MDC_Cold_20_NAs[!is.na(EMV_MDC_Cold_20_NAs$MDC0ACME),]

write.csv(EMV_MDC20_Cold, "EMV_MDC20_Cold.csv")


EMV_MDC_Warm_0514_NAs <- left_join(EMV_Weather_WarmOnly, 
                                   EMV_MDC_0514,
                                   by = c("Date_Update" = "Date"))

EMV_MDC0514_Warm <- EMV_MDC_Warm_0514_NAs[!is.na(EMV_MDC_Warm_0514_NAs$MDCAcme),]

write.csv(EMV_MDC0514_Warm, "EMV_MDC0514_Warm.csv")

EMV_MDC_Warm_1519_NAs <- left_join(EMV_Weather_WarmOnly, 
                                   EMV_MDC_1519,
                                   by = c("Date_Update" = "Date"))

EMV_MDC1519_Warm <- EMV_MDC_Warm_1519_NAs[!is.na(EMV_MDC_Warm_1519_NAs$MDCAcme),]

write.csv(EMV_MDC1519_Warm, "EMV_MDC1519_Warm.csv")

EMV_MDC_Warm_20_NAs <- left_join(EMV_Weather_WarmOnly, 
                                 EMV_MDC_20,
                                 by = c("Date_Update" = "Date"))

EMV_MDC20_Warm <- EMV_MDC_Warm_20_NAs[!is.na(EMV_MDC_Warm_20_NAs$MDC0ACME),]

write.csv(EMV_MDC20_Warm, "EMV_MDC20_Warm.csv")


EMV_MDC_Normal_0514_NAs <- left_join(EMV_Weather_NormalOnly, 
                                     EMV_MDC_0514,
                                     by = c("Date_Update" = "Date"))

EMV_MDC0514_Normal <- EMV_MDC_Normal_0514_NAs[!is.na(EMV_MDC_Normal_0514_NAs$MDCAcme),]

write.csv(EMV_MDC0514_Normal, "EMV_MDC0514_Normal.csv")

EMV_MDC_Normal_1519_NAs <- left_join(EMV_Weather_NormalOnly, 
                                     EMV_MDC_1519,
                                     by = c("Date_Update" = "Date"))

EMV_MDC1519_Normal <- EMV_MDC_Normal_1519_NAs[!is.na(EMV_MDC_Normal_1519_NAs$MDCAcme),]

write.csv(EMV_MDC1519_Normal, "EMV_MDC1519_Normal.csv")

EMV_MDC_Normal_20_NAs <- left_join(EMV_Weather_NormalOnly, 
                                   EMV_MDC_20,
                                   by = c("Date_Update" = "Date"))

EMV_MDC20_Normal <- EMV_MDC_Normal_20_NAs[!is.na(EMV_MDC_Normal_20_NAs$MDC0ACME),]

write.csv(EMV_MDC20_Normal, "EMV_MDC20_Normal.csv")

#EZF

EZF_Final_Weather <- read_excel("~/Desktop/EZF.Final.Weather.xlsx")
EZF_Final_Weather  <- EZF_Final_Weather[1:5844,]
EZF_Final_Weather$Date_Update <- format(as.Date(EZF_Final_Weather$Date), "%Y-%m-%d")

EZF_Final_Weather$MinTLoop <- EZF_Final_Weather$`MinT(C)`
EZF_Final_Weather$MinTLoop[is.na(EZF_Final_Weather$MinTLoop)] <- 10

EZF_Final_Weather$MinTColdLag00 <- NA
EZF_Final_Weather$MinTColdLag01 <- NA
EZF_Final_Weather$MinTColdLag02 <- NA
EZF_Final_Weather$MinTColdLag03 <- NA
EZF_Final_Weather$MinTColdLag04 <- NA
EZF_Final_Weather$MinTColdLag05 <- NA
EZF_Final_Weather$MinTColdLag06 <- NA
EZF_Final_Weather$MinTColdLag07 <- NA


for (i in (1:5844)) {
  print(i)
  if (EZF_Final_Weather$MinTLoop[i] <= quantile(EZF_Final_Weather$`MinT(C)`, probs = .01,na.rm = T)) {
    EZF_Final_Weather$MinTColdLag00[i] = 1
    EZF_Final_Weather$MinTColdLag01[i+1] = 1
    EZF_Final_Weather$MinTColdLag02[i+2] = 1
    EZF_Final_Weather$MinTColdLag03[i+3] = 1
    EZF_Final_Weather$MinTColdLag04[i+4] = 1
    EZF_Final_Weather$MinTColdLag05[i+5] = 1
    EZF_Final_Weather$MinTColdLag06[i+6] = 1
    EZF_Final_Weather$MinTColdLag07[i+7] = 1
  }
}

EZF_Final_Weather$MinTColdLag00[is.na(EZF_Final_Weather$MinTColdLag00)] <- 0
EZF_Final_Weather$MinTColdLag01[is.na(EZF_Final_Weather$MinTColdLag01)] <- 0
EZF_Final_Weather$MinTColdLag02[is.na(EZF_Final_Weather$MinTColdLag02)] <- 0
EZF_Final_Weather$MinTColdLag03[is.na(EZF_Final_Weather$MinTColdLag03)] <- 0
EZF_Final_Weather$MinTColdLag04[is.na(EZF_Final_Weather$MinTColdLag04)] <- 0
EZF_Final_Weather$MinTColdLag05[is.na(EZF_Final_Weather$MinTColdLag05)] <- 0
EZF_Final_Weather$MinTColdLag06[is.na(EZF_Final_Weather$MinTColdLag06)] <- 0
EZF_Final_Weather$MinTColdLag07[is.na(EZF_Final_Weather$MinTColdLag07)] <- 0


EZF_Final_Weather$MinTWarmLag00 <- NA
EZF_Final_Weather$MinTWarmLag01 <- NA
EZF_Final_Weather$MinTWarmLag02 <- NA

for (i in (1:5844)) {
  print(i)
  if (EZF_Final_Weather$MinTLoop[i] >= quantile(EZF_Final_Weather$`MinT(C)`, probs = .99,na.rm = T)) {
    EZF_Final_Weather$MinTWarmLag00[i] = 1
    EZF_Final_Weather$MinTWarmLag01[i+1] = 1
    EZF_Final_Weather$MinTWarmLag02[i+2] = 1
  }
}

EZF_Final_Weather$MinTWarmLag00[is.na(EZF_Final_Weather$MinTWarmLag00)] <- 0
EZF_Final_Weather$MinTWarmLag01[is.na(EZF_Final_Weather$MinTWarmLag01)] <- 0
EZF_Final_Weather$MinTWarmLag02[is.na(EZF_Final_Weather$MinTWarmLag02)] <- 0

EZF_Final_Weather$KeepTheseDaysCold <- NA

for (i in (1:5844)) {
  print(i)
  if (EZF_Final_Weather$MinTColdLag00[i] == 1 | EZF_Final_Weather$MinTColdLag01[i] == 1 | EZF_Final_Weather$MinTColdLag02[i] == 1 | EZF_Final_Weather$MinTColdLag03[i] == 1 | EZF_Final_Weather$MinTColdLag04[i] == 1 | EZF_Final_Weather$MinTColdLag05[i] == 1 | EZF_Final_Weather$MinTColdLag06[i] == 1 | EZF_Final_Weather$MinTColdLag07[i] == 1) {
    EZF_Final_Weather$KeepTheseDaysCold[i] = 1
  } else {
    EZF_Final_Weather$KeepTheseDaysCold[i] = 0
  }
}

EZF_Final_Weather$KeepTheseDaysWarm <- NA

for (i in (1:5844)) {
  print(i)
  if (EZF_Final_Weather$MinTWarmLag00[i] == 1 | EZF_Final_Weather$MinTWarmLag01[i] == 1 | EZF_Final_Weather$MinTWarmLag02[i] == 1) {
    EZF_Final_Weather$KeepTheseDaysWarm[i] = 1
  } else {
    EZF_Final_Weather$KeepTheseDaysWarm[i] = 0
  }
}

EZF_Final_Weather$KeepTheseDaysNoStress <- NA
EZF_Final_Weather$KeepTheseDaysNoStress <- ifelse(EZF_Final_Weather$KeepTheseDaysCold == 1 | EZF_Final_Weather$KeepTheseDaysWarm == 1, 0, 1)

EZF_Weather_ColdOnly <- EZF_Final_Weather[(EZF_Final_Weather$KeepTheseDaysCold == 1), ]
EZF_Weather_WarmOnly <- EZF_Final_Weather[(EZF_Final_Weather$KeepTheseDaysWarm == 1), ]
EZF_Weather_NormalOnly <- EZF_Final_Weather[(EZF_Final_Weather$KeepTheseDaysNoStress == 1), ]

EZF_MDC_0514 <- MDC0514 %>%
  filter(ID == "EZF")

EZF_MDC_1519 <- MDC_Bad2020Removed %>%
  filter(ID == "EZF")

EZF_MDC_20 <- MDC20 %>%
  filter(ID == "EZF")

EZF_MDC_Cold_0514_NAs <- left_join(EZF_Weather_ColdOnly, 
                                   EZF_MDC_0514,
                                   by = c("Date_Update" = "Date"))

EZF_MDC0514_Cold <- EZF_MDC_Cold_0514_NAs[!is.na(EZF_MDC_Cold_0514_NAs$MDCAcme),]

write.csv(EZF_MDC0514_Cold, "EZF_MDC0514_Cold.csv")

EZF_MDC_Cold_1519_NAs <- left_join(EZF_Weather_ColdOnly, 
                                   EZF_MDC_1519,
                                   by = c("Date_Update" = "Date"))

EZF_MDC1519_Cold <- EZF_MDC_Cold_1519_NAs[!is.na(EZF_MDC_Cold_1519_NAs$MDCAcme),]

write.csv(EZF_MDC1519_Cold, "EZF_MDC1519_Cold.csv")

EZF_MDC_Cold_20_NAs <- left_join(EZF_Weather_ColdOnly, 
                                 EZF_MDC_20,
                                 by = c("Date_Update" = "Date"))

EZF_MDC20_Cold <- EZF_MDC_Cold_20_NAs[!is.na(EZF_MDC_Cold_20_NAs$MDC0ACME),]

write.csv(EZF_MDC20_Cold, "EZF_MDC20_Cold.csv")


EZF_MDC_Warm_0514_NAs <- left_join(EZF_Weather_WarmOnly, 
                                   EZF_MDC_0514,
                                   by = c("Date_Update" = "Date"))

EZF_MDC0514_Warm <- EZF_MDC_Warm_0514_NAs[!is.na(EZF_MDC_Warm_0514_NAs$MDCAcme),]

write.csv(EZF_MDC0514_Warm, "EZF_MDC0514_Warm.csv")

EZF_MDC_Warm_1519_NAs <- left_join(EZF_Weather_WarmOnly, 
                                   EZF_MDC_1519,
                                   by = c("Date_Update" = "Date"))

EZF_MDC1519_Warm <- EZF_MDC_Warm_1519_NAs[!is.na(EZF_MDC_Warm_1519_NAs$MDCAcme),]

write.csv(EZF_MDC1519_Warm, "EZF_MDC1519_Warm.csv")

EZF_MDC_Warm_20_NAs <- left_join(EZF_Weather_WarmOnly, 
                                 EZF_MDC_20,
                                 by = c("Date_Update" = "Date"))

EZF_MDC20_Warm <- EZF_MDC_Warm_20_NAs[!is.na(EZF_MDC_Warm_20_NAs$MDC0ACME),]

write.csv(EZF_MDC20_Warm, "EZF_MDC20_Warm.csv")


EZF_MDC_Normal_0514_NAs <- left_join(EZF_Weather_NormalOnly, 
                                     EZF_MDC_0514,
                                     by = c("Date_Update" = "Date"))

EZF_MDC0514_Normal <- EZF_MDC_Normal_0514_NAs[!is.na(EZF_MDC_Normal_0514_NAs$MDCAcme),]

write.csv(EZF_MDC0514_Normal, "EZF_MDC0514_Normal.csv")

EZF_MDC_Normal_1519_NAs <- left_join(EZF_Weather_NormalOnly, 
                                     EZF_MDC_1519,
                                     by = c("Date_Update" = "Date"))

EZF_MDC1519_Normal <- EZF_MDC_Normal_1519_NAs[!is.na(EZF_MDC_Normal_1519_NAs$MDCAcme),]

write.csv(EZF_MDC1519_Normal, "EZF_MDC1519_Normal.csv")

EZF_MDC_Normal_20_NAs <- left_join(EZF_Weather_NormalOnly, 
                                   EZF_MDC_20,
                                   by = c("Date_Update" = "Date"))

EZF_MDC20_Normal <- EZF_MDC_Normal_20_NAs[!is.na(EZF_MDC_Normal_20_NAs$MDC0ACME),]

write.csv(EZF_MDC20_Normal, "EZF_MDC20_Normal.csv")



#IAD

IAD_Final_Weather <- read_excel("~/Desktop/IAD.Final.Weather.xlsx")
IAD_Final_Weather  <- IAD_Final_Weather[1:5844,]
IAD_Final_Weather$Date_Update <- format(as.Date(IAD_Final_Weather$Date), "%Y-%m-%d")

IAD_Final_Weather$MinTLoop <- IAD_Final_Weather$`MinT(C)`
IAD_Final_Weather$MinTLoop[is.na(IAD_Final_Weather$MinTLoop)] <- 10

IAD_Final_Weather$MinTColdLag00 <- NA
IAD_Final_Weather$MinTColdLag01 <- NA
IAD_Final_Weather$MinTColdLag02 <- NA
IAD_Final_Weather$MinTColdLag03 <- NA
IAD_Final_Weather$MinTColdLag04 <- NA
IAD_Final_Weather$MinTColdLag05 <- NA
IAD_Final_Weather$MinTColdLag06 <- NA
IAD_Final_Weather$MinTColdLag07 <- NA


for (i in (1:5844)) {
  print(i)
  if (IAD_Final_Weather$MinTLoop[i] <= quantile(IAD_Final_Weather$`MinT(C)`, probs = .01,na.rm = T)) {
    IAD_Final_Weather$MinTColdLag00[i] = 1
    IAD_Final_Weather$MinTColdLag01[i+1] = 1
    IAD_Final_Weather$MinTColdLag02[i+2] = 1
    IAD_Final_Weather$MinTColdLag03[i+3] = 1
    IAD_Final_Weather$MinTColdLag04[i+4] = 1
    IAD_Final_Weather$MinTColdLag05[i+5] = 1
    IAD_Final_Weather$MinTColdLag06[i+6] = 1
    IAD_Final_Weather$MinTColdLag07[i+7] = 1
  }
}

IAD_Final_Weather$MinTColdLag00[is.na(IAD_Final_Weather$MinTColdLag00)] <- 0
IAD_Final_Weather$MinTColdLag01[is.na(IAD_Final_Weather$MinTColdLag01)] <- 0
IAD_Final_Weather$MinTColdLag02[is.na(IAD_Final_Weather$MinTColdLag02)] <- 0
IAD_Final_Weather$MinTColdLag03[is.na(IAD_Final_Weather$MinTColdLag03)] <- 0
IAD_Final_Weather$MinTColdLag04[is.na(IAD_Final_Weather$MinTColdLag04)] <- 0
IAD_Final_Weather$MinTColdLag05[is.na(IAD_Final_Weather$MinTColdLag05)] <- 0
IAD_Final_Weather$MinTColdLag06[is.na(IAD_Final_Weather$MinTColdLag06)] <- 0
IAD_Final_Weather$MinTColdLag07[is.na(IAD_Final_Weather$MinTColdLag07)] <- 0


IAD_Final_Weather$MinTWarmLag00 <- NA
IAD_Final_Weather$MinTWarmLag01 <- NA
IAD_Final_Weather$MinTWarmLag02 <- NA

for (i in (1:5844)) {
  print(i)
  if (IAD_Final_Weather$MinTLoop[i] >= quantile(IAD_Final_Weather$`MinT(C)`, probs = .99,na.rm = T)) {
    IAD_Final_Weather$MinTWarmLag00[i] = 1
    IAD_Final_Weather$MinTWarmLag01[i+1] = 1
    IAD_Final_Weather$MinTWarmLag02[i+2] = 1
  }
}

IAD_Final_Weather$MinTWarmLag00[is.na(IAD_Final_Weather$MinTWarmLag00)] <- 0
IAD_Final_Weather$MinTWarmLag01[is.na(IAD_Final_Weather$MinTWarmLag01)] <- 0
IAD_Final_Weather$MinTWarmLag02[is.na(IAD_Final_Weather$MinTWarmLag02)] <- 0

IAD_Final_Weather$KeepTheseDaysCold <- NA

for (i in (1:5844)) {
  print(i)
  if (IAD_Final_Weather$MinTColdLag00[i] == 1 | IAD_Final_Weather$MinTColdLag01[i] == 1 | IAD_Final_Weather$MinTColdLag02[i] == 1 | IAD_Final_Weather$MinTColdLag03[i] == 1 | IAD_Final_Weather$MinTColdLag04[i] == 1 | IAD_Final_Weather$MinTColdLag05[i] == 1 | IAD_Final_Weather$MinTColdLag06[i] == 1 | IAD_Final_Weather$MinTColdLag07[i] == 1) {
    IAD_Final_Weather$KeepTheseDaysCold[i] = 1
  } else {
    IAD_Final_Weather$KeepTheseDaysCold[i] = 0
  }
}

IAD_Final_Weather$KeepTheseDaysWarm <- NA

for (i in (1:5844)) {
  print(i)
  if (IAD_Final_Weather$MinTWarmLag00[i] == 1 | IAD_Final_Weather$MinTWarmLag01[i] == 1 | IAD_Final_Weather$MinTWarmLag02[i] == 1) {
    IAD_Final_Weather$KeepTheseDaysWarm[i] = 1
  } else {
    IAD_Final_Weather$KeepTheseDaysWarm[i] = 0
  }
}

IAD_Final_Weather$KeepTheseDaysNoStress <- NA
IAD_Final_Weather$KeepTheseDaysNoStress <- ifelse(IAD_Final_Weather$KeepTheseDaysCold == 1 | IAD_Final_Weather$KeepTheseDaysWarm == 1, 0, 1)

IAD_Weather_ColdOnly <- IAD_Final_Weather[(IAD_Final_Weather$KeepTheseDaysCold == 1), ]
IAD_Weather_WarmOnly <- IAD_Final_Weather[(IAD_Final_Weather$KeepTheseDaysWarm == 1), ]
IAD_Weather_NormalOnly <- IAD_Final_Weather[(IAD_Final_Weather$KeepTheseDaysNoStress == 1), ]

IAD_MDC_0514 <- MDC0514 %>%
  filter(ID == "IAD")

IAD_MDC_1519 <- MDC_Bad2020Removed %>%
  filter(ID == "IAD")

IAD_MDC_20 <- MDC20 %>%
  filter(ID == "IAD")

IAD_MDC_Cold_0514_NAs <- left_join(IAD_Weather_ColdOnly, 
                                   IAD_MDC_0514,
                                   by = c("Date_Update" = "Date"))

IAD_MDC0514_Cold <- IAD_MDC_Cold_0514_NAs[!is.na(IAD_MDC_Cold_0514_NAs$MDCAcme),]

write.csv(IAD_MDC0514_Cold, "IAD_MDC0514_Cold.csv")

IAD_MDC_Cold_1519_NAs <- left_join(IAD_Weather_ColdOnly, 
                                   IAD_MDC_1519,
                                   by = c("Date_Update" = "Date"))

IAD_MDC1519_Cold <- IAD_MDC_Cold_1519_NAs[!is.na(IAD_MDC_Cold_1519_NAs$MDCAcme),]

write.csv(IAD_MDC1519_Cold, "IAD_MDC1519_Cold.csv")

IAD_MDC_Cold_20_NAs <- left_join(IAD_Weather_ColdOnly, 
                                 IAD_MDC_20,
                                 by = c("Date_Update" = "Date"))

IAD_MDC20_Cold <- IAD_MDC_Cold_20_NAs[!is.na(IAD_MDC_Cold_20_NAs$MDC0ACME),]

write.csv(IAD_MDC20_Cold, "IAD_MDC20_Cold.csv")


IAD_MDC_Warm_0514_NAs <- left_join(IAD_Weather_WarmOnly, 
                                   IAD_MDC_0514,
                                   by = c("Date_Update" = "Date"))

IAD_MDC0514_Warm <- IAD_MDC_Warm_0514_NAs[!is.na(IAD_MDC_Warm_0514_NAs$MDCAcme),]

write.csv(IAD_MDC0514_Warm, "IAD_MDC0514_Warm.csv")

IAD_MDC_Warm_1519_NAs <- left_join(IAD_Weather_WarmOnly, 
                                   IAD_MDC_1519,
                                   by = c("Date_Update" = "Date"))

IAD_MDC1519_Warm <- IAD_MDC_Warm_1519_NAs[!is.na(IAD_MDC_Warm_1519_NAs$MDCAcme),]

write.csv(IAD_MDC1519_Warm, "IAD_MDC1519_Warm.csv")

IAD_MDC_Warm_20_NAs <- left_join(IAD_Weather_WarmOnly, 
                                 IAD_MDC_20,
                                 by = c("Date_Update" = "Date"))

IAD_MDC20_Warm <- IAD_MDC_Warm_20_NAs[!is.na(IAD_MDC_Warm_20_NAs$MDC0ACME),]

write.csv(IAD_MDC20_Warm, "IAD_MDC20_Warm.csv")


IAD_MDC_Normal_0514_NAs <- left_join(IAD_Weather_NormalOnly, 
                                     IAD_MDC_0514,
                                     by = c("Date_Update" = "Date"))

IAD_MDC0514_Normal <- IAD_MDC_Normal_0514_NAs[!is.na(IAD_MDC_Normal_0514_NAs$MDCAcme),]

write.csv(IAD_MDC0514_Normal, "IAD_MDC0514_Normal.csv")

IAD_MDC_Normal_1519_NAs <- left_join(IAD_Weather_NormalOnly, 
                                     IAD_MDC_1519,
                                     by = c("Date_Update" = "Date"))

IAD_MDC1519_Normal <- IAD_MDC_Normal_1519_NAs[!is.na(IAD_MDC_Normal_1519_NAs$MDCAcme),]

write.csv(IAD_MDC1519_Normal, "IAD_MDC1519_Normal.csv")

IAD_MDC_Normal_20_NAs <- left_join(IAD_Weather_NormalOnly, 
                                   IAD_MDC_20,
                                   by = c("Date_Update" = "Date"))

IAD_MDC20_Normal <- IAD_MDC_Normal_20_NAs[!is.na(IAD_MDC_Normal_20_NAs$MDC0ACME),]

write.csv(IAD_MDC20_Normal, "IAD_MDC20_Normal.csv")

#LYH

LYH_Final_Weather <- read_excel("~/Desktop/LYH.Final.Weather.xlsx")
LYH_Final_Weather  <- LYH_Final_Weather[1:5844,]
LYH_Final_Weather$Date_Update <- format(as.Date(LYH_Final_Weather$Date), "%Y-%m-%d")

LYH_Final_Weather$MinTLoop <- LYH_Final_Weather$`MinT(C)`
LYH_Final_Weather$MinTLoop[is.na(LYH_Final_Weather$MinTLoop)] <- 10

LYH_Final_Weather$MinTColdLag00 <- NA
LYH_Final_Weather$MinTColdLag01 <- NA
LYH_Final_Weather$MinTColdLag02 <- NA
LYH_Final_Weather$MinTColdLag03 <- NA
LYH_Final_Weather$MinTColdLag04 <- NA
LYH_Final_Weather$MinTColdLag05 <- NA
LYH_Final_Weather$MinTColdLag06 <- NA
LYH_Final_Weather$MinTColdLag07 <- NA


for (i in (1:5844)) {
  print(i)
  if (LYH_Final_Weather$MinTLoop[i] <= quantile(LYH_Final_Weather$`MinT(C)`, probs = .01,na.rm = T)) {
    LYH_Final_Weather$MinTColdLag00[i] = 1
    LYH_Final_Weather$MinTColdLag01[i+1] = 1
    LYH_Final_Weather$MinTColdLag02[i+2] = 1
    LYH_Final_Weather$MinTColdLag03[i+3] = 1
    LYH_Final_Weather$MinTColdLag04[i+4] = 1
    LYH_Final_Weather$MinTColdLag05[i+5] = 1
    LYH_Final_Weather$MinTColdLag06[i+6] = 1
    LYH_Final_Weather$MinTColdLag07[i+7] = 1
  }
}

LYH_Final_Weather$MinTColdLag00[is.na(LYH_Final_Weather$MinTColdLag00)] <- 0
LYH_Final_Weather$MinTColdLag01[is.na(LYH_Final_Weather$MinTColdLag01)] <- 0
LYH_Final_Weather$MinTColdLag02[is.na(LYH_Final_Weather$MinTColdLag02)] <- 0
LYH_Final_Weather$MinTColdLag03[is.na(LYH_Final_Weather$MinTColdLag03)] <- 0
LYH_Final_Weather$MinTColdLag04[is.na(LYH_Final_Weather$MinTColdLag04)] <- 0
LYH_Final_Weather$MinTColdLag05[is.na(LYH_Final_Weather$MinTColdLag05)] <- 0
LYH_Final_Weather$MinTColdLag06[is.na(LYH_Final_Weather$MinTColdLag06)] <- 0
LYH_Final_Weather$MinTColdLag07[is.na(LYH_Final_Weather$MinTColdLag07)] <- 0


LYH_Final_Weather$MinTWarmLag00 <- NA
LYH_Final_Weather$MinTWarmLag01 <- NA
LYH_Final_Weather$MinTWarmLag02 <- NA

for (i in (1:5844)) {
  print(i)
  if (LYH_Final_Weather$MinTLoop[i] >= quantile(LYH_Final_Weather$`MinT(C)`, probs = .99,na.rm = T)) {
    LYH_Final_Weather$MinTWarmLag00[i] = 1
    LYH_Final_Weather$MinTWarmLag01[i+1] = 1
    LYH_Final_Weather$MinTWarmLag02[i+2] = 1
  }
}

LYH_Final_Weather$MinTWarmLag00[is.na(LYH_Final_Weather$MinTWarmLag00)] <- 0
LYH_Final_Weather$MinTWarmLag01[is.na(LYH_Final_Weather$MinTWarmLag01)] <- 0
LYH_Final_Weather$MinTWarmLag02[is.na(LYH_Final_Weather$MinTWarmLag02)] <- 0

LYH_Final_Weather$KeepTheseDaysCold <- NA

for (i in (1:5844)) {
  print(i)
  if (LYH_Final_Weather$MinTColdLag00[i] == 1 | LYH_Final_Weather$MinTColdLag01[i] == 1 | LYH_Final_Weather$MinTColdLag02[i] == 1 | LYH_Final_Weather$MinTColdLag03[i] == 1 | LYH_Final_Weather$MinTColdLag04[i] == 1 | LYH_Final_Weather$MinTColdLag05[i] == 1 | LYH_Final_Weather$MinTColdLag06[i] == 1 | LYH_Final_Weather$MinTColdLag07[i] == 1) {
    LYH_Final_Weather$KeepTheseDaysCold[i] = 1
  } else {
    LYH_Final_Weather$KeepTheseDaysCold[i] = 0
  }
}

LYH_Final_Weather$KeepTheseDaysWarm <- NA

for (i in (1:5844)) {
  print(i)
  if (LYH_Final_Weather$MinTWarmLag00[i] == 1 | LYH_Final_Weather$MinTWarmLag01[i] == 1 | LYH_Final_Weather$MinTWarmLag02[i] == 1) {
    LYH_Final_Weather$KeepTheseDaysWarm[i] = 1
  } else {
    LYH_Final_Weather$KeepTheseDaysWarm[i] = 0
  }
}

LYH_Final_Weather$KeepTheseDaysNoStress <- NA
LYH_Final_Weather$KeepTheseDaysNoStress <- ifelse(LYH_Final_Weather$KeepTheseDaysCold == 1 | LYH_Final_Weather$KeepTheseDaysWarm == 1, 0, 1)

LYH_Weather_ColdOnly <- LYH_Final_Weather[(LYH_Final_Weather$KeepTheseDaysCold == 1), ]
LYH_Weather_WarmOnly <- LYH_Final_Weather[(LYH_Final_Weather$KeepTheseDaysWarm == 1), ]
LYH_Weather_NormalOnly <- LYH_Final_Weather[(LYH_Final_Weather$KeepTheseDaysNoStress == 1), ]

LYH_MDC_0514 <- MDC0514 %>%
  filter(ID == "LYH")

LYH_MDC_1519 <- MDC_Bad2020Removed %>%
  filter(ID == "LYH")

LYH_MDC_20 <- MDC20 %>%
  filter(ID == "LYH")

LYH_MDC_Cold_0514_NAs <- left_join(LYH_Weather_ColdOnly, 
                                   LYH_MDC_0514,
                                   by = c("Date_Update" = "Date"))

LYH_MDC0514_Cold <- LYH_MDC_Cold_0514_NAs[!is.na(LYH_MDC_Cold_0514_NAs$MDCAcme),]

write.csv(LYH_MDC0514_Cold, "LYH_MDC0514_Cold.csv")

LYH_MDC_Cold_1519_NAs <- left_join(LYH_Weather_ColdOnly, 
                                   LYH_MDC_1519,
                                   by = c("Date_Update" = "Date"))

LYH_MDC1519_Cold <- LYH_MDC_Cold_1519_NAs[!is.na(LYH_MDC_Cold_1519_NAs$MDCAcme),]

write.csv(LYH_MDC1519_Cold, "LYH_MDC1519_Cold.csv")

LYH_MDC_Cold_20_NAs <- left_join(LYH_Weather_ColdOnly, 
                                 LYH_MDC_20,
                                 by = c("Date_Update" = "Date"))

LYH_MDC20_Cold <- LYH_MDC_Cold_20_NAs[!is.na(LYH_MDC_Cold_20_NAs$MDC0ACME),]

write.csv(LYH_MDC20_Cold, "LYH_MDC20_Cold.csv")


LYH_MDC_Warm_0514_NAs <- left_join(LYH_Weather_WarmOnly, 
                                   LYH_MDC_0514,
                                   by = c("Date_Update" = "Date"))

LYH_MDC0514_Warm <- LYH_MDC_Warm_0514_NAs[!is.na(LYH_MDC_Warm_0514_NAs$MDCAcme),]

write.csv(LYH_MDC0514_Warm, "LYH_MDC0514_Warm.csv")

LYH_MDC_Warm_1519_NAs <- left_join(LYH_Weather_WarmOnly, 
                                   LYH_MDC_1519,
                                   by = c("Date_Update" = "Date"))

LYH_MDC1519_Warm <- LYH_MDC_Warm_1519_NAs[!is.na(LYH_MDC_Warm_1519_NAs$MDCAcme),]

write.csv(LYH_MDC1519_Warm, "LYH_MDC1519_Warm.csv")

LYH_MDC_Warm_20_NAs <- left_join(LYH_Weather_WarmOnly, 
                                 LYH_MDC_20,
                                 by = c("Date_Update" = "Date"))

LYH_MDC20_Warm <- LYH_MDC_Warm_20_NAs[!is.na(LYH_MDC_Warm_20_NAs$MDC0ACME),]

write.csv(LYH_MDC20_Warm, "LYH_MDC20_Warm.csv")


LYH_MDC_Normal_0514_NAs <- left_join(LYH_Weather_NormalOnly, 
                                     LYH_MDC_0514,
                                     by = c("Date_Update" = "Date"))

LYH_MDC0514_Normal <- LYH_MDC_Normal_0514_NAs[!is.na(LYH_MDC_Normal_0514_NAs$MDCAcme),]

write.csv(LYH_MDC0514_Normal, "LYH_MDC0514_Normal.csv")

LYH_MDC_Normal_1519_NAs <- left_join(LYH_Weather_NormalOnly, 
                                     LYH_MDC_1519,
                                     by = c("Date_Update" = "Date"))

LYH_MDC1519_Normal <- LYH_MDC_Normal_1519_NAs[!is.na(LYH_MDC_Normal_1519_NAs$MDCAcme),]

write.csv(LYH_MDC1519_Normal, "LYH_MDC1519_Normal.csv")

LYH_MDC_Normal_20_NAs <- left_join(LYH_Weather_NormalOnly, 
                                   LYH_MDC_20,
                                   by = c("Date_Update" = "Date"))

LYH_MDC20_Normal <- LYH_MDC_Normal_20_NAs[!is.na(LYH_MDC_Normal_20_NAs$MDC0ACME),]

write.csv(LYH_MDC20_Normal, "LYH_MDC20_Normal.csv")

#LYH

LYH_Final_Weather <- read_excel("~/Desktop/LYH.Final.Weather.xlsx")
LYH_Final_Weather  <- LYH_Final_Weather[1:5844,]
LYH_Final_Weather$Date_Update <- format(as.Date(LYH_Final_Weather$Date), "%Y-%m-%d")

LYH_Final_Weather$MinTLoop <- LYH_Final_Weather$`MinT(C)`
LYH_Final_Weather$MinTLoop[is.na(LYH_Final_Weather$MinTLoop)] <- 10

LYH_Final_Weather$MinTColdLag00 <- NA
LYH_Final_Weather$MinTColdLag01 <- NA
LYH_Final_Weather$MinTColdLag02 <- NA
LYH_Final_Weather$MinTColdLag03 <- NA
LYH_Final_Weather$MinTColdLag04 <- NA
LYH_Final_Weather$MinTColdLag05 <- NA
LYH_Final_Weather$MinTColdLag06 <- NA
LYH_Final_Weather$MinTColdLag07 <- NA


for (i in (1:5844)) {
  print(i)
  if (LYH_Final_Weather$MinTLoop[i] <= quantile(LYH_Final_Weather$`MinT(C)`, probs = .01,na.rm = T)) {
    LYH_Final_Weather$MinTColdLag00[i] = 1
    LYH_Final_Weather$MinTColdLag01[i+1] = 1
    LYH_Final_Weather$MinTColdLag02[i+2] = 1
    LYH_Final_Weather$MinTColdLag03[i+3] = 1
    LYH_Final_Weather$MinTColdLag04[i+4] = 1
    LYH_Final_Weather$MinTColdLag05[i+5] = 1
    LYH_Final_Weather$MinTColdLag06[i+6] = 1
    LYH_Final_Weather$MinTColdLag07[i+7] = 1
  }
}

LYH_Final_Weather$MinTColdLag00[is.na(LYH_Final_Weather$MinTColdLag00)] <- 0
LYH_Final_Weather$MinTColdLag01[is.na(LYH_Final_Weather$MinTColdLag01)] <- 0
LYH_Final_Weather$MinTColdLag02[is.na(LYH_Final_Weather$MinTColdLag02)] <- 0
LYH_Final_Weather$MinTColdLag03[is.na(LYH_Final_Weather$MinTColdLag03)] <- 0
LYH_Final_Weather$MinTColdLag04[is.na(LYH_Final_Weather$MinTColdLag04)] <- 0
LYH_Final_Weather$MinTColdLag05[is.na(LYH_Final_Weather$MinTColdLag05)] <- 0
LYH_Final_Weather$MinTColdLag06[is.na(LYH_Final_Weather$MinTColdLag06)] <- 0
LYH_Final_Weather$MinTColdLag07[is.na(LYH_Final_Weather$MinTColdLag07)] <- 0


LYH_Final_Weather$MinTWarmLag00 <- NA
LYH_Final_Weather$MinTWarmLag01 <- NA
LYH_Final_Weather$MinTWarmLag02 <- NA

for (i in (1:5844)) {
  print(i)
  if (LYH_Final_Weather$MinTLoop[i] >= quantile(LYH_Final_Weather$`MinT(C)`, probs = .99,na.rm = T)) {
    LYH_Final_Weather$MinTWarmLag00[i] = 1
    LYH_Final_Weather$MinTWarmLag01[i+1] = 1
    LYH_Final_Weather$MinTWarmLag02[i+2] = 1
  }
}

LYH_Final_Weather$MinTWarmLag00[is.na(LYH_Final_Weather$MinTWarmLag00)] <- 0
LYH_Final_Weather$MinTWarmLag01[is.na(LYH_Final_Weather$MinTWarmLag01)] <- 0
LYH_Final_Weather$MinTWarmLag02[is.na(LYH_Final_Weather$MinTWarmLag02)] <- 0

LYH_Final_Weather$KeepTheseDaysCold <- NA

for (i in (1:5844)) {
  print(i)
  if (LYH_Final_Weather$MinTColdLag00[i] == 1 | LYH_Final_Weather$MinTColdLag01[i] == 1 | LYH_Final_Weather$MinTColdLag02[i] == 1 | LYH_Final_Weather$MinTColdLag03[i] == 1 | LYH_Final_Weather$MinTColdLag04[i] == 1 | LYH_Final_Weather$MinTColdLag05[i] == 1 | LYH_Final_Weather$MinTColdLag06[i] == 1 | LYH_Final_Weather$MinTColdLag07[i] == 1) {
    LYH_Final_Weather$KeepTheseDaysCold[i] = 1
  } else {
    LYH_Final_Weather$KeepTheseDaysCold[i] = 0
  }
}

LYH_Final_Weather$KeepTheseDaysWarm <- NA

for (i in (1:5844)) {
  print(i)
  if (LYH_Final_Weather$MinTWarmLag00[i] == 1 | LYH_Final_Weather$MinTWarmLag01[i] == 1 | LYH_Final_Weather$MinTWarmLag02[i] == 1) {
    LYH_Final_Weather$KeepTheseDaysWarm[i] = 1
  } else {
    LYH_Final_Weather$KeepTheseDaysWarm[i] = 0
  }
}

LYH_Final_Weather$KeepTheseDaysNoStress <- NA
LYH_Final_Weather$KeepTheseDaysNoStress <- ifelse(LYH_Final_Weather$KeepTheseDaysCold == 1 | LYH_Final_Weather$KeepTheseDaysWarm == 1, 0, 1)

LYH_Weather_ColdOnly <- LYH_Final_Weather[(LYH_Final_Weather$KeepTheseDaysCold == 1), ]
LYH_Weather_WarmOnly <- LYH_Final_Weather[(LYH_Final_Weather$KeepTheseDaysWarm == 1), ]
LYH_Weather_NormalOnly <- LYH_Final_Weather[(LYH_Final_Weather$KeepTheseDaysNoStress == 1), ]

LYH_MDC_0514 <- MDC0514 %>%
  filter(ID == "LYH")

LYH_MDC_1519 <- MDC_Bad2020Removed %>%
  filter(ID == "LYH")

LYH_MDC_20 <- MDC20 %>%
  filter(ID == "LYH")

LYH_MDC_Cold_0514_NAs <- left_join(LYH_Weather_ColdOnly, 
                                   LYH_MDC_0514,
                                   by = c("Date_Update" = "Date"))

LYH_MDC0514_Cold <- LYH_MDC_Cold_0514_NAs[!is.na(LYH_MDC_Cold_0514_NAs$MDCAcme),]

write.csv(LYH_MDC0514_Cold, "LYH_MDC0514_Cold.csv")

LYH_MDC_Cold_1519_NAs <- left_join(LYH_Weather_ColdOnly, 
                                   LYH_MDC_1519,
                                   by = c("Date_Update" = "Date"))

LYH_MDC1519_Cold <- LYH_MDC_Cold_1519_NAs[!is.na(LYH_MDC_Cold_1519_NAs$MDCAcme),]

write.csv(LYH_MDC1519_Cold, "LYH_MDC1519_Cold.csv")

LYH_MDC_Cold_20_NAs <- left_join(LYH_Weather_ColdOnly, 
                                 LYH_MDC_20,
                                 by = c("Date_Update" = "Date"))

LYH_MDC20_Cold <- LYH_MDC_Cold_20_NAs[!is.na(LYH_MDC_Cold_20_NAs$MDC0ACME),]

write.csv(LYH_MDC20_Cold, "LYH_MDC20_Cold.csv")


LYH_MDC_Warm_0514_NAs <- left_join(LYH_Weather_WarmOnly, 
                                   LYH_MDC_0514,
                                   by = c("Date_Update" = "Date"))

LYH_MDC0514_Warm <- LYH_MDC_Warm_0514_NAs[!is.na(LYH_MDC_Warm_0514_NAs$MDCAcme),]

write.csv(LYH_MDC0514_Warm, "LYH_MDC0514_Warm.csv")

LYH_MDC_Warm_1519_NAs <- left_join(LYH_Weather_WarmOnly, 
                                   LYH_MDC_1519,
                                   by = c("Date_Update" = "Date"))

LYH_MDC1519_Warm <- LYH_MDC_Warm_1519_NAs[!is.na(LYH_MDC_Warm_1519_NAs$MDCAcme),]

write.csv(LYH_MDC1519_Warm, "LYH_MDC1519_Warm.csv")

LYH_MDC_Warm_20_NAs <- left_join(LYH_Weather_WarmOnly, 
                                 LYH_MDC_20,
                                 by = c("Date_Update" = "Date"))

LYH_MDC20_Warm <- LYH_MDC_Warm_20_NAs[!is.na(LYH_MDC_Warm_20_NAs$MDC0ACME),]

write.csv(LYH_MDC20_Warm, "LYH_MDC20_Warm.csv")


LYH_MDC_Normal_0514_NAs <- left_join(LYH_Weather_NormalOnly, 
                                     LYH_MDC_0514,
                                     by = c("Date_Update" = "Date"))

LYH_MDC0514_Normal <- LYH_MDC_Normal_0514_NAs[!is.na(LYH_MDC_Normal_0514_NAs$MDCAcme),]

write.csv(LYH_MDC0514_Normal, "LYH_MDC0514_Normal.csv")

LYH_MDC_Normal_1519_NAs <- left_join(LYH_Weather_NormalOnly, 
                                     LYH_MDC_1519,
                                     by = c("Date_Update" = "Date"))

LYH_MDC1519_Normal <- LYH_MDC_Normal_1519_NAs[!is.na(LYH_MDC_Normal_1519_NAs$MDCAcme),]

write.csv(LYH_MDC1519_Normal, "LYH_MDC1519_Normal.csv")

LYH_MDC_Normal_20_NAs <- left_join(LYH_Weather_NormalOnly, 
                                   LYH_MDC_20,
                                   by = c("Date_Update" = "Date"))

LYH_MDC20_Normal <- LYH_MDC_Normal_20_NAs[!is.na(LYH_MDC_Normal_20_NAs$MDC0ACME),]

write.csv(LYH_MDC20_Normal, "LYH_MDC20_Normal.csv")

#OKV

OKV_Final_Weather <- read_excel("~/Desktop/OKV.Final.Weather.xlsx")
OKV_Final_Weather  <- OKV_Final_Weather[1:5844,]
OKV_Final_Weather$Date_Update <- format(as.Date(OKV_Final_Weather$Date), "%Y-%m-%d")

OKV_Final_Weather$MinTLoop <- OKV_Final_Weather$`MinT(C)`
OKV_Final_Weather$MinTLoop[is.na(OKV_Final_Weather$MinTLoop)] <- 10

OKV_Final_Weather$MinTColdLag00 <- NA
OKV_Final_Weather$MinTColdLag01 <- NA
OKV_Final_Weather$MinTColdLag02 <- NA
OKV_Final_Weather$MinTColdLag03 <- NA
OKV_Final_Weather$MinTColdLag04 <- NA
OKV_Final_Weather$MinTColdLag05 <- NA
OKV_Final_Weather$MinTColdLag06 <- NA
OKV_Final_Weather$MinTColdLag07 <- NA


for (i in (1:5844)) {
  print(i)
  if (OKV_Final_Weather$MinTLoop[i] <= quantile(OKV_Final_Weather$`MinT(C)`, probs = .01,na.rm = T)) {
    OKV_Final_Weather$MinTColdLag00[i] = 1
    OKV_Final_Weather$MinTColdLag01[i+1] = 1
    OKV_Final_Weather$MinTColdLag02[i+2] = 1
    OKV_Final_Weather$MinTColdLag03[i+3] = 1
    OKV_Final_Weather$MinTColdLag04[i+4] = 1
    OKV_Final_Weather$MinTColdLag05[i+5] = 1
    OKV_Final_Weather$MinTColdLag06[i+6] = 1
    OKV_Final_Weather$MinTColdLag07[i+7] = 1
  }
}

OKV_Final_Weather$MinTColdLag00[is.na(OKV_Final_Weather$MinTColdLag00)] <- 0
OKV_Final_Weather$MinTColdLag01[is.na(OKV_Final_Weather$MinTColdLag01)] <- 0
OKV_Final_Weather$MinTColdLag02[is.na(OKV_Final_Weather$MinTColdLag02)] <- 0
OKV_Final_Weather$MinTColdLag03[is.na(OKV_Final_Weather$MinTColdLag03)] <- 0
OKV_Final_Weather$MinTColdLag04[is.na(OKV_Final_Weather$MinTColdLag04)] <- 0
OKV_Final_Weather$MinTColdLag05[is.na(OKV_Final_Weather$MinTColdLag05)] <- 0
OKV_Final_Weather$MinTColdLag06[is.na(OKV_Final_Weather$MinTColdLag06)] <- 0
OKV_Final_Weather$MinTColdLag07[is.na(OKV_Final_Weather$MinTColdLag07)] <- 0


OKV_Final_Weather$MinTWarmLag00 <- NA
OKV_Final_Weather$MinTWarmLag01 <- NA
OKV_Final_Weather$MinTWarmLag02 <- NA

for (i in (1:5844)) {
  print(i)
  if (OKV_Final_Weather$MinTLoop[i] >= quantile(OKV_Final_Weather$`MinT(C)`, probs = .99,na.rm = T)) {
    OKV_Final_Weather$MinTWarmLag00[i] = 1
    OKV_Final_Weather$MinTWarmLag01[i+1] = 1
    OKV_Final_Weather$MinTWarmLag02[i+2] = 1
  }
}

OKV_Final_Weather$MinTWarmLag00[is.na(OKV_Final_Weather$MinTWarmLag00)] <- 0
OKV_Final_Weather$MinTWarmLag01[is.na(OKV_Final_Weather$MinTWarmLag01)] <- 0
OKV_Final_Weather$MinTWarmLag02[is.na(OKV_Final_Weather$MinTWarmLag02)] <- 0

OKV_Final_Weather$KeepTheseDaysCold <- NA

for (i in (1:5844)) {
  print(i)
  if (OKV_Final_Weather$MinTColdLag00[i] == 1 | OKV_Final_Weather$MinTColdLag01[i] == 1 | OKV_Final_Weather$MinTColdLag02[i] == 1 | OKV_Final_Weather$MinTColdLag03[i] == 1 | OKV_Final_Weather$MinTColdLag04[i] == 1 | OKV_Final_Weather$MinTColdLag05[i] == 1 | OKV_Final_Weather$MinTColdLag06[i] == 1 | OKV_Final_Weather$MinTColdLag07[i] == 1) {
    OKV_Final_Weather$KeepTheseDaysCold[i] = 1
  } else {
    OKV_Final_Weather$KeepTheseDaysCold[i] = 0
  }
}

OKV_Final_Weather$KeepTheseDaysWarm <- NA

for (i in (1:5844)) {
  print(i)
  if (OKV_Final_Weather$MinTWarmLag00[i] == 1 | OKV_Final_Weather$MinTWarmLag01[i] == 1 | OKV_Final_Weather$MinTWarmLag02[i] == 1) {
    OKV_Final_Weather$KeepTheseDaysWarm[i] = 1
  } else {
    OKV_Final_Weather$KeepTheseDaysWarm[i] = 0
  }
}

OKV_Final_Weather$KeepTheseDaysNoStress <- NA
OKV_Final_Weather$KeepTheseDaysNoStress <- ifelse(OKV_Final_Weather$KeepTheseDaysCold == 1 | OKV_Final_Weather$KeepTheseDaysWarm == 1, 0, 1)

OKV_Weather_ColdOnly <- OKV_Final_Weather[(OKV_Final_Weather$KeepTheseDaysCold == 1), ]
OKV_Weather_WarmOnly <- OKV_Final_Weather[(OKV_Final_Weather$KeepTheseDaysWarm == 1), ]
OKV_Weather_NormalOnly <- OKV_Final_Weather[(OKV_Final_Weather$KeepTheseDaysNoStress == 1), ]

OKV_MDC_0514 <- MDC0514 %>%
  filter(ID == "OKV")

OKV_MDC_1519 <- MDC_Bad2020Removed %>%
  filter(ID == "OKV")

OKV_MDC_20 <- MDC20 %>%
  filter(ID == "OKV")

OKV_MDC_Cold_0514_NAs <- left_join(OKV_Weather_ColdOnly, 
                                   OKV_MDC_0514,
                                   by = c("Date_Update" = "Date"))

OKV_MDC0514_Cold <- OKV_MDC_Cold_0514_NAs[!is.na(OKV_MDC_Cold_0514_NAs$MDCAcme),]

write.csv(OKV_MDC0514_Cold, "OKV_MDC0514_Cold.csv")

OKV_MDC_Cold_1519_NAs <- left_join(OKV_Weather_ColdOnly, 
                                   OKV_MDC_1519,
                                   by = c("Date_Update" = "Date"))

OKV_MDC1519_Cold <- OKV_MDC_Cold_1519_NAs[!is.na(OKV_MDC_Cold_1519_NAs$MDCAcme),]

write.csv(OKV_MDC1519_Cold, "OKV_MDC1519_Cold.csv")

OKV_MDC_Cold_20_NAs <- left_join(OKV_Weather_ColdOnly, 
                                 OKV_MDC_20,
                                 by = c("Date_Update" = "Date"))

OKV_MDC20_Cold <- OKV_MDC_Cold_20_NAs[!is.na(OKV_MDC_Cold_20_NAs$MDC0ACME),]

write.csv(OKV_MDC20_Cold, "OKV_MDC20_Cold.csv")


OKV_MDC_Warm_0514_NAs <- left_join(OKV_Weather_WarmOnly, 
                                   OKV_MDC_0514,
                                   by = c("Date_Update" = "Date"))

OKV_MDC0514_Warm <- OKV_MDC_Warm_0514_NAs[!is.na(OKV_MDC_Warm_0514_NAs$MDCAcme),]

write.csv(OKV_MDC0514_Warm, "OKV_MDC0514_Warm.csv")

OKV_MDC_Warm_1519_NAs <- left_join(OKV_Weather_WarmOnly, 
                                   OKV_MDC_1519,
                                   by = c("Date_Update" = "Date"))

OKV_MDC1519_Warm <- OKV_MDC_Warm_1519_NAs[!is.na(OKV_MDC_Warm_1519_NAs$MDCAcme),]

write.csv(OKV_MDC1519_Warm, "OKV_MDC1519_Warm.csv")

OKV_MDC_Warm_20_NAs <- left_join(OKV_Weather_WarmOnly, 
                                 OKV_MDC_20,
                                 by = c("Date_Update" = "Date"))

OKV_MDC20_Warm <- OKV_MDC_Warm_20_NAs[!is.na(OKV_MDC_Warm_20_NAs$MDC0ACME),]

write.csv(OKV_MDC20_Warm, "OKV_MDC20_Warm.csv")


OKV_MDC_Normal_0514_NAs <- left_join(OKV_Weather_NormalOnly, 
                                     OKV_MDC_0514,
                                     by = c("Date_Update" = "Date"))

OKV_MDC0514_Normal <- OKV_MDC_Normal_0514_NAs[!is.na(OKV_MDC_Normal_0514_NAs$MDCAcme),]

write.csv(OKV_MDC0514_Normal, "OKV_MDC0514_Normal.csv")

OKV_MDC_Normal_1519_NAs <- left_join(OKV_Weather_NormalOnly, 
                                     OKV_MDC_1519,
                                     by = c("Date_Update" = "Date"))

OKV_MDC1519_Normal <- OKV_MDC_Normal_1519_NAs[!is.na(OKV_MDC_Normal_1519_NAs$MDCAcme),]

write.csv(OKV_MDC1519_Normal, "OKV_MDC1519_Normal.csv")

OKV_MDC_Normal_20_NAs <- left_join(OKV_Weather_NormalOnly, 
                                   OKV_MDC_20,
                                   by = c("Date_Update" = "Date"))

OKV_MDC20_Normal <- OKV_MDC_Normal_20_NAs[!is.na(OKV_MDC_Normal_20_NAs$MDC0ACME),]

write.csv(OKV_MDC20_Normal, "OKV_MDC20_Normal.csv")

#ORF

ORF_Final_Weather <- read_excel("~/Desktop/ORF.Final.Weather.xlsx")
ORF_Final_Weather  <- ORF_Final_Weather[1:5844,]
ORF_Final_Weather$Date_Update <- format(as.Date(ORF_Final_Weather$Date), "%Y-%m-%d")

ORF_Final_Weather$MinTLoop <- ORF_Final_Weather$`MinT(C)`
ORF_Final_Weather$MinTLoop[is.na(ORF_Final_Weather$MinTLoop)] <- 10

ORF_Final_Weather$MinTColdLag00 <- NA
ORF_Final_Weather$MinTColdLag01 <- NA
ORF_Final_Weather$MinTColdLag02 <- NA
ORF_Final_Weather$MinTColdLag03 <- NA
ORF_Final_Weather$MinTColdLag04 <- NA
ORF_Final_Weather$MinTColdLag05 <- NA
ORF_Final_Weather$MinTColdLag06 <- NA
ORF_Final_Weather$MinTColdLag07 <- NA


for (i in (1:5844)) {
  print(i)
  if (ORF_Final_Weather$MinTLoop[i] <= quantile(ORF_Final_Weather$`MinT(C)`, probs = .01,na.rm = T)) {
    ORF_Final_Weather$MinTColdLag00[i] = 1
    ORF_Final_Weather$MinTColdLag01[i+1] = 1
    ORF_Final_Weather$MinTColdLag02[i+2] = 1
    ORF_Final_Weather$MinTColdLag03[i+3] = 1
    ORF_Final_Weather$MinTColdLag04[i+4] = 1
    ORF_Final_Weather$MinTColdLag05[i+5] = 1
    ORF_Final_Weather$MinTColdLag06[i+6] = 1
    ORF_Final_Weather$MinTColdLag07[i+7] = 1
  }
}

ORF_Final_Weather$MinTColdLag00[is.na(ORF_Final_Weather$MinTColdLag00)] <- 0
ORF_Final_Weather$MinTColdLag01[is.na(ORF_Final_Weather$MinTColdLag01)] <- 0
ORF_Final_Weather$MinTColdLag02[is.na(ORF_Final_Weather$MinTColdLag02)] <- 0
ORF_Final_Weather$MinTColdLag03[is.na(ORF_Final_Weather$MinTColdLag03)] <- 0
ORF_Final_Weather$MinTColdLag04[is.na(ORF_Final_Weather$MinTColdLag04)] <- 0
ORF_Final_Weather$MinTColdLag05[is.na(ORF_Final_Weather$MinTColdLag05)] <- 0
ORF_Final_Weather$MinTColdLag06[is.na(ORF_Final_Weather$MinTColdLag06)] <- 0
ORF_Final_Weather$MinTColdLag07[is.na(ORF_Final_Weather$MinTColdLag07)] <- 0


ORF_Final_Weather$MinTWarmLag00 <- NA
ORF_Final_Weather$MinTWarmLag01 <- NA
ORF_Final_Weather$MinTWarmLag02 <- NA

for (i in (1:5844)) {
  print(i)
  if (ORF_Final_Weather$MinTLoop[i] >= quantile(ORF_Final_Weather$`MinT(C)`, probs = .99,na.rm = T)) {
    ORF_Final_Weather$MinTWarmLag00[i] = 1
    ORF_Final_Weather$MinTWarmLag01[i+1] = 1
    ORF_Final_Weather$MinTWarmLag02[i+2] = 1
  }
}

ORF_Final_Weather$MinTWarmLag00[is.na(ORF_Final_Weather$MinTWarmLag00)] <- 0
ORF_Final_Weather$MinTWarmLag01[is.na(ORF_Final_Weather$MinTWarmLag01)] <- 0
ORF_Final_Weather$MinTWarmLag02[is.na(ORF_Final_Weather$MinTWarmLag02)] <- 0

ORF_Final_Weather$KeepTheseDaysCold <- NA

for (i in (1:5844)) {
  print(i)
  if (ORF_Final_Weather$MinTColdLag00[i] == 1 | ORF_Final_Weather$MinTColdLag01[i] == 1 | ORF_Final_Weather$MinTColdLag02[i] == 1 | ORF_Final_Weather$MinTColdLag03[i] == 1 | ORF_Final_Weather$MinTColdLag04[i] == 1 | ORF_Final_Weather$MinTColdLag05[i] == 1 | ORF_Final_Weather$MinTColdLag06[i] == 1 | ORF_Final_Weather$MinTColdLag07[i] == 1) {
    ORF_Final_Weather$KeepTheseDaysCold[i] = 1
  } else {
    ORF_Final_Weather$KeepTheseDaysCold[i] = 0
  }
}

ORF_Final_Weather$KeepTheseDaysWarm <- NA

for (i in (1:5844)) {
  print(i)
  if (ORF_Final_Weather$MinTWarmLag00[i] == 1 | ORF_Final_Weather$MinTWarmLag01[i] == 1 | ORF_Final_Weather$MinTWarmLag02[i] == 1) {
    ORF_Final_Weather$KeepTheseDaysWarm[i] = 1
  } else {
    ORF_Final_Weather$KeepTheseDaysWarm[i] = 0
  }
}

ORF_Final_Weather$KeepTheseDaysNoStress <- NA
ORF_Final_Weather$KeepTheseDaysNoStress <- ifelse(ORF_Final_Weather$KeepTheseDaysCold == 1 | ORF_Final_Weather$KeepTheseDaysWarm == 1, 0, 1)

ORF_Weather_ColdOnly <- ORF_Final_Weather[(ORF_Final_Weather$KeepTheseDaysCold == 1), ]
ORF_Weather_WarmOnly <- ORF_Final_Weather[(ORF_Final_Weather$KeepTheseDaysWarm == 1), ]
ORF_Weather_NormalOnly <- ORF_Final_Weather[(ORF_Final_Weather$KeepTheseDaysNoStress == 1), ]

ORF_MDC_0514 <- MDC0514 %>%
  filter(ID == "ORF")

ORF_MDC_1519 <- MDC_Bad2020Removed %>%
  filter(ID == "ORF")

ORF_MDC_20 <- MDC20 %>%
  filter(ID == "ORF")

ORF_MDC_Cold_0514_NAs <- left_join(ORF_Weather_ColdOnly, 
                                   ORF_MDC_0514,
                                   by = c("Date_Update" = "Date"))

ORF_MDC0514_Cold <- ORF_MDC_Cold_0514_NAs[!is.na(ORF_MDC_Cold_0514_NAs$MDCAcme),]

write.csv(ORF_MDC0514_Cold, "ORF_MDC0514_Cold.csv")

ORF_MDC_Cold_1519_NAs <- left_join(ORF_Weather_ColdOnly, 
                                   ORF_MDC_1519,
                                   by = c("Date_Update" = "Date"))

ORF_MDC1519_Cold <- ORF_MDC_Cold_1519_NAs[!is.na(ORF_MDC_Cold_1519_NAs$MDCAcme),]

write.csv(ORF_MDC1519_Cold, "ORF_MDC1519_Cold.csv")

ORF_MDC_Cold_20_NAs <- left_join(ORF_Weather_ColdOnly, 
                                 ORF_MDC_20,
                                 by = c("Date_Update" = "Date"))

ORF_MDC20_Cold <- ORF_MDC_Cold_20_NAs[!is.na(ORF_MDC_Cold_20_NAs$MDC0ACME),]

write.csv(ORF_MDC20_Cold, "ORF_MDC20_Cold.csv")


ORF_MDC_Warm_0514_NAs <- left_join(ORF_Weather_WarmOnly, 
                                   ORF_MDC_0514,
                                   by = c("Date_Update" = "Date"))

ORF_MDC0514_Warm <- ORF_MDC_Warm_0514_NAs[!is.na(ORF_MDC_Warm_0514_NAs$MDCAcme),]

write.csv(ORF_MDC0514_Warm, "ORF_MDC0514_Warm.csv")

ORF_MDC_Warm_1519_NAs <- left_join(ORF_Weather_WarmOnly, 
                                   ORF_MDC_1519,
                                   by = c("Date_Update" = "Date"))

ORF_MDC1519_Warm <- ORF_MDC_Warm_1519_NAs[!is.na(ORF_MDC_Warm_1519_NAs$MDCAcme),]

write.csv(ORF_MDC1519_Warm, "ORF_MDC1519_Warm.csv")

ORF_MDC_Warm_20_NAs <- left_join(ORF_Weather_WarmOnly, 
                                 ORF_MDC_20,
                                 by = c("Date_Update" = "Date"))

ORF_MDC20_Warm <- ORF_MDC_Warm_20_NAs[!is.na(ORF_MDC_Warm_20_NAs$MDC0ACME),]

write.csv(ORF_MDC20_Warm, "ORF_MDC20_Warm.csv")


ORF_MDC_Normal_0514_NAs <- left_join(ORF_Weather_NormalOnly, 
                                     ORF_MDC_0514,
                                     by = c("Date_Update" = "Date"))

ORF_MDC0514_Normal <- ORF_MDC_Normal_0514_NAs[!is.na(ORF_MDC_Normal_0514_NAs$MDCAcme),]

write.csv(ORF_MDC0514_Normal, "ORF_MDC0514_Normal.csv")

ORF_MDC_Normal_1519_NAs <- left_join(ORF_Weather_NormalOnly, 
                                     ORF_MDC_1519,
                                     by = c("Date_Update" = "Date"))

ORF_MDC1519_Normal <- ORF_MDC_Normal_1519_NAs[!is.na(ORF_MDC_Normal_1519_NAs$MDCAcme),]

write.csv(ORF_MDC1519_Normal, "ORF_MDC1519_Normal.csv")

ORF_MDC_Normal_20_NAs <- left_join(ORF_Weather_NormalOnly, 
                                   ORF_MDC_20,
                                   by = c("Date_Update" = "Date"))

ORF_MDC20_Normal <- ORF_MDC_Normal_20_NAs[!is.na(ORF_MDC_Normal_20_NAs$MDC0ACME),]

write.csv(ORF_MDC20_Normal, "ORF_MDC20_Normal.csv")

#PHF

PHF_Final_Weather <- read_excel("~/Desktop/PHF.Final.Weather.xlsx")
PHF_Final_Weather  <- PHF_Final_Weather[1:5844,]
PHF_Final_Weather$Date_Update <- format(as.Date(PHF_Final_Weather$Date), "%Y-%m-%d")

PHF_Final_Weather$MinTLoop <- PHF_Final_Weather$`MinT(C)`
PHF_Final_Weather$MinTLoop[is.na(PHF_Final_Weather$MinTLoop)] <- 10

PHF_Final_Weather$MinTColdLag00 <- NA
PHF_Final_Weather$MinTColdLag01 <- NA
PHF_Final_Weather$MinTColdLag02 <- NA
PHF_Final_Weather$MinTColdLag03 <- NA
PHF_Final_Weather$MinTColdLag04 <- NA
PHF_Final_Weather$MinTColdLag05 <- NA
PHF_Final_Weather$MinTColdLag06 <- NA
PHF_Final_Weather$MinTColdLag07 <- NA


for (i in (1:5844)) {
  print(i)
  if (PHF_Final_Weather$MinTLoop[i] <= quantile(PHF_Final_Weather$`MinT(C)`, probs = .01,na.rm = T)) {
    PHF_Final_Weather$MinTColdLag00[i] = 1
    PHF_Final_Weather$MinTColdLag01[i+1] = 1
    PHF_Final_Weather$MinTColdLag02[i+2] = 1
    PHF_Final_Weather$MinTColdLag03[i+3] = 1
    PHF_Final_Weather$MinTColdLag04[i+4] = 1
    PHF_Final_Weather$MinTColdLag05[i+5] = 1
    PHF_Final_Weather$MinTColdLag06[i+6] = 1
    PHF_Final_Weather$MinTColdLag07[i+7] = 1
  }
}

PHF_Final_Weather$MinTColdLag00[is.na(PHF_Final_Weather$MinTColdLag00)] <- 0
PHF_Final_Weather$MinTColdLag01[is.na(PHF_Final_Weather$MinTColdLag01)] <- 0
PHF_Final_Weather$MinTColdLag02[is.na(PHF_Final_Weather$MinTColdLag02)] <- 0
PHF_Final_Weather$MinTColdLag03[is.na(PHF_Final_Weather$MinTColdLag03)] <- 0
PHF_Final_Weather$MinTColdLag04[is.na(PHF_Final_Weather$MinTColdLag04)] <- 0
PHF_Final_Weather$MinTColdLag05[is.na(PHF_Final_Weather$MinTColdLag05)] <- 0
PHF_Final_Weather$MinTColdLag06[is.na(PHF_Final_Weather$MinTColdLag06)] <- 0
PHF_Final_Weather$MinTColdLag07[is.na(PHF_Final_Weather$MinTColdLag07)] <- 0


PHF_Final_Weather$MinTWarmLag00 <- NA
PHF_Final_Weather$MinTWarmLag01 <- NA
PHF_Final_Weather$MinTWarmLag02 <- NA

for (i in (1:5844)) {
  print(i)
  if (PHF_Final_Weather$MinTLoop[i] >= quantile(PHF_Final_Weather$`MinT(C)`, probs = .99,na.rm = T)) {
    PHF_Final_Weather$MinTWarmLag00[i] = 1
    PHF_Final_Weather$MinTWarmLag01[i+1] = 1
    PHF_Final_Weather$MinTWarmLag02[i+2] = 1
  }
}

PHF_Final_Weather$MinTWarmLag00[is.na(PHF_Final_Weather$MinTWarmLag00)] <- 0
PHF_Final_Weather$MinTWarmLag01[is.na(PHF_Final_Weather$MinTWarmLag01)] <- 0
PHF_Final_Weather$MinTWarmLag02[is.na(PHF_Final_Weather$MinTWarmLag02)] <- 0

PHF_Final_Weather$KeepTheseDaysCold <- NA

for (i in (1:5844)) {
  print(i)
  if (PHF_Final_Weather$MinTColdLag00[i] == 1 | PHF_Final_Weather$MinTColdLag01[i] == 1 | PHF_Final_Weather$MinTColdLag02[i] == 1 | PHF_Final_Weather$MinTColdLag03[i] == 1 | PHF_Final_Weather$MinTColdLag04[i] == 1 | PHF_Final_Weather$MinTColdLag05[i] == 1 | PHF_Final_Weather$MinTColdLag06[i] == 1 | PHF_Final_Weather$MinTColdLag07[i] == 1) {
    PHF_Final_Weather$KeepTheseDaysCold[i] = 1
  } else {
    PHF_Final_Weather$KeepTheseDaysCold[i] = 0
  }
}

PHF_Final_Weather$KeepTheseDaysWarm <- NA

for (i in (1:5844)) {
  print(i)
  if (PHF_Final_Weather$MinTWarmLag00[i] == 1 | PHF_Final_Weather$MinTWarmLag01[i] == 1 | PHF_Final_Weather$MinTWarmLag02[i] == 1) {
    PHF_Final_Weather$KeepTheseDaysWarm[i] = 1
  } else {
    PHF_Final_Weather$KeepTheseDaysWarm[i] = 0
  }
}

PHF_Final_Weather$KeepTheseDaysNoStress <- NA
PHF_Final_Weather$KeepTheseDaysNoStress <- ifelse(PHF_Final_Weather$KeepTheseDaysCold == 1 | PHF_Final_Weather$KeepTheseDaysWarm == 1, 0, 1)

PHF_Weather_ColdOnly <- PHF_Final_Weather[(PHF_Final_Weather$KeepTheseDaysCold == 1), ]
PHF_Weather_WarmOnly <- PHF_Final_Weather[(PHF_Final_Weather$KeepTheseDaysWarm == 1), ]
PHF_Weather_NormalOnly <- PHF_Final_Weather[(PHF_Final_Weather$KeepTheseDaysNoStress == 1), ]

PHF_MDC_0514 <- MDC0514 %>%
  filter(ID == "PHF")

PHF_MDC_1519 <- MDC_Bad2020Removed %>%
  filter(ID == "PHF")

PHF_MDC_20 <- MDC20 %>%
  filter(ID == "PHF")

PHF_MDC_Cold_0514_NAs <- left_join(PHF_Weather_ColdOnly, 
                                   PHF_MDC_0514,
                                   by = c("Date_Update" = "Date"))

PHF_MDC0514_Cold <- PHF_MDC_Cold_0514_NAs[!is.na(PHF_MDC_Cold_0514_NAs$MDCAcme),]

write.csv(PHF_MDC0514_Cold, "PHF_MDC0514_Cold.csv")

PHF_MDC_Cold_1519_NAs <- left_join(PHF_Weather_ColdOnly, 
                                   PHF_MDC_1519,
                                   by = c("Date_Update" = "Date"))

PHF_MDC1519_Cold <- PHF_MDC_Cold_1519_NAs[!is.na(PHF_MDC_Cold_1519_NAs$MDCAcme),]

write.csv(PHF_MDC1519_Cold, "PHF_MDC1519_Cold.csv")

PHF_MDC_Cold_20_NAs <- left_join(PHF_Weather_ColdOnly, 
                                 PHF_MDC_20,
                                 by = c("Date_Update" = "Date"))

PHF_MDC20_Cold <- PHF_MDC_Cold_20_NAs[!is.na(PHF_MDC_Cold_20_NAs$MDC0ACME),]

write.csv(PHF_MDC20_Cold, "PHF_MDC20_Cold.csv")


PHF_MDC_Warm_0514_NAs <- left_join(PHF_Weather_WarmOnly, 
                                   PHF_MDC_0514,
                                   by = c("Date_Update" = "Date"))

PHF_MDC0514_Warm <- PHF_MDC_Warm_0514_NAs[!is.na(PHF_MDC_Warm_0514_NAs$MDCAcme),]

write.csv(PHF_MDC0514_Warm, "PHF_MDC0514_Warm.csv")

PHF_MDC_Warm_1519_NAs <- left_join(PHF_Weather_WarmOnly, 
                                   PHF_MDC_1519,
                                   by = c("Date_Update" = "Date"))

PHF_MDC1519_Warm <- PHF_MDC_Warm_1519_NAs[!is.na(PHF_MDC_Warm_1519_NAs$MDCAcme),]

write.csv(PHF_MDC1519_Warm, "PHF_MDC1519_Warm.csv")

PHF_MDC_Warm_20_NAs <- left_join(PHF_Weather_WarmOnly, 
                                 PHF_MDC_20,
                                 by = c("Date_Update" = "Date"))

PHF_MDC20_Warm <- PHF_MDC_Warm_20_NAs[!is.na(PHF_MDC_Warm_20_NAs$MDC0ACME),]

write.csv(PHF_MDC20_Warm, "PHF_MDC20_Warm.csv")


PHF_MDC_Normal_0514_NAs <- left_join(PHF_Weather_NormalOnly, 
                                     PHF_MDC_0514,
                                     by = c("Date_Update" = "Date"))

PHF_MDC0514_Normal <- PHF_MDC_Normal_0514_NAs[!is.na(PHF_MDC_Normal_0514_NAs$MDCAcme),]

write.csv(PHF_MDC0514_Normal, "PHF_MDC0514_Normal.csv")

PHF_MDC_Normal_1519_NAs <- left_join(PHF_Weather_NormalOnly, 
                                     PHF_MDC_1519,
                                     by = c("Date_Update" = "Date"))

PHF_MDC1519_Normal <- PHF_MDC_Normal_1519_NAs[!is.na(PHF_MDC_Normal_1519_NAs$MDCAcme),]

write.csv(PHF_MDC1519_Normal, "PHF_MDC1519_Normal.csv")

PHF_MDC_Normal_20_NAs <- left_join(PHF_Weather_NormalOnly, 
                                   PHF_MDC_20,
                                   by = c("Date_Update" = "Date"))

PHF_MDC20_Normal <- PHF_MDC_Normal_20_NAs[!is.na(PHF_MDC_Normal_20_NAs$MDC0ACME),]

write.csv(PHF_MDC20_Normal, "PHF_MDC20_Normal.csv")

#RIC

RIC_Final_Weather <- read_excel("~/Desktop/RIC.Final.Weather.xlsx")
RIC_Final_Weather  <- RIC_Final_Weather[1:5844,]
RIC_Final_Weather$Date_Update <- format(as.Date(RIC_Final_Weather$Date), "%Y-%m-%d")

RIC_Final_Weather$MinTLoop <- RIC_Final_Weather$`MinT(C)`
RIC_Final_Weather$MinTLoop[is.na(RIC_Final_Weather$MinTLoop)] <- 10

RIC_Final_Weather$MinTColdLag00 <- NA
RIC_Final_Weather$MinTColdLag01 <- NA
RIC_Final_Weather$MinTColdLag02 <- NA
RIC_Final_Weather$MinTColdLag03 <- NA
RIC_Final_Weather$MinTColdLag04 <- NA
RIC_Final_Weather$MinTColdLag05 <- NA
RIC_Final_Weather$MinTColdLag06 <- NA
RIC_Final_Weather$MinTColdLag07 <- NA


for (i in (1:5844)) {
  print(i)
  if (RIC_Final_Weather$MinTLoop[i] <= quantile(RIC_Final_Weather$`MinT(C)`, probs = .01,na.rm = T)) {
    RIC_Final_Weather$MinTColdLag00[i] = 1
    RIC_Final_Weather$MinTColdLag01[i+1] = 1
    RIC_Final_Weather$MinTColdLag02[i+2] = 1
    RIC_Final_Weather$MinTColdLag03[i+3] = 1
    RIC_Final_Weather$MinTColdLag04[i+4] = 1
    RIC_Final_Weather$MinTColdLag05[i+5] = 1
    RIC_Final_Weather$MinTColdLag06[i+6] = 1
    RIC_Final_Weather$MinTColdLag07[i+7] = 1
  }
}

RIC_Final_Weather$MinTColdLag00[is.na(RIC_Final_Weather$MinTColdLag00)] <- 0
RIC_Final_Weather$MinTColdLag01[is.na(RIC_Final_Weather$MinTColdLag01)] <- 0
RIC_Final_Weather$MinTColdLag02[is.na(RIC_Final_Weather$MinTColdLag02)] <- 0
RIC_Final_Weather$MinTColdLag03[is.na(RIC_Final_Weather$MinTColdLag03)] <- 0
RIC_Final_Weather$MinTColdLag04[is.na(RIC_Final_Weather$MinTColdLag04)] <- 0
RIC_Final_Weather$MinTColdLag05[is.na(RIC_Final_Weather$MinTColdLag05)] <- 0
RIC_Final_Weather$MinTColdLag06[is.na(RIC_Final_Weather$MinTColdLag06)] <- 0
RIC_Final_Weather$MinTColdLag07[is.na(RIC_Final_Weather$MinTColdLag07)] <- 0


RIC_Final_Weather$MinTWarmLag00 <- NA
RIC_Final_Weather$MinTWarmLag01 <- NA
RIC_Final_Weather$MinTWarmLag02 <- NA

for (i in (1:5844)) {
  print(i)
  if (RIC_Final_Weather$MinTLoop[i] >= quantile(RIC_Final_Weather$`MinT(C)`, probs = .99,na.rm = T)) {
    RIC_Final_Weather$MinTWarmLag00[i] = 1
    RIC_Final_Weather$MinTWarmLag01[i+1] = 1
    RIC_Final_Weather$MinTWarmLag02[i+2] = 1
  }
}

RIC_Final_Weather$MinTWarmLag00[is.na(RIC_Final_Weather$MinTWarmLag00)] <- 0
RIC_Final_Weather$MinTWarmLag01[is.na(RIC_Final_Weather$MinTWarmLag01)] <- 0
RIC_Final_Weather$MinTWarmLag02[is.na(RIC_Final_Weather$MinTWarmLag02)] <- 0

RIC_Final_Weather$KeepTheseDaysCold <- NA

for (i in (1:5844)) {
  print(i)
  if (RIC_Final_Weather$MinTColdLag00[i] == 1 | RIC_Final_Weather$MinTColdLag01[i] == 1 | RIC_Final_Weather$MinTColdLag02[i] == 1 | RIC_Final_Weather$MinTColdLag03[i] == 1 | RIC_Final_Weather$MinTColdLag04[i] == 1 | RIC_Final_Weather$MinTColdLag05[i] == 1 | RIC_Final_Weather$MinTColdLag06[i] == 1 | RIC_Final_Weather$MinTColdLag07[i] == 1) {
    RIC_Final_Weather$KeepTheseDaysCold[i] = 1
  } else {
    RIC_Final_Weather$KeepTheseDaysCold[i] = 0
  }
}

RIC_Final_Weather$KeepTheseDaysWarm <- NA

for (i in (1:5844)) {
  print(i)
  if (RIC_Final_Weather$MinTWarmLag00[i] == 1 | RIC_Final_Weather$MinTWarmLag01[i] == 1 | RIC_Final_Weather$MinTWarmLag02[i] == 1) {
    RIC_Final_Weather$KeepTheseDaysWarm[i] = 1
  } else {
    RIC_Final_Weather$KeepTheseDaysWarm[i] = 0
  }
}

RIC_Final_Weather$KeepTheseDaysNoStress <- NA
RIC_Final_Weather$KeepTheseDaysNoStress <- ifelse(RIC_Final_Weather$KeepTheseDaysCold == 1 | RIC_Final_Weather$KeepTheseDaysWarm == 1, 0, 1)

RIC_Weather_ColdOnly <- RIC_Final_Weather[(RIC_Final_Weather$KeepTheseDaysCold == 1), ]
RIC_Weather_WarmOnly <- RIC_Final_Weather[(RIC_Final_Weather$KeepTheseDaysWarm == 1), ]
RIC_Weather_NormalOnly <- RIC_Final_Weather[(RIC_Final_Weather$KeepTheseDaysNoStress == 1), ]

RIC_MDC_0514 <- MDC0514 %>%
  filter(ID == "RIC")

RIC_MDC_1519 <- MDC_Bad2020Removed %>%
  filter(ID == "RIC")

RIC_MDC_20 <- MDC20 %>%
  filter(ID == "RIC")

RIC_MDC_Cold_0514_NAs <- left_join(RIC_Weather_ColdOnly, 
                                   RIC_MDC_0514,
                                   by = c("Date_Update" = "Date"))

RIC_MDC0514_Cold <- RIC_MDC_Cold_0514_NAs[!is.na(RIC_MDC_Cold_0514_NAs$MDCAcme),]

write.csv(RIC_MDC0514_Cold, "RIC_MDC0514_Cold.csv")

RIC_MDC_Cold_1519_NAs <- left_join(RIC_Weather_ColdOnly, 
                                   RIC_MDC_1519,
                                   by = c("Date_Update" = "Date"))

RIC_MDC1519_Cold <- RIC_MDC_Cold_1519_NAs[!is.na(RIC_MDC_Cold_1519_NAs$MDCAcme),]

write.csv(RIC_MDC1519_Cold, "RIC_MDC1519_Cold.csv")

RIC_MDC_Cold_20_NAs <- left_join(RIC_Weather_ColdOnly, 
                                 RIC_MDC_20,
                                 by = c("Date_Update" = "Date"))

RIC_MDC20_Cold <- RIC_MDC_Cold_20_NAs[!is.na(RIC_MDC_Cold_20_NAs$MDC0ACME),]

write.csv(RIC_MDC20_Cold, "RIC_MDC20_Cold.csv")


RIC_MDC_Warm_0514_NAs <- left_join(RIC_Weather_WarmOnly, 
                                   RIC_MDC_0514,
                                   by = c("Date_Update" = "Date"))

RIC_MDC0514_Warm <- RIC_MDC_Warm_0514_NAs[!is.na(RIC_MDC_Warm_0514_NAs$MDCAcme),]

write.csv(RIC_MDC0514_Warm, "RIC_MDC0514_Warm.csv")

RIC_MDC_Warm_1519_NAs <- left_join(RIC_Weather_WarmOnly, 
                                   RIC_MDC_1519,
                                   by = c("Date_Update" = "Date"))

RIC_MDC1519_Warm <- RIC_MDC_Warm_1519_NAs[!is.na(RIC_MDC_Warm_1519_NAs$MDCAcme),]

write.csv(RIC_MDC1519_Warm, "RIC_MDC1519_Warm.csv")

RIC_MDC_Warm_20_NAs <- left_join(RIC_Weather_WarmOnly, 
                                 RIC_MDC_20,
                                 by = c("Date_Update" = "Date"))

RIC_MDC20_Warm <- RIC_MDC_Warm_20_NAs[!is.na(RIC_MDC_Warm_20_NAs$MDC0ACME),]

write.csv(RIC_MDC20_Warm, "RIC_MDC20_Warm.csv")


RIC_MDC_Normal_0514_NAs <- left_join(RIC_Weather_NormalOnly, 
                                     RIC_MDC_0514,
                                     by = c("Date_Update" = "Date"))

RIC_MDC0514_Normal <- RIC_MDC_Normal_0514_NAs[!is.na(RIC_MDC_Normal_0514_NAs$MDCAcme),]

write.csv(RIC_MDC0514_Normal, "RIC_MDC0514_Normal.csv")

RIC_MDC_Normal_1519_NAs <- left_join(RIC_Weather_NormalOnly, 
                                     RIC_MDC_1519,
                                     by = c("Date_Update" = "Date"))

RIC_MDC1519_Normal <- RIC_MDC_Normal_1519_NAs[!is.na(RIC_MDC_Normal_1519_NAs$MDCAcme),]

write.csv(RIC_MDC1519_Normal, "RIC_MDC1519_Normal.csv")

RIC_MDC_Normal_20_NAs <- left_join(RIC_Weather_NormalOnly, 
                                   RIC_MDC_20,
                                   by = c("Date_Update" = "Date"))

RIC_MDC20_Normal <- RIC_MDC_Normal_20_NAs[!is.na(RIC_MDC_Normal_20_NAs$MDC0ACME),]

write.csv(RIC_MDC20_Normal, "RIC_MDC20_Normal.csv")

#ROA

ROA_Final_Weather <- read_excel("~/Desktop/ROA.Final.Weather.xlsx")
ROA_Final_Weather  <- ROA_Final_Weather[1:5844,]
ROA_Final_Weather$Date_Update <- format(as.Date(ROA_Final_Weather$Date), "%Y-%m-%d")

ROA_Final_Weather$MinTLoop <- ROA_Final_Weather$`MinT(C)`
ROA_Final_Weather$MinTLoop[is.na(ROA_Final_Weather$MinTLoop)] <- 10

ROA_Final_Weather$MinTColdLag00 <- NA
ROA_Final_Weather$MinTColdLag01 <- NA
ROA_Final_Weather$MinTColdLag02 <- NA
ROA_Final_Weather$MinTColdLag03 <- NA
ROA_Final_Weather$MinTColdLag04 <- NA
ROA_Final_Weather$MinTColdLag05 <- NA
ROA_Final_Weather$MinTColdLag06 <- NA
ROA_Final_Weather$MinTColdLag07 <- NA


for (i in (1:5844)) {
  print(i)
  if (ROA_Final_Weather$MinTLoop[i] <= quantile(ROA_Final_Weather$`MinT(C)`, probs = .01,na.rm = T)) {
    ROA_Final_Weather$MinTColdLag00[i] = 1
    ROA_Final_Weather$MinTColdLag01[i+1] = 1
    ROA_Final_Weather$MinTColdLag02[i+2] = 1
    ROA_Final_Weather$MinTColdLag03[i+3] = 1
    ROA_Final_Weather$MinTColdLag04[i+4] = 1
    ROA_Final_Weather$MinTColdLag05[i+5] = 1
    ROA_Final_Weather$MinTColdLag06[i+6] = 1
    ROA_Final_Weather$MinTColdLag07[i+7] = 1
  }
}

ROA_Final_Weather$MinTColdLag00[is.na(ROA_Final_Weather$MinTColdLag00)] <- 0
ROA_Final_Weather$MinTColdLag01[is.na(ROA_Final_Weather$MinTColdLag01)] <- 0
ROA_Final_Weather$MinTColdLag02[is.na(ROA_Final_Weather$MinTColdLag02)] <- 0
ROA_Final_Weather$MinTColdLag03[is.na(ROA_Final_Weather$MinTColdLag03)] <- 0
ROA_Final_Weather$MinTColdLag04[is.na(ROA_Final_Weather$MinTColdLag04)] <- 0
ROA_Final_Weather$MinTColdLag05[is.na(ROA_Final_Weather$MinTColdLag05)] <- 0
ROA_Final_Weather$MinTColdLag06[is.na(ROA_Final_Weather$MinTColdLag06)] <- 0
ROA_Final_Weather$MinTColdLag07[is.na(ROA_Final_Weather$MinTColdLag07)] <- 0


ROA_Final_Weather$MinTWarmLag00 <- NA
ROA_Final_Weather$MinTWarmLag01 <- NA
ROA_Final_Weather$MinTWarmLag02 <- NA

for (i in (1:5844)) {
  print(i)
  if (ROA_Final_Weather$MinTLoop[i] >= quantile(ROA_Final_Weather$`MinT(C)`, probs = .99,na.rm = T)) {
    ROA_Final_Weather$MinTWarmLag00[i] = 1
    ROA_Final_Weather$MinTWarmLag01[i+1] = 1
    ROA_Final_Weather$MinTWarmLag02[i+2] = 1
  }
}

ROA_Final_Weather$MinTWarmLag00[is.na(ROA_Final_Weather$MinTWarmLag00)] <- 0
ROA_Final_Weather$MinTWarmLag01[is.na(ROA_Final_Weather$MinTWarmLag01)] <- 0
ROA_Final_Weather$MinTWarmLag02[is.na(ROA_Final_Weather$MinTWarmLag02)] <- 0

ROA_Final_Weather$KeepTheseDaysCold <- NA

for (i in (1:5844)) {
  print(i)
  if (ROA_Final_Weather$MinTColdLag00[i] == 1 | ROA_Final_Weather$MinTColdLag01[i] == 1 | ROA_Final_Weather$MinTColdLag02[i] == 1 | ROA_Final_Weather$MinTColdLag03[i] == 1 | ROA_Final_Weather$MinTColdLag04[i] == 1 | ROA_Final_Weather$MinTColdLag05[i] == 1 | ROA_Final_Weather$MinTColdLag06[i] == 1 | ROA_Final_Weather$MinTColdLag07[i] == 1) {
    ROA_Final_Weather$KeepTheseDaysCold[i] = 1
  } else {
    ROA_Final_Weather$KeepTheseDaysCold[i] = 0
  }
}

ROA_Final_Weather$KeepTheseDaysWarm <- NA

for (i in (1:5844)) {
  print(i)
  if (ROA_Final_Weather$MinTWarmLag00[i] == 1 | ROA_Final_Weather$MinTWarmLag01[i] == 1 | ROA_Final_Weather$MinTWarmLag02[i] == 1) {
    ROA_Final_Weather$KeepTheseDaysWarm[i] = 1
  } else {
    ROA_Final_Weather$KeepTheseDaysWarm[i] = 0
  }
}

ROA_Final_Weather$KeepTheseDaysNoStress <- NA
ROA_Final_Weather$KeepTheseDaysNoStress <- ifelse(ROA_Final_Weather$KeepTheseDaysCold == 1 | ROA_Final_Weather$KeepTheseDaysWarm == 1, 0, 1)

ROA_Weather_ColdOnly <- ROA_Final_Weather[(ROA_Final_Weather$KeepTheseDaysCold == 1), ]
ROA_Weather_WarmOnly <- ROA_Final_Weather[(ROA_Final_Weather$KeepTheseDaysWarm == 1), ]
ROA_Weather_NormalOnly <- ROA_Final_Weather[(ROA_Final_Weather$KeepTheseDaysNoStress == 1), ]

ROA_MDC_0514 <- MDC0514 %>%
  filter(ID == "ROA")

ROA_MDC_1519 <- MDC_Bad2020Removed %>%
  filter(ID == "ROA")

ROA_MDC_20 <- MDC20 %>%
  filter(ID == "ROA")

ROA_MDC_Cold_0514_NAs <- left_join(ROA_Weather_ColdOnly, 
                                   ROA_MDC_0514,
                                   by = c("Date_Update" = "Date"))

ROA_MDC0514_Cold <- ROA_MDC_Cold_0514_NAs[!is.na(ROA_MDC_Cold_0514_NAs$MDCAcme),]

write.csv(ROA_MDC0514_Cold, "ROA_MDC0514_Cold.csv")

ROA_MDC_Cold_1519_NAs <- left_join(ROA_Weather_ColdOnly, 
                                   ROA_MDC_1519,
                                   by = c("Date_Update" = "Date"))

ROA_MDC1519_Cold <- ROA_MDC_Cold_1519_NAs[!is.na(ROA_MDC_Cold_1519_NAs$MDCAcme),]

write.csv(ROA_MDC1519_Cold, "ROA_MDC1519_Cold.csv")

ROA_MDC_Cold_20_NAs <- left_join(ROA_Weather_ColdOnly, 
                                 ROA_MDC_20,
                                 by = c("Date_Update" = "Date"))

ROA_MDC20_Cold <- ROA_MDC_Cold_20_NAs[!is.na(ROA_MDC_Cold_20_NAs$MDC0ACME),]

write.csv(ROA_MDC20_Cold, "ROA_MDC20_Cold.csv")


ROA_MDC_Warm_0514_NAs <- left_join(ROA_Weather_WarmOnly, 
                                   ROA_MDC_0514,
                                   by = c("Date_Update" = "Date"))

ROA_MDC0514_Warm <- ROA_MDC_Warm_0514_NAs[!is.na(ROA_MDC_Warm_0514_NAs$MDCAcme),]

write.csv(ROA_MDC0514_Warm, "ROA_MDC0514_Warm.csv")

ROA_MDC_Warm_1519_NAs <- left_join(ROA_Weather_WarmOnly, 
                                   ROA_MDC_1519,
                                   by = c("Date_Update" = "Date"))

ROA_MDC1519_Warm <- ROA_MDC_Warm_1519_NAs[!is.na(ROA_MDC_Warm_1519_NAs$MDCAcme),]

write.csv(ROA_MDC1519_Warm, "ROA_MDC1519_Warm.csv")

ROA_MDC_Warm_20_NAs <- left_join(ROA_Weather_WarmOnly, 
                                 ROA_MDC_20,
                                 by = c("Date_Update" = "Date"))

ROA_MDC20_Warm <- ROA_MDC_Warm_20_NAs[!is.na(ROA_MDC_Warm_20_NAs$MDC0ACME),]

write.csv(ROA_MDC20_Warm, "ROA_MDC20_Warm.csv")


ROA_MDC_Normal_0514_NAs <- left_join(ROA_Weather_NormalOnly, 
                                     ROA_MDC_0514,
                                     by = c("Date_Update" = "Date"))

ROA_MDC0514_Normal <- ROA_MDC_Normal_0514_NAs[!is.na(ROA_MDC_Normal_0514_NAs$MDCAcme),]

write.csv(ROA_MDC0514_Normal, "ROA_MDC0514_Normal.csv")

ROA_MDC_Normal_1519_NAs <- left_join(ROA_Weather_NormalOnly, 
                                     ROA_MDC_1519,
                                     by = c("Date_Update" = "Date"))

ROA_MDC1519_Normal <- ROA_MDC_Normal_1519_NAs[!is.na(ROA_MDC_Normal_1519_NAs$MDCAcme),]

write.csv(ROA_MDC1519_Normal, "ROA_MDC1519_Normal.csv")

ROA_MDC_Normal_20_NAs <- left_join(ROA_Weather_NormalOnly, 
                                   ROA_MDC_20,
                                   by = c("Date_Update" = "Date"))

ROA_MDC20_Normal <- ROA_MDC_Normal_20_NAs[!is.na(ROA_MDC_Normal_20_NAs$MDC0ACME),]

write.csv(ROA_MDC20_Normal, "ROA_MDC20_Normal.csv")

#SHD

SHD_Final_Weather <- read_excel("~/Desktop/SHD.Final.Weather.xlsx")
SHD_Final_Weather  <- SHD_Final_Weather[1:5844,]
SHD_Final_Weather$Date_Update <- format(as.Date(SHD_Final_Weather$Date), "%Y-%m-%d")

SHD_Final_Weather$MinTLoop <- SHD_Final_Weather$`MinT(C)`
SHD_Final_Weather$MinTLoop[is.na(SHD_Final_Weather$MinTLoop)] <- 10

SHD_Final_Weather$MinTColdLag00 <- NA
SHD_Final_Weather$MinTColdLag01 <- NA
SHD_Final_Weather$MinTColdLag02 <- NA
SHD_Final_Weather$MinTColdLag03 <- NA
SHD_Final_Weather$MinTColdLag04 <- NA
SHD_Final_Weather$MinTColdLag05 <- NA
SHD_Final_Weather$MinTColdLag06 <- NA
SHD_Final_Weather$MinTColdLag07 <- NA


for (i in (1:5844)) {
  print(i)
  if (SHD_Final_Weather$MinTLoop[i] <= quantile(SHD_Final_Weather$`MinT(C)`, probs = .01,na.rm = T)) {
    SHD_Final_Weather$MinTColdLag00[i] = 1
    SHD_Final_Weather$MinTColdLag01[i+1] = 1
    SHD_Final_Weather$MinTColdLag02[i+2] = 1
    SHD_Final_Weather$MinTColdLag03[i+3] = 1
    SHD_Final_Weather$MinTColdLag04[i+4] = 1
    SHD_Final_Weather$MinTColdLag05[i+5] = 1
    SHD_Final_Weather$MinTColdLag06[i+6] = 1
    SHD_Final_Weather$MinTColdLag07[i+7] = 1
  }
}

SHD_Final_Weather$MinTColdLag00[is.na(SHD_Final_Weather$MinTColdLag00)] <- 0
SHD_Final_Weather$MinTColdLag01[is.na(SHD_Final_Weather$MinTColdLag01)] <- 0
SHD_Final_Weather$MinTColdLag02[is.na(SHD_Final_Weather$MinTColdLag02)] <- 0
SHD_Final_Weather$MinTColdLag03[is.na(SHD_Final_Weather$MinTColdLag03)] <- 0
SHD_Final_Weather$MinTColdLag04[is.na(SHD_Final_Weather$MinTColdLag04)] <- 0
SHD_Final_Weather$MinTColdLag05[is.na(SHD_Final_Weather$MinTColdLag05)] <- 0
SHD_Final_Weather$MinTColdLag06[is.na(SHD_Final_Weather$MinTColdLag06)] <- 0
SHD_Final_Weather$MinTColdLag07[is.na(SHD_Final_Weather$MinTColdLag07)] <- 0


SHD_Final_Weather$MinTWarmLag00 <- NA
SHD_Final_Weather$MinTWarmLag01 <- NA
SHD_Final_Weather$MinTWarmLag02 <- NA

for (i in (1:5844)) {
  print(i)
  if (SHD_Final_Weather$MinTLoop[i] >= quantile(SHD_Final_Weather$`MinT(C)`, probs = .99,na.rm = T)) {
    SHD_Final_Weather$MinTWarmLag00[i] = 1
    SHD_Final_Weather$MinTWarmLag01[i+1] = 1
    SHD_Final_Weather$MinTWarmLag02[i+2] = 1
  }
}

SHD_Final_Weather$MinTWarmLag00[is.na(SHD_Final_Weather$MinTWarmLag00)] <- 0
SHD_Final_Weather$MinTWarmLag01[is.na(SHD_Final_Weather$MinTWarmLag01)] <- 0
SHD_Final_Weather$MinTWarmLag02[is.na(SHD_Final_Weather$MinTWarmLag02)] <- 0

SHD_Final_Weather$KeepTheseDaysCold <- NA

for (i in (1:5844)) {
  print(i)
  if (SHD_Final_Weather$MinTColdLag00[i] == 1 | SHD_Final_Weather$MinTColdLag01[i] == 1 | SHD_Final_Weather$MinTColdLag02[i] == 1 | SHD_Final_Weather$MinTColdLag03[i] == 1 | SHD_Final_Weather$MinTColdLag04[i] == 1 | SHD_Final_Weather$MinTColdLag05[i] == 1 | SHD_Final_Weather$MinTColdLag06[i] == 1 | SHD_Final_Weather$MinTColdLag07[i] == 1) {
    SHD_Final_Weather$KeepTheseDaysCold[i] = 1
  } else {
    SHD_Final_Weather$KeepTheseDaysCold[i] = 0
  }
}

SHD_Final_Weather$KeepTheseDaysWarm <- NA

for (i in (1:5844)) {
  print(i)
  if (SHD_Final_Weather$MinTWarmLag00[i] == 1 | SHD_Final_Weather$MinTWarmLag01[i] == 1 | SHD_Final_Weather$MinTWarmLag02[i] == 1) {
    SHD_Final_Weather$KeepTheseDaysWarm[i] = 1
  } else {
    SHD_Final_Weather$KeepTheseDaysWarm[i] = 0
  }
}

SHD_Final_Weather$KeepTheseDaysNoStress <- NA
SHD_Final_Weather$KeepTheseDaysNoStress <- ifelse(SHD_Final_Weather$KeepTheseDaysCold == 1 | SHD_Final_Weather$KeepTheseDaysWarm == 1, 0, 1)

SHD_Weather_ColdOnly <- SHD_Final_Weather[(SHD_Final_Weather$KeepTheseDaysCold == 1), ]
SHD_Weather_WarmOnly <- SHD_Final_Weather[(SHD_Final_Weather$KeepTheseDaysWarm == 1), ]
SHD_Weather_NormalOnly <- SHD_Final_Weather[(SHD_Final_Weather$KeepTheseDaysNoStress == 1), ]

SHD_MDC_0514 <- MDC0514 %>%
  filter(ID == "SHD")

SHD_MDC_1519 <- MDC_Bad2020Removed %>%
  filter(ID == "SHD")

SHD_MDC_20 <- MDC20 %>%
  filter(ID == "SHD")

SHD_MDC_Cold_0514_NAs <- left_join(SHD_Weather_ColdOnly, 
                                   SHD_MDC_0514,
                                   by = c("Date_Update" = "Date"))

SHD_MDC0514_Cold <- SHD_MDC_Cold_0514_NAs[!is.na(SHD_MDC_Cold_0514_NAs$MDCAcme),]

write.csv(SHD_MDC0514_Cold, "SHD_MDC0514_Cold.csv")

SHD_MDC_Cold_1519_NAs <- left_join(SHD_Weather_ColdOnly, 
                                   SHD_MDC_1519,
                                   by = c("Date_Update" = "Date"))

SHD_MDC1519_Cold <- SHD_MDC_Cold_1519_NAs[!is.na(SHD_MDC_Cold_1519_NAs$MDCAcme),]

write.csv(SHD_MDC1519_Cold, "SHD_MDC1519_Cold.csv")

SHD_MDC_Cold_20_NAs <- left_join(SHD_Weather_ColdOnly, 
                                 SHD_MDC_20,
                                 by = c("Date_Update" = "Date"))

SHD_MDC20_Cold <- SHD_MDC_Cold_20_NAs[!is.na(SHD_MDC_Cold_20_NAs$MDC0ACME),]

write.csv(SHD_MDC20_Cold, "SHD_MDC20_Cold.csv")


SHD_MDC_Warm_0514_NAs <- left_join(SHD_Weather_WarmOnly, 
                                   SHD_MDC_0514,
                                   by = c("Date_Update" = "Date"))

SHD_MDC0514_Warm <- SHD_MDC_Warm_0514_NAs[!is.na(SHD_MDC_Warm_0514_NAs$MDCAcme),]

write.csv(SHD_MDC0514_Warm, "SHD_MDC0514_Warm.csv")

SHD_MDC_Warm_1519_NAs <- left_join(SHD_Weather_WarmOnly, 
                                   SHD_MDC_1519,
                                   by = c("Date_Update" = "Date"))

SHD_MDC1519_Warm <- SHD_MDC_Warm_1519_NAs[!is.na(SHD_MDC_Warm_1519_NAs$MDCAcme),]

write.csv(SHD_MDC1519_Warm, "SHD_MDC1519_Warm.csv")

SHD_MDC_Warm_20_NAs <- left_join(SHD_Weather_WarmOnly, 
                                 SHD_MDC_20,
                                 by = c("Date_Update" = "Date"))

SHD_MDC20_Warm <- SHD_MDC_Warm_20_NAs[!is.na(SHD_MDC_Warm_20_NAs$MDC0ACME),]

write.csv(SHD_MDC20_Warm, "SHD_MDC20_Warm.csv")


SHD_MDC_Normal_0514_NAs <- left_join(SHD_Weather_NormalOnly, 
                                     SHD_MDC_0514,
                                     by = c("Date_Update" = "Date"))

SHD_MDC0514_Normal <- SHD_MDC_Normal_0514_NAs[!is.na(SHD_MDC_Normal_0514_NAs$MDCAcme),]

write.csv(SHD_MDC0514_Normal, "SHD_MDC0514_Normal.csv")

SHD_MDC_Normal_1519_NAs <- left_join(SHD_Weather_NormalOnly, 
                                     SHD_MDC_1519,
                                     by = c("Date_Update" = "Date"))

SHD_MDC1519_Normal <- SHD_MDC_Normal_1519_NAs[!is.na(SHD_MDC_Normal_1519_NAs$MDCAcme),]

write.csv(SHD_MDC1519_Normal, "SHD_MDC1519_Normal.csv")

SHD_MDC_Normal_20_NAs <- left_join(SHD_Weather_NormalOnly, 
                                   SHD_MDC_20,
                                   by = c("Date_Update" = "Date"))

SHD_MDC20_Normal <- SHD_MDC_Normal_20_NAs[!is.na(SHD_MDC_Normal_20_NAs$MDC0ACME),]

write.csv(SHD_MDC20_Normal, "SHD_MDC20_Normal.csv")

#VJI

VJI_Final_Weather <- read_excel("~/Desktop/VJI.Final.Weather.xlsx")
VJI_Final_Weather  <- VJI_Final_Weather[1:5844,]
VJI_Final_Weather$Date_Update <- format(as.Date(VJI_Final_Weather$Date), "%Y-%m-%d")

VJI_Final_Weather$MinTLoop <- VJI_Final_Weather$`MinT(C)`
VJI_Final_Weather$MinTLoop[is.na(VJI_Final_Weather$MinTLoop)] <- 10

VJI_Final_Weather$MinTColdLag00 <- NA
VJI_Final_Weather$MinTColdLag01 <- NA
VJI_Final_Weather$MinTColdLag02 <- NA
VJI_Final_Weather$MinTColdLag03 <- NA
VJI_Final_Weather$MinTColdLag04 <- NA
VJI_Final_Weather$MinTColdLag05 <- NA
VJI_Final_Weather$MinTColdLag06 <- NA
VJI_Final_Weather$MinTColdLag07 <- NA


for (i in (1:5844)) {
  print(i)
  if (VJI_Final_Weather$MinTLoop[i] <= quantile(VJI_Final_Weather$`MinT(C)`, probs = .01,na.rm = T)) {
    VJI_Final_Weather$MinTColdLag00[i] = 1
    VJI_Final_Weather$MinTColdLag01[i+1] = 1
    VJI_Final_Weather$MinTColdLag02[i+2] = 1
    VJI_Final_Weather$MinTColdLag03[i+3] = 1
    VJI_Final_Weather$MinTColdLag04[i+4] = 1
    VJI_Final_Weather$MinTColdLag05[i+5] = 1
    VJI_Final_Weather$MinTColdLag06[i+6] = 1
    VJI_Final_Weather$MinTColdLag07[i+7] = 1
  }
}

VJI_Final_Weather$MinTColdLag00[is.na(VJI_Final_Weather$MinTColdLag00)] <- 0
VJI_Final_Weather$MinTColdLag01[is.na(VJI_Final_Weather$MinTColdLag01)] <- 0
VJI_Final_Weather$MinTColdLag02[is.na(VJI_Final_Weather$MinTColdLag02)] <- 0
VJI_Final_Weather$MinTColdLag03[is.na(VJI_Final_Weather$MinTColdLag03)] <- 0
VJI_Final_Weather$MinTColdLag04[is.na(VJI_Final_Weather$MinTColdLag04)] <- 0
VJI_Final_Weather$MinTColdLag05[is.na(VJI_Final_Weather$MinTColdLag05)] <- 0
VJI_Final_Weather$MinTColdLag06[is.na(VJI_Final_Weather$MinTColdLag06)] <- 0
VJI_Final_Weather$MinTColdLag07[is.na(VJI_Final_Weather$MinTColdLag07)] <- 0


VJI_Final_Weather$MinTWarmLag00 <- NA
VJI_Final_Weather$MinTWarmLag01 <- NA
VJI_Final_Weather$MinTWarmLag02 <- NA

for (i in (1:5844)) {
  print(i)
  if (VJI_Final_Weather$MinTLoop[i] >= quantile(VJI_Final_Weather$`MinT(C)`, probs = .99,na.rm = T)) {
    VJI_Final_Weather$MinTWarmLag00[i] = 1
    VJI_Final_Weather$MinTWarmLag01[i+1] = 1
    VJI_Final_Weather$MinTWarmLag02[i+2] = 1
  }
}

VJI_Final_Weather$MinTWarmLag00[is.na(VJI_Final_Weather$MinTWarmLag00)] <- 0
VJI_Final_Weather$MinTWarmLag01[is.na(VJI_Final_Weather$MinTWarmLag01)] <- 0
VJI_Final_Weather$MinTWarmLag02[is.na(VJI_Final_Weather$MinTWarmLag02)] <- 0

VJI_Final_Weather$KeepTheseDaysCold <- NA

for (i in (1:5844)) {
  print(i)
  if (VJI_Final_Weather$MinTColdLag00[i] == 1 | VJI_Final_Weather$MinTColdLag01[i] == 1 | VJI_Final_Weather$MinTColdLag02[i] == 1 | VJI_Final_Weather$MinTColdLag03[i] == 1 | VJI_Final_Weather$MinTColdLag04[i] == 1 | VJI_Final_Weather$MinTColdLag05[i] == 1 | VJI_Final_Weather$MinTColdLag06[i] == 1 | VJI_Final_Weather$MinTColdLag07[i] == 1) {
    VJI_Final_Weather$KeepTheseDaysCold[i] = 1
  } else {
    VJI_Final_Weather$KeepTheseDaysCold[i] = 0
  }
}

VJI_Final_Weather$KeepTheseDaysWarm <- NA

for (i in (1:5844)) {
  print(i)
  if (VJI_Final_Weather$MinTWarmLag00[i] == 1 | VJI_Final_Weather$MinTWarmLag01[i] == 1 | VJI_Final_Weather$MinTWarmLag02[i] == 1) {
    VJI_Final_Weather$KeepTheseDaysWarm[i] = 1
  } else {
    VJI_Final_Weather$KeepTheseDaysWarm[i] = 0
  }
}

VJI_Final_Weather$KeepTheseDaysNoStress <- NA
VJI_Final_Weather$KeepTheseDaysNoStress <- ifelse(VJI_Final_Weather$KeepTheseDaysCold == 1 | VJI_Final_Weather$KeepTheseDaysWarm == 1, 0, 1)

VJI_Weather_ColdOnly <- VJI_Final_Weather[(VJI_Final_Weather$KeepTheseDaysCold == 1), ]
VJI_Weather_WarmOnly <- VJI_Final_Weather[(VJI_Final_Weather$KeepTheseDaysWarm == 1), ]
VJI_Weather_NormalOnly <- VJI_Final_Weather[(VJI_Final_Weather$KeepTheseDaysNoStress == 1), ]

VJI_MDC_0514 <- MDC0514 %>%
  filter(ID == "VJI")

VJI_MDC_1519 <- MDC_Bad2020Removed %>%
  filter(ID == "VJI")

VJI_MDC_20 <- MDC20 %>%
  filter(ID == "VJI")

VJI_MDC_Cold_0514_NAs <- left_join(VJI_Weather_ColdOnly, 
                                   VJI_MDC_0514,
                                   by = c("Date_Update" = "Date"))

VJI_MDC0514_Cold <- VJI_MDC_Cold_0514_NAs[!is.na(VJI_MDC_Cold_0514_NAs$MDCAcme),]

write.csv(VJI_MDC0514_Cold, "VJI_MDC0514_Cold.csv")

VJI_MDC_Cold_1519_NAs <- left_join(VJI_Weather_ColdOnly, 
                                   VJI_MDC_1519,
                                   by = c("Date_Update" = "Date"))

VJI_MDC1519_Cold <- VJI_MDC_Cold_1519_NAs[!is.na(VJI_MDC_Cold_1519_NAs$MDCAcme),]

write.csv(VJI_MDC1519_Cold, "VJI_MDC1519_Cold.csv")

VJI_MDC_Cold_20_NAs <- left_join(VJI_Weather_ColdOnly, 
                                 VJI_MDC_20,
                                 by = c("Date_Update" = "Date"))

VJI_MDC20_Cold <- VJI_MDC_Cold_20_NAs[!is.na(VJI_MDC_Cold_20_NAs$MDC0ACME),]

write.csv(VJI_MDC20_Cold, "VJI_MDC20_Cold.csv")


VJI_MDC_Warm_0514_NAs <- left_join(VJI_Weather_WarmOnly, 
                                   VJI_MDC_0514,
                                   by = c("Date_Update" = "Date"))

VJI_MDC0514_Warm <- VJI_MDC_Warm_0514_NAs[!is.na(VJI_MDC_Warm_0514_NAs$MDCAcme),]

write.csv(VJI_MDC0514_Warm, "VJI_MDC0514_Warm.csv")

VJI_MDC_Warm_1519_NAs <- left_join(VJI_Weather_WarmOnly, 
                                   VJI_MDC_1519,
                                   by = c("Date_Update" = "Date"))

VJI_MDC1519_Warm <- VJI_MDC_Warm_1519_NAs[!is.na(VJI_MDC_Warm_1519_NAs$MDCAcme),]

write.csv(VJI_MDC1519_Warm, "VJI_MDC1519_Warm.csv")

VJI_MDC_Warm_20_NAs <- left_join(VJI_Weather_WarmOnly, 
                                 VJI_MDC_20,
                                 by = c("Date_Update" = "Date"))

VJI_MDC20_Warm <- VJI_MDC_Warm_20_NAs[!is.na(VJI_MDC_Warm_20_NAs$MDC0ACME),]

write.csv(VJI_MDC20_Warm, "VJI_MDC20_Warm.csv")


VJI_MDC_Normal_0514_NAs <- left_join(VJI_Weather_NormalOnly, 
                                     VJI_MDC_0514,
                                     by = c("Date_Update" = "Date"))

VJI_MDC0514_Normal <- VJI_MDC_Normal_0514_NAs[!is.na(VJI_MDC_Normal_0514_NAs$MDCAcme),]

write.csv(VJI_MDC0514_Normal, "VJI_MDC0514_Normal.csv")

VJI_MDC_Normal_1519_NAs <- left_join(VJI_Weather_NormalOnly, 
                                     VJI_MDC_1519,
                                     by = c("Date_Update" = "Date"))

VJI_MDC1519_Normal <- VJI_MDC_Normal_1519_NAs[!is.na(VJI_MDC_Normal_1519_NAs$MDCAcme),]

write.csv(VJI_MDC1519_Normal, "VJI_MDC1519_Normal.csv")

VJI_MDC_Normal_20_NAs <- left_join(VJI_Weather_NormalOnly, 
                                   VJI_MDC_20,
                                   by = c("Date_Update" = "Date"))

VJI_MDC20_Normal <- VJI_MDC_Normal_20_NAs[!is.na(VJI_MDC_Normal_20_NAs$MDC0ACME),]

write.csv(VJI_MDC20_Normal, "VJI_MDC20_Normal.csv")


####################################################################################





