## Average deaths per non-heatwave day on case days

library(readxl)
library(dplyr)
library(lubridate)

AdjustedMortMasterOrg <- read_excel("~/Desktop/AdjustedMortMaster.xlsx")
AdjustedMortMaster <- head(AdjustedMortMasterOrg, - 1) 

## CHO
CHOadj <- data.frame(AdjustedMortMaster$rowSums.newmortcompleteCHO...2.9...na.rm...TRUE.)

CHO_working_weatherprep <- read_excel("~/Desktop/CHO.working.Complete (1).xlsx")
CHOHWSevere <- CHO_working_weatherprep$HeatWavesSevere
Date <- CHO_working_weatherprep$Date

work <- cbind(Date,CHOHWSevere,CHOadj)

CHOfull <- work %>%
  filter(CHOHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
CHOFullMean <- mean(CHOfull$AdjustedMortMaster.rowSums.newmortcompleteCHO...2.9...na.rm...TRUE.)

CHO0510 <- work %>%
  filter("2005-01-01" <= Date & Date < "2010-12-31") %>%
  filter(CHOHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
CHOmean0510 <- mean(CHO0510$AdjustedMortMaster.rowSums.newmortcompleteCHO...2.9...na.rm...TRUE.)

CHO1115 <- work %>%
  filter("2011-01-01" <= Date & Date < "2015-12-31") %>%
  filter(CHOHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
CHOmean1115 <- mean(CHO1115$AdjustedMortMaster.rowSums.newmortcompleteCHO...2.9...na.rm...TRUE.)

CHO1620 <- work %>%
  filter("2016-01-01" <= Date & Date < "2020-12-31") %>%
  filter(CHOHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
CHOmean1620 <- mean(CHO1620$AdjustedMortMaster.rowSums.newmortcompleteCHO...2.9...na.rm...TRUE.)

CHO0519 <- work %>%
  filter("2005-01-01" <= Date & Date < "2019-12-31") %>%
  filter(CHOHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
CHOmean0519 <- mean(CHO0519$AdjustedMortMaster.rowSums.newmortcompleteCHO...2.9...na.rm...TRUE.)

CHO20 <- work %>%
  filter("2020-01-01" <= Date & Date < "2020-12-31") %>%
  filter(CHOHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
CHOmean20 <- mean(CHO20$AdjustedMortMaster.rowSums.newmortcompleteCHO...2.9...na.rm...TRUE.)

CHOdailymorts <- c(CHOFullMean,CHOmean0510,CHOmean1115,CHOmean1620,CHOmean0519,CHOmean20)
CHOdailymortsdf <- data.frame(matrix(data = CHOdailymorts, nrow = 1,ncol = 6))
colnames(CHOdailymortsdf) <- c("CHOFull","2005-2010","2011-2015","2016-2020","2005-2019","2020")

## EMV

EMVadj <- data.frame(AdjustedMortMaster$rowSums.newmortcompleteEMV...2.9...na.rm...TRUE.)

EMV_working_weatherprep <- read_excel("~/Desktop/EMV.working.Complete (3).xlsx")
EMVHWSevere <- EMV_working_weatherprep$HeatWavesSevere
Date <- EMV_working_weatherprep$Date

work <- cbind(Date,EMVHWSevere,EMVadj)

EMVfull <- work %>%
  filter(EMVHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
EMVFullMean <- mean(EMVfull$AdjustedMortMaster.rowSums.newmortcompleteEMV...2.9...na.rm...TRUE.)

EMV0510 <- work %>%
  filter("2005-01-01" <= Date & Date < "2010-12-31") %>%
  filter(EMVHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
EMVmean0510 <- mean(EMV0510$AdjustedMortMaster.rowSums.newmortcompleteEMV...2.9...na.rm...TRUE.)

EMV1115 <- work %>%
  filter("2011-01-01" <= Date & Date < "2015-12-31") %>%
  filter(EMVHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
EMVmean1115 <- mean(EMV1115$AdjustedMortMaster.rowSums.newmortcompleteEMV...2.9...na.rm...TRUE.)

EMV1620 <- work %>%
  filter("2016-01-01" <= Date & Date < "2020-12-31") %>%
  filter(EMVHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
EMVmean1620 <- mean(EMV1620$AdjustedMortMaster.rowSums.newmortcompleteEMV...2.9...na.rm...TRUE.)

EMV0519 <- work %>%
  filter("2005-01-01" <= Date & Date < "2019-12-31") %>%
  filter(EMVHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
EMVmean0519 <- mean(EMV0519$AdjustedMortMaster.rowSums.newmortcompleteEMV...2.9...na.rm...TRUE.)

EMV20 <- work %>%
  filter("2020-01-01" <= Date & Date < "2020-12-31") %>%
  filter(EMVHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
EMVmean20 <- mean(EMV20$AdjustedMortMaster.rowSums.newmortcompleteEMV...2.9...na.rm...TRUE.)

EMVdailymorts <- c(EMVFullMean,EMVmean0510,EMVmean1115,EMVmean1620,EMVmean0519,EMVmean20)
EMVdailymortsdf <- data.frame(matrix(data = EMVdailymorts, nrow = 1,ncol = 6))
colnames(EMVdailymortsdf) <- c("EMVFull","2005-2010","2011-2015","2016-2020","2005-2019","2020")

## EZF

EZFadj <- data.frame(AdjustedMortMaster$rowSums.newmortcompleteEZF...2.9...na.rm...TRUE.)

EZF_working_weatherprep <- read_excel("~/Desktop/EZF.working.Complete (1).xlsx")
EZFHWSevere <- EZF_working_weatherprep$HeatWavesSevere
Date <- EZF_working_weatherprep$Date

work <- cbind(Date,EZFHWSevere,EZFadj)

EZFfull <- work %>%
  filter(EZFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
EZFFullMean <- mean(EZFfull$AdjustedMortMaster.rowSums.newmortcompleteEZF...2.9...na.rm...TRUE.)

EZF0510 <- work %>%
  filter("2005-01-01" <= Date & Date < "2010-12-31") %>%
  filter(EZFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
EZFmean0510 <- mean(EZF0510$AdjustedMortMaster.rowSums.newmortcompleteEZF...2.9...na.rm...TRUE.)

EZF1115 <- work %>%
  filter("2011-01-01" <= Date & Date < "2015-12-31") %>%
  filter(EZFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
EZFmean1115 <- mean(EZF1115$AdjustedMortMaster.rowSums.newmortcompleteEZF...2.9...na.rm...TRUE.)

EZF1620 <- work %>%
  filter("2016-01-01" <= Date & Date < "2020-12-31") %>%
  filter(EZFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
EZFmean1620 <- mean(EZF1620$AdjustedMortMaster.rowSums.newmortcompleteEZF...2.9...na.rm...TRUE.)

EZF0519 <- work %>%
  filter("2005-01-01" <= Date & Date < "2019-12-31") %>%
  filter(EZFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
EZFmean0519 <- mean(EZF0519$AdjustedMortMaster.rowSums.newmortcompleteEZF...2.9...na.rm...TRUE.)

EZF20 <- work %>%
  filter("2020-01-01" <= Date & Date < "2020-12-31") %>%
  filter(EZFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
EZFmean20 <- mean(EZF20$AdjustedMortMaster.rowSums.newmortcompleteEZF...2.9...na.rm...TRUE.)

EZFdailymorts <- c(EZFFullMean,EZFmean0510,EZFmean1115,EZFmean1620,EZFmean0519,EZFmean20)
EZFdailymortsdf <- data.frame(matrix(data = EZFdailymorts, nrow = 1,ncol = 6))
colnames(EZFdailymortsdf) <- c("EZFFull","2005-2010","2011-2015","2016-2020","2005-2019","2020")

## IAD

IADadj <- data.frame(AdjustedMortMaster$rowSums.newmortcompleteIAD...2.9...na.rm...TRUE.)

IAD_working_weatherprep <- read_excel("~/Desktop/IAD.working.weatherprep.xlsx")
IADHWSevere <- IAD_working_weatherprep$HeatWavesSevere
Date <- IAD_working_weatherprep$Date

work <- cbind(Date,IADHWSevere,IADadj)

IADfull <- work %>%
  filter(IADHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
IADFullMean <- mean(IADfull$AdjustedMortMaster.rowSums.newmortcompleteIAD...2.9...na.rm...TRUE.)

IAD0510 <- work %>%
  filter("2005-01-01" <= Date & Date < "2010-12-31") %>%
  filter(IADHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
IADmean0510 <- mean(IAD0510$AdjustedMortMaster.rowSums.newmortcompleteIAD...2.9...na.rm...TRUE.)

IAD1115 <- work %>%
  filter("2011-01-01" <= Date & Date < "2015-12-31") %>%
  filter(IADHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
IADmean1115 <- mean(IAD1115$AdjustedMortMaster.rowSums.newmortcompleteIAD...2.9...na.rm...TRUE.)

IAD1620 <- work %>%
  filter("2016-01-01" <= Date & Date < "2020-12-31") %>%
  filter(IADHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
IADmean1620 <- mean(IAD1620$AdjustedMortMaster.rowSums.newmortcompleteIAD...2.9...na.rm...TRUE.)

IAD0519 <- work %>%
  filter("2005-01-01" <= Date & Date < "2019-12-31") %>%
  filter(IADHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
IADmean0519 <- mean(IAD0519$AdjustedMortMaster.rowSums.newmortcompleteIAD...2.9...na.rm...TRUE.)

IAD20 <- work %>%
  filter("2020-01-01" <= Date & Date < "2020-12-31") %>%
  filter(IADHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
IADmean20 <- mean(IAD20$AdjustedMortMaster.rowSums.newmortcompleteIAD...2.9...na.rm...TRUE.)

IADdailymorts <- c(IADFullMean,IADmean0510,IADmean1115,IADmean1620,IADmean0519,IADmean20)
IADdailymortsdf <- data.frame(matrix(data = IADdailymorts, nrow = 1,ncol = 6))
colnames(IADdailymortsdf) <- c("IADFull","2005-2010","2011-2015","2016-2020","2005-2019","2020")


## LYH

LYHadj <- data.frame(AdjustedMortMaster$rowSums.newmortcompleteLYH...2.9...na.rm...TRUE.)

LYH_working_weatherprep <- read_excel("~/Desktop/LYH.working.weatherprep.xlsx", sheet = "Sheet1")
LYHHWSevere <- LYH_working_weatherprep$HeatWavesSevere
Date <- LYH_working_weatherprep$Date

work <- cbind(Date,LYHHWSevere,LYHadj)

LYHfull <- work %>%
  filter(LYHHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
LYHFullMean <- mean(LYHfull$AdjustedMortMaster.rowSums.newmortcompleteLYH...2.9...na.rm...TRUE.)

LYH0510 <- work %>%
  filter("2005-01-01" <= Date & Date < "2010-12-31") %>%
  filter(LYHHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
LYHmean0510 <- mean(LYH0510$AdjustedMortMaster.rowSums.newmortcompleteLYH...2.9...na.rm...TRUE.)

LYH1115 <- work %>%
  filter("2011-01-01" <= Date & Date < "2015-12-31") %>%
  filter(LYHHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
LYHmean1115 <- mean(LYH1115$AdjustedMortMaster.rowSums.newmortcompleteLYH...2.9...na.rm...TRUE.)

LYH1620 <- work %>%
  filter("2016-01-01" <= Date & Date < "2020-12-31") %>%
  filter(LYHHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
LYHmean1620 <- mean(LYH1620$AdjustedMortMaster.rowSums.newmortcompleteLYH...2.9...na.rm...TRUE.)

LYH0519 <- work %>%
  filter("2005-01-01" <= Date & Date < "2019-12-31") %>%
  filter(LYHHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
LYHmean0519 <- mean(LYH0519$AdjustedMortMaster.rowSums.newmortcompleteLYH...2.9...na.rm...TRUE.)

LYH20 <- work %>%
  filter("2020-01-01" <= Date & Date < "2020-12-31") %>%
  filter(LYHHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
LYHmean20 <- mean(LYH20$AdjustedMortMaster.rowSums.newmortcompleteLYH...2.9...na.rm...TRUE.)

LYHdailymorts <- c(LYHFullMean,LYHmean0510,LYHmean1115,LYHmean1620,LYHmean0519,LYHmean20)
LYHdailymortsdf <- data.frame(matrix(data = LYHdailymorts, nrow = 1,ncol = 6))
colnames(LYHdailymortsdf) <- c("LYHFull","2005-2010","2011-2015","2016-2020","2005-2019","2020")

## OKV

OKVadj <- data.frame(AdjustedMortMaster$rowSums.newmortcompleteOKV...2.9...na.rm...TRUE.)

OKV_working_weatherprep <- read_excel("~/Desktop/OKV.working.weatherprep.xlsx", sheet = "Sheet1")
OKVHWSevere <- OKV_working_weatherprep$HeatWavesSevere
Date <- OKV_working_weatherprep$Date

work <- cbind(Date,OKVHWSevere,OKVadj)

OKVfull <- work %>%
  filter(OKVHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
OKVFullMean <- mean(OKVfull$AdjustedMortMaster.rowSums.newmortcompleteOKV...2.9...na.rm...TRUE.)

OKV0510 <- work %>%
  filter("2005-01-01" <= Date & Date < "2010-12-31") %>%
  filter(OKVHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
OKVmean0510 <- mean(OKV0510$AdjustedMortMaster.rowSums.newmortcompleteOKV...2.9...na.rm...TRUE.)

OKV1115 <- work %>%
  filter("2011-01-01" <= Date & Date < "2015-12-31") %>%
  filter(OKVHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
OKVmean1115 <- mean(OKV1115$AdjustedMortMaster.rowSums.newmortcompleteOKV...2.9...na.rm...TRUE.)

OKV1620 <- work %>%
  filter("2016-01-01" <= Date & Date < "2020-12-31") %>%
  filter(OKVHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
OKVmean1620 <- mean(OKV1620$AdjustedMortMaster.rowSums.newmortcompleteOKV...2.9...na.rm...TRUE.)

OKV0519 <- work %>%
  filter("2005-01-01" <= Date & Date < "2019-12-31") %>%
  filter(OKVHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
OKVmean0519 <- mean(OKV0519$AdjustedMortMaster.rowSums.newmortcompleteOKV...2.9...na.rm...TRUE.)

OKV20 <- work %>%
  filter("2020-01-01" <= Date & Date < "2020-12-31") %>%
  filter(OKVHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
OKVmean20 <- mean(OKV20$AdjustedMortMaster.rowSums.newmortcompleteOKV...2.9...na.rm...TRUE.)

OKVdailymorts <- c(OKVFullMean,OKVmean0510,OKVmean1115,OKVmean1620,OKVmean0519,OKVmean20)
OKVdailymortsdf <- data.frame(matrix(data = OKVdailymorts, nrow = 1,ncol = 6))
colnames(OKVdailymortsdf) <- c("OKVFull","2005-2010","2011-2015","2016-2020","2005-2019","2020")

## ORF

ORFadj <- data.frame(AdjustedMortMaster$rowSums.newmortcompleteORF...2.9...na.rm...TRUE.)

ORF_working_weatherprep <- read_excel("~/Desktop/ORF.working.weatherprep.xlsx", sheet = "Sheet1")
ORFHWSevere <- ORF_working_weatherprep$HeatWavesSevere
Date <- ORF_working_weatherprep$Date

work <- cbind(Date,ORFHWSevere,ORFadj)

ORFfull <- work %>%
  filter(ORFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
ORFFullMean <- mean(ORFfull$AdjustedMortMaster.rowSums.newmortcompleteORF...2.9...na.rm...TRUE.)

ORF0510 <- work %>%
  filter("2005-01-01" <= Date & Date < "2010-12-31") %>%
  filter(ORFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
ORFmean0510 <- mean(ORF0510$AdjustedMortMaster.rowSums.newmortcompleteORF...2.9...na.rm...TRUE.)

ORF1115 <- work %>%
  filter("2011-01-01" <= Date & Date < "2015-12-31") %>%
  filter(ORFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
ORFmean1115 <- mean(ORF1115$AdjustedMortMaster.rowSums.newmortcompleteORF...2.9...na.rm...TRUE.)

ORF1620 <- work %>%
  filter("2016-01-01" <= Date & Date < "2020-12-31") %>%
  filter(ORFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
ORFmean1620 <- mean(ORF1620$AdjustedMortMaster.rowSums.newmortcompleteORF...2.9...na.rm...TRUE.)

ORF0519 <- work %>%
  filter("2005-01-01" <= Date & Date < "2019-12-31") %>%
  filter(ORFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
ORFmean0519 <- mean(ORF0519$AdjustedMortMaster.rowSums.newmortcompleteORF...2.9...na.rm...TRUE.)

ORF20 <- work %>%
  filter("2020-01-01" <= Date & Date < "2020-12-31") %>%
  filter(ORFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
ORFmean20 <- mean(ORF20$AdjustedMortMaster.rowSums.newmortcompleteORF...2.9...na.rm...TRUE.)

ORFdailymorts <- c(ORFFullMean,ORFmean0510,ORFmean1115,ORFmean1620,ORFmean0519,ORFmean20)
ORFdailymortsdf <- data.frame(matrix(data = ORFdailymorts, nrow = 1,ncol = 6))
colnames(ORFdailymortsdf) <- c("ORFFull","2005-2010","2011-2015","2016-2020","2005-2019","2020")

## PHF

PHFadj <- data.frame(AdjustedMortMaster$rowSums.newmortcompletePHF...2.9...na.rm...TRUE.)

PHF_working_weatherprep <- read_excel("~/Desktop/PHF.Working.Complete.xlsx", sheet = "Sheet1")
PHFHWSevere <- PHF_working_weatherprep$HeatWavesSevere
Date <- PHF_working_weatherprep$Date

work <- cbind(Date,PHFHWSevere,PHFadj)

PHFfull <- work %>%
  filter(PHFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
PHFFullMean <- mean(PHFfull$AdjustedMortMaster.rowSums.newmortcompletePHF...2.9...na.rm...TRUE.)

PHF0510 <- work %>%
  filter("2005-01-01" <= Date & Date < "2010-12-31") %>%
  filter(PHFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
PHFmean0510 <- mean(PHF0510$AdjustedMortMaster.rowSums.newmortcompletePHF...2.9...na.rm...TRUE.)

PHF1115 <- work %>%
  filter("2011-01-01" <= Date & Date < "2015-12-31") %>%
  filter(PHFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
PHFmean1115 <- mean(PHF1115$AdjustedMortMaster.rowSums.newmortcompletePHF...2.9...na.rm...TRUE.)

PHF1620 <- work %>%
  filter("2016-01-01" <= Date & Date < "2020-12-31") %>%
  filter(PHFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
PHFmean1620 <- mean(PHF1620$AdjustedMortMaster.rowSums.newmortcompletePHF...2.9...na.rm...TRUE.)

PHF0519 <- work %>%
  filter("2005-01-01" <= Date & Date < "2019-12-31") %>%
  filter(PHFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
PHFmean0519 <- mean(PHF0519$AdjustedMortMaster.rowSums.newmortcompletePHF...2.9...na.rm...TRUE.)

PHF20 <- work %>%
  filter("2020-01-01" <= Date & Date < "2020-12-31") %>%
  filter(PHFHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
PHFmean20 <- mean(PHF20$AdjustedMortMaster.rowSums.newmortcompletePHF...2.9...na.rm...TRUE.)

PHFdailymorts <- c(PHFFullMean,PHFmean0510,PHFmean1115,PHFmean1620,PHFmean0519,PHFmean20)
PHFdailymortsdf <- data.frame(matrix(data = PHFdailymorts, nrow = 1,ncol = 6))
colnames(PHFdailymortsdf) <- c("PHFFull","2005-2010","2011-2015","2016-2020","2005-2019","2020")

## RIC

RICadj <- data.frame(AdjustedMortMaster$rowSums.newmortcompleteRIC...2.9...na.rm...TRUE.)

RIC_working_weatherprep <- read_excel("~/Desktop/RIC.working.weatherprep.xlsx", sheet = "Sheet1")
RICHWSevere <- RIC_working_weatherprep$HeatWavesSevere
Date <- RIC_working_weatherprep$Date

work <- cbind(Date,RICHWSevere,RICadj)

RICfull <- work %>%
  filter(RICHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
RICFullMean <- mean(RICfull$AdjustedMortMaster.rowSums.newmortcompleteRIC...2.9...na.rm...TRUE.)

RIC0510 <- work %>%
  filter("2005-01-01" <= Date & Date < "2010-12-31") %>%
  filter(RICHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
RICmean0510 <- mean(RIC0510$AdjustedMortMaster.rowSums.newmortcompleteRIC...2.9...na.rm...TRUE.)

RIC1115 <- work %>%
  filter("2011-01-01" <= Date & Date < "2015-12-31") %>%
  filter(RICHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
RICmean1115 <- mean(RIC1115$AdjustedMortMaster.rowSums.newmortcompleteRIC...2.9...na.rm...TRUE.)

RIC1620 <- work %>%
  filter("2016-01-01" <= Date & Date < "2020-12-31") %>%
  filter(RICHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
RICmean1620 <- mean(RIC1620$AdjustedMortMaster.rowSums.newmortcompleteRIC...2.9...na.rm...TRUE.)

RIC0519 <- work %>%
  filter("2005-01-01" <= Date & Date < "2019-12-31") %>%
  filter(RICHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
RICmean0519 <- mean(RIC0519$AdjustedMortMaster.rowSums.newmortcompleteRIC...2.9...na.rm...TRUE.)

RIC20 <- work %>%
  filter("2020-01-01" <= Date & Date < "2020-12-31") %>%
  filter(RICHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
RICmean20 <- mean(RIC20$AdjustedMortMaster.rowSums.newmortcompleteRIC...2.9...na.rm...TRUE.)

RICdailymorts <- c(RICFullMean,RICmean0510,RICmean1115,RICmean1620,RICmean0519,RICmean20)
RICdailymortsdf <- data.frame(matrix(data = RICdailymorts, nrow = 1,ncol = 6))
colnames(RICdailymortsdf) <- c("RICFull","2005-2010","2011-2015","2016-2020","2005-2019","2020")

## ROA

ROAadj <- data.frame(AdjustedMortMaster$rowSums.newmortcompleteROA...2.9...na.rm...TRUE.)

ROA_working_weatherprep <- read_excel("~/Desktop/ROA.Working.Complete.xlsx", sheet = "Sheet1")
ROAHWSevere <- ROA_working_weatherprep$HeatWavesSevere
Date <- ROA_working_weatherprep$Date

work <- cbind(Date,ROAHWSevere,ROAadj)

ROAfull <- work %>%
  filter(ROAHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
ROAFullMean <- mean(ROAfull$AdjustedMortMaster.rowSums.newmortcompleteROA...2.9...na.rm...TRUE.)

ROA0510 <- work %>%
  filter("2005-01-01" <= Date & Date < "2010-12-31") %>%
  filter(ROAHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
ROAmean0510 <- mean(ROA0510$AdjustedMortMaster.rowSums.newmortcompleteROA...2.9...na.rm...TRUE.)

ROA1115 <- work %>%
  filter("2011-01-01" <= Date & Date < "2015-12-31") %>%
  filter(ROAHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
ROAmean1115 <- mean(ROA1115$AdjustedMortMaster.rowSums.newmortcompleteROA...2.9...na.rm...TRUE.)

ROA1620 <- work %>%
  filter("2016-01-01" <= Date & Date < "2020-12-31") %>%
  filter(ROAHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
ROAmean1620 <- mean(ROA1620$AdjustedMortMaster.rowSums.newmortcompleteROA...2.9...na.rm...TRUE.)

ROA0519 <- work %>%
  filter("2005-01-01" <= Date & Date < "2019-12-31") %>%
  filter(ROAHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
ROAmean0519 <- mean(ROA0519$AdjustedMortMaster.rowSums.newmortcompleteROA...2.9...na.rm...TRUE.)

ROA20 <- work %>%
  filter("2020-01-01" <= Date & Date < "2020-12-31") %>%
  filter(ROAHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
ROAmean20 <- mean(ROA20$AdjustedMortMaster.rowSums.newmortcompleteROA...2.9...na.rm...TRUE.)

ROAdailymorts <- c(ROAFullMean,ROAmean0510,ROAmean1115,ROAmean1620,ROAmean0519,ROAmean20)
ROAdailymortsdf <- data.frame(matrix(data = ROAdailymorts, nrow = 1,ncol = 6))
colnames(ROAdailymortsdf) <- c("ROAFull","2005-2010","2011-2015","2016-2020","2005-2019","2020")

## SHD
SHDadj <- data.frame(AdjustedMortMaster$rowSums.newmortcompleteSHD...2.9...na.rm...TRUE.)

SHD_working_weatherprep <- read_excel("~/Desktop/SHD.working.Complete.xlsx")
SHDHWSevere <- SHD_working_weatherprep$HeatWavesSevere
Date <- SHD_working_weatherprep$Date

work <- cbind(Date,SHDHWSevere,SHDadj)

SHDfull <- work %>%
  filter(SHDHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
SHDFullMean <- mean(SHDfull$AdjustedMortMaster.rowSums.newmortcompleteSHD...2.9...na.rm...TRUE.)

SHD0510 <- work %>%
  filter("2005-01-01" <= Date & Date < "2010-12-31") %>%
  filter(SHDHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
SHDmean0510 <- mean(SHD0510$AdjustedMortMaster.rowSums.newmortcompleteSHD...2.9...na.rm...TRUE.)

SHD1115 <- work %>%
  filter("2011-01-01" <= Date & Date < "2015-12-31") %>%
  filter(SHDHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
SHDmean1115 <- mean(SHD1115$AdjustedMortMaster.rowSums.newmortcompleteSHD...2.9...na.rm...TRUE.)

SHD1620 <- work %>%
  filter("2016-01-01" <= Date & Date < "2020-12-31") %>%
  filter(SHDHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
SHDmean1620 <- mean(SHD1620$AdjustedMortMaster.rowSums.newmortcompleteSHD...2.9...na.rm...TRUE.)

SHD0519 <- work %>%
  filter("2005-01-01" <= Date & Date < "2019-12-31") %>%
  filter(SHDHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
SHDmean0519 <- mean(SHD0519$AdjustedMortMaster.rowSums.newmortcompleteSHD...2.9...na.rm...TRUE.)

SHD20 <- work %>%
  filter("2020-01-01" <= Date & Date < "2020-12-31") %>%
  filter(SHDHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
SHDmean20 <- mean(SHD20$AdjustedMortMaster.rowSums.newmortcompleteSHD...2.9...na.rm...TRUE.)

SHDdailymorts <- c(SHDFullMean,SHDmean0510,SHDmean1115,SHDmean1620,SHDmean0519,SHDmean20)
SHDdailymortsdf <- data.frame(matrix(data = SHDdailymorts, nrow = 1,ncol = 6))
colnames(SHDdailymortsdf) <- c("SHDFull","2005-2010","2011-2015","2016-2020","2005-2019","2020")

## VJI

VJIadj <- data.frame(AdjustedMortMaster$rowSums.newmortcompleteVJI...2.9...na.rm...TRUE.)

VJI_working_weatherprep <- read_excel("~/Desktop/VJI.working.weatherprep.xlsx", sheet = "Sheet1")
VJIHWSevere <- VJI_working_weatherprep$HeatWavesSevere
Date <- VJI_working_weatherprep$Date

work <- cbind(Date,VJIHWSevere,VJIadj)

VJIfull <- work %>%
  filter(VJIHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
VJIFullMean <- mean(VJIfull$AdjustedMortMaster.rowSums.newmortcompleteVJI...2.9...na.rm...TRUE.)

VJI0510 <- work %>%
  filter("2005-01-01" <= Date & Date < "2010-12-31") %>%
  filter(VJIHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
VJImean0510 <- mean(VJI0510$AdjustedMortMaster.rowSums.newmortcompleteVJI...2.9...na.rm...TRUE.)

VJI1115 <- work %>%
  filter("2011-01-01" <= Date & Date < "2015-12-31") %>%
  filter(VJIHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
VJImean1115 <- mean(VJI1115$AdjustedMortMaster.rowSums.newmortcompleteVJI...2.9...na.rm...TRUE.)

VJI1620 <- work %>%
  filter("2016-01-01" <= Date & Date < "2020-12-31") %>%
  filter(VJIHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
VJImean1620 <- mean(VJI1620$AdjustedMortMaster.rowSums.newmortcompleteVJI...2.9...na.rm...TRUE.)

VJI0519 <- work %>%
  filter("2005-01-01" <= Date & Date < "2019-12-31") %>%
  filter(VJIHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
VJImean0519 <- mean(VJI0519$AdjustedMortMaster.rowSums.newmortcompleteVJI...2.9...na.rm...TRUE.)

VJI20 <- work %>%
  filter("2020-01-01" <= Date & Date < "2020-12-31") %>%
  filter(VJIHWSevere == 0) %>%
  filter(month(Date) > 3 & month(Date) < 10)
VJImean20 <- mean(VJI20$AdjustedMortMaster.rowSums.newmortcompleteVJI...2.9...na.rm...TRUE.)

VJIdailymorts <- c(VJIFullMean,VJImean0510,VJImean1115,VJImean1620,VJImean0519,VJImean20)
VJIdailymortsdf <- data.frame(matrix(data = VJIdailymorts, nrow = 1,ncol = 6))
colnames(VJIdailymortsdf) <- c("VJIFull","2005-2010","2011-2015","2016-2020","2005-2019","2020")

## Total

NonHWfinaldailymortsdf <- rbind(as.matrix(CHOdailymortsdf), as.matrix(EMVdailymortsdf), as.matrix(EZFdailymortsdf), as.matrix(IADdailymortsdf), as.matrix(LYHdailymortsdf), as.matrix(OKVdailymortsdf), as.matrix(ORFdailymortsdf), as.matrix(PHFdailymortsdf), as.matrix(RICdailymortsdf), as.matrix(ROAdailymortsdf), as.matrix(SHDdailymortsdf), as.matrix(VJIdailymortsdf))
rownames(NonHWfinaldailymortsdf) <- c("CHO","EMV","EZF","IAD","LYH","OKV","ORF","PHF","RIC","ROA","SHD","VJI")
colnames(NonHWfinaldailymortsdf) <- c("NonHW0520","J0510","J1115","J1620","J0519","J20")
NonHWfinaldailymortsdf <- data.frame(NonHWfinaldailymortsdf)
NonHWfinaldailymortsdf$diff <- NonHWfinaldailymortsdf$J20 - NonHWfinaldailymortsdf$J0519
View(NonHWfinaldailymortsdf)
