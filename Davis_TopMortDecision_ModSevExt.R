library(readxl)
library(dplyr)
library(lubridate)
#library(boot)

CHO <- read_excel("~/Desktop/CHO.Final.Weather.xlsx",na="NA")
CHO <- CHO[1:5844, ]
CHO %>% mutate_if(is.character, as.numeric)

CHOMort100 <- CHO %>%                                      # Top N highest values by group
  arrange(desc(Mort)) %>% 
  slice(1:100)
CHOMort100$Mort[100]

CHOMortGreat <- CHO %>%                                      # Top N highest values by group
  filter(Mort >= (CHOMort100$Mort[100]))

hist(CHOMortGreat$`MaxT(C)`,breaks = (max(CHOMortGreat$`MaxT(C)`,na.rm = TRUE)-min(CHOMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))


EMV <- read_excel("~/Desktop/EMV.Final.Weather.xlsx",na="NA")
EMV <- EMV[1:5844, ]
EMV %>% mutate_if(is.character, as.numeric)

EMVMort100 <- EMV %>%                                      # Top N highest values by group
  arrange(desc(Mort)) %>% 
  slice(1:100)
EMVMort100$Mort[100]

EMVMortGreat <- EMV %>%                                      # Top N highest values by group
  filter(Mort >= (EMVMort100$Mort[100]))

hist(EMVMortGreat$`MaxT(C)`,breaks = (max(EMVMortGreat$`MaxT(C)`,na.rm = TRUE)-min(EMVMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))

EZF <- read_excel("~/Desktop/EZF.Final.Weather.xlsx",na="NA")
EZF <- EZF[1:5844, ]
EZF %>% mutate_if(is.character, as.numeric)

EZFMort100 <- EZF %>%                                      # Top N highest values by group
  arrange(desc(Mort)) %>% 
  slice(1:100)
EZFMort100$Mort[100]

EZFMortGreat <- EZF %>%                                      # Top N highest values by group
  filter(Mort >= (EZFMort100$Mort[100]))

hist(EZFMortGreat$`MaxT(C)`,breaks = (max(EZFMortGreat$`MaxT(C)`,na.rm = TRUE)-min(EZFMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))

IAD <- read_excel("~/Desktop/IAD.Final.Weather.xlsx",na="NA")
IAD <- IAD[1:5844, ]
IAD %>% mutate_if(is.character, as.numeric)

IADMort100 <- IAD %>%                                      # Top N highest values by group
  arrange(desc(Mort)) %>% 
  slice(1:100)
IADMort100$Mort[100]

IADMortGreat <- IAD %>%                                      # Top N highest values by group
  filter(Mort >= (IADMort100$Mort[100]))

hist(IADMortGreat$`MaxT(C)`,breaks = (max(IADMortGreat$`MaxT(C)`,na.rm = TRUE)-min(IADMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))


LYH <- read_excel("~/Desktop/LYH.Final.Weather.xlsx",na="NA")
LYH <- LYH[1:5844, ]
LYH %>% mutate_if(is.character, as.numeric)

LYHMort100 <- LYH %>%                                      # Top N highest values by group
  arrange(desc(Mort)) %>% 
  slice(1:100)
LYHMort100$Mort[100]

LYHMortGreat <- LYH %>%                                      # Top N highest values by group
  filter(Mort >= (LYHMort100$Mort[100]))

hist(LYHMortGreat$`MaxT(C)`,breaks = (max(LYHMortGreat$`MaxT(C)`,na.rm = TRUE)-min(LYHMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))


OKV <- read_excel("~/Desktop/OKV.Final.Weather.xlsx",na="NA")
OKV <- OKV[1:5844, ]
OKV %>% mutate_if(is.character, as.numeric)

OKVMort100 <- OKV %>%                                      # Top N highest values by group
  arrange(desc(Mort)) %>% 
  slice(1:100)
OKVMort100$Mort[100]

OKVMortGreat <- OKV %>%                                      # Top N highest values by group
  filter(Mort >= (OKVMort100$Mort[100]))

hist(OKVMortGreat$`MaxT(C)`,breaks = (max(OKVMortGreat$`MaxT(C)`,na.rm = TRUE)-min(OKVMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))


ORF <- read_excel("~/Desktop/ORF.Final.Weather.xlsx",na="NA")
ORF <- ORF[1:5844, ]
ORF %>% mutate_if(is.character, as.numeric)

ORFMort100 <- ORF %>%                                      # Top N highest values by group
  arrange(desc(Mort)) %>% 
  slice(1:100)
ORFMort100$Mort[100]

ORFMortGreat <- ORF %>%                                      # Top N highest values by group
  filter(Mort >= (ORFMort100$Mort[100]))

hist(ORFMortGreat$`MaxT(C)`,breaks = (max(ORFMortGreat$`MaxT(C)`,na.rm = TRUE)-min(ORFMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))


PHF <- read_excel("~/Desktop/PHF.Final.Weather.xlsx",na="NA")
PHF <- PHF[1:5844, ]
PHF %>% mutate_if(is.character, as.numeric)

PHFMort100 <- PHF %>%                                      # Top N highest values by group
  arrange(desc(Mort)) %>% 
  slice(1:100)
PHFMort100$Mort[100]

PHFMortGreat <- PHF %>%                                      # Top N highest values by group
  filter(Mort >= (PHFMort100$Mort[100]))

hist(PHFMortGreat$`MaxT(C)`,breaks = (max(PHFMortGreat$`MaxT(C)`,na.rm = TRUE)-min(PHFMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))


RIC <- read_excel("~/Desktop/RIC.Final.Weather.xlsx",na="NA")
RIC <- RIC[1:5844, ]
RIC %>% mutate_if(is.character, as.numeric)

RICMort100 <- RIC %>%                                      # Top N highest values by group
  arrange(desc(Mort)) %>% 
  slice(1:100)
RICMort100$Mort[100]

RICMortGreat <- RIC %>%                                      # Top N highest values by group
  filter(Mort >= (RICMort100$Mort[100]))

hist(RICMortGreat$`MaxT(C)`,breaks = (max(RICMortGreat$`MaxT(C)`,na.rm = TRUE)-min(RICMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))


ROA <- read_excel("~/Desktop/ROA.Final.Weather.xlsx",na="NA")
ROA <- ROA[1:5844, ]
ROA %>% mutate_if(is.character, as.numeric)

ROAMort100 <- ROA %>%                                      # Top N highest values by group
  arrange(desc(Mort)) %>% 
  slice(1:100)
ROAMort100$Mort[100]

ROAMortGreat <- ROA %>%                                      # Top N highest values by group
  filter(Mort >= (ROAMort100$Mort[100]))

hist(ROAMortGreat$`MaxT(C)`,breaks = (max(ROAMortGreat$`MaxT(C)`,na.rm = TRUE)-min(ROAMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))


SHD <- read_excel("~/Desktop/SHD.Final.Weather.xlsx",na="NA")
SHD <- SHD[1:5844, ]
SHD %>% mutate_if(is.character, as.numeric)

SHDMort100 <- SHD %>%                                      # Top N highest values by group
  arrange(desc(Mort)) %>% 
  slice(1:100)
SHDMort100$Mort[100]

SHDMortGreat <- SHD %>%                                      # Top N highest values by group
  filter(Mort >= (SHDMort100$Mort[100]))

hist(SHDMortGreat$`MaxT(C)`,breaks = (max(SHDMortGreat$`MaxT(C)`,na.rm = TRUE)-min(SHDMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))


VJI <- read_excel("~/Desktop/VJI.Final.Weather.xlsx",na="NA")
VJI <- VJI[1:5844, ]
VJI %>% mutate_if(is.character, as.numeric)

VJIMort100 <- VJI %>%                                      # Top N highest values by group
  arrange(desc(Mort)) %>% 
  slice(1:100)
VJIMort100$Mort[100]

VJIMortGreat <- VJI %>%                                      # Top N highest values by group
  filter(Mort >= (VJIMort100$Mort[100]))

hist(VJIMortGreat$`MaxT(C)`,breaks = (max(VJIMortGreat$`MaxT(C)`,na.rm = TRUE)-min(VJIMortGreat$`MaxT(C)`,na.rm = TRUE)),xlim = c(-15,45))

colSums(BacktoBackCHO) #Heat
colSums(BacktoBackEMV)
colSums(BacktoBackEZF)
colSums(BacktoBackIAD)
colSums(BacktoBackLYH) #Heat
colSums(BacktoBackOKV)
colSums(BacktoBackORF) #Heat
colSums(BacktoBackPHF)
colSums(BacktoBackRIC)
colSums(BacktoBackROA)
colSums(BacktoBackSHD)
colSums(BacktoBackVJI) #Heat

#plot hist of mortality, pick thresholds for mod, severe, and extreme amounts, NO MORTALITY LAGS