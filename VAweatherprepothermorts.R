### Creating State-Wide Weather Data Adjusted for Black mortality

#Load in stations
library(readxl)
library(dplyr)
library(stringr)
library(padr)

CHO <- read_excel("~/Desktop/CHO.Working.Complete (1).xlsx", na = "NA")
CHO <- select(CHO,1:91)
CHO %>% mutate_if(is.character,is.numeric)

EMV <- read_excel("~/Desktop/EMV.Working.Complete (3).xlsx", na = "NA")
EMV <- select(EMV,1:91)
EMV %>% mutate_if(is.character,is.numeric)

EZF <- read_excel("~/Desktop/EZF.Working.Complete (1).xlsx", na = "NA")
EZF <- select(EZF,1:91)
EZF %>% mutate_if(is.character,is.numeric)

IAD <- read_excel("~/Desktop/IAD.working.weatherprep.xlsx", na = "NA")
IAD <- select(IAD,1:91)
IAD %>% mutate_if(is.character,is.numeric)

LYH <- read_excel("~/Desktop/LYH.working.weatherprep.xlsx", sheet = "Sheet1", na = "NA")
LYH <- select(LYH,1:91)
LYH %>% mutate_if(is.character,is.numeric)

OKV <- read_excel("~/Desktop/OKV.working.weatherprep.xlsx", sheet = "Sheet1", na = "NA")
OKV <- select(OKV,1:91)
OKV %>% mutate_if(is.character,is.numeric)

ORF <- read_excel("~/Desktop/ORF.working.weatherprep.xlsx", sheet = "Sheet1", na = "NA")
ORF <- select(ORF,1:91)
ORF %>% mutate_if(is.character,is.numeric)

PHF <- read_excel("~/Desktop/PHF.Working.Complete.xlsx", na = "NA")
PHF <- select(PHF,1:91)
PHF %>% mutate_if(is.character,is.numeric)

RIC <- read_excel("~/Desktop/RIC.working.weatherprep.xlsx", sheet = "Sheet1", na = "NA")
RIC <- select(RIC,1:91)
RIC %>% mutate_if(is.character,is.numeric)

ROA <- read_excel("~/Desktop/ROA.Working.Complete.xlsx", na = "NA")
ROA <- select(ROA,1:91)
ROA %>% mutate_if(is.character,is.numeric)

SHD <- read_excel("~/Desktop/SHD.Working.Complete.xlsx", sheet = "Sheet1", na = "NA")
SHD <- select(SHD,1:91)
SHD %>% mutate_if(is.character,is.numeric)

VJI <- read_excel("~/Desktop/VJI.working.weatherprep.xlsx", na = "NA")
VJI <- select(VJI,1:91)
VJI %>% mutate_if(is.character,is.numeric)

test <- rbind(CHO,EMV,EZF,IAD,LYH,OKV,ORF,PHF,RIC,ROA,SHD,VJI)

#boxplot(test$Speedkts1pm)
#sum(test$SLPhPa1pm > 1040,na.rm = TRUE)
#max(test$SLPhPa1pm,na.rm = TRUE)
#test[which.max(test$SLPhPa1pm),]

test <- select(test,-Station,-Date,-Year,-Month,-Day,-`MaxT(C)`,-`MaxTDep(C)`,-`MinT(C)`,-`MinTDep(C)`,-`DTR(C)`,-`T(C)1am`,-`Td(C)1am`,-`Tw(C)1am`,-`AT(C)1am`,-`THI(C)1am`,-`Hx(C)1am`,-`WC(C)1am`,-`T(C)7am`,-`Td(C)7am`,-`Tw(C)7am`,-`AT(C)7am`,-`THI(C)7am`,-`Hx(C)7am`,-`WC(C)7am`,-`T(C)1pm`,-`Td(C)1pm`,-`Tw(C)1pm`,-`AT(C)1pm`,-`THI(C)1pm`,-`Hx(C)1pm`,-`WC(C)1pm`,-`T(C)7pm`,-`Td(C)7pm`,-`Tw(C)7pm`,-`AT(C)7pm`,-`THI(C)7pm`,-`Hx(C)7pm`,-`WC(C)7pm`)

test %>% mutate_if(is.character,is.numeric)

CHO <- test[1:5844, ]
EMV <- test[5845:11688, ]
EZF <- test[11689:17532, ]
IAD <- test[17533:23376, ]
LYH <- test[23377:29220, ]
OKV <- test[29221:35064, ]
ORF <- test[35065:40908, ]
PHF <- test[40909:46752, ]
RIC <- test[46753:52596, ]
ROA <- test[52597:58440, ]
SHD <- test[58441:64284, ]
VJI <- test[64285:70128, ]

#Other Mortality Totals
BigMortR <- read_excel("~/Desktop/BigMort3.xlsx")
BigMortR$`Station ID` <- str_replace_all(BigMortR$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))
BigMortR$Date = as.Date(paste(BigMortR$DOD_YR,BigMortR$DOD_MO,BigMortR$DOD_DY,sep="-"))
BigMortR$Count = 1

BigMortR$RACE <- str_replace_all(BigMortR$RACE, c("Asian/PacificIslander"="Other",
                                             "AmerInd/AlaskNat"="Other",
                                             "Other"="Other"))

Race <- BigMortR %>%
  filter(RACE == 'Other') %>%
  group_by(Date) %>%
  summarise(Other=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

CHOmortdf <- BigMortR %>%
  filter(RACE == 'Other' & `Station ID` == 'CHO') %>%
  group_by(Date) %>%
  summarise(CHOmortOther=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

EMVmortdf <- BigMortR %>%
  filter(RACE == 'Other' & `Station ID` == 'EMV') %>%
  group_by(Date) %>%
  summarise(EMVmortOther=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

EZFmortdf <- BigMortR %>%
  filter(RACE == 'Other' & `Station ID` == 'EZF') %>%
  group_by(Date) %>%
  summarise(EZFmortOther=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

IADmortdf <- BigMortR %>%
  filter(RACE == 'Other' & `Station ID` == 'IAD') %>%
  group_by(Date) %>%
  summarise(IADmortOther=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

LYHmortdf <- BigMortR %>%
  filter(RACE == 'Other' & `Station ID` == 'LYH') %>%
  group_by(Date) %>%
  summarise(LYHmortOther=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

OKVmortdf <- BigMortR %>%
  filter(RACE == 'Other' & `Station ID` == 'OKV') %>%
  group_by(Date) %>%
  summarise(OKVmortOther=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

ORFmortdf <- BigMortR %>%
  filter(RACE == 'Other' & `Station ID` == 'ORF') %>%
  group_by(Date) %>%
  summarise(ORFmortOther=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

PHFmortdf <- BigMortR %>%
  filter(RACE == 'Other' & `Station ID` == 'PHF') %>%
  group_by(Date) %>%
  summarise(PHFmortOther=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

RICmortdf <- BigMortR %>%
  filter(RACE == 'Other' & `Station ID` == 'RIC') %>%
  group_by(Date) %>%
  summarise(RICmortOther=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

ROAmortdf <- BigMortR %>%
  filter(RACE == 'Other' & `Station ID` == 'ROA') %>%
  group_by(Date) %>%
  summarise(ROAmortOther=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

SHDmortdf <- BigMortR %>%
  filter(RACE == 'Other' & `Station ID` == 'SHD') %>%
  group_by(Date) %>%
  summarise(SHDmortOther=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

VJImortdf <- BigMortR %>%
  filter(RACE == 'Other' & `Station ID` == 'VJI') %>%
  group_by(Date) %>%
  summarise(VJImortOther=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

CHOmortOther <- data.frame(CHOmortdf$CHOmortOther)
CHOmortOther[is.na(CHOmortOther)] <- 0

EMVmortOther <- data.frame(EMVmortdf$EMVmortOther)
EMVmortOther[is.na(EMVmortOther)] <- 0

EZFmortOther <- data.frame(EZFmortdf$EZFmortOther)
EZFmortOther[is.na(EZFmortOther)] <- 0

IADmortOther <- data.frame(IADmortdf$IADmortOther)
IADmortOther[is.na(IADmortOther)] <- 0

LYHmortOther <- data.frame(LYHmortdf$LYHmortOther)
LYHmortOther[is.na(LYHmortOther)] <- 0

OKVmortOther <- data.frame(OKVmortdf$OKVmortOther)
OKVmortOther[is.na(OKVmortOther)] <- 0

ORFmortOther <- data.frame(ORFmortdf$ORFmortOther)
ORFmortOther[is.na(ORFmortOther)] <- 0

PHFmortOther <- data.frame(PHFmortdf$PHFmortOther)
PHFmortOther[is.na(PHFmortOther)] <- 0

RICmortOther <- data.frame(RICmortdf$RICmortOther)
RICmortOther[is.na(RICmortOther)] <- 0

ROAmortOther <- data.frame(ROAmortdf$ROAmortOther)
ROAmortOther[is.na(ROAmortOther)] <- 0

SHDmortOther <- data.frame(SHDmortdf$SHDmortOther)
SHDmortOther[is.na(SHDmortOther)] <- 0

VJImortOther <- data.frame(VJImortdf$VJImortOther)
VJImortOther[is.na(VJImortOther)] <- 0

#Run Calculations for General Mortality

CHOtotalOther <- mapply('*',CHO,CHOmortOther)
EMVtotalOther <- mapply('*',EMV,EMVmortOther)
EZFtotalOther <- mapply('*',EZF,EZFmortOther)
IADtotalOther <- mapply('*',IAD,IADmortOther)
LYHtotalOther <- mapply('*',LYH,LYHmortOther)
OKVtotalOther <- mapply('*',OKV,OKVmortOther)
ORFtotalOther <- mapply('*',ORF,ORFmortOther)
PHFtotalOther <- mapply('*',PHF,PHFmortOther)
RICtotalOther <- mapply('*',RIC,RICmortOther)
ROAtotalOther <- mapply('*',ROA,ROAmortOther)
SHDtotalOther <- mapply('*',SHD,SHDmortOther)
VJItotalOther <- mapply('*',VJI,VJImortOther)

CHOtotalOther[is.na(CHOtotalOther)] <- 0
EMVtotalOther[is.na(EMVtotalOther)] <- 0
EZFtotalOther[is.na(EZFtotalOther)] <- 0
IADtotalOther[is.na(IADtotalOther)] <- 0
LYHtotalOther[is.na(LYHtotalOther)] <- 0
OKVtotalOther[is.na(OKVtotalOther)] <- 0
ORFtotalOther[is.na(ORFtotalOther)] <- 0
PHFtotalOther[is.na(PHFtotalOther)] <- 0
RICtotalOther[is.na(RICtotalOther)] <- 0
ROAtotalOther[is.na(ROAtotalOther)] <- 0
SHDtotalOther[is.na(SHDtotalOther)] <- 0
VJItotalOther[is.na(VJItotalOther)] <- 0

bigtotalOther <- CHOtotalOther+EMVtotalOther+EZFtotalOther+IADtotalOther+LYHtotalOther+OKVtotalOther+ORFtotalOther+PHFtotalOther+RICtotalOther+ROAtotalOther+SHDtotalOther+VJItotalOther

finalOtherdf <- bigtotalOther/Race$Other

finalOtherdf <- cbind(finalOtherdf, Race[c("Other")])

colnames(finalOtherdf)[54] <- "OtherMortalities"

write.csv(finalOtherdf,"StateOtherWeatherPrepAdjusted.csv",row.names = FALSE)

