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

#Black Mortality Totals
#BigMortR <- read_excel("~/Desktop/BigMort3.xlsx")
BigMortR$`Station ID` <- str_replace_all(BigMortR$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))
BigMortR$Date = as.Date(paste(BigMortR$DOD_YR,BigMortR$DOD_MO,BigMortR$DOD_DY,sep="-"))
BigMortR$Count = 1

Race <- BigMortR %>%
  filter(RACE == 'Black') %>%
  group_by(Date) %>%
  summarise(Black=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

CHOmortdf <- BigMortR %>%
  filter(RACE == 'Black' & `Station ID` == 'CHO') %>%
  group_by(Date) %>%
  summarise(CHOmortBlack=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

EMVmortdf <- BigMortR %>%
  filter(RACE == 'Black' & `Station ID` == 'EMV') %>%
  group_by(Date) %>%
  summarise(EMVmortBlack=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

EZFmortdf <- BigMortR %>%
  filter(RACE == 'Black' & `Station ID` == 'EZF') %>%
  group_by(Date) %>%
  summarise(EZFmortBlack=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

IADmortdf <- BigMortR %>%
  filter(RACE == 'Black' & `Station ID` == 'IAD') %>%
  group_by(Date) %>%
  summarise(IADmortBlack=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

LYHmortdf <- BigMortR %>%
  filter(RACE == 'Black' & `Station ID` == 'LYH') %>%
  group_by(Date) %>%
  summarise(LYHmortBlack=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

OKVmortdf <- BigMortR %>%
  filter(RACE == 'Black' & `Station ID` == 'OKV') %>%
  group_by(Date) %>%
  summarise(OKVmortBlack=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

ORFmortdf <- BigMortR %>%
  filter(RACE == 'Black' & `Station ID` == 'ORF') %>%
  group_by(Date) %>%
  summarise(ORFmortBlack=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

PHFmortdf <- BigMortR %>%
  filter(RACE == 'Black' & `Station ID` == 'PHF') %>%
  group_by(Date) %>%
  summarise(PHFmortBlack=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

RICmortdf <- BigMortR %>%
  filter(RACE == 'Black' & `Station ID` == 'RIC') %>%
  group_by(Date) %>%
  summarise(RICmortBlack=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

ROAmortdf <- BigMortR %>%
  filter(RACE == 'Black' & `Station ID` == 'ROA') %>%
  group_by(Date) %>%
  summarise(ROAmortBlack=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

SHDmortdf <- BigMortR %>%
  filter(RACE == 'Black' & `Station ID` == 'SHD') %>%
  group_by(Date) %>%
  summarise(SHDmortBlack=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

VJImortdf <- BigMortR %>%
  filter(RACE == 'Black' & `Station ID` == 'VJI') %>%
  group_by(Date) %>%
  summarise(VJImortBlack=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

CHOmortBlack <- data.frame(CHOmortdf$CHOmortBlack)
CHOmortBlack[is.na(CHOmortBlack)] <- 0

EMVmortBlack <- data.frame(EMVmortdf$EMVmortBlack)
EMVmortBlack[is.na(EMVmortBlack)] <- 0

EZFmortBlack <- data.frame(EZFmortdf$EZFmortBlack)
EZFmortBlack[is.na(EZFmortBlack)] <- 0

IADmortBlack <- data.frame(IADmortdf$IADmortBlack)
IADmortBlack[is.na(IADmortBlack)] <- 0

LYHmortBlack <- data.frame(LYHmortdf$LYHmortBlack)
LYHmortBlack[is.na(LYHmortBlack)] <- 0

OKVmortBlack <- data.frame(OKVmortdf$OKVmortBlack)
OKVmortBlack[is.na(OKVmortBlack)] <- 0

ORFmortBlack <- data.frame(ORFmortdf$ORFmortBlack)
ORFmortBlack[is.na(ORFmortBlack)] <- 0

PHFmortBlack <- data.frame(PHFmortdf$PHFmortBlack)
PHFmortBlack[is.na(PHFmortBlack)] <- 0

RICmortBlack <- data.frame(RICmortdf$RICmortBlack)
RICmortBlack[is.na(RICmortBlack)] <- 0

ROAmortBlack <- data.frame(ROAmortdf$ROAmortBlack)
ROAmortBlack[is.na(ROAmortBlack)] <- 0

SHDmortBlack <- data.frame(SHDmortdf$SHDmortBlack)
SHDmortBlack[is.na(SHDmortBlack)] <- 0

VJImortBlack <- data.frame(VJImortdf$VJImortBlack)
VJImortBlack[is.na(VJImortBlack)] <- 0

#Run Calculations for General Mortality

CHOtotalBlack <- mapply('*',CHO,CHOmortBlack)
EMVtotalBlack <- mapply('*',EMV,EMVmortBlack)
EZFtotalBlack <- mapply('*',EZF,EZFmortBlack)
IADtotalBlack <- mapply('*',IAD,IADmortBlack)
LYHtotalBlack <- mapply('*',LYH,LYHmortBlack)
OKVtotalBlack <- mapply('*',OKV,OKVmortBlack)
ORFtotalBlack <- mapply('*',ORF,ORFmortBlack)
PHFtotalBlack <- mapply('*',PHF,PHFmortBlack)
RICtotalBlack <- mapply('*',RIC,RICmortBlack)
ROAtotalBlack <- mapply('*',ROA,ROAmortBlack)
SHDtotalBlack <- mapply('*',SHD,SHDmortBlack)
VJItotalBlack <- mapply('*',VJI,VJImortBlack)

bigtotalBlack <- CHOtotalBlack+EMVtotalBlack+EZFtotalBlack+IADtotalBlack+LYHtotalBlack+OKVtotalBlack+ORFtotalBlack+PHFtotalBlack+RICtotalBlack+ROAtotalBlack+SHDtotalBlack+VJItotalBlack

finalBlackdf <- bigtotalBlack/Race$Black

finalBlackdf <- cbind(finalBlackdf, Race[c("Black")])

colnames(finalBlackdf)[54] <- "BlackMortalities"

#write.csv(finalBlackdf,"StateBlackWeatherPrepAdjusted.csv",row.names = FALSE)



