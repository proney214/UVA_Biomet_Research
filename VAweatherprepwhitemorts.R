### Creating State-Wide Weather Data Adjusted for White mortality

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


#White Mortality Totals
BigMortR <- read_excel("~/Desktop/BigMort3.xlsx")
BigMortR$`Station ID` <- str_replace_all(BigMortR$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))
BigMortR$Date = as.Date(paste(BigMortR$DOD_YR,BigMortR$DOD_MO,BigMortR$DOD_DY,sep="-"))
BigMortR$Count = 1

Race <- BigMortR %>%
  filter(RACE == 'White') %>%
  group_by(Date) %>%
  summarise(White=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

CHOmortdf <- BigMortR %>%
  filter(RACE == 'White' & `Station ID` == 'CHO') %>%
  group_by(Date) %>%
  summarise(CHOmortWhite=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

EMVmortdf <- BigMortR %>%
  filter(RACE == 'White' & `Station ID` == 'EMV') %>%
  group_by(Date) %>%
  summarise(EMVmortWhite=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

EZFmortdf <- BigMortR %>%
  filter(RACE == 'White' & `Station ID` == 'EZF') %>%
  group_by(Date) %>%
  summarise(EZFmortWhite=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

IADmortdf <- BigMortR %>%
  filter(RACE == 'White' & `Station ID` == 'IAD') %>%
  group_by(Date) %>%
  summarise(IADmortWhite=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

LYHmortdf <- BigMortR %>%
  filter(RACE == 'White' & `Station ID` == 'LYH') %>%
  group_by(Date) %>%
  summarise(LYHmortWhite=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

OKVmortdf <- BigMortR %>%
  filter(RACE == 'White' & `Station ID` == 'OKV') %>%
  group_by(Date) %>%
  summarise(OKVmortWhite=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

ORFmortdf <- BigMortR %>%
  filter(RACE == 'White' & `Station ID` == 'ORF') %>%
  group_by(Date) %>%
  summarise(ORFmortWhite=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

PHFmortdf <- BigMortR %>%
  filter(RACE == 'White' & `Station ID` == 'PHF') %>%
  group_by(Date) %>%
  summarise(PHFmortWhite=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

RICmortdf <- BigMortR %>%
  filter(RACE == 'White' & `Station ID` == 'RIC') %>%
  group_by(Date) %>%
  summarise(RICmortWhite=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

ROAmortdf <- BigMortR %>%
  filter(RACE == 'White' & `Station ID` == 'ROA') %>%
  group_by(Date) %>%
  summarise(ROAmortWhite=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

SHDmortdf <- BigMortR %>%
  filter(RACE == 'White' & `Station ID` == 'SHD') %>%
  group_by(Date) %>%
  summarise(SHDmortWhite=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

VJImortdf <- BigMortR %>%
  filter(RACE == 'White' & `Station ID` == 'VJI') %>%
  group_by(Date) %>%
  summarise(VJImortWhite=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

CHOmortWhite <- data.frame(CHOmortdf$CHOmortWhite)
CHOmortWhite[is.na(CHOmortWhite)] <- 0

EMVmortWhite <- data.frame(EMVmortdf$EMVmortWhite)
EMVmortWhite[is.na(EMVmortWhite)] <- 0

EZFmortWhite <- data.frame(EZFmortdf$EZFmortWhite)
EZFmortWhite[is.na(EZFmortWhite)] <- 0

IADmortWhite <- data.frame(IADmortdf$IADmortWhite)
IADmortWhite[is.na(IADmortWhite)] <- 0

LYHmortWhite <- data.frame(LYHmortdf$LYHmortWhite)
LYHmortWhite[is.na(LYHmortWhite)] <- 0

OKVmortWhite <- data.frame(OKVmortdf$OKVmortWhite)
OKVmortWhite[is.na(OKVmortWhite)] <- 0

ORFmortWhite <- data.frame(ORFmortdf$ORFmortWhite)
ORFmortWhite[is.na(ORFmortWhite)] <- 0

PHFmortWhite <- data.frame(PHFmortdf$PHFmortWhite)
PHFmortWhite[is.na(PHFmortWhite)] <- 0

RICmortWhite <- data.frame(RICmortdf$RICmortWhite)
RICmortWhite[is.na(RICmortWhite)] <- 0

ROAmortWhite <- data.frame(ROAmortdf$ROAmortWhite)
ROAmortWhite[is.na(ROAmortWhite)] <- 0

SHDmortWhite <- data.frame(SHDmortdf$SHDmortWhite)
SHDmortWhite[is.na(SHDmortWhite)] <- 0

VJImortWhite <- data.frame(VJImortdf$VJImortWhite)
VJImortWhite[is.na(VJImortWhite)] <- 0

#Run Calculations for General Mortality

CHOtotalWhite <- mapply('*',CHO,CHOmortWhite)
EMVtotalWhite <- mapply('*',EMV,EMVmortWhite)
EZFtotalWhite <- mapply('*',EZF,EZFmortWhite)
IADtotalWhite <- mapply('*',IAD,IADmortWhite)
LYHtotalWhite <- mapply('*',LYH,LYHmortWhite)
OKVtotalWhite <- mapply('*',OKV,OKVmortWhite)
ORFtotalWhite <- mapply('*',ORF,ORFmortWhite)
PHFtotalWhite <- mapply('*',PHF,PHFmortWhite)
RICtotalWhite <- mapply('*',RIC,RICmortWhite)
ROAtotalWhite <- mapply('*',ROA,ROAmortWhite)
SHDtotalWhite <- mapply('*',SHD,SHDmortWhite)
VJItotalWhite <- mapply('*',VJI,VJImortWhite)

bigtotalWhite <- CHOtotalWhite+EMVtotalWhite+EZFtotalWhite+IADtotalWhite+LYHtotalWhite+OKVtotalWhite+ORFtotalWhite+PHFtotalWhite+RICtotalWhite+ROAtotalWhite+SHDtotalWhite+VJItotalWhite

finalWhitedf <- bigtotalWhite/Race$White

finalWhitedf <- cbind(finalWhitedf, Race[c("White")])

colnames(finalWhitedf)[54] <- "WhiteMortalities"

#write.csv(finalWhitedf,"StateWhiteWeatherPrepAdjusted.csv",row.names = FALSE)

