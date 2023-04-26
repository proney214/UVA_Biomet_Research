### Creating State-Wide Weather Data Adjusted for mortality (INCLUDES STATE AND ELDERLY)

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

#Choose your mortality data
#Raw Mortality Totals
deaths_station_date_2005_2020_v2 <- read_excel("~/Desktop/deaths_station_date_2005_2020_v2.xlsx")

mort <- select(deaths_station_date_2005_2020_v2,2:16)
mortrowsum <- rowSums(mort)

CHOmort <- deaths_station_date_2005_2020_v2[,3]
EMVmort <- deaths_station_date_2005_2020_v2[,4]+deaths_station_date_2005_2020_v2[,9]
EZFmort <- deaths_station_date_2005_2020_v2[,5]
IADmort <- deaths_station_date_2005_2020_v2[,6]
LYHmort <- deaths_station_date_2005_2020_v2[,7]
OKVmort <- deaths_station_date_2005_2020_v2[,10]
ORFmort <- deaths_station_date_2005_2020_v2[,11]
PHFmort <- deaths_station_date_2005_2020_v2[,12]+deaths_station_date_2005_2020_v2[,8]
RICmort <- deaths_station_date_2005_2020_v2[,13]
ROAmort <- deaths_station_date_2005_2020_v2[,14]
SHDmort <- deaths_station_date_2005_2020_v2[,15]
VJImort <- deaths_station_date_2005_2020_v2[,16]+deaths_station_date_2005_2020_v2[,2]

#Run Calculations for General Mortality

CHOtotal <- mapply('*',CHO,CHOmort)
EMVtotal <- mapply('*',EMV,EMVmort)
EZFtotal <- mapply('*',EZF,EZFmort)
IADtotal <- mapply('*',IAD,IADmort)
LYHtotal <- mapply('*',LYH,LYHmort)
OKVtotal <- mapply('*',OKV,OKVmort)
ORFtotal <- mapply('*',ORF,ORFmort)
PHFtotal <- mapply('*',PHF,PHFmort)
RICtotal <- mapply('*',RIC,RICmort)
ROAtotal <- mapply('*',ROA,ROAmort)
SHDtotal <- mapply('*',SHD,SHDmort)
VJItotal <- mapply('*',VJI,VJImort)

bigtotal <- CHOtotal+EMVtotal+EZFtotal+IADtotal+LYHtotal+OKVtotal+ORFtotal+PHFtotal+RICtotal+ROAtotal+SHDtotal+VJItotal

finaldf <- bigtotal/mortrowsum

write.csv(finaldf,"StateWeatherPrepAdjusted.csv",row.names = FALSE)

#75+ Mortality Totals
BigMortR <- read_excel("~/Desktop/BigMort3.xlsx")
BigMortR$`Station ID` <- str_replace_all(BigMortR$`Station ID`, c("BLF"= "VJI","MFV"="PHF","MTV"="EMV"))
BigMortR$Date = as.Date(paste(BigMortR$DOD_YR,BigMortR$DOD_MO,BigMortR$DOD_DY,sep="-"))
BigMortR$Count = 1

Elderly <- BigMortR %>%
  filter(AGE_GROUPS == '75+') %>%
  group_by(Date) %>%
  summarise(total75=sum(Count))

CHOmortdf <- BigMortR %>%
  filter(AGE_GROUPS == '75+' & `Station ID` == 'CHO') %>%
  group_by(Date) %>%
  summarise(CHOmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

EMVmortdf <- BigMortR %>%
  filter(AGE_GROUPS == '75+' & `Station ID` == 'EMV') %>%
  group_by(Date) %>%
  summarise(EMVmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

EZFmortdf <- BigMortR %>%
  filter(AGE_GROUPS == '75+' & `Station ID` == 'EZF') %>%
  group_by(Date) %>%
  summarise(EZFmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

IADmortdf <- BigMortR %>%
  filter(AGE_GROUPS == '75+' & `Station ID` == 'IAD') %>%
  group_by(Date) %>%
  summarise(IADmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

LYHmortdf <- BigMortR %>%
  filter(AGE_GROUPS == '75+' & `Station ID` == 'LYH') %>%
  group_by(Date) %>%
  summarise(LYHmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

OKVmortdf <- BigMortR %>%
  filter(AGE_GROUPS == '75+' & `Station ID` == 'OKV') %>%
  group_by(Date) %>%
  summarise(OKVmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

ORFmortdf <- BigMortR %>%
  filter(AGE_GROUPS == '75+' & `Station ID` == 'ORF') %>%
  group_by(Date) %>%
  summarise(ORFmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

PHFmortdf <- BigMortR %>%
  filter(AGE_GROUPS == '75+' & `Station ID` == 'PHF') %>%
  group_by(Date) %>%
  summarise(PHFmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

RICmortdf <- BigMortR %>%
  filter(AGE_GROUPS == '75+' & `Station ID` == 'RIC') %>%
  group_by(Date) %>%
  summarise(RICmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

ROAmortdf <- BigMortR %>%
  filter(AGE_GROUPS == '75+' & `Station ID` == 'ROA') %>%
  group_by(Date) %>%
  summarise(ROAmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

SHDmortdf <- BigMortR %>%
  filter(AGE_GROUPS == '75+' & `Station ID` == 'SHD') %>%
  group_by(Date) %>%
  summarise(SHDmorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

VJImortdf <- BigMortR %>%
  filter(AGE_GROUPS == '75+' & `Station ID` == 'VJI') %>%
  group_by(Date) %>%
  summarise(VJImorteld=sum(Count))%>%
  pad(interval = "day",start_val = as.POSIXct('2005-01-01'),end_val = as.POSIXct('2020-12-31'))

CHOmorteld <- data.frame(CHOmortdf$CHOmorteld)
CHOmorteld[is.na(CHOmorteld)] <- 0

EMVmorteld <- data.frame(EMVmortdf$EMVmorteld)
EMVmorteld[is.na(EMVmorteld)] <- 0

EZFmorteld <- data.frame(EZFmortdf$EZFmorteld)
EZFmorteld[is.na(EZFmorteld)] <- 0

IADmorteld <- data.frame(IADmortdf$IADmorteld)
IADmorteld[is.na(IADmorteld)] <- 0

LYHmorteld <- data.frame(LYHmortdf$LYHmorteld)
LYHmorteld[is.na(LYHmorteld)] <- 0

OKVmorteld <- data.frame(OKVmortdf$OKVmorteld)
OKVmorteld[is.na(OKVmorteld)] <- 0

ORFmorteld <- data.frame(ORFmortdf$ORFmorteld)
ORFmorteld[is.na(ORFmorteld)] <- 0

PHFmorteld <- data.frame(PHFmortdf$PHFmorteld)
PHFmorteld[is.na(PHFmorteld)] <- 0

RICmorteld <- data.frame(RICmortdf$RICmorteld)
RICmorteld[is.na(RICmorteld)] <- 0

ROAmorteld <- data.frame(ROAmortdf$ROAmorteld)
ROAmorteld[is.na(ROAmorteld)] <- 0

SHDmorteld <- data.frame(SHDmortdf$SHDmorteld)
SHDmorteld[is.na(SHDmorteld)] <- 0

VJImorteld <- data.frame(VJImortdf$VJImorteld)
VJImorteld[is.na(VJImorteld)] <- 0

#Run Calculations for General Mortality

CHOtotaleld <- mapply('*',CHO,CHOmorteld)
EMVtotaleld <- mapply('*',EMV,EMVmorteld)
EZFtotaleld <- mapply('*',EZF,EZFmorteld)
IADtotaleld <- mapply('*',IAD,IADmorteld)
LYHtotaleld <- mapply('*',LYH,LYHmorteld)
OKVtotaleld <- mapply('*',OKV,OKVmorteld)
ORFtotaleld <- mapply('*',ORF,ORFmorteld)
PHFtotaleld <- mapply('*',PHF,PHFmorteld)
RICtotaleld <- mapply('*',RIC,RICmorteld)
ROAtotaleld <- mapply('*',ROA,ROAmorteld)
SHDtotaleld <- mapply('*',SHD,SHDmorteld)
VJItotaleld <- mapply('*',VJI,VJImorteld)

bigtotaleld <- CHOtotaleld+EMVtotaleld+EZFtotaleld+IADtotaleld+LYHtotaleld+OKVtotaleld+ORFtotaleld+PHFtotaleld+RICtotaleld+ROAtotaleld+SHDtotaleld+VJItotaleld

finalelddf <- bigtotaleld/Elderly$total75

finalelddf <- cbind(finalelddf, Elderly[c("total75")])

colnames(finalelddf)[54] <- "ElderlyMortalities"

write.csv(finalelddf,"StateElderlyWeatherPrepAdjusted.csv",row.names = FALSE)


#Calculate, but no longer need this loop, took 40 minutes to calculate :(

#for(j in 7:53) {
#  for(i in 1:5844) {
#    print(i,j)
#    VA[i,j] = ((CHO[i,j]*CHOmort[[i,1]])+(EMV[i,j]*EMVmort[[i,1]])+(EZF[i,j]*EZFmort[[i,1]])+(IAD[i,j]*IADmort[[i,1]])+
#                 (LYH[i,j]*LYHmort[[i,1]])+(OKV[i,j]*OKVmort[[i,1]])+(ORF[i,j]*ORFmort[[i,1]])+(PHF[i,j]*PHFmort[[i,1]])+(RIC[i,j]*RICmort[[i,1]])+
#                 (ROA[i,j]*ROAmort[[i,1]])+(SHD[i,j]*SHDmort[[i,1]])+(VJI[i,j]*VJImort[[i,1]]))/(CHOmort[[i,1]]+EMVmort[[i,1]]+EZFmort[[i,1]]+
#                                                                                             IADmort[[i,1]]+LYHmort[[i,1]]+OKVmort[[i,1]]+
#                                                                                             ORFmort[[i,1]]+PHFmort[[i,1]]+RICmort[[i,1]]+ROAmort[[i,1]]+
#                                                                                             SHDmort[[i,1]]+VJImort[[i,1]])
#  }
#}

