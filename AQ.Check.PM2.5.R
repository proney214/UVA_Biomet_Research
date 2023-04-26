#Air Quality Check PM2.5 

#Load in data here
library(readxl)
library(dplyr)
library(lubridate)
library(padr)

## CHO Check

CHO <- read_excel("~/Desktop/CHO.PM2.5.Combined.xlsx", sheet="Sheet1",na="NA")

CHOAlbemarle <- CHO %>%
  filter(`Site Name` == "Albemarle High School")
boxplot(CHOAlbemarle$`Daily Mean PM2.5 Concentration`)
CHOAlbemarlemax <- max(CHOAlbemarle$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
CHOAlbemarlemean <- mean(CHOAlbemarle$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
CHOAlbemarlesd <- sd(CHOAlbemarle$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
CHOAlbemarlestats <- cbind(CHOAlbemarlemax,CHOAlbemarlemean,CHOAlbemarlesd)

CHOFinal <- rbind(CHOAlbemarlestats)
colnames(CHOFinal) <- c("CHO Max","Mean","Standard Deviation")

## IAD Check

IAD2 <- read_excel("~/Desktop/DC.PM2.5.Combined-1.xlsx", sheet="Sheet1",na="NA")

IADAlexandria <- IAD2 %>%
  filter(`Site Name` == "Alexandria Health Dept.")
boxplot(IADAlexandria$`Daily Mean PM2.5 Concentration`)
IADAlexandriamax <- max(IADAlexandria$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADAlexandriamean <- mean(IADAlexandria$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADAlexandriasd <- sd(IADAlexandria$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADAlexandriastats <- cbind(IADAlexandriamax,IADAlexandriamean,IADAlexandriasd)

IADAurora <- IAD2 %>%
  filter(`Site Name` == "Aurora Hills Visitors Center")
boxplot(IADAurora$`Daily Mean PM2.5 Concentration`)
IADAuroramax <- max(IADAurora$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADAuroramean <- mean(IADAurora$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADAurorasd <- sd(IADAurora$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADAurorastats <- cbind(IADAuroramax,IADAuroramean,IADAurorasd)

IADAshburn <- IAD2 %>%
  filter(`Site Name` == "Broad Run High School, Ashburn")
boxplot(IADAshburn$`Daily Mean PM2.5 Concentration`)
IADAshburnmax <- max(IADAshburn$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADAshburnmean <- mean(IADAshburn$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADAshburnsd <- sd(IADAshburn$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADAshburnstats <- cbind(IADAshburnmax,IADAshburnmean,IADAshburnsd)

IADLeeDistrict <- IAD2 %>%
  filter(`Site Name` == "Lee District Park")
boxplot(IADLeeDistrict$`Daily Mean PM2.5 Concentration`)
IADLeeDistrictmax <- max(IADLeeDistrict$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADLeeDistrictmean <- mean(IADLeeDistrict$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADLeeDistrictsd <- sd(IADLeeDistrict$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADLeeDistrictstats <- cbind(IADLeeDistrictmax,IADLeeDistrictmean,IADLeeDistrictsd)

IADSpringfield <- IAD2 %>%
  filter(`Site Name` == "Springfield Near Road Site")
boxplot(IADSpringfield$`Daily Mean PM2.5 Concentration`)
IADSpringfieldmax <- max(IADSpringfield$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADSpringfieldmean <- mean(IADSpringfield$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADSpringfieldsd <- sd(IADSpringfield$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
IADSpringfieldstats <- cbind(IADSpringfieldmax,IADSpringfieldmean,IADSpringfieldsd)

IADFinal <- rbind(IADAlexandriastats, IADAurorastats, IADAshburnstats,IADLeeDistrictstats,IADSpringfieldstats)
colnames(IADFinal) <- c("IAD Max","Mean","Standard Deviation")

## LYH Check

LYH2 <- read_excel("~/Desktop/LYH.PM2.5.Combined-1.xlsx", sheet="LYH 2005-2020",na="NA")

LYHWaterTower <- LYH2 %>%
  filter(`Site Name` == "LYNCHBURG CITY WATER TOWER")
boxplot(LYHWaterTower$`Daily Mean PM2.5 Concentration`)
LYHWaterTowermax <- max(LYHWaterTower$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
LYHWaterTowermean <- mean(LYHWaterTower$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
LYHWaterTowersd <- sd(LYHWaterTower$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
LYHWaterTowerstats <- cbind(LYHWaterTowermax,LYHWaterTowermean,LYHWaterTowersd)

LYHFinal <- rbind(LYHWaterTowerstats)
colnames(LYHFinal) <- c("LYH Max","Mean","Standard Deviation")


## ORF Check

ORF2 <- read_excel("~/Desktop/VABeach.PM2.5.Combined.xlsx", sheet="Raw",na="NA")

ORFDEQ <- ORF2 %>%
  filter(`Site Name` == "DEQ Tidewater Regional Office")
boxplot(ORFDEQ$`Daily Mean PM2.5 Concentration`)
ORFDEQmax <- max(ORFDEQ$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ORFDEQmean <- mean(ORFDEQ$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ORFDEQsd <- sd(ORFDEQ$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ORFDEQstats <- cbind(ORFDEQmax,ORFDEQmean,ORFDEQsd)

ORFNOAA <- ORF2 %>%
  filter(`Site Name` == "NOAA")
boxplot(ORFNOAA$`Daily Mean PM2.5 Concentration`)
ORFNOAAmax <- max(ORFNOAA$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ORFNOAAmean <- mean(ORFNOAA$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ORFNOAAsd <- sd(ORFNOAA$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ORFNOAAstats <- cbind(ORFNOAAmax,ORFNOAAmean,ORFNOAAsd)

ORFFinal <- rbind(ORFDEQstats,ORFNOAAstats)
colnames(ORFFinal) <- c("ORF Max","Mean","Standard Deviation")

## PHF Check

PHF2 <- read_excel("~/Desktop/VABeach.PM2.5.Combined.xlsx", sheet="Raw",na="NA")

PHFNASA <- PHF2 %>%
  filter(`Site Name` == "NASA Langley Research Center")
boxplot(PHFNASA$`Daily Mean PM2.5 Concentration`)
PHFNASAmax <- max(PHFNASA$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
PHFNASAmean <- mean(PHFNASA$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
PHFNASAsd <- sd(PHFNASA$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
PHFNASAstats <- cbind(PHFNASAmax,PHFNASAmean,PHFNASAsd)

PHFSchool <- PHF2 %>%
  filter(`Site Name` == "ON NEWPORT NEWS SCHOOL MAINTENANCE FACILITY")
boxplot(PHFSchool$`Daily Mean PM2.5 Concentration`)
PHFSchoolmax <- max(PHFSchool$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
PHFSchoolmean <- mean(PHFSchool$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
PHFSchoolsd <- sd(PHFSchool$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
PHFSchoolstats <- cbind(PHFSchoolmax,PHFSchoolmean,PHFSchoolsd)

PHFFinal <- rbind(PHFNASAstats,PHFSchoolstats)
colnames(PHFFinal) <- c("PHF Max","Mean","Standard Deviation")

## RIC Check
RIC2 <- read_excel("~/Desktop/RIC.PM2.5.Combined-1.xlsx", sheet="Sheet1",na="NA")

RICShirley <- RIC2 %>%
  filter(`Site Name` == "Shirley Plantation")
boxplot(RICShirley$`Daily Mean PM2.5 Concentration`)
RICShirleymax <- max(RICShirley$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICShirleymean <- mean(RICShirley$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICShirleysd <- sd(RICShirley$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICShirleystats <- cbind(RICShirleymax,RICShirleymean,RICShirleysd)

RICBensley <- RIC2 %>%
  filter(`Site Name` == "Bensley Armory")
boxplot(RICBensley$`Daily Mean PM2.5 Concentration`)
RICBensleymax <- max(RICBensley$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICBensleymean <- mean(RICBensley$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICBensleysd <- sd(RICBensley$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICBensleystats <- cbind(RICBensleymax,RICBensleymean,RICBensleysd)

RICVDOT <- RIC2 %>%
  filter(`Site Name` == "VDOT Chesterfield Residency Shop")
boxplot(RICVDOT$`Daily Mean PM2.5 Concentration`)
RICVDOTmax <- max(RICVDOT$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICVDOTmean <- mean(RICVDOT$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICVDOTsd <- sd(RICVDOT$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICVDOTstats <- cbind(RICVDOTmax,RICVDOTmean,RICVDOTsd)

RICMathScience <- RIC2 %>%
  filter(`Site Name` == "MathScience Innovation Center")
boxplot(RICMathScience$`Daily Mean PM2.5 Concentration`)
RICMathSciencemax <- max(RICMathScience$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICMathSciencemean <- mean(RICMathScience$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICMathSciencesd <- sd(RICMathScience$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICMathSciencestats <- cbind(RICMathSciencemax,RICMathSciencemean,RICMathSciencesd)

RICDEQ <- RIC2 %>%
  filter(`Site Name` == "DEQ Piedmont Regional Office")
boxplot(RICDEQ$`Daily Mean PM2.5 Concentration`)
RICDEQmax <- max(RICDEQ$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICDEQmean <- mean(RICDEQ$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICDEQsd <- sd(RICDEQ$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
RICDEQstats <- cbind(RICDEQmax,RICDEQmean,RICDEQsd)

RICFinal <- rbind(RICShirleystats, RICBensleystats, RICDEQstats,RICVDOTstats,RICMathSciencestats)
colnames(RICFinal) <- c("RIC Max","Mean","Standard Deviation")

## ROA Check

ROA2 <- read_excel("~/Desktop/ROA.PM2.5.Combined.xlsx", sheet="Sheet1",na="NA")

ROARaleighCourt <- ROA2 %>%
  filter(`Site Name` == "ROOF OF RALEIGH COURT LIBRARY")
boxplot(ROARaleighCourt$`Daily Mean PM2.5 Concentration`)
ROARaleighCourtmax <- max(ROARaleighCourt$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROARaleighCourtmean <- mean(ROARaleighCourt$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROARaleighCourtsd <- sd(ROARaleighCourt$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROARaleighCourtstats <- cbind(ROARaleighCourtmax,ROARaleighCourtmean,ROARaleighCourtsd)

ROAMarketFire <- ROA2 %>%
  filter(`Site Name` == "ON ROOF OF MARKET STREET FIRE STATION")
boxplot(ROAMarketFire$`Daily Mean PM2.5 Concentration`)
ROAMarketFiremax <- max(ROAMarketFire$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROAMarketFiremean <- mean(ROAMarketFire$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROAMarketFiresd <- sd(ROAMarketFire$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROAMarketFirestats <- cbind(ROAMarketFiremax,ROAMarketFiremean,ROAMarketFiresd)

ROAMontessori <- ROA2 %>%
  filter(`Site Name` == "ROUND HILL MONTESSORI SCHOOL")
boxplot(ROAMontessori$`Daily Mean PM2.5 Concentration`)
ROAMontessorimax <- max(ROAMontessori$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROAMontessorimean <- mean(ROAMontessori$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROAMontessorisd <- sd(ROAMontessori$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROAMontessoristats <- cbind(ROAMontessorimax,ROAMontessorimean,ROAMontessorisd)

ROASalem <- ROA2 %>%
  filter(`Site Name` == "Salem High School")
boxplot(ROASalem$`Daily Mean PM2.5 Concentration`)
ROASalemmax <- max(ROASalem$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROASalemmean <- mean(ROASalem$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROASalemsd <- sd(ROASalem$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROASalemstats <- cbind(ROASalemmax,ROASalemmean,ROASalemsd)

ROAVinton <- ROA2 %>%
  filter(`Site Name` == "East Vinton Elementary School")
boxplot(ROAVinton$`Daily Mean PM2.5 Concentration`)
ROAVintonmax <- max(ROAVinton$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROAVintonmean <- mean(ROAVinton$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROAVintonsd <- sd(ROAVinton$`Daily Mean PM2.5 Concentration`,na.rm = TRUE)
ROAVintonstats <- cbind(ROAVintonmax,ROAVintonmean,ROAVintonsd)

ROAFinal <- rbind(ROARaleighCourtstats, ROAMarketFirestats, ROAVintonstats,ROAMontessoristats,ROASalemstats)
colnames(ROAFinal) <- c("ROAMax","Mean","Standard Deviation")

