## Ozone Air Quality Check

library(readxl)
library(dplyr)
library(lubridate)
library(padr)

## EZF Check

EZFOz <- read_excel("~/Desktop/EZF.Ozone.Combined.xlsx", sheet="Sheet1",na="NA")
unique(EZFOz$`Site Name`)

EZFSumerduck <- EZFOz %>%
  filter(`Site Name` == "Chester Phelps Wildlife Management Area, Sumerduck")
boxplot(EZFSumerduck$`Daily Max 8-hour Ozone Concentration`)
EZFSumerduckmax <- max(EZFSumerduck$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
EZFSumerduckmean <- mean(EZFSumerduck$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
EZFSumerducksd <- sd(EZFSumerduck$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
EZFSumerduckstats <- cbind(EZFSumerduckmax,EZFSumerduckmean,EZFSumerducksd)

EZFWidewater <- EZFOz %>%
  filter(`Site Name` == "Widewater Elementary School")
boxplot(EZFWidewater$`Daily Max 8-hour Ozone Concentration`)
EZFWidewatermax <- max(EZFWidewater$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
EZFWidewatermean <- mean(EZFWidewater$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
EZFWidewatersd <- sd(EZFWidewater$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
EZFWidewaterstats <- cbind(EZFWidewatermax,EZFWidewatermean,EZFWidewatersd)

EZFFinal <- rbind(EZFSumerduckstats, EZFWidewaterstats)
colnames(EZFFinal) <- c("EZF Max","Mean","Standard Deviation")

## IAD Ozone

IADOz <- read_excel("~/Desktop/DC.Ozone.Combined-1.xlsx", sheet="Sheet1",na="NA")

IADAlexandria <- IADOz %>%
  filter(`Site Name` == "Alexandria Health Dept.")
boxplot(IADAlexandria$`Daily Max 8-hour Ozone Concentration`)
IADAlexandriamax <- max(IADAlexandria$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADAlexandriamean <- mean(IADAlexandria$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADAlexandriasd <- sd(IADAlexandria$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADAlexandriastats <- cbind(IADAlexandriamax,IADAlexandriamean,IADAlexandriasd)

IADAurora <- IADOz %>%
  filter(`Site Name` == "Aurora Hills Visitors Center")
boxplot(IADAurora$`Daily Max 8-hour Ozone Concentration`)
IADAuroramax <- max(IADAurora$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADAuroramean <- mean(IADAurora$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADAurorasd <- sd(IADAurora$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADAurorastats <- cbind(IADAuroramax,IADAuroramean,IADAurorasd)

IADAshburn <- IADOz %>%
  filter(`Site Name` == "Broad Run High School, Ashburn")
boxplot(IADAshburn$`Daily Max 8-hour Ozone Concentration`)
IADAshburnmax <- max(IADAshburn$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADAshburnmean <- mean(IADAshburn$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADAshburnsd <- sd(IADAshburn$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADAshburnstats <- cbind(IADAshburnmax,IADAshburnmean,IADAshburnsd)


IADLeeDistrict <- IADOz %>%
  filter(`Site Name` == "Lee District Park")
boxplot(IADLeeDistrict$`Daily Max 8-hour Ozone Concentration`)
IADLeeDistrictmax <- max(IADLeeDistrict$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADLeeDistrictmean <- mean(IADLeeDistrict$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADLeeDistrictsd <- sd(IADLeeDistrict$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADLeeDistrictstats <- cbind(IADLeeDistrictmax,IADLeeDistrictmean,IADLeeDistrictsd)

IADJames <- IADOz %>%
  filter(`Site Name` == "James S. Long Park")
boxplot(IADJames$`Daily Max 8-hour Ozone Concentration`)
IADJamesmax <- max(IADJames$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADJamesmean <- mean(IADJames$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADJamessd <- sd(IADJames$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADJamesstats <- cbind(IADJamesmax,IADJamesmean,IADJamessd)

IADTransportation <- IADOz %>%
  filter(`Site Name` == "City of Alexandria Transportation and Env. Services Maintenance Bldg")
boxplot(IADTransportation$`Daily Max 8-hour Ozone Concentration`)
IADTransportationmax <- max(IADTransportation$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADTransportationmean <- mean(IADTransportation$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADTransportationsd <- sd(IADTransportation$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
IADTransportationstats <- cbind(IADTransportationmax,IADTransportationmean,IADTransportationsd)

IADFinal <- rbind(IADAlexandriastats, IADAurorastats,IADLeeDistrictstats,IADJamesstats, IADTransportationstats)
colnames(IADFinal) <- c("IAD Max","Mean","Standard Deviation")

## ORF Ozone

ORFOz <- read_excel("~/Desktop/ORF.Ozone.Combined.xlsx", sheet="Sheet1",na="NA")

ORFTech <- ORFOz %>%
  filter(`Site Name` == "VA Tech Agricultural Research Station, Holland")
boxplot(ORFTech$`Daily Max 8-hour Ozone Concentration`)
ORFTechmax <- max(ORFTech$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
ORFTechmean <- mean(ORFTech$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
ORFTechsd <- sd(ORFTech$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
ORFTechstats <- cbind(ORFTechmax,ORFTechmean,ORFTechsd)

ORFFinal <- rbind(ORFTechstats)
colnames(ORFFinal) <- c("ORF Max","Mean","Standard Deviation")

## PHF Ozone Check

PHFOz <- read_excel("~/Desktop/VABeach.Ozone.Combined-1.xlsx", sheet="Sheet1",na="NA")

PHFNASA <- PHFOz %>%
  filter(`Site Name` == "NASA Langley Research Center")
boxplot(PHFNASA$`Daily Max 8-hour Ozone Concentration`)
PHFNASAmax <- max(PHFNASA$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
PHFNASAmean <- mean(PHFNASA$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
PHFNASAsd <- sd(PHFNASA$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
PHFNASAstats <- cbind(PHFNASAmax,PHFNASAmean,PHFNASAsd)

PHFSchool <- PHFOz %>%
  filter(`Site Name` == "ON NEWPORT NEWS SCHOOL MAINTENANCE FACILITY")
boxplot(PHFSchool$`Daily Max 8-hour Ozone Concentration`)
PHFSchoolmax <- max(PHFSchool$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
PHFSchoolmean <- mean(PHFSchool$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
PHFSchoolsd <- sd(PHFSchool$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
PHFSchoolstats <- cbind(PHFSchoolmax,PHFSchoolmean,PHFSchoolsd)

PHFTidewater <- PHFOz %>%
  filter(`Site Name` == "Tidewater Community College")
boxplot(PHFTidewater$`Daily Max 8-hour Ozone Concentration`)
PHFTidewatermax <- max(PHFTidewater$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
PHFTidewatermean <- mean(PHFTidewater$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
PHFTidewatersd <- sd(PHFTidewater$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
PHFTidewaterstats <- cbind(PHFTidewatermax,PHFTidewatermean,PHFTidewatersd)

PHFFinal <- rbind(PHFNASAstats,PHFSchoolstats,PHFTidewaterstats)
colnames(PHFFinal) <- c("PHF Max","Mean","Standard Deviation")

## RIC Check

RICOz <- read_excel("~/Desktop/RIC.Ozone.Combined.xlsx", sheet="Sheet1",na="NA")

RICShirley <- RICOz %>%
  filter(`Site Name` == "Shirley Plantation")
boxplot(RICShirley$`Daily Max 8-hour Ozone Concentration`)
RICShirleymax <- max(RICShirley$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICShirleymean <- mean(RICShirley$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICShirleysd <- sd(RICShirley$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICShirleystats <- cbind(RICShirleymax,RICShirleymean,RICShirleysd)

RICUSGS <- RICOz %>%
  filter(`Site Name` == "USGS Geomagnetic Center, Corbin")
boxplot(RICUSGS$`Daily Max 8-hour Ozone Concentration`)
RICUSGSmax <- max(RICUSGS$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICUSGSmean <- mean(RICUSGS$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICUSGSsd <- sd(RICUSGS$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICUSGSstats <- cbind(RICUSGSmax,RICUSGSmean,RICUSGSsd)

RICVDOT <- RICOz %>%
  filter(`Site Name` == "VDOT Chesterfield Residency Shop")
boxplot(RICVDOT$`Daily Max 8-hour Ozone Concentration`)
RICVDOTmax <- max(RICVDOT$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICVDOTmean <- mean(RICVDOT$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICVDOTsd <- sd(RICVDOT$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICVDOTstats <- cbind(RICVDOTmax,RICVDOTmean,RICVDOTsd)

RICMathScience <- RICOz %>%
  filter(`Site Name` == "MathScience Innovation Center")
boxplot(RICMathScience$`Daily Max 8-hour Ozone Concentration`)
RICMathSciencemax <- max(RICMathScience$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICMathSciencemean <- mean(RICMathScience$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICMathSciencesd <- sd(RICMathScience$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICMathSciencestats <- cbind(RICMathSciencemax,RICMathSciencemean,RICMathSciencesd)

RICTurner <- RICOz %>%
  filter(`Site Name` == "Turner Property, Old Church")
boxplot(RICTurner$`Daily Max 8-hour Ozone Concentration`)
RICTurnermax <- max(RICTurner$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICTurnermean <- mean(RICTurner$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICTurnersd <- sd(RICTurner$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
RICTurnerstats <- cbind(RICTurnermax,RICTurnermean,RICTurnersd)

RICFinal <- rbind(RICShirleystats, RICUSGSstats, RICTurnerstats,RICVDOTstats,RICMathSciencestats)
colnames(RICFinal) <- c("RIC Max","Mean","Standard Deviation")

## ROA Check

ROAOz <- read_excel("~/Desktop/ROA.Ozone.Combined.xlsx", sheet="Sheet1",na="NA")

ROAVinton <- ROAOz %>%
  filter(`Site Name` == "East Vinton Elementary School")
boxplot(ROAVinton$`Daily Max 8-hour Ozone Concentration`)
ROAVintonmax <- max(ROAVinton$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
ROAVintonmean <- mean(ROAVinton$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
ROAVintonsd <- sd(ROAVinton$`Daily Max 8-hour Ozone Concentration`,na.rm = TRUE)
ROAVintonstats <- cbind(ROAVintonmax,ROAVintonmean,ROAVintonsd)

ROAFinal <- rbind(ROAVintonstats)
colnames(ROAFinal) <- c("ROAMax","Mean","Standard Deviation")






