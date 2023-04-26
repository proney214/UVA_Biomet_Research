#Take out COVID cases / Create Codes 1520 without Covid

COVID <- Codes1520[grepl("U071",Codes1520$RAC), ] #Count is 6033
Check <- COVID[grepl("2020",COVID$DOD_YR), ] #Count is 6033

write.csv(COVID, file = "COVID_U071.csv")
write.csv(NewDF, file = "ICD_MDC_MatchingFullLength.csv")

pregnant1520 <- Codes1520[grepl("P05|P07|P08",Codes1520$RAC), ] #Count is 6033
pregnant0514 <- Codes0514[grepl("P05|P07|P08",Codes0514$MULTIPLE_CAUSES), ] #Count is 6033

Aggpregnant0514 <- pregnant0514 %>%
  group_by(Date) %>%
  summarize(count_of_cases = n())

max(Aggpregnant0514$count_of_cases)

plot(x = Aggpregnant0514$Date,y = Aggpregnant0514$count_of_cases,type = 'l')
lines(rollmean(rollmean(Aggpregnant0514$count_of_cases,40),40),col='yellow')

AggCOVID <- COVID %>%
  group_by(DOD_YR) %>%
  summarize(count_of_cases = n())

#Remove Covid Cases from 2015-2020 Data

NewDF1520_NoCovid <- NewDF1520[!grepl("U071", NewDF1520$RAC), ]

write.csv(NewDF1520, file = "ICD_MDC_Matching_2015_2020_No_Covid.csv")

#COVID

COVID <- Codes1520[grepl("U071",Codes1520$RAC), ] #Count is 6033

NewDFCovid <- left_join(COVID, key, by = c("SexCheck" = "Mortality_ICD"))
NewDFCovid <- NewDFCovid[order(NewDFCovid$Date), ]

write.csv(NewDFCovid, file = "ICD_MDC_Matching_COVID.csv")

#C80 (2015-2020)

C80 <- Codes1520[grepl("C80",Codes1520$RAC), ] #Count is 6033

NewDFC80 <- left_join(C80, key, by = c("SexCheck" = "Mortality_ICD"))
NewDFC80 <- NewDFC80[order(NewDFC80$Date), ]

write.csv(NewDFC80, file = "ICD_MDC_Matching_C80_2015_2020.csv")

#C80 (2005-2014)

C80 <- Codes0514[grepl("C80",Codes0514$MULTIPLE_CAUSES), ] #Count is 6033

NewDFC80 <- left_join(C80, key, by = c("SexCheck" = "Mortality_ICD"))
NewDFC80 <- NewDFC80[order(NewDFC80$Date), ]

write.csv(NewDFC80, file = "ICD_MDC_Matching_C80_2005_2014.csv")

#Count how many in each category

AggFull <- NewDF %>%
  group_by(MDC) %>%
  summarize(count_of_cases = n())

Agg <- aggregate(NewDF$Count, by=list(Category=NewDF$MDC), FUN=sum)
colnames(Agg) <- c("Category","CountTotal")
View(Agg)

#Covid Removal

FullWithoutCovid <- NewDF[grepl("U071|U049",NewDF$Mortality_ICD), ] #Count is 6033

#Prelim Analysis looking only at Primary (ACME)

Agg0514 <- Codes0514 %>%
  group_by(ACME) %>%
  summarize(count_of_cases = n())

Agg1520 <- Codes1520 %>%
  group_by(ACME,DOD_YR) %>%
  summarize(count_of_cases = n())

Agg1520[Agg1520$ACME == "U071", ]## Count is 5556

Agg0514[Agg0514$ACME == "U071", ]

#Scratch

a <- KeyOrder$Mortality_ICD
b <- RealFinal$ACME

Yes <- data.frame(a[a %in% b])
No <- data.frame(a[!a %in% b])
Eh <- data.frame(b[b %in% c])

View(Yes)
View(No)
View(Eh)

Duplicates <- KeyOrder[duplicated(KeyOrder$Mortality_ICD) | duplicated(KeyOrder$Mortality_ICD, fromLast=TRUE), ]

Lost <- NewDF[is.na(NewDF$MDC), ]
unique(Lost$Mortality_ICD)

A <- n_distinct(KeyOrder$Mortality_ICD)
n_distinct(RealFinal$ACME)

a <- KeyOrder$Mortality_ICD
b <- RealFinal$ACME
c <- Duplicates$Mortality_ICD

Yes <- data.frame(a[a %in% b])
No <- data.frame(a[!a %in% b])
Eh <- data.frame(b[b %in% c])

View(Yes)
View(No)
View(Eh)

C80 <- Codes1520[grepl("C80",Codes1520$RAC), ] #Count is 6033
C802 <- Codes0514[grepl("C80",Codes0514$MULTIPLE_CAUSES), ]

write.csv(C80, file = "C80_2015_2020.csv")
write.csv(C802, file = "C80_2005_2014.csv")
write.csv(COVID, file = "COVID_U071.csv")


C80A <- Codes1520[grepl("C80",Codes1520$ACME), ] #Count is 6033
C802A <- Codes0514[grepl("C80",Codes0514$ACME), ]

write.csv(C80A, file = "C80_ACME_2015_2020.csv")
write.csv(C802A, file = "C80_ACME_2005_2014.csv")

Codes0514$MDC <- NewDF[1:length(Codes0514$X), 1]

#Key Work

ICD10_Mapping_v8 <- read_excel("~/Desktop/ICD10_Mapping_v8.xlsx", 
                               sheet = "MDC_Crosswalk")

KeyDuplicatesOrg <- ICD10_Mapping_v8[duplicated(ICD10_Mapping_v8$Mortality_ICD) | duplicated(ICD10_Mapping_v8$Mortality_ICD, fromLast=TRUE), ]

KeyDuplicatesOrder <- KeyDuplicatesOrg[order(KeyDuplicatesOrg$Mortality_ICD),]

Key <- ICD10_Mapping_v8 %>%
  select(Mortality_ICD,MDC)

Key$Mortality_ICD <-gsub("-$","",Key$Mortality_ICD )

KeyDuplicates <- Key[duplicated(Key$Mortality_ICD) | duplicated(Key$Mortality_ICD, fromLast=TRUE), ]


#Get just the columns you need

RealData1 <- Codes0514 %>%
  select(SexCheck,Date)

RealData2 <- Codes1520 %>%
  select(SexCheck,Date)

RealData <- rbind(RealData1,RealData2)

key <- ICD10_Mapping_v8 %>%
  select(Mortality_ICD,MDC)

#Creating Updated Patient Sheets

#Key Check

NewDF = right_join(key, RealData, by = c("Mortality_ICD" = "SexCheck"))

#write.csv(NewDF, file = "ICD_MDC_MatchingFullLength.csv")


#Data Review

UVA_Climate_Study_2015_2020 <- read_csv("UVA_Climate_Study_2015_2020_update1 (1).csv")


BigMort3 <- read_excel("~/Desktop/BigMort3.xlsx")

BigMort20052014 <- BigMort3[grepl("2005|2006|2007|2008|2009|2010|2011|2012|2013|2014", BigMort3$DOD_YR), ]

BigMort201520 <- BigMort3[grepl("2015|2016|2017|2018|2019|2020", BigMort3$DOD_YR), ]

MDC0514<- read.csv("MDC_Station_Matching_0514.csv")
MDC1520<- read.csv("MDC_Station_Matching_1520.csv")

I60 <- MDC1520[grepl("I60",MDC1520$ICD_SC), ]
I61 <- MDC1520[grepl("I61",MDC1520$ICD_SC), ]


#Data Age and Race Check

#group ICD file by age and race

FullSM_0514 <- read.csv("MDC_Station_Matching_0514.csv")
FullSM_1520 <- read.csv("MDC_Station_Matching_1520.csv")

groupsEMV1520 <- FullSM_1520 %>%
  filter(ID == "EMV") %>%
  group_by(Date, AGE) %>%
  summarize(count_by_siteyear =  n()) 

groupsEMV0514 <- FullSM_0514 %>%
  filter(ID == "EMV")
#  group_by(Date, AGE_GROUPING) %>%
# summarize(count_by_siteyear =  n()) 

EMV_20140913 <- groupsEMV0514[grepl("20140913",groupsEMV0514$Date), ]

groupsEMV0514BM <- BigMort3 %>%
  filter(`Station ID` == "EMV")
#  group_by(Date, AGE_GROUPING) %>%
# summarize(count_by_siteyear =  n()) 

groupsEMV0514BM$DateOfDeath <- as.character(groupsEMV0514BM$DateOfDeath)

EMV_20140913_BM <- groupsEMV0514BM[grepl("2014-09-13",groupsEMV0514BM$DateOfDeath), ]

MDC20152020_Short <- FullSM_1520[1:1000, ]
write.csv(MDC20152020_Short, "MDC_Abridged_forWendy_10242023.csv")

WayOld <- FullSM_0514[FullSM_0514$AGE_OF_DECEASED > 600, ]


### Counts for each MDC by year

#Check how many per year

MDC_Year_group_1520 <- FullSM_1520 %>%
  group_by(DOD_YR, MDCAcme) %>%
  summarise(count_by_year =  n())

write.csv(MDC_Year_group_1520,"MDC_CountByYear_1520.csv")

MDC_Year_group_0514 <- FullSM_0514 %>%
  group_by(DATE_OF_DEATH_YEAR, MDCAcme) %>%
  summarise(count_by_year =  n())

write.csv(MDC_Year_group_0514,"MDC_CountByYear_0514.csv")

## Yearly MDC Data

FullMDCYear <- rbind(MDC_Year_group_0514,MDC_Year_group_1520)

write.csv(FullMDCYear,"MDC_Yearly_Totals.csv")

MDC01_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 01", ]
plot(MDC01_Yearly$count_by_year, type='l',main="MDC 01 Yearly")

MDC02_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 02", ]
plot(MDC02_Yearly$count_by_year, type='l',main="MDC 02 Yearly")

MDC03_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 03", ]
plot(MDC03_Yearly$count_by_year, type='l',main="MDC 03 Yearly")

MDC04_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 04", ]
plot(MDC04_Yearly$count_by_year, type='l',main="MDC 04 Yearly")

MDC05_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 05", ]
plot(MDC05_Yearly$count_by_year, type='l',main="MDC 05 Yearly")

MDC06_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 06", ]
plot(MDC06_Yearly$count_by_year, type='l',main="MDC 06 Yearly")

MDC07_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 07", ]
plot(MDC07_Yearly$count_by_year, type='l',main="MDC 07 Yearly")

MDC08_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 08", ]
plot(MDC08_Yearly$count_by_year, type='l',main="MDC 08 Yearly")

MDC09_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 09", ]
plot(MDC09_Yearly$count_by_year, type='l',main="MDC 09 Yearly")

MDC10_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 10", ]
plot(MDC10_Yearly$count_by_year, type='l',main="MDC 10 Yearly")

MDC11_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 11", ]
plot(MDC11_Yearly$count_by_year, type='l',main="MDC 11 Yearly")

MDC12_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 12", ]
plot(MDC12_Yearly$count_by_year, type='l',main="MDC 12 Yearly")

MDC13_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 13", ]
plot(MDC13_Yearly$count_by_year, type='l',main="MDC 13 Yearly")

MDC14_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 14", ]
plot(MDC14_Yearly$count_by_year, type='l',main="MDC 14 Yearly")

MDC15_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 15", ]
plot(MDC15_Yearly$count_by_year, type='l',main="MDC 15 Yearly")

MDC16_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 16", ]
plot(MDC16_Yearly$count_by_year, type='l',main="MDC 16 Yearly")

MDC17_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 17", ]
plot(MDC17_Yearly$count_by_year, type='l',main="MDC 17 Yearly")

MDC18_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 18", ]
plot(MDC18_Yearly$count_by_year, type='l',main="MDC 18 Yearly")

MDC19_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 19", ]
plot(MDC19_Yearly$count_by_year, type='l',main="MDC 19 Yearly")

MDC20_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 20", ]
plot(MDC20_Yearly$count_by_year, type='l',main="MDC 20 Yearly")

MDC21_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 21", ]
plot(MDC21_Yearly$count_by_year, type='l',main="MDC 21 Yearly")

MDC22_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 22", ]
plot(MDC22_Yearly$count_by_year, type='l',main="MDC 22 Yearly")

MDC23_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 23", ]
plot(MDC23_Yearly$count_by_year, type='l',main="MDC 23 Yearly")

MDC24_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 24", ]
plot(MDC24_Yearly$count_by_year, type='l',main="MDC 24 Yearly")

MDC25_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "MDC 25", ]
plot(MDC25_Yearly$count_by_year, type='l',main="MDC 25 Yearly")

MDCCancerNOS_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "Cancer NOS", ]
plot(MDCCancerNOS_Yearly$count_by_year, type='l',main="MDC CancerNOS Yearly")

MDCNonBillable_Yearly <- FullMDCYear[FullMDCYear$MDCAcme == "Non-billable ICD", ]
plot(MDCNonBillable_Yearly$count_by_year, type='l',main="MDC NonBillable Yearly")



