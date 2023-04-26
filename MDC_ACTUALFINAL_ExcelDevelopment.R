#Create Excel Sheets for each MDC by station

#FullSM_1520 <- read.csv("MDC_Station_Matching_1520.csv")

#FullNAS <- FullSM_1520[is.na(FullSM_1520$MDCAcme), ]

groupsROA1520 <- FullSM_1520 %>%
  filter(ID == "ROA") %>%
  group_by(Date, MDCAcme) %>%
  summarize(count_by_siteyear =  n()) 

#NAS <- groupsROA1520[is.na(groupsROA1520$MDCAcme), ]

groupswideROA1520 <- pivot_wider(groupsROA1520,id_cols = "Date",names_from = MDCAcme,values_from = count_by_siteyear)

OrderedROA1520 <- groupswideROA1520 %>% relocate(`MDC 02`, `MDC 03`,`MDC 04`,`MDC 05`,`MDC 06`,`MDC 07`,
                                                 `MDC 08`, `MDC 09`, `MDC 10`,`MDC 11`, `MDC 12`,
                                                 `MDC 13`, `MDC 14`, `MDC 15`, `MDC 16`, `MDC 17`,
                                                 `MDC 18`, `MDC 19`, `MDC 20`, `MDC 21`,#`MDC 22`,
                                                 `MDC 23`, `MDC 25`,#`MDC 24`,
                                                 `Cancer NOS`,`Non-billable ICD`,.after = `MDC 01`) 

#2005-2014 Dataset

#FullSM_0514 <- read.csv("MDC_Station_Matching_0514.csv")

#FullNAS0514 <- FullSM_0514[is.na(FullSM_0514$MDCAcme), ]

groupsROA0514 <- FullSM_0514 %>%
  filter(ID == "ROA") %>%
  group_by(Date,MDCAcme) %>%
  summarize(count_by_siteyear =  n()) 

#NAS <- groupsROA0514[is.na(groupsROA0514$MDCAcme), ]

#NAS0514 <- FullSM_0514[is.na(FullSM_0514$MDCAcme), ]

#write.csv(NAS0514, file = "MDC_NAs_2005_2014.csv")

groupswideROA0514 <- pivot_wider(groupsROA0514,id_cols = "Date",names_from = MDCAcme,values_from = count_by_siteyear)
groupswideROA0514 $`MDC 02` <- NA

OrderedROA0514 <- groupswideROA0514 %>% relocate(`MDC 02`, `MDC 03`,`MDC 04`,`MDC 05`,`MDC 06`,`MDC 07`,
                                                  `MDC 08`, `MDC 09`, `MDC 10`,`MDC 11`, `MDC 12`,
                                                  `MDC 13`, `MDC 14`, `MDC 15`, `MDC 16`, `MDC 17`,
                                                  `MDC 18`, `MDC 19`, `MDC 20`, `MDC 21`,#`MDC 22`,
                                                  `MDC 23`, `MDC 25`,#`MDC 24`,
                                                  `Cancer NOS`,`Non-billable ICD`,.after = `MDC 01`)

ROAFullMDC <- rbind(OrderedROA0514,OrderedROA1520)

ROAFullMDC$`MDC 22` <- NA
ROAFullMDC$`MDC 24` <- NA

ROAFullMDCdf <- data.frame(ROAFullMDC)
ROAFullMDCdf$Date <- as.Date(ROAFullMDCdf$Date)

ROAFullMDCpad <- pad(ROAFullMDCdf, interval = "day")#, start_val = as.POSIXct('2005-01-01'), end_val = as.POSIXct('2020-12-31'))
ROAFullMDCpad[is.na(ROAFullMDCpad)] <- 0

write.csv(ROAFullMDCpad, file = "ROAFullMDC.csv")

ROAdaymorts <- rbind(groupsROA0514,groupsROA1520)
ROAdaymorts$Date <- as.Date(ROAdaymorts$Date)
ROAdaymortsfull <- pad(ROAdaymorts, interval = "day")

#write.csv(ROAdaymortsfull, file = "ROAdaymortsfull.csv")


