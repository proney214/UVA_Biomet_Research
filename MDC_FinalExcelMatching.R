#Grouping MDC (Acme Guide) by station

#2015-2020 Dataset

groupsVJI1520 <- FullSM_1520 %>%
  filter(ID == "VJI") %>%
  group_by(Date, MDCA) %>%
  summarize(count_by_siteyear =  n()) 

NAS <- groupsVJI1520[is.na(groupsVJI1520$MDCA), ]

groupswideVJI1520 <- pivot_wider(groupsVJI1520,id_cols = "Date",names_from = MDCA,values_from = count_by_siteyear)

OrderedVJI1520 <- groupswideVJI1520 %>% relocate(`MDC 02`, `MDC 03`,`MDC 04`,`MDC 05`,`MDC 06`,`MDC 07`,
                                                 `MDC 08`, `MDC 09`, `MDC 10`,`MDC 11`, `MDC 12`,
                                                 `MDC 13`, `MDC 14`, `MDC 15`, `MDC 16`, `MDC 17`,
                                                 `MDC 18`, `MDC 19`, `MDC 20`, `MDC 21`,#`MDC 22`,
                                                 `MDC 23`, `MDC 25`,#`MDC 24`,
                                                 `Cancer NOS`,`Non-billable ICD`,.after = `MDC 01`) 
#2005-2014 Dataset

groupsVJI0514 <- FullSM_0514 %>%
  filter(ID == "VJI") %>%
  group_by(Date, MDCA) %>%
  summarize(count_by_siteyear =  n()) 

groupswideVJI0514 <- pivot_wider(groupsVJI0514,id_cols = "Date",names_from = MDCA,values_from = count_by_siteyear)

OrderedVJI0514 <- groupswideVJI0514 %>% relocate( `MDC 03`,`MDC 04`,`MDC 05`,`MDC 06`,`MDC 07`,
                                                  `MDC 08`, `MDC 09`, `MDC 10`,`MDC 11`, `MDC 12`,
                                                  `MDC 13`, `MDC 14`, `MDC 15`, `MDC 16`, `MDC 17`,
                                                  `MDC 18`, `MDC 19`, `MDC 20`, `MDC 21`,#`MDC 22`,
                                                  `MDC 23`, `MDC 25`,#`MDC 24`,
                                                  `Cancer NOS`,`Non-billable ICD`,.after = `MDC 01`)
OrderedVJI0514$`MDC 02` <- NA

VJIFullMDC <- rbind(OrderedVJI0514,OrderedVJI1520)

VJIFullMDC$`MDC 22` <- NA
VJIFullMDC$`MDC 24` <- NA

VJIFullMDCdf <- data.frame(VJIFullMDC)
VJIFullMDCdf$Date <- as.Date(VJIFullMDCdf$Date)

VJIFullMDCpad <- pad(VJIFullMDCdf, interval = "day")

write.csv(VJIFullMDCpad, file = "VJIFullMDC.csv")
