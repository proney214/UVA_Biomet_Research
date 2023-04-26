#EVSC 4340 Stroke Data Pull

#install.packages(stringr)
library(dplyr)
library(stringr)

#Take out COVID cases / Create Codes 1520 without Covid

#Run MDC_UltimateMort prior to this data grab in order to attach race data to raw

Strokes1520 <- FullSM_1520[grepl("I60|I61|I62|I63|I64|I65|I66|I67|I68|I69",FullSM_1520$RAC), ] #Count is 34819
Strokes0514 <- FullSM_0514[grepl("I60|I61|I62|I63|I64|I65|I66|I67|I68|I69",FullSM_0514$MULTIPLE_CAUSES), ] #Count is 34819

StrokesDemo1520 <- Strokes1520 %>%
  filter(ID == "CHO") %>%
  group_by(Date, ID, RaceGroupings) %>%
  summarize(count_by_siteyear =  n()) 

groupswideStrokes <- pivot_wider(StrokesDemo1520,id_cols = "Date",names_from = RaceGroupings,values_from = count_by_siteyear)

test = data.frame(c("CHO","EMV","EZF","IAD","LYH","OKV","ORF","PHF","RIC","ROA","SHD","VJI"))

for (i in (1:12)) {
  print(i)
  print(test[i,])
  StrokesDemo0514 <- Strokes0514 %>%
    filter(ID == test[i,]) %>%
    group_by(Date, RaceGroupings) %>%
    summarize(count_by_siteyear =  n()) 
  groupswideStrokes0514 <- pivot_wider(StrokesDemo0514,id_cols = "Date",names_from = RaceGroupings,values_from = count_by_siteyear)
  
  StrokesDemo1520 <- Strokes1520 %>%
    filter(ID == test[i,]) %>%
    group_by(Date, RaceGroupings) %>%
    summarize(count_by_siteyear =  n()) 
  groupswideStrokes1520 <- pivot_wider(StrokesDemo1520,id_cols = "Date",names_from = RaceGroupings,values_from = count_by_siteyear)
  
  StrokesFull <- rbind(groupswideStrokes0514,groupswideStrokes1520)
  StrokesFulldf <- data.frame(StrokesFull)
  StrokesFulldf$Date <- as.Date(StrokesFulldf$Date)
  
  StrokesFulldfpad <- pad(StrokesFulldf, interval = "day", start_val = as.POSIXct('2005-01-01'), end_val = as.POSIXct('2020-12-31'))
  
  StrokesFulldfpad[is.na(StrokesFulldfpad)] <- 0
  
  StrokesFulldfpad$ID <- test[i,]
  
  write.csv(StrokesFulldfpad, paste0("StrokesDemo1520",i,"StrokeCleaningV1.csv"))
}


