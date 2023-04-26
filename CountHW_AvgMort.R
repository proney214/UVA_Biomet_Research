## Average deaths per day in Possible HW days

library(readxl)
library(dplyr)
library(ggplot2)

AdjustedMortMasterOrg <- read_excel("~/Desktop/AdjustedMortMaster.xlsx")
AdjustedMortMaster <- head(AdjustedMortMasterOrg, - 1) 

RICadj <- data.frame(AdjustedMortMaster$rowSums.newmortcompleteRIC...2.9...na.rm...TRUE.)

RIC_working_weatherprep <- read_excel("~/Downloads/RIC.working.weatherprep-2.xlsx")
RICHWSevere <- RIC_working_weatherprep$HeatWavesSevere
Date <- RIC_working_weatherprep$Date

plot(RIC_working_weatherprep$`MaxT(C)`,type = 'l')
lines(RIC_working_weatherprep$`MaxT(C)`,col = gray)
lines(rollmean(rollmean(RIC_working_weatherprep$`MaxT(C)`,40),40),col='yellow')

hist(RIC_working_weatherprep$ATF1pm, 
     breaks = (max(RIC_working_weatherprep$ATF1pm,na.rm = TRUE) - min(RIC_working_weatherprep$ATF1pm,na.rm = TRUE)),
     main = " ",
     xlab = "Apparent Temperature (F)",
     xlim = c(min(RIC_working_weatherprep$ATF1pm,na.rm = TRUE),max(RIC_working_weatherprep$ATF1pm,na.rm = TRUE)))

max(RIC_working_weatherprep$ATF1pm,na.rm = TRUE) - min(RIC_working_weatherprep$ATF1pm,na.rm = TRUE)))

work <- cbind(Date,RICHWSevere,RICadj)

RIC1115 <- work %>%
  filter("2011-01-01" <= Date & Date < "2015-12-31") %>%
  filter(RICHWSevere == 1)
mean(RIC1115$AdjustedMortMaster.rowSums.newmortcompleteRIC...2.9...na.rm...TRUE.)

RIC1620 <- work %>%
  filter("2016-01-01" <= Date & Date < "2020-12-31") %>%
  filter(RICHWSevere == 1)
mean(RIC1620$AdjustedMortMaster.rowSums.newmortcompleteRIC...2.9...na.rm...TRUE.)

df1 <- aggregate(RICadj,by = RICHWSevere, FUN = "mean")



#Daily Deaths with mort
df <- cbind(HeatWavesSevere,mort)
df1 <- aggregate(mort,by = HeatWavesSevere, FUN = "mean")

#Daily Deaths with adjusted mort
df2 <- cbind(HeatWavesSevere,RICadj)
df3 <- aggregate(RICadj,by = HeatWavesSevere, FUN = "mean")

#Daily Heatwave Deaths Adj Mort 2011-2015

RICHW1115 <- as.list(RIC1115$HeatWavesSevere)
RICAdjMort1115 <- as.list(AdjMort1115$rowSums.newmortcompleteRIC...2.9...na.rm...TRUE.)
#Daily Deaths with adjusted mort
df4 <- cbind(RICHW1115,RICAdjMort1115)
df5 <- aggregate(RICAdjMort1115,by = RICHW1115, FUN = "mean")
