### HEAT/COLD WAVE DETECTION TEST ###

library(dplyr)
library(ggpubr)
library(heatwaveR)
library(ggplot2)
library(readxl)
RIC_heatwave_test <- read_excel("~/Desktop/RIC.maxmin.final.xlsx")
RIC <- RIC_heatwave_test
RIC$Date <- as.Date(RIC$Date)

#The tMax threshold
# The current WMO standard climatology period is 1981-01-01 to 2010-12-31 and should be used when possible
# We rather use 1961-01-01 to 1990-01-01 as this is the oldest 30 year period available in the data
tMax_clim <- ts2clm(data = RIC, x = Date, y = tmax, climatologyPeriod = c("2005-01-01", "2020-12-31"), pctile = 90)

# The tMin exceedance
# Note the use here of 'minDuration = 3' and 'maxGap = 1' as the default atmospheRIC arguments
# The default marine arguments are 'minDuration = 5' and 'maxGap = 2'
tMin_exc <- exceedance(data = RIC, x = Date, y = tmin, threshold = 70, minDuration = 3, maxGap = 1)$threshold
threshClim2 = tMin_exc$exceedance

events <- detect_event(data = tMax_clim, x = Date, y = tmax, # The 90th percentile threshold
                       threshClim2 = tMin_exc$exceedance) # The flat exceedance threshold
#coldSpells=TRUE changes the code from searching for heatwaves to searching for cold waves

View(events$event)
