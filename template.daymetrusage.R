setwd("~/Desktop/EVSC_Research")
library(daymetr)

#test <- download_daymet(
  location = c(18.9103, -114.6109),
  start = 1980,
  end = 1980,
  path=tempdir("~/Desktop/EVSC_Research"),
  param = "ALL",
  silent = FALSE,
  force = FALSE
)

df <- download_daymet(site = "Oak Ridge National Laboratories",
                      lat = 36.0133,
                      lon = -84.2625,
                      start = 2000,
                      end = 2010,
                      internal = TRUE,
                      simplify = TRUE) # return tidy data !!
View(df)

##Convert RIC to wide to long table
# convert into a dataframe
#Make aa==unique(longdf$variable)
#curDat=df[df$variable]

ColdDaysExtreme$Variable <- "ColdDayExtreme"
ColdDaysMod$Variable <- "ColdDaysMod"
test <- cbind(ColdDaysExtreme, ColdDaysMod)
View(test)
test2 <- as.data.frame(ColdDaysExtreme)
View(test2)
