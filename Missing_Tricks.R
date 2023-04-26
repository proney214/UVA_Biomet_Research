### Missing Tricks ###

library(lubridate)

### 1) Removing all missing data when loading in dataset

MissingTestSet = read.csv("NewNewTest.csv", na.strings = "M")
ActuallyMissing = na.omit(Missing)


### 2) Find missing days

MissingTestSet = read.csv("NewNewTest.csv", na.strings = "M")
MissingTestSet$DTEST = ymd_hm(MissingTestSet$valid)
MissingTestSet$DTEST = force_tz(mdy_hm(MissingTestSet$valid), tzone = 'EST')
MissingTestSet$DTEST = as.POSIXct(MissingTestSet$DTEST)

i = which(is.na(MissingTestSet)) #identifies cells which contain missing data
length(i)
print(i[1:10])

MissingData = MissingTestSet$DTEST[i]
print(MissingData[1:10])
