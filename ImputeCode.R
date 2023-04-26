library(readxl)
library(imputeTS)
library(dplyr)

#THIS ATTEMPT SUCCESSFUL

#Read in data, remove all columns except Temp, Dew, and Wind which are the columns we want to impute
raw <- read_excel("PHFimputeREAL.xlsx")
rawnum <- raw %>%
  select(-...7,-Date_correct,-Check,-Station,-Time_correct,-`Valid Time`,-`Valid Date`)

#Make sure Wind speed is a numeric type, check to see how many cells are missing
rawnum$`Wind Speed (sknt)` <- as.numeric(rawnum$`Wind Speed (sknt)`)
i <- which(is.na(rawnum))

#Impute the data, check to see nothing is missing
imp <- na_interpolation(rawnum)
i2 <- which(is.na(imp))

#Final Step: Put Date Time Info with reference time, make new column in df to get time in
ActDate <- read_excel("Date.time.Actual.xlsx")
imp$Date <- ActDate$TIME

#Didn't like having the Date on the right column so changed the order to be consistent with original df format
ActFinal <- imp[,c(4, 1, 2, 3)]
ActFinal

write.csv(ActFinal,'PHFimputedFinal.csv')

#Test
#set <- read_excel("ImputeTest.xlsx")
#test <- na_interpolation(set)

#ATTEMPT FAILS DUE TO NOT FULL LENGTH; MUST MAKE DATES REACH ALL THE WAY TO BOTTOM
#real <- read_excel("PHFimpute.xlsx")
#real <- real %>%
#select(-...7,-Date_correct,-Check,-Station,-Time_correct,-`Valid Time`,-`Valid Date`)
#real$`Wind Speed (sknt)` <- as.numeric(real$`Wind Speed (sknt)`)
#i <- which(is.na(real))

#imp <- na_interpolation(real)
#i2 <- which(is.na(imp))
