# TEST PROGRAM FOR 3-CAVS WEATHER DATA EXTRACTION AND CLEANING
#
#  INSTALL PACKAGES AND LOAD LIBRARIES
#
library(readxl)
library(zoo)
library(lubridate)
#
# READ IN RAW DATA, Convert to numeric
#
NewNew.test=read_excel("NewExcelTest.xlsx",na="M")
temp=as.numeric(NewNew.test$tmpc)
tdew=as.numeric(NewNew.test$dwpc)
wind=as.numeric(NewNew.test$sknt)
#
# LINEARLY INTERPOLATE ALL MISSING VALUES
#
temp.interp=na.approx(temp,rule=2)
tdew.interp=na.approx(tdew,rule=2)
wind.interp=na.approx(wind, rule=2)
#
# CREATE NEW DATA FRAME USING THE INTERPOLATED VALUES
#
NewFinal.test=data.frame(NewNew.test$station,NewNew.test$valid,temp.interp,tdew.interp,wind.interp)
i = which(is.na(NewFinal.test)) #find remaining NA's
length(i) #Completely full data with interpolated values
#
#
#NewFinal.test$NewNew.test.valid=as.POSIXct(NewFinal.test$NewNew.test.valid)
#typeof(NewFinal.test$NewNew.test.valid)
#NewFinal.test$DTEST = ymd_hm(NewFinal.test$NewNew.test.valid)
#NewFinal.test$DTEST = force_tz(mdy_hm(NewFinal.test$NewNew.test.valid),tzone='EST')
#NewFinal.test$DTEST = as.POSIXct(NewFinal.test$DTEST)

#
# READ IN FILE WITH CORRECT, REFERENCE TIMES (365 OBS)
#
Ref.time=read_excel("Date.Time.Actual.xlsx")
#
# INITIATE VALUES TO PASS INTO LOOP
#
# Compare time in raw data file to sequential times in the reference data file. 
# Reference time is the outer loop.
#
print (nrow(NewFinal.test))
for (i in 1:365) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(NewFinal.test)) {
    test_diff=difftime(NewFinal.test$NewNew.test.valid[j],Ref.time$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  #print(c(keep_row,NewFinal.test$valid[keep_row]))
  #
  write.table(NewFinal.test[keep_row,],file="~/Desktop/EVSC_Research/NewIntPut6.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(NewFinal.test[keep_row,])
}
#H = read.csv("NewIntPut4.csv", sep = " ")
#G = read.csv("NewIntPut5.csv", sep = " ")
#W = read.table("NewIntPut5.csv", sep = " ", skip = 11)
F = read.table("NewIntPut6.csv")


### Attempt at 7:00

Ref.time.7 =read_excel("Date.Time.Actual7.xlsx")
#
# INITIATE VALUES TO PASS INTO LOOP
#
# Compare time in raw data file to sequential times in the reference data file. 
# Reference time is the outer loop.
#
print (nrow(NewFinal.test))
for (i in 1:365) {
  keep_diff=91
  keep_row=0
  for (j in 1:nrow(NewFinal.test)) {
    test_diff=difftime(NewFinal.test$NewNew.test.valid[j],Ref.time.7$TIME[i],units="mins")
    #
    # Within the loop, keep index of date (j) if the time difference ('test_diff) is within # 90 minutes of the reference time AND if it's absolute value is less than the current smallest time difference ("keep_diff").  By keeping track of the row, we know the entire observation in the raw data matrix, which can then be written to a dataframe or output file.
    #
    if ((abs(test_diff))<=90 & abs(test_diff)<abs(keep_diff)) {
      keep_diff=abs(test_diff)
      keep_row=j
    }
  }
  #print(c(keep_row,NewFinal.test$valid[keep_row]))
  #
  write.table(NewFinal.test[keep_row,],file="~/Desktop/EVSC_Research/NewIntPut5.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(NewFinal.test[keep_row,])
}
#H = read.csv("NewIntPut4.csv", sep = " ")

