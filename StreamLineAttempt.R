# TEST STREAMLINED PROGRAM FOR 3-CAVS WEATHER DATA EXTRACTION AND CLEANING
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
temp.interp=na.approx(temp, maxgap = 5, rule=2)
tdew.interp=na.approx(tdew, maxgap = 5,rule=2)
wind.interp=na.approx(wind, maxgap = 5, rule=2)
#
# CREATE NEW DATA FRAME USING THE INTERPOLATED VALUES
#
NewFinal.test=data.frame(NewNew.test$station,NewNew.test$valid,temp.interp,tdew.interp,wind.interp)
i = which(is.na(NewFinal.test)) #find remaining NA's
length(i) #Completely full data with interpolated values
#
#
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
for (i in 1:1460) {
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
  write.table(NewFinal.test[keep_row,],file="~/Desktop/EVSC_Research/NewIntPut6.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(NewFinal.test[keep_row,])
}
F = read.table("NewIntPut6.csv")

### 2006 PHF ###
#
#
# CREATE NEW DATA FRAME USING THE INTERPOLATED VALUES
#
NewFinal.test=data.frame(NewNew.test$station,NewNew.test$valid,temp.interp,tdew.interp,wind.interp)
i = which(is.na(NewFinal.test)) #find remaining NA's
length(i) #Completely full data with interpolated values
#
#
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
for (i in 1461:2920) {
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
  write.table(NewFinal.test[keep_row,],file="~/Desktop/EVSC_Research/NewIntPut7.csv",row.names=FALSE,col.names=FALSE,append=TRUE)
  print(NewFinal.test[keep_row,])
}
G = read.table("NewIntPut7.csv")